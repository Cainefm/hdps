
#' Identify candidate covariates based on prevalence
#'
#' @param dt Data table with patient and covariate information
#' @param id Column name for patient identifier
#' @param code Column name for codes
#' @param type Prefix for output (dx, px, rx)
#' @param n Maximum number of candidates to return
#' @param min_patients Minimum patients required per covariate
#' @return List with candidates, filtered data, and patient IDs
#' @export
identify_candidates <- function(dt, id, code, type, n = 200, min_patients = 10) {
    if (missing(dt) || (!is.data.table(dt) && !is.data.frame(dt))) {
        stop("'dt' must be a data.frame or data.table")
    }
    if (missing(id) || !is.character(id)) {
        stop("'id' must be a character column name")
    }
    if (missing(code) || !is.character(code)) {
        stop("'code' must be a character column name")
    }
    if (missing(type) || !is.character(type)) {
        stop("'type' must be a character")
    }
    
    dt <- copy(dt)
    setDT(dt)
    
    if (!id %in% names(dt)) stop("Column '", id, "' not found")
    if (!code %in% names(dt)) stop("Column '", code, "' not found")
    
    setnames(dt, c(id, code), c("pid", "code"))
    
    prevalence <- dt[, .(n_patients = length(unique(pid))), by = code]
    total_patients <- dt[, length(unique(pid))]
    prevalence[, prevalence := (n_patients / total_patients) * 100]
    prevalence[, prevalence_truncated := ifelse(prevalence > 50, 100 - prevalence, prevalence)]
    
    candidates <- prevalence[n_patients >= min_patients][order(prevalence_truncated, decreasing = TRUE)]
    candidates <- candidates[1:min(n, nrow(candidates))]
    
    result <- dt[code %in% candidates$code]
    result[, code := paste0(type, "_", code)]
    
    list(candidates = candidates, data = result, patient_ids = unique(dt$pid))
}

#' Assess recurrence of covariates
#'
#' @param dt Data table with patient and covariate information
#' @param id Column name for patient identifier
#' @param code Column name for codes
#' @param type Prefix for output (dx, px, rx)
#' @param rank Maximum number of covariates to include
#' @return Data table with recurrence assessment
#' @export
assess_recurrence <- function(dt, id, code, type, rank = Inf) {
    if (missing(dt) || (!is.data.table(dt) && !is.data.frame(dt))) {
        stop("'dt' must be a data.frame or data.table")
    }
    if (missing(id) || !is.character(id)) {
        stop("'id' must be a character column name")
    }
    if (missing(code) || !is.character(code)) {
        stop("'code' must be a character column name")
    }
    if (missing(type) || !is.character(type)) {
        stop("'type' must be a character")
    }
    if (!is.numeric(rank)) {
        stop("'rank' must be numeric")
    }
    
    dt <- copy(dt)
    setDT(dt)
    
    if (!id %in% names(dt)) stop("Column '", id, "' not found")
    if (!code %in% names(dt)) stop("Column '", code, "' not found")
    
    setnames(dt, c(id, code), c("pid", "code"))
    
    count_cutoff <- if (rank == Inf) Inf else {
        dt[, .(count = .N), .(pid, code)][order(count, decreasing = TRUE)][rank, count]
    }
    
    counts <- dt[, .(count = .N), .(pid, code)][count <= count_cutoff]
    quantiles <- dt[, .(count = .N), .(pid, code)][, .(
        Q1 = quantile(count, 0.25),
        Q2 = quantile(count, 0.5),
        Q3 = quantile(count, 0.75)
    ), code][, .(code, Q1, Q2 = ifelse(Q1 == Q2, NA, Q2), Q3 = ifelse(Q2 == Q3, NA, Q3))]
    
    merged <- merge(counts, quantiles, by = "code", all.x = TRUE)
    patterns <- merged[, .(
        once = as.numeric(ifelse(count >= 1, 1, 0)),
        spor = as.numeric(ifelse(!is.na(Q2) & count >= Q2, 1, ifelse(is.na(Q2), NA, 0))),
        freq = as.numeric(ifelse(!is.na(Q3) & count >= Q3, 1, ifelse(is.na(Q3), NA, 0)))
    ), .(pid, code)]
    
    melted <- melt(patterns, id.vars = c("pid", "code"))[!is.na(value)]
    melted[, code_type := paste(type, code, variable, sep = "_")]
    
    output <- dcast(melted, pid ~ code_type, value.var = "value")
    output[is.na(output)] <- 0
    
    output
}

#' Covariate assessment wrapper function
#'
#' Combines identify_candidates and assess_recurrence for backward compatibility.
#' Creates binary covariates based on frequency cut-offs (once, median, 75th percentile).
#'
#' @param dt Data table with patient and covariate information
#' @param id Column name for patient identifier
#' @param code Column name for codes
#' @param type Prefix for output (dx, px, rx)
#' @param rank Maximum number of covariates to include
#' @return Data table with recurrence assessment
#' @export
rec_assess <- function(dt, id, code, type, rank = Inf) {
    candidates_result <- identify_candidates(dt, id, code, type, n = ifelse(rank == Inf, 200, rank))
    assess_recurrence(candidates_result$data, "pid", "code", type, rank)
}


#' Create 2x2 table for bias estimation
#'
#' @param dt Data table with exposure and covariate
#' @param expo Column name for exposure
#' @param cova Column name for covariate
#' @return 2x2 contingency table
estBiasTable <- function(dt, expo, cova) {
    # Use .() method for better parallel compatibility
    temp <- dt[, .(count = .N), by = .(get(expo), get(cova))]
    # Get the actual column names and rename them
    col_names <- names(temp)
    setnames(temp, col_names[1:2], c("e", "c"))
    temp <- merge(temp, CJ(e = c(0, 1), c = c(0, 1)), by = c("e", "c"), all.y = TRUE)
    temp[is.na(count), count := 0]
    temp
}



#' Estimate apparent relative risk bias for a covariate
#'
#' @param hdpsCohort Dataset after recurrence assessment
#' @param cova Column name for covariate
#' @param expo Column name for exposure
#' @param outc Column name for outcome
#' @param correction Apply 0.1 correction for zero cells
#' @return Data table with bias estimates
estBias <- function(hdpsCohort, cova, expo, outc, correction = TRUE) {
    setDT(hdpsCohort)
    
    # Use .() method for better parallel compatibility
    e1 <- hdpsCohort[get(expo) == 1, .N]
    e0 <- hdpsCohort[get(expo) == 0, .N]
    d1 <- hdpsCohort[get(outc) == 1, .N]
    d0 <- hdpsCohort[get(outc) == 0, .N]
    n <- hdpsCohort[, .N]
    
    temp <- estBiasTable(hdpsCohort, expo, cova)
    c1 <- temp[c == 1, sum(count)]
    c0 <- n - c1
    e0c1 <- temp[e == 0 & c == 1, count]
    e1c1 <- temp[e == 1 & c == 1, count]
    e0c0 <- temp[e == 0 & c == 0, count]
    e1c0 <- temp[e == 1 & c == 0, count]

    temp <- estBiasTable(hdpsCohort, outc, cova)
    d0c1 <- temp[e == 0 & c == 1, count]
    d1c1 <- temp[e == 1 & c == 1, count]
    d0c0 <- temp[e == 0 & c == 0, count]
    d1c0 <- temp[e == 1 & c == 0, count]
    
    if (correction) {
        if (e0c1 == 0 | e1c1 == 0 | e0c0 == 0 | e1c0 == 0) {
            e0c1 <- e0c1 + 0.1
            e1c1 <- e1c1 + 0.1
            e0c0 <- e0c0 + 0.1
            e1c0 <- e1c0 + 0.1
        }
        if (d0c1 == 0 | d1c1 == 0 | d0c0 == 0 | d1c0 == 0) {
            d0c1 <- d0c1 + 0.1
            d1c1 <- d1c1 + 0.1
            d0c0 <- d0c0 + 0.1
            d1c0 <- d1c0 + 0.1
        }
    }

    pc1 <- e1c1 / e1
    pc0 <- ifelse(e0c1 / e0 == 0, 0.1, e0c1 / e0)
    rrCE <- ifelse(pc1 / pc0 == 0, NA, pc1 / pc0)
    rrCD <- ifelse((d1c1 / c1) / (d1c0 / c0) == 0, NA, (d1c1 / c1) / (d1c0 / c0))

    bias <- (pc1 * (rrCD - 1) + 1) / (pc0 * (rrCD - 1) + 1)
    absLogBias <- abs(log(bias))
    ce_strength <- abs(rrCE - 1)
    cd_strength <- abs(rrCD - 1)
    
    data.table(
        code = cova, e1, e0, d1, d0, c1, c0,
        e1c1, e0c1, e1c0, e0c0, d1c1, d0c1, d1c0, d0c0,
        pc1, pc0, rrCE, rrCD, bias, absLogBias, ce_strength, cd_strength
    )
}
#' Prioritize covariates using bias estimation
#'
#' @param dt Dataset after recurrence assessment
#' @param pid Column name for patient ID
#' @param expo Column name for exposure
#' @param outc Column name for outcome
#' @param correction Apply 0.1 correction for zero cells
#' @return Data table with bias estimates for all covariates
#' @export
prioritize <- function(dt, pid, expo, outc, correction = TRUE) {
    cova <- setdiff(colnames(dt), c(expo, outc, pid))
    
    rbindlist(pbapply::pblapply(cova, function(x) {
        estBias(dt, cova = x, expo = expo, outc = outc, correction = correction)
    }))
}

#' HDPS workflow for multiple domains
#'
#' @param data_list List of data.tables for different domains (dx, px, rx)
#' @param id_col Column name for patient ID
#' @param code_col Column name for codes
#' @param exposure_col Column name for exposure
#' @param outcome_col Column name for outcome
#' @param n_candidates Maximum number of candidates per domain
#' @param min_patients Minimum patients per covariate
#' @param parallel Use parallel processing
#' @param n_cores Number of cores for parallel processing
#' @return List containing results for each domain
#' @export
hdps_multi_domain <- function(data_list, id_col, code_col, exposure_col, outcome_col, 
                             n_candidates = 200, min_patients = 10, parallel = FALSE, n_cores = NULL) {
    
    if (!is.list(data_list)) {
        stop("data_list must be a list of data.tables")
    }
    
    if (!all(c("dx", "px", "rx") %in% names(data_list))) {
        warning("Expected domains: dx, px, rx. Some domains may be missing.")
    }
    
    results <- list()
    
    for (domain in names(data_list)) {
        cat("Processing domain:", domain, "\n")
        
        # Step 1: Identify candidates
        candidates <- identify_candidates(data_list[[domain]], id_col, code_col, domain, 
                                         n = n_candidates, min_patients = min_patients)
        
        # Step 2: Assess recurrence
        recurrence <- assess_recurrence(candidates$data, "pid", "code", domain)
        
        # Step 3: Prioritize (if we have exposure and outcome data)
        if (!is.null(exposure_col) && !is.null(outcome_col)) {
            # Merge with exposure/outcome data
            master_data <- merge(recurrence, 
                               data_list[[domain]][, c(id_col, exposure_col, outcome_col), with = FALSE], 
                               by.x = "pid", by.y = id_col, all.x = TRUE)
            
            prioritization <- prioritize(master_data, "pid", exposure_col, outcome_col, 
                                       parallel = parallel, n_cores = n_cores)
            
            results[[domain]] <- list(
                candidates = candidates,
                recurrence = recurrence,
                prioritization = prioritization
            )
        } else {
            results[[domain]] <- list(
                candidates = candidates,
                recurrence = recurrence
            )
        }
    }
    
    return(results)
}

#' Plot bias distribution for top covariates
#'
#' @param hdps_result Results from prioritize function
#' @param top_n Number of top covariates to display
#' @param interactive Whether to create interactive plots
#' @return ggplot or plotly object
#' @export
plot_bias_distribution <- function(hdps_result, top_n = 50, interactive = FALSE) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot2 package is required for plotting")
    }
    
    top_covariates <- hdps_result[order(absLogBias, decreasing = TRUE)][1:top_n]
    
    p <- ggplot2::ggplot(top_covariates, ggplot2::aes(x = reorder(code, absLogBias), y = absLogBias)) +
        ggplot2::geom_col(fill = "steelblue", alpha = 0.7) +
        ggplot2::coord_flip() +
        ggplot2::labs(
            title = paste("Top", top_n, "Covariates by Absolute Log Bias"),
            x = "Covariates",
            y = "Absolute Log Bias"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8))
    
    if (interactive && requireNamespace("plotly", quietly = TRUE)) {
        plotly::ggplotly(p)
    } else {
        p
    }
}

#' Plot covariate strength relationships
#'
#' @param hdps_result Results from prioritize function
#' @param interactive Whether to create interactive plots
#'
#' @return ggplot or plotly object
#' @export
#'
plot_covariate_strength <- function(hdps_result, interactive = FALSE) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot2 package is required for plotting")
    }
    
    p <- ggplot2::ggplot(hdps_result, ggplot2::aes(x = ce_strength, y = cd_strength, 
                                                  color = absLogBias, size = absLogBias)) +
        ggplot2::geom_point(alpha = 0.6) +
        ggplot2::scale_color_gradient(low = "blue", high = "red") +
        ggplot2::labs(
            title = "Covariate Strength Relationships",
            x = "Covariate-Exposure Association Strength",
            y = "Covariate-Outcome Association Strength",
            color = "Absolute Log Bias",
            size = "Absolute Log Bias"
        ) +
        ggplot2::theme_minimal()
    
    if (interactive && requireNamespace("plotly", quietly = TRUE)) {
        return(plotly::ggplotly(p))
    } else {
        return(p)
    }
}

#' Plot bias vs prevalence
#'
#' @param hdps_result Results from prioritize function
#' @param interactive Whether to create interactive plots
#'
#' @return ggplot or plotly object
#' @export
#'
plot_bias_vs_prevalence <- function(hdps_result, interactive = FALSE) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot2 package is required for plotting")
    }
    
    # Calculate prevalence
    hdps_result[, prevalence := (c1 + c0) / (e1 + e0)]
    
    p <- ggplot2::ggplot(hdps_result, ggplot2::aes(x = prevalence, y = absLogBias, 
                                                   color = ce_strength, size = cd_strength)) +
        ggplot2::geom_point(alpha = 0.6) +
        ggplot2::scale_color_gradient(low = "blue", high = "red") +
        ggplot2::labs(
            title = "Bias vs Covariate Prevalence",
            x = "Covariate Prevalence",
            y = "Absolute Log Bias",
            color = "CE Strength",
            size = "CD Strength"
        ) +
        ggplot2::theme_minimal()
    
    if (interactive && requireNamespace("plotly", quietly = TRUE)) {
        return(plotly::ggplotly(p))
    } else {
        return(p)
    }
}

#' Complete HDPS workflow wrapper
#'
#' @title Complete HDPS workflow
#' @param data Input data (data.frame, data.table, or list for multi-domain)
#' @param id_col Column name for patient ID
#' @param code_col Column name for codes
#' @param exposure_col Column name for exposure
#' @param outcome_col Column name for outcome
#' @param type_col Column name for domain type (if multi-domain)
#' @param n_candidates Maximum number of candidates per domain
#' @param min_patients Minimum patients per covariate
#' @param parallel Use parallel processing
#' @param n_cores Number of cores for parallel processing
#' @param correction Apply correction for rare outcomes
#'
#' @return Complete HDPS results
#' @export
#'
hdps_screen <- function(data, id_col, code_col, exposure_col, outcome_col, 
                       type_col = NULL, master_data = NULL, n_candidates = 200, min_patients = 10, 
                       correction = TRUE) {
    
    # Input validation
    if (missing(data) || is.null(data)) {
        stop("data must be provided")
    }
    
    if (missing(id_col) || missing(code_col)) {
        stop("id_col and code_col must be specified")
    }
    
    # Handle different input formats
    if (is.list(data) && !is.data.table(data) && !is.data.frame(data)) {
        # Multi-domain data (list of data.tables)
        if (is.null(type_col)) {
            stop("type_col must be specified for multi-domain data")
        }
        return(hdps_multi_domain(data, id_col, code_col, exposure_col, outcome_col, 
                                n_candidates, min_patients, parallel, n_cores))
    } else {
        # Single domain data
        if (!is.data.table(data)) {
            data <- as.data.table(data)
        }
        
        # Step 1: Identify candidates
        candidates <- identify_candidates(data, id_col, code_col, "cov", 
                                       n = n_candidates, min_patients = min_patients)
        
        # Step 2: Assess recurrence
        recurrence <- assess_recurrence(candidates$data, "pid", "code", "cov")
        
        # Step 3: Prioritize (if exposure and outcome provided)
        if (!missing(exposure_col) && !missing(outcome_col)) {
            # Merge with exposure/outcome data
            if (!is.null(master_data)) {
                # Use provided master data
                cohort_data <- merge(recurrence, master_data, by = "pid", all.x = TRUE)
            } else {
                # Use exposure/outcome columns from the same dataset
                cohort_data <- merge(recurrence, 
                                   data[, c(id_col, exposure_col, outcome_col), with = FALSE], 
                                   by.x = "pid", by.y = id_col, all.x = TRUE)
            }
            
            prioritization <- prioritize(cohort_data, "pid", exposure_col, outcome_col, 
                                       correction = correction)
            
            return(list(
                candidates = candidates,
                recurrence = recurrence,
                prioritization = prioritization
            ))
        } else {
            return(list(
                candidates = candidates,
                recurrence = recurrence
            ))
        }
    }
}

#' Flexible data input handler
#'
#' @param data Input data in various formats
#' @param format Input format: "long", "wide", "matrix"
#' @param id_col Column name for patient ID
#' @param code_col Column name for codes
#' @param type_col Column name for domain type
#' @param value_col Column name for values (for wide format)
#' @return Standardized data.table
#' @export
hdps_input <- function(data, format = c("long", "wide", "matrix"), 
                      id_col = "pid", code_col = "code", type_col = "type", 
                      value_col = "value") {
    
    format <- match.arg(format)
    
    if (!is.data.table(data)) {
        data <- as.data.table(data)
    }
    
    switch(format,
        "long" = data,
        "wide" = {
            if (missing(value_col)) {
                stop("value_col must be specified for wide format")
            }
            melt(data, id.vars = id_col, variable.name = code_col, value.name = value_col)
        },
        "matrix" = {
            if (!is.matrix(data)) {
                stop("data must be a matrix for matrix format")
            }
            rownames <- rownames(data) %||% 1:nrow(data)
            colnames <- colnames(data) %||% 1:ncol(data)
            
            data.table(
                pid = rep(rownames, ncol(data)),
                code = rep(colnames, each = nrow(data)),
                value = as.vector(data)
            )
        }
    )
}
