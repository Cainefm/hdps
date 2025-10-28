
#' @importFrom data.table ":=" ".N" as.data.table data.table dcast is.data.table melt rbindlist setDT setnames uniqueN
#' @importFrom stats quantile reorder
#' @importFrom utils write.csv
#' @importFrom pbapply pblapply

# Optimized batch bias estimation function
estBiasBatch <- function(hdpsCohort, cova_list, expo, outc, correction = TRUE) {
    setDT(hdpsCohort)
    
    # Pre-calculate totals once for all covariates
    e1 <- sum(hdpsCohort[[expo]] == 1, na.rm = TRUE)
    e0 <- sum(hdpsCohort[[expo]] == 0, na.rm = TRUE)
    d1 <- sum(hdpsCohort[[outc]] == 1, na.rm = TRUE)
    d0 <- sum(hdpsCohort[[outc]] == 0, na.rm = TRUE)
    n <- nrow(hdpsCohort)
    
    # Process all covariates in a single data.table operation
    results <- rbindlist(lapply(cova_list, function(cova) {
        # Single pass contingency table calculation
        expo_outc_counts <- hdpsCohort[, .(
            e0c0 = sum((get(expo) == 0) & (get(cova) == 0), na.rm = TRUE),
            e0c1 = sum((get(expo) == 0) & (get(cova) == 1), na.rm = TRUE),
            e1c0 = sum((get(expo) == 1) & (get(cova) == 0), na.rm = TRUE),
            e1c1 = sum((get(expo) == 1) & (get(cova) == 1), na.rm = TRUE),
            d0c0 = sum((get(outc) == 0) & (get(cova) == 0), na.rm = TRUE),
            d0c1 = sum((get(outc) == 0) & (get(cova) == 1), na.rm = TRUE),
            d1c0 = sum((get(outc) == 1) & (get(cova) == 0), na.rm = TRUE),
            d1c1 = sum((get(outc) == 1) & (get(cova) == 1), na.rm = TRUE)
        )]
        
        # Extract values
        e0c0 <- expo_outc_counts$e0c0
        e0c1 <- expo_outc_counts$e0c1
        e1c0 <- expo_outc_counts$e1c0
        e1c1 <- expo_outc_counts$e1c1
        d0c0 <- expo_outc_counts$d0c0
        d0c1 <- expo_outc_counts$d0c1
        d1c0 <- expo_outc_counts$d1c0
        d1c1 <- expo_outc_counts$d1c1
        
        c1 <- e1c1 + e0c1
        c0 <- e1c0 + e0c0
        
        # Apply correction if needed
        if (correction) {
            zero_expo <- (e0c1 == 0 | e1c1 == 0 | e0c0 == 0 | e1c0 == 0)
            zero_outc <- (d0c1 == 0 | d1c1 == 0 | d0c0 == 0 | d1c0 == 0)
            
            if (zero_expo) {
                e0c1 <- e0c1 + 0.1
                e1c1 <- e1c1 + 0.1
                e0c0 <- e0c0 + 0.1
                e1c0 <- e1c0 + 0.1
            }
            if (zero_outc) {
                d0c1 <- d0c1 + 0.1
                d1c1 <- d1c1 + 0.1
                d0c0 <- d0c0 + 0.1
                d1c0 <- d1c0 + 0.1
            }
        }
        
        # Calculate metrics
        pc1 <- c1 / n
        pc0 <- c0 / n
        rrCE <- ifelse(c1 > 0 & c0 > 0, (e1c1 / c1) / (e1c0 / c0), NA_real_)
        rrCD <- ifelse(c1 > 0 & c0 > 0, (d1c1 / c1) / (d1c0 / c0), NA_real_)
        
        bias <- ifelse(!is.na(rrCD), (pc1 * (rrCD - 1) + 1) / (pc0 * (rrCD - 1) + 1), NA_real_)
        absLogBias <- ifelse(!is.na(bias) & bias > 0, abs(log(bias)), NA_real_)
        ce_strength <- ifelse(!is.na(rrCE), abs(rrCE - 1), NA_real_)
        cd_strength <- ifelse(!is.na(rrCD), abs(rrCD - 1), NA_real_)
        
        data.table(
            code = cova, e1, e0, d1, d0, c1, c0,
            e1c1, e0c1, e1c0, e0c0, d1c1, d0c1, d1c0, d0c0,
            pc1, pc0, rrCE, rrCD, bias, absLogBias, ce_strength, cd_strength
        )
    }))
    
    results
}

# Suppress R CMD check warnings for data.table variables
utils::globalVariables(c(
  ".", ".N", "pid", "exposure", "outcome", "count", "Q1", "Q2", "Q3", 
  "once", "spor", "freq", "value", "code_type", "variable", "n_patients",
  "prevalence_truncated", "absLogBias", "code", "prevalence", "c1", "c0", 
  "e1", "e0", "ce_strength", "cd_strength"
))

#' Identify candidate covariates based on prevalence
#'
#' @param dt Data table with patient and covariate information
#' @param id Column name for patient identifier
#' @param code Column name for codes
#' @param type Prefix for output (dx, px, rx)
#' @param n Maximum number of candidates to return
#' @param min_patients Minimum number of patients required for a covariate to be considered (filters out rare codes with insufficient sample size)
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
    
    if (!is.data.table(dt)) dt <- as.data.table(dt)
    if (!id %in% names(dt)) stop("Column '", id, "' not found")
    if (!code %in% names(dt)) stop("Column '", code, "' not found")
    
    # Check if data is empty or has no valid combinations
    if (nrow(dt) == 0) {
        return(list(candidates = data.table(), data = dt[0], patient_ids = character(0)))
    }
    
    # Check if columns have any non-NA values
    id_col_data <- dt[[id]]
    code_col_data <- dt[[code]]
    if (sum(!is.na(id_col_data) & !is.na(code_col_data)) == 0) {
        return(list(candidates = data.table(), data = dt[0], patient_ids = character(0)))
    }
    
    # Use a minimal copy approach - only copy the necessary columns
    # This is much more efficient than copying the entire dataset
    
    # Create a minimal working dataset with only needed columns
    work_dt <- dt[, c(id, code), with = FALSE]
    setnames(work_dt, c(id, code), c("pid", "code"))
    
    # Ensure pid is character for consistent merging
    work_dt[, pid := as.character(pid)]
    
    # Calculate prevalence
    prevalence <- work_dt[, .(n_patients = uniqueN(pid)), by = code]
    total_patients <- work_dt[, uniqueN(pid)]
    prevalence[, prevalence := (n_patients / total_patients) * 100]
    prevalence[, prevalence_truncated := ifelse(prevalence > 50, 100 - prevalence, prevalence)]
    
    candidates <- prevalence[n_patients >= min_patients][order(prevalence_truncated, decreasing = TRUE)]
    candidates <- candidates[1:min(n, nrow(candidates))]
    
    # Filter data and create result with renamed columns
    result <- work_dt[code %in% candidates$code]
    result[, code := paste0(type, "_", code)]
    
    list(candidates = candidates, data = result, patient_ids = unique(dt[[id]]))
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
    
    if (!is.data.table(dt)) dt <- as.data.table(dt)
    if (!id %in% names(dt)) stop("Column '", id, "' not found")
    if (!code %in% names(dt)) stop("Column '", code, "' not found")
    
    # Check if data is empty or has no valid combinations
    if (nrow(dt) == 0) {
        return(data.table(pid = character(0)))
    }
    
    # Check if columns have any non-NA values  
    id_col_data <- dt[[id]]
    code_col_data <- dt[[code]]
    if (sum(!is.na(id_col_data) & !is.na(code_col_data)) == 0) {
        return(data.table(pid = character(0)))
    }
    
    # Use a minimal copy approach - only copy the necessary columns
    # This is much more efficient than copying the entire dataset
    
    # Create a minimal working dataset with only needed columns
    work_dt <- dt[, c(id, code), with = FALSE]
    setnames(work_dt, c(id, code), c("pid", "code"))
    
    # Ensure pid is character for consistent merging
    work_dt[, pid := as.character(pid)]
    
    # Calculate count cutoff based on rank
    count_cutoff <- if (rank == Inf) Inf else {
        work_dt[, .(count = .N), .(pid, code)][order(count, decreasing = TRUE)][rank, count]
    }
    
    # Calculate counts
    counts <- work_dt[, .(count = .N), .(pid, code)][count <= count_cutoff]
    
    quantiles <- counts[, .(
        Q1 = quantile(count, 0.25),
        Q2 = quantile(count, 0.5),
        Q3 = quantile(count, 0.75)
    ), code]
    quantiles[, Q2 := ifelse(Q1 == Q2, NA, Q2)]
    quantiles[, Q3 := ifelse(Q2 == Q3, NA, Q3)]
    
    merged <- counts[quantiles, on = "code"]
    patterns <- merged[, .(
        once = as.numeric(count >= 1),
        spor = as.numeric(!is.na(Q2) & count >= Q2),
        freq = as.numeric(!is.na(Q3) & count >= Q3)
    ), .(pid, code)]
    
    patterns_long <- patterns[, .(pid, code, variable = c("once", "spor", "freq"), 
                                  value = c(once, spor, freq))]
    patterns_long <- patterns_long[!is.na(value)]
    patterns_long[, code_type := paste(type, code, variable, sep = "_")]
    
    output <- dcast(patterns_long, pid ~ code_type, value.var = "value", fill = 0, fun.aggregate = max)
    
    output
}

#' Covariate assessment wrapper function
#'
#' Combines identify_candidates and assess_recurrence for backward compatibility.





#' Estimate apparent relative risk bias for a covariate
#'
#' @param hdpsCohort Dataset after recurrence assessment
#' @param cova Column name for covariate
#' @param expo Column name for exposure
#' @param outc Column name for outcome
#' @param correction Apply 0.1 correction for zero cells
#' @return Data table with bias estimates
#' @export
estBias <- function(hdpsCohort, cova, expo, outc, correction = TRUE) {
    setDT(hdpsCohort)
    
    # Pre-calculate totals for efficiency (vectorized)
    e1 <- sum(hdpsCohort[[expo]] == 1, na.rm = TRUE)
    e0 <- sum(hdpsCohort[[expo]] == 0, na.rm = TRUE)
    d1 <- sum(hdpsCohort[[outc]] == 1, na.rm = TRUE)
    d0 <- sum(hdpsCohort[[outc]] == 0, na.rm = TRUE)
    n <- nrow(hdpsCohort)
    
    # OPTIMIZED: Use data.table aggregation instead of table() - much faster
    # Single pass to get all contingency table counts
    expo_outc_counts <- hdpsCohort[, .(
        e0c0 = sum((get(expo) == 0) & (get(cova) == 0), na.rm = TRUE),
        e0c1 = sum((get(expo) == 0) & (get(cova) == 1), na.rm = TRUE),
        e1c0 = sum((get(expo) == 1) & (get(cova) == 0), na.rm = TRUE),
        e1c1 = sum((get(expo) == 1) & (get(cova) == 1), na.rm = TRUE),
        d0c0 = sum((get(outc) == 0) & (get(cova) == 0), na.rm = TRUE),
        d0c1 = sum((get(outc) == 0) & (get(cova) == 1), na.rm = TRUE),
        d1c0 = sum((get(outc) == 1) & (get(cova) == 0), na.rm = TRUE),
        d1c1 = sum((get(outc) == 1) & (get(cova) == 1), na.rm = TRUE)
    )]
    
    # Extract values (single row, so we can use direct indexing)
    e0c0 <- expo_outc_counts$e0c0
    e0c1 <- expo_outc_counts$e0c1
    e1c0 <- expo_outc_counts$e1c0
    e1c1 <- expo_outc_counts$e1c1
    d0c0 <- expo_outc_counts$d0c0
    d0c1 <- expo_outc_counts$d0c1
    d1c0 <- expo_outc_counts$d1c0
    d1c1 <- expo_outc_counts$d1c1
    
    c1 <- e1c1 + e0c1
    c0 <- e1c0 + e0c0
    
    # Apply correction if needed (vectorized)
    if (correction) {
        # Check for zero cells and apply correction
        zero_expo <- (e0c1 == 0 | e1c1 == 0 | e0c0 == 0 | e1c0 == 0)
        zero_outc <- (d0c1 == 0 | d1c1 == 0 | d0c0 == 0 | d1c0 == 0)
        
        if (zero_expo) {
            e0c1 <- e0c1 + 0.1
            e1c1 <- e1c1 + 0.1
            e0c0 <- e0c0 + 0.1
            e1c0 <- e1c0 + 0.1
        }
        if (zero_outc) {
            d0c1 <- d0c1 + 0.1
            d1c1 <- d1c1 + 0.1
            d0c0 <- d0c0 + 0.1
            d1c0 <- d1c0 + 0.1
        }
    }
    
    # Calculate proportions and ratios (vectorized)
    pc1 <- c1 / n
    pc0 <- c0 / n
    
    # Avoid division by zero with safer calculations
    rrCE <- ifelse(c1 > 0 & c0 > 0, (e1c1 / c1) / (e1c0 / c0), NA_real_)
    rrCD <- ifelse(c1 > 0 & c0 > 0, (d1c1 / c1) / (d1c0 / c0), NA_real_)
    
    # Calculate bias and related metrics
    bias <- ifelse(!is.na(rrCD), (pc1 * (rrCD - 1) + 1) / (pc0 * (rrCD - 1) + 1), NA_real_)
    absLogBias <- ifelse(!is.na(bias) & bias > 0, abs(log(bias)), NA_real_)
    ce_strength <- ifelse(!is.na(rrCE), abs(rrCE - 1), NA_real_)
    cd_strength <- ifelse(!is.na(rrCD), abs(rrCD - 1), NA_real_)
    
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
#' @param n_cores Number of cores for parallel processing (NULL for auto-detection)
#' @param batch_size Batch size for parallel processing
#' @param progress Show progress bar
#' @return Data table with bias estimates for all covariates
#' @export
prioritize <- function(dt, pid, expo, outc, correction = TRUE, n_cores = NULL, 
                      batch_size = 50, progress = TRUE) {
    cova <- setdiff(colnames(dt), c(expo, outc, pid))
    
    # Auto-detect optimal number of cores
    if (is.null(n_cores)) {
        n_cores <- min(parallel::detectCores() - 1, 4)
        n_cores <- max(1, n_cores)  # At least 1 core
    }
    
    # Pre-calculate common statistics once
    stats <- list(
        e1 = sum(dt[[expo]] == 1, na.rm = TRUE),
        e0 = sum(dt[[expo]] == 0, na.rm = TRUE),
        d1 = sum(dt[[outc]] == 1, na.rm = TRUE),
        d0 = sum(dt[[outc]] == 0, na.rm = TRUE),
        n = nrow(dt)
    )
    
    # OPTIMIZED: Use parallel processing for large datasets
    if (n_cores > 1 && length(cova) > 10) {
        # Set up parallel cluster with optimized settings
        cl <- parallel::makeCluster(n_cores, type = "PSOCK")
        
        # Export necessary functions and data (minimal set)
        parallel::clusterExport(cl, c("estBias"), envir = environment())
        
        # Pre-calculate common statistics once to avoid recalculation
        stats <- list(
            e1 = sum(dt[[expo]] == 1, na.rm = TRUE),
            e0 = sum(dt[[expo]] == 0, na.rm = TRUE),
            d1 = sum(dt[[outc]] == 1, na.rm = TRUE),
            d0 = sum(dt[[outc]] == 0, na.rm = TRUE),
            n = nrow(dt)
        )
        parallel::clusterExport(cl, "stats", envir = environment())
        
        # Process in optimized batches for memory efficiency
        results <- list()
        n_batches <- ceiling(length(cova) / batch_size)
        
        # OPTIMIZED: Use batch processing for better performance
        if (progress && requireNamespace("pbapply", quietly = TRUE)) {
            batch_results <- pbapply::pblapply(seq_len(n_batches), function(i) {
                start_idx <- (i - 1) * batch_size + 1
                end_idx <- min(i * batch_size, length(cova))
                batch_cova <- cova[start_idx:end_idx]
                
                # Use optimized batch function for better performance
                estBiasBatch(dt, batch_cova, expo, outc, correction)
            }, cl = NULL)
            
            # Combine all batch results
            rbindlist(batch_results)
        } else {
            # Standard processing without progress bar
            batch_results <- lapply(seq_len(n_batches), function(i) {
                start_idx <- (i - 1) * batch_size + 1
                end_idx <- min(i * batch_size, length(cova))
                batch_cova <- cova[start_idx:end_idx]
                
                # Use optimized batch function
                estBiasBatch(dt, batch_cova, expo, outc, correction)
            })
            
            # Combine all batch results
            rbindlist(batch_results)
        }
        
        parallel::stopCluster(cl)
        
    } else {
        # OPTIMIZED: Sequential processing for small datasets or single core
        n_batches <- ceiling(length(cova) / batch_size)
        
        if (progress && length(cova) > 5) {
            # Use batch processing even for sequential execution
            batch_results <- pbapply::pblapply(seq_len(n_batches), function(i) {
                start_idx <- (i - 1) * batch_size + 1
                end_idx <- min(i * batch_size, length(cova))
                batch_cova <- cova[start_idx:end_idx]
                
                estBiasBatch(dt, batch_cova, expo, outc, correction)
            }, cl = NULL)
            
            rbindlist(batch_results)
        } else {
            # Use optimized batch function for better performance
            estBiasBatch(dt, cova, expo, outc, correction)
        }
    }
}


#' Plot bias distribution for top covariates
#'
#' @param hdps_result Results from prioritize function
#' @param top_n Number of top covariates to display
#' @return ggplot object
#' @export
plot_bias_distribution <- function(hdps_result, top_n = 50) {
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
    
    return(p)
}

#' Plot covariate strength relationships
#'
#' @param hdps_result Results from prioritize function
#' @return ggplot object
#' @export
#'
plot_covariate_strength <- function(hdps_result) {
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
    
    return(p)
}

#' Plot bias vs prevalence
#'
#' @param hdps_result Results from prioritize function
#' @return ggplot object
#' @export
#'
plot_bias_vs_prevalence <- function(hdps_result) {
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
    
    return(p)
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
#' @param master_data Master dataset with exposure and outcome (if separate from data)
#' @param n_candidates Maximum number of candidates per domain
#' @param min_patients Minimum number of patients required for a covariate to be considered (filters out rare codes with insufficient sample size)
#' @param correction Apply correction for rare outcomes
#' @param n_cores Number of cores for parallel processing (NULL for auto-detection)
#' @param batch_size Batch size for parallel processing
#' @param progress Show progress bar
#'
#' @return Complete HDPS results
#' @export
#'
hdps <- function(data, id_col, code_col, exposure_col, outcome_col, 
                type_col = NULL, master_data = NULL, n_candidates = 200, min_patients = 10, 
                correction = TRUE, n_cores = NULL, batch_size = 50, progress = TRUE) {
    
    # Input validation
    if (missing(data) || is.null(data)) {
        stop("data must be provided")
    }
    
    if (missing(id_col) || missing(code_col)) {
        stop("id_col and code_col must be specified")
    }
    
    # Convert to data.table if needed
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
            # Prepare exposure/outcome data
            if (!is.null(master_data)) {
                # Use provided master data - avoid copy() by using column references
                cohort_data <- as.data.table(master_data)
                if (!id_col %in% names(cohort_data)) {
                    stop("Column '", id_col, "' not found in master_data")
                }
                
                # Create new data.table with renamed columns instead of modifying original
                if (id_col == "pid") {
                    cohort_data <- cohort_data[, list(pid = get(id_col), 
                                                     exposure = get(exposure_col), 
                                                     outcome = get(outcome_col))]
                } else {
                    cohort_data <- cohort_data[, list(pid = get(id_col), 
                                                     exposure = get(exposure_col), 
                                                     outcome = get(outcome_col))]
                }
            } else {
                # Use exposure/outcome from same dataset
                cohort_data <- data[, list(pid = get(id_col), 
                                         exposure = get(exposure_col), 
                                         outcome = get(outcome_col))]
            }
            
            # Standardize data types and merge
            cohort_data[, pid := as.character(pid)]
            cohort_data[, exposure := as.numeric(exposure)]
            cohort_data[, outcome := as.numeric(outcome)]
            recurrence[, pid := as.character(pid)]
            cohort_data <- merge(recurrence, cohort_data, by = "pid", all.x = TRUE)
            
            prioritization <- prioritize(cohort_data, "pid", "exposure", "outcome", 
                                       correction = correction, n_cores = n_cores, 
                                       batch_size = batch_size, progress = progress)
            
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

