
#' Identify candidate empirical covariates based on prevalence
#'
#' @title identify candidate covariates
#' @param dt A database including the pid and covariates for assessment
#' @param id The column name of patient identification
#' @param code The column name of codes, eg. ICD-9, ICD-19, Drug Codes, or Lab results
#' @param type The prefix for output table, indicating type of data dimensions, eg. dx for diagnoses, px for procedures
#' @param n The maximum number of empirical candidate baseline covariates that should be returned within each domain
#' @param min_patients Minimum number of patients that should be present for each covariate to be selected
#'
#' @return A data.table with candidate covariates
#' @export
#'
identify_candidates <- function(dt, id, code, type, n = 200, min_patients = 10) {
    if (missing(dt) || !is.data.table(dt) && !is.data.frame(dt)) {
        stop("'dt' must be provided as a data.frame or data.table")
    }
    if (missing(id) || !is.character(id)) {
        stop("'id' must be the column name of identifier and provided as a character")
    }
    if (missing(code) || !is.character(code)) {
        stop("'code' must be the column name of codes and provided as a character")
    }
    if (missing(type) || !is.character(type)) {
        stop("'type' must be provided as a character")
    }
    
    dt <- copy(dt)
    setDT(dt)
    setnames(dt, c(id, code), c("pid", "code"))
    
    # Calculate prevalence for each code
    prevalence <- dt[, .(n_patients = length(unique(pid))), by = code]
    total_patients <- dt[, length(unique(pid))]
    prevalence[, prevalence := (n_patients / total_patients) * 100]
    prevalence[, prevalence_truncated := ifelse(prevalence > 50, 100 - prevalence, prevalence)]
    
    # Filter by minimum patients and select top n
    candidates <- prevalence[n_patients >= min_patients][order(prevalence_truncated, decreasing = TRUE)]
    candidates <- candidates[1:min(n, nrow(candidates))]
    
    # Filter original data to only include candidates
    result <- dt[code %in% candidates$code]
    result[, code := paste0(type, "_", code)]
    
    return(list(
        candidates = candidates,
        data = result,
        patient_ids = unique(dt$pid)
    ))
}

#' Assess recurrence of covariates
#'
#' @title assess recurrence
#' @param dt A database including the pid and covariates for assessment
#' @param id The column name of patient identification
#' @param code The column name of codes, eg. ICD-9, ICD-19, Drug Codes, or Lab results
#' @param type The prefix for output table, indicating type of data dimensions, eg. dx for diagnoses, px for procedures
#' @param rank The desired number of covariates to include in the model. eg. 500, 1000, Inf, etc
#'
#' @return A data.table including the recurrent assessment of the codes
#' @export
#'
assess_recurrence <- function(dt, id, code, type, rank = Inf) {
    if (missing(dt) || !is.data.table(dt) || !is.data.frame(dt)) {
        stop("'dt' must be provided as a data.frame or data.table")
    }
    if (missing(id) || !is.character(id)) {
        stop("'id' must be the column name of identifier and provided as a character")
    }
    if (missing(code) || !is.character(code)) {
        stop("'code' must be the column name of codes and provided as a character")
    }
    if (missing(type) || !is.character(type)) {
        stop("'type' must be provided as a character")
    }
    if (!is.numeric(rank) || !is.infinite(rank)) {
        stop("'rank' must be provided as a number or Inf")
    }
    dt <- copy(dt)
    setDT(dt)
    setnames(dt,c(id,code),c("pid","code"))
    # catch
    if(!rank==Inf){
        count_cutoff <- dt[,.(count=.N),.(pid,code)][order(count,decreasing = T)][rank,count]
    }else(
        count_cutoff <- Inf
    )

    output <- dcast(
        melt(merge(dt[,.(count=.N),.(pid,code)][count <= count_cutoff],
                   dt[,.(count=.N),.(pid,code)][,.(Q1=quantile(count,0.25),
                                                   Q2=quantile(count,0.5),
                                                   Q3=quantile(count,0.75)),
                                                code
                                                ][,.(code,Q1,Q2=ifelse(Q1==Q2,NA,Q2),
                                                     Q3=ifelse(Q2==Q3,NA,Q3))],
                   by="code",
                   all.x=T)[,.(once=ifelse(count>=1,1,0),
                                 spor=ifelse(!is.na(Q2) & count>=Q2,1,ifelse(is.na(Q2),as.numeric(NA),0)),
                                 freq=ifelse(!is.na(Q3) & count>=Q3,1,ifelse(is.na(Q3),as.numeric(NA),0))),
                              .(pid,code)],
             id.vars = c("pid","code"))[!is.na(value)][,.(pid,code_type=paste(type,code,variable,sep="_"),count=value)]
        ,pid~code_type)
    output[is.na(output)] <- 0
    return(output)
}

#' The recurrence of features is assessed in a pre-exposure period, creating binary covariates based on a set of frequency-based cut-offs.The standard implementation of the HDPS defines three indicators for each patient capturing whether a feature was recorded:larger than once,larger than the median, and larger the75th percentile.
#'
#' @title covariate assessment (wrapper function)
#' @param dt A database including the pid and covariates for assessment
#' @param id The column name of patient identification
#' @param code The column name of codes, eg. ICD-9, ICD-19, Drug Codes, or Lab results
#' @param type The prefix for output table, indicating type of data dimensions, eg. dx for diagnoses, px for procedures
#' @param rank The desired number of covariates to include in the model. eg. 500, 1000, Inf, etc
#'
#' @return A data.table including the recurrent assessment of the codes
#' @export
#'
rec_assess <- function(dt, id, code, type, rank = Inf) {
    # This is a wrapper function that combines identify_candidates and assess_recurrence
    # for backward compatibility
    candidates_result <- identify_candidates(dt, id, code, type, n = ifelse(rank == Inf, 200, rank))
    recurrence_result <- assess_recurrence(candidates_result$data, "pid", "code", type, rank)
    return(recurrence_result)
}


#' Estimate the apparent relative risk for one covariate
#'
#' @param dt A dataset in data.table format, including
#' @param expo The column name for exposure or outcome
#' @param cova The column name for covariates
#' @param ...
#'
#' @return A data.table including the apparent relative risk for one covariate
#'
estBiasTable<-function(dt,expo,cova,...){
    temp<-dt[, .(count = .N), by = c(expo,cova)]
    setnames(temp,c(expo,cova),c("e","c"))
    temp <- merge(temp,
                  CJ(e=c(0,1),c=c(0,1)),by=c("e","c"),
                  all.y=T)
    temp[is.na(count), count := 0]
    return(temp)
}



#' ARR Bias estimation for all covariates
#'
#' @param hdpsCohort The dataset after recurrent assesemnt in data.table format
#' @param cova Column name of the desire covariates
#' @param expo Column name of the desire exposure
#' @param outc Column name of the desire outcome
#' @param correction When the outcome is rare, one of the 2 by 2 table could be zero. In this case, the algorithm will add 0.1 to the cell.
#' @param ...
#'
#' @return A data.table including the apparent relative risk for one covariates
#'
estBias <- function(hdpsCohort,cova,expo,outc,correction=TRUE,...){
    setDT(hdpsCohort)
    e1 <- hdpsCohort[get(expo)==1,.N]
    e0 <- hdpsCohort[get(expo)==0,.N]
    d1 <- hdpsCohort[get(outc)==1,.N]
    d0 <- hdpsCohort[get(outc)==0,.N]
    n <- hdpsCohort[,.N]

    temp <- estBiasTable(hdpsCohort,expo,cova)
    c1 <- temp[c==1,sum(count)]
    c0 <- n - c1
    e0c1 <- temp[e == 0 & c == 1, count]
    e1c1 <- temp[e == 1 & c == 1, count]
    e0c0 <- temp[e == 0 & c == 0, count]
    e1c0 <- temp[e == 1 & c == 0, count]

    temp <- estBiasTable(hdpsCohort,outc,cova)
    d0c1 <- temp[e == 0 & c == 1, count]
    d1c1 <- temp[e == 1 & c == 1, count]
    d0c0 <- temp[e == 0 & c == 0, count]
    d1c0 <- temp[e == 1 & c == 0, count]
    if(correction==TRUE){
        if(e0c1 ==0|e1c1==0|e0c0==0|e1c0==0){
            e0c1 <- e0c1 + 0.1
            e1c1 <- e1c1 + 0.1
            e0c0 <- e0c0 + 0.1
            e1c0 <- e1c0 + 0.1
        }
        if(d0c1==0|d1c1==0|d0c0==0|d1c0 == 0){
            d0c1 <- d0c1 + 0.1
            d1c1 <- d1c1 + 0.1
            d0c0 <- d0c0 + 0.1
            d1c0 <- d1c0 + 0.1
        }
    }

    pc1 <- e1c1 / e1
    pc0 <- ifelse(e0c1 / e0==0,0.1,e0c1 / e0)
    rrCE <- ifelse(pc1/pc0==0,NA,pc1/pc0)

    rrCD <- ifelse((d1c1/c1)/(d1c0/c0)==0,NA,(d1c1/c1)/(d1c0/c0))

    bias <- (pc1*(rrCD - 1) + 1)/ (pc0*(rrCD - 1) + 1)
    absLogBias <- abs(log(bias))

    ce_strength <- abs(rrCE-1)
    cd_strength <- abs(rrCD-1)
    temp <- data.table(code = paste(cova),
                       e1,e0,d1,d0,c1,c0,
                       e1c1,e0c1,e1c0,e0c0,
                       d1c1,d0c1,d1c0,d0c0,
                       pc1,pc0,
                       rrCE,rrCD,
                       bias,absLogBias,
                       ce_strength,cd_strength)
    return(temp)
}
# estBias(hdpsCohort,"dx_335.20_once","exposed","outcome")
# estBias(hdpsCohort,"dx_000_freq","exposed","outcome")


#' Prioritise the large pool of covariates generated in the previous step is prioritised. This is typically achieved using the Bross formula, which uses univariate associations of covariates with treatment and outcome, to identify those with the highest potential tobias the treatment-outcome relationship
#' @title Calculate the apparent relative risk for all covariates
#'
#' @param dt Dataset after reccurent assessment in data.table format
#' @param pid The column name for patient ID
#' @param expo The column name for exposure
#' @param outc The column name for outcome
#' @param correction When the outcome is rare, one of the 2 by 2 table could be zero. In this case, the algorithm will add 0.1 to the cell.
#'
#' @return A data.table including the apparent relative risk for all covariates
#' @export
#'
prioritize <- function(dt, pid, expo, outc, correction = TRUE, parallel = FALSE, n_cores = NULL) {
    cova <- setdiff(colnames(dt), c(expo, outc, pid))
    
    if (parallel) {
        if (is.null(n_cores)) {
            n_cores <- min(4, parallel::detectCores() - 1)
        }
        
        # Set up parallel processing
        cl <- parallel::makeCluster(n_cores)
        parallel::clusterExport(cl, c("estBias", "estBiasTable"), envir = environment())
        
        # Apply function in parallel
        result <- parallel::parLapply(cl, cova, function(x) {
            estBias(dt, cova = x, expo = expo, outc = outc, correction = correction)
        })
        
        parallel::stopCluster(cl)
        return(rbindlist(result))
    } else {
        # Original sequential processing with progress bar
    return(rbindlist(pbapply::pblapply(cova,
                                       function(x) estBias(dt,
                                                           cova = x,
                                                               expo = expo,
                                                               outc = outc,
                                                           correction = correction))))
}
}

#' HDPS workflow for multiple domains
#'
#' @title HDPS multi-domain workflow
#' @param data_list List of data.tables for different domains (dx, px, rx)
#' @param id_col Column name for patient ID
#' @param code_col Column name for codes
#' @param exposure_col Column name for exposure
#' @param outcome_col Column name for outcome
#' @param n_candidates Maximum number of candidates per domain
#' @param min_patients Minimum patients per covariate
#' @param parallel Use parallel processing
#' @param n_cores Number of cores for parallel processing
#'
#' @return List containing results for each domain
#' @export
#'
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

#' Enhanced visualization functions for HDPS results
#'
#' @title HDPS visualization functions
#' @param hdps_result Results from prioritize function
#' @param top_n Number of top covariates to display
#' @param interactive Whether to create interactive plots
#'
#' @return ggplot or plotly object
#' @export
#'
plot_bias_distribution <- function(hdps_result, top_n = 50, interactive = FALSE) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot2 package is required for plotting")
    }
    
    # Get top n covariates by absolute log bias
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
        return(plotly::ggplotly(p))
    } else {
        return(p)
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
                       type_col = NULL, n_candidates = 200, min_patients = 10, 
                       parallel = FALSE, n_cores = NULL, correction = TRUE) {
    
    # Input validation
    if (missing(data) || is.null(data)) {
        stop("data must be provided")
    }
    
    if (missing(id_col) || missing(code_col)) {
        stop("id_col and code_col must be specified")
    }
    
    # Handle different input formats
    if (is.list(data)) {
        # Multi-domain data
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
            master_data <- merge(recurrence, 
                               data[, c(id_col, exposure_col, outcome_col), with = FALSE], 
                               by.x = "pid", by.y = id_col, all.x = TRUE)
            
            prioritization <- prioritize(master_data, "pid", exposure_col, outcome_col, 
                                       correction = correction, parallel = parallel, n_cores = n_cores)
            
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
#' @title Flexible data input
#' @param data Input data in various formats
#' @param format Input format: "long", "wide", "matrix"
#' @param id_col Column name for patient ID
#' @param code_col Column name for codes
#' @param type_col Column name for domain type
#' @param value_col Column name for values (for wide format)
#'
#' @return Standardized data.table
#' @export
#'
hdps_input <- function(data, format = c("long", "wide", "matrix"), 
                      id_col = "pid", code_col = "code", type_col = "type", 
                      value_col = "value") {
    
    format <- match.arg(format)
    
    if (!is.data.table(data)) {
        data <- as.data.table(data)
    }
    
    switch(format,
        "long" = {
            # Data is already in long format
            return(data)
        },
        "wide" = {
            # Convert from wide to long format
            if (missing(value_col)) {
                stop("value_col must be specified for wide format")
            }
            melted <- melt(data, id.vars = id_col, variable.name = code_col, value.name = value_col)
            return(melted)
        },
        "matrix" = {
            # Convert matrix to long format
            if (is.matrix(data)) {
                rownames <- rownames(data)
                colnames <- colnames(data)
                if (is.null(rownames)) rownames <- 1:nrow(data)
                if (is.null(colnames)) colnames <- 1:ncol(data)
                
                result <- data.table(
                    pid = rep(rownames, ncol(data)),
                    code = rep(colnames, each = nrow(data)),
                    value = as.vector(data)
                )
                return(result)
            } else {
                stop("data must be a matrix for matrix format")
            }
        }
    )
}
