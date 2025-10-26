## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 6
)

## ----small-datasets-----------------------------------------------------------
library(hdps)

# Load example data
data(dx)
data(master)

# For small datasets, use single-core processing
result <- hdps(
  data = dx,
  id_col = "pid",
  code_col = "icd9code",
  exposure_col = "exposure",
  outcome_col = "outcome",
  master_data = master,
  n_candidates = 50,
  min_patients = 5,
  n_cores = 1  # Single core is sufficient
)

## ----medium-datasets----------------------------------------------------------
# For medium datasets, use moderate parallel processing
result <- hdps(
  data = dx,
  id_col = "pid",
  code_col = "icd9code",
  exposure_col = "exposure",
  outcome_col = "outcome",
  master_data = master,
  n_candidates = 100,
  min_patients = 10,
  n_cores = 1  # Use single core for vignette stability
)

## ----large-datasets-----------------------------------------------------------
# For large datasets, use full parallel processing
result <- hdps(
  data = dx,
  id_col = "pid",
  code_col = "icd9code",
  exposure_col = "exposure",
  outcome_col = "outcome",
  master_data = master,
  n_candidates = 200,
  min_patients = 10,
  n_cores = 1,  # Use single core for vignette stability
  progress = TRUE  # Enable progress tracking
)

## ----memory-optimization------------------------------------------------------
# Convert to data.table for better performance
if (!is.data.table(dx)) {
  dx <- as.data.table(dx)
}

# Remove unnecessary columns before processing
essential_cols <- c("pid", "icd9code")
dx <- dx[, essential_cols, with = FALSE]

# Process in chunks for very large datasets
chunk_size <- 50000
if (nrow(dx) > chunk_size) {
  # Process in chunks
  chunks <- split(1:nrow(dx), 
                  ceiling(seq_along(1:nrow(dx)) / chunk_size))
  
  results <- lapply(chunks, function(chunk_idx) {
    chunk_data <- dx[chunk_idx]
    hdps(chunk_data, "pid", "icd9code", 
         "exposure", "outcome", master_data = master, n_cores = 1)
  })
}

## ----core-tuning--------------------------------------------------------------
# Determine optimal core count
available_cores <- parallel::detectCores()
optimal_cores <- min(available_cores - 1, 8)  # Leave 1 core free, max 8

cat("Available cores:", available_cores, "\n")
cat("Recommended cores:", optimal_cores, "\n")

## ----batch-tuning-------------------------------------------------------------
# Batch size depends on dataset size and available memory
n_covariates <- 1000
n_patients <- 10000

# Small batches for memory-constrained environments
batch_size_small <- min(25, n_covariates / 4)

# Large batches for memory-rich environments  
batch_size_large <- min(100, n_covariates / 2)

cat("Small batch size:", batch_size_small, "\n")
cat("Large batch size:", batch_size_large, "\n")

## ----performance-monitoring---------------------------------------------------
# Load example data
data(dx)
data(master)

# Monitor performance
start_time <- Sys.time()
start_memory <- gc()[, 2]

result <- hdps(
  data = dx,
  id_col = "pid",
  code_col = "icd9code",
  exposure_col = "exposure", 
  outcome_col = "outcome",
  master_data = master,
  n_candidates = 100,
  min_patients = 5,
  n_cores = 1,
  progress = TRUE
)

end_time <- Sys.time()
end_memory <- gc()[, 2]

# Performance metrics
duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
memory_used <- end_memory - start_memory
processing_rate <- nrow(result$prioritization) / duration

cat("Duration:", round(duration, 2), "seconds\n")
cat("Memory used:", round(memory_used, 1), "MB\n") 
cat("Processing rate:", round(processing_rate, 0), "covariates/second\n")

## ----profiling, eval=FALSE----------------------------------------------------
# # Profile your analysis (optional - requires profvis package)
# # install.packages("profvis")  # Uncomment to install
# library(profvis)
# 
# profvis({
#   result <- hdps(
#     data = dx,
#     id_col = "pid",
#     code_col = "icd9code",
#     exposure_col = "exposure",
#     outcome_col = "outcome",
#     master_data = master,
#     n_candidates = 50,
#     min_patients = 5,
#     n_cores = 1
#   )
# })

## ----complete-example---------------------------------------------------------
# Complete performance-optimized workflow
library(hdps)

# 1. Data preparation
data(dx)
data(master)
dx <- as.data.table(dx)
dx <- dx[!is.na(pid) & !is.na(icd9code)]

# 2. Determine optimal parameters
n_patients <- length(unique(dx$pid))
n_cores <- min(parallel::detectCores() - 1, 4)
batch_size <- ifelse(n_patients > 5000, 100, 50)

# For vignette stability, use single core
n_cores <- 1
batch_size <- 50

# 3. Run analysis with monitoring
start_time <- Sys.time()
cat("Starting HDPS analysis...\n")
cat("Patients:", n_patients, "\n")
cat("Cores:", n_cores, "\n")
cat("Batch size:", batch_size, "\n")

result <- hdps(
  data = dx,
  id_col = "pid",
  code_col = "icd9code",
  exposure_col = "exposure",
  outcome_col = "outcome", 
  master_data = master,
  n_candidates = 100,
  min_patients = 10,
  n_cores = n_cores,
  batch_size = batch_size,
  progress = TRUE
)

end_time <- Sys.time()
duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

cat("Analysis completed in", round(duration, 2), "seconds\n")
cat("Candidates found:", nrow(result$candidates), "\n")
cat("Covariates prioritized:", nrow(result$prioritization), "\n")

