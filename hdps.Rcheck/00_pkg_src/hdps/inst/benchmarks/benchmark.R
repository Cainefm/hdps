#!/usr/bin/env Rscript
# HDPS Package Benchmarking Suite
# Run with: Rscript inst/benchmarks/benchmark.R

library(hdps)
library(data.table)
library(microbenchmark)
library(ggplot2)

# Create benchmark datasets of different sizes
create_benchmark_data <- function(n_patients, n_codes) {
  set.seed(123)
  
  # Create diagnosis data
  dx_data <- data.table(
    pid = rep(1:n_patients, each = sample(1:5, n_patients, replace = TRUE)),
    icd9code = sample(paste0("ICD", 1:n_codes), 
                     sum(sample(1:5, n_patients, replace = TRUE)), 
                     replace = TRUE)
  )
  
  # Create master data
  master_data <- data.table(
    pid = 1:n_patients,
    exposure = rbinom(n_patients, 1, 0.3),
    outcome = rbinom(n_patients, 1, 0.2)
  )
  
  list(dx = dx_data, master = master_data)
}

# Benchmark different dataset sizes
benchmark_sizes <- function() {
  sizes <- list(
    small = list(patients = 500, codes = 50),
    medium = list(patients = 2000, codes = 200), 
    large = list(patients = 5000, codes = 500)
  )
  
  results <- list()
  
  for (size_name in names(sizes)) {
    cat("Benchmarking", size_name, "dataset...\n")
    
    data_config <- sizes[[size_name]]
    data <- create_benchmark_data(data_config$patients, data_config$codes)
    
    # Benchmark hdps function
    benchmark_result <- microbenchmark(
      hdps_result = hdps(
        data = data$dx,
        id_col = "pid",
        code_col = "icd9code",
        exposure_col = "exposure",
        outcome_col = "outcome",
        master_data = data$master,
        n_candidates = min(100, data_config$codes),
        min_patients = 10,
        n_cores = 1
      ),
      times = 3
    )
    
    # Extract performance metrics
    hdps_result <- hdps(
      data = data$dx,
      id_col = "pid", 
      code_col = "icd9code",
      exposure_col = "exposure",
      outcome_col = "outcome",
      master_data = data$master,
      n_candidates = min(100, data_config$codes),
      min_patients = 10,
      n_cores = 1
    )
    
    results[[size_name]] <- list(
      size = size_name,
      patients = data_config$patients,
      codes = data_config$codes,
      median_time = median(benchmark_result$time) / 1e9,  # Convert to seconds
      mean_time = mean(benchmark_result$time) / 1e9,
      candidates = nrow(hdps_result$candidates),
      covariates = nrow(hdps_result$prioritization),
      processing_rate = nrow(hdps_result$prioritization) / (median(benchmark_result$time) / 1e9)
    )
  }
  
  results
}

# Benchmark parallel processing
benchmark_parallel <- function() {
  cat("Benchmarking parallel processing...\n")
  
  data <- create_benchmark_data(2000, 200)
  
  # Test different core counts
  core_counts <- c(1, 2, 4)
  parallel_results <- list()
  
  for (cores in core_counts) {
    if (cores <= parallel::detectCores()) {
      benchmark_result <- microbenchmark(
        hdps_parallel = hdps(
          data = data$dx,
          id_col = "pid",
          code_col = "icd9code", 
          exposure_col = "exposure",
          outcome_col = "outcome",
          master_data = data$master,
          n_candidates = 100,
          min_patients = 10,
          n_cores = cores
        ),
        times = 3
      )
      
      parallel_results[[as.character(cores)]] <- list(
        cores = cores,
        median_time = median(benchmark_result$time) / 1e9,
        mean_time = mean(benchmark_result$time) / 1e9
      )
    }
  }
  
  parallel_results
}

# Generate performance report
generate_report <- function(size_results, parallel_results) {
  cat("\n=== HDPS Performance Benchmark Report ===\n\n")
  
  # Dataset size performance
  cat("Dataset Size Performance:\n")
  cat("========================\n")
  for (result in size_results) {
    cat(sprintf("%s dataset (%d patients, %d codes):\n", 
                result$size, result$patients, result$codes))
    cat(sprintf("  - Median time: %.2f seconds\n", result$median_time))
    cat(sprintf("  - Processing rate: %.0f covariates/second\n", result$processing_rate))
    cat(sprintf("  - Candidates found: %d\n", result$candidates))
    cat(sprintf("  - Covariates prioritized: %d\n", result$covariates))
    cat("\n")
  }
  
  # Parallel processing performance
  cat("Parallel Processing Performance:\n")
  cat("===============================\n")
  for (result in parallel_results) {
    cat(sprintf("%d cores: %.2f seconds (%.1fx speedup)\n", 
                result$cores, result$median_time,
                parallel_results[["1"]]$median_time / result$median_time))
  }
  
  # Memory usage
  cat("\nMemory Usage:\n")
  cat("=============\n")
  gc_result <- gc()
  cat(sprintf("Current memory usage: %.1f MB\n", gc_result[2, 2]))
}

# Main benchmarking function
main <- function() {
  cat("Starting HDPS performance benchmarks...\n\n")
  
  # Run benchmarks
  size_results <- benchmark_sizes()
  parallel_results <- benchmark_parallel()
  
  # Generate report
  generate_report(size_results, parallel_results)
  
  # Save results
  benchmark_data <- list(
    size_results = size_results,
    parallel_results = parallel_results,
    timestamp = Sys.time(),
    r_version = R.version.string,
    hdps_version = packageVersion("hdps")
  )
  
  saveRDS(benchmark_data, "benchmark_results.rds")
  cat("\nBenchmark results saved to benchmark_results.rds\n")
}

# Run benchmarks if script is executed directly
if (!interactive()) {
  main()
}
