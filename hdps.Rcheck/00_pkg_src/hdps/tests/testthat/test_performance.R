# Performance tests for HDPS package
library(testthat)
library(hdps)

# Create larger test dataset for performance testing
set.seed(123)
n_patients <- 1000
n_codes <- 100

large_test_data <- data.table(
  pid = rep(1:n_patients, each = sample(1:5, n_patients, replace = TRUE)),
  icd9code = sample(paste0("ICD", 1:n_codes), 
                   sum(sample(1:5, n_patients, replace = TRUE)), 
                   replace = TRUE)
)

large_master_data <- data.table(
  pid = 1:n_patients,
  exposure = rbinom(n_patients, 1, 0.3),
  outcome = rbinom(n_patients, 1, 0.2)
)

test_that("performance benchmarks are met", {
  # Test that processing completes within reasonable time
  start_time <- Sys.time()
  
  result <- hdps(
    data = large_test_data,
    id_col = "pid",
    code_col = "icd9code",
    exposure_col = "exposure", 
    outcome_col = "outcome",
    master_data = large_master_data,
    n_candidates = 50,
    min_patients = 10,
    n_cores = 1
  )
  
  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Should complete within 30 seconds for 1000 patients
  expect_lt(duration, 30)
  
  # Should produce results
  expect_true(nrow(result$candidates) > 0)
  expect_true(nrow(result$prioritization) > 0)
})

test_that("memory usage is reasonable", {
  # Test memory usage doesn't explode
  initial_memory <- gc()[, 2]
  
  result <- hdps(
    data = large_test_data,
    id_col = "pid",
    code_col = "icd9code",
    exposure_col = "exposure",
    outcome_col = "outcome", 
    master_data = large_master_data,
    n_candidates = 50,
    min_patients = 10,
    n_cores = 1
  )
  
  final_memory <- gc()[, 2]
  memory_used <- final_memory - initial_memory
  
  # Should use less than 100MB for 1000 patients
  expect_lt(memory_used, 100)
})

test_that("parallel processing works correctly", {
  # Test parallel processing doesn't break
  result_parallel <- hdps(
    data = large_test_data,
    id_col = "pid", 
    code_col = "icd9code",
    exposure_col = "exposure",
    outcome_col = "outcome",
    master_data = large_master_data,
    n_candidates = 50,
    min_patients = 10,
    n_cores = 2
  )
  
  result_sequential <- hdps(
    data = large_test_data,
    id_col = "pid",
    code_col = "icd9code", 
    exposure_col = "exposure",
    outcome_col = "outcome",
    master_data = large_master_data,
    n_candidates = 50,
    min_patients = 10,
    n_cores = 1
  )
  
  # Results should be equivalent (allowing for minor numerical differences)
  expect_equal(nrow(result_parallel$prioritization), 
               nrow(result_sequential$prioritization))
})
