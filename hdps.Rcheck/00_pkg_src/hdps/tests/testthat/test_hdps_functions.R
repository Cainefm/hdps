# Test HDPS functions
library(testthat)
library(hdps)

# Create test data
test_data <- data.table(
  pid = rep(1:10, each = 5),
  code = rep(c("A", "B", "C", "D", "E"), 10),
  type = rep("dx", 50)
)

test_master <- data.table(
  pid = 1:10,
  exposure = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
  outcome = c(0, 1, 1, 0, 1, 0, 0, 1, 1, 0)
)

test_that("identify_candidates works correctly", {
  result <- identify_candidates(test_data, "pid", "code", "dx", n = 3, min_patients = 2)
  
  expect_is(result, "list")
  expect_true("candidates" %in% names(result))
  expect_true("data" %in% names(result))
  expect_true("patient_ids" %in% names(result))
  expect_true(nrow(result$candidates) <= 3)
})

test_that("assess_recurrence works correctly", {
  candidates <- identify_candidates(test_data, "pid", "code", "dx", n = 3, min_patients = 2)
  result <- assess_recurrence(candidates$data, "pid", "code", "dx")
  
  expect_is(result, "data.table")
  expect_true("pid" %in% colnames(result))
  expect_true(nrow(result) > 0)
})

test_that("prioritize works correctly", {
  # Create test data with exposure and outcome
  test_cohort <- data.table(
    pid = 1:10,
    dx_A_once = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
    dx_B_once = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
    exposure = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
    outcome = c(0, 1, 1, 0, 1, 0, 0, 1, 1, 0)
  )
  
  result <- prioritize(test_cohort, "pid", "exposure", "outcome")
  
  expect_is(result, "data.table")
  expect_true("code" %in% colnames(result))
  expect_true("absLogBias" %in% colnames(result))
  expect_true(nrow(result) > 0)
})

test_that("prioritize with parallel processing works", {
  test_cohort <- data.table(
    pid = 1:10,
    dx_A_once = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
    dx_B_once = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
    exposure = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
    outcome = c(0, 1, 1, 0, 1, 0, 0, 1, 1, 0)
  )
  
  result <- prioritize(test_cohort, "pid", "exposure", "outcome", n_cores = 2)
  
  expect_is(result, "data.table")
  expect_true("code" %in% colnames(result))
  expect_true("absLogBias" %in% colnames(result))
})

test_that("hdps works for single domain", {
  result <- hdps(test_data, "pid", "code", "exposure", "outcome", 
                 n_candidates = 3, min_patients = 2)
  
  expect_is(result, "list")
  expect_true("candidates" %in% names(result))
  expect_true("recurrence" %in% names(result))
  expect_true("prioritization" %in% names(result))
})

test_that("hdps_input works for different formats", {
  # Test long format
  result_long <- hdps_input(test_data, "long")
  expect_is(result_long, "data.table")
  
  # Test wide format
  wide_data <- dcast(test_data, pid ~ code, value.var = "code", fun.aggregate = length)
  result_wide <- hdps_input(wide_data, "wide", value_col = "value")
  expect_is(result_wide, "data.table")
  
  # Test matrix format
  matrix_data <- matrix(rbinom(20, 1, 0.5), nrow = 4, ncol = 5)
  rownames(matrix_data) <- 1:4
  colnames(matrix_data) <- LETTERS[1:5]
  result_matrix <- hdps_input(matrix_data, "matrix")
  expect_is(result_matrix, "data.table")
})

test_that("plot functions work", {
  # Create test result
  test_result <- data.table(
    code = c("dx_A_once", "dx_B_once"),
    absLogBias = c(0.5, 0.3),
    ce_strength = c(0.2, 0.4),
    cd_strength = c(0.6, 0.8),
    c1 = c(5, 3),
    c0 = c(5, 7),
    e1 = c(5, 5),
    e0 = c(5, 5)
  )
  
  # Test plot functions (they should not error)
  expect_error(plot_bias_distribution(test_result), NA)
  expect_error(plot_covariate_strength(test_result), NA)
  expect_error(plot_bias_vs_prevalence(test_result), NA)
})

test_that("error handling works", {
  # Test missing data
  expect_error(identify_candidates(), "dt must be provided")
  
  # Test invalid column names
  expect_error(identify_candidates(test_data, "invalid", "code", "dx"), 
               "id must be the column name of identifier")
  
  # Test invalid data types
  expect_error(identify_candidates("not_a_dataframe", "pid", "code", "dx"), 
               "dt must be provided as a data.frame or data.table")
})
