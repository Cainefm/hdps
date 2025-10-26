# Edge case tests for HDPS package
library(testthat)
library(data.table)
library(hdps)

test_that("handles empty datasets gracefully", {
  empty_data <- data.table(pid = integer(0), icd9code = character(0))
  empty_master <- data.table(pid = integer(0), exposure = integer(0), outcome = integer(0))
  
  result <- hdps(
    data = empty_data,
    id_col = "pid",
    code_col = "icd9code",
    exposure_col = "exposure",
    outcome_col = "outcome",
    master_data = empty_master
  )
  
  expect_true(nrow(result$candidates) == 0)
  expect_true(nrow(result$prioritization) == 0)
})

test_that("handles single patient datasets", {
  single_patient_data <- data.table(pid = 1, icd9code = "ICD001")
  single_master <- data.table(pid = 1, exposure = 1, outcome = 0)
  
  result <- hdps(
    data = single_patient_data,
    id_col = "pid", 
    code_col = "icd9code",
    exposure_col = "exposure",
    outcome_col = "outcome",
    master_data = single_master,
    min_patients = 1
  )
  
  expect_true(nrow(result$candidates) >= 0)
})

test_that("handles datasets with all NA values", {
  na_data <- data.table(
    pid = c(1, 2, 3),
    icd9code = c(NA, NA, NA)
  )
  na_master <- data.table(
    pid = c(1, 2, 3),
    exposure = c(NA, NA, NA),
    outcome = c(NA, NA, NA)
  )
  
  result <- hdps(
    data = na_data,
    id_col = "pid",
    code_col = "icd9code", 
    exposure_col = "exposure",
    outcome_col = "outcome",
    master_data = na_master
  )
  
  expect_true(nrow(result$candidates) == 0)
})

test_that("handles datasets with only one unique code", {
  single_code_data <- data.table(
    pid = rep(1:10, each = 2),
    icd9code = rep("ICD001", 20)
  )
  single_master <- data.table(
    pid = 1:10,
    exposure = rep(c(1, 0), 5),
    outcome = rep(c(0, 1), 5)
  )
  
  result <- hdps(
    data = single_code_data,
    id_col = "pid",
    code_col = "icd9code",
    exposure_col = "exposure", 
    outcome_col = "outcome",
    master_data = single_master,
    min_patients = 5
  )
  
  expect_true(nrow(result$candidates) <= 1)
})

test_that("handles very large number of candidates", {
  large_candidates_data <- data.table(
    pid = rep(1:100, each = 10),
    icd9code = rep(paste0("ICD", 1:1000), 10)
  )
  large_master <- data.table(
    pid = 1:100,
    exposure = rbinom(100, 1, 0.3),
    outcome = rbinom(100, 1, 0.2)
  )
  
  result <- hdps(
    data = large_candidates_data,
    id_col = "pid",
    code_col = "icd9code",
    exposure_col = "exposure",
    outcome_col = "outcome", 
    master_data = large_master,
    n_candidates = 1000,
    min_patients = 5
  )
  
  expect_true(nrow(result$candidates) <= 1000)
})

test_that("handles missing master data", {
  test_data <- data.table(
    pid = rep(1:10, each = 2),
    icd9code = rep(c("ICD001", "ICD002"), 10),
    exposure = rep(c(1, 0), 10),
    outcome = rep(c(0, 1), 10)
  )
  
  result <- hdps(
    data = test_data,
    id_col = "pid",
    code_col = "icd9code",
    exposure_col = "exposure",
    outcome_col = "outcome"
  )
  
  expect_true(nrow(result$candidates) > 0)
  expect_true(nrow(result$prioritization) > 0)
})
