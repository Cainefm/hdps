## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.width = 8,
  fig.height = 6
)

## ----installation, eval=FALSE-------------------------------------------------
# # Install from GitHub
# devtools::install_github("Cainefm/hdps")
# 
# # Or install from local .tar.gz file
# install.packages("hdps_0.9.0.tar.gz", repos = NULL, type = "source")

## ----load-package-------------------------------------------------------------
library(hdps)
library(ggplot2)

## ----create-sample-data-------------------------------------------------------
# Create sample diagnosis data
set.seed(123)
n_patients <- 100
n_codes <- 50

# Generate patient IDs and diagnosis codes
dx_data <- data.table(
  pid = rep(1:n_patients, each = n_codes),
  code = rep(paste0("ICD", sprintf("%03d", 1:n_codes)), n_patients),
  type = "dx"
)

# Add some variation - not all patients have all codes
dx_data <- dx_data[sample(nrow(dx_data), nrow(dx_data) * 0.3), ]

# Create master table with exposure and outcome
master_data <- data.table(
  pid = 1:n_patients,
  exposure = rbinom(n_patients, 1, 0.5),
  outcome = rbinom(n_patients, 1, 0.3)
)

# Merge with exposure/outcome data
dx_data <- merge(dx_data, master_data, by = "pid", all.x = TRUE)

head(dx_data)

## ----hdps-complete------------------------------------------------------------
# Complete HDPS workflow in one function (using single core to avoid vignette issues)
results <- hdps(
  data = dx_data,
  id_col = "pid",
  code_col = "code", 
  exposure_col = "exposure",
  outcome_col = "outcome",
  master_data = master_data,
  n_candidates = 20,
  min_patients = 5,
  n_cores = 1  # Use single core for vignette
)

# View results
head(results$prioritization[order(absLogBias, decreasing = TRUE)])

## ----multi-domain, eval=FALSE-------------------------------------------------
# # Create multi-domain data
# dx_data <- data.table(pid = 1:100, code = paste0("DX", 1:100), type = "dx")
# px_data <- data.table(pid = 1:100, code = paste0("PX", 1:100), type = "px")
# rx_data <- data.table(pid = 1:100, code = paste0("RX", 1:100), type = "rx")
# 
# # Multi-domain analysis
# multi_domain_results <- hdps_multi_domain(
#   data_list = list(dx = dx_data, px = px_data, rx = rx_data),
#   id_col = "pid",
#   code_col = "code",
#   exposure_col = "exposure",
#   outcome_col = "outcome"
# )

## ----complete-workflow--------------------------------------------------------
# Complete workflow in one function
results <- hdps(
  data = dx_data,
  id_col = "pid",
  code_col = "code",
  exposure_col = "exposure",
  outcome_col = "outcome",
  master_data = master_data,
  n_candidates = 20,
  min_patients = 5,
  n_cores = 1
)

# View results
str(results)

## ----bias-plot----------------------------------------------------------------
# Plot bias distribution
p1 <- plot_bias_distribution(results$prioritization, top_n = 10)
print(p1)

## ----strength-plot------------------------------------------------------------
# Plot covariate strength relationships
p2 <- plot_covariate_strength(results$prioritization)
print(p2)

## ----prevalence-plot----------------------------------------------------------
# Plot bias vs prevalence
p3 <- plot_bias_vs_prevalence(results$prioritization)
print(p3)

## ----interactive-plots, eval=FALSE--------------------------------------------
# # Note: For interactive analysis, use the hdps_interactive() function
# # which provides a Shiny interface for covariate selection
# 
# # Example of interactive analysis:
# # hdps_interactive()

## ----flexible-input-----------------------------------------------------------
# Long format (default)
data_long <- hdps_input(dx_data, format = "long")

# Wide format
wide_data <- dcast(dx_data, pid ~ code, value.var = "code", fun.aggregate = length)
data_wide <- hdps_input(wide_data, format = "wide", value_col = "count")

# Matrix format (example)
# matrix_data <- matrix(rbinom(100, 1, 0.5), nrow = 10, ncol = 10)
# rownames(matrix_data) <- paste0("P", 1:10)
# colnames(matrix_data) <- paste0("VAR", 1:10)
# data_matrix <- hdps_input(matrix_data, format = "matrix")

## ----memory-tips, eval=FALSE--------------------------------------------------
# # Monitor memory usage
# library(pryr)
# mem_used()
# 
# # Use gc() to free memory
# gc()

## ----quality-control----------------------------------------------------------
# Check data quality
cat("Number of patients:", length(unique(dx_data$pid)), "\n")
cat("Number of codes:", length(unique(dx_data$code)), "\n")
cat("Missing values:", sum(is.na(dx_data)), "\n")

# Check exposure/outcome distribution
table(master_data$exposure)
table(master_data$outcome)

## ----debugging----------------------------------------------------------------
# Check intermediate results
candidates <- identify_candidates(dx_data, "pid", "code", "dx", n = 10)
cat("Number of candidates:", nrow(candidates$candidates), "\n")

# Check recurrence data
recurrence <- assess_recurrence(candidates$data, "pid", "code", "dx")
cat("Number of recurrence variables:", ncol(recurrence) - 1, "\n")

