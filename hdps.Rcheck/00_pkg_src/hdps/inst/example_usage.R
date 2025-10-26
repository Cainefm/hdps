# Example Usage of HDPS Package with Generated Datasets
# This script demonstrates how to use the example datasets with the HDPS package

# Load required packages
library(hdps)

# Note: To use the HDPS functions, you would need to:
# 1. Install the package: devtools::install()
# 2. Load the package: library(hdps)
# For this example, we'll just show the data structure

# Load the example datasets
data(dx)
data(master)
data(px)
data(rx)
data(multi_domain_data)

# Display dataset information
cat("=== HDPS Package Example Datasets ===\n\n")

cat("1. Diagnosis Data (dx):\n")
cat("   - Records:", nrow(dx), "\n")
cat("   - Patients:", length(unique(dx$pid)), "\n")
cat("   - Sample data:\n")
print(head(dx, 5))

cat("\n2. Master Table (master):\n")
cat("   - Patients:", nrow(master), "\n")
cat("   - Exposure rate:", round(mean(master$exposure) * 100, 1), "%\n")
cat("   - Outcome rate:", round(mean(master$outcome) * 100, 1), "%\n")
cat("   - Sample data:\n")
print(head(master, 5))

cat("\n3. Procedure Data (px):\n")
cat("   - Records:", nrow(px), "\n")
cat("   - Patients:", length(unique(px$pid)), "\n")
cat("   - Sample data:\n")
print(head(px, 5))

cat("\n4. Medication Data (rx):\n")
cat("   - Records:", nrow(rx), "\n")
cat("   - Patients:", length(unique(rx$pid)), "\n")
cat("   - Sample data:\n")
print(head(rx, 5))

cat("\n5. Multi-Domain Data:\n")
cat("   - Domains:", names(multi_domain_data), "\n")
cat("   - Total records:", sum(sapply(multi_domain_data, nrow)), "\n")

# Example HDPS workflow (requires package to be installed)
cat("\n=== Example HDPS Workflow ===\n\n")
cat("To run HDPS analysis, first install and load the package:\n")
cat("  devtools::install()\n")
cat("  library(hdps)\n\n")

cat("Then you can run the following workflow:\n\n")

cat("# Step 1: Identify candidate covariates\n")
cat("candidates <- identify_candidates(dx, 'pid', 'icd9code', 'dx', n = 50, min_patients = 5)\n\n")

cat("# Step 2: Assess recurrence patterns\n")
cat("recurrence <- assess_recurrence(candidates$data, 'pid', 'code', 'dx')\n\n")

cat("# Step 3: Prioritize covariates\n")
cat("cohort_data <- merge(recurrence, master, by = 'pid', all.x = TRUE)\n")
cat("prioritization <- prioritize(cohort_data, 'pid', 'exposure', 'outcome')\n\n")

cat("# Complete workflow using hdps_screen\n")
cat("results <- hdps_screen(\n")
cat("  data = dx,\n")
cat("  id_col = 'pid',\n")
cat("  code_col = 'icd9code',\n")
cat("  exposure_col = 'exposure',\n")
cat("  outcome_col = 'outcome',\n")
cat("  n_candidates = 50,\n")
cat("  min_patients = 5\n")
cat(")\n\n")

cat("# Multi-domain analysis\n")
cat("multi_results <- hdps_multi_domain(\n")
cat("  data_list = multi_domain_data,\n")
cat("  id_col = 'pid',\n")
cat("  code_col = c('icd9code', 'code', 'code'),\n")
cat("  exposure_col = 'exposure',\n")
cat("  outcome_col = 'outcome'\n")
cat(")\n")

cat("\n=== Example Complete ===\n")
cat("All example datasets are ready for use with the HDPS package!\n")
