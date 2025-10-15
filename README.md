
# High-dimensional propensity score (HDPS)

[![R-CMD-check](https://github.com/Cainefm/hdps/workflows/R-CMD-check/badge.svg)](https://github.com/Cainefm/hdps/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![CRAN status](https://www.r-pkg.org/badges/version/hdps)](https://CRAN.R-project.org/package=hdps)

A comprehensive R package for conducting High-Dimensional Propensity Score (HDPS) analyses in epidemiological studies. This package implements the HDPS algorithm with modern R practices, enhanced performance, and user-friendly features.

## ✨ Key Features

- **🚀 Modular 3-step workflow**: Identify candidates → Assess recurrence → Prioritize covariates
- **🏥 Domain-specific handling**: Support for diagnosis (dx), procedures (px), and medications (rx)
- **⚡ Parallel processing**: Optional parallel processing for large datasets
- **📊 Enhanced visualizations**: Interactive plots for bias analysis and covariate relationships
- **🔄 Flexible data input**: Support for various data formats (long, wide, matrix)
- **🧪 Comprehensive testing**: Full test coverage with testthat
- **📱 Interactive Shiny app**: User-friendly interface for covariate selection
- **📚 Complete documentation**: Comprehensive vignettes and examples

## 📦 Installation

### From GitHub (Development Version)
```r
# Install from GitHub
devtools::install_github("Cainefm/hdps")
```

### From Local Package File
```r
# Install from .tar.gz file
install.packages("hdps_0.9.1.tar.gz", repos = NULL, type = "source")
```

### Dependencies
The package requires the following R packages:
- `data.table` - For efficient data manipulation
- `pbapply` - For progress bars
- `parallel` - For parallel processing
- `ggplot2` - For visualizations (suggested)
- `plotly` - For interactive plots (suggested)
- `shiny` - For interactive app (suggested)

## 🚀 Quick Start

### Basic Usage
```r
library(hdps)
library(data.table)

# Load your data
data(dx)  # Example diagnosis data
data(master)  # Example master table with exposure/outcome

# Complete HDPS workflow in one function
results <- hdps_screen(
  data = dx,
  id_col = "pid",
  code_col = "icd9code", 
  exposure_col = "exposure",
  outcome_col = "outcome",
  n_candidates = 200,
  min_patients = 10
)

# View results
head(results$prioritization)
```

### Modular 3-Step Workflow
```r
# Step 1: Identify candidate covariates
candidates <- identify_candidates(dx, "pid", "icd9code", "dx", n = 200, min_patients = 10)

# Step 2: Assess recurrence patterns
recurrence <- assess_recurrence(candidates$data, "pid", "code", "dx")

# Step 3: Prioritize covariates
cohort_data <- merge(recurrence, master, by = "pid", all.x = TRUE)
prioritization <- prioritize(cohort_data, "pid", "exposure", "outcome")
```

### Interactive Analysis
```r
# Launch interactive Shiny app
hdps_interactive()
```

## 📊 Enhanced Visualizations

### Bias Distribution Plot
```r
# Plot top covariates by bias
plot_bias_distribution(prioritization, top_n = 20)
```

### Covariate Strength Relationships
```r
# Plot CE vs CD strength relationships
plot_covariate_strength(prioritization)
```

### Interactive Plots
```r
# Create interactive plots
plot_bias_distribution(prioritization, interactive = TRUE)
plot_covariate_strength(prioritization, interactive = TRUE)
```

## 🏥 Multi-Domain Analysis

```r
# Analyze multiple domains (diagnosis, procedures, medications)
data_list <- list(
  dx = dx_data,    # Diagnosis data
  px = px_data,    # Procedure data  
  rx = rx_data     # Medication data
)

multi_results <- hdps_multi_domain(
  data_list = data_list,
  id_col = "pid",
  code_col = "code",
  exposure_col = "exposure", 
  outcome_col = "outcome"
)
```

## ⚡ Performance Optimization

### Parallel Processing
```r
# Use parallel processing for large datasets
prioritization <- prioritize(
  cohort_data, 
  "pid", "exposure", "outcome",
  parallel = TRUE,
  n_cores = 4
)
```

### Flexible Data Input
```r
# Support for different data formats
data_long <- hdps_input(data, format = "long")      # Long format
data_wide <- hdps_input(data, format = "wide")      # Wide format  
data_matrix <- hdps_input(data, format = "matrix")  # Matrix format
```

## 📚 Documentation

- **Complete Tutorial**: See `vignette("hdps-tutorial")` for comprehensive examples
- **Function Reference**: All functions are fully documented with examples
- **Interactive App**: Use `hdps_interactive()` for user-friendly analysis

## 🧪 Testing

The package includes comprehensive tests:
```r
# Run tests
library(testthat)
test_package("hdps")
```

## 🔧 Advanced Features

### Custom Parameters
```r
# Fine-tune analysis parameters
results <- hdps_screen(
  data = dx,
  id_col = "pid",
  code_col = "icd9code",
  exposure_col = "exposure", 
  outcome_col = "outcome",
  n_candidates = 500,        # More candidates
  min_patients = 20,         # Higher threshold
  parallel = TRUE,           # Use parallel processing
  n_cores = 8,              # Use 8 cores
  correction = TRUE          # Apply rare outcome correction
)
```

### Quality Control
```r
# Check data quality
summary(results$candidates$candidates)
summary(results$prioritization)
```

## 📖 Citation

If you use this package in your research, please cite:

```r
citation("hdps")
```

## 🤝 Contributing

Contributions are welcome! Please see our [Contributing Guidelines](CONTRIBUTING.md) for details.

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- Based on the HDPS algorithm by Schneeweiss et al. (2009)
- Inspired by the original `lendle/hdps` and `autoCovariateSelection` packages
- Built with modern R practices using `data.table`, `testthat`, and `roxygen2`
