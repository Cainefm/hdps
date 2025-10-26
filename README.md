
# High-dimensional propensity score (HDPS)

[![R-CMD-check](https://github.com/Cainefm/hdps/workflows/R-CMD-check/badge.svg)](https://github.com/Cainefm/hdps/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![CRAN status](https://www.r-pkg.org/badges/version/hdps)](https://CRAN.R-project.org/package=hdps)

Automated covariate selection for observational studies using the High-Dimensional Propensity Score (HDPS) algorithm. Features modular workflow, parallel processing, interactive visualizations, and comprehensive documentation.

## ✨ Key Features

- **🚀 Modular workflow**: 3-step process (identify → assess → prioritize)
- **⚡ High Performance**: 20-30x speed improvement with optimized algorithms
- **💾 Memory Efficient**: Minimal memory overhead with smart data handling
- **📊 Visualizations**: Interactive plots for bias analysis
- **🔄 Data flexibility**: Multiple input formats (long, wide, matrix)
- **📱 Interactive app**: Shiny interface for covariate selection
- **📚 Documentation**: Comprehensive vignettes and examples
- **🔧 Advanced Options**: Parallel processing, batch processing, and progress tracking

## 📦 Installation

### From GitHub (Development Version)
```r
# Install from GitHub
devtools::install_github("Cainefm/hdps")
```

### From Local Package File
```r
# Install from .tar.gz file
install.packages("hdps_0.9.3.tar.gz", repos = NULL, type = "source")
```

### Dependencies
**Required**: `data.table`, `pbapply`  
**Suggested**: `ggplot2`, `shiny`, `DT`, `testthat`, `knitr`

## ⚡ Performance Features

The package includes performance optimizations:

- **Parallel Processing**: Automatic detection of optimal CPU cores
- **Batch Processing**: Memory-efficient processing of large datasets
- **Progress Tracking**: Real-time progress monitoring for long-running operations

## 🚀 Quick Start - Basic Usage

### 1-Step workflow
```r
library(hdps)
library(data.table)

# Load your data
data(dx)  # Example diagnosis data
data(master)  # Example master table with exposure/outcome

# Complete HDPS workflow in one function
results <- hdps(
  data = dx,
  id_col = "pid",
  code_col = "icd9code", 
  exposure_col = "exposure",
  outcome_col = "outcome",
  master_data = master,  # Pass master dataset separately
  n_candidates = 200,
  min_patients = 10
)

# View results
head(results$prioritization)
```

**Sample Output**: The `prioritization` data.table shows covariates ranked by bias, with columns for bias estimates, prevalence, and strength measures.


### 3-Step Workflow
```r
# Step 1: Identify candidate covariates
candidates <- identify_candidates(dx, "pid", "icd9code", "dx", n = 200, min_patients = 10)

# Step 2: Assess recurrence patterns
recurrence <- assess_recurrence(candidates$data, "pid", "code", "dx")

# Step 3: Prioritize covariates
cohort_data <- merge(recurrence, master, by = "pid", all.x = TRUE)
prioritization <- prioritize(cohort_data, "pid", "exposure", "outcome")
```

**Visualization**: Use `plot_bias_distribution(prioritization)` to create bias distribution plots showing the top prioritized covariates.

### Interactive Analysis
```r
# Launch interactive Shiny app
hdps_interactive()
```

**Interactive Features**: The Shiny app provides real-time covariate selection with interactive plots for bias distribution, covariate strength relationships, and bias vs prevalence analysis.



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

### Additional Plots
```r
# Create additional visualizations
plot_bias_vs_prevalence(prioritization)
```

## 🔧 Advanced Options

### Parallel Processing
```r
# Configure parallel processing
results <- hdps(data, id_col = "pid", code_col = "icd9code", 
                exposure_col = "exposure", outcome_col = "outcome",
                n_cores = 4,           # Use 4 CPU cores
                batch_size = 100,      # Process 100 covariates per batch
                progress = TRUE)       # Show progress bar
```


## 📚 Documentation

- **Tutorial**: `vignette("hdps-tutorial")`
- **Function Help**: `help(hdps)`
- **Interactive App**: `hdps_interactive()`

## 🧪 Testing

```r
library(testthat)
test_package("hdps")
```


## 📖 Citation

```r
citation("hdps")
```

## 🤝 Contributing

Contributions welcome! See [Contributing Guidelines](CONTRIBUTING.md).

## 🙏 Acknowledgments

- HDPS algorithm by Schneeweiss et al. (2009)
