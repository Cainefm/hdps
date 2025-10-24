
# High-dimensional propensity score (HDPS)

[![R-CMD-check](https://github.com/Cainefm/hdps/workflows/R-CMD-check/badge.svg)](https://github.com/Cainefm/hdps/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![CRAN status](https://www.r-pkg.org/badges/version/hdps)](https://CRAN.R-project.org/package=hdps)

Automated covariate selection for observational studies using the High-Dimensional Propensity Score (HDPS) algorithm. Features modular workflow, parallel processing, interactive visualizations, and comprehensive documentation.

## ✨ Key Features

- **🚀 Modular workflow**: 3-step process (identify → assess → prioritize)
- **⚡ Performance**: Parallel processing for large datasets
- **📊 Visualizations**: Interactive plots for bias analysis
- **🔄 Data flexibility**: Multiple input formats (long, wide, matrix)
- **📱 Interactive app**: Shiny interface for covariate selection
- **📚 Documentation**: Comprehensive vignettes and examples

## 📦 Installation

### From GitHub (Development Version)
```r
# Install from GitHub
devtools::install_github("Cainefm/hdps")
```

### From Local Package File
```r
# Install from .tar.gz file
install.packages("hdps_0.9.2.tar.gz", repos = NULL, type = "source")
```

### Dependencies
**Required**: `data.table`, `pbapply`, `parallel`  
**Suggested**: `ggplot2`, `plotly`, `shiny`, `DT`

## 🚀 Quick Start - Basic Usage

### 1-Step workflow
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
  master_data = master,  # Pass master dataset separately
  n_candidates = 200,
  min_patients = 10
)

# View results
head(results$prioritization)
```
<img src="data/Snipaste_2025-10-24_17-07-48.png" alt="Sample output of hdps_screen prioritization result" width="650"/>


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
<img src="data/Snipaste_2025-10-24_17-09-12.png" alt="Plot of absolute log-bias for top prioritized covariates by hdps_screen" width="650"/>

### Interactive Analysis
```r
# Launch interactive Shiny app
hdps_interactive()
```
<img src="data/Snipaste_2025-10-24_17-10-32.png" alt="Screenshot of the HDPS Interactive Covariate Selection Shiny app, showing bias distribution, covariate strength, and bias vs prevalence plots." width="650"/>



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


## 📚 Documentation

- **Tutorial**: `vignette("hdps-tutorial")`
- **Function Help**: `help(hdps_screen)`
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
