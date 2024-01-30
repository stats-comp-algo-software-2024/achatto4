# hiperglm: High-Performance GLM Package

## Overview

`hiperglm` is an R package that provides a high-performance implementation of Generalized Linear Models (GLM). This package is designed to efficiently fit GLMs, and it exports a set of functions for model fitting, coefficient extraction, variance-covariance matrix computation, and model printing.

## Installation

You can install the `hiperglm` package from GitHub using the `devtools` package:

```R
devtools::install_github("yourusername/hiperglm")
```

# Usage

## Model Fitting

Use the hiper_glm function for fitting high-performance GLMs:

```R
library(hiperglm)

# Example data
design <- matrix(rnorm(100 * 5), ncol = 5)
outcome <- rbinom(100, 1, 0.5)

# Fit the high-performance GLM
model <- hiper_glm(design, outcome)
```

## Coefficient Extraction
Use the coef function to extract coefficients from the model:

```R
# Extract coefficients
coefficients <- coef(model)
print(coefficients)
```

##Variance-Covariance Matrix
Use the vcov function to obtain the variance-covariance matrix:

```R
# Get variance-covariance matrix
var_cov_matrix <- vcov(model)
print(var_cov_matrix)
```

## Model Printing
Use the print function to display information about the model:

```R
# Print model information
print(model)
```

## License

MIT License

## Contact
For any issues, questions, or feedback, please contact:

Anagh Chattopadhyay
Email: achatto4@jhu.edu
