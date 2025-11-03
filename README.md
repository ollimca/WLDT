---
title: "WLDT Dyadic Revision Project"
output: github_document
---

## 📘 Project Overview

This repository contains the analysis and simulation code for the **Weak Learning Direction Theory (WLDT) Dyadic Revision Project**.  
The code is structured for clarity, reproducibility, and easy extension.

### Folder structure

| Folder | Purpose |
|:--|:--|
| **R/** | Core reusable R functions grouped by purpose (criteria, proportion/spread calculations, and reporting). |
| **scripts/** | Main analysis scripts (entry points for reproducing results). |
| **data/** | Raw input data files (not tracked by Git; see `.gitignore`). |
| **results/** | Generated figures, tables, and output files (ignored by Git except for `README.md`). |
| **tests/** | Local unit and randomized tests for verifying function correctness (ignored by Git). |
| **benchmarks/** | Performance benchmarks used during development (ignored by Git). |
| **deprecated/** | Archived or legacy functions retained for reference. |

---

## ⚙️ How to Run

To reproduce the main analysis:

```r
# Install required packages (if needed)
packages <- c("dplyr", "BSDA", "ggplot2", "tikzDevice")
install.packages(setdiff(packages, rownames(installed.packages())))

# Load packages
library(dplyr)
library(BSDA)
library(ggplot2)
library(tikzDevice)

# Source project functions
source("R/criteria.R")
source("R/proportion.R")
source("R/reporting.R")

# Run the main analysis script
source("scripts/WLDT_dyadic2025.R")
