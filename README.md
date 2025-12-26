
## Project Overview

This repository contains the analysis and simulation code for the **Weak Learning Direction Theory (WLDT) Dyadic Revision Project**.  
The code is structured for clarity, reproducibility, and easy extension.

### Folder structure

| Folder | Purpose |
|:--|:--|
| **R/** | Core reusable R functions grouped by purpose (criteria, proportion/spread calculations, and reporting). |
| **scripts/** | Main analysis scripts (entry points for reproducing results). |
|**data** | Not included — place the required raw data files here after obtaining them from the original authors. |

---

### Data Availability

The dataset required to run the analysis is **not included in this repository**.

To obtain the data, please contact the authors of the original study:

Di Cagno, D., Galliera, A., Güth, W., Pace, N., & Panaccione, L. (2017).
*Experience and gender effects in acquisition experiment with value messages*.
**Small Business Economics, 48**(1), 71–97.
[https://doi.org/10.1007/s11187-016-9766-1](https://doi.org/10.1007/s11187-016-9766-1)


Once obtained, place the data files in the `data/` folder as described in the folder structure above.



## How to Run

To reproduce the results shown in **Table 1**, **Table 2**, and **Figure 5** of the paper:

```r
# Run the main analysis script for Tables 1 and 2.
# (Uncomment `correct.dist.print()` inside the script to generate the TikZ file for Figure 4 in results/density_plot.tex)
source("scripts/WLDT_dyadic2025.R")

# Run this script to generate the output table used to build Figure 5
source("scripts/WLDT_dyadic2025_q.R")
