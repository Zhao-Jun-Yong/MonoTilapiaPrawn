# MonoTilapiaPrawn — Data and Analysis Repository

[![DOI](https://zenodo.org/badge/1204679331.svg)](https://doi.org/10.5281/zenodo.19467725)

Data and R scripts supporting the article:

> Yong, Z.-J., Lin, W.-J., Lee, Y.-C., Liang, Y.-C., Yang, S.-D., Lin, H.-J. (2026). Species-specific greenhouse gas emissions and carbon budgets in freshwater monoculture ponds. *Aquaculture International*, 34, 258. https://doi.org/10.1007/s10499-026-02664-2

---

## Repository Contents

| Path | Description |
|------|-------------|
| `R/00_R_preparation.R` | Package loading, custom functions, and constants |
| `MonoTilapiaPrawn.R` | Main analysis script (environmental factors, GHG, DO metabolism, carbon budget, PLS-SEM) |
| `R/PrawnCarbonBudget.R` | Per-period carbon budget calculations for prawn ponds |
| `R/TilapiaCarbonBudget.R` | Per-period carbon budget calculations for tilapia ponds |
| `data/*.rds` | Pre-processed datasets |
| `data/PT_info.xlsx` | Culture period definitions for prawn tanks |
| `data/20260219_tab_ghg_studies.xlsx` | Literature GHG values used for comparison |
| `output/` | Generated figures and tables (not tracked in git; reproduced by running the script) |

---

## How to Reproduce

### Requirements

R (≥ 4.2) with the following packages:

- `aelab` (install from GitHub: `remotes::install_github("yangshaojun/aelab")`)
- CRAN packages: `openxlsx`, `tidyverse`, `ggplot2`, `ggpubr`, `purrr`, `Hmisc`, `corrplot`, `see`, `extrafont`, `rnaturalearth`, `sf`, `ggspatial`, `seminr`

### Steps

1. Open the project in RStudio (or set your working directory to this repository root).
2. Run the main analysis script:
   ```r
   source("MonoTilapiaPrawn.R")
   ```
   All figures and tables will be saved to `output/`.

---

## Data Description

All `data/*.rds` files contain data filtered to freshwater monoculture systems (`water == "fw"`, `type == "mono"`), excluding prawn tank T1 which was not used in the analysis.

| File | Description |
|------|-------------|
| `do.rds` | Dissolved oxygen metabolism (GPP, ER, NEP) |
| `ghg.rds` | Greenhouse gas fluxes (CO₂, CH₄, N₂O) |
| `ghg_co2e.rds` | GHG fluxes converted to CO₂-equivalents |
| `doc.rds` | Dissolved organic carbon |
| `poc.rds` | Particulate organic carbon |
| `sa.rds` | Sediment accumulation |
| `envr.rds` | Environmental parameters (temperature, DO, pH, EC, turbidity, etc.) |
| `raw_budget.rds` | Carbon budget (each sampling campaign) |
| `feed.rds` | Feed input |
| `info.rds` | Pond information (area, depth, stocking density, etc.) |
| `nutrient.rds` | Nutrient concentrations (NH₄⁺, NO₂⁻, NO₃⁻, PO₄³⁻) |
| `animals.rds` | Cultured animal biomass and harvest records |
| `weather_year.rds` | Annual weather summary at sampling site |


---

## Citation

If you use this repository, please cite the article above (https://doi.org/10.1007/s10499-026-02664-2) and this dataset (https://doi.org/10.5281/zenodo.19467725).
