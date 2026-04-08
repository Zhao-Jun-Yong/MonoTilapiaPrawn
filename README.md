# MonoTilapiaPrawn — Data and Analysis Repository

Data and R scripts supporting the manuscript:

> **[Title]**  
> [Authors]  
> *[Journal]*, [Year]. DOI: [to be added after Zenodo archiving]

---

## Repository Contents

| Path | Description |
|------|-------------|
| `R/00_R_preparation.R` | Package loading, custom functions, and constants |
| `R/utils/modified_corrplot.R` | Custom corrplot helper function |
| `project/MonoTilapiaPrawn.R` | Main analysis script (environmental factors, GHG, DO metabolism, carbon budget, PLS-SEM) |
| `project/PrawnCarbonBudget.R` | Per-period carbon budget calculations for prawn tanks |
| `data/*.rds` | Pre-processed datasets (freshwater monoculture: tilapia and prawn) |
| `data/weather/weather_MonoTilapiaPrawn/` | Raw HOBO data-logger CSV files |
| `data/PT_info.xlsx` | Culture period definitions for prawn tanks |
| `data/20260219_tab_ghg_studies.xlsx` | Literature GHG values used for comparison |
| `output/` | Generated figures and tables (not tracked in git; reproduced by running the script) |

---

## How to Reproduce

### Requirements

R (≥ 4.2) with the following packages:

- `aelab` (install from GitHub: `remotes::install_github("yangshaojun/aelab")`)
- CRAN packages: `readxl`, `openxlsx`, `tidyverse`, `ggplot2`, `pastecs`, `ggbreak`, `ggpubr`, `car`, `FSA`, `purrr`, `multcompView`, `rcompanion`, `Hmisc`, `corrplot`, `png`, `ghibli`, `ggthemr`, `ggsignif`, `see`, `extrafont`, `Rcpp`, `rnaturalearth`, `rnaturalearthdata`, `sf`, `ggspatial`, `G2Sd`, `circlize`, `recolorize`, `dichromat`, `rgdal`, `seminr`

### Steps

1. Open the project in RStudio (or set your working directory to this repository root).
2. Run the main analysis script:
   ```r
   source("project/MonoTilapiaPrawn.R")
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
| `soc.rds` | Soil organic carbon |
| `sa.rds` | Sediment carbon accumulation |
| `envr.rds` | Environmental parameters (temperature, DO, pH, EC, turbidity, etc.) |
| `budget.rds` | Aggregated carbon budget by culture period |
| `raw_budget.rds` | Carbon budget components at measurement scale |
| `feed.rds` | Feed input records |
| `info.rds` | Pond information (area, depth, stocking density, etc.) |
| `nutrient.rds` | Nutrient concentrations (NH₄⁺, NO₂⁻, NO₃⁻, PO₄³⁻) |
| `aq_area.rds` | Taiwan aquaculture area statistics by species and year |
| `animals.rds` | Cultured animal biomass and harvest records |
| `weather_year.rds` | Annual weather summary |

---

## Citation

If you use this repository, please cite the associated manuscript (DOI above) and this dataset:

> [Authors] ([Year]). *MonoTilapiaPrawn: Data and analysis repository* [Data set]. Zenodo. DOI: [to be added]
