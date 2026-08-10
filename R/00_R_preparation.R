# preperation -------------------------------------------------------------

## load packages -----------------------------------------------------------

library(tibble)
library(openxlsx) #export xlsx
library(tidyverse) #data transformation
library(ggplot2) #data visualiazation
library(ggpubr) #arranging plot
library(purrr) #map() function
library(Hmisc) #correlation matrix
library(corrplot) #correlation plot
library(see) #for scale_fill_okabeito(), palette for color blind
library(extrafont) #registers custom fonts (e.g. Century Gothic) used throughout the plots
#for map making
library(rnaturalearth)
library(sf)
library(ggspatial)
library(dplyr)

library(aelab)


## load function -----------------------------------------------------------

separate_culture_period <- function(df) {

  if (!"sampling" %in% colnames(df)) {
    stop("No culture period to separate: 'sampling' column is missing.")
  }

  # Define common levels for the period factor
  common_levels <- c("DP", "BS", "CP", "AH", "EM", "Other")

  # Add 'period' column based on sampling (date)

  fw_mono_prawn <- df %>%
    filter(water == "fw", type == "mono", species == "prawn") %>%
    mutate(period = case_when(
      sampling == "dry" ~ "DP",
      TRUE ~ "CP")) %>%
    mutate(period = factor(period, levels = common_levels))

  if ("sampling_date_start" %in% colnames(df)) {

    fw_mono_tilapia <- df %>%
      filter(water == "fw", type == "mono", species == "tilapia") %>%
      mutate(period = case_when(
                                group == "T1" & sampling_date_start < as.Date("2025-03-30") ~ "EM",
                                group == "T1" & sampling_date_start >= as.Date("2026-03-10") ~ "EM",
                                group == "T2" & sampling_date_start < as.Date("2025-01-23") ~ "EM",
                                group == "T3" & sampling_date_start < as.Date("2024-09-15") ~ "EM",
                                sampling == "dry" ~ "DP",
                                TRUE ~ "CP"
                                )) %>%
      mutate(period = factor(period, levels = common_levels))

  } else if ("sampling_date" %in% colnames(df)) {

    fw_mono_tilapia <- df %>%
      filter(water == "fw", type == "mono", species == "tilapia") %>%
      mutate(period = case_when(
                                group == "T1" & sampling_date < as.Date("2025-03-30") ~ "EM",
                                group == "T1" & sampling_date >= as.Date("2026-03-09") ~ "EM",
                                group == "T2" & sampling_date < as.Date("2025-01-23") ~ "EM",
                                group == "T3" & sampling_date < as.Date("2024-09-15") ~ "EM",
                                sampling == "dry" ~ "DP",
                                TRUE ~ "CP"
                                )) %>%
      mutate(period = factor(period, levels = common_levels))
  }

  # Combine the data frames
  all <- bind_rows(fw_mono_prawn, fw_mono_tilapia)

  return(all)
}


## input value -------------------------------------------------------------------

### Most used parameters ----------------------------------------------------

ch4_mass <- 16.04 #g mol-1
c_mass <- 12.01 #g mol-1
alter_direction <- -1

### Unit transformation-------------------------------------------------------------------------

hr_to_day <- (1/24)
day_to_yr <- (1/365)
µg_to_g <- (1/1000000)
µg_to_mg <- (1/1000)
mg_to_Mg <- (1/1000000000)
kg_to_Mg <- (1/1000)
g_to_kg <- (1/1000)
kg_to_g <- 1000
m3_to_L <- 1000
g_to_Mg <- (1/1000000)
m2_to_ha <- (1/10000)
gm2hr_to_Mghayr <- (g_to_Mg)*(1/m2_to_ha)*(1/hr_to_day)*(1/day_to_yr)
gm2_to_Mgha <- (g_to_Mg)*(1/m2_to_ha)

### Axis label-----

lab_co2 <- expression(bold(paste("C",O[2], " fluxes (mg ", m^-2~h^-1, ")")))
lab_ch4 <- expression(bold(paste("C",H[4], " fluxes (µg ", m^-2~h^-1, ")")))
lab_ch4_mg <- expression(bold(paste("C",H[4], " fluxes (mg ", m^-2~h^-1, ")")))
lab_n2o <- expression(bold(paste(N[2], "O fluxes (µg ", m^-2~h^-1, ")")))
lab_n2o_mg <- expression(bold(paste(N[2], "O fluxes (mg ", m^-2~h^-1, ")")))
lab_temp <- expression(bold(paste("Temperature"~"(˚C)")))
lab_do_w <- expression(bold(paste("DO concentration"~"(mg"~L^-1, ")")))
lab_ph <- expression(bold(paste("pH")))
lab_orp <- expression(bold(paste("ORP (mV)")))
lab_silt <- expression(bold(paste("Silt/clay content (%)")))
