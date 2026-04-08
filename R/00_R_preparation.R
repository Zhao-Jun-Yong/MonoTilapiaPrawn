# preperation -------------------------------------------------------------

## load packages -----------------------------------------------------------

library(readxl) #import xlsx
library(tibble)
library(openxlsx) #export xlsx
library(tidyverse) #data transformation
library(ggplot2) #data visualiazation
library(pastecs) #descriptive statistics
library(ggbreak) #axis break
library(ggpubr) #arranging plot
library(car) #Levene test
library(FSA) #dunn test
library(purrr) #map() function
library(multcompView) #label of TukeyHSD
library(stats) #TukeyHSD test
library(rcompanion) #label of DunnTest
library(Hmisc) #correlation matrix
library(corrplot) #correlation plot
library(png) #export png with transparent background
library(ghibli) #ggplot2 color scheme
library(ggthemr) #ggplot2 color scheme
library(ggsignif) #for significance labeling in ggplot2
library(see) #for scale_fill_okabeito(), palette for color blind
#for map making
library(extrafont)
library(Rcpp)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggspatial)
library(G2Sd) #for sediment grain size analysis
library(circlize)
library(recolorize) #for color blind check
library(dichromat) #for color blind check
library(dplyr)
library(multcompView)
library(rgdal)

library(aelab)


## load function -----------------------------------------------------------

check_cb <- function(palette, return_cb_palettes = FALSE, ...) {

  # make an empty list
  cb_palettes <- setNames(vector("list", length = 3),
                          nm = c("protan", "deutan", "tritan"))

  # generate colorblindness approximations
  for (i in 1:length(cb_palettes)) {
    cb_palettes[[i]] <- dichromat::dichromat(palette, names(cb_palettes)[i])
  }

  # reset graphical parameters when function exits:
  current_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(current_par))

  # plot for comparison
  layout(matrix(1:4, nrow = 4)); par(mar = rep(1, 4))
  recolorize::plotColorPalette(palette, main = "Trichromacy", ...)
  pnames <- c("Protanopia", "Deutanopia", "Tritanopia")
  for (i in 1:3) {
    recolorize::plotColorPalette(cb_palettes[[i]], main = pnames[i], ...)
  }

  if (return_cb_palettes) {
    return(cb_palettes)
  }
}

create_cor_table <- function(cor) {

# Calculate Spearman correlation
results <- rcorr(as.matrix(cor), type = "spearman")

# Extract correlation coefficients and p-values
results_r <- as.data.frame(results$r)
results_p <- as.data.frame(results$P)

# Add row names as a column
results_r <- results_r %>%
  rownames_to_column(var = "name")

results_p <- results_p %>%
  rownames_to_column(var = "name")

# Modify correlation coefficients to include asterisks based on p-value thresholds
results_r <- results_r %>%
  mutate(across(-name, ~ {
    # Format to two decimal places
    formatted_value <- formatC(., format = "f", digits = 2)

    # Add asterisks based on p-value
    p_value <- results_p[[cur_column()]]
    result_with_stars <- ifelse(p_value < 0.001, paste0(formatted_value, "***"),
                                ifelse(p_value < 0.01, paste0(formatted_value, "**"),
                                       ifelse(p_value < 0.05, paste0(formatted_value, "*"), formatted_value)))

    # Replace NA with 1
    result_with_stars[is.na(result_with_stars)] <- "1"

    return(result_with_stars)
  }))

return(results_r)

}


## function for FRI-----

turn_factor <- function(data) {
  
  # Convert specified columns to factors
  data <- data %>% 
    mutate(across(c(water, type, species, group), factor),
           site = ifelse(type == "photo", 
                         paste(water, type, group, sep = "_"),
                         paste(water, type, species, group, sep = "_")),
           site = factor(site),
           name = ifelse(type == "photo", 
                         as.character(group), 
                         paste(species, group, sep = " ")),
           name = factor(name)
    )
  
  # Check for sampling_date and apply logic only to FL or LB species
  if ("sampling_date" %in% colnames(data) && "sampling" %in% colnames(data)) {
    data$sampling_date <- as.Date(data$sampling_date)
    
    # Apply sampling logic only to rows where species is FL or LB
    data <- data %>%
      group_by(site) %>%
      mutate(sampling = ifelse(species %in% c("FL", "LB"),
                               as.character(cumsum(c(1, diff(sampling_date) > 1))),
                               sampling)) %>%
      mutate(sampling = ifelse(species %in% c("FL", "LB"),
                               paste0(sampling, 
                                      case_when(sampling == 1 ~ "st",
                                                sampling == 2 ~ "nd",
                                                sampling == 3 ~ "rd",
                                                TRUE ~ "th")),
                               sampling)) %>%
      ungroup()
  } else {
    
  }
  
  # Convert sampling to factor if it exists
  if ("sampling" %in% colnames(data)) {
    data <- data %>%
      mutate(sampling = factor(sampling))
  }
  
  # Handle sampling_date
  if ("sampling_date" %in% colnames(data)) {
    data <- data %>%
      mutate(year = factor(year(sampling_date)))
    # Fix: fw_mono_tilapia 9th sampling spans year boundary (Dec 2025 / Jan 2026);
    # Jan 2026 data belongs to the 2025 culture period
    if (all(c("sampling", "water", "type", "species") %in% colnames(data))) {
      data <- data %>%
        mutate(year = ifelse(
          water == "fw" & type == "mono" & species == "tilapia" &
            as.character(sampling) == "9th" & as.character(year) == "2026",
          "2025", as.character(year)
        ),
        year = factor(year))
    }
  }
  
  return(data)
}

create_con <- function(df, method = "co2e", grouping_vars = "year, species, name") {
  # Get variable names from the dataframe
  var_names <- names(df)
  
  # Initialize method_vars based on the method
  method_vars <- character(0)  # Initialize as an empty character vector
  
  # Determine the variables based on the method
  if (method == "co2e") {
    method_vars <- var_names[grep("^co2e_", var_names)]
    path_start <- 6  # Starting position for path extraction
  } else if (method == "cb") {
    method_vars <- var_names[grep("^cb_", var_names)]
    path_start <- 4  # Starting position for path extraction
  } else {
    stop("Invalid method specified. Use 'co2e' or 'cb'.")
  }
  
  # Check if any method_vars were found
  if (length(method_vars) == 0) {
    stop(paste("No variables found for method:", method))
  }
  
  # Split additional_vars into a vector
  grouping_vars_vec <- unlist(strsplit(grouping_vars, ",\\s*"))
  
  # Initialize a list to store data frames
  df_list <- vector("list", length(method_vars))
  
  # Loop through each method variable and create a data frame
  for (var in method_vars) {
    input <- df %>%
      ungroup() %>%
      select(all_of(grouping_vars_vec), !!sym(var)) %>%  # Use all_of to safely select additional variables
      rename(!!paste0(method, "_value") := !!sym(var)) %>%  # Dynamic renaming based on method
      mutate(path = substr(var, path_start, nchar(var)))  # Dynamic path extraction
    
    df_list[[var]] <- input  # Store each input data frame in the list
  }
  
  # Combine all data frames into one
  df_input <- dplyr::bind_rows(df_list)
  
  return(df_input)
}

separate_culture_period <- function(df) {
  
  if (!"sampling" %in% colnames(df)) {
    stop("No culture period to separate: 'sampling' column is missing.")
  }
  
  # Define common levels for the period factor
  common_levels <- c("DP", "BS", "CP", "AH", "EM", "Other")
  
  # Add 'period' column based on sampling (date)
  sw_imta_mono_2023 <- df %>%
    filter(year == "2023", water == "sw", type %in% c("imta", "mono")) %>%
    mutate(period = case_when(
      sampling %in% c("2nd", "3rd", "4th", "5th", "6th", "7th") ~ "CP",
      sampling == "1st" ~ "BS",
      sampling == "8th" ~ "AH",
      TRUE ~ "Other")) %>%
    mutate(period = factor(period, levels = common_levels))  # Set common levels
  
  sw_photo_2023 <- df %>%
    filter(year == "2023", water == "sw", type == "photo") %>%
    mutate(period = case_when(
      sampling %in% c("2nd", "3rd", "4th", "5th", "6th", "7th", "8th") ~ "CP",
      sampling == "1st" ~ "BS",
      TRUE ~ "Other")) %>%
    mutate(period = factor(period, levels = common_levels))  # Set common levels
  
  fw_imta_2023 <- df %>%
    filter(year == "2023", water == "fw", type == "imta") %>%
    mutate(period = case_when(
      sampling %in% c("2nd", "3rd", "4th", "5th", "6th") ~ "CP",
      sampling == "1st" ~ "BS",
      TRUE ~ "Other")) %>%
    mutate(period = factor(period, levels = common_levels))  # Set common levels
  
  sw_imta_2024 <- df %>%
    filter(year == "2024", water == "sw", type == "imta") %>%
    mutate(period = case_when(
      sampling %in% c("3rd", "4th", "5th", "6th", "7th", "8th", "9th") ~ "CP",
      sampling == "2nd" ~ "BS",
      sampling %in% c("1st", "10th") ~ "DP",
      TRUE ~ "Other")) %>%
    mutate(period = factor(period, levels = common_levels))  # Set common levels
  
  sw_photo_2024 <- df %>%
    filter(year == "2024", water == "sw", type == "photo") %>%
    mutate(period = case_when(
      sampling %in% c("1st", "2nd", "3rd", "4th") ~ "BS",
      sampling %in% c("7th", "8th") ~ "EM",
      sampling %in% c("5th", "6th") ~ "CP",
      TRUE ~ "Other")) %>%
    mutate(period = factor(period, levels = common_levels))  
  
  fw_imta_2024 <- df %>%
    filter(year == "2024", water == "fw", type == "imta") %>%
    mutate(period = case_when(
      sampling %in% c("2nd", "3rd", "4th", "5th", "6th", "7th", "8th") ~ "CP",
      sampling == "1st" ~ "BS",
      sampling == "9th" ~ "DP",
      TRUE ~ "Other")) %>%
    mutate(period = factor(period, levels = common_levels)) 
  
  fw_mono_prawn <- df %>%
    filter(water == "fw", type == "mono", species == "prawn") %>%
    mutate(period = case_when(
      sampling == "dry" ~ "DP",
      TRUE ~ "CP")) %>%
    mutate(period = factor(period, levels = common_levels)) 
  
  fw_mono_tilapia <- df %>%
    filter(water == "fw", type == "mono", species == "tilapia") %>%
    mutate(period = case_when(
      sampling == "dry" ~ "DP",
      group == "T1" & sampling == "1st" ~ "EM",
      TRUE ~ "CP")) %>%
    mutate(period = factor(period, levels = common_levels)) 
  
  if ("sampling_date_start" %in% colnames(df)) {
    sw_mono_2024.2025 <- df %>%
      filter(year %in% c("2024", "2025"), water == "sw", type == "mono") %>%
      mutate(period = case_when(
        name == "FL T1" & sampling_date_start >= as.Date("2023-07-15") & sampling_date_start < as.Date("2024-06-05") ~ "CP",
        name == "FL T1" & sampling_date_start >= as.Date("2024-06-05") & sampling_date_start < as.Date("2024-06-08") ~ "EM",
        name == "FL T1" & sampling_date_start >= as.Date("2024-06-08") & sampling_date_start < as.Date("2024-06-17") ~ "DP",
        name == "FL T1" & sampling_date_start >= as.Date("2024-06-17") & sampling_date_start < as.Date("2024-07-15") ~ "EM",
        name == "FL T1" & sampling_date_start >= as.Date("2024-07-15") & sampling_date_start <= as.Date("2025-04-15") ~ "CP",
        name == "FL T1" & sampling_date_start > as.Date("2025-04-15") & sampling_date_start < as.Date("2025-05-17") ~ "DP",
        name == "FL T1" & sampling_date_start >= as.Date("2025-05-17") & sampling_date_start < as.Date("2025-08-16") ~ "EM",
        name == "FL T1" & sampling_date_start >= as.Date("2025-08-16") ~ "CP",
        
        name == "FL T2" & sampling_date_start < as.Date("2024-02-23") ~ "EM",
        name == "FL T2" & sampling_date_start >= as.Date("2024-02-23") & sampling_date_start <= as.Date("2025-06-11") ~ "CP",
        name == "FL T2" & sampling_date_start > as.Date("2025-06-11") & sampling_date_start < as.Date("2025-06-17") ~ "EM",
        name == "FL T2" & sampling_date_start >= as.Date("2025-06-17") & sampling_date_start < as.Date("2025-08-02") ~ "DP",
        name == "FL T2" & sampling_date_start >= as.Date("2025-08-02") ~ "EM",
        
        name == "FL T3" & sampling_date_start < as.Date("2024-06-20") ~ "EM",
        name == "FL T3" & sampling_date_start >= as.Date("2024-06-20") ~ "CP",
        
        name == "LB T1" & sampling_date_start < as.Date("2023-11-22") ~ "EM",
        name == "LB T1" & sampling_date_start >= as.Date("2023-11-22") & sampling_date_start <= as.Date("2025-02-21") ~ "CP",
        name == "LB T1" & sampling_date_start > as.Date("2025-02-21") & sampling_date_start < as.Date("2025-03-14") ~ "EM",
        name == "LB T1" & sampling_date_start >= as.Date("2025-03-14") & sampling_date_start <= as.Date("2025-07-14") ~ "DP",
        name == "LB T1" & sampling_date_start > as.Date("2025-07-14") & sampling_date_start < as.Date("2025-08-04") ~ "EM", #not sure
        name == "LB T1" & sampling_date_start >= as.Date("2025-08-04") ~ "CP",
        
        name == "LB C" ~ "EM",
        TRUE ~ "")) %>%
      mutate(period = factor(period, levels = common_levels)) 
  } else if ("sampling_date" %in% colnames(df)) {
    sw_mono_2024.2025 <- df %>%
      filter(year %in% c("2024", "2025"), water == "sw", type == "mono") %>%
      mutate(period = case_when(
        name == "FL T1" & sampling_date >= as.Date("2023-07-15") & sampling_date < as.Date("2024-06-05") ~ "CP",
        name == "FL T1" & sampling_date >= as.Date("2024-06-05") & sampling_date < as.Date("2024-06-08") ~ "EM",
        name == "FL T1" & sampling_date >= as.Date("2024-06-08") & sampling_date < as.Date("2024-06-17") ~ "DP",
        name == "FL T1" & sampling_date >= as.Date("2024-06-17") & sampling_date < as.Date("2024-07-15") ~ "EM",
        name == "FL T1" & sampling_date >= as.Date("2024-07-15") & sampling_date <= as.Date("2025-04-15") ~ "CP",
        name == "FL T1" & sampling_date > as.Date("2025-04-15") & sampling_date < as.Date("2025-05-17") ~ "DP",
        name == "FL T1" & sampling_date >= as.Date("2025-05-17") & sampling_date < as.Date("2025-08-16") ~ "EM",
        name == "FL T1" & sampling_date >= as.Date("2025-08-16") ~ "CP",
        
        name == "FL T2" & sampling_date < as.Date("2024-02-23") ~ "EM",
        name == "FL T2" & sampling_date >= as.Date("2024-02-23") & sampling_date <= as.Date("2025-06-11") ~ "CP",
        name == "FL T2" & sampling_date > as.Date("2025-06-11") & sampling_date < as.Date("2025-06-17") ~ "EM",
        name == "FL T2" & sampling_date >= as.Date("2025-06-17") & sampling_date < as.Date("2025-08-02") ~ "DP",
        name == "FL T2" & sampling_date >= as.Date("2025-08-02") ~ "EM",
        
        name == "FL T3" & sampling_date < as.Date("2024-06-20") ~ "EM",
        name == "FL T3" & sampling_date >= as.Date("2024-06-20") ~ "CP",
        
        name == "LB T1" & sampling_date < as.Date("2023-11-22") ~ "EM",
        name == "LB T1" & sampling_date >= as.Date("2023-11-22") & sampling_date <= as.Date("2025-02-21") ~ "CP",
        name == "LB T1" & sampling_date > as.Date("2025-02-21") & sampling_date < as.Date("2025-03-14") ~ "EM",
        name == "LB T1" & sampling_date >= as.Date("2025-03-14") & sampling_date <= as.Date("2025-07-14") ~ "DP",
        name == "LB T1" & sampling_date > as.Date("2025-07-14") & sampling_date < as.Date("2025-08-04") ~ "EM", #not sure
        name == "LB T1" & sampling_date >= as.Date("2025-08-04") ~ "CP",
        
        name == "LB C" ~ "EM",
        TRUE ~ "")) %>%
      mutate(period = factor(period, levels = common_levels)) 
    
  }
  
  # Combine the data frames
  all <- bind_rows(sw_imta_mono_2023, sw_photo_2023, fw_imta_2023, 
                   sw_imta_2024, sw_photo_2024, fw_imta_2024, 
                   fw_mono_prawn, fw_mono_tilapia,
                   sw_mono_2024.2025)
  
  return(all)
}


## input value -------------------------------------------------------------------


### Rmd ---------------------------------------------------------------------


en_dash <- "\u2013"
plus_minus <- " \u00B1 "
rmd_co2 <- "CO~2~ fluxes (mmol m^-2^ d^-1^)"
rmd_ch4 <- "CH~4~ fluxes (µmol m^-2^ d^-1^)"
rmd_n2o <- "N~2~O fluxes (µmol m^-2^ d^-1^)"
rmd_ghg_co2e <- "Total CO~2~e fluxes (g CO~2~e m^-2^ d^-1^)"
rmd_rday_c <- "Ecosystem Respiration (g C m^-2^ d^-1^)"
rmd_gpp_c <- "Gross primary production (g C m^-2^ d^-1^)"
rmd_nep_c <- "Net ecosystem metabolism (g C m^-2^ d^-1^)"
rmd_sa_c <- "Sediment accumulation rate (g C m^-2^ d^-1^)"
rmd_doc <- "DOC (mg L^-1^)"
rmd_dic <- "DIC (mg L^-1^)"
rmd_poc <- "POC (mg L^-1^)"
rmd_budget <- "Mg CO~2~e ha^-1^ yr^-1^"


### Most used parameters ----------------------------------------------------

pq <- 1.25 #O2 to CO2
ch4_mass <- 16.04 #g mol-1
n2o_mass <- 44.013 #g mol-1
p_mass <- 30.973762 #g mol-1
n_mass <- 14.0067 #g mol-1
n2_mass <- 14.0067*2 #g mol-1
co2_mass <- 44.01 #g mol-1
c_mass <- 12.01 #g mol-1
n_mass <-  14.0067 #g mol-1
o2_molar_mass <- 32 #g mol-1
ch4_gwp100_ar6 <- 27 #AR6
n2o_gwp100_ar6 <- 273 #AR6
ch4_gwp100_ar5 <- 28
n2o_gwp100_ar5 <- 265
floating_chamber_area <- 0.07 #m2
soil_chamber_area <- 0.07 #m2
floating_chamber_upper_volume <- 6.4 #L
soil_chamber_upper_volume <- 6.4 #L
floating_chamber_lower_height <- 0.02 #m
alter_direction <- -1
half_day <- 0.5
peri_chla_to_c <- 67 #Riemann1989Carbon
phyto_chla_to_c <- 67 #Riemann1989Carbon
benthic_chla_to_c <- 47.63 #DeJonge1980Fluctuations
oc_to_ww_organicTissue <- 10 #Opitz1996
dw_to_ww_organicTissue <- 4.5 #Opitz1996
c_to_dw_producer <- 3.33 #Opitz1996
dw_to_ww_producer <- 5.71 #Opitz1996
ideal_gas_constant <- 0.082057 #L atm K-1 mol-1

### Unit transformation-------------------------------------------------------------------------

µmol_to_mmol <- (1/1000)
mmol_to_µmol <- 1000
ppb_to_ppm <- (1/1000)
ppm_to_ppb <- 1000
s_to_day <- (1/86400)
day_to_s <- 86400
s_to_hr <- (1/3600)
hr_to_s <- 3600
hr_to_day <- (1/24)
day_to_hr <- 24
day_to_yr <- (1/365)
yr_to_day <- 365
hr_to_yr <- hr_to_day * day_to_yr
yr_to_hr <- 1/hr_to_yr
µg_to_g <- (1/1000000)
µg_to_kg <- 1e-9
g_to_µg <- (1000000)
µg_to_mg <- (1/1000)
mg_to_µg <- 1000
mg_to_g <- (1/1000)
mg_to_kg <- (1/1000000)
mg_to_Mg <- (1/1000000000)
mg_to_Gg <- (1/1000000000000)
Mg_to_mg <- 1000000000
g_to_mg <- 1000
kg_to_Mg <- (1/1000)
kg_to_mg <- 1000000
Mg_to_kg <- 1000
g_to_kg <- (1/1000)
kg_to_g <- 1000
m3_to_L <- 1000
L_to_m3 <- (1/1000)
g_to_Mg <- (1/1000000)
cm_to_m <- (1/100)
cm2_to_m2 <- (1/10000)
cm3_to_m3 <- (1/1000000)
cm3_to_L <- (1/1000)
m2_to_cm2 <- 10000
m2_to_ha <- (1/10000)
ha_to_m2 <- (10000)
gm2d_to_Mghayr <- (g_to_Mg)*(1/m2_to_ha)*(1/day_to_yr)
kgm2d_to_Mghayr <- (kg_to_Mg)*(1/m2_to_ha)*(1/day_to_yr)
gm2hr_to_Mghayr <- (g_to_Mg)*(1/m2_to_ha)*(1/hr_to_day)*(1/day_to_yr)
gm2_to_Mgha <- (g_to_Mg)*(1/m2_to_ha)
oc_to_co2e <- 44.01/12.01 #OC to CO2e
co2e_to_c <- 12.01/44.01
kwh_to_kg_co2e <- 0.495 #kg CO2e per kWh
o2_to_c <- 0.375
ratio_to_percent <- 100


### Map ---------------------------------------------------------------------


world <- ne_countries(scale = "medium", returnclass = "sf")
map_taiwan <- ggplot(data = world) +
  geom_sf() +
  xlab(expression(bold(paste("Longitude")))) +
  ylab(expression(bold(paste("Latitude")))) +
  coord_sf(xlim = c(118.50, 123.50), ylim = c(21.00, 26.00), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotate(geom = "text", x = 121, y = 24, label = "Taiwan",
           fontface = "italic",  size = 5) +
  theme_bw(base_size=16)

### Axis label-----

lab_co2 <- expression(bold(paste("C",O[2], " fluxes (mg ", m^-2~h^-1, ")")))
lab_ch4 <- expression(bold(paste("C",H[4], " fluxes (µg ", m^-2~h^-1, ")")))
lab_ch4_mg <- expression(bold(paste("C",H[4], " fluxes (mg ", m^-2~h^-1, ")")))
lab_n2o <- expression(bold(paste(N[2], "O fluxes (µg ", m^-2~h^-1, ")")))
lab_n2o_mg <- expression(bold(paste(N[2], "O fluxes (mg ", m^-2~h^-1, ")")))
lab_ghg <- expression(bold(paste("Total C",O[2], "e fluxes (g"~m^-2~d^-1, ")")))
lab_rday_c <- expression(bold(paste("ER"~"(","g"~"C"~m^-2~d^-1, ")")))
lab_gpp_c <- expression(bold(paste("GPP"~"(","g"~"C"~m^-2~d^-1, ")")))
lab_nep_c <- expression(bold(paste("NEP"~"(","g"~"C"~m^-2~d^-1, ")")))
lab_rday_o2 <- expression(bold(paste("ER"~"(","g"~O[2]~m^-2~d^-1, ")")))
lab_gpp_o2 <- expression(bold(paste("GPP"~"(","g"~O[2]~m^-2~d^-1, ")")))
lab_nep_o2 <- expression(bold(paste("NEP"~"(","g"~O[2]~m^-2~d^-1, ")")))
lab_sa <- expression(bold(paste("Accumulation rate"~"(","g"~"C"~m^-2~d^-1, ")")))
lab_doc_flux_g <- expression(bold(paste("DOC"~"(","g"~"C"~m^-2~d^-1, ")")))
lab_dic_flux_g <- expression(bold(paste("DIC"~"(","g"~"C"~m^-2~d^-1, ")")))
lab_poc_flux_g <- expression(bold(paste("POC"~"(","g"~"C"~m^-2~d^-1, ")")))
lab_doc_flux_mg <- expression(bold(paste("DOC"~"(","mg"~"C"~m^-2~d^-1, ")")))
lab_dic_flux_mg <- expression(bold(paste("DIC"~"(","mg"~"C"~m^-2~d^-1, ")")))
lab_poc_flux_mg <- expression(bold(paste("POC"~"(","mg"~"C"~m^-2~d^-1, ")")))
lab_doc_flux_µg <- expression(bold(paste("DOC"~"(","µg"~"C"~m^-2~d^-1, ")")))
lab_dic_flux_µg <- expression(bold(paste("DIC"~"(","µg"~"C"~m^-2~d^-1, ")")))
lab_poc_flux_µg <- expression(bold(paste("POC"~"(","µg"~"C"~m^-2~d^-1, ")")))
lab_temp <- expression(bold(paste("Temperature"~"(˚C)")))
lab_ec_µs <- expression(bold(paste("Electric conductivity"~"(µs"~cm^-1, ")")))
lab_ec_ms <- expression(bold(paste("Electric conductivity"~"(ms"~cm^-1, ")")))
lab_do_w <- expression(bold(paste("DO concentration"~"(mg"~L^-1, ")")))
lab_do_pc <- expression(bold(paste("DO saturation (%)")))
lab_ph <- expression(bold(paste("pH")))
lab_ntu <- expression(bold(paste("Turbidity (NTU)")))
lab_sal <- expression(bold(paste("Salintiy")))
lab_ss <- expression(bold(paste("Suspended solid"~"(mg"~L^-1, ")")))
lab_chla <- expression(bold(paste("chl",italic("a")," concentration"~"(µg"~L^-1, ")")))
lab_doc_conc <- expression(bold(paste("DOC"~"(mg"~L^-1, ")")))
lab_dic_conc <- expression(bold(paste("DIC"~"(mg"~L^-1, ")")))
lab_poc_conc <- expression(bold(paste("POC"~"(mg"~L^-1, ")")))
lab_cb_co2e <- expression(bold(paste("Carbon budget"~"(","Tonne"~CO[2],"e"~ha^-1~yr^-1, ")")))
lab_cb_c <- expression(bold(paste("Carbon budget"~"(","Tonne C"~ha^-1, ")")))
lab_feed_c <- expression(bold(paste("Feed input"~"(","g"~"C"~m^-2~d^-1, ")")))
lab_orp <- expression(bold(paste("ORP (mV)")))
lab_dbd <- expression(bold(paste("Dry bulk density (g" ~ cm^3, ")")))
lab_smc <- expression(bold(paste("Mass wetness (%)")))
lab_om <- expression(bold(paste("OM content (%)")))
lab_silt <- expression(bold(paste("Silt/clay content (%)")))
lab_median <- expression(bold(paste("Median grain size (mm)")))
lab_sort <- expression(bold(paste("Sorting coefficient")))
lab_po4_µM <- expression(bold(paste("PO"[4]^"-", "-P" ~ "(µM)")))
lab_no3_µM <- expression(bold(paste("NO"[3]^"-", "-N" ~ "(µM)")))
lab_no2_µM <- expression(bold(paste("NO"[2]^"-", "-N" ~ "(µM)")))
lab_nh4_µM <- expression(bold(paste("NH"[4]^"+", "-N" ~ "(µM)")))

# temporary-----

#convert_ghg_unit("0.00211 ± 0.00281",mass = "mmol", area = "m2", time = "day", ghg = "ch4", digits = 2)

#calculate_total_co2e(co2 = 4.01591, ch4 = 0.00141, n2o = 0.00303)
