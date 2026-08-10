# PrawnCarbonBudget.R (REVISED VERSION)
# ====================
# Calculate carbon budgets for freshwater prawn monoculture by culture period
# Output: One row per pond per culture period with properly calculated cb_ values
#
# ============================================================================

# Section 1: Load Base Data ==================================================

cat("Loading dependencies and base data...\n")

source('R/00_R_preparation.R')

# Load BASE dataframes with rates (BEFORE aggregation into raw_budget)
do           <- readRDS("data/do.rds")
ghg_co2e     <- readRDS("data/ghg_co2e.rds")
sa           <- readRDS("data/sa.rds")
doc          <- readRDS("data/doc.rds")
poc          <- readRDS("data/poc.rds")
feed         <- readRDS("data/feed.rds")
info         <- readRDS("data/info.rds")
nutrient     <- readRDS("data/nutrient.rds")
animals      <- readRDS("data/animals.rds")
weather_year <- readRDS("data/weather_year.rds")

# Load pt_info with culture period definitions
pt_info <- read.xlsx('data/PT_info.xlsx')

cat("✓ Data loaded\n\n")

# Section 2: Process pt_info ==================================================

cat("Processing pt_info...\n")

pt_info <- read.xlsx('data/PT_info.xlsx')

# Convert dates and create identifiers
pt_info <- pt_info %>%
  mutate(
    stock_date = as.Date(stock_date, origin = "1899-12-30"),
    harvest_date = as.Date(harvest_date, origin = "1899-12-30"),
    culture_period_id = paste(name, culture_period, sep = "_")
  )

# Get area and depth information from info dataframe
pt_area <- info %>%
  filter(year == "2024", water == "fw", type == "mono", species == "prawn", group != "T1") %>%
  mutate(name = paste(species, group),
         name = dplyr::recode(name,
                              'prawn T2' = 'PT2',
                              'prawn T3' = 'PT3',
                              'prawn T4' = 'PT4')) %>%
  select(name, area_m2, depth_m)

# Merge with pt_info
pt_info <- merge(pt_info, pt_area, by = "name", all.x = T)

# Calculate additional metrics
pt_info <- pt_info %>%
  mutate(
    input_rate = total_feed_input_kg / culture_day,
    stock_density = stock_ind / area_m2
  )

cat("✓ pt_info processed:", nrow(pt_info), "culture periods\n\n")

# Section 3: Calculate Each Component by Culture Period ======================

## 3.1 DO Metabolism (cb_gpp, cb_rday, cb_nep) ==============================

cat("Calculating DO metabolism...\n")

do_cp <- do %>%
  filter(water == "fw", type == "mono", species == "prawn",
         sample == "water") %>%
  separate_culture_period() %>%
  filter(period == "CP") %>%
  mutate(name = dplyr::recode(name,
                              'prawn T2' = 'PT2',
                              'prawn T3' = 'PT3',
                              'prawn T4' = 'PT4')) %>%
  inner_join(pt_info %>% select(name, culture_period, culture_period_id,
                                 stock_date, harvest_date, culture_day),
             by = "name", relationship = "many-to-many") %>%
  filter(sampling_date >= stock_date, sampling_date <= harvest_date) %>%
  group_by(culture_period_id, name, culture_period, culture_day) %>%
  summarise(
    n_do_samplings = n(),
    # Average the RATES (c_X in g m⁻² d⁻¹)
    c_gpp_avg = mean(c_gpp, na.rm = TRUE),
    c_rday_avg = mean(c_rday, na.rm = TRUE),
    c_nep_avg = mean(c_nep, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Recalculate cb_ using THIS culture period's culture_day
    cb_gpp = c_gpp_avg * culture_day * gm2_to_Mgha,
    cb_rday = c_rday_avg * culture_day * gm2_to_Mgha,
    cb_nep = c_nep_avg * culture_day * gm2_to_Mgha
  ) %>%
  select(culture_period_id, name, culture_period, n_do_samplings,
         cb_gpp, cb_rday, cb_nep)

cat("✓ DO metabolism calculated for", nrow(do_cp), "culture periods\n")

## 3.2 GHG Emissions (cb_ghg, cb_ghg_in, cb_ghg_out) =========================

cat("Calculating GHG emissions...\n")

# Note: ghg_co2e already has cb_ghg calculated, but with wrong culture_day
# Need to recalculate or extract the underlying rate

ghg_cp <- ghg_co2e %>%
  filter(water == "fw", type == "mono", species == "prawn",
         con %in% c("light", "day"), sample == "water") %>%
  separate_culture_period() %>%
  filter(period == "CP") %>%
  mutate(name = dplyr::recode(name,
                              'prawn T2' = 'PT2',
                              'prawn T3' = 'PT3',
                              'prawn T4' = 'PT4')) %>%
  inner_join(pt_info %>% select(name, culture_period, culture_period_id,
                                 stock_date, harvest_date, culture_day),
             by = "name", relationship = "many-to-many") %>%
  filter(sampling_date >= stock_date, sampling_date <= harvest_date) %>%
  group_by(culture_period_id, name, culture_period, culture_day) %>%
  summarise(
    n_ghg_samplings = n(),
    # For GHG, average the existing cb_ghg values (they're already calculated)
    # Then recalculate for the specific culture period
    # Use ghg_ch4 rate and recalculate
    ghg_ch4_avg = mean(ghg_ch4, na.rm = TRUE),  # μg m⁻² h⁻¹
    .groups = "drop"
  ) %>%
  mutate(
    # Recalculate cb_ghg from CH4 flux
    yr_culture = culture_day * day_to_yr,
    cb_ghg = ghg_ch4_avg * µg_to_g * gm2hr_to_Mghayr * yr_culture * (c_mass / ch4_mass),
    cb_ghg_in = ifelse(cb_ghg < 0, abs(cb_ghg), 0),
    cb_ghg_out = ifelse(cb_ghg > 0, cb_ghg, 0)
  ) %>%
  select(culture_period_id, name, culture_period, n_ghg_samplings,
         cb_ghg, cb_ghg_in, cb_ghg_out)

cat("✓ GHG emissions calculated for", nrow(ghg_cp), "culture periods\n")

## 3.3 Sediment Accumulation (cb_sa) =========================================

cat("Calculating sediment accumulation...\n")

sa_cp <- sa %>%
  filter(water == "fw", type == "mono", species == "prawn", trap == "old") %>%
  separate_culture_period() %>%
  filter(period == "CP") %>%
  mutate(name = dplyr::recode(name,
                              'prawn T2' = 'PT2',
                              'prawn T3' = 'PT3',
                              'prawn T4' = 'PT4')) %>%
  inner_join(pt_info %>% select(name, culture_period, culture_period_id,
                                 stock_date, harvest_date, culture_day),
             by = "name", relationship = "many-to-many") %>%
  filter(sampling_date_start >= stock_date, sampling_date_start <= harvest_date) %>%
  group_by(culture_period_id, name, culture_period, culture_day) %>%
  summarise(
    n_sa_samplings = n(),
    c_sa_avg = mean(c_sa, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    cb_sa = c_sa_avg * culture_day * gm2_to_Mgha
  ) %>%
  select(culture_period_id, name, culture_period, n_sa_samplings, cb_sa)

cat("✓ Sediment accumulation calculated for", nrow(sa_cp), "culture periods\n")

## 3.4 Feed (cb_feed) - From pt_info =========================================

cat("Calculating feed carbon...\n")

# Get feed C content from feed dataframe
feed_c_percent <- feed %>%
  filter(water == "fw", type == "mono", species == "prawn") %>%
  summarise(c_percent = mean(c_percent, na.rm = TRUE)) %>%
  pull(c_percent)

# If no data, use default
if (length(feed_c_percent) == 0 || is.na(feed_c_percent)) {
  feed_c_percent <- 45  # Default value
  cat("  Using default feed C content: 45%\n")
} else {
  cat("  Using measured feed C content:", round(feed_c_percent, 2), "%\n")
}

feed_cp <- pt_info %>%
  select(culture_period_id, name, culture_period, area_m2,
         total_feed_input_kg, culture_day) %>%
  distinct() %>%
  mutate(
    cb_feed = (total_feed_input_kg * (feed_c_percent/100) * kg_to_Mg) / (area_m2 * m2_to_ha)
  ) %>%
  select(culture_period_id, cb_feed)

cat("✓ Feed carbon calculated for", nrow(feed_cp), "culture periods\n")

## 3.5 Stock/Harvest Biomass - From pt_info ==================================

cat("Calculating stock/harvest biomass carbon...\n")

prawn_stock_c_percent <- animals %>%
  filter(water == "fw", type == "mono", species == "prawn", status == "stocked") %>%
  summarise(c_percent = mean(C_percent, na.rm = TRUE)) %>%
  pull(c_percent)
prawn_harvest_c_percent <- animals %>%
  filter(water == "fw", type == "mono", species == "prawn", status == "harvested") %>%
  summarise(c_percent = mean(C_percent, na.rm = TRUE)) %>%
  pull(c_percent)

animals_cp <- pt_info %>%
  select(culture_period_id, name, culture_period, area_m2,
         stock_ind, stock_ind_g, harvest_total_kg) %>%
  distinct() %>%
  mutate(
    stock_total_kg = (stock_ind * stock_ind_g) / 1000,
    cb_stock = (stock_total_kg * (prawn_stock_c_percent/100) * kg_to_Mg) / (area_m2 * m2_to_ha),
    cb_harvest = (harvest_total_kg * (prawn_harvest_c_percent/100) * kg_to_Mg) / (area_m2 * m2_to_ha)
  ) %>%
  select(culture_period_id, cb_stock, cb_harvest)

cat("✓ Stock/harvest carbon calculated for", nrow(animals_cp), "culture periods\n")

## 3.6 Water Exchange (cb_inflow, cb_outflow) ================================

cat("Calculating water exchange carbon...\n")

# 3.6a: Continuous inflow (from "in" source) - DOC/DIC
cat("  Processing continuous inflow (DOC/DIC)...\n")

inflow_doc_cp <- doc %>%
  filter(water == "fw", type == "mono", species == "prawn", source == "in") %>%
  separate_culture_period() %>%
  filter(period == "CP") %>%
  mutate(name = dplyr::recode(name,
                              'prawn T2' = 'PT2',
                              'prawn T3' = 'PT3',
                              'prawn T4' = 'PT4')) %>%
  inner_join(pt_info %>% select(name, culture_period, culture_period_id,
                                 stock_date, harvest_date, culture_day),
             by = "name", relationship = "many-to-many") %>%
  filter(sampling_date >= stock_date, sampling_date <= harvest_date) %>%
  group_by(culture_period_id, name, culture_period, culture_day) %>%
  summarise(
    c_doc_in_avg = mean(c_doc, na.rm = TRUE),
    c_dic_in_avg = mean(c_dic, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    cb_doc_in = c_doc_in_avg * culture_day * gm2_to_Mgha,
    cb_dic_in = c_dic_in_avg * culture_day * gm2_to_Mgha
  ) %>%
  select(culture_period_id, cb_doc_in, cb_dic_in)

# Continuous inflow - POC
inflow_poc_cp <- poc %>%
  filter(water == "fw", type == "mono", species == "prawn", source == "in") %>%
  separate_culture_period() %>%
  filter(period == "CP") %>%
  mutate(name = dplyr::recode(name,
                              'prawn T2' = 'PT2',
                              'prawn T3' = 'PT3',
                              'prawn T4' = 'PT4')) %>%
  inner_join(pt_info %>% select(name, culture_period, culture_period_id,
                                 stock_date, harvest_date, culture_day),
             by = "name", relationship = "many-to-many") %>%
  filter(sampling_date >= stock_date, sampling_date <= harvest_date) %>%
  group_by(culture_period_id, name, culture_period, culture_day) %>%
  summarise(
    c_poc_in_avg = mean(c_poc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    cb_poc_in = c_poc_in_avg * culture_day * gm2_to_Mgha
  ) %>%
  select(culture_period_id, cb_poc_in)

# Combine continuous inflow
inflow_continuous_cp <- merge(inflow_doc_cp, inflow_poc_cp,
                               by = "culture_period_id", all = TRUE)

cat("  ✓ Continuous inflow calculated\n")

# 3.6b: Initial water carbon (FIRST sampling) - DOC/POC
cat("  Processing initial water carbon (first sampling)...\n")

inflow_start_cp <- doc %>%
  filter(water == "fw", type == "mono", species == "prawn", source == "water") %>%
  mutate(name = dplyr::recode(name,
                              'prawn T2' = 'PT2',
                              'prawn T3' = 'PT3',
                              'prawn T4' = 'PT4')) %>%
  inner_join(pt_info %>% select(name, culture_period, culture_period_id,
                                 stock_date, harvest_date, area_m2, depth_m),
             by = "name", relationship = "many-to-many") %>%
  filter(sampling_date >= stock_date, sampling_date <= harvest_date) %>%
  group_by(culture_period_id) %>%
  filter(sampling_date == min(sampling_date)) %>%  # FIRST sampling
  ungroup() %>%
  # Re-join to get area and depth for calculation
  inner_join(pt_info %>% select(culture_period_id, area_m2, depth_m),
             by = "culture_period_id") %>%
  group_by(culture_period_id) %>%
  summarise(
    doc_start = mean(doc, na.rm = TRUE),
    area_m2 = first(area_m2.y),
    depth_m = first(depth_m.y),
    .groups = "drop"
  ) %>%
  mutate(
    pond_volume_L = area_m2 * depth_m * m3_to_L,
    cb_doc_in_start = (doc_start * pond_volume_L * mg_to_Mg) / (area_m2 * m2_to_ha)
  ) %>%
  select(culture_period_id, cb_doc_in_start)

# POC start
inflow_start_poc_cp <- poc %>%
  filter(water == "fw", type == "mono", species == "prawn", source == "water") %>%
  mutate(name = dplyr::recode(name,
                              'prawn T2' = 'PT2',
                              'prawn T3' = 'PT3',
                              'prawn T4' = 'PT4')) %>%
  inner_join(pt_info %>% select(name, culture_period, culture_period_id,
                                 stock_date, harvest_date),
             by = "name", relationship = "many-to-many") %>%
  filter(sampling_date >= stock_date, sampling_date <= harvest_date) %>%
  group_by(culture_period_id) %>%
  filter(sampling_date == min(sampling_date)) %>%  # FIRST sampling
  ungroup() %>%
  inner_join(pt_info %>% select(culture_period_id, area_m2, depth_m),
             by = "culture_period_id") %>%
  group_by(culture_period_id) %>%
  summarise(
    poc_start = mean(poc, na.rm = TRUE),
    area_m2 = first(area_m2.y),
    depth_m = first(depth_m.y),
    .groups = "drop"
  ) %>%
  mutate(
    pond_volume_L = area_m2 * depth_m * m3_to_L,
    cb_poc_in_start = (poc_start * pond_volume_L * mg_to_Mg) / (area_m2 * m2_to_ha)
  ) %>%
  select(culture_period_id, cb_poc_in_start)

# Combine initial water carbon
inflow_start_all_cp <- merge(inflow_start_cp, inflow_start_poc_cp,
                              by = "culture_period_id", all = TRUE)

cat("  ✓ Initial water carbon calculated\n")

# 3.6c: Continuous outflow - Set equal to continuous inflow
cat("  Setting continuous outflow equal to continuous inflow...\n")

# Create outflow by copying inflow values
outflow_continuous_cp <- inflow_continuous_cp %>%
  mutate(
    cb_doc_out = cb_doc_in,
    cb_dic_out = cb_dic_in,
    cb_poc_out = cb_poc_in
  ) %>%
  select(culture_period_id, cb_doc_out, cb_dic_out, cb_poc_out)

cat("  ✓ Continuous outflow set equal to inflow\n")

# 3.6d: Final water carbon (LAST sampling) - DOC/POC
cat("  Processing final water carbon (last sampling)...\n")

outflow_end_cp <- doc %>%
  filter(water == "fw", type == "mono", species == "prawn", source == "water") %>%
  mutate(name = dplyr::recode(name,
                              'prawn T2' = 'PT2',
                              'prawn T3' = 'PT3',
                              'prawn T4' = 'PT4')) %>%
  inner_join(pt_info %>% select(name, culture_period, culture_period_id,
                                 stock_date, harvest_date),
             by = "name", relationship = "many-to-many") %>%
  filter(sampling_date >= stock_date, sampling_date <= harvest_date) %>%
  group_by(culture_period_id) %>%
  filter(sampling_date == max(sampling_date)) %>%  # LAST sampling
  ungroup() %>%
  inner_join(pt_info %>% select(culture_period_id, area_m2, depth_m),
             by = "culture_period_id") %>%
  group_by(culture_period_id) %>%
  summarise(
    doc_end = mean(doc, na.rm = TRUE),
    area_m2 = first(area_m2.y),
    depth_m = first(depth_m.y),
    .groups = "drop"
  ) %>%
  mutate(
    pond_volume_L = area_m2 * depth_m * m3_to_L,
    cb_doc_out_end = (doc_end * pond_volume_L * mg_to_Mg) / (area_m2 * m2_to_ha)
  ) %>%
  select(culture_period_id, cb_doc_out_end)

# POC end
outflow_end_poc_cp <- poc %>%
  filter(water == "fw", type == "mono", species == "prawn", source == "water") %>%
  mutate(name = dplyr::recode(name,
                              'prawn T2' = 'PT2',
                              'prawn T3' = 'PT3',
                              'prawn T4' = 'PT4')) %>%
  inner_join(pt_info %>% select(name, culture_period, culture_period_id,
                                 stock_date, harvest_date),
             by = "name", relationship = "many-to-many") %>%
  filter(sampling_date >= stock_date, sampling_date <= harvest_date) %>%
  group_by(culture_period_id) %>%
  filter(sampling_date == max(sampling_date)) %>%  # LAST sampling
  ungroup() %>%
  inner_join(pt_info %>% select(culture_period_id, area_m2, depth_m),
             by = "culture_period_id") %>%
  group_by(culture_period_id) %>%
  summarise(
    poc_end = mean(poc, na.rm = TRUE),
    area_m2 = first(area_m2.y),
    depth_m = first(depth_m.y),
    .groups = "drop"
  ) %>%
  mutate(
    pond_volume_L = area_m2 * depth_m * m3_to_L,
    cb_poc_out_end = (poc_end * pond_volume_L * mg_to_Mg) / (area_m2 * m2_to_ha)
  ) %>%
  select(culture_period_id, cb_poc_out_end)

# Combine final water carbon
outflow_end_all_cp <- merge(outflow_end_cp, outflow_end_poc_cp,
                             by = "culture_period_id", all = TRUE)

cat("  ✓ Final water carbon calculated\n")
cat("✓ Water exchange carbon calculated\n")

## 3.7 Rain (cb_rain) ========================================================

cat("Calculating rain carbon...\n")

# Extract overall average DOC, DIC, and POC content of rainwater (not by year)
rain_doc_avg <- doc %>%
  filter(source == "rainwater", water == "pd_xy") %>%
  summarise(doc = mean(doc, na.rm = TRUE),
            dic = mean(dic, na.rm = TRUE)) %>%
  pull(doc)

rain_dic_avg <- doc %>%
  filter(source == "rainwater", water == "pd_xy") %>%
  summarise(dic = mean(dic, na.rm = TRUE)) %>%
  pull(dic)

rain_poc_avg <- poc %>%
  filter(source == "rainwater", water == "pd_xy") %>%
  summarise(poc = mean(poc, na.rm = TRUE)) %>%
  pull(poc)

# Prepare monthly precipitation data for pd_xy zone
weather_monthly <- weather_year %>%
  filter(zone == "pd_xy") %>%
  mutate(rain_mm = as.numeric(rain_mm),
         month = as.integer(time),
         year = as.integer(year))

# Calculate rain carbon for each culture period
rain_cp <- pt_info %>%
  select(culture_period_id, name, culture_period, area_m2, culture_day,
         stock_date, harvest_date) %>%
  distinct() %>%
  filter(!is.na(stock_date), !is.na(harvest_date)) %>%
  rowwise() %>%
  mutate(
    # Generate sequence of year-month combinations for this culture period
    year_months = list({
      dates <- seq.Date(stock_date, harvest_date, by = "month")
      data.frame(
        year = as.integer(format(dates, "%Y")),
        month = as.integer(format(dates, "%m"))
      )
    }),
    # Calculate total precipitation for these months
    precipitation_total = {
      ym <- year_months
      sum(weather_monthly$rain_mm[
        paste(weather_monthly$year, weather_monthly$month) %in%
        paste(ym$year, ym$month)
      ], na.rm = TRUE)
    }
  ) %>%
  ungroup() %>%
  mutate(
    area_ha = area_m2 * m2_to_ha,
    rain_total_litre = precipitation_total * area_m2,  # mm * m2 = L
    cb_rain_doc = rain_total_litre * rain_doc_avg,
    cb_rain_poc = rain_total_litre * rain_poc_avg,
    cb_rain = (cb_rain_doc + cb_rain_poc) * mg_to_Mg / area_ha
  ) %>%
  select(culture_period_id, cb_rain)

# Add culture periods without harvest dates (set cb_rain = 0)
rain_cp_complete <- pt_info %>%
  select(culture_period_id) %>%
  distinct() %>%
  left_join(rain_cp, by = "culture_period_id") %>%
  mutate(cb_rain = ifelse(is.na(cb_rain), 0, cb_rain))

rain_cp <- rain_cp_complete

cat("✓ Rain carbon calculated for", nrow(rain_cp), "culture periods\n\n")

# Section 4: Merge All Components ============================================

cat("Merging all carbon budget components...\n")

budget_cp <- pt_info %>%
  select(culture_period_id, name, culture_period, stock_date, harvest_date,
         culture_day, area_m2, total_feed_input_kg, harvest_total_kg,
         stock_ind, stock_ind_g, stock_density, input_rate) %>%
  distinct() %>%
  left_join(do_cp %>% select(-name, -culture_period),
            by = "culture_period_id") %>%
  left_join(ghg_cp %>% select(-name, -culture_period),
            by = "culture_period_id") %>%
  left_join(sa_cp %>% select(-name, -culture_period),
            by = "culture_period_id") %>%
  left_join(feed_cp, by = "culture_period_id") %>%
  left_join(animals_cp, by = "culture_period_id") %>%
  left_join(inflow_continuous_cp, by = "culture_period_id") %>%
  left_join(inflow_start_all_cp, by = "culture_period_id") %>%
  left_join(outflow_continuous_cp, by = "culture_period_id") %>%
  left_join(outflow_end_all_cp, by = "culture_period_id") %>%
  left_join(rain_cp, by = "culture_period_id")

cat("✓ All components merged\n\n")

# Section 5: Calculate Totals =================================================

cat("Calculating budget totals...\n")

budget_cp <- budget_cp %>%
  mutate(
    # Replace NaN and NA with 0 for all cb_ columns
    across(starts_with("cb_"), ~ifelse(is.na(.) | is.nan(.), 0, .)),

    # Calculate total inflow/outflow
    cb_inflow_all = cb_doc_in + cb_poc_in + cb_doc_in_start + cb_poc_in_start,
    cb_outflow_all = cb_doc_out + cb_poc_out + cb_doc_out_end + cb_poc_out_end,

    # Calculate budget totals
    cb_input = cb_gpp + cb_ghg_in + cb_feed + cb_inflow_all + cb_stock + cb_rain,
    cb_output = cb_rday + cb_ghg_out + cb_sa + cb_outflow_all + cb_harvest,
    cb_budget = cb_input - cb_output,

    # Calculate contributions (%)
    con_gpp = (cb_gpp / cb_input) * 100,
    con_ghg_in = (cb_ghg_in / cb_input) * 100,
    con_feed = (cb_feed / cb_input) * 100,
    con_inflow = (cb_inflow_all / cb_input) * 100,
    con_stock = (cb_stock / cb_input) * 100,
    con_rain = (cb_rain / cb_input) * 100,

    con_rday = (cb_rday / cb_output) * 100,
    con_ghg_out = (cb_ghg_out / cb_output) * 100,
    con_sa = (cb_sa / cb_output) * 100,
    con_outflow = (cb_outflow_all / cb_output) * 100,
    con_harvest = (cb_harvest / cb_output) * 100
  ) 

cat("✓ Budget totals calculated\n\n")

# Section 6: Export Results ===================================================

cat("Exporting results...\n")

# Export to Excel

budget_in <- budget_cp %>% 
  filter(culture_period != "CP6") %>% 
  select(name, cb_gpp, cb_ghg_in, cb_feed, 
         cb_inflow_all, cb_stock, cb_rain, cb_input) 
budget_in <- descriptive_statistic(budget_in, 
                      vars = c(cb_gpp, cb_ghg_in, cb_feed, 
                               cb_inflow_all, cb_stock, cb_rain, cb_input),
                      groups = name) %>% select(name, contains("mean"))
write.xlsx(budget_in, file = 'output/prawn_budget_in.xlsx',
           rowNames = FALSE)

budget_out <- budget_cp %>% 
  filter(culture_period != "CP6") %>% 
  select(name, cb_rday, cb_ghg_out, cb_sa, 
         cb_outflow_all, cb_harvest, cb_output)
budget_out <- descriptive_statistic(budget_out, 
                                   vars = c(cb_rday, cb_ghg_out, cb_sa, 
                                            cb_outflow_all, cb_harvest, cb_output),
                                   groups = name) %>% select(name, contains("mean"))
write.xlsx(budget_out , file = 'output/prawn_budget_out.xlsx',
           rowNames = FALSE)

cat("✓ Results saved to: output/prawn_carbon_budget_by_culture_period_REVISED.xlsx\n\n")

# Section 7: Summary and Validation ===========================================

cat("=== Prawn Carbon Budget by Culture Period (REVISED) ===\n")
cat("Rows:", nrow(budget_cp), "culture periods\n\n")

cat("Summary by pond:\n")
summary_table <- budget_cp %>%
  select(name, culture_period, stock_date, harvest_date, culture_day,
         n_do_samplings, n_ghg_samplings, n_sa_samplings,
         cb_input, cb_output, cb_budget)
print(summary_table)

cat("\n=== Validation Checks ===\n")

# Check 1: Row count
cat("1. Row count:", nrow(budget_cp), "culture periods with data\n")

# Check 2: Culture day verification
cat("2. Culture day range:", min(budget_cp$culture_day, na.rm = TRUE),
    "to", max(budget_cp$culture_day, na.rm = TRUE), "days\n")

# Check 3: Budget balance
cat("3. Budget balance verification:\n")
balance_check <- budget_cp %>%
  mutate(balance_diff = abs(cb_budget - (cb_input - cb_output))) %>%
  summarise(max_diff = max(balance_diff, na.rm = TRUE))
cat("   Max difference:", round(balance_check$max_diff, 6), "Mg C/ha (should be ~0)\n")

# Check 4: Summary statistics
cat("\n4. Summary statistics (Mg C/ha):\n")
budget_summary <- budget_cp %>%
  summarise(
    mean_cb_input = mean(cb_input, na.rm = TRUE),
    mean_cb_output = mean(cb_output, na.rm = TRUE),
    mean_cb_budget = mean(cb_budget, na.rm = TRUE),
    mean_cb_feed = mean(cb_feed, na.rm = TRUE),
    mean_cb_harvest = mean(cb_harvest, na.rm = TRUE)
  )
print(budget_summary)

cat("\n=== Analysis Complete ===\n")

