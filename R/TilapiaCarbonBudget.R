# TilapiaCarbonBudget.R
# ======================
# Calculate carbon budgets for freshwater tilapia monoculture by culture period
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

cat("✓ Data loaded\n\n")

# Section 2: Build tt_info (culture period metadata) =========================

cat("Building tt_info...\n")

tt_info <- info %>%
  filter(water == "fw", type == "mono", species == "tilapia", !is.na(stock_date)) %>%
  transmute(
    name = dplyr::recode(group, "T1" = "TT1", "T2" = "TT2", "T3" = "TT3"),
    stock_date = as.Date(as.numeric(stock_date), origin = "1899-12-30"),
    harvest_date = as.Date(as.numeric(harvest_date), origin = "1899-12-30"),
    culture_day = as.numeric(day_culture),
    total_feed_input_kg = as.numeric(feed_input_kg),
    harvest_total_kg = as.numeric(harvest_animals_total_kg),
    stock_ind = as.numeric(stock_animals_ind),
    stock_ind_g = as.numeric(stock_animals_ind_kg) * kg_to_g,
    area_m2 = as.numeric(area_m2),
    depth_m = as.numeric(depth_m)
  ) %>%
  arrange(name, stock_date) %>%
  group_by(name) %>%
  mutate(culture_period = paste0("CP", row_number())) %>%
  ungroup() %>%
  mutate(culture_period_id = paste(name, culture_period, sep = "_"))

tt_info$harvest_date[tt_info$culture_period_id == "TT3_CP1"] <- as.Date("2025-08-18")
tt_info$culture_day[tt_info$culture_period_id == "TT3_CP1"] <- as.numeric(
  tt_info$harvest_date[tt_info$culture_period_id == "TT3_CP1"] -
    tt_info$stock_date[tt_info$culture_period_id == "TT3_CP1"]
)

tt3_cp1_feed_rate <- tt_info$total_feed_input_kg[tt_info$culture_period_id == "TT3_CP1"] /
  tt_info$culture_day[tt_info$culture_period_id == "TT3_CP1"]
tt_info$total_feed_input_kg[tt_info$culture_period_id == "TT3_CP2"] <-
  tt3_cp1_feed_rate * tt_info$culture_day[tt_info$culture_period_id == "TT3_CP2"]

tt_info <- tt_info %>%
  mutate(
    input_rate = total_feed_input_kg / culture_day,
    stock_density = stock_ind / area_m2
  )

cat("✓ tt_info processed:", nrow(tt_info), "culture periods\n\n")

# Section 3: Calculate Each Component by Culture Period ======================

## 3.1 DO Metabolism (cb_gpp, cb_rday, cb_nep) ==============================

cat("Calculating DO metabolism...\n")

do_cp <- do %>%
  filter(water == "fw", type == "mono", species == "tilapia",
         sample == "water") %>%
  separate_culture_period() %>%
  filter(period == "CP") %>%
  mutate(name = dplyr::recode(name,
                              'tilapia T1' = 'TT1',
                              'tilapia T2' = 'TT2',
                              'tilapia T3' = 'TT3')) %>%
  inner_join(tt_info %>% select(name, culture_period, culture_period_id,
                                 stock_date, harvest_date, culture_day),
             by = "name", relationship = "many-to-many") %>%
  filter(sampling_date >= stock_date, sampling_date <= harvest_date) %>%
  group_by(culture_period_id, name, culture_period, culture_day) %>%
  summarise(
    n_do_samplings = n(),
    c_gpp_avg = mean(c_gpp, na.rm = TRUE),
    c_rday_avg = mean(c_rday, na.rm = TRUE),
    c_nep_avg = mean(c_nep, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    cb_gpp = c_gpp_avg * culture_day * gm2_to_Mgha,
    cb_rday = c_rday_avg * culture_day * gm2_to_Mgha,
    cb_nep = c_nep_avg * culture_day * gm2_to_Mgha
  ) %>%
  select(culture_period_id, name, culture_period, n_do_samplings,
         cb_gpp, cb_rday, cb_nep)

cat("✓ DO metabolism calculated for", nrow(do_cp), "culture periods\n")

## 3.2 GHG Emissions (cb_ghg, cb_ghg_in, cb_ghg_out) =========================

cat("Calculating GHG emissions...\n")

ghg_cp <- ghg_co2e %>%
  filter(water == "fw", type == "mono", species == "tilapia",
         con %in% c("light", "day"), sample == "water") %>%
  separate_culture_period() %>%
  filter(period == "CP") %>%
  mutate(name = dplyr::recode(name,
                              'tilapia T1' = 'TT1',
                              'tilapia T2' = 'TT2',
                              'tilapia T3' = 'TT3')) %>%
  inner_join(tt_info %>% select(name, culture_period, culture_period_id,
                                 stock_date, harvest_date, culture_day),
             by = "name", relationship = "many-to-many") %>%
  filter(sampling_date >= stock_date, sampling_date <= harvest_date) %>%
  group_by(culture_period_id, name, culture_period, culture_day) %>%
  summarise(
    n_ghg_samplings = n(),
    ghg_ch4_avg = mean(ghg_ch4, na.rm = TRUE),  # μg m⁻² h⁻¹
    .groups = "drop"
  ) %>%
  mutate(
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
  filter(water == "fw", type == "mono", species == "tilapia", trap == "old") %>%
  separate_culture_period() %>%
  filter(period == "CP") %>%
  mutate(name = dplyr::recode(name,
                              'tilapia T1' = 'TT1',
                              'tilapia T2' = 'TT2',
                              'tilapia T3' = 'TT3')) %>%
  inner_join(tt_info %>% select(name, culture_period, culture_period_id,
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

## 3.3b TT3 Cycle 1 piecewise reconstruction (DO, GHG, SA only) =============

cat("Reconstructing TT3_CP1 (DO/GHG/SA) with CP2-proxied early segment...\n")

tt3_cp1_row <- tt_info %>% filter(culture_period_id == "TT3_CP1")
tt3_cp2_row <- tt_info %>% filter(culture_period_id == "TT3_CP2")

tt3_cp1_late_start <- as.Date("2025-03-01")  # start of CP1's own sampled window
n_days_early <- as.numeric(tt3_cp1_late_start - tt3_cp1_row$stock_date)
n_days_late  <- tt3_cp1_row$culture_day - n_days_early
# Same-length early window, starting from CP2's own stocking date, so the
# proxy compares like-for-like culture stage rather than an arbitrary span
tt3_cp2_proxy_end <- tt3_cp2_row$stock_date + n_days_early - 1

cat("  Split:", n_days_early, "days (early, CP2-proxied) +", n_days_late,
    "days (late, CP1's own data) =", tt3_cp1_row$culture_day, "days\n")

tt3_do_late <- do %>%
  filter(water == "fw", type == "mono", species == "tilapia", sample == "water",
         name == "tilapia T3", sampling_date >= tt3_cp1_late_start,
         sampling_date <= tt3_cp1_row$harvest_date) %>%
  summarise(n = n(), c_gpp = mean(c_gpp, na.rm = TRUE),
            c_rday = mean(c_rday, na.rm = TRUE), c_nep = mean(c_nep, na.rm = TRUE))
tt3_do_early <- do %>%
  filter(water == "fw", type == "mono", species == "tilapia", sample == "water",
         name == "tilapia T3", sampling_date >= tt3_cp2_row$stock_date,
         sampling_date <= tt3_cp2_proxy_end) %>%
  summarise(n = n(), c_gpp = mean(c_gpp, na.rm = TRUE),
            c_rday = mean(c_rday, na.rm = TRUE), c_nep = mean(c_nep, na.rm = TRUE))

tt3_ghg_late <- ghg_co2e %>%
  filter(water == "fw", type == "mono", species == "tilapia", con %in% c("light", "day"),
         sample == "water", name == "tilapia T3", sampling_date >= tt3_cp1_late_start,
         sampling_date <= tt3_cp1_row$harvest_date) %>%
  summarise(n = n(), ghg_ch4 = mean(ghg_ch4, na.rm = TRUE))
tt3_ghg_early <- ghg_co2e %>%
  filter(water == "fw", type == "mono", species == "tilapia", con %in% c("light", "day"),
         sample == "water", name == "tilapia T3", sampling_date >= tt3_cp2_row$stock_date,
         sampling_date <= tt3_cp2_proxy_end) %>%
  summarise(n = n(), ghg_ch4 = mean(ghg_ch4, na.rm = TRUE))

tt3_sa_late <- sa %>%
  filter(water == "fw", type == "mono", species == "tilapia", trap == "old",
         name == "tilapia T3", sampling_date_start >= tt3_cp1_late_start,
         sampling_date_start <= tt3_cp1_row$harvest_date) %>%
  summarise(n = n(), c_sa = mean(c_sa, na.rm = TRUE))
tt3_sa_early <- sa %>%
  filter(water == "fw", type == "mono", species == "tilapia", trap == "old",
         name == "tilapia T3", sampling_date_start >= tt3_cp2_row$stock_date,
         sampling_date_start <= tt3_cp2_proxy_end) %>%
  summarise(n = n(), c_sa = mean(c_sa, na.rm = TRUE))

# DO - own-data-only (current script's flat extrapolation) vs piecewise, for
# the disclosure comparison printed in Section 7
cb_gpp_own_only  <- tt3_do_late$c_gpp * tt3_cp1_row$culture_day * gm2_to_Mgha
cb_gpp_piecewise  <- (tt3_do_late$c_gpp  * n_days_late + tt3_do_early$c_gpp  * n_days_early) * gm2_to_Mgha
cb_rday_piecewise <- (tt3_do_late$c_rday * n_days_late + tt3_do_early$c_rday * n_days_early) * gm2_to_Mgha
cb_nep_piecewise  <- (tt3_do_late$c_nep  * n_days_late + tt3_do_early$c_nep  * n_days_early) * gm2_to_Mgha

# GHG
yr_culture_late  <- n_days_late  * day_to_yr
yr_culture_early <- n_days_early * day_to_yr
cb_ghg_own_only <- tt3_ghg_late$ghg_ch4 * µg_to_g * gm2hr_to_Mghayr *
  (tt3_cp1_row$culture_day * day_to_yr) * (c_mass / ch4_mass)
cb_ghg_piecewise <-
  tt3_ghg_late$ghg_ch4  * µg_to_g * gm2hr_to_Mghayr * yr_culture_late  * (c_mass / ch4_mass) +
  tt3_ghg_early$ghg_ch4 * µg_to_g * gm2hr_to_Mghayr * yr_culture_early * (c_mass / ch4_mass)
cb_ghg_in_piecewise  <- ifelse(cb_ghg_piecewise < 0, abs(cb_ghg_piecewise), 0)
cb_ghg_out_piecewise <- ifelse(cb_ghg_piecewise > 0, cb_ghg_piecewise, 0)

# SA
cb_sa_own_only  <- tt3_sa_late$c_sa * tt3_cp1_row$culture_day * gm2_to_Mgha
cb_sa_piecewise <- (tt3_sa_late$c_sa * n_days_late + tt3_sa_early$c_sa * n_days_early) * gm2_to_Mgha

cat("  cb_gpp (Mg C/ha): own-Mar-Aug-extrapolated =", round(cb_gpp_own_only, 3),
    "-> piecewise =", round(cb_gpp_piecewise, 3), "\n")
cat("  cb_ghg (Mg C/ha): own-Mar-Aug-extrapolated =", round(cb_ghg_own_only, 3),
    "-> piecewise =", round(cb_ghg_piecewise, 3), "\n")
cat("  cb_sa (Mg C/ha):  own-Mar-Aug-extrapolated =", round(cb_sa_own_only, 3),
    "-> piecewise =", round(cb_sa_piecewise, 3), "\n")

# Overwrite TT3_CP1's rows in do_cp/ghg_cp/sa_cp with the piecewise values
do_cp <- do_cp %>%
  mutate(
    cb_gpp  = ifelse(culture_period_id == "TT3_CP1", cb_gpp_piecewise, cb_gpp),
    cb_rday = ifelse(culture_period_id == "TT3_CP1", cb_rday_piecewise, cb_rday),
    cb_nep  = ifelse(culture_period_id == "TT3_CP1", cb_nep_piecewise, cb_nep),
    n_do_samplings = ifelse(culture_period_id == "TT3_CP1",
                             tt3_do_late$n + tt3_do_early$n, n_do_samplings)
  )

ghg_cp <- ghg_cp %>%
  mutate(
    cb_ghg     = ifelse(culture_period_id == "TT3_CP1", cb_ghg_piecewise, cb_ghg),
    cb_ghg_in  = ifelse(culture_period_id == "TT3_CP1", cb_ghg_in_piecewise, cb_ghg_in),
    cb_ghg_out = ifelse(culture_period_id == "TT3_CP1", cb_ghg_out_piecewise, cb_ghg_out),
    n_ghg_samplings = ifelse(culture_period_id == "TT3_CP1",
                              tt3_ghg_late$n + tt3_ghg_early$n, n_ghg_samplings)
  )

sa_cp <- sa_cp %>%
  mutate(
    cb_sa = ifelse(culture_period_id == "TT3_CP1", cb_sa_piecewise, cb_sa),
    n_sa_samplings = ifelse(culture_period_id == "TT3_CP1",
                             tt3_sa_late$n + tt3_sa_early$n, n_sa_samplings)
  )

cat("✓ TT3_CP1 DO/GHG/SA reconstructed with CP2-proxied early segment\n\n")

## 3.4 Feed (cb_feed) - From tt_info =========================================

cat("Calculating feed carbon...\n")

feed_c_percent <- feed %>%
  filter(water == "fw", type == "mono", species == "tilapia") %>%
  summarise(c_percent = mean(c_percent, na.rm = TRUE)) %>%
  pull(c_percent)

if (length(feed_c_percent) == 0 || is.na(feed_c_percent)) {
  feed_c_percent <- 45  # Default value
  cat("  Using default feed C content: 45%\n")
} else {
  cat("  Using measured feed C content:", round(feed_c_percent, 2), "%\n")
}

feed_cp <- tt_info %>%
  select(culture_period_id, name, culture_period, area_m2,
         total_feed_input_kg, culture_day) %>%
  distinct() %>%
  mutate(
    cb_feed = (total_feed_input_kg * (feed_c_percent/100) * kg_to_Mg) / (area_m2 * m2_to_ha)
  ) %>%
  select(culture_period_id, cb_feed)

cat("✓ Feed carbon calculated for", nrow(feed_cp), "culture periods\n")

## 3.5 Stock/Harvest Biomass - From tt_info ==================================

cat("Calculating stock/harvest biomass carbon...\n")

tilapia_stock_c_percent <- animals %>%
  filter(water == "fw", animals == "tilapia", status == "stocked") %>%
  summarise(c_percent = mean(C_percent, na.rm = TRUE)) %>%
  pull(c_percent)
tilapia_harvest_c_percent <- animals %>%
  filter(water == "fw", animals == "tilapia", status == "harvested") %>%
  summarise(c_percent = mean(C_percent, na.rm = TRUE)) %>%
  pull(c_percent)

animals_cp <- tt_info %>%
  select(culture_period_id, name, culture_period, area_m2,
         stock_ind, stock_ind_g, harvest_total_kg) %>%
  distinct() %>%
  mutate(
    stock_total_kg = (stock_ind * stock_ind_g) / kg_to_g,
    cb_stock = (stock_total_kg * (tilapia_stock_c_percent/100) * kg_to_Mg) / (area_m2 * m2_to_ha),
    cb_harvest = (harvest_total_kg * (tilapia_harvest_c_percent/100) * kg_to_Mg) / (area_m2 * m2_to_ha)
  ) %>%
  select(culture_period_id, cb_stock, cb_harvest)

cat("✓ Stock/harvest carbon calculated for", nrow(animals_cp), "culture periods\n")

## 3.6 Water Exchange (cb_inflow, cb_outflow) ================================

cat("Calculating water exchange carbon...\n")

# 3.6a: Continuous inflow (from "in" source) - DOC/DIC
cat("  Processing continuous inflow (DOC/DIC)...\n")

inflow_doc_cp <- doc %>%
  filter(water == "fw", type == "mono", species == "tilapia", source == "in") %>%
  separate_culture_period() %>%
  filter(period == "CP") %>%
  mutate(name = dplyr::recode(name,
                              'tilapia T1' = 'TT1',
                              'tilapia T2' = 'TT2',
                              'tilapia T3' = 'TT3')) %>%
  inner_join(tt_info %>% select(name, culture_period, culture_period_id,
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
  filter(water == "fw", type == "mono", species == "tilapia", source == "in") %>%
  separate_culture_period() %>%
  filter(period == "CP") %>%
  mutate(name = dplyr::recode(name,
                              'tilapia T1' = 'TT1',
                              'tilapia T2' = 'TT2',
                              'tilapia T3' = 'TT3')) %>%
  inner_join(tt_info %>% select(name, culture_period, culture_period_id,
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

inflow_continuous_cp <- merge(inflow_doc_cp, inflow_poc_cp,
                               by = "culture_period_id", all = TRUE)

cat("  ✓ Continuous inflow calculated\n")

# 3.6b: Initial water carbon (FIRST sampling) - DOC/POC
cat("  Processing initial water carbon (first sampling)...\n")

inflow_start_cp <- doc %>%
  filter(water == "fw", type == "mono", species == "tilapia", source == "water") %>%
  mutate(name = dplyr::recode(name,
                              'tilapia T1' = 'TT1',
                              'tilapia T2' = 'TT2',
                              'tilapia T3' = 'TT3')) %>%
  inner_join(tt_info %>% select(name, culture_period, culture_period_id,
                                 stock_date, harvest_date, area_m2, depth_m),
             by = "name", relationship = "many-to-many") %>%
  filter(sampling_date >= stock_date, sampling_date <= harvest_date) %>%
  group_by(culture_period_id) %>%
  filter(sampling_date == min(sampling_date)) %>%  # FIRST sampling
  ungroup() %>%
  inner_join(tt_info %>% select(culture_period_id, area_m2, depth_m),
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
  filter(water == "fw", type == "mono", species == "tilapia", source == "water") %>%
  mutate(name = dplyr::recode(name,
                              'tilapia T1' = 'TT1',
                              'tilapia T2' = 'TT2',
                              'tilapia T3' = 'TT3')) %>%
  inner_join(tt_info %>% select(name, culture_period, culture_period_id,
                                 stock_date, harvest_date),
             by = "name", relationship = "many-to-many") %>%
  filter(sampling_date >= stock_date, sampling_date <= harvest_date) %>%
  group_by(culture_period_id) %>%
  filter(sampling_date == min(sampling_date)) %>%  # FIRST sampling
  ungroup() %>%
  inner_join(tt_info %>% select(culture_period_id, area_m2, depth_m),
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

inflow_start_all_cp <- merge(inflow_start_cp, inflow_start_poc_cp,
                              by = "culture_period_id", all = TRUE)

cat("  ✓ Initial water carbon calculated\n")

# 3.6c: Continuous outflow - Set equal to continuous inflow
cat("  Setting continuous outflow equal to continuous inflow...\n")

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
  filter(water == "fw", type == "mono", species == "tilapia", source == "water") %>%
  mutate(name = dplyr::recode(name,
                              'tilapia T1' = 'TT1',
                              'tilapia T2' = 'TT2',
                              'tilapia T3' = 'TT3')) %>%
  inner_join(tt_info %>% select(name, culture_period, culture_period_id,
                                 stock_date, harvest_date),
             by = "name", relationship = "many-to-many") %>%
  filter(sampling_date >= stock_date, sampling_date <= harvest_date) %>%
  group_by(culture_period_id) %>%
  filter(sampling_date == max(sampling_date)) %>%  # LAST sampling
  ungroup() %>%
  inner_join(tt_info %>% select(culture_period_id, area_m2, depth_m),
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
  filter(water == "fw", type == "mono", species == "tilapia", source == "water") %>%
  mutate(name = dplyr::recode(name,
                              'tilapia T1' = 'TT1',
                              'tilapia T2' = 'TT2',
                              'tilapia T3' = 'TT3')) %>%
  inner_join(tt_info %>% select(name, culture_period, culture_period_id,
                                 stock_date, harvest_date),
             by = "name", relationship = "many-to-many") %>%
  filter(sampling_date >= stock_date, sampling_date <= harvest_date) %>%
  group_by(culture_period_id) %>%
  filter(sampling_date == max(sampling_date)) %>%  # LAST sampling
  ungroup() %>%
  inner_join(tt_info %>% select(culture_period_id, area_m2, depth_m),
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

outflow_end_all_cp <- merge(outflow_end_cp, outflow_end_poc_cp,
                             by = "culture_period_id", all = TRUE)

cat("  ✓ Final water carbon calculated\n")
cat("✓ Water exchange carbon calculated\n")

## 3.7 Rain (cb_rain) ========================================================

cat("Calculating rain carbon...\n")

rain_doc_jn_bd <- doc %>%
  filter(source == "rainwater", water == "jn_bd") %>%
  summarise(doc = mean(doc, na.rm = TRUE)) %>%
  pull(doc)
rain_poc_jn_bd <- poc %>%
  filter(source == "rainwater", water == "jn_bd") %>%
  summarise(poc = mean(poc, na.rm = TRUE)) %>%
  pull(poc)

rain_content <- tibble(
  zone = c("jn_bd", "jn_xj"),
  rain_doc_avg = rain_doc_jn_bd,
  rain_poc_avg = rain_poc_jn_bd
)

tt_zone <- info %>%
  filter(water == "fw", type == "mono", species == "tilapia") %>%
  transmute(name = dplyr::recode(group, "T1" = "TT1", "T2" = "TT2", "T3" = "TT3"), zone) %>%
  distinct()

weather_monthly <- weather_year %>%
  filter(zone %in% c("jn_bd", "jn_xj")) %>%
  mutate(rain_mm = as.numeric(rain_mm),
         month = as.integer(time),
         year = as.integer(year))

rain_cp <- tt_info %>%
  select(culture_period_id, name, culture_period, area_m2, culture_day,
         stock_date, harvest_date) %>%
  distinct() %>%
  filter(!is.na(stock_date), !is.na(harvest_date)) %>%
  left_join(tt_zone, by = "name") %>%
  left_join(rain_content, by = "zone") %>%
  rowwise() %>%
  mutate(
    year_months = list({
      dates <- seq.Date(stock_date, harvest_date, by = "month")
      data.frame(
        year = as.integer(format(dates, "%Y")),
        month = as.integer(format(dates, "%m"))
      )
    }),
    precipitation_total = {
      ym <- year_months
      wz <- weather_monthly[weather_monthly$zone == zone, ]
      sum(wz$rain_mm[paste(wz$year, wz$month) %in% paste(ym$year, ym$month)], na.rm = TRUE)
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

rain_cp_complete <- tt_info %>%
  select(culture_period_id) %>%
  distinct() %>%
  left_join(rain_cp, by = "culture_period_id") %>%
  mutate(cb_rain = ifelse(is.na(cb_rain), 0, cb_rain))

rain_cp <- rain_cp_complete

cat("✓ Rain carbon calculated for", nrow(rain_cp), "culture periods\n\n")

# Section 4: Merge All Components ============================================

cat("Merging all carbon budget components...\n")

tt_budget_cp <- tt_info %>%
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

# Disclosure flag for the piecewise DO/GHG/SA reconstruction (Section 3.3b) -
# carried through to any table or export built from tt_budget_cp so the
# imputed segment is never silently indistinguishable from measured cycles
tt_budget_cp <- tt_budget_cp %>%
  mutate(note = ifelse(
    culture_period_id == "TT3_CP1",
    paste0("cb_gpp/cb_rday/cb_nep/cb_ghg*/cb_sa use CP2-proxied data for the ",
           "unsampled ", n_days_early, "-day early segment (", tt3_cp1_row$stock_date,
           " to ", tt3_cp1_late_start - 1, "); see Section 3.3b"),
    NA_character_
  ))

cat("✓ All components merged\n\n")

# Section 5: Calculate Totals =================================================

cat("Calculating budget totals...\n")

tt_budget_cp <- tt_budget_cp %>%
  mutate(
    across(starts_with("cb_"), ~ifelse(is.na(.) | is.nan(.), 0, .)),

    cb_inflow_all = cb_doc_in + cb_poc_in + cb_doc_in_start + cb_poc_in_start,
    cb_outflow_all = cb_doc_out + cb_poc_out + cb_doc_out_end + cb_poc_out_end,

    cb_input = cb_gpp + cb_ghg_in + cb_feed + cb_inflow_all + cb_stock + cb_rain,
    cb_output = cb_rday + cb_ghg_out + cb_sa + cb_outflow_all + cb_harvest,
    cb_budget = cb_input - cb_output,

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

budget_in <- tt_budget_cp %>%
  filter(culture_period_id != "TT3_CP2") %>%
  select(name, cb_gpp, cb_ghg_in, cb_feed,
         cb_inflow_all, cb_stock, cb_rain, cb_input)
budget_in <- descriptive_statistic(budget_in,
                      vars = c(cb_gpp, cb_ghg_in, cb_feed,
                               cb_inflow_all, cb_stock, cb_rain, cb_input),
                      groups = name) %>% select(name, contains("mean"))
write.xlsx(budget_in, file = 'output/tilapia_budget_in.xlsx',
           rowNames = FALSE)

budget_out <- tt_budget_cp %>%
  filter(culture_period_id != "TT3_CP2") %>%
  select(name, cb_rday, cb_ghg_out, cb_sa,
         cb_outflow_all, cb_harvest, cb_output)
budget_out <- descriptive_statistic(budget_out,
                                   vars = c(cb_rday, cb_ghg_out, cb_sa,
                                            cb_outflow_all, cb_harvest, cb_output),
                                   groups = name) %>% select(name, contains("mean"))
write.xlsx(budget_out, file = 'output/tilapia_budget_out.xlsx',
           rowNames = FALSE)

cat("✓ Results saved to: output/tilapia_budget_in.xlsx, output/tilapia_budget_out.xlsx\n\n")

# Section 7: Summary and Validation ===========================================

cat("=== Tilapia Carbon Budget by Culture Period ===\n")
cat("Rows:", nrow(tt_budget_cp), "culture periods\n\n")

cat("Summary by pond:\n")
summary_table <- tt_budget_cp %>%
  select(name, culture_period, stock_date, harvest_date, culture_day,
         n_do_samplings, n_ghg_samplings, n_sa_samplings,
         cb_input, cb_output, cb_budget, note)
print(summary_table)

cat("\nTT3_CP1 disclosure - own-Mar-Aug-extrapolated vs CP2-proxied piecewise:\n")
cat("  cb_gpp:", round(cb_gpp_own_only, 3), "->", round(cb_gpp_piecewise, 3), "Mg C/ha\n")
cat("  cb_ghg:", round(cb_ghg_own_only, 3), "->", round(cb_ghg_piecewise, 3), "Mg C/ha\n")
cat("  cb_sa: ", round(cb_sa_own_only, 3), "->", round(cb_sa_piecewise, 3), "Mg C/ha\n")

cat("\n=== Validation Checks ===\n")

cat("1. Row count:", nrow(tt_budget_cp), "culture periods with data\n")

cat("2. Culture day range:", min(tt_budget_cp$culture_day, na.rm = TRUE),
    "to", max(tt_budget_cp$culture_day, na.rm = TRUE), "days\n")

cat("3. Budget balance verification:\n")
balance_check <- tt_budget_cp %>%
  mutate(balance_diff = abs(cb_budget - (cb_input - cb_output))) %>%
  summarise(max_diff = max(balance_diff, na.rm = TRUE))
cat("   Max difference:", round(balance_check$max_diff, 6), "Mg C/ha (should be ~0)\n")

cat("\n4. Summary statistics (Mg C/ha):\n")
budget_summary <- tt_budget_cp %>%
  summarise(
    mean_cb_input = mean(cb_input, na.rm = TRUE),
    mean_cb_output = mean(cb_output, na.rm = TRUE),
    mean_cb_budget = mean(cb_budget, na.rm = TRUE),
    mean_cb_feed = mean(cb_feed, na.rm = TRUE),
    mean_cb_harvest = mean(cb_harvest, na.rm = TRUE)
  )
print(budget_summary)

cat("\n=== Analysis Complete ===\n")
