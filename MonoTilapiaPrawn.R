# prep --------------------------------------------------------------------

source('R/00_R_preparation.R')
source("R/PrawnCarbonBudget.R")

do         <- readRDS("data/do.rds")
ghg        <- readRDS("data/ghg.rds")
ghg_co2e   <- readRDS("data/ghg_co2e.rds")
doc        <- readRDS("data/doc.rds")
poc        <- readRDS("data/poc.rds")
soc        <- readRDS("data/soc.rds")
sa         <- readRDS("data/sa.rds")
envr       <- readRDS("data/envr.rds")
budget     <- readRDS("data/budget.rds")
raw_budget <- readRDS("data/raw_budget.rds")
feed       <- readRDS("data/feed.rds")
info       <- readRDS("data/info.rds")
nutrient   <- readRDS("data/nutrient.rds")

df_envr <- envr %>% 
  filter(water == 'fw', type == 'mono', name != 'prawn T1',
         con %in% c('light', 'day')) %>% 
  mutate(name = dplyr::recode(name, 
                              'prawn T2' = 'PT1',
                              'prawn T3' = 'PT2',
                              'prawn T4' = 'PT3',
                              'tilapia T1' = 'TT1',
                              'tilapia T2' = 'TT2',
                              'tilapia T3' = 'TT3'),
         year_month = format(sampling_date, '%Y-%m'),
         month = format(sampling_date, '%b'),
         month = factor(month, levels = month.abb))
df_envr <- separate_culture_period(df_envr)

df_ghg <- ghg %>% 
  filter(water == 'fw', type == 'mono', name != 'prawn T1',
         con %in% c('light', 'day')) %>%
  mutate(ghg_ch4_mg = ghg_ch4 * µg_to_mg,
         name = dplyr::recode(name, 
                              'prawn T2' = 'PT1',
                              'prawn T3' = 'PT2',
                              'prawn T4' = 'PT3',
                              'tilapia T1' = 'TT1',
                              'tilapia T2' = 'TT2',
                              'tilapia T3' = 'TT3'),
         year_month = format(sampling_date, '%Y-%m'),
         month = format(sampling_date, '%b'),
         month = factor(month, levels = month.abb))
df_ghg <- separate_culture_period(df_ghg)

df_ghg_co2e <- ghg_co2e %>% 
  filter(water == 'fw', type == 'mono', name != 'prawn T1',
         con %in% c('light', 'day')) %>% 
  mutate(name = dplyr::recode(name, 
                              'prawn T2' = 'PT1',
                              'prawn T3' = 'PT2',
                              'prawn T4' = 'PT3',
                              'tilapia T1' = 'TT1',
                              'tilapia T2' = 'TT2',
                              'tilapia T3' = 'TT3'),
         year_month = format(sampling_date, '%Y-%m'),
         month = format(sampling_date, '%b'),
         month = factor(month, levels = month.abb))
df_ghg_co2e <- separate_culture_period(df_ghg_co2e)

df_do <- do %>% 
  filter(water == 'fw', type == 'mono', name != 'prawn T1') %>% 
  mutate(name = dplyr::recode(name, 
                              'prawn T2' = 'PT1',
                              'prawn T3' = 'PT2',
                              'prawn T4' = 'PT3',
                              'tilapia T1' = 'TT1',
                              'tilapia T2' = 'TT2',
                              'tilapia T3' = 'TT3'),
         year_month = format(sampling_date, '%Y-%m'),
         month = format(sampling_date, '%b'),
         month = factor(month, levels = month.abb))
df_do <- separate_culture_period(df_do)

df_nutrient <- nutrient |> 
  filter(water == "fw", type == "mono") %>% 
  mutate(name = dplyr::recode(name, 
                              'prawn T2' = 'PT1',
                              'prawn T3' = 'PT2',
                              'prawn T4' = 'PT3',
                              'tilapia T1' = 'TT1',
                              'tilapia T2' = 'TT2',
                              'tilapia T3' = 'TT3'),
         year_month = format(sampling_date, '%Y-%m'),
         month = format(sampling_date, '%b'),
         month = factor(month, levels = month.abb))
df_nutrient <- separate_culture_period(df_nutrient)

plot_ghg_all <- function(df, y, ysd = NULL, plot){

  if (plot == 'line'){

  ggplot(df, aes(x = month, y = {{y}}, color = name, group = name)) +
    geom_ribbon(aes(ymin = {{y}} - {{ysd}}, ymax = {{y}} + {{ysd}}, fill = name),
                alpha = 0.1, color = NA) +
    geom_line(linewidth = 2) +
    geom_point(shape = 21, size = 2, stroke = 1, fill = "white") +
    facet_grid(. ~ new_species, scales = "free") +
    xlab(NULL) +
    theme_classic(base_size = 25) +
    theme(text = element_text(family = "Century Gothic", color = "black"),
          strip.text = element_text(face = "bold", color = "black"),
          axis.text = element_text(face = "bold", color = "black"),
          legend.text = element_text(face = "bold", color = "black"),
          plot.background = element_rect(fill = "transparent", color = NA),
          strip.background = element_rect(fill = "transparent", color = "black"),
          panel.background = element_rect(fill = "transparent", color = NA),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.title = element_blank(),
          legend.position = "top",
          legend.key = element_blank(),
          strip.text.x = element_text(face = "bold.italic", color = "black"))

  } else if (plot == 'box'){
  
  plot_box(df,
           x = name, y = {{y}}, z = name,
           facet = T, facet_x = "new_species", style = 'classic') +
    xlab(NULL) +
    theme(legend.position = "none", 
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          strip.text.x = element_text(face = "bold.italic", color = "black"),
          strip.background = element_rect(fill = "transparent", color = "black")) 
  } 
}

plot_ghg_feed <- function(y){
  ggplot(pd_feed, aes(x = input_rate, y = {{y}}, color = name)) +
    geom_point(size = 5) +
    geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed", alpha = 0.3) +
    xlab(expression(bold("Feed input rate (kg day"^-1*")"))) +
    scale_color_manual(values = c("PT1" = "#ee9b00", "PT2" = "#ca6702", "PT3" = "#bb3e03")) +
    theme_bw(base_size = 25) +
    theme(text = element_text(family = "Century Gothic", color = "black"),
          strip.text = element_text(face = "bold", color = "black"),
          axis.text = element_text(face = "bold", color = "black"),
          legend.text = element_text(face = "bold", color = "black"),
          plot.background = element_rect(fill = "transparent", color = NA),
          strip.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.title = element_blank(),
          legend.position = "top",
          legend.key=element_blank())
}

create_cor_matrix <- function(df) {
  # Extract and rename variables for correlation test
  tidy_df <- df %>% 
    ungroup() %>%
    select(ghg_co2, ghg_ch4, ghg_n2o, rday, gpp, c_sa,
           envr_temp, envr_ec, envr_do_w, envr_ph, envr_ntu, envr_sal, 
           envr_ss, envr_chla, doc, dic, poc, envr_po4, envr_nh4, envr_no3, envr_no2) %>% 
    rename(temp = envr_temp,
           ec = envr_ec,
           do = envr_do_w,
           ph = envr_ph,
           ntu = envr_ntu,
           sal = envr_sal,
           ss = envr_ss,
           chla = envr_chla,
           co2 = ghg_co2,
           ch4 = ghg_ch4,
           n2o = ghg_n2o,
           po4 = envr_po4,
           nh4 = envr_nh4,
           no3 = envr_no3,
           no2 = envr_no2,
           er = rday,
           sa = c_sa) %>% 
    mutate(gpp = gpp * alter_direction)
  
  # Perform test
  cor_matrix <- rcorr(as.matrix(tidy_df), type = "spearman")
  
  return(cor_matrix)
  
}

library("seminr")
#library("MASS")
#library("rsvg")
theme_sem <- seminr_theme_create(
  plot.fontname = "Century Gothic",
  plot.splines = T,
  plot.rounding = 2,
  plot.adj = F,
  plot.specialcharacters = T,
  plot.randomizedweights = F,
  plot.bgcolor = "white",
  mm.node.color = "dimgrey", 
  mm.node.fill = "white",
  mm.node.label.fontsize = 8,
  mm.node.label.fontcolor = "black",
  mm.edge.positive.color = "dimgrey", #arrow
  mm.edge.negative.color = "dimgrey", #arrow
  mm.edge.positive.style = "solid",
  mm.edge.negative.style = "dashed",
  mm.edge.label.fontsize = 7,
  mm.edge.label.fontcolor = "black",
  mm.edge.label.show = T,
  mm.edge.minlen = 1,
  mm.edge.width_multiplier = 0, #width.correspond.to.coefficient
  mm.edge.width_offset = 0.5,
  mm.edge.use_outer_weights = T,
  mm.edge.boot.show_t_value = F,
  mm.edge.boot.show_p_value = F,
  mm.edge.boot.show_p_stars = T,
  mm.edge.boot.show_ci = F,
  mm.edge.boot.template = edge_template_minimal(),
  sm.node.color = "black",
  sm.node.fill = "white",
  sm.node.label.fontsize = 12,
  sm.node.label.fontcolor = "black",
  sm.node.endo.template = node_endo_template_default(),
  sm.node.exo.template = node_exo_template_default(),
  sm.edge.boot.show_t_value = FALSE,
  sm.edge.boot.show_p_value = FALSE,
  sm.edge.boot.show_p_stars = TRUE,
  sm.edge.boot.show_ci = TRUE,
  sm.edge.boot.template = edge_template_default(),
  sm.edge.positive.color = "black",
  sm.edge.negative.color = "black",
  sm.edge.positive.style = "solid",
  sm.edge.negative.style = "dashed",
  sm.edge.label.fontsize = 9,
  sm.edge.label.fontcolor = "black",
  sm.edge.label.show = TRUE,
  sm.edge.label.all_betas = TRUE,
  sm.edge.minlen = NA_integer_,
  sm.edge.width_offset = 0.5,
  sm.edge.width_multiplier = 0,
  construct.reflective.shape = "ellipse",
  construct.reflective.arrow = "backward",
  construct.reflective.use_weights = FALSE,
  construct.compositeA.shape = "box",
  construct.compositeA.arrow = "backward",
  construct.compositeA.use_weights = FALSE,
  construct.compositeB.shape = "box",
  construct.compositeB.arrow = "forward",
  construct.compositeB.use_weights = TRUE,
  manifest.reflective.shape = "ellipse",
  manifest.compositeA.shape = "ellipse",
  manifest.compositeB.shape = "ellipse")

extract_values <- function(value) {
  if (grepl("Median:", value)) {
    median_value <- str_extract(value, "(?<=Median: )\\S+")
    return(c(min = NA, max = NA, mean = NA, sd = NA, median = median_value))
  } else if (grepl("ND", value)) {
    return(c(min = NA, max = NA, mean = NA, sd = NA, median = NA))
  } else {
    # Match different formats
    matches <- str_match(value, "([-\\d.]+) to ([-\\d.]+) \\(([^±]+) ± ([-\\d.]+)\\)") # format 1
    if (!is.na(matches[1, 1])) {
      return(c(min = matches[1, 2], max = matches[1, 3], mean = matches[1, 4], sd = matches[1, 5], median = NA))
    }

    matches <- str_match(value, "([-\\d.]+) to ([-\\d.]+)\\(([^±]+) ± ([-\\d.]+)\\)") # format 1b (no space before parenthesis)
    if (!is.na(matches[1, 1])) {
      return(c(min = matches[1, 2], max = matches[1, 3], mean = matches[1, 4], sd = matches[1, 5], median = NA))
    }

    matches <- str_match(value, "([-\\d.]+) to ([-\\d.]+) \\(([^)]+)\\)") # format 5
    if (!is.na(matches[1, 1])) {
      return(c(min = matches[1, 2], max = matches[1, 3], mean = matches[1, 4], sd = NA, median = NA))
    }

    matches <- str_match(value, "([-\\d.]+) to ([-\\d.]+)\\(([^)]+)\\)") # format 5b (no space before parenthesis)
    if (!is.na(matches[1, 1])) {
      return(c(min = matches[1, 2], max = matches[1, 3], mean = matches[1, 4], sd = NA, median = NA))
    }

    matches <- str_match(value, "([-\\d.]+)\u2013([-\\d.]+)\\(([^)]+)\\)") # format 5c (en-dash range)
    if (!is.na(matches[1, 1])) {
      return(c(min = matches[1, 2], max = matches[1, 3], mean = matches[1, 4], sd = NA, median = NA))
    }

    matches <- str_match(value, "([-\\d.]+) to ([-\\d.]+)") # format 2
    if (!is.na(matches[1, 1])) {
      return(c(min = matches[1, 2], max = matches[1, 3], mean = NA, sd = NA, median = NA))
    }

    matches <- str_match(value, "\\(([^±]+) ± ([-\\d.]+)\\)") # format 3
    if (!is.na(matches[1, 1])) {
      return(c(min = NA, max = NA, mean = matches[1, 2], sd = matches[1, 3], median = NA))
    }
    
    matches <- str_match(value, "\\(\\+([\\d.]+)\\)") # format 4b (+number in parentheses, e.g. (+9))
    if (!is.na(matches[1, 1])) {
      return(c(min = NA, max = NA, mean = matches[1, 2], sd = NA, median = NA))
    }

    matches <- str_match(value, "\\(([^)]+)\\)") # format 4
    if (!is.na(matches[1, 1])) {
      return(c(min = NA, max = NA, mean = matches[1, 2], sd = NA, median = NA))
    }

    matches <- str_match(trimws(value), "^[-\\d.eE+]+$") # format 6 (single number, incl. scientific notation)
    if (!is.na(matches[1, 1])) {
      return(c(min = NA, max = NA, mean = as.character(as.numeric(matches[1, 1])), sd = NA, median = NA))
    }

    return(c(min = NA, max = NA, mean = NA, sd = NA, median = NA)) # Default case
  }
}

classify_species <- function(x) {
  fin <- str_detect(x, regex("carp|fish|bass|bream|tilapia|milkfish|culter", ignore_case = TRUE))
  cru <- str_detect(x, regex("shrimp|prawn|crab|crayfish", ignore_case = TRUE))
  mol <- str_detect(x, regex("clam|abalone|oyster|mussel", ignore_case = TRUE))
  oth <- str_detect(x, regex("sea cucumber|snail|turtle", ignore_case = TRUE))
  
  case_when(
    fin & cru & mol & oth ~ "finfish/crustaceans/molluscs/others",
    fin & cru & mol       ~ "finfish/crustaceans/molluscs",
    fin & cru & oth       ~ "finfish/crustaceans/others",
    fin & mol & oth       ~ "finfish/molluscs/others",
    cru & mol & oth       ~ "crustaceans/molluscs/others",
    fin & cru             ~ "finfish/crustaceans",
    fin & mol             ~ "finfish/molluscs",
    fin & oth             ~ "finfish/others",
    cru & mol             ~ "crustaceans/molluscs",
    cru & oth             ~ "crustaceans/others",
    mol & oth             ~ "molluscs/others",
    fin                   ~ "finfish",
    cru                   ~ "crustaceans",
    mol                   ~ "molluscs",
    oth                   ~ "others",
    .default              = NA_character_
  )
}

# sampling site-----

taiwan_map_sf <- rnaturalearth::ne_states(country = "Taiwan", returnclass = "sf")
name <- c("TT1", "TT2–3", "PT1–3")
species <- c("O. aureus × O. niloticus", "O. aureus × O. niloticus", "M. rosenbergii")
long <- c(120.18974, 120.20304, 120.46372)
lat <- c(23.40736, 23.28207, 22.53057)
site <- data_frame(name, species, lat, long)
tiff("output/site.png", units="in", width = 8, height = 8, res=300)
ggplot(data = taiwan_map_sf) + 
  geom_sf(fill= "lightgray") +
  xlab(expression(bold(paste("Longitude")))) +
  ylab(expression(bold(paste("Latitude")))) +
  coord_sf(xlim = c(119, 123), ylim = c(21.50, 25.50), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotate(geom = "text", x = c(120.18974, 120.40304, 120.46372),
           y = c(23.50736, 23.28207, 22.63057),
           label = c("TT1", "TT2–3", "PT1–3"),
           fontface = "bold",  size = 4) +
  annotate(geom = "text", x = 121, y = 24, label = "Taiwan", 
           fontface = "italic",  size = 4) +
  geom_point(data = site, aes(x=long, y=lat, fill = species, shape = species), size = 3, 
             stroke = 1) +
  scale_shape_manual(values = c(21, 22, 23, 24)) +
  scale_fill_manual(values = c("#ee9b00", "#0a9396")) +
  theme_bw(base_size=16) +
  theme(legend.text = element_text(face = "bold.italic"),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(family = "Century Gothic", color = "black"),
        axis.text.x = element_text(face = "bold"),  
        axis.text.y = element_text(face = "bold"), 
        panel.background = element_rect(fill = "white"))
dev.off()

# envr --------------------------------------------------------------------

tab_envr <- descriptive_statistic(filter(df_envr, period == 'CP'),
                                  vars = c(envr_temp, envr_ec, envr_do_pc, envr_do_w, 
                                           envr_ph, envr_ntu, envr_sal, 
                                           envr_ss, envr_chla),
                                  groups = c(name)) %>% 
  select(-contains('min'))
colnames(tab_envr) <- gsub('_mean_sd', '', colnames(tab_envr))
write.xlsx(tab_envr, file = 'output/envr.xlsx')

descriptive_statistic(pt_info, 
                      groups = name,
                      vars = c(culture_day, stock_ind_g, 
                               stock_density, harvest_total_kg)) %>% 
  select(name, contains('mean'))

descriptive_statistic(df_envr,
                      vars = envr_temp,
                      groups = c(species, sampling_month))

## test --------------------------------------------------------------------

# for prawn

df <- df_envr %>% 
  filter(species == 'prawn', period == 'CP')

variables <- c('envr_temp', 'envr_ec', 'envr_do_pc','envr_do_w', 
               'envr_ph', 'envr_ntu', 'envr_sal', 'envr_ss', 'envr_chla')
map(variables, ~ {
  normality_test_aov(df, .x, 'name') %>%
    filter(p_value > 0.05)
})

ks_test(df, 'envr_temp', 'name') #>0.05
ks_test(df, 'envr_ec', 'name') # a a b
ks_test(df, 'envr_do_pc', 'name') #>0.05
ks_test(df, 'envr_do_w', 'name')  #>0.05
ks_test(df, 'envr_ph', 'name')  #>0.05
ks_test(df, 'envr_ntu', 'name')  #>0.05
ks_test(df, 'envr_sal', 'name') # a a b
ks_test(df, 'envr_ss', 'name')  #>0.05
ks_test(df, 'envr_chla', 'name')#>0.05

# for tilapia

df <- df_envr %>% 
  filter(species == 'tilapia', period == 'CP')

variables <- c('envr_temp', 'envr_ec', 'envr_do_pc','envr_do_w', 
               'envr_ph', 'envr_ntu', 'envr_sal', 'envr_ss', 'envr_chla')
map(variables, ~ {
  normality_test_aov(df, .x, 'name') %>%
    filter(p_value > 0.05)
})

ks_test(df, 'envr_temp', 'name')
ks_test(df, 'envr_ec', 'name') 
ks_test(df, 'envr_do_pc', 'name')
ks_test(df, 'envr_do_w', 'name')  
ks_test(df, 'envr_ph', 'name')  
ks_test(df, 'envr_ntu', 'name') 
ks_test(df, 'envr_sal', 'name') 
ks_test(df, 'envr_ss', 'name')  
ks_test(df, 'envr_chla', 'name')

# between species


df <- df_envr %>% 
  filter(period == 'CP')

variables <- c('envr_temp', 'envr_ec', 'envr_do_pc','envr_do_w', 
               'envr_ph', 'envr_ntu', 'envr_sal', 'envr_ss', 'envr_chla')
map(variables, ~ {
  normality_test_t(df, .x, species, 'prawn', 'tilapia') %>%
    filter(p_value > 0.05)
})

wilcox.test(envr_temp ~ species, df)
wilcox.test(envr_ec ~ species, df)
wilcox.test(envr_do_pc ~ species, df)
wilcox.test(envr_do_w ~ species, df)
wilcox.test(envr_ph ~ species, df)
wilcox.test(envr_ntu ~ species, df)
wilcox.test(envr_sal ~ species, df)
wilcox.test(envr_ss ~ species, df)
wilcox.test(envr_chla ~ species, df)
aggregate(envr_chla ~ species, df, mean)

## nutrient-----

tab_nutrient <- descriptive_statistic(df_nutrient,
                      vars = c(conc_µM), 
                      groups = c(name, nutrient)) |> 
  select(-contains("min")) |> 
  as.data.frame()
tab_nutrient <- tab_nutrient %>%
  pivot_wider(names_from = nutrient, values_from = conc_µM_mean_sd)
write.xlsx(tab_nutrient, file = "output/tab_nutrient.xlsx")

# ghg ---------------------------------------------------------------------


df_ghg |> 
  distinct(species, sampling_date) |> 
  arrange(species, sampling_date)

descriptive_statistic(filter(df_ghg, period == "CP"),
                      groups = c(species, name),
                      vars = c(ghg_co2, ghg_ch4_mg, ghg_n2o)) %>% 
  select(-contains("min")) %>% 
  as.data.frame()


descriptive_statistic(filter(df_ghg, period == "CP"),
                      groups = c(species, sampling_month),
                      vars = c(ghg_co2, ghg_ch4_mg, ghg_n2o)) %>% 
  select(-contains("min")) %>% 
  as.data.frame()

descriptive_statistic(filter(df_ghg_co2e, period == "CP"),
                      groups = c(species, name),
                      vars = c(co2e_ghg, co2e_co2, co2e_ch4, co2e_n2o)) %>% 
  select(-contains("min")) %>% 
  as.data.frame()
wilcox.test(co2e_ghg ~ species, 
            data = filter(df_ghg_co2e, period == "CP"))


df_ghg_co2e %>% 
  filter(period == "CP") %>% 
  group_by(species, name) %>% 
  summarise(co2 = mean(co2e_co2, na.rm = T),
            ch4 = mean(co2e_ch4, na.rm = T),
            n2o = mean(co2e_n2o, na.rm = T),
            ghg = mean(co2e_ghg, na.rm = T)
  ) %>% 
  mutate(con_co2 = co2*100/ghg,
         con_ch4 = ch4*100/ghg,
         con_n2o = n2o*100/ghg
  ) %>% 
  select(species, name, ghg, contains('con'))


df <- df_ghg %>% 
  filter(period != 'EM', species == 'prawn',
         con %in% c('light', 'day'))
descriptive_statistic(df,
                      groups = c(period),
                      vars = c(ghg_co2, ghg_ch4_mg, ghg_n2o)) %>% 
  select(-contains("min")) %>% 
  as.data.frame()

## all-----

df <- filter(df_ghg, species == "prawn", period == "CP")

ks_test(df, "ghg_co2", "name") 
ks_test(df, "ghg_ch4", "name") 
ks_test(df, "ghg_n2o", "name") 

df <- filter(df_ghg, period == "CP")
wilcox.test(ghg_ch4 ~ species, df)
aggregate(ghg_ch4 ~ species, df, mean)
wilcox.test(ghg_n2o ~ species, df)
aggregate(ghg_ch4 ~ species, df, mean)


df_ghg_plot <- df_ghg %>%
  filter(period == "CP") %>% 
  mutate(new_species = factor(species, 
                                   levels = c("prawn", "tilapia"), 
                                   labels = c("M. rosenbergii", "O. aureus × O. niloticus"))
  ) %>%
  group_by(month, new_species, species, name) %>%
  summarise(ghg_co2    = mean(ghg_co2,    na.rm = T),
            sd_ch4_mg  = sd(ghg_ch4_mg,   na.rm = T),
            ghg_ch4_mg = mean(ghg_ch4_mg, na.rm = T),
            sd_n2o     = sd(ghg_n2o,      na.rm = T),
            ghg_n2o    = mean(ghg_n2o,    na.rm = T),
  )

fig_ghg_prawn_a <- plot_ghg_all(df = filter(df_ghg_plot, species == 'prawn'),
                                y = ghg_ch4_mg, ysd = sd_ch4_mg, plot = 'line') +
  ylab(lab_ch4_mg) +
  scale_color_manual(values = c("#ee9b00", "#ca6702", "#bb3e03")) +
  scale_fill_manual(values = c("#ee9b00", "#ca6702", "#bb3e03")) +
  ylim(-1, 3)
fig_ghg_prawn_b <- plot_ghg_all(df = filter(df_ghg_plot, species == 'prawn'),
                                y = ghg_ch4_mg, plot = 'box') + 
  ylab(NULL) +
  scale_fill_manual(values = c("#ee9b00", "#ca6702", "#bb3e03")) +
  ylim(-1, 3)
fig_ghg_prawn_c <- plot_ghg_all(df = filter(df_ghg_plot, species == 'prawn'),
                                y = ghg_n2o, ysd = sd_n2o, plot = 'line') +
  ylab(lab_n2o) +
  theme(strip.text.x = element_blank()) +
  scale_color_manual(values = c("#ee9b00", "#ca6702", "#bb3e03")) +
  scale_fill_manual(values = c("#ee9b00", "#ca6702", "#bb3e03")) +
  ylim(-300, 200)
fig_ghg_prawn_d <- plot_ghg_all(df = filter(df_ghg_plot, species == 'prawn'),
                                y = ghg_n2o, plot = 'box') + 
  ylab(NULL) +
  theme(strip.text.x = element_blank()) +
  scale_fill_manual(values = c("#ee9b00", "#ca6702", "#bb3e03")) +
  ylim(-300, 200)

fig_ghg_tilapia_a <- plot_ghg_all(df = filter(df_ghg_plot, species == 'tilapia'),
                                y = ghg_ch4_mg, ysd = sd_ch4_mg, plot = 'line') +
  ylab(lab_ch4_mg) +
  scale_color_manual(values = c("#94d2bd", "#0a9396", "#005f73")) +
  scale_fill_manual(values = c("#94d2bd", "#0a9396", "#005f73")) +
  ylim(-10, 40)
fig_ghg_tilapia_b <- plot_ghg_all(df = filter(df_ghg_plot, species == 'tilapia'),
                                y = ghg_ch4_mg, plot = 'box') + 
  ylab(NULL) +
  scale_fill_manual(values = c("#94d2bd", "#0a9396", "#005f73")) +
  ylim(-10, 40)
fig_ghg_tilapia_c <- plot_ghg_all(df = filter(df_ghg_plot, species == 'tilapia'),
                                y = ghg_n2o, ysd = sd_n2o, plot = 'line') +
  ylab(lab_n2o) +
  theme(strip.text.x = element_blank()) +
  scale_color_manual(values = c("#94d2bd", "#0a9396", "#005f73")) +
  scale_fill_manual(values = c("#94d2bd", "#0a9396", "#005f73")) +
  ylim(-50, 500)
fig_ghg_tilapia_d <- plot_ghg_all(df = filter(df_ghg_plot, species == 'tilapia'),
                                y = ghg_n2o, plot = 'box') + 
  ylab(NULL) +
  theme(strip.text.x = element_blank()) +
  scale_fill_manual(values = c("#94d2bd", "#0a9396", "#005f73")) +
  ylim(-50, 500)

fig_ghg_prawn <- ggarrange(fig_ghg_prawn_a, fig_ghg_prawn_b, 
          fig_ghg_prawn_c, fig_ghg_prawn_d, 
          ncol = 2, nrow = 2,
          widths = c(2, 1),
          font.label = list(size = 20, family = "Century Gothic"),
          common.legend = T, legend = "bottom")
fig_ghg_tilapia <- ggarrange(fig_ghg_tilapia_a, fig_ghg_tilapia_b, 
                           fig_ghg_tilapia_c, fig_ghg_tilapia_d, 
                           ncol = 2, nrow = 2,
                           widths = c(2, 1),
                           font.label = list(size = 20, family = "Century Gothic"),
                           common.legend = T, legend = "bottom")

png(filename = "output/ghg_all.png", width = 22, height = 11, units = "in", res = 300)
ggarrange(fig_ghg_prawn, fig_ghg_tilapia, 
          ncol = 2, nrow = 1,
          labels = c("(a)", "(b)"), 
          font.label = list(size = 20, family = "Century Gothic"),
          common.legend = T, legend = "bottom")
dev.off()

## drainage-----

df_ghg_drainage <- df_ghg %>% 
  filter(period != 'EM', species == 'prawn',
         con %in% c('light', 'day')) |> 
  mutate(day_drainage = case_when(
    name == "PT1" & sampling_date == as.Date("2025-02-26") ~ "D12",
    name == "PT2" & sampling_date == as.Date("2024-08-06") ~ "D2",
    name == "PT2" & sampling_date == as.Date("2025-02-05") ~ "D4",
    name == "PT2" & sampling_date == as.Date("2025-02-26") ~ "D25",
    name == "PT3" & sampling_date == as.Date("2025-02-26") ~ "D5",
    name == "PT3" & sampling_date == as.Date("2025-08-23") ~ "D2",
    name == "PT3" & sampling_date == as.Date("2025-10-29") ~ "D5"
  ),
  day_drainage = factor(day_drainage, 
                        levels = c("D2", "D4", "D5", "D12", "D25"))
  )

df_ghg_drainage %>% count(period)
tab_ghg_drainage <- descriptive_statistic(filter(df_ghg_drainage, period == "DP"),
                      vars = c(ghg_co2, ghg_ch4_mg, ghg_n2o),
                      groups = c(name, day_drainage, sampling_date)) |>
  select(-contains("min")) |> 
  as.data.frame() |> 
  mutate(day_drainage = factor(day_drainage, 
                               levels = c("D2", "D4", "D5", "D12", "D25"))) |> 
  arrange(name, day_drainage)
write.xlsx(tab_ghg_drainage, "output/df_ghg_drainage.xlsx")
tab_ghg_drainage

### test-----

descriptive_statistic(df_ghg_drainage,
                      vars = c(ghg_co2, ghg_ch4_mg, ghg_n2o),
                      groups = c(period)) |>
  select(-contains("min")) |> 
  as.data.frame()

# between period

wilcox.test(ghg_co2 ~ period, 
            data = df_ghg_drainage)
aggregate(ghg_co2 ~ period, 
          data = df_ghg_drainage,
          FUN = mean)
wilcox.test(ghg_ch4_mg ~ period, 
            data = df_ghg_drainage)
aggregate(ghg_ch4_mg ~ period, 
          data = df_ghg_drainage,
          FUN = mean)
wilcox.test(ghg_n2o ~ period, 
            data = df_ghg_drainage)
aggregate(ghg_n2o ~ period, 
          data = df_ghg_drainage,
          FUN = mean)

# between light/dark

df <- ghg %>% 
  filter(water == 'fw', type == 'mono', name != 'prawn T1',
         con %in% c('light', 'day', 'dark')) %>%
  mutate(ghg_ch4_mg = ghg_ch4 * µg_to_mg,
         name = dplyr::recode(name, 
                              'prawn T2' = 'PT1',
                              'prawn T3' = 'PT2',
                              'prawn T4' = 'PT3',
                              'tilapia T1' = 'TT1',
                              'tilapia T2' = 'TT2',
                              'tilapia T3' = 'TT3'),
         year_month = format(sampling_date, '%Y-%m'),
         month = format(sampling_date, '%b'),
         month = factor(month, levels = month.abb))
df <- separate_culture_period(df)
df <- df |> filter(period == 'DP', species == 'prawn')

wilcox.test(ghg_co2 ~ con, df)
wilcox.test(ghg_ch4 ~ con, df)

### envr-----

df_envr_soil <- df_envr |> filter(sample == "soil") |> 
  select(sampling_date, name, rep, envr_orp_0, envr_orp_5, envr_orp_10,
         envr_temp_0, envr_temp_5, envr_temp_10,
         envr_smc, envr_om, envr_dbd,
         envr_ph, envr_sal,
         envr_silt, envr_median, envr_sort) |> 
  mutate(day_drainage = case_when(
    name == "PT1" & sampling_date == as.Date("2025-02-26") ~ "D12",
    name == "PT2" & sampling_date == as.Date("2024-08-06") ~ "D2",
    name == "PT2" & sampling_date == as.Date("2025-02-05") ~ "D4",
    name == "PT2" & sampling_date == as.Date("2025-02-26") ~ "D25",
    name == "PT3" & sampling_date == as.Date("2025-02-26") ~ "D5",
    name == "PT3" & sampling_date == as.Date("2025-08-23") ~ "D2",
    name == "PT3" & sampling_date == as.Date("2025-10-29") ~ "D5"
  ),
  day_drainage = factor(day_drainage, 
                        levels = c("D2", "D4", "D5", "D12", "D25"))
  )

df_1 <- df_ghg_drainage |> 
  filter(period == "DP") |> 
  summarise(ghg_co2 = mean(ghg_co2, na.rm = TRUE),
            ghg_ch4 = mean(ghg_ch4, na.rm = TRUE),
            ghg_n2o = mean(ghg_n2o, na.rm = TRUE),
            .by = c(day_drainage, rep, name))

df_2 <- df_envr_soil |> 
  summarise(envr_orp_0 = mean(envr_orp_0, na.rm = TRUE),
            envr_orp_5 = mean(envr_orp_5, na.rm = TRUE),
            envr_orp_10 = mean(envr_orp_10, na.rm = TRUE),
            envr_temp_0 = mean(envr_temp_0, na.rm = TRUE),
            envr_temp_5 = mean(envr_temp_5, na.rm = TRUE),
            envr_temp_10 = mean(envr_temp_10, na.rm = TRUE),
            envr_smc = mean(envr_smc, na.rm = TRUE),
            envr_om = mean(envr_om, na.rm = TRUE),
            envr_dbd = mean(envr_dbd, na.rm = TRUE),
            envr_ph = mean(envr_ph, na.rm = TRUE),
            envr_sal = mean(envr_sal, na.rm = TRUE),
            envr_silt = mean(envr_silt, na.rm = TRUE),
            envr_median = mean(envr_median, na.rm = TRUE),
            envr_sort = mean(envr_sort, na.rm = TRUE),
            .by = c(day_drainage, rep, name))

df <- merge(df_1, df_2, by = c("day_drainage", "rep", "name"))

results <- rcorr(as.matrix(df[, -(1:3)]), type = "spearman")
results$P[is.na(results$P)] <- 0
corrplot(results$r, 
         method = 'color', 
         tl.pos = 'd',        
         tl.col = "black",    
         p.mat = results$P,   
         cl.pos = 'b',        
         col = colorRampPalette(c("#bb3e03", "#ca6702", "#ee9b00", 
                                  "#94d2bd", "#0a9396", "#005f73"))(500), 
         insig = 'label_sig',
         sig.level = c(0.001, 0.01, 0.05),  
         pch.cex = 1.5,      
         pch.col = 'black') 

period_ghg_fig_g <- plot_box(df_envr_soil, x = day_drainage, y = envr_orp_0, z = name) +
  xlab(NULL) +
  ylab(lab_orp) +
  scale_fill_manual(values = c("#ee9b00", "#ca6702", "#bb3e03"))
period_ghg_fig_h <- plot_box(df_envr_soil, x = day_drainage, y = envr_ph, z = name) +
  xlab(NULL) +
  ylab(lab_ph) +
  scale_fill_manual(values = c("#ee9b00", "#ca6702", "#bb3e03"))
period_ghg_fig_i <- plot_box(df_envr_soil, x = day_drainage, y = envr_silt, z = name) +
  xlab(NULL) +
  ylab(lab_silt) +
  scale_fill_manual(values = c("#ee9b00", "#ca6702", "#bb3e03"))


### plot----


period_ghg_fig_a <- plot_box(df_ghg_drainage, 
         x = period, y = ghg_co2, z = period) +
  xlab(NULL) +
  ylab(lab_co2) +
  see::scale_fill_okabeito() +
  ylim(-500, 1500) +
  annotate("text",
           x = c("DP", "CP"),
           y = c(1400, 650),
           label = c("a", "b"),
           fontface = "bold", family = "Century Gothic",
           size = 7) +
  theme(legend.position = "none")
period_ghg_fig_b <- plot_box(df_ghg_drainage, 
         x = period, y = ghg_ch4_mg, z = period) +
  xlab(NULL) +
  ylab(lab_ch4_mg) +
  see::scale_fill_okabeito() +
  ylim(-10, 150) +
  annotate("text",
           x = c("DP", "CP"),
           y = c(120, 25),
           label = c("a", "b"),
           fontface = "bold", family = "Century Gothic",
           size = 7) +
  theme(legend.position = "none")
period_ghg_fig_c <- plot_box(df_ghg_drainage,
         x = period, y = ghg_n2o, z = period) +
  xlab(NULL) +
  ylab(lab_n2o) +
  see::scale_fill_okabeito() +
  coord_cartesian(ylim = c(-500, 15000)) +
  annotate("text",
           x = c("DP", "CP"),
           y = c(14000, 1000),
           label = c("a", "b"),
           fontface = "bold", family = "Century Gothic",
           size = 7) +
  theme(legend.position = "none")
period_ghg_fig_d <- plot_box(filter(df_ghg_drainage, period == "DP"),
         x = day_drainage, y = ghg_co2, z = name) +
  xlab(NULL) +
  ylab(lab_co2) +
  scale_fill_manual(values = c("#ee9b00", "#ca6702", "#bb3e03"))
period_ghg_fig_e <- plot_box(filter(df_ghg_drainage, period == "DP"),
         x = day_drainage, y = ghg_ch4, z = name) +
  xlab(NULL) +
  ylab(lab_ch4) +
  scale_fill_manual(values = c("#ee9b00", "#ca6702", "#bb3e03"))
period_ghg_fig_f <- plot_box(filter(df_ghg_drainage, period == "DP"),
         x = day_drainage, y = ghg_n2o, z = name) +
  xlab(NULL) +
  ylab(lab_n2o) +
  scale_fill_manual(values = c("#ee9b00", "#ca6702", "#bb3e03"))

period_ghg_fig_abc <- ggarrange(period_ghg_fig_a, period_ghg_fig_b, period_ghg_fig_c,
          ncol = 3, nrow = 1,
          labels = c("(a)", "(b)", "(c)"), 
          font.label = list(size = 20, family = "Century Gothic"),
          common.legend = T, legend = "right")
period_ghg_fig_def <- ggarrange(period_ghg_fig_d, period_ghg_fig_e, period_ghg_fig_f,
                                period_ghg_fig_g, period_ghg_fig_h, period_ghg_fig_i,
          ncol = 3, nrow = 2,
          labels = c("(d)", "(e)", "(f)",
                     "(g)", "(h)", "(i)"), 
          font.label = list(size = 20, family = "Century Gothic"),
          common.legend = T, legend = "right")

png(filename = "output/ghg_drainage.png", width = 15, height = 15, units = "in", res = 300)
ggarrange(period_ghg_fig_abc, period_ghg_fig_def,
          ncol = 1, nrow = 2,
          heights = c(1, 2))
dev.off()


## feed regression-----

# For each row in pd_feed, calculate average ghg_n2o from df_ghg
pd_feed <- pt_info %>%
  mutate(name = case_when(
    name == "PT2" ~ "PT1",
    name == "PT3" ~ "PT2",
    name == "PT4" ~ "PT3"
  )) %>% 
  rowwise() %>%
  mutate(
    avg_ghg_co2 = {
      pond_name <- name
      df_ghg %>%
        filter(
          name == pond_name,
          sampling_date >= stock_date,
          sampling_date <= harvest_date
        ) %>%
        pull(ghg_co2) %>%
        mean(na.rm = TRUE)
    },
    avg_ghg_ch4 = {
      pond_name <- name
      df_ghg %>%
        filter(
          name == pond_name,
          sampling_date >= stock_date,
          sampling_date <= harvest_date
        ) %>%
        pull(ghg_ch4) %>%
        mean(na.rm = TRUE)
    },
    avg_ghg_n2o = {
      pond_name <- name
      df_ghg %>%
        filter(
          name == pond_name,
          sampling_date >= stock_date,
          sampling_date <= harvest_date
        ) %>%
        pull(ghg_n2o) %>%
        mean(na.rm = TRUE)
    }
  ) %>%
  ungroup()

# Linear regression
lm_feed_co2 <- lm(avg_ghg_co2 ~ input_rate, data = pd_feed)
summary(lm_feed_co2)
lm_feed_ch4 <- lm(avg_ghg_ch4 ~ input_rate, data = pd_feed)
summary(lm_feed_ch4)
lm_feed_n2o <- lm(avg_ghg_n2o ~ input_rate, data = pd_feed)
summary(lm_feed_n2o)

fig_feed_co2 <- plot_ghg_feed(avg_ghg_co2) + ylab(lab_co2)
fig_feed_ch4 <- plot_ghg_feed(avg_ghg_ch4) + ylab(lab_ch4_mg)
fig_feed_n2o <- plot_ghg_feed(avg_ghg_n2o) + ylab(lab_n2o)

png(filename = "output/ghg_feed.png", width = 18, height = 7, units = "in", res = 300)
ggarrange(fig_feed_co2, fig_feed_ch4, fig_feed_n2o,
          ncol = 3, nrow = 1,
          labels = c("(a)", "(b)", "(c)"), 
          font.label = list(size = 20, family = "Century Gothic"),
          common.legend = T, legend = "bottom")
dev.off()

## diurnal-----


df_ghg_diurnal <- ghg %>% 
  filter(water == 'fw', type == 'mono', name != 'prawn T1') %>%
  filter(year == '2025' & sampling %in% c('4th', '6th')) %>% 
  mutate(ghg_ch4_mg = ghg_ch4 * µg_to_mg,
         name = dplyr::recode(name, 
                              'prawn T2' = 'PT2',
                              'prawn T3' = 'PT3',
                              'prawn T4' = 'PT4',
                              'tilapia T1' = 'TT1',
                              'tilapia T2' = 'TT2',
                              'tilapia T3' = 'TT3'),
         year_month = format(sampling_date, '%Y-%m'),
         month = format(sampling_date, '%b'),
         month = factor(month, levels = month.abb)) %>% 
  mutate(new_species = factor(species, 
                              levels = c("prawn", "tilapia"), 
                              labels = c("M. rosenbergii", "O. aureus × O. niloticus")),
         con = factor(con, levels = c("day", "night"),
                      labels = c("Day", "Night"))
  )

descriptive_statistic(df_ghg_diurnal,
                      vars = c(ghg_co2, ghg_ch4_mg, ghg_n2o), 
                      groups = c(species, con)) %>% 
  select(-contains('min'))

df_envr_diurnal <- envr %>% 
  filter(water == 'fw', type == 'mono', name != 'prawn T1') %>%
  filter(year == '2025' & sampling %in% c('4th', '6th')) %>% 
  mutate(name = dplyr::recode(name, 
                              'prawn T2' = 'PT2',
                              'prawn T3' = 'PT3',
                              'prawn T4' = 'PT4',
                              'tilapia T1' = 'TT1',
                              'tilapia T2' = 'TT2',
                              'tilapia T3' = 'TT3'),
         year_month = format(sampling_date, '%Y-%m'),
         month = format(sampling_date, '%b'),
         month = factor(month, levels = month.abb)) %>% 
  mutate(new_species = factor(species, 
                              levels = c("prawn", "tilapia"), 
                              labels = c("M. rosenbergii", "O. aureus × O. niloticus")),
         con = factor(con, levels = c("day", "night"),
                      labels = c("Day", "Night"))
  )

descriptive_statistic(df_envr_diurnal,
                      vars = c(envr_temp, envr_ec, envr_do_pc, envr_do_w, 
                               envr_ph, envr_ntu, envr_sal),
                      groups = c(species, con)) %>% 
  select(-contains('min')) |> 
  as.data.frame()

### test-----

df <- df_ghg_diurnal %>% filter(species == "prawn")
wilcox.test(ghg_co2 ~ con, df)
aggregate(ghg_co2 ~ con, df, FUN = mean)
wilcox.test(ghg_ch4 ~ con, df)
aggregate(ghg_ch4 ~ con, df, FUN = mean)
wilcox.test(ghg_n2o ~ con, df)
aggregate(ghg_n2o ~ con, df, FUN = mean)

df <- df_ghg_diurnal %>% filter(species == "tilapia")
wilcox.test(ghg_co2 ~ con, df)
aggregate(ghg_co2 ~ con, df, FUN = mean)
wilcox.test(ghg_ch4 ~ con, df)
aggregate(ghg_ch4 ~ con, df, FUN = mean)
wilcox.test(ghg_n2o ~ con, df)
aggregate(ghg_n2o ~ con, df, FUN = mean)


df <- filter(df_envr_diurnal, species == "prawn")
wilcox.test(envr_temp ~ con, df)
wilcox.test(envr_ec ~ con, df)
wilcox.test(envr_do_pc ~ con, df)
wilcox.test(envr_do_w ~ con, df)
wilcox.test(envr_ph ~ con, df)
wilcox.test(envr_ntu ~ con, df)
df <- filter(df_envr_diurnal, species == "tilapia")
wilcox.test(envr_temp ~ con, df)
wilcox.test(envr_ec ~ con, df)
wilcox.test(envr_do_pc ~ con, df)
wilcox.test(envr_do_w ~ con, df)
wilcox.test(envr_ph ~ con, df)
wilcox.test(envr_ntu ~ con, df)


### plot----

ghg_diurnal_fig_a <-
  ghg_diurnal_fig_a <-plot_box(df_ghg_diurnal, x = new_species, y = ghg_co2, z = con) +
  xlab(NULL) +
  ylab(lab_co2) +
  see::scale_fill_okabeito() +
  ylim(-200, 800) +
  annotate("text",
           x = c(0.81, 1.19, 1.81, 2.19),
           y = c(125, 175, 770, 300),
           label = c("b", "a", "a", "b"),
           fontface = "bold", family = "Century Gothic",
           size = 6) +
  theme(axis.text.x = element_text(face = "bold.italic"))
ghg_diurnal_fig_b <-
  plot_box(df_ghg_diurnal, x = new_species, y = ghg_ch4_mg, z = con) +
  xlab(NULL) +
  ylab(lab_ch4_mg) +
  see::scale_fill_okabeito() +
  ylim(-0.4, 40) +
  annotate("text",
           x = c(0.81, 1.19, 1.81, 2.19),
           y = c(5, 5, 38, 5),
           label = c("a", "b", "a", "b"),
           fontface = "bold", family = "Century Gothic",
           size = 6) +
  theme(axis.text.x = element_text(face = "bold.italic"))
ghg_diurnal_fig_c <- 
  plot_box(df_ghg_diurnal, x = new_species, y = ghg_n2o, z = con) +
  xlab(NULL) +
  ylab(lab_n2o) +
  see::scale_fill_okabeito() +
  theme(axis.text.x = element_text(face = "bold.italic")) +
  ylim(-100, 400)
ghg_diurnal_fig_d <- 
  plot_box(df_envr_diurnal, x = new_species, y = envr_temp, z = con) +
  xlab(NULL) +
  ylab(lab_temp) +
  see::scale_fill_okabeito() +
  ylim(25, 40) +
  annotate("text",
           x = c(0.81, 1.19, 1.81, 2.19),
           y = c(37.5, 35, 35, 34),
           label = c("a", "b", "a", "b"),
           fontface = "bold", family = "Century Gothic",
           size = 6) +
  theme(axis.text.x = element_text(face = "bold.italic")) 
ghg_diurnal_fig_e <-
  plot_box(df_envr_diurnal, x = new_species, y = envr_do_w, z = con) +
  xlab(NULL) +
  ylab(lab_do_w) +
  see::scale_fill_okabeito() +
  ylim(0, 20) +
  annotate("text",
           x = c(0.81, 1.19),
           y = c(15, 12),
           label = c("a", "b"),
           fontface = "bold", family = "Century Gothic",
           size = 6) +
  theme(axis.text.x = element_text(face = "bold.italic")) 
ghg_diurnal_fig_f <- 
  plot_box(df_envr_diurnal, x = new_species, y = envr_ph, z = con) +
  xlab(NULL) +
  ylab(lab_ph) +
  see::scale_fill_okabeito() +
  ylim(7.5, 9.5) +
  annotate("text",
           x = c(0.81, 1.19),
           y = c(9.3, 8.3),
           label = c("a", "b"),
           fontface = "bold", family = "Century Gothic",
           size = 6) +
  theme(axis.text.x = element_text(face = "bold.italic")) 

png("output/ghg_diurnal.png", 
    width = 22, height = 12, units = "in", res = 300)
ggarrange(ghg_diurnal_fig_a, ghg_diurnal_fig_b, ghg_diurnal_fig_c,
          ghg_diurnal_fig_d, ghg_diurnal_fig_e, ghg_diurnal_fig_f,
          ncol = 3, nrow = 2,
          labels = c("(a)", "(b)", "(c)",
                     "(d)", "(e)", "(f)"),
          font.label = list(size = 20, family = "Century Gothic"),
          common.legend = T, legend = "bottom")
dev.off()

## comparison-----

ghg_lit_raw <- read.xlsx("data/20260219_tab_ghg_studies.xlsx")

df_lit <- ghg_lit_raw %>%
  fill(species, system, period, method, reference,
       .direction = "down")

df_lit$ch4_mgm2h <- gsub("−", "-", df_lit$ch4_mgm2h)
df_lit$n2o_mgm2h <- gsub("−", "-", df_lit$n2o_mgm2h)

df_lit <- df_lit[-1,]

df_lit <- df_lit |> filter(ch4_mgm2h != "ND" | n2o_mgm2h != "ND")
df_lit$ID <- 1:55

df_lit <- df_lit |>
  mutate(new_species = classify_species(species),
         new_species = factor(new_species))

df_lit <- df_lit %>%
  mutate(
    # Remove the brackets
    ref_cleaned = str_remove_all(reference, "\\[\\[|\\]\\]"),
    # Extract author and year
    author = str_extract(ref_cleaned, "^[A-Za-z]+"),
    year = str_extract(ref_cleaned, "\\d{4}"),
    # Format the output
    formatted_ref = paste(author, " et al. (", year, ")", sep = "")
  )
write.xlsx(df_lit, "output/ID_ref.xlsx")

### ch4-----

lit_ch4 <- df_lit %>%
  filter(ch4_mgm2h != "ND")
# Apply the function to the ch4 column
lit_ch4 <- lit_ch4 %>%
  rowwise() %>%
  mutate(values = list(extract_values(ch4_mgm2h)),
         system = ifelse(system == "Monoculture", "Monoculture", "Polyculture/others")) %>%
  unnest_wider(values) %>%
  mutate(min = as.numeric(min),
         max = as.numeric(max),
         mean = as.numeric(mean),
         sd = as.numeric(sd),
         median = as.numeric(median),
         study = "B.Ref") |>
  select(new_species, species, system, ID, min, max, mean, sd, median, formatted_ref)
numbers_part <- unique(lit_ch4$ID)




this_ch4 <- df_ghg %>%
  filter(period == "CP") |>
  group_by(species) %>%
  summarise(min = min(ghg_ch4_mg, na.rm = T),
            max = max(ghg_ch4_mg, na.rm = T),
            mean = mean(ghg_ch4_mg, na.rm = T),
            sd = sd(ghg_ch4_mg, na.rm = T),
            median = median(ghg_ch4_mg, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(formatted_ref = "This study",
         system = "Monoculture",
         ID = toupper(letters[1:2]),
         new_species = ifelse(species == "prawn", "crustaceans", "finfish")
  ) |>
  dplyr::select(new_species, species, system, ID, min, max, mean, sd, median, formatted_ref)

cmp_ch4 <- rbind(lit_ch4, this_ch4)

letters_part <- toupper(letters[1:2])
cmp_ch4$ID <- factor(cmp_ch4$ID, levels = c(letters_part, numbers_part))

#tiff("compare_ch4.png", units="in", width = 5, height = 10, res=300)
#dev.off()

fig_cmp_ch4 <- ggplot(cmp_ch4, aes(x = ID)) +
  facet_wrap(system ~ new_species, scale = "free", 
             ncol = 3) +
  geom_point(aes(y = min), shape = 25, size = 4, fill = "#F5C710", stroke = 0, na.rm = TRUE) +  # Change x to y
  geom_point(aes(y = max), shape = 24, size = 4, fill = "#D55E00", stroke = 0, na.rm = TRUE) +  # Change x to y
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.0, color = "black", na.rm = TRUE) +  # Change xmin/xmax to ymin/ymax
  geom_point(aes(y = mean), shape = 21, size = 4, fill = "#0072B2", stroke = 0, na.rm = TRUE) +  # Change x to y
  geom_point(aes(y = median), shape = 22, size = 3, fill = "#999999", stroke = 0, na.rm = TRUE) +  # Change x to y
  theme_bw(base_size = 15) +
  theme(text = element_text(family = "Century Gothic", color = "black"),
        strip.text = element_text(face = "bold", color = "black"),
        axis.text = element_text(face = "bold", color = "black"),
        legend.text = element_text(face = "bold", color = "black"),
        plot.background = element_rect(fill = "transparent", color = NA),
        strip.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.title = element_blank(),
        panel.grid.major = element_line(color = "gray95"),  
        panel.grid.minor = element_blank(),
        legend.position = "top",
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab(lab_ch4_mg) +  
  xlab(expression(bold(paste("ID")))) 

### n2o-----

lit_n2o <- df_lit %>%
  filter(n2o_mgm2h != "ND")

# Apply the function to the n2o column
lit_n2o <- lit_n2o %>%
  rowwise() %>%
  mutate(values = list(extract_values(n2o_mgm2h)),
         system = ifelse(system == "Monoculture", "Monoculture", "Polyculture/others")) %>%
  unnest_wider(values) %>%
  mutate(min = as.numeric(min),
         max = as.numeric(max),
         mean = as.numeric(mean),
         sd = as.numeric(sd),
         median = as.numeric(median),
         study = "B.Ref") |>
  select(new_species, species, system, ID, min, max, mean, sd, median, formatted_ref)
numbers_part <- unique(lit_n2o$ID)

this_n2o <- df_ghg %>%
  filter(period == "CP") |>
  mutate(ghg_n2o_mg = ghg_n2o * µg_to_mg) |>
  group_by(species) %>%
  summarise(min = min(ghg_n2o_mg, na.rm = T),
            max = max(ghg_n2o_mg, na.rm = T),
            mean = mean(ghg_n2o_mg, na.rm = T),
            sd = sd(ghg_n2o_mg, na.rm = T),
            median = median(ghg_n2o_mg, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(formatted_ref = "This study",
         system = "Monoculture",
         ID = toupper(letters[1:2]),
         new_species = ifelse(species == "prawn", "crustaceans", "finfish")
  ) |>
  dplyr::select(new_species, species, system, ID, min, max, mean, sd, median, formatted_ref)

cmp_n2o <- rbind(lit_n2o, this_n2o)

letters_part <- toupper(letters[1:2])
cmp_n2o$ID <- factor(cmp_n2o$ID, levels = c(letters_part, numbers_part))

fig_cmp_n2o <- ggplot(cmp_n2o, aes(x = ID)) +
  facet_wrap(system ~ new_species, scale = "free") +
  geom_point(aes(y = min), shape = 25, size = 4, fill = "#F5C710", stroke = 0, na.rm = TRUE) +
  geom_point(aes(y = max), shape = 24, size = 4, fill = "#D55E00", stroke = 0, na.rm = TRUE) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.0, color = "black", na.rm = TRUE) +
  geom_point(aes(y = mean), shape = 21, size = 4, fill = "#0072B2", stroke = 0, na.rm = TRUE) +
  geom_point(aes(y = median), shape = 22, size = 3, fill = "#999999", stroke = 0, na.rm = TRUE) +
  theme_bw(base_size = 15) +
  theme(text = element_text(family = "Century Gothic", color = "black"),
        strip.text = element_text(face = "bold", color = "black"),
        axis.text = element_text(face = "bold", color = "black"),
        legend.text = element_text(face = "bold", color = "black"),
        plot.background = element_rect(fill = "transparent", color = NA),
        strip.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.title = element_blank(),
        panel.grid.major = element_line(color = "gray95"),  
        panel.grid.minor = element_blank(),
        legend.position = "top",
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab(lab_n2o_mg) +
  xlab(NULL)

png(filename = "output/comparison.png", width = 9, height = 18, units = "in", res = 300)
ggarrange(fig_cmp_ch4, fig_cmp_n2o, 
          ncol = 1, nrow = 2,
          heights = c(2, 1),
          labels = c("(a)", "(b)"),  
          font.label = list(size = 16, family = "Century Gothic"))
dev.off()

# co2e --------------------------------------------------------------------

df_ghg_co2e <- df_ghg_co2e %>% 
  mutate(new_species = factor(species, 
                              levels = c("prawn", "tilapia"), 
                              labels = c("M. rosenbergii", "O. aureus × O. niloticus"))
  ) %>% 
  filter(period == "CP")

co2_flux <- df_ghg_co2e %>%
  mutate(flux = co2e_co2, ghg = "co2") %>%
  group_by(year, sampling, species, new_species, name, ghg) %>%
  summarise(flux = mean(flux, na.rm = T))
ch4_flux <- df_ghg_co2e %>%
  mutate(flux = co2e_ch4, ghg = "ch4") %>%
  group_by(year, sampling, species, new_species, name, ghg) %>%
  summarise(flux = mean(flux, na.rm = T))
n2o_flux <- df_ghg_co2e %>%
  mutate(flux = co2e_n2o, ghg = "n2o") %>%
  group_by(year, sampling, species, new_species, name, ghg) %>%
  summarise(flux = mean(flux, na.rm = T))
df_ghg_flux <- rbind(co2_flux, ch4_flux, n2o_flux)
df_ghg_flux$ghg <- factor(df_ghg_flux$ghg, levels = c("co2", "ch4", "n2o"))

co2e_fig_a <- ggplot(filter(df_ghg_co2e, species == "prawn")) + 
  geom_boxplot(aes(x = name, y = co2e_ghg), show.legend = F,fill = "lightgray") +
  theme_classic(base_size = 25) + 
  facet_grid(. ~ new_species, scales = "free") +
  geom_jitter(data = filter(df_ghg_flux, species == "prawn"),
              aes(x = name, y = flux, fill = ghg),
              shape = 21, position = position_jitterdodge(jitter.width = 0.9, dodge.width = 0.3), size = 3) +
  stat_summary(aes(x = name, y = co2e_ghg), 
               fun = mean, geom = "point",
               shape = 22, size = 2,
               color="black", fill = "lightgray",
               position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("co2" = "#e76f51", "ch4" = "#ffbe0b", "n2o" = "#729aba"), 
                    labels = c(bquote(bold("CO"[2]~"flux")),
                               bquote(bold("CH"[4]~"flux")), 
                               bquote(bold("N"[2]*"O flux"))),
                    name = "") +
  ylab(expression(bold(paste("Total C", O[2], "e fluxes (g C", O[2], "e" ~ m^-2 ~ 
                               d^-1, ")")))) +
  xlab(NULL) +
  theme(text = element_text(family = "Century Gothic", color = "black"),
        strip.text.x = element_text(face = "bold.italic", color = "black"),
        strip.text.y = element_text(face = "bold", color = "black"),
        axis.text = element_text(face = "bold", color = "black"),
        legend.text = element_text(face = "bold", color = "black"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.position = "bottom")

co2e_fig_b <- ggplot(filter(df_ghg_co2e, species == "tilapia")) + 
  geom_boxplot(aes(x = name, y = co2e_ghg), show.legend = F,fill = "lightgray") +
  theme_classic(base_size = 25) + 
  facet_grid(. ~ new_species, scales = "free") +
  geom_jitter(data = filter(df_ghg_flux, species == "tilapia"),
              aes(x = name, y = flux, fill = ghg),
              shape = 21, position = position_jitterdodge(jitter.width = 0.9, dodge.width = 0.3), size = 3) +
  stat_summary(aes(x = name, y = co2e_ghg), 
               fun = mean, geom = "point",
               shape = 22, size = 2,
               color="black", fill = "lightgray",
               position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("co2" = "#e76f51", "ch4" = "#ffbe0b", "n2o" = "#729aba"), 
                    labels = c(bquote(bold("CO"[2]~"flux")),
                               bquote(bold("CH"[4]~"flux")), 
                               bquote(bold("N"[2]*"O flux"))),
                    name = "") +
  ylab(expression(bold(paste("Total C", O[2], "e fluxes (g C", O[2], "e" ~ m^-2 ~ 
                               d^-1, ")")))) +
  xlab(NULL) +
  theme(text = element_text(family = "Century Gothic", color = "black"),
        strip.text.x = element_text(face = "bold.italic", color = "black"),
        strip.text.y = element_text(face = "bold", color = "black"),
        axis.text = element_text(face = "bold", color = "black"),
        legend.text = element_text(face = "bold", color = "black"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.position = "bottom")

png(filename = "output/ghg_co2e.png", width = 16, height = 8, units = "in", res = 300)
ggarrange(co2e_fig_a, co2e_fig_b, ncol = 2, nrow = 1,
          labels = c("(a)", "(b)"),  
          font.label = list(size = 16, family = "Century Gothic"),
          common.legend = T, legend = "bottom")
dev.off()


# do ----------------------------------------------------------------------

df_do %>% 
  filter(gpp <= 0 | rday <= 0) 

df_do_filtered <- df_do %>%
  filter(gpp > 0 & rday > 0) 

descriptive_statistic(filter(df_do_filtered, period == "CP"),
                      vars = c(gpp, rday),
                      groups = c(species, name)) %>%
  select(-contains("min"))

descriptive_statistic(filter(df_do_filtered, period == "CP"),
                      vars = c(gpp, rday),
                      groups = c(species, name, sampling_month)) %>%
  select(-contains("min")) |> 
  as.data.frame()

df_do_plot <- df_do_filtered %>%
  filter(period == "CP") %>%
  mutate(new_species = factor(species,
                              levels = c("prawn", "tilapia"),
                              labels = c("M. rosenbergii", "O. aureus × O. niloticus"))) %>% 
  group_by(new_species, year_month, name) %>% 
  summarise(gpp = mean(gpp),
            rday = mean(rday)) %>% 
  mutate(pr_ratio = gpp/rday)


descriptive_statistic(filter(df_do_plot),
                      vars = c(pr_ratio),
                      groups = c(new_species))

fig_do_pr <- ggplot(df_do_plot, aes(x = new_species, y = pr_ratio)) +
  geom_jitter(aes(color = name), width = 0.2, size = 2, alpha = 0.6) +
  geom_violin(trim = FALSE, alpha = 0.7, fill = "gray90", linewidth = 0.5) +
  geom_boxplot(width = 0.1, outliers = FALSE, fill = "gray90", linewidth = 0.5) +
  stat_summary(fun = mean, geom = "point",shape = 22, size = 2, fill = 'gray90', stroke = 0.5) +
  scale_color_manual(values = c("PT1" = "#ee9b00", "PT2" = "#ca6702", "PT3" = "#bb3e03",
                                "TT1" = "#94d2bd", "TT2" = "#0a9396", "TT3" = "#005f73")) +
  xlab(NULL) +
  ylab(expression(bold("P:R ratio (GPP/ER)"))) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", linewidth = 0.5) +
  theme_classic(base_size = 25) +
  theme(text = element_text(family = "Century Gothic", color = "black"),
        axis.text.x = element_text(face = "bold.italic", color = "black"),
        axis.text.y = element_text(face = "bold", color = "black"),
        legend.text = element_text(face = "bold", color = "black"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.title = element_blank(),
        legend.position = "bottom") +
  coord_cartesian(ylim = c(0, 2)) 

df_do_plot %>% filter(pr_ratio > 2) 

df_do_line <- df_do_filtered %>%
  filter(period == "CP") %>%
  select(year, species, name, sampling_month, gpp, rday) %>%
  pivot_longer(cols = c(gpp, rday),
               names_to = "do_name",
               values_to = "do_value") %>%
  mutate(new_species = factor(species,
                              levels = c("prawn", "tilapia"),
                              labels = c("M. rosenbergii", "O. aureus × O. niloticus")),
         species = factor(species,
                              levels = c("prawn", "tilapia"),
                              labels = c("Prawn", "Tilapia"))) %>%
  group_by(new_species, species, name, sampling_month, do_name) %>%
  summarise(mean = mean(do_value, na.rm = TRUE),
            sd = sd(do_value, na.rm = TRUE)) %>%
  ungroup()

fig_do_line <- ggplot(df_do_line, aes(x = sampling_month, y = mean,
                              color = name, linetype = do_name,
                              group = interaction(name, do_name))) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = name),
              alpha = 0.1, color = NA) +
  geom_line(linewidth = 1.5) +
  facet_grid(new_species ~ ., scales = "free") +
  scale_color_manual(values = c("PT1" = "#ee9b00", "PT2" = "#ca6702", "PT3" = "#bb3e03",
                                "TT1" = "#94d2bd", "TT2" = "#0a9396", "TT3" = "#005f73")) +
  scale_fill_manual(values = c("PT1" = "#ee9b00", "PT2" = "#ca6702", "PT3" = "#bb3e03",
                                "TT1" = "#94d2bd", "TT2" = "#0a9396", "TT3" = "#005f73")) +
  scale_linetype_manual(values = c("gpp" = "solid", "rday" = "dotdash"),
                        labels = c("gpp" = "GPP", "rday" = expression("ER")),
                        name = NULL) +
  xlab(NULL) +
  ylab(expression(bold("DO metabolism (g O"[2] ~ m^-2 ~ d^-1*")"))) +
  theme_classic(base_size = 25) +
  theme(text = element_text(family = "Century Gothic", color = "black"),
        axis.text = element_text(face = "bold", color = "black"),
        legend.text = element_text(face = "bold", color = "black"),
        strip.text.y = element_text(face = "bold.italic"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.title = element_blank(),
        legend.position = "top")

png(filename = "output/do_line.png", width = 16, height = 10, units = "in", res = 300)
ggarrange(fig_do_line, fig_do_pr, 
          ncol = 2, nrow = 1,
          labels = c("(a)", "(b)"), 
          font.label = list(size = 20, family = "Century Gothic"),
          common.legend = F, legend = "bottom")
dev.off()

# budget -------------------------------------------------------------------------

df_budget <- budget %>% 
  filter(water == 'fw', type == 'mono', species == 'tilapia') %>% 
  select(year, name, contains("cb"))

df_budget <- df_budget %>% 
  mutate(cb_output = cb_rday + cb_ghg_out + cb_sa + cb_outflow_all + cb_harvest,
         cb_input = cb_gpp + cb_ghg_in + cb_feed + cb_inflow_all + cb_stock + cb_rain,
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

df_budget |> 
  select(name, contains("con_"))

budget_out <- df_budget %>%
  select(name, cb_rday, cb_ghg_out, cb_sa, 
         cb_outflow_all, cb_harvest, cb_output)
write.xlsx(budget_out, file = "output/tilapia_budget_out.xlsx")

budget_in <- df_budget %>%
  select(name, cb_gpp, cb_ghg_in, cb_feed, 
         cb_inflow_all, cb_stock, cb_rain, cb_input) 
write.xlsx(budget_in, file = "output/tilapia_budget_in.xlsx")

pt_info <- pt_info %>% mutate(stock_total_kg = stock_ind * stock_ind_g * g_to_kg)
descriptive_statistic(pt_info,
                      groups = name,
                      vars = c(area_m2, stock_density,
                               stock_total_kg, harvest_total_kg,
                               input_rate, culture_day)) %>% 
  select(-contains("min")) %>% 
  as.data.frame()

info %>% 
  filter(species == "tilapia") %>% 
  select(site, area_m2, stock_density,
         stock_animals_total_kg, harvest_animals_total_kg,
         feed_input_rate, day_culture) %>% 
  as.data.frame()


# cor ---------------------------------------------------------------------

df_cor <- raw_budget %>%
  filter(water == 'fw', type == 'mono', name != 'prawn T1')
results_cor <- create_cor_matrix(df_cor)
results_cor$P[is.na(results_cor$P)] <- 0
cor_name <- c("$CO[2]", "$CH[4]", "$N[2]*O", "ER", "GPP",
              "SA", "Temp", "EC", "DO", "pH", "NTU", "Sal", "SS", "Chla", 
              "DOC", "DIC", "POC", '$PO[4]^"3-"', '$NH[4]^"+"', '$NO[3]^"-"', '$NO[2]^"-"')
colnames(results_cor$r) <- cor_name
rownames(results_cor$r) <- cor_name
colnames(results_cor$P) <- cor_name
rownames(results_cor$P) <- cor_name

png(filename = "output/cor.png", width = 10, height = 10, units = "in", res = 300)
corrplot.mixed(results_cor$r, 
               lower = 'number',
               upper = 'circle',
               tl.pos = 'd',        
               tl.col = "black",    
               p.mat = results_cor$P, 
               upper.col = COL2('PRGn'), 
               lower.col = COL2('PRGn'), 
               insig = 'blank',
               sig.level = c(0.001, 0.01, 0.05),  
               pch.cex = 1.5,      
               pch.col = 'black')
dev.off()

# pls-sem -----------------------------------------------------------------
# Model specification history:
#   v1 (original)  — all constructs mode_B
#   v2             — OM mode_B; envr_temp, pon, envr_chla, envr_nh4, envr_no2 removed
#   v3             — OM switched to reflective (mode_A): poc, pon, NTU, SS
#                    REDOX: envr_temp removed; NUTRIENT: envr_nh4, envr_no2 removed
#   v4             — SA (sediment carbon accumulation, c_sa) added
#   v5             — TEMP (envr_temp) added as standalone single-item exogenous
#                    construct with direct paths to CO2, CH4, N2O.
#                    Tested as formative indicator within REDOX (Options 1a/1b) —
#                    both reduced R²; collinearity with pH and DIC confirmed.
#                    Standalone specification produced significant TEMP -> CH4
#                    (T=2.15) and TEMP -> N2O (T=2.04) paths, improving R² for
#                    CH4 (+0.026) and N2O (+0.033) vs v4. VIF(TEMP) = 1.49.
#                    Weather variables (wx_temp, wx_wind, wx_pressure, wx_humidity)
#                    tested in mode_B 4-item, mode_A pressure+wind, single-item
#                    wx_wind, and reflective envr_temp+wx_temp specifications;
#                    all structural paths non-significant, all GHG R² declined.
#                    Structural alternatives M2 (CO2→CH4/N2O), M3 (OM→SA), M4
#                    (M2+M3) tested via BIC and bootstrapped paths: M2 produces
#                    β>1.0 on CO2→N2O, suppresses 6/8 significant paths, 40 boot
#                    non-convergences, and 5 MGA non-invariant paths. M3 loses
#                    SA→CO2 and SA→N2O significance; 5 MGA non-invariant paths.
#                    BIC favours M1/M2 over M3/M4 for OM and CO2 equations.
#                    v5 (M1) retained: 8 significant paths, full MGA invariance,
#                    no bootstrap instability.
#   v6 (final)     — Two structural paths added from exploratory tests:
#                    (1) TEMP -> REDOX: temperature regulates water-column redox
#                    chemistry (DO, pH, DIC). T=3.45, β=0.39, f²=0.183 (medium).
#                    TEMP -> REDOX/NUTRIENT/OM all tested; TEMP -> OM non-sig
#                    (T=0.55); TEMP -> NUTRIENT borderline (T=2.04, CI clips 0).
#                    (2) OM -> SA: particulate organic matter drives sediment
#                    carbon accumulation. T=3.14, β=0.39, f²=0.174 (medium).
#                    REDOX -> SA and NUTRIENT -> SA also tested; NUTRIENT non-sig
#                    (T=0.66); REDOX borderline (T=1.84, CI clips 0).
#                    DOC tested as standalone construct -> GHG (Option A): all
#                    three DOC paths non-sig (max T=1.10); dropped.
#                    v6 adds REDOX and SA as endogenous variables; GHG R²
#                    unchanged; full MGA invariance; VIF < 5.0 throughout.
#                    New HTMT note: TEMP-NUTRIENT 90% CI upper = 0.907 (marginal
#                    violation, structural artifact of TEMP -> REDOX -> NUTRIENT
#                    chain; acknowledged limitation alongside REDOX-NUTRIENT
#                    HTMT = 1.059).

df_sem <- raw_budget %>%
  filter(water == "fw", type == "mono") %>%
  dplyr::select(year, sampling_date, name, species, sampling,
                envr_ph, envr_ec,
                envr_do_w, envr_do_pc,
                envr_ss, envr_ntu,
                poc, pon, doc, dic, dtn,
                envr_no3, envr_po4,
                ghg_co2, ghg_ch4, ghg_n2o,
                c_sa, envr_temp) %>%
  filter(if_all(everything(), ~ !is.na(.) & !is.nan(.)))

## Sample size check -------------------------------------------------------

cat("=== Sample size after listwise deletion ===\n")
cat("n total =", nrow(df_sem), "\n")
cat("Species breakdown:\n")
print(table(df_sem$species))

beta_min      <- 0.38
n_min         <- ceiling((2.486 / beta_min)^2)
max_arrows_in <- 5  # CO2/CH4/N2O receive 5 paths: REDOX, OM, NUTRIENT, SA, TEMP
                    # (unchanged from v5; TEMP->REDOX and OM->SA do not add arrows into GHG)
cat("\n10x rule minimum n:", max_arrows_in * 10,
    "| Your n =", nrow(df_sem),
    "->", ifelse(nrow(df_sem) >= max_arrows_in * 10, "[OK]", "[WARNING]"), "\n")
cat("beta_min minimum n:", n_min,
    "| Your n =", nrow(df_sem),
    "->", ifelse(nrow(df_sem) >= n_min, "[OK]", "[WARNING]"), "\n")


## Model specification -----------------------------------------------------

library("seminr")


# Final measurement model (v6)
measurements <- constructs(
  composite("CO2",      single_item("ghg_co2")),
  composite("CH4",      single_item("ghg_ch4")),
  composite("N2O",      single_item("ghg_n2o")),

  # REDOX: formative (mode_B)
  # envr_temp excluded: collinear with pH and DIC (tested in v5, reduced R²)
  composite("REDOX",    multi_items("", c("envr_ph", "dic", "envr_do_w")),    mode_B),

  # OM: reflective (mode_A)
  # poc, pon, NTU, SS are interchangeable manifestations of the total POM pool.
  # All loadings >= 0.708 (except poc=0.506, pon=0.547, retained: AVE=0.507,
  # rho_c=0.795 already meet thresholds; direct chemical representation of POM).
  reflective("OM",      multi_items("", c("poc", "pon", "envr_ntu", "envr_ss"))),

  # NUTRIENT: formative (mode_B)
  # envr_nh4 and envr_no2 removed: non-sig weight AND |loading| < 0.50
  # NOTE: REDOX-NUTRIENT HTMT = 1.059 — acknowledged limitation.
  # NOTE: TEMP-NUTRIENT HTMT 90% CI upper = 0.907 — marginal violation, structural
  #       artifact of the TEMP -> REDOX -> NUTRIENT chain; acknowledged limitation.
  composite("NUTRIENT", multi_items("", c("dtn", "envr_no3", "envr_po4")),   mode_B),

  # SA: sediment carbon accumulation rate — single-item construct.
  #     Now endogenous: predicted by OM (v6). Still predicts GHG directly.
  composite("SA",       single_item("c_sa")),

  # TEMP: water temperature — single-item construct with direct paths to GHG
  #       and to REDOX (v6). Temperature regulates water-column redox chemistry
  #       (DO, pH, DIC), justifying the upstream TEMP -> REDOX path. Also retains
  #       direct paths to GHG for the residual enzymatic temperature effect.
  composite("TEMP",     single_item("envr_temp"))
)

structure <- relationships(
  paths(from = "TEMP",     to = "REDOX"),
  paths(from = "REDOX",    to = c("OM", "NUTRIENT")),
  paths(from = "NUTRIENT", to = "OM"),
  paths(from = "OM",       to = "SA"),
  paths(from = c("REDOX", "OM", "NUTRIENT", "SA", "TEMP"),
        to = c("CO2", "CH4", "N2O"))
)

pls_model <- estimate_pls(data = df_sem,
                          measurement_model = measurements,
                          structural_model  = structure)
model_summary <- summary(pls_model)
cat("Iterations:", model_summary$iterations, "(should be < 300)\n")

set.seed(123)
boot_estimates <- bootstrap_model(pls_model, nboot = 10000,
                                  cores = parallel::detectCores())


## Assessing measurement model ---------------------------------------------

### A. Reflective construct (OM) — mode_A assessment ----------------------

boot_sum05 <- summary(boot_estimates, alpha = 0.05)
boot_sum10 <- summary(boot_estimates, alpha = 0.10)

cat("\n=== [OM — REFLECTIVE] Indicator loadings (>= 0.708) ===\n")
print(round(model_summary$loadings, 3))

cat("\n=== [OM — REFLECTIVE] Reliability and AVE ===\n")
cat("Thresholds: alpha >= 0.70, rho_c >= 0.70, AVE >= 0.50\n")
print(round(model_summary$reliability, 3))

cat("\n=== [OM — REFLECTIVE] Bootstrapped loadings ===\n")
print(round(boot_sum05$bootstrapped_loadings, 3))

cat("\n=== [OM — REFLECTIVE] HTMT discriminant validity (alpha = 0.10) ===\n")
cat("Threshold: HTMT < 0.85; 90% CI upper < 0.90\n")
htmt <- boot_sum10$bootstrapped_HTMT
print(round(htmt, 3))
cat("Note: REDOX-NUTRIENT HTMT > 1.0 and TEMP-NUTRIENT CI upper = 0.907 are known limitations (see spec comment).\n")

write.xlsx(as.data.frame(model_summary$reliability), file = "output/reliability.xlsx")
write.xlsx(as.data.frame(htmt),                      file = "output/htmt.xlsx")


### B. Formative constructs (REDOX, NUTRIENT) — mode_B assessment ----------

#### B1. Redundancy analysis (path coefficient >= 0.708) ------------------

m_redox <- constructs(
  composite("REDOX_global", single_item("envr_do_pc")),
  composite("REDOX_var",    multi_items("", c("envr_ph", "dic", "envr_do_w")), mode_B)
)
pls_redox <- estimate_pls(df_sem,
                          m_redox,
                          relationships(paths(from = "REDOX_var", to = "REDOX_global")))
cat("\n=== [REDOX] Redundancy analysis (proxy: envr_do_pc) ===\n")
print(round(summary(pls_redox)$paths, 3))

m_nut <- constructs(
  composite("NUTRIENT_global", single_item("envr_ec")),
  composite("NUTRIENT_var",    multi_items("", c("dtn", "envr_no3", "envr_po4")), mode_B)
)
pls_nut <- estimate_pls(df_sem,
                        m_nut,
                        relationships(paths(from = "NUTRIENT_var", to = "NUTRIENT_global")))
cat("\n=== [NUTRIENT] Redundancy analysis (proxy: envr_ec) ===\n")
print(round(summary(pls_nut)$paths, 3))


#### B2. VIF — formative indicator collinearity (< 3.0) -------------------

cat("\n=== [FORMATIVE] Indicator VIF (threshold < 3.0) ===\n")
print(model_summary$validity$vif_items)


#### B3. Bootstrapped weights and loadings --------------------------------

cat("\n=== [FORMATIVE] Bootstrapped weights (alpha = 0.05) ===\n")
cat("Retain if: weight p < 0.05 OR |loading| >= 0.50\n")
print(round(boot_sum05$bootstrapped_weights, 3))

cat("\n=== [FORMATIVE] Bootstrapped loadings ===\n")
print(round(boot_sum05$bootstrapped_loadings, 3))

write.xlsx(as.data.frame(boot_sum05$bootstrapped_weights),  file = "output/weights_boot.xlsx")
write.xlsx(as.data.frame(boot_sum05$bootstrapped_loadings), file = "output/loadings_boot.xlsx")


## Assessing structural model -----------------------------------------------

### Step 1: Antecedent collinearity (VIF < 5.0) ---------------------------

cat("\n=== VIF antecedents (threshold < 5.0) ===\n")
print(model_summary$vif_antecedents)


### Step 2: Path coefficients and significance ----------------------------

s.path    <- as.data.frame(boot_sum05$bootstrapped_paths)
total.eff <- as.data.frame(boot_sum05$bootstrapped_total_paths)

cat("\n=== Bootstrapped path coefficients ===\n")
print(round(s.path, 3))
cat("\n=== Total effects ===\n")
print(round(total.eff, 3))

write.xlsx(data.frame(RowNames = rownames(s.path),    s.path),    file = "output/s.path.xlsx")
write.xlsx(data.frame(RowNames = rownames(total.eff), total.eff), file = "output/total.effect.xlsx")


### Step 3: R² and f² ---------------------------------------------------

cat("\n=== R² ===\n")
print(round(model_summary$paths[c("R^2", "AdjR^2"), ], 3))

fsquare <- data.frame(RowNames = rownames(model_summary$fSquare),
                      model_summary$fSquare)
fsquare_long <- fsquare %>%
  pivot_longer(-RowNames, names_to = "Target", values_to = "f2") %>%
  filter(!is.na(f2), f2 > 0) %>%
  mutate(effect = case_when(
    f2 >= 0.35 ~ "large",
    f2 >= 0.15 ~ "medium",
    f2 >= 0.02 ~ "small",
    TRUE        ~ "negligible"
  ))
cat("\n=== f² effect sizes (0.02 small | 0.15 medium | 0.35 large) ===\n")
print(fsquare_long)
write.xlsx(fsquare, file = "output/fsquare.xlsx")


### Step 4: Indirect effects ---------------------------------------------

cat("\n=== Indirect effects (total - direct) ===\n")
total_est  <- boot_sum05$bootstrapped_total_paths[, "Original Est."]
direct_est <- boot_sum05$bootstrapped_paths[, "Original Est."]

indirect_paths <- c("REDOX  ->  CO2",    "REDOX  ->  CH4",    "REDOX  ->  N2O",
                    "NUTRIENT  ->  CO2", "NUTRIENT  ->  CH4", "NUTRIENT  ->  N2O",
                    "TEMP  ->  CO2",     "TEMP  ->  CH4",     "TEMP  ->  N2O",
                    "OM  ->  CO2",       "OM  ->  N2O")
for (p in indirect_paths) {
  cat(sprintf("%-35s  direct=%+.3f  total=%+.3f  indirect=%+.3f\n",
              p, direct_est[p], total_est[p], total_est[p] - direct_est[p]))
}


### Step 5: Predictive power (PLSpredict) --------------------------------

set.seed(123)
predict_pls_model <- predict_pls(model = pls_model, technique = predict_DA,
                                 noFolds = 10, reps = 10)
summary_predict   <- summary(predict_pls_model)

par(mfrow = c(1, 3))
plot(summary_predict, indicator = "ghg_co2")
plot(summary_predict, indicator = "ghg_ch4")
plot(summary_predict, indicator = "ghg_n2o")
par(mfrow = c(1, 1))

rmse <- rbind(
  data.frame(summary_predict$PLS_in_sample,     note = "PLS_in"),
  data.frame(summary_predict$PLS_out_of_sample, note = "PLS_out"),
  data.frame(summary_predict$LM_in_sample,      note = "LM_in"),
  data.frame(summary_predict$LM_out_of_sample,  note = "LM_out")
)
cat("\n=== RMSE (PLS_out < LM_out = predictive) ===\n")
print(rmse)
write.xlsx(data.frame(RowNames = rownames(rmse), rmse), file = "output/rmse.xlsx")


## MGA: prawn vs tilapia ---------------------------------------------------

cat("\n=== MGA: prawn vs tilapia (nboot = 10000) ===\n")
set.seed(123)
mga_result <- estimate_pls_mga(pls_model,
                               df_sem$species == "prawn",
                               nboot = 10000)
print(mga_result)

mga_df <- data.frame(
  Path    = rownames(as.data.frame(mga_result)),
  Prawn   = mga_result[, 1],
  Tilapia = mga_result[, 2],
  p       = mga_result[, 3]
)
write.xlsx(mga_df, file = "output/mga.xlsx")


## Plots -------------------------------------------------------------------

plot(boot_estimates, title = "PLS-SEM",
     theme = theme_sem, structure_only = T)
save_plot("output/SEM.png")

plot(boot_estimates, title = "PLS-SEM",
     theme = theme_sem, structure_only = F)
save_plot("output/SEM(Full).png")


# structural model comparison ----

# helper: run model + return R^2/AdjR^2 for all endogenous variables
run_pls_r2 <- function(structure_def, data = df_sem, seed = 123) {
  set.seed(seed)
  m <- estimate_pls(data = data,
                    measurement_model = measurements,
                    structural_model  = structure_def)
  s <- summary(m)
  r2_rows <- s$paths[c("R^2", "AdjR^2"), , drop = FALSE]
  endog_cols <- !is.na(r2_rows["R^2", ])
  r2_rows[, endog_cols, drop = FALSE]
}

# M1 (= v5 structure)
structure_M1 <- relationships(
  paths(from = "REDOX",    to = c("OM", "NUTRIENT")),
  paths(from = "NUTRIENT", to = "OM"),
  paths(from = c("REDOX", "OM", "NUTRIENT", "SA", "TEMP"),
        to = c("CO2", "CH4", "N2O"))
)

# M2:
structure_M2 <- relationships(
  paths(from = "REDOX",    to = c("OM", "NUTRIENT")),
  paths(from = "NUTRIENT", to = "OM"),
  paths(from = c("REDOX", "OM", "NUTRIENT", "SA", "TEMP"),
        to = c("CO2", "CH4", "N2O")),
  paths(from = "CO2",      to = c("CH4", "N2O"))
)

# M3:
structure_M3 <- relationships(
  paths(from = "REDOX",    to = c("OM", "NUTRIENT")),
  paths(from = "NUTRIENT", to = "OM"),
  paths(from = "OM",       to = "SA"),
  paths(from = c("REDOX", "OM", "NUTRIENT", "SA", "TEMP"),
        to = c("CO2", "CH4", "N2O"))
)

# M4:
structure_M4 <- relationships(
  paths(from = "REDOX",    to = c("OM", "NUTRIENT")),
  paths(from = "NUTRIENT", to = "OM"),
  paths(from = "OM",       to = "SA"),
  paths(from = c("REDOX", "OM", "NUTRIENT", "SA", "TEMP"),
        to = c("CO2", "CH4", "N2O")),
  paths(from = "CO2",      to = c("CH4", "N2O"))
)

# M5 (= v6 structure — current final model)
structure_M5 <- relationships(
  paths(from = "TEMP",     to = "REDOX"),
  paths(from = "REDOX",    to = c("OM", "NUTRIENT")),
  paths(from = "NUTRIENT", to = "OM"),
  paths(from = "OM",       to = "SA"),
  paths(from = c("REDOX", "OM", "NUTRIENT", "SA", "TEMP"),
        to = c("CO2", "CH4", "N2O"))
)

# Run all models
model_list <- list(
  M1 = structure_M1,
  M2 = structure_M2,
  M3 = structure_M3,
  M4 = structure_M4,
  M5 = structure_M5
)

r2_results <- lapply(model_list, run_pls_r2)

# All latent variables that can be endogenous across any model
all_endog <- c("REDOX", "OM", "NUTRIENT", "SA", "CO2", "CH4", "N2O")

# Compile comparison table — NA where a variable is exogenous in a given model
r2_table <- do.call(rbind, lapply(names(r2_results), function(nm) {
  r   <- r2_results[[nm]]
  row <- data.frame(Model = nm)
  for (v in all_endog) {
    row[[paste0(v, "_R2")]]    <- if (v %in% colnames(r)) r["R^2",    v] else NA_real_
    row[[paste0(v, "_AdjR2")]] <- if (v %in% colnames(r)) r["AdjR^2", v] else NA_real_
  }
  row
}))

print(r2_table)
writexl::write_xlsx(r2_table, "output/structural_model_comparison_r2.xlsx")
