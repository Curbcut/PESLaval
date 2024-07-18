#Loading up libraries
source("R/01_startup.R")
library(patchwork)
library(sf)
library(RMySQL)
library(classInt)
library(ggnewscale)
library(biscale)
library(cowplot)
library(gt)
library(scales)
library(qs)

#Setting up the cancensus api key
set_cancensus_api_key("CensusMapper_4308d496f011429cf814385050f083dc", install = TRUE)

#Setting up cache path for faster loading (edit for your own folder)
set_cancensus_cache_path("D:/McGill/can_cache", install = TRUE, overwrite = TRUE)

#Grabbing the Laval shapefiles
laval_csd <- cancensus::get_census(dataset = "CA21", 
                                   regions = list(CSD = 2465005), 
                                   level = "CSD", 
                                   geo_format = "sf")

laval_ct <- cancensus::get_census(dataset = "CA21", 
                                  regions = list(CSD = 2465005), 
                                  level = "CT", 
                                  geo_format = "sf")

laval_da <- cancensus::get_census(dataset = "CA21", 
                                  regions = list(CSD = 2465005), 
                                  level = "DA", 
                                  geo_format = "sf")

laval_db <- cancensus::get_census(dataset = "CA21", 
                                  regions = list(CSD = 2465005), 
                                  level = "DB", 
                                  geo_format = "sf")

#Grabbing the shapefile for the Montreal CMA
mtlcma_sf <- cancensus::get_census(dataset = "CA21", 
                                   regions = list(CSD = 24462), 
                                   level = "CMA", 
                                   geo_format = "sf")

#Setting the Laval bound box for maps
laval_bbox <- st_bbox(laval_ct)

#Setting up the curbcut scale
curbcut_scale <- c("#C4CDE1", "#98A8CB", "#6C83B5", "#4C5C7F", "#2B3448")
curbcut_na <- "#B3B3BB"

# Importing and Cleaning Data for DBs ---------------------------------------------
#Import 15 minute walking TTM and modifying it with necessary rows
#Check mobility_transport.R to get the file
ttm_walk_15 <- read.csv("D://McGill/can_cache/walk15.csv") |> 
  select(-X, -travel_seconds) |> 
  mutate(across(everything(), as.character)) |> 
  rename("GeoUID" = "from") |> 
  bind_rows(ttm_walk_15_rows) |> 
  arrange(GeoUID)

ttm_walk_15_rows <- laval_db |> 
  st_drop_geometry() |> 
  select(GeoUID) |> 
  mutate(to = GeoUID)

#Importing school data
school <- read_sf("D://McGill/can_cache/etablissements-meq-mes-esrishp/PPS_Public_Ecole.shp") |> 
  filter(ADULTE != 1, FORM_PRO != 1, ORDRE_ENS != "Préscolaire") |> 
  st_intersection(laval_csd) |> 
  distinct(ADRS_ORG_1, .keep_all = TRUE) |> 
  select(PRIM, SEC, TYPE_CS)

#Filtering for school locations for mapping
eng_prim_points <- school |> 
  filter(PRIM == 1, TYPE_CS == "Anglo") |> 
  select(geometry) |> 
  mutate(type = "Anglophone")

fr_prim_points <- school |> 
  filter(PRIM == 1, TYPE_CS == "Franco") |> 
  select(geometry) |> 
  mutate(type = "Francophone")

primary_points <- bind_rows(
  eng_prim_points %>% st_as_sf(),
  fr_prim_points %>% st_as_sf()
)

eng_sec_points <- school |> 
  filter(SEC == 1, TYPE_CS == "Anglo") |> 
  select(geometry) |> 
  mutate(type = "Anglophone")

fr_sec_points <- school |> 
  filter(SEC == 1, TYPE_CS == "Franco") |> 
  select(geometry) |> 
  mutate(type = "Francophone")

secondary_points <- bind_rows(
  eng_sec_points %>% st_as_sf(),
  fr_sec_points %>% st_as_sf()
)

#Filtering for school types
prim <- school |> 
  filter(PRIM == 1) |> 
  select() |> 
  mutate("primary" = 1) |> 
  st_join(laval_db) |> 
  st_drop_geometry() |> 
  group_by(GeoUID) |> 
  summarize("primary" = sum(primary, na.rm = TRUE))

eng_prim <- school |> 
  filter(PRIM == 1, TYPE_CS == "Anglo") |> 
  select() |> 
  mutate("eng_primary" = 1) |> 
  st_join(laval_db) |> 
  st_drop_geometry() |> 
  group_by(GeoUID) |> 
  summarize("eng_primary" = sum(eng_primary, na.rm = TRUE))

fr_prim <- school |> 
  filter(PRIM == 1, TYPE_CS == "Franco") |> 
  select() |> 
  mutate("fr_primary" = 1) |> 
  st_join(laval_db) |> 
  st_drop_geometry() |> 
  group_by(GeoUID) |> 
  summarize("fr_primary" = sum(fr_primary, na.rm = TRUE))

sec <- school |> 
  filter(SEC == 1) |> 
  select() |> 
  mutate("secondary" = 1) |> 
  st_join(laval_db) |> 
  st_drop_geometry() |> 
  group_by(GeoUID) |> 
  summarize("secondary" = sum(secondary, na.rm = TRUE))

eng_sec <- school |> 
  filter(SEC == 1, TYPE_CS == "Anglo") |> 
  select() |> 
  mutate("eng_secondary" = 1) |> 
  st_join(laval_db) |> 
  st_drop_geometry() |> 
  group_by(GeoUID) |> 
  summarize("eng_secondary" = sum(eng_secondary, na.rm = TRUE))

fr_sec <- school |> 
  filter(SEC == 1, TYPE_CS == "Franco") |> 
  select() |> 
  mutate("fr_secondary" = 1) |> 
  st_join(laval_db) |> 
  st_drop_geometry() |> 
  group_by(GeoUID) |> 
  summarize("fr_secondary" = sum(fr_secondary, na.rm = TRUE))


school_ttm <- ttm_walk_15 |>
  left_join(prim, join_by("to" == "GeoUID")) |> 
  left_join(eng_prim, join_by("to" == "GeoUID")) |> 
  left_join(fr_prim, join_by("to" == "GeoUID")) |> 
  left_join(sec, join_by("to" == "GeoUID")) |> 
  left_join(eng_sec, join_by("to" == "GeoUID")) |> 
  left_join(fr_sec, join_by("to" == "GeoUID")) |> 
  select(-to) |> 
  group_by(GeoUID) |> 
  summarize(across(everything(), ~ sum(., na.rm = TRUE)))

school_sf <- laval_db |> 
  left_join(school_ttm, by = "GeoUID") |> 
  mutate_all(~ replace_na(., 0)) |> 
  mutate(across(c("primary", "eng_primary", "fr_primary",
                  "secondary", "eng_secondary", "fr_secondary"),
                ~ case_when(. == 0 ~ "0", . == 1 ~ "1", . >= 2 ~ "2+", TRUE ~ as.character(.)))) |> 
  mutate(access_pri = case_when(
    eng_primary != 0 & fr_primary != 0 ~ "pri_both",
    eng_primary != 0 & fr_primary == 0 ~ "pri_en",
    eng_primary == 0 & fr_primary != 0 ~ "pri_fr",
    eng_primary == 0 & fr_primary == 0 ~ "pri_none"
  )) |> 
  mutate(access_sec = case_when(
    eng_secondary != 0 & fr_secondary != 0 ~ "sec_both",
    eng_secondary != 0 & fr_secondary == 0 ~ "sec_en",
    eng_secondary == 0 & fr_secondary != 0 ~ "sec_fr",
    eng_secondary == 0 & fr_secondary == 0 ~ "sec_none"
  ))


# Data for CT level -------------------------------------------------------
#Prepping extra rows for TTM 15 for CTs
ttm_walk_15_ct_rows <- laval_ct |> 
  st_drop_geometry() |> 
  select(GeoUID) |> 
  mutate(to = GeoUID)

#15 minute TTM for CTs
ttm_walk_15_ct <- qread("D://McGill/can_cache/CT_laval_foot_ttm.qs") |> 
  enframe(name = "GeoUID", value = "to") |> 
  unnest(to) |> 
  rowwise() %>%
  unite(time, 3:83, na.rm = TRUE, sep = ", ") |> 
  mutate(time = as.numeric(time)) |> 
  select(GeoUID, DA_ID, time) |> 
  rename("to" = DA_ID) |> 
  filter(time <= 900) |> 
  select(-time) |> 
  bind_rows(ttm_walk_15_ct_rows) |> 
  arrange(GeoUID)

#Importing school data
school <- read_sf("D://McGill/can_cache/etablissements-meq-mes-esrishp/PPS_Public_Ecole.shp") |> 
  filter(ADULTE != 1, FORM_PRO != 1, ORDRE_ENS != "Préscolaire") |> 
  st_intersection(laval_csd) |> 
  distinct(ADRS_ORG_1, .keep_all = TRUE) |> 
  select(PRIM, SEC, TYPE_CS)

#Filtering for school locations for mapping
eng_prim_points <- school |> 
  filter(PRIM == 1, TYPE_CS == "Anglo") |> 
  select(geometry) |> 
  mutate(type = "Anglophone")

fr_prim_points <- school |> 
  filter(PRIM == 1, TYPE_CS == "Franco") |> 
  select(geometry) |> 
  mutate(type = "Francophone")

primary_points <- bind_rows(
  eng_prim_points %>% st_as_sf(),
  fr_prim_points %>% st_as_sf()
)

eng_sec_points <- school |> 
  filter(SEC == 1, TYPE_CS == "Anglo") |> 
  select(geometry) |> 
  mutate(type = "Anglophone")

fr_sec_points <- school |> 
  filter(SEC == 1, TYPE_CS == "Franco") |> 
  select(geometry) |> 
  mutate(type = "Francophone")

secondary_points <- bind_rows(
  eng_sec_points %>% st_as_sf(),
  fr_sec_points %>% st_as_sf()
)

#Filtering for school types
prim <- school |> 
  filter(PRIM == 1) |> 
  select() |> 
  mutate("primary" = 1) |> 
  st_join(laval_ct) |> 
  st_drop_geometry() |> 
  group_by(GeoUID) |> 
  summarize("primary" = sum(primary, na.rm = TRUE))

eng_prim <- school |> 
  filter(PRIM == 1, TYPE_CS == "Anglo") |> 
  select() |> 
  mutate("eng_primary" = 1) |> 
  st_join(laval_ct) |> 
  st_drop_geometry() |> 
  group_by(GeoUID) |> 
  summarize("eng_primary" = sum(eng_primary, na.rm = TRUE))

fr_prim <- school |> 
  filter(PRIM == 1, TYPE_CS == "Franco") |> 
  select() |> 
  mutate("fr_primary" = 1) |> 
  st_join(laval_ct) |> 
  st_drop_geometry() |> 
  group_by(GeoUID) |> 
  summarize("fr_primary" = sum(fr_primary, na.rm = TRUE))

sec <- school |> 
  filter(SEC == 1) |> 
  select() |> 
  mutate("secondary" = 1) |> 
  st_join(laval_ct) |> 
  st_drop_geometry() |> 
  group_by(GeoUID) |> 
  summarize("secondary" = sum(secondary, na.rm = TRUE))

eng_sec <- school |> 
  filter(SEC == 1, TYPE_CS == "Anglo") |> 
  select() |> 
  mutate("eng_secondary" = 1) |> 
  st_join(laval_ct) |> 
  st_drop_geometry() |> 
  group_by(GeoUID) |> 
  summarize("eng_secondary" = sum(eng_secondary, na.rm = TRUE))

fr_sec <- school |> 
  filter(SEC == 1, TYPE_CS == "Franco") |> 
  select() |> 
  mutate("fr_secondary" = 1) |> 
  st_join(laval_ct) |> 
  st_drop_geometry() |> 
  group_by(GeoUID) |> 
  summarize("fr_secondary" = sum(fr_secondary, na.rm = TRUE))


school_ttm <- ttm_walk_15_ct |>
  left_join(prim, join_by("to" == "GeoUID")) |> 
  left_join(eng_prim, join_by("to" == "GeoUID")) |> 
  left_join(fr_prim, join_by("to" == "GeoUID")) |> 
  left_join(sec, join_by("to" == "GeoUID")) |> 
  left_join(eng_sec, join_by("to" == "GeoUID")) |> 
  left_join(fr_sec, join_by("to" == "GeoUID")) |> 
  select(-to) |> 
  group_by(GeoUID) |> 
  summarize(across(everything(), ~ sum(., na.rm = TRUE)))

school_sf <- laval_ct |> 
  left_join(school_ttm, by = "GeoUID") |> 
  mutate_all(~ replace_na(., 0)) |> 
  #mutate(across(c("primary", "eng_primary", "fr_primary",
                  #"secondary", "eng_secondary", "fr_secondary"),
               # ~ case_when(. == 0 ~ "0", . == 1 ~ "1", . >= 2 ~ "2+", TRUE ~ as.character(.)))) |> 
  mutate(access_pri = case_when(
    eng_primary != 0 & fr_primary != 0 ~ "pri_both",
    eng_primary != 0 & fr_primary == 0 ~ "pri_en",
    eng_primary == 0 & fr_primary != 0 ~ "pri_fr",
    eng_primary == 0 & fr_primary == 0 ~ "pri_none"
  )) |> 
  mutate(access_sec = case_when(
    eng_secondary != 0 & fr_secondary != 0 ~ "sec_both",
    eng_secondary != 0 & fr_secondary == 0 ~ "sec_en",
    eng_secondary == 0 & fr_secondary != 0 ~ "sec_fr",
    eng_secondary == 0 & fr_secondary == 0 ~ "sec_none"
  ))


# Comparison Data ---------------------------------------------------------
table5 <- read.csv("D://McGill/can_cache/table5.csv") |> 
  rename("notimm_low_child" = "With.children",
         "imm_low_child" = "With.children.1",
         "notimm_notlow_child" = "With.children.2",
         "imm_notlow_child" = "With.children.3",
         "GeoUID" = "X") |> 
  slice(-1:-5)

table6 <- read.csv("D://McGill/can_cache/table6.csv") |> 
  rename("notimm_low_child" = "With.children",
         "imm_low_child" = "With.children.1",
         "notimm_notlow_child" = "With.children.2",
         "imm_notlow_child" = "With.children.3",
         "GeoUID" = "X") |> 
  slice(-1:-2) |> 
  mutate(GeoUID = substr(GeoUID, 1, 10))

table7 <- read.csv("D://McGill/can_cache/table7.csv") |> 
  rename("notimm_low_child" = "With.children",
         "imm_low_child" = "With.children.1",
         "notimm_notlow_child" = "With.children.2",
         "imm_notlow_child" = "With.children.3",
         "GeoUID" = "X") |> 
  slice(-1:-2) |> 
  mutate(GeoUID = substr(GeoUID, 1, 10))

table8a <- read.csv("D://McGill/can_cache/table8a.csv") |> 
  rename("notimm_low_child" = "With.children",
         "imm_low_child" = "With.children.1",
         "notimm_notlow_child" = "With.children.2",
         "imm_notlow_child" = "With.children.3",
         "GeoUID" = "X") |> 
  slice(-1:-2) |> 
  mutate(GeoUID = substr(GeoUID, 1, 10))

table8b <- read.csv("D://McGill/can_cache/table8b.csv") |> 
  rename("notimm_low_child" = "With.children",
         "imm_low_child" = "With.children.1",
         "notimm_notlow_child" = "With.children.2",
         "imm_notlow_child" = "With.children.3",
         "GeoUID" = "X") |> 
  slice(-1:-2) |> 
  slice(1:7) |> 
  mutate(GeoUID = substr(GeoUID, 1, 10))

lowincome_imm <- bind_rows(table5, table6, table7, table8a, table8b) |> 
  mutate_all(~ na_if(., "x"))

lowincome_imm_sf <- laval_db|> 
  left_join(lowincome_imm, by = join_by("CT_UID" == "GeoUID")) |> 
  select(notimm_low_child, imm_low_child, notimm_notlow_child, imm_notlow_child) |> 
  st_join(laval_db)

lvl_demo <- get_census(dataset = "CA21", 
                       regions = list(CSD = 2465005), 
                       level = "DA",
                       vectors = c("age5_9" = "v_CA21_32", "age10" = "v_CA21_53",
                                   "age11" = "v_CA21_56", "age12" = "v_CA21_59",
                                   "age13" = "v_CA21_62", "age14" = "v_CA21_65",
                                   "age15" = "v_CA21_74", "age16" = "v_CA21_77"))
# Maps --------------------------------------------------------------------
primary_map <- ggplot() +
  geom_sf(data = mtlcma_sf, color = "black", fill = "#FCFCFC") +
  geom_sf(data = laval_csd, color = "black", fill = NA) +
  geom_sf(data = school_sf, aes(fill = primary), color = "#f7f7f7", size = 0.1) +
  geom_sf(data = primary_points) +
  scale_fill_manual(values = c("0" = "#C4CDE1", 
                               "1" = "#6C83B5",
                               "2+" = "#2B3448"),
                    na.value = curbcut_na) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        legend.position = "none", axis.title = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank(),
        panel.background = element_rect(fill = "#525252")) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
  labs(title = "École primaire") +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

eng_primary_map <- ggplot() +
  geom_sf(data = mtlcma_sf, color = "black", fill = "#FCFCFC") +
  geom_sf(data = laval_csd, color = "black", fill = NA) +
  geom_sf(data = school_sf, aes(fill = eng_primary), color = "#f7f7f7", size = 0.1) +
  geom_sf(data = eng_prim_points, aes(color = "Eng Prim Points")) +
  scale_fill_manual(values = c("0" = "#C4CDE1", 
                               "1" = "#6C83B5",
                               "2+" = "#2B3448"),
                    na.value = curbcut_na,
                    name = "Écoles accessibles") +
  scale_color_manual(name = "École primaire", values = "black") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        legend.position = "bottom", legend.justification = "center",
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.background = element_rect(fill = "#525252")) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5),
         color = guide_legend(title.position = "top", title.hjust = 0.5))+
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

fr_primary_map <- ggplot() +
  geom_sf(data = mtlcma_sf, color = "black", fill = "#FCFCFC") +
  geom_sf(data = laval_csd, color = "black", fill = NA) +
  geom_sf(data = school_sf, aes(fill = fr_primary), color = "#f7f7f7", size = 0.1) +
  geom_sf(data = fr_prim_points) +
  scale_fill_manual(values = c("0" = "#C4CDE1", 
                               "1" = "#6C83B5",
                               "2+" = "#2B3448"),
                    na.value = curbcut_na,
                    name = "Nombre de points de vente accessibles") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        legend.position = "none",
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.background = element_rect(fill = "#525252")) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))+
  labs(title = "École primaire francophone") +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

combined_primary <- fr_primary_map + eng_primary_map + primary_map +
  plot_annotation(title = "Écoles primaires accessibles à Laval",
                  theme = theme(plot.title = element_text(hjust = 0.5)))

combined_primary

#Max map
ggplot() +
  geom_sf(data = mtlcma_sf, color = "black", fill = "#FCFCFC") +
  geom_sf(data = laval_csd, color = "black", fill = NA) +
  geom_sf(data = school_sf, aes(fill = primary), color = NA) +
  geom_sf(data = primary_points, aes(color = type)) +
  scale_fill_manual(values = c("0" = "#C4CDE1", 
                               "1" = "#6C83B5",
                               "2+" = "#2B3448"),
                    na.value = curbcut_na,
                    name = "Écoles accessibles") +
  scale_color_manual(values = c("Anglophone" = "indianred2", 
                               "Francophone" = "royalblue2"),
                    na.value = curbcut_na,
                    name = "Langue scolaire") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        legend.position = "bottom",
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.background = element_rect(fill = "#525252")) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5),
         color = guide_legend(title.position = "top", title.hjust = 0.5))+
  labs(title = "Écoles primaires accessibles à Laval") +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))


#Kevin Map
ggplot() +
  gg_cc_tiles +
  geom_sf(data = school_sf, aes(fill = access_pri), color = NA) +
  geom_sf(data = primary_points, aes(color = type), size = 0.9, alpha = 0.8) +
  geom_sf(data = laval_sectors, fill = "transparent", color = "black") +
  scale_fill_manual(values = c("pri_none" = "#E8E8E8", 
                               "pri_fr" = "#6C83B5",
                               "pri_en" = "#73AE80",
                               "pri_both" = "#2A5A5B"),
                    labels = c("pri_none" = "Aucun accès",
                               "pri_fr" = "Accès à au moins une école francophone",
                               "pri_en" = "Accès à au moins une école anglophone",
                               "pri_both" = "Accès à au moins une école des deux langues"),
                    na.value = curbcut_na) +
  scale_color_manual(values = c("Anglophone" = "#F5D574", 
                                "Francophone" = "#CD718C"),
                     na.value = curbcut_na) +
  theme_void() +
  theme(plot.title = element_blank(), axis.text = element_blank(),
        legend.position = "bottom", legend.title = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  guides(fill = guide_legend(ncol = 2),
         color = guide_legend(ncol = 1, override.aes = list(size = 3)))

ggplot() +
  gg_cc_tiles +
  geom_sf(data = school_sf, aes(fill = access_sec), color = NA) +
  geom_sf(data = secondary_points, aes(color = type), size = 0.9, alpha = 0.8) +
  geom_sf(data = laval_sectors, fill = "transparent", color = "black") +
  scale_fill_manual(values = c("sec_none" = "#E8E8E8", 
                               "sec_fr" = "#6C83B5",
                               "sec_en" = "#73AE80",
                               "sec_both" = "#2A5A5B"),
                    labels = c("sec_none" = "Aucun accès",
                               "sec_fr" = "Accès à au moins une école francophone",
                               "sec_en" = "Accès à au moins une école anglophone",
                               "sec_both" = "Accès à au moins une école des deux langues"),
                    na.value = curbcut_na) +
  scale_color_manual(values = c("Anglophone" = "#F5D574", 
                                "Francophone" = "#CD718C"),
                     na.value = curbcut_na) +
  theme_void() +
  theme(plot.title = element_blank(), axis.text = element_blank(),
        legend.position = "bottom", legend.title = element_blank(),
        axis.title = element_blank(), panel.grid = element_blank()) +
  guides(fill = guide_legend(ncol = 2),
         color = guide_legend(ncol = 1, override.aes = list(size = 3)))

noprimary <- school_sf |> 
  filter(eng_secondary >= 1)

# Bivariate CT Maps ---------------------------------------------------------
#Cross tabulation data for immigrants and low income (only run if school_sf is at CT level)
low_imm_ct <- school_sf |> 
  select(GeoUID, Population) |> 
  left_join(lowincome_imm, by = "GeoUID") |> 
  left_join(st_drop_geometry(school_sf), by = "GeoUID") |> 
  select(notimm_low_child, imm_low_child, notimm_notlow_child, imm_notlow_child,
         fr_primary, fr_secondary) |> 
  mutate(
    notimm_low_child = as.numeric(notimm_low_child), imm_low_child = as.numeric(imm_low_child),
    notimm_notlow_child = as.numeric(notimm_notlow_child), imm_notlow_child = as.numeric(imm_notlow_child),
    notimm_low_child = replace_na(notimm_low_child, 0), imm_low_child = replace_na(imm_low_child, 0),
    notimm_notlow_child = replace_na(notimm_notlow_child, 0), imm_notlow_child = replace_na(imm_notlow_child, 0)
  ) |> 
  mutate(total = notimm_low_child + imm_low_child +
                 notimm_notlow_child + imm_notlow_child) |> 
  mutate(notimm_low_child_prop = notimm_low_child / total * 100,
         imm_low_child_prop = imm_low_child / total * 100,
         notimm_notlow_child_prop = notimm_notlow_child / total * 100,
         imm_notlow_child_prop = imm_notlow_child / total * 100)
  
#Make sure to comment the mutate that creates 2+ in school_sf
ct_breaks <- low_imm_ct |> 
  mutate(fr_primary = as.integer(fr_primary),
         fr_secondary = as.integer(fr_secondary))

#Non-Immigrant low income and primary schools
pri_notimm_low_child_breaks <- bi_class_breaks(ct_breaks, x ="fr_primary", y = "notimm_low_child_prop",
                             style = "fisher", dim = 3, split = TRUE)

pri_notimm_low_child_legend <- bi_legend(pal = "DkCyan2", dim = 3, xlab = "Accessibilité à l'école", ylab = "% Ménages",
          flip_axes = TRUE, size = 10, breaks = pri_notimm_low_child_breaks)

ct_bi <- bi_class(ct_breaks, x = "fr_primary", y = "notimm_low_child_prop",
                          style = "fisher", dim = 3) |> 
  mutate(pri_notimm_low_child = bi_class) |> 
  select(-bi_class)

pri_notimm_low_child_map <- ggplot() +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#525252", color = NA) +
  geom_sf(data = mtlcma_sf, fill = "#FCFCFC", color = NA) +
  geom_sf(data = ct_bi, mapping = aes_string(fill = "pri_notimm_low_child"), color = NA,
          size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan2", dim = 3, flip_axes = TRUE) +
  bi_theme() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank(),
        panel.background = element_rect(fill = "#525252", color = NA)) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

ggdraw() +
  draw_plot(pri_notimm_low_child_map, 0, 0, 1, 1) +
  draw_plot(pri_notimm_low_child_legend, 0.665, 0.041, 0.32, 0.32)

#Primary school and low income immigrant households
pri_imm_low_child_breaks <- bi_class_breaks(ct_breaks, x ="fr_primary", y = "imm_low_child_prop",
                                               style = "fisher", dim = 3, split = TRUE)

pri_imm_low_child_legend <- bi_legend(pal = "DkCyan2", dim = 3, xlab = "Accessibilité à l'école", ylab = "% Ménages",
                                         flip_axes = TRUE, size = 10, breaks = pri_imm_low_child_breaks)

ct_bi <- bi_class(ct_breaks, x = "fr_primary", y = "imm_low_child_prop",
                  style = "fisher", dim = 3) |> 
  mutate(pri_imm_low_child = bi_class) |> 
  select(-bi_class)

pri_imm_low_child_map <- ggplot() +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#525252", color = NA) +
  geom_sf(data = mtlcma_sf, fill = "#FCFCFC", color = NA) +
  geom_sf(data = ct_bi, mapping = aes_string(fill = "pri_imm_low_child"), color = NA,
          size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan2", dim = 3, flip_axes = TRUE) +
  bi_theme() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank(),
        panel.background = element_rect(fill = "#525252", color = NA)) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

ggdraw() +
  draw_plot(pri_imm_low_child_map, 0, 0, 1, 1) +
  draw_plot(pri_imm_low_child_legend, 0.675, 0.041, 0.32, 0.32)

#Primary school and immigrant households
pri_imm_notlow_child_breaks <- bi_class_breaks(ct_breaks, x ="fr_primary", y = "imm_notlow_child_prop",
                                            style = "fisher", dim = 3, split = TRUE)

pri_imm_notlow_child_legend <- bi_legend(pal = "DkCyan2", dim = 3, xlab = "Accessibilité à l'école", ylab = "% Ménages",
                                      flip_axes = TRUE, size = 10, breaks = pri_imm_notlow_child_breaks)

ct_bi <- bi_class(ct_breaks, x = "fr_primary", y = "imm_notlow_child_prop",
                  style = "fisher", dim = 3) |> 
  mutate(pri_imm_notlow_child = bi_class) |> 
  select(-bi_class)

pri_imm_notlow_child_map <- ggplot() +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#525252", color = NA) +
  geom_sf(data = mtlcma_sf, fill = "#FCFCFC", color = NA) +
  geom_sf(data = ct_bi, mapping = aes_string(fill = "pri_imm_notlow_child"), color = NA,
          size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan2", dim = 3, flip_axes = TRUE) +
  bi_theme() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank(),
        panel.background = element_rect(fill = "#525252", color = NA)) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

ggdraw() +
  draw_plot(pri_imm_notlow_child_map, 0, 0, 1, 1) +
  draw_plot(pri_imm_notlow_child_legend, 0.675, 0.041, 0.32, 0.32)

#Primary school and non-immigrant households
pri_notimm_notlow_child_breaks <- bi_class_breaks(ct_breaks, x ="fr_primary", y = "notimm_notlow_child_prop",
                                               style = "fisher", dim = 3, split = TRUE)

pri_notimm_notlow_child_legend <- bi_legend(pal = "DkCyan2", dim = 3, xlab = "Accessibilité à l'école", ylab = "% Ménages",
                                         flip_axes = TRUE, size = 10, breaks = pri_notimm_notlow_child_breaks)

ct_bi <- bi_class(ct_breaks, x = "fr_primary", y = "notimm_notlow_child_prop",
                  style = "fisher", dim = 3) |> 
  mutate(pri_notimm_notlow_child = bi_class) |> 
  select(-bi_class)

pri_notimm_notlow_child_map <- ggplot() +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#525252", color = NA) +
  geom_sf(data = mtlcma_sf, fill = "#FCFCFC", color = NA) +
  geom_sf(data = ct_bi, mapping = aes_string(fill = "pri_notimm_notlow_child"), color = NA,
          size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan2", dim = 3, flip_axes = TRUE) +
  bi_theme() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank(),
        panel.background = element_rect(fill = "#525252", color = NA)) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

ggdraw() +
  draw_plot(pri_notimm_notlow_child_map, 0, 0, 1, 1) +
  draw_plot(pri_notimm_notlow_child_legend, 0.675, 0.041, 0.32, 0.32)

#Non-Immigrant low income and secondary schools
sec_notimm_low_child_breaks <- bi_class_breaks(ct_breaks, x ="fr_secondary", y = "notimm_low_child_prop",
                                               style = "fisher", dim = 3, split = TRUE)

sec_notimm_low_child_legend <- bi_legend(pal = "DkCyan2", dim = 3, xlab = "Accessibilité à l'école", ylab = "% Ménages",
                                         flip_axes = TRUE, size = 10, breaks = sec_notimm_low_child_breaks)

ct_bi <- bi_class(ct_breaks, x = "fr_secondary", y = "notimm_low_child_prop",
                  style = "fisher", dim = 3) |> 
  mutate(sec_notimm_low_child = bi_class) |> 
  select(-bi_class)

sec_notimm_low_child_map <- ggplot() +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#525252", color = NA) +
  geom_sf(data = mtlcma_sf, fill = "#FCFCFC", color = NA) +
  geom_sf(data = ct_bi, mapping = aes_string(fill = "sec_notimm_low_child"), color = NA,
          size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan2", dim = 3, flip_axes = TRUE) +
  bi_theme() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank(),
        panel.background = element_rect(fill = "#525252", color = NA)) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

ggdraw() +
  draw_plot(sec_notimm_low_child_map, 0, 0, 1, 1) +
  draw_plot(sec_notimm_low_child_legend, 0.665, 0.041, 0.32, 0.32)

#secondary school and low income immigrant households
sec_imm_low_child_breaks <- bi_class_breaks(ct_breaks, x ="fr_secondary", y = "imm_low_child_prop",
                                            style = "fisher", dim = 3, split = TRUE)

sec_imm_low_child_legend <- bi_legend(pal = "DkCyan2", dim = 3, xlab = "Accessibilité à l'école", ylab = "% Ménages",
                                      flip_axes = TRUE, size = 10, breaks = sec_imm_low_child_breaks)

ct_bi <- bi_class(ct_breaks, x = "fr_secondary", y = "imm_low_child_prop",
                  style = "fisher", dim = 3) |> 
  mutate(sec_imm_low_child = bi_class) |> 
  select(-bi_class)

sec_imm_low_child_map <- ggplot() +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#525252", color = NA) +
  geom_sf(data = mtlcma_sf, fill = "#FCFCFC", color = NA) +
  geom_sf(data = ct_bi, mapping = aes_string(fill = "sec_imm_low_child"), color = NA,
          size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan2", dim = 3, flip_axes = TRUE) +
  bi_theme() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank(),
        panel.background = element_rect(fill = "#525252", color = NA)) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

ggdraw() +
  draw_plot(sec_imm_low_child_map, 0, 0, 1, 1) +
  draw_plot(sec_imm_low_child_legend, 0.675, 0.041, 0.32, 0.32)

#secondary school and immigrant households
sec_imm_notlow_child_breaks <- bi_class_breaks(ct_breaks, x ="fr_secondary", y = "imm_notlow_child_prop",
                                               style = "fisher", dim = 3, split = TRUE)

sec_imm_notlow_child_legend <- bi_legend(pal = "DkCyan2", dim = 3, xlab = "Accessibilité à l'école", ylab = "% Ménages",
                                         flip_axes = TRUE, size = 10, breaks = sec_imm_notlow_child_breaks)

ct_bi <- bi_class(ct_breaks, x = "fr_secondary", y = "imm_notlow_child_prop",
                  style = "fisher", dim = 3) |> 
  mutate(sec_imm_notlow_child = bi_class) |> 
  select(-bi_class)

sec_imm_notlow_child_map <- ggplot() +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#525252", color = NA) +
  geom_sf(data = mtlcma_sf, fill = "#FCFCFC", color = NA) +
  geom_sf(data = ct_bi, mapping = aes_string(fill = "sec_imm_notlow_child"), color = NA,
          size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan2", dim = 3, flip_axes = TRUE) +
  bi_theme() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank(),
        panel.background = element_rect(fill = "#525252", color = NA)) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

ggdraw() +
  draw_plot(sec_imm_notlow_child_map, 0, 0, 1, 1) +
  draw_plot(sec_imm_notlow_child_legend, 0.675, 0.041, 0.32, 0.32)

#secondary school and non-immigrant households
sec_notimm_notlow_child_breaks <- bi_class_breaks(ct_breaks, x ="fr_secondary", y = "notimm_notlow_child_prop",
                                                  style = "fisher", dim = 3, split = TRUE)

sec_notimm_notlow_child_legend <- bi_legend(pal = "DkCyan2", dim = 3, xlab = "Accessibilité à l'école", ylab = "% Ménages",
                                            flip_axes = TRUE, size = 10, breaks = sec_notimm_notlow_child_breaks)

ct_bi <- bi_class(ct_breaks, x = "fr_secondary", y = "notimm_notlow_child_prop",
                  style = "fisher", dim = 3) |> 
  mutate(sec_notimm_notlow_child = bi_class) |> 
  select(-bi_class)

sec_notimm_notlow_child_map <- ggplot() +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#525252", color = NA) +
  geom_sf(data = mtlcma_sf, fill = "#FCFCFC", color = NA) +
  geom_sf(data = ct_bi, mapping = aes_string(fill = "sec_notimm_notlow_child"), color = NA,
          size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan2", dim = 3, flip_axes = TRUE) +
  bi_theme() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank(),
        panel.background = element_rect(fill = "#525252", color = NA)) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

ggdraw() +
  draw_plot(sec_notimm_notlow_child_map, 0, 0, 1, 1) +
  draw_plot(sec_notimm_notlow_child_legend, 0.675, 0.041, 0.32, 0.32)


# Tables ------------------------------------------------------------------
#Cross-tabulation table, ONLY use if CT data was used in school_sf
ct_table_primary <- low_imm_ct |>
  st_drop_geometry() |> 
  summarise(across(
      c(notimm_low_child, imm_low_child, notimm_notlow_child, imm_notlow_child),
      list(
        sum_filtered = ~ sum(.x[fr_primary >= 1], na.rm = TRUE),
        total_sum = ~ sum(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}")
  ) |> 
  mutate(
    ratio_notimm_low_child = round(notimm_low_child_sum_filtered / notimm_low_child_total_sum * 100, 1),
    ratio_imm_low_child = round(imm_low_child_sum_filtered / imm_low_child_total_sum * 100, 1),
    ratio_notimm_notlow_child = round(notimm_notlow_child_sum_filtered / notimm_notlow_child_total_sum * 100, 1),
    ratio_imm_notlow_child = round(imm_notlow_child_sum_filtered / imm_notlow_child_total_sum * 100, 1)) |> 
  mutate(school = "Primary",
         families = round(notimm_low_child_sum_filtered + imm_low_child_sum_filtered +
                            notimm_notlow_child_sum_filtered + imm_notlow_child_sum_filtered)) |> 
  mutate(families_ratio = round(families / (notimm_low_child_total_sum + imm_low_child_total_sum +
                                              notimm_notlow_child_total_sum + imm_notlow_child_total_sum) * 100, 1)) |> 
  select(school, notimm_low_child_sum_filtered, ratio_notimm_low_child, 
         imm_low_child_sum_filtered, ratio_imm_low_child,
         notimm_notlow_child_sum_filtered, ratio_notimm_notlow_child,
         imm_notlow_child_sum_filtered, ratio_imm_notlow_child, families, families_ratio)

ct_table_secondary <- low_imm_ct |>
  st_drop_geometry() |> 
  summarise(across(
    c(notimm_low_child, imm_low_child, notimm_notlow_child, imm_notlow_child),
    list(
      sum_filtered = ~ sum(.x[fr_secondary >= 1], na.rm = TRUE),
      total_sum = ~ sum(.x, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}")
  ) |> 
  mutate(
    ratio_notimm_low_child = round(notimm_low_child_sum_filtered / notimm_low_child_total_sum * 100, 1),
    ratio_imm_low_child = round(imm_low_child_sum_filtered / imm_low_child_total_sum * 100, 1),
    ratio_notimm_notlow_child = round(notimm_notlow_child_sum_filtered / notimm_notlow_child_total_sum * 100, 1),
    ratio_imm_notlow_child = round(imm_notlow_child_sum_filtered / imm_notlow_child_total_sum * 100, 1)) |> 
  mutate(school = "Secondary",
         families = round(notimm_low_child_sum_filtered + imm_low_child_sum_filtered +
           notimm_notlow_child_sum_filtered + imm_notlow_child_sum_filtered)) |> 
  mutate(families_ratio = round(families / (notimm_low_child_total_sum + imm_low_child_total_sum +
                                              notimm_notlow_child_total_sum + imm_notlow_child_total_sum) * 100, 1)) |>
  select(school, notimm_low_child_sum_filtered, ratio_notimm_low_child, 
         imm_low_child_sum_filtered, ratio_imm_low_child,
         notimm_notlow_child_sum_filtered, ratio_notimm_notlow_child,
         imm_notlow_child_sum_filtered, ratio_imm_notlow_child, families, families_ratio)

ct_table <- bind_rows(ct_table_primary, ct_table_secondary) |>
  rename("Low-Income Non-Immigrant (n)" = "notimm_low_child_sum_filtered",
         "Low-Income Non-Immigrant (%)" = "ratio_notimm_low_child",
         "Low-Income Immigrant (n)" = "imm_low_child_sum_filtered",
         "Low-Income Immigrant (%)" = "ratio_imm_low_child",
         "Non-Immigrant (n)" = "notimm_notlow_child_sum_filtered",
         "Non-Immigrant (%)" = "ratio_notimm_notlow_child",
         "Immigrant (n)" = "imm_notlow_child_sum_filtered",
         "Immigrant (%)" = "ratio_imm_notlow_child",
         "School Type" = "school",
         "Families with Children (n)" = "families",
         "Families with Children (%)" = "families_ratio") |>
  select("School Type", "Families with Children (n)", "Families with Children (%)",
         "Non-Immigrant (n)", "Non-Immigrant (%)",
         "Immigrant (n)", "Immigrant (%)",
         "Low-Income Non-Immigrant (n)", "Low-Income Non-Immigrant (%)",
         "Low-Income Immigrant (n)", "Low-Income Immigrant (%)")

ct_table |> gt() |> 
  tab_style(
    style = cell_fill(color = "#F0F0F0"),
    locations = cells_body(rows = 2, columns = everything())
  ) |> 
  tab_style(
    style = cell_borders(
      sides = "right",
      color = "darkgrey",
      weight = px(2)
    ),
    locations = cells_body(columns = 3)
  ) |> 
  tab_style(
    style = cell_borders(
      sides = "right",
      color = "darkgrey",
      weight = px(2)
    ),
    locations = cells_body(columns = 7)
  ) |> 
  fmt_number(
    columns = ends_with("%)"),
    suffixing = TRUE,
    pattern = "{x}%",
    decimals = 0,
  ) |> 
  cols_label(
    `School Type` = ""
  )
