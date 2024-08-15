#Loading up libraries
source("R/01_startup.R")
library(mapboxapi)
library(patchwork)
library(sf)
library(RMySQL)
library(classInt)
library(ggnewscale)
library(biscale)
library(cowplot)
library(gt)
library(scales)
library(showtext)
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

# TTM Import ------------------------------------------------------------
#Import 15 minute walking TTM and modifying it with necessary rows
#Check mobility_transport.R to get the file
ttm_walk_15_rows <- laval_db |> 
  st_drop_geometry() |> 
  select(GeoUID) |> 
  mutate(to = GeoUID)

ttm_walk_15 <- read.csv("D://McGill/can_cache/walk15.csv") |> 
  select(-X, -travel_seconds) |> 
  mutate(across(everything(), as.character)) |> 
  rename("GeoUID" = "from") |> 
  bind_rows(ttm_walk_15_rows) |> 
  arrange(GeoUID)


# Data Processing ---------------------------------------------------------
#Importing the data and filtering out unnecessary data
#Categorized
municipal_csv <- read.csv("D://McGill/can_cache/lieux.csv") |> 
  filter(categorie.id != "Lieu privé") |> 
  mutate(latitude = case_when(id.lieu == 525 ~ 45.55916, id.lieu == 540 ~ 45.55813,
                              id.lieu == 549 ~ 45.55613, TRUE ~ latitude),
         longitude = case_when(id.lieu == 525 ~  -73.84607, id.lieu == 540 ~ -73.74693,
                               id.lieu == 549 ~ -73.72122, TRUE ~ longitude)) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  select(type.commun) |> 
  mutate(type.commun = case_when(#Editing types of public places to those we want
                                 type.commun == "Autres" ~ "Places publiques",
                                 type.commun == "Belvédère" ~ "Places publiques",
                                 type.commun == "Centres et galeries d'art" ~ "Places publiques",
                                 type.commun == "Édifice municipal" ~ "Places publiques",
                                 type.commun == "Piscine extérieure" ~ "Places publiques", 
                                 type.commun == "Piscine intérieure" ~ "Places publiques",
                                 type.commun == "Jeux d'eau" ~ "Sites avec équipement de parcs",
                                 type.commun == "Patinoire extérieure" ~ "Sites avec équipement de parcs",
                                 TRUE ~ type.commun)) |> 
  filter(type.commun != "Berge",
         type.commun != "Bois",
         type.commun != "Bureau municipal lavallois",
         type.commun != "Caserne de pompiers",
         type.commun != "Gare",
         type.commun != "Halte",
         type.commun != "Parc",
         type.commun != "Poste de police de quartier",
         type.commun != "Station de métro")

#All in One
municipal_csv <- read.csv("D://McGill/can_cache/lieux.csv") |> 
  filter(categorie.id != "Lieu privé",
         type.commun != "Parc") |> 
  mutate(latitude = case_when(id.lieu == 525 ~ 45.55916, id.lieu == 540 ~ 45.55813,
                              id.lieu == 549 ~ 45.55613, TRUE ~ latitude),
         longitude = case_when(id.lieu == 525 ~  -73.84607, id.lieu == 540 ~ -73.74693,
                               id.lieu == 549 ~ -73.72122, TRUE ~ longitude)) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  select(type.commun)

#Determining which points intersected and adding a new column to make the calculation easier
municipal_fromto <- laval_db |> 
  st_join(municipal_csv) |> 
  filter(!is.na(type.commun)) |> 
  select(GeoUID) |> 
  mutate(public = 1) |> 
  st_drop_geometry()

#Combining with the TTM
municipal_ttm <- ttm_walk_15 |> 
  left_join(municipal_fromto, join_by("to" == "GeoUID"), relationship = "many-to-many") |> 
  group_by(GeoUID) |> 
  summarise(public_sum = sum(public, na.rm = TRUE), .groups = 'drop')

#Creating the shapefile
municipal_sf <- laval_db |> 
  left_join(municipal_ttm, by = "GeoUID")

# Maps --------------------------------------------------------------------
#Determining the breaks
list(classInt::classIntervals(municipal_sf$public_sum, n = 5, style = "jenks")$brks)

#Mapping municipal accessibility
municipal_map <- ggplot() +
  gg_cc_tiles +
  geom_sf(data = municipal_sf, aes(fill = cut(public_sum, breaks = c(-Inf, 0, 3, 5, 7, Inf),
                         labels = c("0", "1-3", "3-5", "5-7", "> 7"))), color = NA) +
  geom_sf(data = laval_sectors, fill = "transparent", color = "black") +
  scale_fill_manual(values = curbcut_scale, na.value = curbcut_na, name = "Édifices et lieux municipaux accessibles (n)") +
  geom_sf(data = municipal_csv, aes(color = "Lieux et édifices municipaux"), size = 0.8, alpha = 0.8 ) +
  scale_color_manual(values = c("Lieux et édifices municipaux" = "#CD718C")) +
  labs(color = "") +
  theme_void() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        legend.position = "bottom", legend.box = "horizontal",
        legend.title = element_text(hjust = 0.5), legend.title.align = 0.5,
        legend.text.align = 0.5, text=element_text(family="KMR Apparat Regular")) +
  guides(
    fill = guide_legend(order = 2, label.position = "bottom", title.position = "top"), 
    color = guide_legend(order = 1, label.position = "top", override.aes = list(size = 3)))

# Tables ------------------------------------------------------------------
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

table_sf <- get_census(dataset = "CA21", 
                       regions = list(CSD = 2465005), 
                       level = "CT",
                       vectors = c("low" ="v_CA21_1025", "immigrant" = "v_CA21_4410"),
                       geo_format = "sf")

municipal_ct <- laval_ct |> 
  st_join(municipal_csv) |> 
  st_drop_geometry() |> 
  filter(!is.na(type.commun)) |> 
  mutate(place = 1) |> 
  select(GeoUID, place)

municipal_table_ttm <- ttm_walk_15_ct |> 
  left_join(municipal_ct, join_by("to" == "GeoUID"), relationship = "many-to-many") |> 
  group_by(GeoUID) |> 
  summarise(public_sum = sum(place, na.rm = TRUE), .groups = 'drop')

list(classInt::classIntervals(municipal_table_ttm$public_sum, n = 5, style = "jenks")$brks)

averages <- table_sf |> 
  left_join(municipal_table_ttm, by = "GeoUID") |> 
  st_drop_geometry() |> 
  select(low, immigrant, Population, public_sum) |> 
  mutate(total_low = low * public_sum,
         total_imm = immigrant * public_sum,
         total_population = Population * public_sum) |> 
  summarize(across(everything(), sum, na.rm = TRUE))

municipal_sf_ct <- table_sf |> 
  left_join(municipal_table_ttm, by = "GeoUID") |> 
  st_drop_geometry() |> 
  select(low, immigrant, Population, public_sum) |> 
  group_by(public_sum) |> 
  summarize("Faible revenu (n)" = sum(low, na.rm = TRUE),
            "Les immigrants (n)" = sum(immigrant, na.rm = TRUE),
            "Population (n)" = sum(Population, na.rm = TRUE)) |> 
  mutate(`Lieux et édifices municipaux accessibles (n)` = case_when(public_sum == 0 ~ "0",
                                                                    public_sum >= 1 & public_sum <= 2 ~ "1-2",
                                                                    public_sum > 2 & public_sum <= 6 ~ "3-6",
                                                                    public_sum > 6 & public_sum <= 10 ~ "7-10",
                                                                    public_sum > 10 ~ "> 10")) |> 
  group_by(`Lieux et édifices municipaux accessibles (n)`) |> 
  summarize("Faible revenu (n)" = sum(`Faible revenu (n)`),
            "Les immigrants (n)" = sum(`Les immigrants (n)`),
            "Population (n)" = sum(`Population (n)`)) |> 
  mutate(`Lieux et édifices municipaux accessibles (n)` = factor(`Lieux et édifices municipaux accessibles (n)`,
                                                                 levels = c("0", "1-2", "3-6", "7-10", "> 10"))) |> 
  mutate("Faible revenu (%)" = round(`Faible revenu (n)` * 100 / sum(`Faible revenu (n)`), 2),
         "Les immigrants (%)" = round(`Les immigrants (n)` * 100 / sum(`Les immigrants (n)`), 2),
         "Population (%)" = round(`Population (n)` * 100 / sum(`Population (n)`), 2)) |> 
  arrange(`Lieux et édifices municipaux accessibles (n)`) |> 
  select(`Lieux et édifices municipaux accessibles (n)`, `Faible revenu (n)`, `Faible revenu (%)`,
         `Les immigrants (n)`, `Les immigrants (%)`, `Population (n)`, `Population (%)`) |> 
  mutate("Faible revenu (n)" = convert_number(`Faible revenu (n)`),
         "Les immigrants (n)" = convert_number(`Les immigrants (n)`),
         "Population (n)" = convert_number(`Population (n)`))

municipal_table <- municipal_sf_ct |> gt() |> 
  tab_options(
    table.font.names = "KMR Apparat Regular",
    table.font.size = px(14)
  ) |> 
  data_color(
    columns = ends_with("%)"),
    colors = scales::col_numeric(
      palette = c("white", c("white", "#C9C3FA")),
      domain = NULL
    )
  ) |> 
  fmt(
    columns = ends_with("%)"),
    fns = function(x) {
      formatted <- sprintf("%.1f %%", x)
      gsub("\\.", ",", formatted)
    }
  )

#Numbers for markdown
municipal <- table_sf |> 
  left_join(municipal_table_ttm, by = "GeoUID") |> 
  st_drop_geometry() |> 
  select(low, immigrant, Population, public_sum) |> 
  group_by(public_sum) |> 
  summarize("Faible revenu (n)" = sum(low, na.rm = TRUE),
            "Les immigrants (n)" = sum(immigrant, na.rm = TRUE),
            "Population (n)" = sum(Population, na.rm = TRUE)) |> 
  mutate(`public_sum` = case_when(public_sum == 0 ~ "0",
                                  public_sum >= 1 & public_sum <= 2 ~ "1-2",
                                  public_sum > 2 & public_sum <= 6 ~ "3-6",
                                  public_sum > 6 & public_sum <= 10 ~ "7-10",
                                  public_sum > 10 ~ "> 10")) |> 
  group_by(`public_sum`) |> 
  summarize("Faible revenu (n)" = sum(`Faible revenu (n)`),
            "Les immigrants (n)" = sum(`Les immigrants (n)`),
            "Population (n)" = sum(`Population (n)`))

#Municipal numbers but broken down
municipal_breakdown <- table_sf |> 
  left_join(municipal_table_ttm, by = "GeoUID") |> 
  st_drop_geometry() |> 
  select(low, immigrant, Population, public_sum) |> 
  group_by(public_sum) |> 
  summarize("Faible revenu (n)" = sum(low, na.rm = TRUE),
            "Les immigrants (n)" = sum(immigrant, na.rm = TRUE),
            "Population (n)" = sum(Population, na.rm = TRUE))

municipal_population <- municipal_breakdown |> 
  summarize(Population = sum(`Population (n)`, na.rm = TRUE)) |> 
  pull(Population)

municipal_lowincome <- municipal_breakdown |> 
  summarize(low = sum(`Faible revenu (n)`, na.rm = TRUE)) |> 
  pull(low)

municipal_immigrant <- municipal_breakdown |> 
  summarize(imm = sum(`Les immigrants (n)`, na.rm = TRUE)) |> 
  pull(imm)

municipal_access <- municipal |> 
  filter(public_sum != 0) |> 
  summarise(Population = sum(`Population (n)`, na.rm = TRUE)) |> 
  mutate(Population = convert_pct(Population / municipal_population)) |> 
  pull(Population)

municipal_access_three <- municipal |> 
  filter(public_sum != 0 & public_sum != "1-2") |> 
  summarise(Population = sum(`Population (n)`, na.rm = TRUE)) |> 
  mutate(Population = convert_pct(Population / municipal_population)) |> 
  pull(Population)

municipal_immigrant_ratio <- table_sf |> 
  left_join(municipal_table_ttm, by = "GeoUID") |> 
  st_drop_geometry() |> 
  select(immigrant, public_sum) |>
  mutate(total = immigrant * public_sum) |> 
  summarise(immigrant = sum(immigrant, na.rm = TRUE),
            total = sum(total, na.rm = TRUE)) |> 
  mutate(ratio = round(total / immigrant, 1)) |> 
  mutate(ratio = gsub("\\.", ",", as.character(ratio))) |> 
  pull(ratio)

municipal_population_ratio <- table_sf |> 
  left_join(municipal_table_ttm, by = "GeoUID") |> 
  st_drop_geometry() |> 
  select(Population, public_sum) |>
  mutate(total = Population * public_sum) |> 
  summarise(Population = sum(Population, na.rm = TRUE),
            total = sum(total, na.rm = TRUE)) |> 
  mutate(ratio = round(total / Population, 1)) |> 
  mutate(ratio = gsub("\\.", ",", as.character(ratio))) |> 
  pull(ratio)

municipal_lowincome_ratio <- table_sf |> 
  left_join(municipal_table_ttm, by = "GeoUID") |> 
  st_drop_geometry() |> 
  select(low, public_sum) |>
  mutate(total = low * public_sum) |> 
  summarise(low = sum(low, na.rm = TRUE),
            total = sum(total, na.rm = TRUE)) |> 
  mutate(ratio = round(total / low, 1)) |> 
  mutate(ratio = gsub("\\.", ",", as.character(ratio))) |> 
  pull(ratio)

municipal_lowincome_lowaccess <- municipal_breakdown |> 
  filter(public_sum < 4) |> 
  summarise(low = sum(`Faible revenu (n)`, na.rm = TRUE)) |> 
  mutate(low = convert_pct(low / municipal_lowincome)) |> 
  pull(low)

municipal_immigrant_lowaccess <- municipal_breakdown |> 
  filter(public_sum < 4) |> 
  summarise(imm = sum(`Les immigrants (n)`, na.rm = TRUE)) |> 
  mutate(imm = convert_pct(imm / municipal_immigrant)) |> 
  pull(imm)

municipal_pop_lowaccess <- municipal_breakdown |> 
  filter(public_sum < 4) |> 
  summarise(pop = sum(`Population (n)`, na.rm = TRUE)) |> 
  mutate(pop = convert_pct(pop / municipal_population)) |> 
  pull(pop)

# R Markdown --------------------------------------------------------------
ggplot2::ggsave(filename = here::here("output/axe3/access/municipal_map.pdf"), 
                plot = municipal_map, width = 7.5, height = 6)

qs::qsavem(municipal_map, municipal_table, municipal_access, municipal_access_three,
           municipal_immigrant_ratio, municipal_population_ratio, municipal_lowincome_ratio,
           municipal_lowincome_lowaccess, municipal_immigrant_lowaccess, municipal_pop_lowaccess,
           file = "data/axe3/municipal.qsm")

