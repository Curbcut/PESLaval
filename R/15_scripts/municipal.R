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

font_add("KMR Apparat", "C://Users/lolju/AppData/Local/Microsoft/Windows/Fonts/KMR-Apparat-Regular.ttf")

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
ggplot() +
  gg_cc_tiles +
  geom_sf(data = municipal_sf, aes(fill = cut(public_sum, breaks = c(-Inf, 0, 3, 5, 7, Inf),
                         labels = c("0", "1-3", "3-5", "5-7", "> 7"))), color = NA) +
  geom_sf(data = laval_sectors, fill = "transparent", color = "black") +
  scale_fill_manual(values = curbcut_scale, na.value = curbcut_na, name = "Nombre de lieux et édifices municipaux accessibles") +
  geom_sf(data = municipal_csv, aes(color = "Lieux et édifices municipaux"), size = 0.8, alpha = 0.8 ) +
  scale_color_manual(values = c("Lieux et édifices municipaux" = "#CD718C")) +
  labs(
    fill = "Nombre de lieux et édifices municipaux accessibles",
    color = ""
  ) +
  theme_void() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        legend.position = "bottom", legend.box = "horizontal",
        legend.title = element_text(hjust = 0.5, size = 9), legend.title.align = 0.5,
        legend.text.align = 0.5, text=element_text(family="KMR Apparat")) +
  guides(
    fill = guide_legend(order = 2, label.position = "bottom", title.position = "top"), 
    color = guide_legend(order = 1, label.position = "top", override.aes = list(size = 4)))

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

list(classInt::classIntervals(municipal_table_ttm$public_sum, n = 4, style = "jenks")$brks)

municipal_sf_ct <- table_sf |> 
  left_join(municipal_table_ttm, by = "GeoUID") |> 
  st_drop_geometry() |> 
  select(low, immigrant, Population, public_sum) |> 
  group_by(public_sum) |> 
  summarize("Faible revenu (n)" = sum(low, na.rm = TRUE),
            "Les immigrants (n)" = sum(immigrant, na.rm = TRUE),
            "Population (n)" = sum(Population, na.rm = TRUE)) |> 
  mutate(`Lieux et édifices municipaux accessibles (n)` = case_when(public_sum < 2 ~ "< 2",
                                                                    public_sum >= 2 & public_sum <= 6 ~ "2-6",
                                                                    public_sum > 6 & public_sum <= 12 ~ "6-12",
                                                                    public_sum > 12 ~ "> 12")) |> 
  group_by(`Lieux et édifices municipaux accessibles (n)`) |> 
  summarize("Faible revenu (n)" = sum(`Faible revenu (n)`),
            "Les immigrants (n)" = sum(`Les immigrants (n)`),
            "Population (n)" = sum(`Population (n)`)) |> 
  mutate(`Lieux et édifices municipaux accessibles (n)` = factor(`Lieux et édifices municipaux accessibles (n)`,
                                                                 levels = c("< 2", "2-6", "6-12", "> 12"))) |> 
  mutate("Faible revenu (%)" = round(`Faible revenu (n)` * 100 / sum(`Faible revenu (n)`), 2),
         "Les immigrants (%)" = round(`Les immigrants (n)` * 100 / sum(`Les immigrants (n)`), 2),
         "Population (%)" = round(`Population (n)` * 100 / sum(`Population (n)`), 2)) |> 
  arrange(`Lieux et édifices municipaux accessibles (n)`) |> 
  select(`Lieux et édifices municipaux accessibles (n)`, `Faible revenu (n)`, `Faible revenu (%)`,
         `Les immigrants (n)`, `Les immigrants (%)`, `Population (n)`, `Population (%)`)

municipal_sf_ct |> gt() |> 
  data_color(
    columns = ends_with("%)"),
    colors = scales::col_numeric(
      palette = c("white", c("white", "#C9C3FA")),
      domain = NULL
    )
  ) |> 
  fmt_number(
    columns = ends_with("%)"),
    suffixing = TRUE,
    pattern = "{x}%",
    decimals = 1,
  )
