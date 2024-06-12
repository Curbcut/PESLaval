#Loading libraries
source("R/01_startup.R")
library(sf)
library(readxl)
library(cmhc)
library(scales)
library(raster) #Only load this if you don't need to use select()

#Setting CensusMapper API Key because it won't save
set_cancensus_api_key("CensusMapper_4308d496f011429cf814385050f083dc")

#Grabbing all cancensus vector
can21 <- list_census_vectors(dataset = "CA21")

#Caching census and CMHC data to reduce amount of calls and speed up process.
#Personal use only, change the folder to your own folder if you want to use it
set_cancensus_cache_path("D:/McGill/can_cache", install = TRUE, overwrite = TRUE)
set_cmhc_cache_path("D:/McGill/can_cache", install = TRUE, overwrite = TRUE)

#Reading the Montreal CMA shapefile
mtlcma_sf <- cancensus::get_census(dataset = "CA21",
                                   regions = list(CMA = 24462),
                                   level = "CMA",
                                   geo_format = "sf")

#Grabbing Laval's shapefile by census subdivision
laval_csd <- cancensus::get_census(dataset = "CA21", 
                                  regions = list(CSD = 2465005), 
                                  level = "CSD", 
                                  geo_format = "sf")

#Grabbing Laval's shapefile by census tract
laval_ct <- cancensus::get_census(dataset = "CA21", 
                                  regions = list(CSD = 2465005), 
                                  level = "CT", 
                                  geo_format = "sf")

#Grabbing Laval shapefile by dissemination area
laval_da <- cancensus::get_census(dataset = "CA21", 
                                  regions = list(CSD = 2465005), 
                                  level = "DA", 
                                  geo_format = "sf")

#Loading the colors for the curbcut and heat scale for ouranos_sf
curbcut_scale <- c("#C4CDE1", "#98A8CB", "#6C83B5", "#4C5C7F", "#2B3448")
heat_scale <- c("1" = "#2CA25F", "2" = "#98D6C7", "3" = "#E5F5F9", "4" = "#FCDFD2", "5" = "#FA9073")
curbcut_na <- "#B3B3BB"

# Heat --------------------------------------------------------------------
#Shapefile for heat index in Laval
#src = https://www.donneesquebec.ca/recherche/dataset/ilots-de-chaleur-fraicheur-urbains-et-ecarts-de-temperature-relatifs-2020-2022
#Edited using QGIS to be Laval only
heat_sf <- read_sf(dsn = "D://McGill/can_cache/heat", layer = "heat") |> 
  st_transform(4326)

if (is.na(st_crs(heat_sf))) {
  heat_sf <- st_set_crs(heat_sf, 4326)
}

#Mapping out heat_sf
ggplot() +
  geom_sf(data = heat_sf, aes(fill = `_label`), color = NA) +
  labs(title = "Indice d’intensité d’îlots de chaleur urbains de Laval 2020-2022 ",
       fill = "Indice d'Intensité de Chaleur") +
  scale_fill_gradientn(colors = curbcut_scale, na.value = curbcut_na,
                       labels = label_comma(big.mark = ".", decimal.mark = ",")) +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.justification = "center") +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
                               barwidth = 10, barheight = 1))

#Shapefile for heat vulnerability and edited using QGIS for Laval
#src = https://atlas-vulnerabilite.ulaval.ca/vague-de-chaleur/
ouranos_sf <- read_sf("D://McGill/can_cache/heat/ouranosheat.shp") |> 
  select(-URL_12) |> 
  mutate(Index = case_when(
    N_Vulnre_2 == 'Donnée manquante' ~ NA,
    N_Vulnre_2 == 'Très faible vulnérabilité' ~ "1",
    N_Vulnre_2 == 'Faible vulnérabilité' ~ "2",
    N_Vulnre_2 == 'Vulnérabilité modérée' ~ "3",
    N_Vulnre_2 == 'Vulnérabilité moyenne' ~ "4",
    N_Vulnre_2 == 'Forte vulnérabilité' ~ "5",
    TRUE ~ "0"  # Default value for all other cases
  ))

#calculating number of people who are highly vulnerable to heat waves
vuln_pop <- ouranos_sf |> 
  filter(N_Vulnre_2 %in% c("Forte vulnérabilité")) |> 
  summarise(sum_Pop = sum(Pop16_12, na.rm = TRUE))

#Mapping out heat vulnerability
ggplot(data = ouranos_sf) +
  geom_sf(data = laval_csd, fill = "#F1F1F1", color = "black") +
  geom_sf(aes(fill = `Index`), color = "#454545", size = 0.3) +
  labs(title = "Vulnérabilité à la chaleur à Laval 2018",
       fill = "Niveau de vulnérabilité") +
  scale_fill_manual(values = heat_scale) +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.justification = "center") +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, 
                             label.position = "bottom", keywidth = 3, keyheight = 0.5))

# Biodiversity ------------------------------------------------------------
#Reading greenspace shapefile and canopy .tif
greenspace_shp <- st_read("/Users/justin/Documents/R/curbcut/parc_espace_vert_20240607_PG.shp")
canopy_tif <- raster("D://Mcgill/can_cache/treecanopy/treecanopy.tif")

#Adding artificial margins, breaks, and associated colors
par(mar = c(0, 0, 0, 0))
breaks <- c(0, 1, 2, 3, 4, 5)
color_breaks <- c("#feb24c", "#fff7bc", "#e5f5f9", "#99d8c9", "#a6bddb")

#Plotting the .tif file
plot(canopy_tif, main = "", breaks = breaks, col = color_breaks,
    box = FALSE, axes = FALSE, legend.shrink = 0.3)

#Adding title to the .tif map
title(main = "Couverture du Couvert Végétal à Laval 2021", adj = 0.9, line = -2, cex.main = 1.2)

#Mapping out greenspaces (unused)
ggplot() +
  geom_sf(data = laval_csd, color = "black", fill = "#F8F8F8") +
  geom_sf(data = greenspace_shp, color = "darkgreen", fill = "darkgreen") +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.justification = "center")

# Flooding ----------------------------------------------------------------
#Vectors for the flooding
flooding <- st_read("D://Mcgill/can_cache/flood.gpkg") |> 
  st_transform(crs = 4326)
curbcut_flooding <- st_read("/Users/justin/Documents/R/curbcut/zone_inondable_RCI_CDU_20240607_PG.shp") |> 
  st_transform(crs = 4326)
flooding0_2 <- curbcut_flooding |> 
  filter(LIMITE == "Zone inondable 0-2 ans")
flooding2_20 <- curbcut_flooding |> 
  filter(LIMITE == "Zone inondable 2-20 ans")
flooding20_100 <- curbcut_flooding |> 
  filter(LIMITE == "Zone inondable 20-100 ans")

#Grabbing the shapefile for Laval DB so number of areas affected by flooding can be calculated
laval_db <- cancensus::get_census(dataset = "CA21", 
                                  regions = list(CSD = 2465005), 
                                  level = "DB", 
                                  geo_format = "sf") |> 
  st_transform(32198)

#Bounding box of Laval
laval_bbox <- st_bbox(laval_csd)

#Mapping the Quebec given data
ggplot() +
  geom_sf(data = mtlcma_sf, fill = "grey", color = "black") +
  geom_sf(data = laval_csd, fill = "#FAF9F6", color = NA) +
  geom_sf(data = flooding, fill = "#74ccf4", color = NA) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  labs(title = "Zones inondables de Laval 2023",
       fill = "Level of Vulnerability") +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.justification = "center") +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, 
                             label.position = "bottom", keywidth = 3, keyheight = 0.5)) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

#Flood map using the more granular data, not included in the first draft
ggplot() +
  geom_sf(data = mtlcma_sf, fill = "grey", color = "black") +
  geom_sf(data = laval_csd, fill = "#F8F8F8", color = "black") +
  geom_sf(data = flooding20_100, fill = "#bdc9e1",color = NA) +
  geom_sf(data = flooding2_20, fill = "#74a9cf",color = NA) +
  geom_sf(data = flooding0_2, fill = "#045a8d",color = NA) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  labs(title = "Flood Zones in Laval 2023",
       fill = "Level of Vulnerability") +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        panel.background = element_rect(fill = "#cedbdc"),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.justification = "center") +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

#Validating shapefiles to ensure the analysis works
flooding <- st_make_valid(flooding)
laval_db <- st_make_valid(laval_db)
#Determining which DBs intersect with the flood zones, and then giving them a value of 1 in the flood column
intersects <- st_intersects(laval_db, flooding, sparse = FALSE)
laval_db$flooded <- apply(intersects, 1, function(x) as.integer(any(x)))
#Summing up the population of DBs that intersect with the flood zone and calculating the proportion of the population
flood_lvl <- laval_db |> 
  summarise(
    total_pop = sum(Population, na.rm = TRUE),
    flood_pop = sum(Population[flooded == 1], na.rm = TRUE)
  ) |> 
  mutate(flood_prop = flood_pop / total_pop)
