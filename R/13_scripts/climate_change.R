#Loading libraries
source("R/01_startup.R")
library(sf)
library(readxl)
library(cmhc)
library(scales)
library(raster)
library(rasterVis)

#Setting CensusMapper API Key because it won't save
set_cancensus_api_key("CensusMapper_4308d496f011429cf814385050f083dc")

#Grabbing all cancensus vector
can21 <- list_census_vectors(dataset = "CA21")

#Caching census and CMHC data to reduce amount of calls and speed up process.
#Personal use only, change the folder to your own folder if you want to use it
set_cancensus_cache_path("D:/McGill/can_cache", install = TRUE, overwrite = TRUE)
set_cmhc_cache_path("D:/McGill/can_cache", install = TRUE, overwrite = TRUE)

#Grabbing Laval's shapefile by census tract
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
heat_sf <- read_sf(dsn = "D://McGill/can_cache/heat", layer = "heat")

#Mapping out heat_sf
ggplot(data = heat_sf) +
  geom_sf(aes(fill = `_label`), color = NA) +
  labs(title = "Laval Heat and Coolness Islands 2020-2022",
       fill = "Heat Index") +
  scale_fill_gradientn(colors = curbcut_scale, na.value = curbcut_na) +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.justification = "center") +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
                               barwidth = 10, barheight = 1))

#Shapefile for heat vulnerability and edited using QGIS for Laval
#src = https://atlas-vulnerabilite.ulaval.ca/vague-de-chaleur/
ouranos_sf <- heat_sf <- read_sf(dsn = "D://McGill/can_cache/heat", layer = "ouranosheat") |> 
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

#Mapping out heat vulnerability
ggplot(data = ouranos_sf) +
  geom_sf(data = laval_csd, fill = "#F1F1F1", color = "black") +
  geom_sf(aes(fill = `Index`), color = "#454545", size = 0.3) +
  labs(title = "Heat Vulnerability in Laval 2018",
       fill = "Level of Vulnerability") +
  scale_fill_manual(values = heat_scale) +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.justification = "center") +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, 
                             label.position = "bottom", keywidth = 3, keyheight = 0.5))

# Biodiversity ------------------------------------------------------------

canopy_tif <- raster("D://Mcgill/can_cache/treecanopy/treecanopy.tif")

par(mar = c(0, 0, 0, 0))
breaks <- c(0, 1, 2, 3, 4, 5)
color_breaks <- c("#feb24c", "#fff7bc", "#e5f5f9", "#99d8c9", "#a6bddb")

plot(canopy_tif, main = "", breaks = breaks, col = color_breaks,
    box = FALSE, axes = FALSE, legend.shrink = 0.3)

title(main = "Canopy Coverage in Laval 2021", adj = 0.7, line = -2, cex.main = 1.2)

# Flooding ----------------------------------------------------------------
flooding <- st_read("D://Mcgill/can_cache/flood.gpkg")

laval_bbox <- st_bbox(laval_csd)

ggplot() +
  geom_sf(data = laval_csd, fill = "grey", color = NA) +
  geom_sf(data = flooding, fill = "lightblue", color = NA) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  labs(title = "Flood Zones in Laval 2023",
       fill = "Level of Vulnerability") +
  scale_fill_manual(values = heat_scale) +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.justification = "center") +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, 
                             label.position = "bottom", keywidth = 3, keyheight = 0.5)) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))
