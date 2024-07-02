#Loading up libraries
source("R/01_startup.R")
library(gbfs)
library(sf)
library(tidytransit)
library(osmdata)
library(tidygeocoder)

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

#Grabbing the shapefile for the Montreal CMA
mtlcma_sf <- cancensus::get_census(dataset = "CA21", 
                                regions = list(CSD = 24462), 
                                level = "CMA", 
                                geo_format = "sf")

#Setting the Laval bound box for maps
laval_bbox <- st_bbox(laval_ct)

# Bus Stop Location -------------------------------------------------------
#Grabbing GTFS data from the STL
stl_gtfs <- read_gtfs("https://translate.google.com/website?sl=fr&tl=en&hl=en&prev=search&u=https://www.stlaval.ca/datas/opendata/GTF_STL.zip")

#Grabbing bus stops, converting it to be usable to plot, and removing duplicates
#Removing duplicates doesn't necessarily reflect real life as there are usually
#two stops at one intersection, but makes map readability significantly better
bus_stops <- st_as_sf(stl_gtfs$stops, coords = c("stop_lon", "stop_lat"), crs = 4326) |> 
  distinct(stop_name, .keep_all = TRUE)

#Plotting the bus stops
ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey", color = "black") +
  geom_sf(data = laval_ct, fill = "white", color = "black") +
  geom_sf(data = bus_stops, color = "indianred3", size = 0.65) +
  theme_minimal() +
  labs(title = "Arrêts d'autobus de la Société de transport de Laval 2024") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.background = element_rect(fill = "lightblue")) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

# Cycle Paths -------------------------------------------------------------
#Grabbing all bike lanes within the Laval bound box
cycle_osm <- opq(bbox = laval_bbox, timeout = 300) |> 
  add_osm_feature(key = "highway", value = "cycleway") |> 
  osmdata_sf()

#Keeping only the bike lanes within Laval
cycle_lvl <- cycle_osm$osm_lines |> 
  st_intersection(laval_ct)

#Mapping out the bike lanes
ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey", color = "black") +
  geom_sf(data = laval_csd, fill = "white", color = "black") +
  geom_sf(data = cycle_lvl, color = "darkred", size = 5) +
  coord_sf() +
  theme_minimal() +
  labs(title = "Pistes cyclables à Laval 2024") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.background = element_rect(fill = "lightblue")) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

# Bixi Stations -----------------------------------------------------------
#Grabbing all Bixi station information
bixi_stations <- get_station_information(city = "Montreal")

#Converting the bixi station information into a usable coordinate system
bixi_sf <- st_as_sf(bixi_stations, coords = c("lon", "lat"), crs = 4326) |> 
  filter(name != "cpatel") |> 
  st_intersection(laval_ct)

#Mapping out the stations onto the city of Laval
ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey", color = "black") +
  geom_sf(data = laval_ct, fill = "white", color = "black") +
  geom_sf(data = bixi_sf, color = "red", size = 1) +
  theme_minimal() +
  labs(title = "Stations Bixi à Laval 2024") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "lightblue")) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

# Points de vente (transport collectif) -----------------------------------
#Using the shapefile below and STL_accessibilite-points-vente.pdf and
#the STL website to determine the level of accessibility
full_acc <- c("475, boul. de l'Avenir", "600, montée du Moulin", "800b, boul. Chomedey (160)",
              "275, boul. Samson", "2500, boul. Saint-Martin Est", "165, boul. Curé-Labelle",
              "2220, boul. des Laurentides")
partial_acc <- c("10B, rue Dufferin", "555, boul. Samson", "650, rue Principale",
                 "1263, boul. Jolibourg", "155, boul. de La Concorde Est",
                 "2397, boul. Curé-Labelle", "4600, boul. Samson", "4650, boul. du Souvenir",
                 "4219, boul. Samson", "965, boul. Curé-Labelle", "5000, boul. des Laurentides",
                 "1690, boul. Sainte-Rose", "4425, boul. de La Concorde", "3475, boul. Dagenais",
                 "5680, boul. des Laurentides", "255, boul. de La Concorde Ouest", "430, boul. Cartier",
                 "44, boul. des Laurentides", "545, rue Lucien-Paiement", "3000, boul. Le Carrefour",
                 "580,  boul. Curé-Labelle (suite 10)", "4672, boul. Saint-Martin",
                 "2955, boul. de La Concorde", "1295, boul. de la Concorde O.", "405, boul. des Laurentides",
                 "6155, boul. Arthur-Sauvé", "4347, boul. Ste-Rose")
no_acc <- c("3875, boul. Sainte-Rose", "501, rue Guillemette", "265, 15e Rue",
            "3667, boul. Lévesque Ouest", "45, boul. Lévesque Est", "3323, boul. de la Concorde E.",
            "2795, boul. René-Laennec", "5162, de la Fabrique")
unknown <- c("2250, avenue Francis-Hughes", "308, Bd Cartier O.", "1859, boul. René-Laennec",
             "705, chemin du Trait Carré")

#Importing GEOADMIN_POINT_VENTE.shp from the data folder and 
#assigning each row their accessibility value
vending_sf <- st_read("/Users/justin/Documents/R/curbcut/Points_de_vente_shp/GEOADMIN_POINT_VENTE.shp") |> 
  st_transform(crs = 4326) |> 
  mutate(accessible = case_when(
    ADRESSE %in% full_acc ~ "Accessible",
    ADRESSE %in% partial_acc ~ "Partiellement accessible",
    ADRESSE %in% no_acc ~ "Non accessible",
    ADRESSE %in% unknown ~ "Accessibilité inconnue",
    TRUE ~ NA_character_)) |> 
  mutate(accessible = factor(accessible, levels = c("Accessible", "Partiellement accessible",
                                                    "Non accessible", "Accessibilité inconnue")))

#Mapping out the point of sale data
ggplot(vending_sf) + 
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = laval_csd, fill = "white", color = "black") +
  geom_sf(aes(color = accessible)) +
  labs(title = "Point de vente de la Société de transport de Laval") +
  scale_color_manual(values = c("Accessible" = "#648FFF", 
                                "Partiellement accessible" = "#FFB000", 
                                "Non accessible" = "#DC267F",
                                "Accessibilité inconnue" = "grey")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        legend.title = element_blank(), legend.position = "bottom",
        panel.grid = element_blank(), panel.background = element_rect(fill = "lightblue")) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

# Bus Stop Data -----------------------------------------------------------
#Use stl_gtfs above
bus_stops <- st_as_sf(stl_gtfs$stops, coords = c("stop_lon", "stop_lat"), crs = 4326)
stl_stoptimes <- stl_gtfs$stop_times
combined_stl <- bus_stops |> 
  select(-location_type, -stop_display, -stop_abribus) |> 
  left_join(headways_count, join_by("stop_id" == "Var1"))


headways_count <- as.data.frame(table(stl_stoptimes$stop_id))
