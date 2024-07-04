#Loading up libraries
source("R/01_startup.R")
library(gbfs)
library(sf)
library(tidytransit)
library(osmdata)
library(tidygeocoder)
library(RMySQL)
library(classInt)

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

#Grabbing username and password from .Renviron
username <- Sys.getenv("DBI_USERNAME")
password <- Sys.getenv("DBI_PASSWORD")

#Setting up the curbcut scale
curbcut_scale <- c("#C4CDE1", "#98A8CB", "#6C83B5", "#4C5C7F", "#2B3448")
curbcut_na <- "#B3B3BB"

# Bus Stop Location -------------------------------------------------------
#Importing GTFS data. Available in the data folder under justin
stl_gtfs <- read_gtfs("D://McGill/can_cache/GTF_STL.zip")

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

# Bus Data ----------------------------------------------------------------
#Grabbing necessary STL data
stl_stops <- stl_gtfs$stops
stl_stoptimes <- stl_gtfs$stop_times
stl_trips <- stl_gtfs$trips
stl_routes <- stl_gtfs$routes

#Filtering only for weekday trips
weekday_trips <- stl_stoptimes |> 
  filter(trip_id %in% (stl_trips |> 
                         filter(service_id == "JUIN24SEM") |> 
                         pull(trip_id)))

#Number of vehicle stops per stop on a weekday
weekday_headways <- weekday_trips |> 
  group_by(stop_id) |> 
  summarize(weekday_headways = n())

#Weekday routes per stop
weekday_lines <- weekday_trips |> 
  rowwise() |> 
  mutate(route_id = str_extract(trip_id, paste(stl_routes$route_id, collapse = "|"))) |> 
  filter(!is.na(route_id)) |> 
  distinct(route_id, stop_id) |> 
  select(stop_id, route_id)

#Binding the weekday data together with bus stops and converting it into a shapefile
lvl_stops_sf <- stl_stops |> 
  select(stop_id, stop_lon, stop_lat) |> 
  left_join(weekday_lines, by = "stop_id") |> 
  left_join(weekday_headways, by = "stop_id") |> 
  mutate(bus_stop = 1) |> 
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)

#Isolating stop_ids to their DAs
stops_sf <- st_join(lvl_stops_sf, laval_db, join = st_within) |> 
  st_drop_geometry() |> 
  select(GeoUID, stop_id) |> 
  na.omit()

#Assigning each bus stop to their respective DBs and then summing up the total number of buses and stop each one has
laval_bus_pre <- st_join(lvl_stops_sf, laval_db, join = st_within) |> 
  filter(!is.na(GeoUID)) |> 
  group_by(GeoUID) |> 
  summarize(wd_headways = sum(weekday_headways, na.rm = TRUE),
    bus_stops = sum(bus_stop, na.rm = TRUE), .groups = 'drop')

#Joining all the data back to all the Laval DBs
laval_bus <- laval_db |> 
  st_join(laval_bus_pre) |> 
  select(GeoUID.x, Population, wd_headways, bus_stops) |> 
  rename("GeoUID" = "GeoUID.x") |> 
  replace_na(list(wd_headways = 0, bus_stops = 0))

#Grabbing the travel time matrix data and filtering for under 7 minutes
connection <- DBI::dbConnect(
  drv = RMySQL::MySQL(),
  username = username,
  password = password,
  host = "ccdb-instance-1.cplnwzthenux.us-east-1.rds.amazonaws.com",
  port = 3306,
  dbname = "ccdb"
)
ids <- paste0(paste0("'", laval_db$GeoUID, "'"), collapse = ", ")
ttm_7 <- DBI::dbGetQuery(connection, sprintf("SELECT * FROM ttm_foot_DB WHERE `from` IN (%s)", ids)) |> 
  filter(travel_seconds <= 420)
DBI::dbDisconnect(connection)

#Writing and importing the ttm_7 data so we don't have to call from the API every time
write.csv(ttm_7, "D://McGill/can_cache/ttm.csv")
ttm_7 <- read.csv("D://McGill/can_cache/ttm.csv") |> 
  select(-X, -travel_seconds) |> 
  mutate(across(everything(), as.character)) |> 
  rename("GeoUID" = "from")

#Creating extra rows so each DB can access their own DB and binding to ttm7
laval_ttm <- laval_bus |> 
  distinct(GeoUID) |> 
  mutate(to = GeoUID) |> 
  bind_rows(ttm_7) |> 
  arrange(GeoUID)

#Calculating number of accessible stops for each DB
laval_accessibilty_stop <- laval_ttm |> 
  left_join(stops_sf, by = c("to" = "GeoUID"), relationship = "many-to-many") |> 
  na.omit() |> 
  group_by(GeoUID) |> 
  summarise(stop_count = n())

#Calculating number of accessible unique routes for each DB
laval_accessibility_route <- laval_ttm |> 
  left_join(stops_sf, by = c("to" = "GeoUID"), relationship = "many-to-many") |> 
  left_join(weekday_lines, by = "stop_id", relationship = "many-to-many") |> 
  distinct(GeoUID, route_id) |> 
  na.omit() |> 
  group_by(GeoUID) |> 
  summarise(route_count = n())

#Calculating number of accessible bus runs for each DB
laval_accessibility_headway <- laval_ttm |> 
  left_join(stops_sf, by = c("to" = "GeoUID"), relationship = "many-to-many") |>
  left_join(weekday_trips, by = "stop_id", relationship = "many-to-many") |> 
  na.omit() |> 
  group_by(GeoUID) |> 
  summarise(headway_count = n())

#Joining data to the original Laval DB shapefile and replacing NAs with 0
laval_accessibility <- laval_db |> 
  left_join(laval_accessibilty_stop, by = "GeoUID") |> 
  left_join(laval_accessibility_route, by = "GeoUID") |> 
  left_join(laval_accessibility_headway, by = "GeoUID") |> 
  replace_na(list(headway_count = 0, route_count = 0, stop_count = 0))

#Finding proper breaks
list(classInt::classIntervals(laval_accessibility$stop_count, n = 5, style = "jenks")$brks)
list(classInt::classIntervals(laval_accessibility$route_count, n = 5, style = "jenks")$brks)
list(classInt::classIntervals(laval_accessibility$headway_count, n = 5, style = "jenks")$brks)

#Mapping out bus stops
ggplot(data = laval_accessibility) +
  geom_sf(data = mtlcma_sf, color = "black", fill = "#FCFCFC") +
  geom_sf(aes(fill = cut(stop_count, breaks = c(0, 9, 22, 45, 86, 150),
                         labels = c("< 9", "9-22", "22-45", "45-86", "> 86"))), color = NA) +
  geom_sf(data = laval_csd, color = "black", fill = NA) +
  scale_fill_manual(values = curbcut_scale, na.value = curbcut_na) +
  labs(title = "Nombre d'arrêts de bus accessibles à moins de 7 minutes à pied",
       fill = "Nombre total d'arrêts de bus accessibles") +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.justification = "center",
        panel.background = element_rect(fill = "#525252")) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5,
                             barwidth = 1, barheight = 1, nrow = 1)) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

#Mapping out route accessibility
ggplot(data = laval_accessibility) +
  geom_sf(data = mtlcma_sf, color = "black", fill = "#FCFCFC") +
  geom_sf(aes(fill = cut(route_count, breaks = c(0, 3, 8, 19, 35, 45),
                         labels = c("< 3", "3-8", "8-19", "19-35", "> 35"))), color = NA) +
  geom_sf(data = laval_csd, color = "black", fill = NA) +
  scale_fill_manual(values = curbcut_scale, na.value = curbcut_na) +
  labs(title = "Nombre de lignes de bus accessibles à moins de 7 minutes à pied",
       fill = "Nombre total de lignes de bus accessibles") +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.justification = "center",
        panel.background = element_rect(fill = "#525252")) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5,
                             barwidth = 1, barheight = 1, nrow = 1)) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

#Mapping out bus runs accessibility
ggplot(data = laval_accessibility) +
  geom_sf(data = mtlcma_sf, color = "black", fill = "#FCFCFC") +
  geom_sf(aes(fill = cut(headway_count, breaks = c(0, 1202, 3601, 10715, 21869, 39934),
                         labels = c("< 1202", "1202-3601", "3601-10715", "10715-21869", "> 21869"))), color = NA) +
  geom_sf(data = laval_csd, color = "black", fill = NA) +
  scale_fill_manual(values = curbcut_scale, na.value = curbcut_na) +
  labs(title = "Nombre de trajets de bus accessibles à moins de 7 minutes à pied",
       fill = "Nombre total de parcours d'autobus accessibles") +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.justification = "center",
        panel.background = element_rect(fill = "#525252")) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5,
                             barwidth = 1, barheight = 1, nrow = 1)) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))
