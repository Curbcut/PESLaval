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

# Bus Stop Data -----------------------------------------------------------
#Use stl_gtfs above. Vector names are self explanatory
stl_stops <- stl_gtfs$stops
stl_stoptimes <- stl_gtfs$stop_times
stl_trips <- stl_gtfs$trips
stl_routes <- stl_gtfs$routes

#Filtering only for weekday trips
weekday_trips <- stl_stoptimes |> 
  filter(trip_id %in% (stl_trips |> 
                         filter(service_id == "JUIN24SEM") |> 
                         pull(trip_id)))

#Finding the number of lines per stop on a weekday
weekday_lines <- weekday_trips |> 
  rowwise() |> 
  mutate(route_id = str_extract(trip_id, paste(stl_routes$route_id, collapse = "|"))) |> 
  filter(!is.na(route_id)) |> 
  distinct(route_id, stop_id) |> 
  group_by(stop_id) |> 
  summarize(weekday_lines = n())

#Number of vehicle stops per stop on a weekday
weekday_headways <- weekday_trips |> 
  group_by(stop_id) |> 
  summarize(weekday_headways = n())

#Filtering only for saturday trips
saturday_trips <- stl_stoptimes |> 
  filter(trip_id %in% (stl_trips |> 
                         filter(service_id == "JUIN24SAM") |> 
                         pull(trip_id)))

#Finding the number of lines per stop on a saturday
saturday_lines <- saturday_trips |> 
  rowwise() |> 
  mutate(route_id = str_extract(trip_id, paste(stl_routes$route_id, collapse = "|"))) |> 
  filter(!is.na(route_id)) |> 
  distinct(route_id, stop_id) |> 
  group_by(stop_id) |> 
  summarize(saturday_lines = n())

#Number of vehicle stops per stop on a saturday
saturday_headways <- saturday_trips |> 
  group_by(stop_id) |> 
  summarize(saturday_headways = n())

#Filtering only for sunday trips
sunday_trips <- stl_stoptimes |> 
  filter(trip_id %in% (stl_trips |> 
                         filter(service_id == "JUIN24DIM") |> 
                         pull(trip_id)))

#Finding the number of lines per stop on a sunday
sunday_lines <- sunday_trips |> 
  rowwise() |> 
  mutate(route_id = str_extract(trip_id, paste(stl_routes$route_id, collapse = "|"))) |> 
  filter(!is.na(route_id)) |> 
  distinct(route_id, stop_id) |> 
  group_by(stop_id) |> 
  summarize(sunday_lines = n())

#Number of vehicle stops per stop on a sunday
sunday_headways <- sunday_trips |> 
  group_by(stop_id) |> 
  summarize(sunday_headways = n())

#Combining all the data together in one master file
combined_stl <- stl_stops |> 
  select(-location_type, -stop_display, -stop_abribus, -stop_code) |> 
  left_join(weekday_lines, by = "stop_id") |> 
  left_join(weekday_headways, by = "stop_id") |> 
  left_join(saturday_lines, by = "stop_id") |> 
  left_join(saturday_headways, by = "stop_id") |> 
  left_join(sunday_lines, by = "stop_id") |> 
  left_join(sunday_headways, by = "stop_id") |> 
  replace_na(list(
    weekday_lines = 0, weekday_headways = 0,
    saturday_lines = 0, saturday_headways = 0,
    sunday_lines = 0, sunday_headways = 0
  )) |> 
  mutate(bus_stop = 1) |> 
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)

#Prepping join with DB by combining the data together and summarizing the counts
laval_bus_pre <- st_join(combined_stl, laval_db, join = st_within) |> 
  filter(!is.na(GeoUID)) |> 
  group_by(GeoUID) |> 
  summarize(
    wd_headways = sum(weekday_headways, na.rm = TRUE), wd_lines = sum(weekday_lines, na.rm = TRUE),
    sat_headways = sum(saturday_headways, na.rm = TRUE), sat_lines = sum(saturday_lines, na.rm = TRUE),
    sun_headways = sum(sunday_headways, na.rm = TRUE), sun_lines = sum(sunday_lines, na.rm = TRUE),
    bus_stops = sum(bus_stop, na.rm = TRUE), .groups = 'drop')

#Combining the data and cleaning it up with the shapefile
laval_bus <- laval_db |> 
  st_join(laval_bus_pre) |> 
  select(GeoUID.x, Population, wd_headways, wd_lines, sat_headways, sat_lines,
         sun_headways, sun_lines, bus_stops) |> 
  rename("GeoUID" = "GeoUID.x") |> 
  replace_na(list(
    wd_lines = 0, wd_headways = 0, sat_lines = 0, sat_headways = 0,
    sun_lines = 0, sun_headways = 0, bus_stops = 0
  ))

#Grabbing the travel time matrix data
connection <- DBI::dbConnect(
  drv = RMySQL::MySQL(),
  username = username,
  password = password,
  host = "ccdb-instance-1.cplnwzthenux.us-east-1.rds.amazonaws.com",
  port = 3306,
  dbname = "ccdb"
)
ids <- paste0(paste0("'", laval_db$GeoUID, "'"), collapse = ", ")
ttm <- DBI::dbGetQuery(connection, sprintf("SELECT * FROM ttm_foot_DB WHERE `from` IN (%s)", ids))
DBI::dbDisconnect(connection)

#Filtering for under 7 minutes
ttm_7 <- ttm |> 
  filter(travel_seconds <= 420)

#Preparing extra rows for the combined data
laval_pre_ttm <- laval_bus |> 
  distinct(GeoUID) |> 
  mutate(to = GeoUID)

#Joining and summarizing the data
laval_ttm <- laval_bus |> 
  left_join(ttm_7, by = c("GeoUID" = "from")) |> 
  bind_rows(laval_pre_ttm) |> 
  select(-Population, -wd_headways, -wd_lines, -sat_headways, -sat_lines,
         -sun_headways, -sun_lines, -bus_stops, -travel_seconds) |> 
  left_join(as.data.frame(laval_bus), by = c("to" = "GeoUID")) |> 
  select(-geometry.y) |> 
  rename("geometry" = "geometry.x") |> 
  group_by(GeoUID) |> 
  summarize(weekday_headways = sum(wd_headways, na.rm = TRUE),
            weekday_lines = sum(wd_lines, na.rm = TRUE),
            satday_headways = sum(sat_headways, na.rm = TRUE),
            satday_lines = sum(sat_lines, na.rm = TRUE),
            sunday_headways = sum(sun_headways, na.rm = TRUE),
            sunday_lines = sum(sun_lines, na.rm = TRUE),
            bus_stop = sum(bus_stops, na.rm = TRUE))

#Finding proper breaks
list(classInt::classIntervals(laval_ttm$weekday_headways, n = 5, style = "jenks")$brks)
list(classInt::classIntervals(laval_ttm$weekday_lines, n = 5, style = "jenks")$brks)
list(classInt::classIntervals(laval_ttm$bus_stop, n = 5, style = "jenks")$brks)

#Mapping frequency for weekdays
ggplot(data = laval_ttm) +
  geom_sf(data = mtlcma_sf, color = "black", fill = "#FCFCFC") +
  geom_sf(aes(fill = cut(weekday_headways, breaks = c(0, 258, 590, 1105, 1996, 3785),
                         labels = c("< 258", "258-590", "590-1105", "1105-1996", "> 1996"))), color = NA) +
  geom_sf(data = laval_csd, color = "black", fill = NA) +
  scale_fill_manual(values = curbcut_scale, na.value = curbcut_na) +
  labs(title = "STL fréquence des passages d'autobus 2024",
       fill = "Fréquence totale des passages de bus") +
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

#Mapping line density for weekdays
ggplot(data = laval_ttm) +
  geom_sf(data = mtlcma_sf, color = "black", fill = "#FCFCFC") +
  geom_sf(aes(fill = cut(weekday_lines, breaks = c(0, 9, 22, 45, 86, 150),
                         labels = c("< 9", "9-22", "22-45", "45-86", "> 86"))), color = NA) +
  geom_sf(data = laval_csd, color = "black", fill = NA) +
  scale_fill_manual(values = curbcut_scale, na.value = curbcut_na) +
  labs(title = "STL densité du nombre de lignes d'autobus 2024",
       fill = "Densité du nombre de lignes d'autobus") +
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

#Mapping bus stops
ggplot(data = laval_ttm) +
  geom_sf(data = mtlcma_sf, color = "black", fill = "#FCFCFC") +
  geom_sf(aes(fill = cut(bus_stop, breaks = c(0, 4, 8, 13, 32, 64),
                         labels = c("< 4", "4-8", "8-13", "13-32", "> 32"))), color = NA) +
  geom_sf(data = laval_csd, color = "black", fill = NA) +
  scale_fill_manual(values = curbcut_scale, na.value = curbcut_na) +
  labs(title = "Nombre d'arrêts de bus accessibles 2024",
       fill = "Nombre d'arrêts de bus accessibles") +
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
