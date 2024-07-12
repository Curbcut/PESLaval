#Loading up libraries
source("R/01_startup.R")
library(gbfs)
library(sf)
library(tidytransit)
library(osmdata)
library(tidygeocoder)
library(RMySQL)
library(classInt)
library(ggnewscale)
library(cowplot)
library(extrafont)

#Setting up the cancensus api key
set_cancensus_api_key("CensusMapper_4308d496f011429cf814385050f083dc", install = TRUE)

#Setting up cache path for faster loading (edit for your own folder)
set_cancensus_cache_path("D:/McGill/can_cache", install = TRUE, overwrite = TRUE)

#Importing Fonts
font_import()
loadfonts(device = "win")

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

#Import 15 minute walking TTM and modifying it with necessary rows
ttm_walk_15_p <- read.csv("D://McGill/can_cache/walk15.csv") |> 
  select(-X, -travel_seconds) |> 
  mutate(across(everything(), as.character)) |> 
  rename("GeoUID" = "from")

ttm_walk_15 <- laval_db |> 
  st_drop_geometry() |> 
  select(GeoUID) |> 
  mutate(to = GeoUID) |> 
  bind_rows(ttm_walk_15_p) |> 
  arrange(GeoUID)

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

# Bike Map -------------------------------------------------------------
#Grabbing all bike lanes within the Laval bound box
cycle_osm <- opq(bbox = laval_bbox, timeout = 300) |> 
  add_osm_feature(key = "highway", value = "cycleway") |> 
  osmdata_sf()

#Keeping only the bike lanes within Laval
cycle_lvl <- cycle_osm$osm_lines |> 
  st_intersection(laval_ct)

#Grabbing all Bixi station information
bixi_stations <- get_station_information(city = "Montreal")

#Converting the bixi station information into a usable coordinate system
bixi_sf <- st_as_sf(bixi_stations, coords = c("lon", "lat"), crs = 4326) |> 
  filter(name != "cpatel") |> 
  st_intersection(laval_ct)

#Importing bike comfort csv
bics <- read.csv("D://McGill/can_cache/CAN_BICS_metric_Jan_2022.csv") |> 
  mutate("dauid" = as.character(dauid))

#Binding can-BICs score to Laval DAs
bike_comfort <- laval_da |> 
  left_join(bics, by = join_by("GeoUID" == "dauid"))

# Mapping out the bike map
ggplot(data = bike_comfort) +
  geom_sf(data = mtlcma_sf, aes(fill = "mtlcma_sf"), color = "#525252", fill = "#FCFCFC") +
  geom_sf(data = laval_csd, aes(fill = NA), color = "#525252") +
  geom_sf(data = bike_comfort, aes(fill = cut(CBICS_cat, breaks = c(0, 1, 2, 3, 4, 5),
                                              labels = c("Low", "2", "3", "4", "High"))), color = NA) +
  geom_sf(data = cycle_lvl, aes(color = "cycle_lvl"), size = 1.5) +
  geom_sf(data = bixi_sf, aes(color = "bixi_sf"), shape = 15, size = 2) +
  scale_fill_manual(name = "Confort et sécurité des pistes cyclables",
                    values = c("Low" = curbcut_scale[1], "2" = curbcut_scale[2],
                               "3" = curbcut_scale[3], "4" = curbcut_scale[4], 
                               "High" = curbcut_scale[5]),
                    labels = c("Low", "", "", "", "High"),
                    na.value = curbcut_na,
                    guide = guide_legend(label.position = "bottom", title.position = "top", nrow = 1)) +
  scale_color_manual(name = "",
                     values = c("cycle_lvl" = "white", "bixi_sf" = "black"),
                     labels = c("Stations Bixi", "Piste cyclable"),
                     guide = guide_legend(label.position = "bottom", title.position = "top", nrow = 1)) +
  labs(title = "Carte et score vélo de Laval 2024") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        legend.position = "bottom", panel.grid = element_blank(),
        panel.background = element_rect(fill = "#525252"), legend.spacing.x = unit(-0.05, "cm"),
        legend.key.width = unit(1.95, "cm"), legend.background = element_blank(),
        legend.box.background = element_blank()) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax)) +
  guides(fill = guide_legend(order = 1, label.position = "bottom", title.position = "top", nrow = 1),
    color = guide_legend(order = 2, label.position = "bottom", title.position = "top", nrow = 1))

# Charging Stations -------------------------------------------------------
charging_osm <- opq(bbox = laval_bbox, timeout = 300) |> 
  add_osm_feature(key = "amenity", value = "charging_station") |> 
  osmdata_sf()

charging_lvl <- charging_osm$osm_points |> 
  st_intersection(laval_ct)

ggplot() +
  geom_sf(data = mtlcma_sf, color = "black", fill = "#FCFCFC") +
  geom_sf(data = laval_csd, color = "black", fill = NA) +
  geom_sf(data = charging_lvl, color = "darkred", size = 1) +
  coord_sf() +
  theme_minimal() +
  labs(title = "Bornes de recharge pour véhicules électriques à Laval 2024") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.background = element_rect(fill = "#525252")) +
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

#Importing GEOADMIN_POINT_VENTE.shp from the data folder and 
#assigning each row their accessibility value
vending_sf <- st_read("D://McGill/can_cache/Points_de_vente_shp/GEOADMIN_POINT_VENTE.shp") |> 
  st_transform(crs = 4326) |> 
  mutate(accessible = case_when(
    ADRESSE %in% full_acc ~ "Accessible",
    ADRESSE %in% partial_acc ~ "Partiellement accessible",
    ADRESSE %in% no_acc ~ "Non accessible",
    TRUE ~ NA_character_)) |> 
  select(NOM, accessible) |> 
  drop_na() |> 
  mutate(accessible = factor(accessible, levels = c("Accessible", "Partiellement accessible",
                                                    "Non accessible")))

#Converting vending points to shapefile and then joining
vending_join <- st_join(laval_db, vending_sf) |> 
  st_drop_geometry() |> 
  select(GeoUID, accessible) |> 
  mutate(accessible = ifelse(is.na(accessible), NA, 1)) |> 
  filter(accessible == 1)

#Joining the 15 minute TTM with the data
vending_walk <- ttm_walk_15 |> 
  left_join(vending_join, by = join_by("to" == "GeoUID")) |> 
  filter(accessible == 1) |> 
  group_by(GeoUID) |> 
  summarize(sum_accessible = sum(accessible, na.rm = TRUE)) |> 
  rename("accessible" = "sum_accessible")
  #mutate(accessible = replace_na(accessible, 0))

#Calculating accessibility to vending locations by DB
vending_accessibility <- laval_db |> 
  left_join(vending_walk, by = "GeoUID") |> 
  mutate(accessible = replace_na(accessible, 0)) |> 
  mutate(accessible_cat = factor(case_when(
    accessible == 0 ~ "0",
    accessible == 1 ~ "1",
    accessible >= 2 ~ "2+"
  )))

ggplot() + 
  geom_sf(data = mtlcma_sf, fill = "#FCFCFC", color = NA) +
  geom_sf(data = vending_accessibility, aes(fill = factor(accessible_cat)), color = NA) +
  scale_fill_manual(values = c("0" = "#C4CDE1", 
                               "1" = "#6C83B5",
                               "2+" = "#2B3448"),
                    na.value = curbcut_na,
                    name = "Nombre de points de vente accessibles") +
  labs(title = "Accessibilité aux points de vente STL 2024") +
  new_scale_color() +
  geom_sf(data = vending_sf, aes(color = accessible)) +
  scale_color_manual(values = c("Accessible" = "#0096FF", 
                                "Partiellement accessible" = "#FFB000", 
                                "Non accessible" = "#DC267F",
                                "Accessibilité inconnue" = "grey"),
                     name = "Points de vente") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        legend.position = "bottom", panel.grid = element_blank(),
        panel.background = element_rect(fill = "#525252"),
        legend.direction = "vertical",  # Vertical orientation for legends
        legend.box = "vertical",        # Stack legends vertically
        legend.key.height = unit(1.25, "lines"),  # Adjust key height for both legends
        legend.key.width = unit(1.25, "lines"),   # Adjust key width for both legends
        legend.title.align = 0.5,
        legend.spacing.y = unit(0.1, "cm")) +
  guides(
    fill = guide_legend(order = 1, ncol = 3, title.position = "top",
                        title.hjust = 0.5,
                        label.position = "bottom",
                        label.hjust = 0.5),
    color = guide_legend(order = 2, ncol = 3)
  ) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

# Bus Data ----------------------------------------------------------------
#Grabbing necessary STL data
stl_stops <- stl_gtfs$stops
stl_stoptimes <- stl_gtfs$stop_times
stl_trips <- stl_gtfs$trips
stl_routes <- stl_gtfs$routes

#Filtering only for weekday trips between 7 and 9 AM
weekday_trips <- stl_stoptimes |> 
  filter(trip_id %in% (stl_trips |> 
                         filter(service_id == "JUIN24SEM") |> 
                         pull(trip_id))) |> 
  mutate(arrival_time = hms(arrival_time)) |> 
  filter(arrival_time >= hms("07:00:00") & arrival_time <= hms("09:00:00"))

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

#Weekday stops between 7 and 9 AM
weekday_stops <- weekday_trips |> 
  distinct(stop_id, .keep_all = TRUE) |> 
  select(stop_id) |> 
  left_join(stl_stops, by = "stop_id")

#Binding the weekday data together with bus stops and converting it into a shapefile
lvl_stops_sf <- weekday_stops |> 
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
  username = Sys.getenv("DBI_USERNAME"),
  password = Sys.getenv("DBI_PASSWORD"),
  host = "ccdb-instance-1.cplnwzthenux.us-east-1.rds.amazonaws.com",
  port = 3306,
  dbname = "ccdb")
ids <- paste0(paste0("'", laval_db$GeoUID, "'"), collapse = ", ")
ttm_7 <- DBI::dbGetQuery(connection, sprintf("SELECT * FROM ttm_foot_DB WHERE `from` IN (%s)", ids)) |> 
  filter(travel_seconds <= 420)
ttm_walk_15 <- DBI::dbGetQuery(connection, sprintf("SELECT * FROM ttm_foot_DB WHERE `from` IN (%s)", ids)) |> 
  filter(travel_seconds <= 900)
DBI::dbDisconnect(connection)

#Writing and importing the ttm data so we don't have to call from the API every time
write.csv(ttm_7, "D://McGill/can_cache/ttm.csv")
write.csv(ttm_walk_15, "D://McGill/can_cache/walk15.csv")
ttm_7 <- read.csv("D://McGill/can_cache/ttm.csv") |> 
  select(-X, -travel_seconds) |> 
  mutate(across(everything(), as.character)) |> 
  rename("GeoUID" = "from")

#Creating extra rows so each DB can access their own DB and binding to ttm7
laval_ttm <- laval_db |> 
  st_drop_geometry() |> 
  select(GeoUID) |> 
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
  geom_sf(aes(fill = cut(stop_count, breaks = c(0, 7, 17, 34, 68, 123),
                         labels = c("< 7", "7-17", "17-34", "34-68", "> 68"))), color = NA) +
  geom_sf(data = laval_csd, color = "black", fill = NA) +
  scale_fill_manual(values = curbcut_scale, na.value = curbcut_na) +
  labs(title = "Nombre d’arrêts d’autobus STL accessibles à Laval*",
       subtitle = "*À moins de 7 minutes à pied entre 7h et 9h en semaine",
       fill = "Nombre total d'arrêts de bus accessibles") +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
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
  geom_sf(aes(fill = cut(route_count, breaks = c(0, 3, 7, 16, 29, 38),
                         labels = c("< 3", "3-7", "7-16", "16-29", "> 29"))), color = NA) +
  geom_sf(data = laval_csd, color = "black", fill = NA) +
  scale_fill_manual(values = curbcut_scale, na.value = curbcut_na) +
  labs(title = "Nombre de lignes d'autobus STL accessibles à Laval 2024*",
       subtitle = "*À moins de 7 minutes à pied entre 7h et 9h en semaine",
       fill = "Nombre total de lignes de bus accessibles") +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
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
  geom_sf(aes(fill = cut(headway_count, breaks = c(0, 141, 420, 1231, 2424, 4317),
                         labels = c("< 141", "141-420", "420-1231", "1231-2424", "> 2424"))), color = NA) +
  geom_sf(data = laval_csd, color = "black", fill = NA) +
  scale_fill_manual(values = curbcut_scale, na.value = curbcut_na) +
  labs(title = "Nombre de parcours d'autobus STL accessibles à Laval 2024*",
       subtitle = "*À moins de 7 minutes à pied entre 7h et 9h en semaine",
       fill = "Nombre total de parcours d'autobus accessibles") +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.justification = "center",
        panel.background = element_rect(fill = "#525252")) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5,
                             barwidth = 1, barheight = 1, nrow = 1)) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

test <- filter(laval_accessibility, laval_accessibility$stop_count == 0)
