#Loading up libraries
source("R/01_startup.R")

#Grabbing the shapefile for the Montreal CMA
mtlcma_sf <- cancensus::get_census(dataset = "CA21", 
                                   regions = list(CSD = 24462), 
                                   level = "CMA", 
                                   geo_format = "sf")
laval_db <- cancensus::get_census(dataset = "CA21", 
                                  regions = list(CSD = 2465005), 
                                  level = "DB", 
                                  geo_format = "sf")

#Import 7 minute walking TTM and modifying it with necessary rows
ttm_walk_7 <- ttm(under_x_minutes = 7)


# Bus Stop Location -------------------------------------------------------
#Importing GTFS data. Available in the data folder under justin
stl_gtfs <- tidytransit::read_gtfs("https://www.stlaval.ca/datas/opendata/GTF_STL.zip")

bus_stops <- sf::st_as_sf(stl_gtfs$stops, coords = c("stop_lon", "stop_lat"), crs = 4326)

bus_stops_cleaned <- sf::st_as_sf(stl_gtfs$stops, coords = c("stop_lon", "stop_lat"), crs = 4326) |> 
  nrow() |> 
  convert_number_noround()

# Plot accessibility to bus stops in 7 minutes walk
bs_db_int <- sf::st_intersects(bus_stops, laval_db["GeoUID"], prepared = TRUE)
bus_stops$GeoUID <- sapply(bs_db_int, \(x) {
  ID <- laval_db$GeoUID[x]
  if (length(ID) == 0) return(NA)
  ID
}, simplify = TRUE)

access_busstops <- merge(ttm_walk_7, bus_stops[c("GeoUID", "stop_id")], by.x = "to", by.y = "GeoUID")
access_busstops <- sf::st_drop_geometry(access_busstops)
access_busstops <- unique(access_busstops[c("from", "stop_id")])

access_busstops <- table(access_busstops$from)

access_busstops <- 
  tibble::tibble(GeoUID = names(access_busstops),
                 busstops = as.vector(access_busstops))

no_spots <- laval_db$GeoUID[!laval_db$GeoUID %in% access_busstops$GeoUID]
access_busstops <- rbind(access_busstops, tibble::tibble(GeoUID = no_spots, busstops = 0))
access_busstops <- tibble::as_tibble(access_busstops)

access_busstops <- cc.buildr::merge(access_busstops, laval_db[c("GeoUID", "Population")])

#Plotting the bus stops
labels <- c("0", "1-4", "5-8", "9-12", "13+")

# Add our bins in the data
access_busstops <- add_bins(df = access_busstops,
                               variable = "busstops",
                               breaks = c(-Inf, 0.0001, 4.1, 8.1, 12.1, Inf),
                               labels = labels
)

# # Union the features so the polygons don't show their borders. Might revisit
# # with the addition of streets!
# t <- Reduce(rbind,
#             split(access_busstops, access_busstops$binned_variable) |>
#               lapply(\(x) {
#                 out <- tibble::tibble(x$binned_variable)
#                 out$geometry <- sf::st_union(x)
#                 sf::st_as_sf(out, crs = 4326)[1, ]
#               })
# ) |> sf::st_as_sf()
# names(t)[1] <- "binned_variable"

bus_stops_map <- ggplot(data = access_busstops) +
  gg_cc_tiles +
  geom_sf(aes(fill = binned_variable), color = "transparent") +
  scale_fill_manual(values = curbcut_colors$left_5$fill[c(2:6)],
                    name = "Nombre d'arrêts d'autobus accessibles",
                    labels = labels,
                    guide = guide_legend(title.position = "top",
                                         label.position = "bottom", nrow = 1)) +
  geom_sf(data = bus_stops, color = color_theme("greenurbanlife"),
          size = 0.2, alpha = 0.5) +
  gg_cc_theme

ggplot2::ggsave(filename = here::here("output/axe3/bus_stops_map.pdf"), 
                plot = bus_stops_map, width = 9, height = 6)

# Values
no_bus_stops <- sum(access_busstops$Population[access_busstops$busstops == 0])
no_bus_stops_pct <- convert_pct(no_bus_stops / sum(access_busstops$Population))
no_bus_stops <- convert_number(no_bus_stops)
one_two_bus_stops <- sum(access_busstops$Population[access_busstops$busstops %in% c(1,2)])
one_two_bus_stops_pct <- convert_pct(one_two_bus_stops / sum(access_busstops$Population))
one_two_bus_stops <- convert_number(one_two_bus_stops)

# Bus routes accessibles ---------------------------------------------------

# Filter stop times using the weekday service
weekday_service <- stl_gtfs$calendar$service_id[stl_gtfs$calendar$wednesday == 1]
trip_id <- stl_gtfs$trips$trip_id[stl_gtfs$trips$service_id == weekday_service]
stop_times <- stl_gtfs$stop_times[stl_gtfs$stop_times$trip_id %in% trip_id, ]
stop_times <- stop_times[stop_times$arrival_time > hms::as_hms("07:00:00") & 
                           stop_times$arrival_time < hms::as_hms("09:00:00"), ]

stop_times <- cc.buildr::merge(stop_times, sf::st_as_sf(stl_gtfs$stops, 
                                                        coords = c("stop_lon", "stop_lat"), 
                                                        crs = 4326)["stop_id"])
st_db_int <- sf::st_intersects(stop_times, laval_db["GeoUID"], prepared = TRUE)
stop_times$GeoUID <- sapply(st_db_int, \(x) {
  ID <- laval_db$GeoUID[x]
  if (length(ID) == 0) return(NA)
  ID
}, simplify = TRUE)
stop_times <- stop_times[c("trip_id", "GeoUID")]
# Trip_id is assigned to which route_id?
trips <- cc.buildr::merge(stop_times, stl_gtfs$trips[c("route_id", "trip_id")])

trips <- cc.buildr::merge(ttm_walk_7, trips, by.x = "to", by.y = "GeoUID")
trips <- sf::st_drop_geometry(trips)
trips <- unique(trips[c("from", "route_id")])

trips <- table(trips$from)

trips <- 
  tibble::tibble(GeoUID = names(trips),
                 trips = as.vector(trips))

no_spots <- laval_db$GeoUID[!laval_db$GeoUID %in% trips$GeoUID]
trips <- rbind(trips, tibble::tibble(GeoUID = no_spots, trips = 0))
trips <- tibble::as_tibble(trips)

# Plot lines
trips <- cc.buildr::merge(trips, laval_db[c("GeoUID", "Population")])

#Plotting the bus stops
labels <- c("0", "1-2", "3-4", "5-6", "7+")

# Add our bins in the data
trips <- add_bins(df = trips,
                       variable = "trips",
                       breaks = c(-Inf, 0.0001, 2.1, 4.1, 6.1, Inf),
                       labels = labels
)

# # Union the features so the polygons don't show their borders. Might revisit
# # with the addition of streets!
# t <- Reduce(rbind,
#             split(trips, trips$binned_variable) |>
#               lapply(\(x) {
#                 out <- tibble::tibble(x$binned_variable)
#                 out$geometry <- sf::st_union(x)
#                 sf::st_as_sf(out, crs = 4326)[1, ]
#               })
# ) |> sf::st_as_sf()
# names(t)[1] <- "binned_variable"

bus_lines_map <- ggplot(data = trips) +
  gg_cc_tiles +
  geom_sf(aes(fill = binned_variable), color = "transparent") +
  scale_fill_manual(values = curbcut_colors$left_5$fill[c(2:6)],
                    name = "Nombre de trajets d'autobus accessibles",
                    labels = labels,
                    guide = guide_legend(title.position = "top",
                                         label.position = "bottom", nrow = 1)) +
  gg_cc_theme

ggplot2::ggsave(filename = here::here("output/axe3/bus_stops_map.pdf"), 
                plot = bus_stops_map, width = 9, height = 6)

# Values
no_bus_lines <- sum(trips$Population[trips$trips == 0])
no_bus_lines_pct <- convert_pct(no_bus_lines / sum(trips$Population))
no_bus_lines <- convert_number(no_bus_lines)
one_two_bus_lines <- convert_number(sum(trips$Population[trips$trips %in% c(1,2)]))
one_two_bus_lines_pct <- convert_pct(sum(trips$Population[trips$trips %in% c(1,2)]) / sum(trips$Population))

# Bus vehicles accessibles ---------------------------------------------------

# Filter stop times using the weekday service
weekday_service <- stl_gtfs$calendar$service_id[stl_gtfs$calendar$wednesday == 1]
trip_id <- stl_gtfs$trips$trip_id[stl_gtfs$trips$service_id == weekday_service]
stop_times <- stl_gtfs$stop_times[stl_gtfs$stop_times$trip_id %in% trip_id, ]

stop_times <- stop_times[stop_times$arrival_time > hms::as_hms("07:00:00") & 
                           stop_times$arrival_time < hms::as_hms("09:00:00"), ]

stop_times <- cc.buildr::merge(stop_times, sf::st_as_sf(stl_gtfs$stops, 
                                                    coords = c("stop_lon", "stop_lat"), 
                                                    crs = 4326)["stop_id"])
st_db_int <- sf::st_intersects(stop_times, laval_db["GeoUID"], prepared = TRUE)
stop_times$GeoUID <- sapply(st_db_int, \(x) {
  ID <- laval_db$GeoUID[x]
  if (length(ID) == 0) return(NA)
  ID
}, simplify = TRUE)
stop_times <- stop_times[c("trip_id", "GeoUID")]
stop_times <- cc.buildr::merge(ttm_walk_7, stop_times, by.x = "to", by.y = "GeoUID")
stop_times <- sf::st_drop_geometry(stop_times)
stop_times <- unique(stop_times[c("from", "trip_id")])

stop_times <- table(stop_times$from)

stop_times <- 
  tibble::tibble(GeoUID = names(stop_times),
                 vehicles = as.vector(stop_times))

no_spots <- laval_db$GeoUID[!laval_db$GeoUID %in% stop_times$GeoUID]
stop_times <- rbind(stop_times, tibble::tibble(GeoUID = no_spots, vehicles = 0))
stop_times <- tibble::as_tibble(stop_times)

# Plot lines
stop_times <- cc.buildr::merge(stop_times, laval_db[c("GeoUID", "Population")])

#Plotting the bus stops
labels <- c("0", "1-10", "11-25", "26-50", "50+")

# Add our bins in the data
stop_times <- add_bins(df = stop_times,
                            variable = "vehicles",
                            breaks = c(-Inf, 0.0001, 10.1, 25.1, 50.1, Inf),
                            labels = labels
)

# # Union the features so the polygons don't show their borders. Might revisit
# # with the addition of streets!
# t <- Reduce(rbind,
#             split(stop_times, stop_times$binned_variable) |>
#               lapply(\(x) {
#                 out <- tibble::tibble(x$binned_variable)
#                 out$geometry <- sf::st_union(x)
#                 sf::st_as_sf(out, crs = 4326)[1, ]
#               })
# ) |> sf::st_as_sf()
# names(t)[1] <- "binned_variable"

bus_trips_map <- ggplot(data = stop_times) +
  gg_cc_tiles +
  geom_sf(aes(fill = binned_variable), color = "transparent") +
  scale_fill_manual(values = curbcut_colors$left_5$fill[c(2:6)],
                    name = "Nombre de véhicules (autobus) accessibles",
                    labels = labels,
                    guide = guide_legend(title.position = "top",
                                         label.position = "bottom", nrow = 1)) +
  geom_sf(data = laval_sectors, fill = "transparent", color = "black") +
  gg_cc_theme

ggplot2::ggsave(filename = here::here("output/axe3/bus_trips_map.pdf"), 
                plot = bus_trips_map, width = 5.5, height = 4)

# Values
no_bus_trips <- sum(stop_times$Population[stop_times$vehicles == 0])
no_bus_trips_pct <- convert_pct(no_bus_trips/ sum(stop_times$Population))
no_bus_trips <- convert_number(no_bus_trips)
twenty_min_bus_trip <- convert_number(sum(stop_times$Population[stop_times$vehicles < 120/20]))
twenty_min_bus_trip_prop <- convert_pct(sum(stop_times$Population[stop_times$vehicles < 120/20]) / sum(stop_times$Population))
five_min_bus_trip <- convert_number(sum(stop_times$Population[stop_times$vehicles > 120/5]))
five_min_bus_trip_prop <- convert_pct(sum(stop_times$Population[stop_times$vehicles > 120/5]) / sum(stop_times$Population))

# POIs accessible ---------------------------------------------------------

# # Get all points of interest
# pois <- cc.data::bucket_read_object_zip_shp(object = "poi/poi_2022_1.zip", bucket = "curbcut.amenities")
# pois2 <- cc.data::bucket_read_object_zip_shp(object = "poi/poi_2022_2.zip", bucket = "curbcut.amenities")
# pois <- rbind(pois, pois2)
# qs::qsave(pois, file = "data/axe3/pois.qs")
pois <- qs::qread("data/axe3/pois.qs")


# Travel time matrix in transit
transit_ttm <- ttm_DA("transit_pwd")

# Filer the CMA
CMA <- cancensus::get_census("CA21", regions = list(CMA = 24462), level = "CMA",
                             geo_format = "sf")
pois <- sf::st_transform(pois, crs = sf::st_crs(CMA))
pois <- sf::st_filter(pois, CMA)

# Intersects number of POIs per DA
CMA_da <- cancensus::get_census("CA21", regions = list(CMA = 24462), level = "DA",
                                geo_format = "sf")
intersects_nb <- sf::st_intersects(CMA_da, pois)
CMA_da$pois <- lengths(intersects_nb)
DA_poi <- CMA_da[c("GeoUID", "pois")]

# Only keep transit times under 20 minutes
transit_ttm <- transit_ttm[transit_ttm$travel_seconds <= 20*60, ]


DA_poi <- cc.buildr::merge(transit_ttm, DA_poi, by.x = "to", by.y = "GeoUID")

DA_poi <- aggregate(pois ~ from, data = DA_poi, FUN = sum)


no_spots <- laval_db$GeoUID[!laval_db$GeoUID %in% DA_poi$GeoUID]
DA_poi <- rbind(DA_poi, tibble::tibble(from = no_spots, pois = 0))
DA_poi <- tibble::as_tibble(DA_poi)

DA_poi <- DA_poi[DA_poi$from %in% laval_da$GeoUID, ]
DA_poi <- cc.buildr::merge(DA_poi, laval_da[c("GeoUID")], 
                           by.x = "from", by.y = "GeoUID")


labels <- c("0-15K", "15K-30K", "30K-45K", "45K-60K", "60+")

# Add our bins in the data
DA_poi <- add_bins(df = DA_poi,
                   variable = "pois",
                   breaks = c(0, 15000, 30000, 45000, 60000, Inf),
                   labels = labels
)

# # Union the features so the polygons don't show their borders. Might revisit
# # with the addition of streets!
# t <- Reduce(rbind,
#             split(DA_poi, DA_poi$binned_variable) |>
#               lapply(\(x) {
#                 out <- tibble::tibble(x$binned_variable)
#                 out$geometry <- sf::st_union(x)
#                 sf::st_as_sf(out, crs = 4326)[1, ]
#               })
# ) |> sf::st_as_sf()
# names(t)[1] <- "binned_variable"

poi_map <- DA_poi |> 
  ggplot() +
  gg_cc_tiles +
  geom_sf(aes(fill = binned_variable), color = "transparent") +
  scale_fill_manual(values = curbcut_colors$left_5$fill[c(2:6)],
                    name = "Nombre de lieux d'intérêts accessibles",
                    labels = labels,
                    guide = guide_legend(title.position = "top",
                                         label.position = "bottom", nrow = 1)) +
  gg_cc_theme +
  theme(legend.spacing.x = unit(2, 'cm'),
        legend.spacing.y = unit(1, 'cm'))

ggplot2::ggsave(filename = here::here("output/axe3/bus_poi_map.pdf"), 
                plot = poi_map, width = 5.5, height = 4)

#R Markdown Numbers
poi_total <- convert_number(nrow(pois))

poi_laval <- pois |> 
  mutate(intersect = map_lgl(st_intersects(pois, laval_csd), ~ length(.) > 0)) |>
  filter(intersect == TRUE) |> 
  mutate(number = 1) |> 
  summarise(number = sum(number)) |> 
  mutate(number = convert_number(number)) |> 
  pull(number)

# Transit usage -----------------------------------------------------------

# flows <- tibble::as_tibble(read.csv("data/axe3/mtl_flow_21.csv"))
# flows <- flows[c("POR", "Total...Main.mode.of.commuting", "Public.transit")]
# flows$POR <- as.character(flows$POR)
# 
# flows$POR <- sapply(flows$POR, \(x) {
#   if (grepl("\\.\\d{2}", x)) return(x)
#   if (grepl("\\.\\d{1}", x)) return(paste0(x, "0"))
#   paste0(x, ".00")
# })
# 
# flows_pr <- aggregate(Public.transit ~ POR, data = flows, FUN = sum)
# flows_tot <- aggregate(Total...Main.mode.of.commuting ~ POR, data = flows, FUN = sum)
# 
# flows <- tibble::as_tibble(merge(flows_tot, flows_pr))
# flows$density <- flows$Public.transit / flows$Total...Main.mode.of.commuting

flows <- cancensus::get_census(dataset = "CA21", 
                            regions = list(CSD = 2465005), 
                            level = "DA", 
                            geo_format = "sf",
                            vectors = c(total = "v_CA21_7632", public_transit = "v_CA21_7644"))
flows$density <- flows$public_transit / flows$total

#Plotting the bus stops
labels <- c("0", "0 - 5%", "5% - 10%", "10% - 15%", "+15%")

# Add our bins in the data
flows <- add_bins(df = flows,
                   variable = "density",
                   breaks = c(-Inf, 0.00000001, 0.050001, 0.100001, 0.1500001, Inf),
                   labels = labels
)

lvls <- levels(flows$binned_variable)
lvls <- c(lvls[length(lvls)], lvls[-length(lvls)])

# # Union the features so the polygons don't show their borders. Might revisit
# # with the addition of streets!
# t <- Reduce(rbind,
#             split(flows, flows$binned_variable) |>
#               lapply(\(x) {
#                 out <- tibble::tibble(x$binned_variable)
#                 out$geometry <- sf::st_union(x)
#                 sf::st_as_sf(out, crs = 4326)[1, ]
#               })
# ) |> sf::st_as_sf()
# names(t)[1] <- "binned_variable"

transit_usage_map <- ggplot(data = flows) +
  gg_cc_tiles +
  geom_sf(aes(fill = binned_variable), color = "transparent") +
  scale_fill_manual(values = curbcut_colors$left_5$fill[c(1:6)],
                    na.value = curbcut_colors$left_5$fill[1],
                    breaks = lvls,  # NA first
                    labels = lvls,  # NA label first
                    name = "Transport en commun pour les déplacements domicile-travail (%)",
                    guide = guide_legend(title.position = "top",
                                         label.position = "bottom", nrow = 1)) +
  gg_cc_theme

ggplot2::ggsave(filename = here::here("output/axe3/transit_usage_map.pdf"), 
                plot = transit_usage_map, width = 7.5, height = 6)

# Values
flows <- cancensus::get_census(dataset = "CA21", 
                               regions = list(CSD = 2465005), 
                               level = "CSD", 
                               geo_format = "sf",
                               vectors = c(total = "v_CA21_7632", public_transit = "v_CA21_7644"))
transit_usage <- convert_pct(flows$public_transit / flows$total)

flows_2016 <- cancensus::get_census(dataset = "CA16", 
                               regions = list(CSD = 2465005), 
                               level = "CSD", 
                               geo_format = "sf",
                               vectors = c(total = "v_CA16_5792", public_transit = "v_CA16_5801"))
transit_usage_2016 <- convert_pct(flows_2016$public_transit / flows_2016$total)


# # Charging Stations -------------------------------------------------------
# charging_osm <- opq(bbox = lvlbbox, timeout = 300) |> 
#   add_osm_feature(key = "amenity", value = "charging_station") |> 
#   osmdata_sf()
# 
# charging_lvl <- charging_osm$osm_points |> 
#   st_intersection(laval_ct)
# 
# ggplot() +
#   geom_sf(data = mtlcma_sf, color = "black", fill = "#FCFCFC") +
#   geom_sf(data = laval_csd, color = "black", fill = NA) +
#   geom_sf(data = charging_lvl, color = "darkred", size = 1) +
#   coord_sf() +
#   theme_minimal() +
#   labs(title = "Bornes de recharge pour véhicules électriques à Laval 2024") +
#   theme(plot.title = element_text(hjust = 0.5),
#         axis.text = element_blank(),
#         axis.title = element_blank(), axis.ticks = element_blank(),
#         panel.grid = element_blank(), panel.background = element_rect(fill = "#525252")) +
#   coord_sf(xlim = c(lvlbbox$xmin, lvlbbox$xmax),
#            ylim = c(lvlbbox$ymin, lvlbbox$ymax))
# 
# # Points de vente (transport collectif) -----------------------------------
# #Using the shapefile below and STL_accessibilite-points-vente.pdf and
# #the STL website to determine the level of accessibility
# full_acc <- c("475, boul. de l'Avenir", "600, montée du Moulin", "800b, boul. Chomedey (160)",
#               "275, boul. Samson", "2500, boul. Saint-Martin Est", "165, boul. Curé-Labelle",
#               "2220, boul. des Laurentides")
# partial_acc <- c("10B, rue Dufferin", "555, boul. Samson", "650, rue Principale",
#                  "1263, boul. Jolibourg", "155, boul. de La Concorde Est",
#                  "2397, boul. Curé-Labelle", "4600, boul. Samson", "4650, boul. du Souvenir",
#                  "4219, boul. Samson", "965, boul. Curé-Labelle", "5000, boul. des Laurentides",
#                  "1690, boul. Sainte-Rose", "4425, boul. de La Concorde", "3475, boul. Dagenais",
#                  "5680, boul. des Laurentides", "255, boul. de La Concorde Ouest", "430, boul. Cartier",
#                  "44, boul. des Laurentides", "545, rue Lucien-Paiement", "3000, boul. Le Carrefour",
#                  "580,  boul. Curé-Labelle (suite 10)", "4672, boul. Saint-Martin",
#                  "2955, boul. de La Concorde", "1295, boul. de la Concorde O.", "405, boul. des Laurentides",
#                  "6155, boul. Arthur-Sauvé", "4347, boul. Ste-Rose")
# no_acc <- c("3875, boul. Sainte-Rose", "501, rue Guillemette", "265, 15e Rue",
#             "3667, boul. Lévesque Ouest", "45, boul. Lévesque Est", "3323, boul. de la Concorde E.",
#             "2795, boul. René-Laennec", "5162, de la Fabrique")
# 
# #Importing GEOADMIN_POINT_VENTE.shp from the data folder and 
# #assigning each row their accessibility value
# vending_sf <- st_read("D://McGill/can_cache/Points_de_vente_shp/GEOADMIN_POINT_VENTE.shp") |> 
#   st_transform(crs = 4326) |> 
#   mutate(accessible = case_when(
#     ADRESSE %in% full_acc ~ "Accessible",
#     ADRESSE %in% partial_acc ~ "Partiellement accessible",
#     ADRESSE %in% no_acc ~ "Non accessible",
#     TRUE ~ NA_character_)) |> 
#   select(NOM, accessible) |> 
#   drop_na() |> 
#   mutate(accessible = factor(accessible, levels = c("Accessible", "Partiellement accessible",
#                                                     "Non accessible")))
# 
# #Converting vending points to shapefile and then joining
# vending_join <- st_join(laval_db, vending_sf) |> 
#   st_drop_geometry() |> 
#   select(GeoUID, accessible) |> 
#   mutate(accessible = ifelse(is.na(accessible), NA, 1)) |> 
#   filter(accessible == 1)
# 
# #Joining the 15 minute TTM with the data
# vending_walk <- ttm_walk_7 |> 
#   left_join(vending_join, by = join_by("to" == "GeoUID")) |> 
#   filter(accessible == 1) |> 
#   group_by(GeoUID) |> 
#   summarize(sum_accessible = sum(accessible, na.rm = TRUE)) |> 
#   rename("accessible" = "sum_accessible")
#   #mutate(accessible = replace_na(accessible, 0))
# 
# #Calculating accessibility to vending locations by DB
# vending_accessibility <- laval_db |> 
#   left_join(vending_walk, by = "GeoUID") |> 
#   mutate(accessible = replace_na(accessible, 0)) |> 
#   mutate(accessible_cat = factor(case_when(
#     accessible == 0 ~ "0",
#     accessible == 1 ~ "1",
#     accessible >= 2 ~ "2+"
#   )))
# 
# ggplot() + 
#   geom_sf(data = mtlcma_sf, fill = "#FCFCFC", color = NA) +
#   geom_sf(data = vending_accessibility, aes(fill = factor(accessible_cat)), color = NA) +
#   scale_fill_manual(values = c("0" = "#C4CDE1", 
#                                "1" = "#6C83B5",
#                                "2+" = "#2B3448"),
#                     na.value = curbcut_na,
#                     name = "Nombre de points de vente accessibles") +
#   labs(title = "Accessibilité aux points de vente STL 2024") +
#   new_scale_color() +
#   geom_sf(data = vending_sf, aes(color = accessible)) +
#   scale_color_manual(values = c("Accessible" = "#0096FF", 
#                                 "Partiellement accessible" = "#FFB000", 
#                                 "Non accessible" = "#DC267F",
#                                 "Accessibilité inconnue" = "grey"),
#                      name = "Points de vente") +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
#         axis.title = element_blank(), axis.ticks = element_blank(),
#         legend.position = "bottom", panel.grid = element_blank(),
#         panel.background = element_rect(fill = "#525252"),
#         legend.direction = "vertical",  # Vertical orientation for legends
#         legend.box = "vertical",        # Stack legends vertically
#         legend.key.height = unit(1.25, "lines"),  # Adjust key height for both legends
#         legend.key.width = unit(1.25, "lines"),   # Adjust key width for both legends
#         legend.title.align = 0.5,
#         legend.spacing.y = unit(0.1, "cm")) +
#   guides(
#     fill = guide_legend(order = 1, ncol = 3, title.position = "top",
#                         title.hjust = 0.5,
#                         label.position = "bottom",
#                         label.hjust = 0.5),
#     color = guide_legend(order = 2, ncol = 3)
#   ) +
#   coord_sf(xlim = c(lvlbbox$xmin, lvlbbox$xmax),
#            ylim = c(lvlbbox$ymin, lvlbbox$ymax))

# # Bus Data ----------------------------------------------------------------
# #Grabbing necessary STL data
# stl_stops <- stl_gtfs$stops
# stl_stoptimes <- stl_gtfs$stop_times
# stl_trips <- stl_gtfs$trips
# stl_routes <- stl_gtfs$routes
# 
# #Filtering only for weekday trips between 7 and 9 AM
# weekday_trips <- stl_stoptimes |> 
#   filter(trip_id %in% (stl_trips |> 
#                          filter(service_id == "JUIN24SEM") |> 
#                          pull(trip_id))) |> 
#   mutate(arrival_time = hms(arrival_time)) |> 
#   filter(arrival_time >= hms("07:00:00") & arrival_time <= hms("09:00:00"))
# 
# #Number of vehicle stops per stop on a weekday
# weekday_headways <- weekday_trips |> 
#   group_by(stop_id) |> 
#   summarize(weekday_headways = n())
# 
# #Weekday routes per stop
# weekday_lines <- weekday_trips |> 
#   rowwise() |> 
#   mutate(route_id = str_extract(trip_id, paste(stl_routes$route_id, collapse = "|"))) |> 
#   filter(!is.na(route_id)) |> 
#   distinct(route_id, stop_id) |> 
#   select(stop_id, route_id)
# 
# #Weekday stops between 7 and 9 AM
# weekday_stops <- weekday_trips |> 
#   distinct(stop_id, .keep_all = TRUE) |> 
#   select(stop_id) |> 
#   left_join(stl_stops, by = "stop_id")
# 
# #Binding the weekday data together with bus stops and converting it into a shapefile
# lvl_stops_sf <- weekday_stops |> 
#   select(stop_id, stop_lon, stop_lat) |> 
#   left_join(weekday_lines, by = "stop_id") |> 
#   left_join(weekday_headways, by = "stop_id") |> 
#   mutate(bus_stop = 1) |> 
#   st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)
# 
# #Isolating stop_ids to their DAs
# stops_sf <- st_join(lvl_stops_sf, laval_db, join = st_within) |> 
#   st_drop_geometry() |> 
#   select(GeoUID, stop_id) |> 
#   na.omit()
# 
# #Assigning each bus stop to their respective DBs and then summing up the total number of buses and stop each one has
# laval_bus_pre <- st_join(lvl_stops_sf, laval_db, join = st_within) |> 
#   filter(!is.na(GeoUID)) |> 
#   group_by(GeoUID) |> 
#   summarize(wd_headways = sum(weekday_headways, na.rm = TRUE),
#     bus_stops = sum(bus_stop, na.rm = TRUE), .groups = 'drop')
# 
# #Joining all the data back to all the Laval DBs
# laval_bus <- laval_db |> 
#   st_join(laval_bus_pre) |> 
#   select(GeoUID.x, Population, wd_headways, bus_stops) |> 
#   rename("GeoUID" = "GeoUID.x") |> 
#   replace_na(list(wd_headways = 0, bus_stops = 0))
# 
# #Grabbing the travel time matrix data and filtering for under 7 minutes
# connection <- DBI::dbConnect(
#   drv = RMySQL::MySQL(),
#   username = Sys.getenv("DBI_USERNAME"),
#   password = Sys.getenv("DBI_PASSWORD"),
#   host = "ccdb-instance-1.cplnwzthenux.us-east-1.rds.amazonaws.com",
#   port = 3306,
#   dbname = "ccdb")
# ids <- paste0(paste0("'", laval_db$GeoUID, "'"), collapse = ", ")
# ttm_7 <- DBI::dbGetQuery(connection, sprintf("SELECT * FROM ttm_foot_DB WHERE `from` IN (%s)", ids)) |> 
#   filter(travel_seconds <= 420)
# ttm_walk_7 <- DBI::dbGetQuery(connection, sprintf("SELECT * FROM ttm_foot_DB WHERE `from` IN (%s)", ids)) |> 
#   filter(travel_seconds <= 900)
# DBI::dbDisconnect(connection)
# 
# #Writing and importing the ttm data so we don't have to call from the API every time
# write.csv(ttm_7, "D://McGill/can_cache/ttm.csv")
# write.csv(ttm_walk_7, "D://McGill/can_cache/walk15.csv")
# ttm_7 <- read.csv("D://McGill/can_cache/ttm.csv") |> 
#   select(-X, -travel_seconds) |> 
#   mutate(across(everything(), as.character)) |> 
#   rename("GeoUID" = "from")
# 
# #Creating extra rows so each DB can access their own DB and binding to ttm7
# laval_ttm <- laval_db |> 
#   st_drop_geometry() |> 
#   select(GeoUID) |> 
#   mutate(to = GeoUID) |> 
#   bind_rows(ttm_7) |> 
#   arrange(GeoUID)
# 
# #Calculating number of accessible stops for each DB
# laval_accessibilty_stop <- laval_ttm |> 
#   left_join(stops_sf, by = c("to" = "GeoUID"), relationship = "many-to-many") |> 
#   na.omit() |> 
#   group_by(GeoUID) |> 
#   summarise(stop_count = n())
# 
# #Calculating number of accessible unique routes for each DB
# laval_accessibility_route <- laval_ttm |> 
#   left_join(stops_sf, by = c("to" = "GeoUID"), relationship = "many-to-many") |> 
#   left_join(weekday_lines, by = "stop_id", relationship = "many-to-many") |> 
#   distinct(GeoUID, route_id) |> 
#   na.omit() |> 
#   group_by(GeoUID) |> 
#   summarise(route_count = n())
# 
# #Calculating number of accessible bus runs for each DB
# laval_accessibility_headway <- laval_ttm |> 
#   left_join(stops_sf, by = c("to" = "GeoUID"), relationship = "many-to-many") |>
#   left_join(weekday_trips, by = "stop_id", relationship = "many-to-many") |> 
#   na.omit() |> 
#   group_by(GeoUID) |> 
#   summarise(headway_count = n())
# 
# #Joining data to the original Laval DB shapefile and replacing NAs with 0
# laval_accessibility <- laval_db |> 
#   left_join(laval_accessibilty_stop, by = "GeoUID") |> 
#   left_join(laval_accessibility_route, by = "GeoUID") |> 
#   left_join(laval_accessibility_headway, by = "GeoUID") |> 
#   replace_na(list(headway_count = 0, route_count = 0, stop_count = 0))
# 
# #Finding proper breaks
# list(classInt::classIntervals(laval_accessibility$stop_count, n = 5, style = "jenks")$brks)
# list(classInt::classIntervals(laval_accessibility$route_count, n = 5, style = "jenks")$brks)
# list(classInt::classIntervals(laval_accessibility$headway_count, n = 5, style = "jenks")$brks)
# 
# #Mapping out bus stops
# ggplot(data = laval_accessibility) +
#   geom_sf(data = mtlcma_sf, color = "black", fill = "#FCFCFC") +
#   geom_sf(aes(fill = cut(stop_count, breaks = c(0, 7, 17, 34, 68, 123),
#                          labels = c("< 7", "7-17", "17-34", "34-68", "> 68"))), color = NA) +
#   geom_sf(data = laval_csd, color = "black", fill = NA) +
#   scale_fill_manual(values = curbcut_scale, na.value = curbcut_na) +
#   labs(title = "Nombre d’arrêts d’autobus STL accessibles à Laval*",
#        subtitle = "*À moins de 7 minutes à pied entre 7h et 9h en semaine",
#        fill = "Nombre total d'arrêts de bus accessibles") +
#   theme_minimal() +
#   theme(axis.line = element_blank(), axis.text = element_blank(),
#         axis.title = element_blank(), axis.ticks = element_blank(),
#         plot.subtitle = element_text(hjust = 0.5),
#         panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
#         legend.position = "bottom", legend.justification = "center",
#         panel.background = element_rect(fill = "#525252")) +
#   guides(fill = guide_legend(title.position = "top", title.hjust = 0.5,
#                              barwidth = 1, barheight = 1, nrow = 1)) +
#   coord_sf(xlim = c(lvlbbox$xmin, lvlbbox$xmax),
#            ylim = c(lvlbbox$ymin, lvlbbox$ymax))
# 
# #Mapping out route accessibility
# ggplot(data = laval_accessibility) +
#   geom_sf(data = mtlcma_sf, color = "black", fill = "#FCFCFC") +
#   geom_sf(aes(fill = cut(route_count, breaks = c(0, 3, 7, 16, 29, 38),
#                          labels = c("< 3", "3-7", "7-16", "16-29", "> 29"))), color = NA) +
#   geom_sf(data = laval_csd, color = "black", fill = NA) +
#   scale_fill_manual(values = curbcut_scale, na.value = curbcut_na) +
#   labs(title = "Nombre de lignes d'autobus STL accessibles à Laval 2024*",
#        subtitle = "*À moins de 7 minutes à pied entre 7h et 9h en semaine",
#        fill = "Nombre total de lignes de bus accessibles") +
#   theme_minimal() +
#   theme(axis.line = element_blank(), axis.text = element_blank(),
#         axis.title = element_blank(), axis.ticks = element_blank(),
#         plot.subtitle = element_text(hjust = 0.5),
#         panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
#         legend.position = "bottom", legend.justification = "center",
#         panel.background = element_rect(fill = "#525252")) +
#   guides(fill = guide_legend(title.position = "top", title.hjust = 0.5,
#                              barwidth = 1, barheight = 1, nrow = 1)) +
#   coord_sf(xlim = c(lvlbbox$xmin, lvlbbox$xmax),
#            ylim = c(lvlbbox$ymin, lvlbbox$ymax))
# 
# #Mapping out bus runs accessibility
# ggplot(data = laval_accessibility) +
#   geom_sf(data = mtlcma_sf, color = "black", fill = "#FCFCFC") +
#   geom_sf(aes(fill = cut(headway_count, breaks = c(0, 141, 420, 1231, 2424, 4317),
#                          labels = c("< 141", "141-420", "420-1231", "1231-2424", "> 2424"))), color = NA) +
#   geom_sf(data = laval_csd, color = "black", fill = NA) +
#   scale_fill_manual(values = curbcut_scale, na.value = curbcut_na) +
#   labs(title = "Nombre de parcours d'autobus STL accessibles à Laval 2024*",
#        subtitle = "*À moins de 7 minutes à pied entre 7h et 9h en semaine",
#        fill = "Nombre total de parcours d'autobus accessibles") +
#   theme_minimal() +
#   theme(axis.line = element_blank(), axis.text = element_blank(),
#         axis.title = element_blank(), axis.ticks = element_blank(),
#         plot.subtitle = element_text(hjust = 0.5),
#         panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
#         legend.position = "bottom", legend.justification = "center",
#         panel.background = element_rect(fill = "#525252")) +
#   guides(fill = guide_legend(title.position = "top", title.hjust = 0.5,
#                              barwidth = 1, barheight = 1, nrow = 1)) +
#   coord_sf(xlim = c(lvlbbox$xmin, lvlbbox$xmax),
#            ylim = c(lvlbbox$ymin, lvlbbox$ymax))
# 
# test <- filter(laval_accessibility, laval_accessibility$stop_count == 0)
# 
# # R Markdown --------------------------------------------------------------
# ggplot2::ggsave(filename = here::here("output/axe3/mobility/bus_stops_map.pdf"), 
#                 plot = bus_stops_map, width = 7.5, height = 6)
# ggplot2::ggsave(filename = here::here("output/axe3/mobility/bus_lines_map.pdf"), 
#                 plot = bus_lines_map, width = 7.5, height = 6)
# ggplot2::ggsave(filename = here::here("output/axe3/mobility/bus_trips_map.pdf"), 
#                 plot = bus_trips_map, width = 7.5, height = 6)
# ggplot2::ggsave(filename = here::here("output/axe3/mobility/transit_usage_map.pdf"), 
#                 plot = transit_usage_map, width = 7.5, height = 6)

qs::qsavem(bus_stops_cleaned, no_bus_stops, no_bus_stops_pct, one_two_bus_stops, 
           one_two_bus_stops_pct, bus_stops_map, bus_lines_map, no_bus_lines, 
           no_bus_lines_pct, one_two_bus_lines, one_two_bus_lines_pct, 
           bus_trips_map, no_bus_trips, no_bus_trips_pct, twenty_min_bus_trip, 
           twenty_min_bus_trip_prop, five_min_bus_trip, five_min_bus_trip_prop, 
           poi_map, poi_total, poi_laval, transit_usage_map, transit_usage, 
           transit_usage_2016, file = "data/axe3/transport.qsm")
