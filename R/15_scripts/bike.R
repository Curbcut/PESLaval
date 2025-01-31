source("R/01_startup.R")

laval_da <- cancensus::get_census(dataset = "CA21", 
                                   regions = list(CSD = 2465005), 
                                   level = "DA", 
                                   geo_format = "sf")
laval_csd <- cancensus::get_census(dataset = "CA21", 
                                   regions = list(CSD = 2465005), 
                                   level = "CSD", 
                                   geo_format = "sf")

# Bike Map -------------------------------------------------------------
#Grabbing all bike lanes coming from donnees quebec
cycle <- sf::st_read("data/axe3/piste-cyclables/pistes_cyclables_et_pietonnieres.shp")
cycle <- sf::st_transform(cycle, crs = sf::st_crs(laval_csd))

bixi_stations <- jsonlite::read_json("https://gbfs.velobixi.com/gbfs/en/station_information.json")$data$stations
bixi_stations <- purrr::map_dfr(bixi_stations, list)

#Converting the bixi station information into a usable coordinate system
bixi_sf <- sf::st_as_sf(bixi_stations, coords = c("lon", "lat"), crs = 4326)

#Keeping only bixi stations in Laval
bixi_sf_laval <- sf::st_filter(bixi_sf, laval_csd)

#Importing bike comfort csv
bics <- read.csv("data/axe3/CAN_BICS_metric_Jan_2022.csv") |> 
  mutate("dauid" = as.character(dauid))

#Binding can-BICs score to Laval DAs
bike_comfort <- laval_da |> 
  left_join(bics, by = join_by("GeoUID" == "dauid"))

bike_comfort <- sf::st_transform(bike_comfort, crs = sf::st_crs(laval_csd))


# t <- Reduce(rbind,
#             split(bike_comfort, bike_comfort$CBICS_cat) |>
#               lapply(\(x) {
#                 out <- tibble::tibble(x$CBICS_cat)
#                 out$geometry <- sf::st_union(x)
#                 sf::st_as_sf(out, crs = 4326)[1, ]
#               })
# ) |> sf::st_as_sf()
# names(t)[1] <- "CBICS_cat"
bike_comfort$CBICS_cat <- as.factor(bike_comfort$CBICS_cat)

bike_map <- ggplot(data = bike_comfort) +
  gg_cc_tiles +
  geom_sf(aes(fill = CBICS_cat), color = "transparent") +
  scale_fill_manual(name = "Confort et sécurité des pistes cyclables",
                    values = curbcut_colors$left_5$fill[c(2:6)],
                    labels = c("Low", "", "", "", "High"),
                    na.value = curbcut_colors$left_5$fill[1],
                    guide = guide_legend(label.position = "bottom", 
                                         title.position = "top", nrow = 1)) +
  geom_sf(data = cycle, aes(color = "cycle_lvl"), lwd = 0.3) +
  geom_sf(data = bixi_sf_laval, aes(color = "bixi_sf"),
          size = 1, alpha = 1) +
  scale_color_manual(name = "",
                     values = c("cycle_lvl" = color_theme("pinkhealth"), 
                                "bixi_sf" = color_theme("purpletransport")),
                     labels = c("Stations Bixi", "Pistes cyclable"),
                     guide = guide_legend(label.position = "bottom", 
                                          title.position = "top", nrow = 1)) +
  gg_cc_theme

ggplot2::ggsave(filename = here::here("output/axe3/bike_map.pdf"), 
                plot = bike_map, width = 9, height = 6)

bixi_stations_number <- convert_number(nrow(bixi_sf_laval))

qs::qsavem(bike_map, bixi_stations_number,
           file = "data/axe3/bike.qsm")

