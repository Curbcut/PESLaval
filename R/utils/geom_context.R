## PLOT CONTEXT FUNCTION #######################################################

# Get the Laval CSD to filter the rest
laval <- cancensus::get_census(dataset = "CA21",
                               regions = list(CSD = 2465005),
                               level = "CSD",
                               geo_format = "sf")

# Montreal CMA
CSDs <- cancensus::get_census(dataset = "CA21", 
                              regions = list(CMA = 24462), 
                              level = "CSD",
                              geo_format = "sf")
CSDs <- CSDs[CSDs$GeoUID != "2465005", ]
CSDs <- sf::st_union(CSDs)

# Get the streets
street <- sf::st_read("data/geom_context/lrnf000r21a_e.shp")
street <- street[street$PRUID_L == 24, ]
street <- sf::st_transform(street, crs = sf::st_crs(laval))
street <- street[c("OBJECTID", "RANK", "CLASS")]
laval_street <- sf::st_filter(street[street$RANK %in% c(1,2,3,4), ], laval)
laval_CSD <- sf::st_filter(street[street$RANK %in% c(1,2,3), ], 
                           sf::st_union(sf::st_buffer(laval, 1000), CSDs))

qs::qsave(laval_street, "data/geom_context/laval_street.qs")
qs::qsave(laval_CSD, "data/geom_context/laval_CSD.qs")

# Get water
water_river <- sf::st_read("data/geom_context/lhy_000c16a_e.shp")
water_river <- water_river[water_river$PRUID == 24, ]
water_river <- sf::st_transform(water_river, crs = sf::st_crs(laval))
water_river <- sf::st_make_valid(water_river)
water_river <- sf::st_filter(water_river, sf::st_buffer(laval, 5000))

qs::qsave(water_river, "data/geom_context/water_river.qs")

coastal_water <- sf::st_read("data/geom_context/lhy_000h16a_e.shp")
coastal_water <- coastal_water[coastal_water$PRUID == 24, ]
coastal_water <- sf::st_transform(coastal_water, crs = sf::st_crs(laval))
coastal_water <- sf::st_make_valid(coastal_water)
coastal_water <- sf::st_filter(coastal_water, sf::st_buffer(laval, 5000))

qs::qsave(coastal_water, "data/geom_context/coastal_water.qs")

# Write the context geom function
geom_context <- function(..., sf_focus) {
  water_river <- qs::qread(here::here("data/geom_context/water_river.qs"))
  coastal_water <- qs::qread(here::here("data/geom_context/coastal_water.qs"))
  
  laval_street <- qs::qread(here::here("data/geom_context/laval_street.qs"))
  laval_CSD <- qs::qread(here::here("data/geom_context/laval_CSD.qs"))
  
  # Set aesthetics
  highway_wdth <- 0.25
  medium_wdth <- 0.1
  street_color <- "black"
  water_color <- "#A3B0D1"
  
  list(
    theme_void(),
    geom_sf(data = CSDs, fill = "grey95", color = "transparent"),
    geom_sf(data = water_river, fill = water_color, color = "transparent"),
    geom_sf(data = coastal_water, fill = water_color, color = "transparent"),
    geom_sf(data = laval_street[laval_street$RANK %in% c(1,2,3), ], fill = street_color,
            linewidth = highway_wdth),
    geom_sf(data = laval_street[laval_street$RANK %in% c(4), ], fill = street_color,
            linewidth = medium_wdth),
    geom_sf(data = laval_CSD, fill = street_color, linewidth = highway_wdth),
    coord_sf(xlim = sf::st_bbox(sf_focus)[c(1,3)],
             ylim = sf::st_bbox(sf_focus)[c(2,4)])
  )
}

qs::qsave(geom_context, file = "data/geom_context/geom_context.qs")
