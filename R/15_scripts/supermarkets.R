laval_ct <- cancensus::get_census(dataset = "CA21", 
                                  regions = list(CSD = 2465005), 
                                  level = "CT", 
                                  geo_format = "sf")

#Setting the Laval bound box for maps
laval_bbox <- sf::st_bbox(laval_ct)

z <- opq(bbox = laval_bbox, timeout = 300) |> 
  add_osm_feature(key = "shop", value = "supermarket") |> 
  osmdata_sf()

library(ggplot2)
ggplot() +
  geom_sf(data = laval_ct) +
  geom_sf(data =  z$osm_points)
