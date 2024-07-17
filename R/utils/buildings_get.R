
# Grab buildings from the database ----------------------------------------

laval_da <- cancensus::get_census(dataset = "CA21", 
                                  regions = list(CSD = 2465005), 
                                  level = "DA", 
                                  geo_format = "sf")

buildings <- cc.data::db_read_data("buildings", column_to_select = "DA_ID", 
                                   IDs = laval_da$GeoUID)

# Attach DB IDs to them ---------------------------------------------------

DB <- cancensus::get_census(dataset = "CA21", 
                            regions = list(CSD = 2465005), 
                            level = "DA", 
                            geo_format = "sf")
buildings <- sf::st_transform(buildings, crs = sf::st_crs(DB))

inter <- sf::st_intersects(sf::st_centroid(buildings), DB)

buildings$DB_ID <- sapply(inter, \(x) DB$GeoUID[x])


# Save the buildings dataset ----------------------------------------------

qs::qsave(buildings, "data/buildings.qs")
