# ## DAYCARE
# daycares <- tempfile(fileext = ".csv")
# download.file("https://www.donneesquebec.ca/recherche/dataset/be36f85e-e419-4978-9c34-cb5795622595/resource/89af3537-4506-488c-8d0e-6d85b4033a0e/download/repertoire-installation.csv",
#               daycares)
# daycares <- tibble::as_tibble(read.csv(daycares))
# Encoding(daycares$REGION) <- "latin1"
# Encoding(daycares$ADRESSE) <- "latin1"
# Encoding(daycares$NOM_MUN_COMPO) <- "latin1"
# Encoding(daycares$NOM) <- "latin1"
# daycares <-
#   daycares |>
#   dplyr::filter(REGION %in% c("13 - Laval")) |>
#   dplyr::mutate(ADRESSE =
#                   stringr::str_remove_all(ADRESSE,
#                                           ", (bureau| bureau|rez-de-chaussée|AG-10|local|suite|appartement|porte) .*$") |>
#                   stringr::str_remove_all("      \\de étage|, \\de étage") |>
#                   stringr::str_remove_all("(?<=\\d)-\\d*|[A-Z](?=,)")) |>
#   dplyr::mutate(ADRESSE = paste0(ADRESSE, ", ",NOM_MUN_COMPO, ", QC"))
# daycares <- daycares[c("ADRESSE", "PLACE_TOTAL")]
# # Geolocate with addresses
# daycares$geometry <- future.apply::future_sapply(
#   daycares$ADRESSE, cc.data::geocode_localhost,
#   simplify = FALSE, USE.NAMES = FALSE, future.seed = NULL)
# daycares <- sf::st_as_sf(daycares, crs = 4326)
# 
# qs::qsave(daycares, "data/axe3/locations/daycares.qs")
daycares <- qs::qread("data/axe3/locations/daycares.qs")
tt <- ttm()


# How many daycare spots accessible in 15 minutes walk? -------------------

DBs <- cancensus::get_census("CA21", regions = list(CSD = 2465005), level = "DB",
                             geo_format = "sf")
daycares <- sf::st_transform(daycares, crs = sf::st_crs(DBs))
daycares <- sf::st_intersection(daycares, DBs["GeoUID"])

access_spots <- cc.buildr::merge(tt, sf::st_drop_geometry(daycares), by.x = "to", by.y = "GeoUID")
access_spots <- aggregate(PLACE_TOTAL ~ from, data = access_spots, sum)
no_spots <- DBs$GeoUID[!DBs$GeoUID %in% access_spots$from]
access_spots <- rbind(access_spots, tibble::tibble(from = no_spots, PLACE_TOTAL = 0))
access_spots <- tibble::as_tibble(access_spots)


# How many daycare-aged children per DBs? ---------------------------------

DAs <- cancensus::get_census("CA21", regions = list(CSD = 2465005), level = "DA",
                             vectors = c("v_CA21_23", "v_CA21_26", "v_CA21_29", "v_CA21_35"))
DAs <- DAs[c(1, 5,  12:15)]
DAs$children <- DAs$`v_CA21_23: 2` + DAs$`v_CA21_26: 3` + DAs$`v_CA21_29: 4` + DAs$`v_CA21_35: 5`
DAs <- DAs[c("GeoUID", "Population", "children")]
names(DAs) <- c("DA_UID", "DA_pop", "children")

DB_children <- cc.buildr::merge(DBs[c("GeoUID", "DA_UID", "Population")], DAs, by = "DA_UID")
DB_children$pop_ratio <- DB_children$Population / DB_children$DA_pop
DB_children$children <- DB_children$children * DB_children$pop_ratio
DB_children <- DB_children[c("GeoUID", "children")]


# Population deserved by every daycare ------------------------------------

spots_per_child <- lapply(seq_along(daycares$geometry), \(x) {
  dc <- daycares[x, ]
  
  served_pop <- tt$from[tt$to == dc$GeoUID]
  
  # If there is no entry in the traveltime matrix, then it's not serving anyone
  if (length(served_pop) == 0) return(NULL)
  
  nb_children_served <- sapply(served_pop, \(served_ID) {
    # Spots accessible from that DB
    spots <- access_spots$PLACE_TOTAL[access_spots$from == served_ID]
    
    # Children in that DB
    children <- DB_children$children[DB_children$GeoUID == served_ID]
    
    # If the spots accessible by that DB is equal to the spots in the daycare
    # under study, then the whole population of the DB is only deserved by that
    # one daycare
    if (spots == dc$PLACE_TOTAL) return(children)
    
    # If not, how many spots are accessible by the DB? Do a ration between the
    # spots available in that daycare vs all of them accessible, and assume
    # this proportion is the same as the number of children served by every daycare
    children * dc$PLACE_TOTAL / spots
    
  }) |> sum()
  
  # Ratio of the number of children the daycare is supposed to serve, and the
  # actual number of spots
  dc$spots_per_child <- dc$PLACE_TOTAL / nb_children_served
  
  return(dc)
})

children_per_spot <- children_per_spot[!sapply(children_per_spot, is.null)]
Reduce(rbind, children_per_spot)["children_per_spot"] |> .mv()



lapply()

# One daycare spot = one point
progressr::with_progress({
  pb <- progressr::progressor(steps = nrow(daycares))
  daycares_spots <-
    future.apply::future_lapply(seq_len(nrow(daycares)), \(r) {
      pb()
      row <- daycares[r, ]
      Reduce(rbind, lapply(seq_len(row$PLACE_TOTAL), \(x) row))["geometry"]
    })
  pb <- progressr::progressor(steps = length(daycares))
  daycares_spots <-
    Reduce(\(a, b) {
      pb()
      rbind(a, b)
    }, daycares_spots)
})

daycares_spots