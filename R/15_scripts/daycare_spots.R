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
source("R/utils/tt_fun.R")
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
                             vectors = c("v_CA21_23", "v_CA21_26", "v_CA21_29", "v_CA21_35"),
                             geo_format = "sf")
DAs <- DAs[c(3, 8,  15:18)]
DAs$children <- DAs$`v_CA21_23: 2` + DAs$`v_CA21_26: 3` + DAs$`v_CA21_29: 4` + DAs$`v_CA21_35: 5`
DAs <- DAs[c("GeoUID", "Population", "children")]
names(DAs) <- c("DA_UID", "DA_pop", "children", "geometry")

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
    
  }) |> sum(na.rm = TRUE)
  
  # If there are no children deserved by the daycare, ignore the daycare
  if (nb_children_served == 0) return(NULL)
  
  # Ratio of the number of children the daycare is supposed to serve, and the
  # actual number of spots
  dc$spots_per_child <- dc$PLACE_TOTAL / nb_children_served
  
  return(dc)
})

spots_per_child <- spots_per_child[!sapply(spots_per_child, is.null)]
spots_per_child <- Reduce(rbind, spots_per_child)


# Access to spots per child -----------------------------------------------

# Loop over all DBs. Look at all the daycares they can access. What are their
# ratio? Can they meet demand?
daycare_access_comp <- purrr::map_dfr(DBs$GeoUID, \(DB_ID) {
  accessible_DBs <- tt$to[tt$from == DB_ID]
  
  accessible_daycares <- spots_per_child[spots_per_child$GeoUID %in% accessible_DBs, ]
  
  # If no daycare, worst score
  if (nrow(accessible_daycares) == 0) 
    return(tibble::tibble(DB = DB_ID, daycare_comp_access = 0))
  
  spots_p_child_weighted_by_spots <- 
    weighted.mean(accessible_daycares$spots_per_child, w = accessible_daycares$PLACE_TOTAL)
  
  tibble::tibble(DB = DB_ID, daycare_comp_access = spots_p_child_weighted_by_spots)
  
})

daycare_access_comp <- cc.buildr::merge(DBs, daycare_access_comp, 
                                        by.x = "GeoUID", by.y = "DB")[
                                          c("GeoUID", "daycare_comp_access")]

library(ggplot2)

# Setting up the curbcut scale
curbcut_scale <- c("#C4CDE1", "#98A8CB", "#6C83B5", "#4C5C7F", "#2B3448")
curbcut_na <- "#B3B3BB"

# Custom breaks for 5 bins
breaks <- seq(0, 2, length.out = 5)

ggplot(daycare_access_comp) +
  geom_sf(aes(fill = daycare_comp_access), color = "transparent") + 
  scale_fill_stepsn(colors = curbcut_scale, limits = c(0, 2), na.value = curbcut_na, breaks = breaks) +
  labs(fill = "Daycare Access") +
  theme_minimal()

breaks <- seq(0, 60, length.out = 5)

ggplot(DAs) +
  geom_sf(aes(fill = children), color = "transparent") + 
  scale_fill_stepsn(colors = curbcut_scale, limits = c(0, 50), na.value = curbcut_na, breaks = breaks) +
  labs(fill = "Daycare Access") +
  theme_minimal()

