
source("R/01_startup.R")


# ## DAYCARE
# daycares <- tempfile(fileext = ".csv")
# download.file("https://www.donneesquebec.ca/recherche/dataset/be36f85e-e419-4978-9c34-cb5795622595/resource/89af3537-4506-488c-8d0e-6d85b4033a0e/download/repertoire-installation.csv",
#               daycares)
# daycares <- tibble::as_tibble(read.csv(daycares))
# # Encoding(daycares$REGION) <- "latin1"
# # Encoding(daycares$ADRESSE) <- "latin1"
# # Encoding(daycares$NOM_MUN_COMPO) <- "latin1"
# # Encoding(daycares$NOM) <- "latin1"
# daycares <-
#   daycares |>
#   dplyr::filter(REGION %in% c("13 - Laval")) |>
#   dplyr::mutate(ADRESSE =
#                   stringr::str_remove_all(ADRESSE,
#                                           ", (bureau| bureau|rez-de-chaussée|AG-10|local|suite|appartement|porte) .*$") |>
#                   stringr::str_remove_all("      \\de étage|, \\de étage") |>
#                   stringr::str_remove_all("(?<=\\d)-\\d*|[A-Z](?=,)")) |>
#   dplyr::mutate(ADRESSE = paste0(ADRESSE, ", ",NOM_MUN_COMPO, ", QC"))
# daycares <- daycares[c("ADRESSE", "PLACE_TOTAL", "TYPE", "SUBV")]
# daycares <- daycares[daycares$SUBV == "CR", ]
# # Geolocate with addresses
# daycares$geometry <- future.apply::future_sapply(
#   daycares$ADRESSE, cc.data::geocode_localhost,
#   simplify = FALSE, USE.NAMES = FALSE, future.seed = NULL)
# daycares <- sf::st_as_sf(daycares, crs = 4326)
# 
# qs::qsave(daycares, "data/axe3/locations/daycares.qs")
daycares <- qs::qread("data/axe3/locations/daycares.qs")
daycares$ID <- paste0("dc_", seq_along(daycares$ADRESSE))

daycares$geometry[daycares$ADRESSE == "1305, boul. de la Concorde, Laval, QC"] <-
  sf::st_point(rev(c(45.559836823184625, -73.71356854482919)))
daycares$geometry[daycares$ADRESSE == "440, boulevard Ivan-Pavlov appartement 10, Laval, QC"] <-
  sf::st_point(rev(c(45.609889185608125, -73.70707443145565)))
daycares$geometry[daycares$ADRESSE == "97, 101, 8e rue, Laval, QC"] <-
  sf::st_point(rev(c(45.56244221119917, -73.69175450362846)))
daycares$geometry[daycares$ADRESSE == "2242, rue Desserte Ouest, unité 20, Autoroute Chomedey, Laval, QC"] <-
  sf::st_point(rev(c(45.53344954737205, -73.7909081564965)))
daycares$geometry[daycares$ADRESSE == "100, croissant des Callières, Laval, QC"] <-
  sf::st_point(rev(c(45.591003070955566, -73.66673376017127)))
daycares$geometry[daycares$ADRESSE == "720, Montgolfier suite 102, Laval, QC"] <-
  sf::st_point(rev(c(45.532954184278886, -73.78154349976236)))
daycares$geometry[daycares$ADRESSE == "5555, boul. des Laurentides, locaux 26 et 27, Laval, QC"] <-
  sf::st_point(rev(c(45.627163865082295, -73.75090128900545)))

if (nrow(daycares[sf::st_is_empty(daycares), ]) > 0) {
  stop("Daycares didn't geolocate")
}

tt <- ttm()
DBs <- cancensus::get_census("CA21", regions = list(CSD = 2465005), level = "DB",
                             geo_format = "sf")
DAs <- cancensus::get_census("CA21", regions = list(CSD = 2465005), level = "DA",
                             vectors = c("v_CA21_17", "v_CA21_20", "v_CA21_23", "v_CA21_26", "v_CA21_29", "v_CA21_35"),
                             geo_format = "sf")
DAs <- DAs[c(3, 8,  15:21)]
DAs$children <- DAs$`v_CA21_17: Under 1 year` + DAs$`v_CA21_20: 1` + DAs$`v_CA21_23: 2` + DAs$`v_CA21_26: 3` + DAs$`v_CA21_29: 4` + DAs$`v_CA21_35: 5`
DAs <- DAs[c("GeoUID", "Population", "children")]
names(DAs) <- c("DA_UID", "DA_pop", "children", "geometry")



# # Relevant digits ---------------------------------------------------------

# # Number of places
places_garderie <- sum(daycares$PLACE_TOTAL)
places_CPE <- sum(daycares$PLACE_TOTAL[daycares$TYPE == "CPE"])
places_sgsn <- sum(daycares$PLACE_TOTAL[daycares$TYPE == "GARD"])

places_CPE + places_sgsn

daycares[daycares$TYPE == "CPE", ] |> nrow()
daycares[daycares$TYPE == "GARD", ] |> nrow()

# Number of children in age of kindergarden (projections ISQ)
inage <- 20495

kinder_children <- convert_number_noround(inage)

# Ajout en milieu familial
places_garderie_avec_milieu_familial <- places_garderie + 3951

kinder_ratio <- round(places_garderie_avec_milieu_familial / inage, 2)
kinder_ratio <- gsub("\\.", ",", as.character(kinder_ratio))


# Rework a travel time matrix which is really from X daycare to Y  --------

# cc.data::tt_local_osrm("foot")

travel_time <- function(FROMs, TOs, routing_server = "http://localhost:5001/",
                        max_dist = 3000) {
  if (sf::st_crs(FROMs)$input != "EPSG:4326") {
    FROMs <- suppressWarnings(sf::st_transform(FROMs, 4326))
  }
  if (sf::st_crs(TOs)$input != "EPSG:4326") {
    TOs <- suppressWarnings(sf::st_transform(TOs, 4326))
  }

  # Split the FROM for each entry
  splitted_froms <- split(FROMs, seq_along(FROMs[[1]]))

  # Split the TOs in smaller dataframes for faster paralleled calculations
  list_centroids <- split(TOs, seq_len(nrow(TOs) / min(nrow(TOs), 100))) |>
    suppressWarnings()

  progressr::with_progress({
    pb <- progressr::progressor(steps = length(splitted_froms))
    out <- future.apply::future_lapply(splitted_froms, \(dc) {

      first_coords <- sf::st_coordinates(dc) |>
        tibble::as_tibble()
      first_coords <- paste0(first_coords$X, ",", first_coords$Y)

      it <- lapply(list_centroids, \(df) {

        dist <- nngeo::st_nn(dc,
                             df,
                             k = nrow(df),
                             maxdist = max_dist,
                             progress = FALSE) |>
          suppressMessages()
        near_df <- df[unlist(dist), ]

        # if (nrow(near_df) == 0) return(NULL)
        # near_df <- near_df[near_df$ID != id, ]
        if (nrow(near_df) == 0) return(NULL)

        samp <- sf::st_coordinates(near_df) |>
          tibble::as_tibble()

        coords <- paste0(mapply(paste0, samp$X, ",", samp$Y), collapse = ";")
        coords <- paste0(first_coords, ";", coords)

        time <- httr::GET(paste0(routing_server, "table/v1/mode/",
                                 coords, "?sources=0")) |>
          httr::content()
        time <- unlist(time$durations)

        tryCatch({
          out <- tibble::tibble(DB_ID = near_df$GeoUID)
          out$time_seconds <- time[2:length(time)]
          out}, error = function(e) NULL)

      })

      pb()

      it[!sapply(it, is.null)] |>
        data.table::rbindlist(fill = TRUE) |>
        tibble::as_tibble()

    })
  })

  # Return
  out <- out[!sapply(out, \(x) nrow(x) == 0)]
  return(out)

}


# daycares <- daycares[daycares$TYPE == "CPE", ]
daycares_to_DBs <- travel_time(daycares, sf::st_centroid(DBs))
names(daycares_to_DBs) <- daycares$ID


daycares_to_DBs <- mapply(\(n, df) {
  df$ID <- n
  df
}, names(daycares_to_DBs), daycares_to_DBs, SIMPLIFY = FALSE)

daycares_to_DBs <- Reduce(rbind, daycares_to_DBs)

# Accessible in 15 minutes
daycares_to_DBs <- daycares_to_DBs[daycares_to_DBs$time_seconds <= 15*60, ]
daycares_to_DBs <- merge(daycares_to_DBs, sf::st_drop_geometry(daycares[c("ID", "PLACE_TOTAL")]),
      by = "ID")

places_garderie_avec_milieu_familial <- convert_number_noround(places_garderie_avec_milieu_familial)
qs::qsavem(daycares, daycares_to_DBs, places_garderie, inage, kinder_children,
           places_garderie_avec_milieu_familial,
           kinder_ratio, file = "data/axe3/daycares_rawish.qsm")
qs::qload("data/axe3/daycares_rawish.qsm")


# How many daycare spots accessible in 15 minutes walk? -------------------

# daycares <- sf::st_transform(daycares, crs = sf::st_crs(DBs))
# daycares <- sf::st_intersection(daycares, DBs["GeoUID"])
# 
# access_spots <- cc.buildr::merge(tt, sf::st_drop_geometry(daycares), by.x = "to", by.y = "GeoUID")
# access_spots <- aggregate(PLACE_TOTAL ~ from, data = access_spots, sum)
# no_spots <- DBs$GeoUID[!DBs$GeoUID %in% access_spots$from]
# access_spots <- rbind(access_spots, tibble::tibble(from = no_spots, PLACE_TOTAL = 0))
# access_spots <- tibble::as_tibble(access_spots)

access_spots <- aggregate(PLACE_TOTAL ~ DB_ID, data = daycares_to_DBs, sum)
no_spots <- DBs$GeoUID[!DBs$GeoUID %in% access_spots$DB_ID]
access_spots <- rbind(access_spots, tibble::tibble(DB_ID = no_spots, PLACE_TOTAL = 0))
access_spots <- tibble::as_tibble(access_spots)

# How many daycare-aged children per DBs? ---------------------------------

DB_children <- cc.buildr::merge(DBs[c("GeoUID", "DA_UID", "Population")], 
                                sf::st_drop_geometry(DAs), by = "DA_UID")
DB_children$pop_ratio <- DB_children$Population / DB_children$DA_pop
DB_children$children <- DB_children$children * DB_children$pop_ratio
DB_children <- DB_children[c("GeoUID", "children")]

# How many children can reach them? ---------------------------------------

children_cant_reach <- 
  DB_children$children[!DB_children$GeoUID %in% daycares_to_DBs$DB_ID] |> 
  sum(na.rm = TRUE)


# Population deserved by every daycare ------------------------------------

spots_per_child <- lapply(seq_along(daycares$geometry), \(x) {
  dc <- daycares[x, ]
  
  # served_pop <- tt$from[tt$to == dc$GeoUID]
  
  served_pop <- daycares_to_DBs$DB_ID[daycares_to_DBs$ID == dc$ID]
  
  # If there is no entry in the traveltime matrix, then it's not serving anyone
  if (length(served_pop) == 0) return(NULL)
  
  nb_children_served <- sapply(served_pop, \(served_ID) {
    # Spots accessible from that DB
    spots <- access_spots$PLACE_TOTAL[access_spots$DB_ID == served_ID]
    
    # Children in that DB
    children <- DB_children$children[DB_children$GeoUID == served_ID]
    
    # If the spots accessible by that DB is equal to the spots in the daycare
    # under study, then the whole population of the DB is only deserved by that
    # one daycare
    if (spots == dc$PLACE_TOTAL) return(children)
    
    # If not, how many spots are accessible by the DB? Do a ratio between the
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

# spots_per_child <- sf::st_intersection(spots_per_child, DBs["GeoUID"])

# Access to spots per child -----------------------------------------------

# Loop over all DBs. Look at all the daycares they can access. What are their
# ratio? Can they meet demand?
daycare_access_comp <- purrr::map_dfr(DBs$GeoUID, \(DB_ID) {
  
  dc_ID <- daycares_to_DBs$ID[daycares_to_DBs$DB_ID == DB_ID]
  
  accessible_daycares <- spots_per_child[spots_per_child$ID %in% dc_ID, ]
  
  # If no daycare, worst score
  if (nrow(accessible_daycares) == 0) 
    return(tibble::tibble(DB = DB_ID, daycare_comp_access = 0))
  
  spots_p_child_weighted_by_spots <- 
    sum(accessible_daycares$spots_per_child)
  
  tibble::tibble(DB = DB_ID, daycare_comp_access = spots_p_child_weighted_by_spots)
  
})

daycare_access_comp <- cc.buildr::merge(DBs, daycare_access_comp, 
                                        by.x = "GeoUID", by.y = "DB")[
                                          c("GeoUID", "daycare_comp_access")]

# Plot the accessibility measure ------------------------------------------

t <- daycare_access_comp

t <- add_bins(df = t,
         variable = "daycare_comp_access",
         breaks = c(-Inf, 
                    0.00000000000000000000000000001, 
                    quantile(t$daycare_comp_access, probs = c(.25, .50, .75)), 
                    Inf),
         labels = c("Aucun accès", "Un peu", "a", "b", "Bon accès")
)
labels <- c("Aucun accès", "Un peu", "", "", "Bon accès")

# # Union the features so the polygons don't show their borders. Might revisit
# # with the addition of streets!
# t <- Reduce(rbind,
#        split(t, t$binned_variable) |>
#          lapply(\(x) {
#            out <- tibble::tibble(x$binned_variable)
#            out$geometry <- sf::st_union(x)
#            sf::st_as_sf(out, crs = 4326)[1, ]
#          })
# ) |> sf::st_as_sf()
# names(t)[1] <- "binned_variable"

# Bin the PLACE_TOTAL variable
daycares$binned_PLACE_TOTAL <- cut(daycares$PLACE_TOTAL, 
                                   breaks = c(-Inf, 20, 40, 60, 80, Inf), 
                                   labels = c("0-20", "21-40", "41-60", "61-80", "80+"))
place_colors <- c("0-20" = "#fee5e9", "21-40" = "#fbccce", "41-60" = "#f6b3af", 
                  "61-80" = "#ed9c8c", 
                  "80+" = curbcut_colors$brandbook$color[curbcut_colors$brandbook$theme == "redhousing"])

# Plotting the sf map with custom bins
daycare_map <- ggplot(t) +
  gg_cc_tiles +
  geom_sf(aes(fill = binned_variable), color = "transparent", lwd = 0) +
  scale_fill_manual(values = curbcut_colors$left_5$fill[2:6],
                    name = "Accessibilité (concurrence)",
                    labels = labels,
                    guide = guide_legend(title.position = "top", label.position = "bottom", nrow = 1)) +
  geom_sf(data = daycares, aes(color = binned_PLACE_TOTAL), size = 0.8, alpha = 0.8) +
  scale_color_manual(values = place_colors, 
                     name = "Places par\nservice de garde", 
                     guide = guide_legend(title.position = "top", label.position = "bottom", nrow = 1,
                                          override.aes = list(size = 5, stroke = 0.5))) +
  gg_cc_theme +
  theme(legend.spacing.x = unit(2, 'cm'),
        legend.spacing.y = unit(1, 'cm'))

ggplot2::ggsave(filename = here::here("output/axe3/daycare.pdf"), 
                plot = daycare_map, width = 7, height = 5.5, bg = "transparent")




# How many children in the first bins? ------------------------------------

low <- t$GeoUID[t$binned_variable %in% c("Aucun accès", "Un peu")]
combien_pas_peu <- sum(DB_children$children[DB_children$GeoUID %in% low])

high <- t$GeoUID[t$binned_variable %in% c("Bon accès")]
combien_haut <- sum(DB_children$children[DB_children$GeoUID %in% high], na.rm = TRUE)

# Plot the number of children ---------------------------------------------

curbcut_green_scale <- c("#C7DFCC", "#9DC6A6", "#73AE80", "#517A5A", "#2E4633")

DAs$area <- cc.buildr::get_area(DAs) / 1e6
DAs$children_density <- DAs$children / DAs$area

# Define your breaks
breaks <- c(-Inf, 50, 100, 200, 400, Inf) # Add more breaks as needed

# Use cut to create the binned variable
DAs$binned_variable <- cut(DAs$children_density, 
                           breaks = breaks, 
                           labels = c("0-50", "50-100", "100-200", "200-400", "400+"))

children_colors <- curbcut_green_scale
names(children_colors) <- c("0-50", "50-100", "100-200", "200-400", "400+")

child_map <- ggplot(DAs) +
  gg_cc_tiles +
  geom_sf(aes(fill = binned_variable), color = NA) +
  scale_fill_manual(values = children_colors, 
                    name = "Densité d'enfants par kilomètre carré", 
                    guide = guide_legend(title.position = "top", 
                                         label.position = "bottom", 
                                         nrow = 1,
                                         override.aes = list(size = 5, stroke = 0.5))) +
  gg_cc_theme

ggplot2::ggsave(filename = here::here("output/axe3/child_map.pdf"), 
                plot = child_map, width = 7, height = 5.5, bg = "transparent")


# R Markdown Numbers ------------------------------------------------------

daycare_spots <- convert_number_noround(places_garderie)
daycare_total <- daycares |> 
  mutate(number = 1) |> 
  summarise(number = sum(number)) |> 
  pull(number)
zero_access <- convert_number(children_cant_reach)
poor_no_access <- convert_number(combien_pas_peu)
good_access <- convert_number(combien_haut)
# R Markdown --------------------------------------------------------------
#ggplot2::ggsave(filename = here::here("output/axe3/mobility/bike_map.pdf"), 
                #plot = bike_map, width = 7.5, height = 6)

qs::qsavem(kinder_children, kinder_ratio, daycare_map, child_map, daycare_spots,
           daycare_total, zero_access, poor_no_access, good_access,
           places_garderie_avec_milieu_familial,
           file = "data/axe3/daycare.qsm")

