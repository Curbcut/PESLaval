source("R/01_startup.R")
library(qs)

#Loading in CPE locations
CPEs <- qread("data/cpe.qs")
CPEs$ID <- paste0("cp_", seq_along(CPEs$ADRESSE))

DBs <- cancensus::get_census("CA21", regions = list(CSD = 2465005), level = "DB",
                             geo_format = "sf")

DAs <- cancensus::get_census("CA21", regions = list(CSD = 2465005), level = "DA",
                             vectors = c("v_CA21_17", "v_CA21_20", "v_CA21_23", "v_CA21_26", "v_CA21_29", "v_CA21_35"),
                             geo_format = "sf")
DAs <- DAs[c(3, 8,  15:21)]
DAs$children <- DAs$`v_CA21_17: Under 1 year` + DAs$`v_CA21_20: 1` + DAs$`v_CA21_23: 2` + DAs$`v_CA21_26: 3` + DAs$`v_CA21_29: 4` + DAs$`v_CA21_35: 5`
DAs <- DAs[c("GeoUID", "Population", "children")]
names(DAs) <- c("DA_UID", "DA_pop", "children", "geometry")

#Numbers
places_CPE <- sum(CPEs$PLACE_TOTAL)

#Calculating ratio between kids and CPE spaces
kids_five <- D

inage <- 20495
kinder_children <- convert_number_noround(inage)

cpe_ratio <- round(places_CPE / inage, 2)
cpe_ratio <- gsub("\\.", ",", as.character(cpe_ratio))

#Results of the ttm
cpe_DB <- CPEs |> 
  st_join(DBs |> select(GeoUID), join = st_intersects) |> 
  mutate(
    GeoUID = if_else(
      ADRESSE == "6250, boulevard Arthur-Sauvé, Laval, QC",
      "24650519011",
      GeoUID
    )
  ) |> 
  rename("DB_ID" = "GeoUID")

CPE_to_DBs <- qread("data/travel_time.qs")

names(CPE_to_DBs) <- CPEs$ID

CPE_to_DBs <- mapply(\(n, df) {
  df$ID <- n
  df
}, names(CPE_to_DBs), CPE_to_DBs, SIMPLIFY = FALSE)

CPE_to_DBs <- Reduce(rbind, CPE_to_DBs)

CPE_to_DBs <- CPE_to_DBs[CPE_to_DBs$time_seconds <= 15*60, ]
CPE_to_DBs <- merge(CPE_to_DBs, sf::st_drop_geometry(CPEs[c("ID", "PLACE_TOTAL")]),
                         by = "ID")
# How many CPE spots accessible in 15 minutes walk? -------------------

access_spots <- aggregate(PLACE_TOTAL ~ DB_ID, data = CPE_to_DBs, sum)
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
  DB_children$children[!DB_children$GeoUID %in% CPE_to_DBs$DB_ID] |> 
  sum(na.rm = TRUE)

# Population served by every daycare ------------------------------------

spots_per_child <- lapply(seq_along(CPEs$geometry), \(x) {
  dc <- CPEs[x, ]
  
  # served_pop <- tt$from[tt$to == dc$GeoUID]
  
  served_pop <- CPE_to_DBs$DB_ID[CPE_to_DBs$ID == dc$ID]
  
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

# Access to spots per child -----------------------------------------------

# Loop over all DBs. Look at all the CPEs they can access. What are their
# ratio? Can they meet demand?
CPE_access_comp <- purrr::map_dfr(DBs$GeoUID, \(DB_ID) {
  
  dc_ID <- CPE_to_DBs$ID[CPE_to_DBs$DB_ID == DB_ID]
  
  accessible_CPEs <- spots_per_child[spots_per_child$ID %in% dc_ID, ]
  
  # If no daycare, worst score
  if (nrow(accessible_CPEs) == 0) 
    return(tibble::tibble(DB = DB_ID, CPE_access_comp = 0))
  
  spots_p_child_weighted_by_spots <- 
    sum(accessible_CPEs$spots_per_child)
  
  tibble::tibble(DB = DB_ID, CPE_access_comp = spots_p_child_weighted_by_spots)
  
})

CPE_access_comp <- cc.buildr::merge(DBs, CPE_access_comp, 
                                        by.x = "GeoUID", by.y = "DB")[
                                          c("GeoUID", "CPE_access_comp")]

t <- CPE_access_comp

t <- add_bins(df = t,
              variable = "CPE_access_comp",
              breaks = c(-Inf, 
                         0.00000000000000000000000000001, 
                         quantile(t$CPE_access_comp, probs = c(.25, .50, .75)), 
                         Inf),
              labels = c("Aucun accès", "Un peu", "", "Bon accès")
)

labels <- c("Aucun accès", "", "", "Bon accès")

# Bin the PLACE_TOTAL variable
CPEs$binned_PLACE_TOTAL <- cut(CPEs$PLACE_TOTAL, 
                                   breaks = c(-Inf, 20, 40, 60, 80, Inf), 
                                   labels = c("0-20", "21-40", "41-60", "61-80", "80+"))
place_colors <- c("0-20" = "#fee5e9", "21-40" = "#fbccce", "41-60" = "#f6b3af", 
                  "61-80" = "#ed9c8c", 
                  "80+" = curbcut_colors$brandbook$color[curbcut_colors$brandbook$theme == "redhousing"])

# Plotting the sf map with custom bins
CPEs_map <- ggplot(t) +
  gg_cc_tiles +
  geom_sf(aes(fill = binned_variable), color = "transparent", lwd = 0) +
  scale_fill_manual(values = curbcut_colors$left_5$fill[2:6],
                    name = "Accessibilité (concurrence)",
                    labels = labels,
                    guide = guide_legend(title.position = "top", label.position = "bottom", nrow = 1)) +
  geom_sf(data = CPEs, aes(color = binned_PLACE_TOTAL), size = 0.8, alpha = 0.8) +
  scale_color_manual(values = place_colors, 
                     name = "Places par\nservice de garde", 
                     guide = guide_legend(title.position = "top", label.position = "bottom", nrow = 1,
                                          override.aes = list(size = 6, stroke = 0.5))) +
  gg_cc_theme +
  theme(legend.spacing.x = unit(2, 'cm'),
        legend.spacing.y = unit(1, 'cm'))

<<<<<<< HEAD

ggsave(filename = here::here("output/axe3/CPE_map.png"),
       plot = CPEs_map, width = 9, height = 7.5, bg = "white")

ggplot2::ggsave(filename = here::here("output/axe3/CPE_map.pdf"), 
                plot = CPEs_map, width = 7, height = 5.5, bg = "transparent")
=======
ggplot2::ggsave(filename = here::here("output/axe3/daycare.pdf"), 
                plot = daycare_map, width = 7, height = 5.5, bg = "transparent")

ggsave(filename = here::here("output/axe3/daycare_map.png"),
       plot = daycare_map, width = 9, height = 7.5, bg = "white")
>>>>>>> 0992fe94ce9fa924322cb5e334b9811ff99f1e6c


low <- t$GeoUID[t$binned_variable %in% c("Aucun accès")]
combien_pas_peu <- sum(DB_children$children[DB_children$GeoUID %in% low], na.rm = TRUE)

high <- t$GeoUID[t$binned_variable %in% c("Bon accès")]
combien_haut <- sum(DB_children$children[DB_children$GeoUID %in% high], na.rm = TRUE)
