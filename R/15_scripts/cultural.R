
source("R/01_startup.R")

# Load cultural facilities ------------------------------------------------

# De BDOICA
read_method <- function(file) {
  content <- utils::unzip(file, list = TRUE, exdir = tempdir())$Name
  csv_file <- content[grepl("\\.csv", content)]
  csv_file <- csv_file[!grepl("Data_Sources", csv_file)]
  utils::unzip(file, files = csv_file, exdir = tempdir())
  suppressWarnings(utils::read.csv(paste0(tempdir(), "\\", 
                                          csv_file)))
}
# cultural <- tibble::as_tibble(cc.data::bucket_read_object(object = "open_db_cultural_facilities.zip", 
#                                                           bucket = "curbcut.amenities", objectext = ".zip", method = read_method))
# qs::qsave(cultural, file = "data/axe3/cultural_data.qs")
cultural <- qs::qread("data/axe3/cultural_data.qs")
cultural$Longitude <- suppressWarnings(as.numeric(cultural$Longitude))
cultural <- cultural[!is.na(cultural$Longitude), ]
cultural$Latitude <- suppressWarnings(as.numeric(cultural$Latitude))
cultural <- cultural[!is.na(cultural$Latitude), ]
cultural <- sf::st_as_sf(cultural, 
                         coords = c("Longitude", 
                                    "Latitude"), 
                         crs = 4326)


# De Signé laval
sig <- tibble::tibble(name = c("comotion", "jazz", "synapses"),
                      geometry = sf::st_sfc(sf::st_point(c(-73.7187425098842, 45.56003891491458)),
                                            sf::st_point(c(-73.72620606945262, 45.566202830787915)),
                                            sf::st_point(c(-73.7320292118133, 45.60717957929867))))
sig <- sf::st_as_sf(sig, crs = 4326)

# Du diagnostic culturel (2017)
dia <- qs::qread("data/axe3/locations/culturels_from_diagnostic.qs")


cultural <- cultural[c("Facility_Name")]
names(cultural)[1] <- "name"
names(dia)[1] <- "name"

cultural <- rbind(sig, dia, cultural)
cultural$name <- sapply(cultural$name, utf8::as_utf8)

CMA <- cancensus::get_census("CA21", regions = list(CMA = 24462), level = "CMA",
                             geo_format = "sf")
CMA_DBs <- cancensus::get_census("CA21", regions = list(CMA = 24462), level = "DB",
                                 geo_format = "sf")

# Keep only the ones in the CMA
cultural <- sf::st_transform(cultural, crs = sf::st_crs(CMA_DBs))
cultural <- sf::st_filter(cultural, CMA)

# Remove one of the points if they are within 20m (double counting)
threshold_distance <- units::set_units(20, "meters")
distance_matrix <- sf::st_distance(cultural, )
close_points <- distance_matrix < threshold_distance
diag(close_points) <- FALSE
keep_indices <- which(!apply(close_points, 1, any))
filtered_points <- cultural[keep_indices, ]

pairs <- cultural[-keep_indices, ]
threshold_distance <- units::set_units(20, "meters")
distance_matrix <- sf::st_distance(pairs)
close_points <- distance_matrix < threshold_distance
diag(close_points) <- FALSE
to_remove <- rep(FALSE, nrow(pairs))

# Iterate over the pairs and mark points for removal
for (i in 1:nrow(close_points)) {
  if (!to_remove[i]) {
    close_indices <- which(close_points[i, ])
    to_remove[close_indices] <- TRUE
  }
}
pairs <- pairs[!to_remove, ]
cultural <- rbind(filtered_points, pairs)

# How many in Laval?
CSD <- cancensus::get_census("CA21", regions = list(CSD = 2465005), level = "CSD",
                             geo_format = "sf")
nrow(sf::st_filter(cultural, CSD))

# Attach a DB ID
cultural <- sf::st_intersection(cultural, CMA_DBs["GeoUID"])

tt <- ttm()

# How many cultural accessible --------------------------------------------

hm_access_cultural <- merge(tt, cultural[c("GeoUID", "name")], 
                               by.x = "to", by.y = "GeoUID")

hm_access_cultural <- unique(hm_access_cultural[c("from", "name")])
hm_access_cultural <- table(hm_access_cultural$from)

hm_access_cultural <- 
  tibble::tibble(GeoUID = names(hm_access_cultural),
                 cultural = as.vector(hm_access_cultural))

no_access <- DBs$GeoUID[!DBs$GeoUID %in% hm_access_cultural$GeoUID]
hm_access_cultural <- rbind(hm_access_cultural, tibble::tibble(GeoUID = no_access, cultural = 0))
hm_access_cultural <- tibble::as_tibble(hm_access_cultural)

# Add spatial features
hm_access_cultural <- 
  hm_access_cultural |> 
  dplyr::left_join(DBs["GeoUID"]) |> 
  sf::st_as_sf()


# Plot it -----------------------------------------------------------------

labels <- c("0", "1-2", "3+")

# Add our bins in the data
hm_access_cultural <- add_bins(df = hm_access_cultural,
                                  variable = "cultural",
                                  breaks = c(-Inf, 0.1, 2.1, Inf),
                                  labels = labels
)

# Union the features so the polygons don't show their borders. Might revisit
# with the addition of streets!
t <- Reduce(rbind,
            split(hm_access_cultural, hm_access_cultural$binned_variable) |>
              lapply(\(x) {
                out <- tibble::tibble(x$binned_variable)
                out$geometry <- sf::st_union(x)
                sf::st_as_sf(out, crs = 4326)[1, ]
              })
) |> sf::st_as_sf()
names(t)[1] <- "binned_variable"

t |> 
  ggplot() +
  gg_cc_tiles +
  geom_sf(aes(fill = binned_variable), color = "transparent") +
  scale_fill_manual(values = curbcut_colors$left_5$fill[c(2, 4, 6)],
                    name = "Installations culturelles accessibles (n)",
                    labels = labels,
                    guide = guide_legend(title.position = "top",
                                         label.position = "bottom", nrow = 1)) +
  geom_sf(data = cultural, color = color_theme("pinkhealth"),
          size = 0.8, alpha = 0.8) +
  gg_cc_theme +
  theme(legend.spacing.x = unit(2, 'cm'),
        legend.spacing.y = unit(1, 'cm'))


# Values ------------------------------------------------------------------

cultural_pop <- merge(DBs[c("GeoUID", "Population")], 
                   sf::st_drop_geometry(hm_access_cultural),
                   by = "GeoUID")

cultural_pop <- 
  cultural_pop |> 
  sf::st_drop_geometry() |> 
  group_by(binned_variable) |> 
  summarize(pop = sum(Population))

# Make sure to use the same numbers as in the table shown!!!!
zeroaccess <- sum(cultural_pop$pop[cultural_pop$binned_variable %in% c("0")])
zeroaccess / lvl$Population
more_3 <- sum(cultural_pop$pop[cultural_pop$binned_variable %in% c("3+")])
more_3 / lvl$Population

# Look for more vulnerable population. lowincome and old age?
DAs <- cancensus::get_census("CA21", regions = list(CSD = 2465005), level = "DA",
                             vectors = c(lowincome = "v_CA21_1025",
                                         immigrant = "v_CA21_4410"),
                             geo_format = "sf")

DAs <- DAs[c("GeoUID", "Population", "lowincome", "immigrant")]
names(DAs) <- c("DA_UID", "DA_pop", "lowincome", "immigrant", "geometry")
DB_lowincome <- cc.buildr::merge(DBs[c("GeoUID", "DA_UID", "Population")], 
                                 sf::st_drop_geometry(DAs), by = "DA_UID")
DB_lowincome <- sf::st_drop_geometry(DB_lowincome)
DB_lowincome$pop_ratio <- DB_lowincome$Population / DB_lowincome$DA_pop
DB_lowincome$lowincome <- DB_lowincome$lowincome * DB_lowincome$pop_ratio
DB_lowincome$immigrant <- DB_lowincome$immigrant * DB_lowincome$pop_ratio
DB_lowincome <- DB_lowincome[c("GeoUID", "lowincome", "immigrant")]

cultural_demo <- merge(DB_lowincome, 
                   sf::st_drop_geometry(hm_access_cultural),
                   by = "GeoUID")

cultural_demo <- 
  cultural_demo |> 
  sf::st_drop_geometry() |> 
  group_by(binned_variable) |> 
  summarize(lowincome = sum(lowincome, na.rm = TRUE),
            immigrant = sum(immigrant, na.rm = TRUE)) |> 
  mutate(lowincome_pct = lowincome / sum(lowincome), 
         immigrant_pct = immigrant / sum(immigrant))

cultural_demo$lowincome <- convert_hundreds(x = cultural_demo$lowincome)
cultural_demo$immigrant <- convert_hundreds(x = cultural_demo$immigrant)
cultural_demo$lowincome_pct <- curbcut:::convert_unit.pct(x = cultural_demo$lowincome_pct, decimal = 0)
cultural_demo$immigrant_pct <- curbcut:::convert_unit.pct(x = cultural_demo$immigrant_pct, decimal = 0)

names(cultural_demo) <- c("Installations culturelles (n)", "Faible revenu (n)", 
                      "Immigrants (n)", "Faible revenu (%)", 
                      "Immigrants (%)")
cultural_demo <- cultural_demo[c(1,2,4,3,5)]

# Add general population
cultural_demo$`Population (n)` <- curbcut::convert_unit(x = cultural_pop$pop)
cultural_demo$`Population (%)` <- curbcut:::convert_unit.pct(x = cultural_pop$pop / sum(cultural_pop$pop), decimal = 0)

# Format for the table
cultural_demo$`Immigrants (%)` <- gsub("%", "", cultural_demo$`Immigrants (%)`) |> as.numeric()
cultural_demo$`Faible revenu (%)` <- gsub("%", "", cultural_demo$`Faible revenu (%)`) |> as.numeric()
cultural_demo$`Population (%)` <- gsub("%", "", cultural_demo$`Population (%)`) |> as.numeric()
cultural_demo |> 
  gt() |> 
  data_color(
    columns = ends_with("%)"),
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = NULL
    )
  ) |> 
  fmt_number(
    columns = ends_with("%)"),
    suffixing = TRUE,
    pattern = "{x}%",
    decimals = 0,
  )
