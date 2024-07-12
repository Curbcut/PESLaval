
source("R/01_startup.R")

# Load cultural facilities ------------------------------------------------

read_method <- function(file) {
  content <- utils::unzip(file, list = TRUE, exdir = tempdir())$Name
  csv_file <- content[grepl("\\.csv", content)]
  csv_file <- csv_file[!grepl("Data_Sources", csv_file)]
  utils::unzip(file, files = csv_file, exdir = tempdir())
  suppressWarnings(utils::read.csv(paste0(tempdir(), "\\", 
                                          csv_file)))
}
cultural <- tibble::as_tibble(cc.data::bucket_read_object(object = "open_db_cultural_facilities.zip", 
                                                          bucket = "curbcut.amenities", objectext = ".zip", method = read_method))
cultural$Longitude <- suppressWarnings(as.numeric(cultural$Longitude))
cultural <- cultural[!is.na(cultural$Longitude), ]
cultural$Latitude <- suppressWarnings(as.numeric(cultural$Latitude))
cultural <- cultural[!is.na(cultural$Latitude), ]
cultural <- sf::st_transform(sf::st_as_sf(cultural, 
                                          coords = c("Longitude", 
                                                     "Latitude"), 
                                          crs = 4326), 3347)

CMA <- cancensus::get_census("CA21", regions = list(CMA = 24462), level = "CMA",
                             geo_format = "sf")
CMA_DBs <- cancensus::get_census("CA21", regions = list(CMA = 24462), level = "DB",
                                 geo_format = "sf")

# Keep only the ones in the CMA
cultural <- sf::st_transform(cultural, crs = sf::st_crs(CMA_DBs))
cultural <- sf::st_filter(cultural, CMA)
cultural <- sf::st_intersection(cultural, CMA_DBs["GeoUID"])

tt <- ttm()

# How many cultural accessible --------------------------------------------

hm_access_cultural <- merge(tt, cultural[c("GeoUID", "Index")], 
                               by.x = "to", by.y = "GeoUID")

hm_access_cultural <- unique(hm_access_cultural[c("from", "Index")])
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

labels <- c("0", "1-2", "2+")

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
                    name = "Supermarchés accessibles (n)",
                    labels = labels,
                    guide = guide_legend(title.position = "top",
                                         label.position = "bottom", nrow = 1)) +
  geom_sf(data = cultural, color = color_theme("purpletransport"),
          size = 0.8, alpha = 0.8) +
  gg_cc_theme +
  theme(legend.spacing.x = unit(2, 'cm'),
        legend.spacing.y = unit(1, 'cm'))
