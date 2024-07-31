source("R/01_startup.R")


# Grab supermarkets from OSM ----------------------------------------------

CMA <- cancensus::get_census("CA21", regions = list(CMA = 24462), level = "CMA",
                             geo_format = "sf")

#Setting the Laval bound box for maps
CMA_bbox <- sf::st_bbox(CMA)


library(osmdata)
supermarkets <- opq(bbox = CMA_bbox, timeout = 300) |> 
  add_osm_feature(key = "shop", value = "supermarket") |> 
  osmdata_sf()
supermarkets <- supermarkets$osm_points

# Remove supermarkets with no name (bug in classification?)
supermarkets <- supermarkets[!is.na(supermarkets$name), ]

tt <- ttm(under_x_minutes = 60)


# How long to get to the closest supermarket ------------------------------

# Take all Montreal's, as there are health establishement outside the island
CMA_DBs <- cancensus::get_census("CA21", regions = list(CMA = 24462), level = "DB",
                                 geo_format = "sf")
CMA <- cancensus::get_census("CA21", regions = list(CMA = 24462), level = "CMA",
                             geo_format = "sf")

supermarkets <- sf::st_transform(supermarkets, crs = sf::st_crs(CMA_DBs))
supermarkets <- sf::st_filter(supermarkets, CMA)
supermarkets <- sf::st_intersection(supermarkets, CMA_DBs["GeoUID"])

access_supermarkets <- cc.buildr::merge(tt, sf::st_drop_geometry(supermarkets), 
                                 by.x = "to", by.y = "GeoUID")

access_supermarkets <- 
  access_supermarkets |> 
  dplyr::group_by(from) |> 
  dplyr::filter(travel_seconds == min(travel_seconds)) |> 
  dplyr::select(from, travel_seconds) |> 
  distinct()

# Add dropped DBs for no accessibility
DBs <- cancensus::get_census("CA21", regions = list(CSD = 2465005), level = "DB",
                             geo_format = "sf")

no_access <- DBs$GeoUID[!DBs$GeoUID %in% access_supermarkets$from]
access_supermarkets <- rbind(access_supermarkets, tibble::tibble(from = no_access, travel_seconds = Inf))
access_supermarkets <- tibble::as_tibble(access_supermarkets)

# Add spatial features
access_supermarkets <- 
  access_supermarkets |> 
  dplyr::left_join(DBs["GeoUID"], c("from"= "GeoUID")) |> 
  sf::st_as_sf()


# Plot it -----------------------------------------------------------------

access_supermarkets$travel_mins <- access_supermarkets$travel_seconds / 60

labels <- c("0-10", "10-20", "20-30", "30-40", "40+")

# Add our bins in the data
access_supermarkets <- add_bins(df = access_supermarkets,
                         variable = "travel_mins",
                         breaks = c(0, 10, 20, 30, 40, Inf),
                         labels = labels
)

t <- access_supermarkets
t <- Reduce(rbind,
            split(t, t$binned_variable) |>
              lapply(\(x) {
                out <- tibble::tibble(x$binned_variable)
                out$geometry <- sf::st_union(x)
                sf::st_as_sf(out, crs = 4326)[1, ]
              })
) |> sf::st_as_sf()
names(t)[1] <- "binned_variable"

how_long_access_supermarkets <-
  t |> 
  ggplot() +
  gg_cc_tiles +
  geom_sf(aes(fill = binned_variable), color = "transparent") +
  scale_fill_manual(values = rev(curbcut_colors$left_5$fill[2:6]),
                    name = "Temps de marche (minutes)",
                    labels = labels,
                    guide = guide_legend(title.position = "top", 
                                         label.position = "bottom", nrow = 1)) +
  geom_sf(data = supermarkets, color = color_theme("purpletransport"),
          size = 0.8, alpha = 0.8) +
  gg_cc_theme +
  theme(legend.spacing.x = unit(2, 'cm'),
        legend.spacing.y = unit(1, 'cm'))


# How many supermarkets accessible ----------------------------------------

tt_15 <- ttm()

supermarkets$ID <- paste0("sm_", seq_along(supermarkets$osm_id))

hm_access_supermarket <- merge(tt_15, supermarkets[c("GeoUID", "ID")], 
                               by.x = "to", by.y = "GeoUID")

hm_access_supermarket <- unique(hm_access_supermarket[c("from", "ID")])
hm_access_supermarket <- table(hm_access_supermarket$from)

hm_access_supermarket <- 
  tibble::tibble(GeoUID = names(hm_access_supermarket),
                 supermarkets = as.vector(hm_access_supermarket))

no_access <- DBs$GeoUID[!DBs$GeoUID %in% hm_access_supermarket$GeoUID]
hm_access_supermarket <- rbind(hm_access_supermarket, tibble::tibble(GeoUID = no_access, supermarkets = 0))
hm_access_supermarket <- tibble::as_tibble(hm_access_supermarket)

# Add spatial features
hm_access_supermarket <- 
  hm_access_supermarket |> 
  dplyr::left_join(DBs["GeoUID"]) |> 
  sf::st_as_sf()


# Plot it -----------------------------------------------------------------

labels <- c("0", "1-2", "3+")

# Add our bins in the data
hm_access_supermarket <- add_bins(df = hm_access_supermarket,
                                  variable = "supermarkets",
                                  breaks = c(-Inf, 0.1, 2.1, Inf),
                                  labels = labels
)

# Union the features so the polygons don't show their borders. Might revisit
# with the addition of streets!
t <- Reduce(rbind,
            split(hm_access_supermarket, hm_access_supermarket$binned_variable) |>
              lapply(\(x) {
                out <- tibble::tibble(x$binned_variable)
                out$geometry <- sf::st_union(x)
                sf::st_as_sf(out, crs = 4326)[1, ]
              })
) |> sf::st_as_sf()
names(t)[1] <- "binned_variable"

variety_access_supermarkets <- 
  t |> 
  ggplot() +
  gg_cc_tiles +
  geom_sf(aes(fill = binned_variable), color = "transparent") +
  scale_fill_manual(values = curbcut_colors$left_5$fill[c(2, 4, 6)],
                    name = "Supermarchés accessibles (n)",
                    labels = labels,
                    guide = guide_legend(title.position = "top",
                                         label.position = "bottom", nrow = 1)) +
  geom_sf(data = supermarkets, color = color_theme("purpletransport"),
          size = 0.8, alpha = 0.8) +
  gg_cc_theme +
  theme(legend.spacing.x = unit(2, 'cm'),
        legend.spacing.y = unit(1, 'cm'))


# Bind two plots horizontally ---------------------------------------------

library(patchwork)

# Assuming how_long_access_supermarkets and variety_access_supermarkets are your two ggplot objects
combined_plot <- how_long_access_supermarkets + variety_access_supermarkets

# Display the combined plot
print(combined_plot)



# Add values --------------------------------------------------------------

supermarkets_pop <- merge(DBs[c("GeoUID", "Population")], 
                   sf::st_drop_geometry(access_supermarkets),
                   by.x = "GeoUID", by.y = "from")

supermarkets_pop <- 
  supermarkets_pop |> 
  sf::st_drop_geometry() |> 
  group_by(binned_variable) |> 
  summarize(pop = sum(Population))

# Make sure to use the same numbers as in the table shown!!!!
less15 <- sum(supermarkets_pop$pop[supermarkets_pop$binned_variable %in% c("0-10")])
less15 / lvl$Population
less30 <- sum(supermarkets_pop$pop[supermarkets_pop$binned_variable %in% c("0-10", "10-20")])
less30 / lvl$Population

# Look for more vulnerable population. lowincome and old age?
DAs <- cancensus::get_census("CA21", regions = list(CSD = 2465005), level = "DA",
                             vectors = c(lowincome = "v_CA21_1025",
                                         elderly = "v_CA21_251"),
                             geo_format = "sf")

DAs <- DAs[c("GeoUID", "Population", "lowincome", "elderly")]
names(DAs) <- c("DA_UID", "DA_pop", "lowincome", "elderly", "geometry")
DB_lowincome <- cc.buildr::merge(DBs[c("GeoUID", "DA_UID", "Population")], 
                                 sf::st_drop_geometry(DAs), by = "DA_UID")
DB_lowincome <- sf::st_drop_geometry(DB_lowincome)
DB_lowincome$pop_ratio <- DB_lowincome$Population / DB_lowincome$DA_pop
DB_lowincome$lowincome <- DB_lowincome$lowincome * DB_lowincome$pop_ratio
DB_lowincome$elderly <- DB_lowincome$elderly * DB_lowincome$pop_ratio
DB_lowincome <- DB_lowincome[c("GeoUID", "lowincome", "elderly")]

supermarkets_demo <- merge(DB_lowincome, 
                   sf::st_drop_geometry(access_supermarkets),
                   by.x = "GeoUID", by.y = "from")

supermarkets_demo <- 
  supermarkets_demo |> 
  sf::st_drop_geometry() |> 
  group_by(binned_variable) |> 
  summarize(lowincome = sum(lowincome, na.rm = TRUE),
            elderly = sum(elderly, na.rm = TRUE)) |> 
  mutate(lowincome_pct = lowincome / sum(lowincome), 
         elderly_pct = elderly / sum(elderly))

supermarkets_demo$lowincome <- convert_hundreds(x = supermarkets_demo$lowincome)
supermarkets_demo$elderly <- convert_hundreds(x = supermarkets_demo$elderly)
supermarkets_demo$lowincome_pct <- curbcut:::convert_unit.pct(x = supermarkets_demo$lowincome_pct, decimal = 0)
supermarkets_demo$elderly_pct <- curbcut:::convert_unit.pct(x = supermarkets_demo$elderly_pct, decimal = 0)
supermarkets_demo$binned_variable <- paste0(supermarkets_demo$binned_variable, " minutes")
supermarkets_demo$binned_variable <- gsub("-", " à ", supermarkets_demo$binned_variable)

names(supermarkets_demo) <- c("Temps de marche", "Faible revenu (n)", 
                      "65 ans et plus (n)", "Faible revenu (%)", 
                      "65 ans et plus (%)")
supermarkets_demo <- supermarkets_demo[c(1,2,4,3,5)]

# Add general population
supermarkets_demo$`Population (n)` <- curbcut::convert_unit(x = supermarkets_pop$pop)
supermarkets_demo$`Population (%)` <- curbcut:::convert_unit.pct(x = supermarkets_pop$pop / sum(supermarkets_pop$pop), decimal = 0)

# Format for the table
supermarkets_demo$`65 ans et plus (%)` <- gsub("%", "", supermarkets_demo$`65 ans et plus (%)`) |> as.numeric()
supermarkets_demo$`Faible revenu (%)` <- gsub("%", "", supermarkets_demo$`Faible revenu (%)`) |> as.numeric()
supermarkets_demo$`Population (%)` <- gsub("%", "", supermarkets_demo$`Population (%)`) |> as.numeric()

modified_columns <- c("Faible revenu (n)", "65 ans et plus (n)", "Population (n)")
supermarkets_demo <- supermarkets_demo |> mutate(across(all_of(modified_columns), ~ gsub(",", " ", .)))

grocery_table <- supermarkets_demo |> 
  gt() |> 
  tab_options(
    table.font.names = "KMR Apparat Regular",
    table.font.size = px(14)
  ) |>
  data_color(
    columns = ends_with("%)"),
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = NULL
    )
  ) |> 
  fmt(
    columns = ends_with("%)"),
    fns = function(x) {
      formatted <- sprintf("%.1f %%", x)
      gsub("\\.", ",", formatted)
    }
  )

CTs <- cancensus::get_census("CA21", regions = list(CSD = 2465005), level = "CT",
                             geo_format = "sf")

CTs <- sf::st_centroid(CTs)
CTs$ID <- CTs$GeoUID

cc.data::tt_calculate(centroids = CTs, max_dist = 5000)

# R Markdown --------------------------------------------------------------
grocery_maps <- combined_plot
grocery_data <- merge(DB_lowincome, 
                      sf::st_drop_geometry(access_supermarkets),
                      by.x = "GeoUID", by.y = "from") |> 
  sf::st_drop_geometry() |> 
  group_by(binned_variable) |> 
  summarize(lowincome = sum(lowincome, na.rm = TRUE),
            elderly = sum(elderly, na.rm = TRUE)) |> 
  mutate(lowincome_pct = lowincome / sum(lowincome), 
         elderly_pct = elderly / sum(elderly))

low_income_20 <- grocery_data |> 
  filter(binned_variable == "0-10" | binned_variable == "10-20") |> 
  summarise(lowincome_pct = sum(lowincome_pct)) |> 
  mutate(lowincome_pct = convert_pct(lowincome_pct)) |> 
  pull(lowincome_pct)
age_65_20 <- grocery_data |> 
  filter(binned_variable == "0-10" | binned_variable == "10-20") |> 
  summarise(elderly_pct = sum(elderly_pct)) |> 
  mutate(elderly_pct = convert_pct(elderly_pct)) |> 
  pull(elderly_pct)
population_20 <- grocery_data |> 
  mutate(population_pct = supermarkets_pop$pop / sum(supermarkets_pop$pop)) |> 
  filter(binned_variable == "0-10" | binned_variable == "10-20") |> 
  summarise(population_pct = sum(population_pct)) |> 
  mutate(population_pct = convert_pct(population_pct)) |> 
  pull(population_pct)

qs::qsavem(grocery_maps, grocery_table, low_income_20, age_65_20, population_20,
           file = "data/axe3/grocery.qsm")
