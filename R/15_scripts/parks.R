source("R/01_startup.R")


# Load the parks ----------------------------------------------------------

parks <- sf::read_sf("data/axe3/locations/parc_espace_vert_20240607_PG.shp")
parks$area <-  cc.buildr::get_area(parks)
# Only keep the ones with more than 2000m^2
parks <- parks[parks$area >= 2000, ]

# Travel time matrix (default, 15 minutes)
tt <- ttm()


# Calculate m^2 of parks accessible ---------------------------------------

DBs <- cancensus::get_census("CA21", regions = list(CSD = 2465005), 
                             level = "DB", geo_format = "sf")
parks <- sf::st_transform(parks, sf::st_crs(DBs))
parks <- sf::st_make_valid(parks)

# If one can access a piece of the park that's in a DB they can access in 15
# minutes, then they have access to the park.
access <- function(x) {
  park <- parks[x, ]
  inters <- sf::st_intersection(DBs, park)$GeoUID
  unique(tt$from[tt$to %in% inters])
}

which_have_access <- lapply(seq_along(parks$geometry), access)

which_have_access <- table(unlist(which_have_access))
parks_access <- tibble::tibble(GeoUID = names(which_have_access),
                               parks = as.vector(which_have_access))

no_access <- DBs$GeoUID[!DBs$GeoUID %in% parks_access$GeoUID]
parks_access <- rbind(parks_access, tibble::tibble(GeoUID = no_access, parks = 0))
parks_access <- tibble::as_tibble(parks_access)

parks_access <- cc.buildr::merge(parks_access, DBs[c("GeoUID", "geometry")])


# Map it ------------------------------------------------------------------

labels <- c("0", "1", "2", "4", "5+")

# Add our bins in the data
parks_access <- add_bins(df = parks_access,
                         variable = "parks",
                         breaks = c(-Inf, 0.1, 1.1, 2.1, 4.1, Inf),
                         labels = labels
)

# Union the features so the polygons don't show their borders. Might revisit
# with the addition of streets!
t <- Reduce(rbind,
       split(parks_access, parks_access$binned_variable) |>
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
  scale_fill_manual(values = curbcut_colors$left_5$fill[2:6],
                    name = "Parcs accessibles (n)",
                    labels = labels,
                    guide = guide_legend(title.position = "top",
                                         label.position = "bottom", nrow = 1)) +
  geom_sf(data = parks, fill = color_theme("greenecology"),
          color = curbcut_colors$left_5$fill[6]) +
  gg_cc_theme +
  theme(legend.spacing.x = unit(2, 'cm'),
        legend.spacing.y = unit(1, 'cm'))


# Values ------------------------------------------------------------------

parks_pop <- merge(DBs[c("GeoUID", "Population")], 
                   sf::st_drop_geometry(parks_access),
                   by = "GeoUID")

parks_pop <- 
  parks_pop |> 
  sf::st_drop_geometry() |> 
  group_by(binned_variable) |> 
  summarize(pop = sum(Population))

# Make sure to use the same numbers as in the table shown!!!!
less15 <- sum(parks_pop$pop[parks_pop$binned_variable %in% c("0")])
less15 / lvl$Population
less30 <- sum(parks_pop$pop[parks_pop$binned_variable %in% c("4", "5+")])
less30 / lvl$Population

# Look for more vulnerable population. lowincome and old age?
DAs <- cancensus::get_census("CA21", regions = list(CSD = 2465005), level = "DA",
                             vectors = c(lowincome = "v_CA21_1025",
                                         withchildren = "v_CA21_502"),
                             geo_format = "sf")

DAs <- DAs[c("GeoUID", "Population", "lowincome", "withchildren")]
names(DAs) <- c("DA_UID", "DA_pop", "lowincome", "withchildren", "geometry")
DB_lowincome <- cc.buildr::merge(DBs[c("GeoUID", "DA_UID", "Population")], 
                                sf::st_drop_geometry(DAs), by = "DA_UID")
DB_lowincome <- sf::st_drop_geometry(DB_lowincome)
DB_lowincome$pop_ratio <- DB_lowincome$Population / DB_lowincome$DA_pop
DB_lowincome$lowincome <- DB_lowincome$lowincome * DB_lowincome$pop_ratio
DB_lowincome$withchildren <- DB_lowincome$withchildren * DB_lowincome$pop_ratio
DB_lowincome <- DB_lowincome[c("GeoUID", "lowincome", "withchildren")]

park_demo <- merge(DB_lowincome, 
                   sf::st_drop_geometry(parks_access),
                   by = "GeoUID")

park_demo <- 
  park_demo |> 
  sf::st_drop_geometry() |> 
  group_by(binned_variable) |> 
  summarize(lowincome = sum(lowincome, na.rm = TRUE),
            withchildren = sum(withchildren, na.rm = TRUE)) |> 
  mutate(lowincome_pct = lowincome / sum(lowincome), 
         withchildren_pct = withchildren / sum(withchildren))

park_demo$lowincome <- convert_hundreds(x = park_demo$lowincome)
park_demo$withchildren <- convert_hundreds(x = park_demo$withchildren)
park_demo$lowincome_pct <- curbcut:::convert_unit.pct(x = park_demo$lowincome_pct, decimal = 0)
park_demo$withchildren_pct <- curbcut:::convert_unit.pct(x = park_demo$withchildren_pct, decimal = 0)

names(park_demo) <- c("Parcs accessibles (n)", "Faible revenu (n)", 
                      "Familles avec enfant(s) (n)", "Faible revenu (%)", 
                      "Familles avec enfant(s) (%)")
park_demo <- park_demo[c(1,2,4,3,5)]

# Add general population
park_demo$`Population (n)` <- curbcut::convert_unit(x = parks_pop$pop)
park_demo$`Population (%)` <- curbcut:::convert_unit.pct(x = parks_pop$pop / sum(parks_pop$pop), decimal = 0)

# Format for the table
park_demo$`Familles avec enfant(s) (%)` <- gsub("%", "", park_demo$`Familles avec enfant(s) (%)`) |> as.numeric()
park_demo$`Faible revenu (%)` <- gsub("%", "", park_demo$`Faible revenu (%)`) |> as.numeric()
park_demo$`Population (%)` <- gsub("%", "", park_demo$`Population (%)`) |> as.numeric()
park_demo |> 
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




