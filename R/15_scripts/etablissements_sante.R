
source("R/01_startup.R")

# Établissements du réseau de la santé et des services sociaux ------------

sante <- tempfile(fileext = ".csv")
download.file("https://www.donneesquebec.ca/recherche/dataset/51998b55-7d4c-4381-8c20-0ac1cd9c1b87/resource/2aa06e66-c1d0-4e2f-bf3c-c2e413c3f84d/download/installationscsv.csv",
              sante)
sante <- tibble::as_tibble(read.csv(sante))

etablissements <- tempfile(fileext = ".csv")
download.file("https://www.donneesquebec.ca/recherche/dataset/51998b55-7d4c-4381-8c20-0ac1cd9c1b87/resource/a1988030-1f8b-4c67-bc29-ca8b9f710afd/download/etablissementscsv.csv",
              etablissements)
etablissements <- tibble::as_tibble(read.csv(etablissements))

common_cols <- c(names(sante), names(etablissements))
common_cols <- names(table(common_cols)[table(common_cols) == 2]) |> unique()
sante <- rbind(etablissements[common_cols], sante[common_cols])

sante <- sf::st_as_sf(sante, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)


# Grab travel times (foot, under 15 minutes)
source("R/utils/tt_fun.R")
tt <- ttm(under_x_minutes = 60)

# Map avec densité de la population + 
#centre hospitalier de soins généraiux ou de services communautaires ? + 
# divers centre de réadaptation + CHSLD


# Attach a DB to health locations -----------------------------------------

# Take all Montreal's, as there are health establishement outside the island
CMA_DBs <- cancensus::get_census("CA21", regions = list(CMA = 24462), level = "DB",
                                 geo_format = "sf")
CMA <- cancensus::get_census("CA21", regions = list(CMA = 24462), level = "CMA",
                                 geo_format = "sf")

sante <- sf::st_transform(sante, crs = sf::st_crs(CMA_DBs))
sante <- sf::st_filter(sante, CMA)
sante <- sf::st_intersection(sante, CMA_DBs["GeoUID"])

access_sante <- cc.buildr::merge(tt, sf::st_drop_geometry(sante), 
                                 by.x = "to", by.y = "GeoUID")

access_sante <- 
  access_sante |> 
  dplyr::filter(CLSC == "Oui" | CHSGS == "Oui") |> 
  dplyr::group_by(from) |> 
  dplyr::filter(travel_seconds == min(travel_seconds)) |> 
  dplyr::select(from, travel_seconds) |> 
  distinct()

# Add dropped DBs for no accessibility
DBs <- cancensus::get_census("CA21", regions = list(CSD = 2465005), level = "DB",
                             geo_format = "sf")

no_access <- DBs$GeoUID[!DBs$GeoUID %in% access_sante$from]
access_sante <- rbind(access_sante, tibble::tibble(from = no_access, travel_seconds = Inf))
access_sante <- tibble::as_tibble(access_sante)

# Add spatial features
access_sante <- 
  access_sante |> 
  dplyr::left_join(DBs["GeoUID"], c("from"= "GeoUID")) |> 
  sf::st_as_sf()


# Plot it -----------------------------------------------------------------

access_sante$travel_mins <- access_sante$travel_seconds / 60

labels <- c("0-15", "15-30", "30-45", "45-60", "60+")

# Add our bins in the data
access_sante <- add_bins(df = access_sante,
                         variable = "travel_mins",
                         breaks = c(0, 15, 30, 45, 60, Inf),
                         labels = labels
)

# CLSC or CHSGS?
general_soins <- dplyr::filter(sante, CLSC == "Oui" | CHSGS == "Oui")
general_soins <- general_soins |> 
  mutate(centre = case_when(CLSC == "Oui" ~ "CSLS", .default = "CHSGS"))

t <- access_sante
# t <- Reduce(rbind,
#             split(t, t$binned_variable) |>
#               lapply(\(x) {
#                 out <- tibble::tibble(x$binned_variable)
#                 out$geometry <- sf::st_union(x)
#                 sf::st_as_sf(out, crs = 4326)[1, ]
#               })
# ) |> sf::st_as_sf()
# names(t)[1] <- "binned_variable"

healthcare_map <- t |> 
  ggplot() +
  gg_cc_tiles +
  geom_sf(aes(fill = binned_variable), color = "transparent") +
  scale_fill_manual(values = rev(curbcut_colors$left_5$fill[2:6]),
                    name = "Temps de marche (minutes)",
                    labels = labels,
                    guide = guide_legend(title.position = "top", 
                                         label.position = "bottom", nrow = 1)) +
  geom_sf(data = general_soins, aes(color = centre)) +
  scale_color_manual(values = c(color_theme("pinkhealth"),
                                color_theme("greenecology")),
                     name = "Type de centre de santé",
                     guide = guide_legend(title.position = "top", 
                                          label.position = "bottom", nrow = 1,
                                          override.aes = list(size = 5, stroke = 0.5))) +
  gg_cc_theme +
  theme(legend.spacing.x = unit(2, 'cm'),
        legend.spacing.y = unit(1, 'cm'))

ggplot2::ggsave(filename = here::here("output/axe3/healthcare_map.pdf"), 
                plot = healthcare_map, width = 6.5, height = 5, bg = "transparent")
  
  
  
# Centre de réadaptation divers -------------------------------------------

cr <- c("CRDITED", "CRDPA", "CRDPM", "CRDPV", "CRDPL", "CRJDA", "CRMDA", "CRD")

sante_laval <- sante[sante$RSS_NOM == "Laval", ] |> sf::st_drop_geometry()
sante_laval[sante_laval == "Oui"] <- "1"
sante_laval[sante_laval == "Non"] <- "0"
sante_laval <- sante_laval[cr]

sante_laval[] <- lapply(sante_laval, as.numeric)

sum(rowSums(sante_laval) > 0 )


# Values ------------------------------------------------------------------

sante_pop <- merge(DBs[c("GeoUID", "Population")], 
                   sf::st_drop_geometry(access_sante),
                   by.x = "GeoUID", by.y = "from")

sante_pop <- 
  sante_pop |> 
  sf::st_drop_geometry() |> 
  group_by(binned_variable) |> 
  summarize(pop = sum(Population))

# Make sure to use the same numbers as in the table shown!!!!
less15 <- sum(sante_pop$pop[sante_pop$binned_variable %in% c("0-15")])
less15 / lvl$Population
less30 <- sum(sante_pop$pop[sante_pop$binned_variable %in% c("0-15", "15-30")])
less30 / lvl$Population

# Look for more vulnerable population. Children and old age?
DAs <- cancensus::get_census("CA21", regions = list(CSD = 2465005), level = "DA",
                             vectors = c(children = "v_CA21_11",
                                         elderly = "v_CA21_251"),
                             geo_format = "sf")

DAs <- DAs[c("GeoUID", "Population", "children", "elderly")]
names(DAs) <- c("DA_UID", "DA_pop", "children", "elderly", "geometry")
DB_children <- cc.buildr::merge(DBs[c("GeoUID", "DA_UID", "Population")], 
                                sf::st_drop_geometry(DAs), by = "DA_UID")
DB_children <- sf::st_drop_geometry(DB_children)
DB_children$pop_ratio <- DB_children$Population / DB_children$DA_pop
DB_children$children <- DB_children$children * DB_children$pop_ratio
DB_children$elderly <- DB_children$elderly * DB_children$pop_ratio
DB_children <- DB_children[c("GeoUID", "children", "elderly")]

sante_age <- merge(DB_children, 
                   sf::st_drop_geometry(access_sante),
                   by.x = "GeoUID", by.y = "from")

sante_age <- 
  sante_age |> 
  sf::st_drop_geometry() |> 
  group_by(binned_variable) |> 
  summarize(children = sum(children, na.rm = TRUE),
            elderly = sum(elderly, na.rm = TRUE)) |> 
  mutate(children_pct = children / sum(children), 
         elderly_pct = elderly / sum(elderly))

sante_age[c(0,1,4,3,5)]

less30 <- sum(sante_age$children[sante_age$binned_variable %in% c("0-15", "15-30")])

sante_age$children <- curbcut::convert_unit(x = sante_age$children)
sante_age$elderly <- curbcut::convert_unit(x = sante_age$elderly)
sante_age$children_pct <- curbcut:::convert_unit.pct(x = sante_age$children_pct, decimal = 0)
sante_age$elderly_pct <- curbcut:::convert_unit.pct(x = sante_age$elderly_pct, decimal = 0)
sante_age$binned_variable <- paste0(sante_age$binned_variable, " minutes")
sante_age$binned_variable <- gsub("-", " à ", sante_age$binned_variable)

names(sante_age) <- c("Temps de marche", "0 à 14 ans (n)", "65 ans et plus (n)",
                      "0 à 14 ans (%)", "65 ans et plus (%)")
sante_age <- sante_age[c(1,2,4,3,5)]

# Add general population
sante_age$`Population (n)` <- curbcut::convert_unit(x = sante_pop$pop)
sante_age$`Population (%)` <- curbcut:::convert_unit.pct(x = sante_pop$pop / sum(sante_pop$pop), decimal = 0)

# Format for the table
sante_age$`0 à 14 ans (%)` <- gsub("%", "", sante_age$`0 à 14 ans (%)`) |> as.numeric()
sante_age$`65 ans et plus (%)` <- gsub("%", "", sante_age$`65 ans et plus (%)`) |> as.numeric()
sante_age$`Population (%)` <- gsub("%", "", sante_age$`Population (%)`) |> as.numeric()

sante_age_modified_columns <- c("0 à 14 ans (n)", "65 ans et plus (n)", "Population (n)")
sante_age <- sante_age |> mutate(across(all_of(sante_age_modified_columns), ~ gsub(",", " ", .)))

healthcare_table <- sante_age |> 
  gt() |> 
  data_color(
    columns = vars(`0 à 14 ans (%)`, `65 ans et plus (%)`, `Population (%)`),
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
  ) |> 
  # Appliquer le style de la police à toute la table
  tab_style(
    style = cell_text(
      font = "KMR Apparat Regular"
    ),
    locations = cells_body()
  ) |> 
  tab_style(
    style = cell_text(
      font = "KMR Apparat Regular"
    ),
    locations = cells_column_labels()
  ) |> 
  # Options générales pour la table
  tab_options(
    table.font.size = indesign_fontsize,
    row_group.font.size = indesign_title_fontsize
  )

gtsave(healthcare_table, "output/axe3/healthcare_table.pdf")


# R Markdown --------------------------------------------------------------
sante_age_uncleaned <- merge(DB_children, 
              sf::st_drop_geometry(access_sante),
              by.x = "GeoUID", by.y = "from") |> 
  sf::st_drop_geometry() |> 
  group_by(binned_variable) |> 
  summarize(children = sum(children, na.rm = TRUE),
            elderly = sum(elderly, na.rm = TRUE)) |> 
  mutate(children_pct = children / sum(children), 
         elderly_pct = elderly / sum(elderly))

laval_pop <- get_census(dataset = "CA21", 
                        regions = list(CSD = c(2465005)), 
                        level = "CSD") |> 
  pull(Population)

less_15 <- convert_number(less15)
less_15_prop <- convert_pct(less15 / laval_pop)
less_30 <- sante_pop |> 
  filter(binned_variable == "0-15" | binned_variable == "15-30") |> 
  summarise(pop = sum(pop)) |> 
  mutate(pop = convert_number(pop)) |> 
  pull(pop)
less_30_prop <- sante_pop |> 
  filter(binned_variable == "0-15" | binned_variable == "15-30") |> 
  summarise(pop = sum(pop)) |> 
  mutate(pop = convert_pct(pop/ laval_pop)) |> 
  pull(pop)
more_60 <- sante_pop |> 
  filter(binned_variable == "60+") |> 
  summarise(pop = sum(pop)) |> 
  mutate(pop = convert_number(pop)) |> 
  pull(pop)
more_60_pct <- sante_pop |> 
  filter(binned_variable == "60+") |> 
  summarise(pop = sum(pop)) |> 
  mutate(pop = convert_pct(pop / laval_pop)) |> 
  pull(pop)

less_15_child <- sante_age_uncleaned |> 
  filter(binned_variable == "0-15") |> 
  mutate(children = convert_number(children)) |> 
  pull(children)
less_15_child_pct <- sante_age_uncleaned |> 
  filter(binned_variable == "0-15") |> 
  mutate(children_pct = convert_pct(children_pct)) |> 
  pull(children_pct)
less_30_child <- sante_age_uncleaned |> 
  filter(binned_variable == "0-15" | binned_variable == "15-30") |> 
  summarise(children = sum(children)) |> 
  mutate(children = convert_number(children)) |> 
  pull(children)
less_30_child_pct <- sante_age_uncleaned |> 
  filter(binned_variable == "0-15" | binned_variable == "15-30") |> 
  summarise(children_pct = sum(children_pct)) |> 
  mutate(children_pct = convert_pct(children_pct)) |> 
  pull(children_pct)
more_60_child <- sante_age_uncleaned |> 
  filter(binned_variable == "60+") |> 
  mutate(children = convert_number(children)) |> 
  pull(children)
more_60_child_pct <- sante_age_uncleaned |> 
  filter(binned_variable == "60+") |> 
  mutate(children_pct = convert_pct(children_pct)) |> 
  pull(children_pct)

qs::qsavem(healthcare_map, healthcare_table, less_15, less_15_prop, less_30,
           less_30_prop, more_60, more_60_pct, less_15_child, less_15_child_pct, less_30_child,
           less_30_child_pct, more_60_child, more_60_child_pct,
           file = "data/axe3/healthcare.qsm")
