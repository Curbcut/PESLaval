#Loading up libraries
source("R/01_startup.R")
library(qs)

DBs <- cancensus::get_census("CA21", regions = list(CSD = 2465005), level = "DB",
                             geo_format = "sf")

# TTM Import ------------------------------------------------------------
ttm_walk_15 <- ttm()

# Data Processing ---------------------------------------------------------
# municipal_csv <- read.csv(paste0("https://www.donneesquebec.ca/recherche/dataset",
#                                  "/fddf1658-248e-49d6-99ed-4899a737f14a/resource",
#                                  "/f5ad512e-2451-4f2d-b6ff-1d5cc8b618f9/download",
#                                  "/lieux.csv")) |> 
#   filter(categorie.id != "Lieu privé") |> 
#   mutate(latitude = case_when(id.lieu == 525 ~ 45.55916, id.lieu == 540 ~ 45.55813,
#                               id.lieu == 549 ~ 45.55613, TRUE ~ latitude),
#          longitude = case_when(id.lieu == 525 ~  -73.84607, id.lieu == 540 ~ -73.74693,
#                                id.lieu == 549 ~ -73.72122, TRUE ~ longitude)) |> 
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
#   select(type.commun) |> 
#   mutate(type.commun = case_when(#Editing types of public places to those we want
#                                  type.commun == "Autres" ~ "Places publiques",
#                                  type.commun == "Belvédère" ~ "Places publiques",
#                                  type.commun == "Centres et galeries d'art" ~ "Places publiques",
#                                  type.commun == "Édifice municipal" ~ "Places publiques",
#                                  type.commun == "Piscine extérieure" ~ "Places publiques", 
#                                  type.commun == "Piscine intérieure" ~ "Places publiques",
#                                  type.commun == "Jeux d'eau" ~ "Sites avec équipement de parcs",
#                                  type.commun == "Patinoire extérieure" ~ "Sites avec équipement de parcs",
#                                  TRUE ~ type.commun)) |> 
#   filter(type.commun != "Berge",
#          type.commun != "Bois",
#          type.commun != "Bureau municipal lavallois",
#          type.commun != "Caserne de pompiers",
#          type.commun != "Gare",
#          type.commun != "Halte",
#          type.commun != "Parc",
#          type.commun != "Poste de police de quartier",
#          type.commun != "Station de métro")

#All in One
municipal_csv <- read.csv(paste0("https://www.donneesquebec.ca/recherche/dataset",
                                 "/fddf1658-248e-49d6-99ed-4899a737f14a/resource",
                                 "/f5ad512e-2451-4f2d-b6ff-1d5cc8b618f9/download",
                                 "/lieux.csv")) |> 
  filter(categorie.id != "Lieu privé",
         type.commun != "Parc") |> 
  mutate(latitude = case_when(id.lieu == 525 ~ 45.55916, id.lieu == 540 ~ 45.55813,
                              id.lieu == 549 ~ 45.55613, TRUE ~ latitude),
         longitude = case_when(id.lieu == 525 ~  -73.84607, id.lieu == 540 ~ -73.74693,
                               id.lieu == 549 ~ -73.72122, TRUE ~ longitude)) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  select(type.commun)

municipal_csv <- sf::st_join(municipal_csv, DBs["GeoUID"])

equipment_count <- left_join(sf::st_drop_geometry(municipal_csv), ttm_walk_15, 
                             by = c("GeoUID" = "from"),
          relationship = "many-to-many") |> 
  count(to) |> 
  tibble::as_tibble() |> 
  full_join(DBs["GeoUID"], by = c("to" = "GeoUID")) |> 
  sf::st_as_sf() |> 
  mutate(n = if_else(is.na(n), 0, n))

# Maps --------------------------------------------------------------------

#Mapping municipal accessibility
municipal_map <- ggplot(equipment_count) +
  gg_cc_tiles +
  geom_sf(aes(fill = cut(n, breaks = c(-Inf, 0, 3, 5, 7, Inf),
                         labels = c("0", "1-3", "3-5", "5-7", "> 7"))), color = NA) +
  scale_fill_manual(values = curbcut_colors$left_5$fill[2:6], 
                    na.value = curbcut_colors$left_5$fill[7], 
                    name = "Nombre d'édifices et lieux\nmunicipaux accessibles") +
  geom_sf(data = municipal_csv, aes(color = " "), 
          size = 0.8, alpha = 0.6) +
  scale_color_manual(values = c(" " = "#CD718C"),
                     name = "Lieux et édifices\n municipaux",
                     guide = guide_legend(title.position = "top", label.position = "bottom", nrow = 1,
                                          override.aes = list(size = 5, stroke = 0.5))) +
  labs(color = "") +
  gg_cc_theme +
  guides(
    fill = guide_legend(label.position = "bottom", title.position = "top")) +
  theme(legend.spacing.x = unit(2, 'cm'),
        legend.spacing.y = unit(1, 'cm'))

ggplot2::ggsave(filename = here::here("output/axe3/municipal_map.pdf"), 
                plot = municipal_map, width = 6.5, height = 5.5)

# Tables ------------------------------------------------------------------

# table_sf <- get_census(dataset = "CA21", 
#                        regions = list(CSD = 2465005), 
#                        level = "DA",
#                        vectors = c("low" ="v_CA21_1025", "immigrant" = "v_CA21_4410"),
#                        geo_format = "sf")
# names(table_sf)[names(table_sf) == "Population"] <- "Pop_DA"
# table_sf <- left_join(DBs[c("GeoUID", "DA_UID", "Population")], 
#                     sf::st_drop_geometry(table_sf), by = c("DA_UID" = "GeoUID"))
# table_sf <- sf::st_drop_geometry(table_sf)
# table_sf$pop_ratio <- table_sf$Population / table_sf$Pop_DA
# table_sf$low <- as.numeric(table_sf$low) * table_sf$pop_ratio
# table_sf$immigrant <- as.numeric(table_sf$immigrant) * table_sf$pop_ratio
# 
# DA_demo <- table_sf[c("GeoUID", "Population", "low", "immigrant")]
# 
# equipment_demo_j <- left_join(DA_demo, sf::st_drop_geometry(equipment_count), by = c("GeoUID" = "to")) 
# 
# equipment_demo <- 
#   equipment_demo_j|> 
#   mutate(`Lieux et édifices municipaux accessibles (n)` = 
#            case_when(n == 0 ~ "0",
#                      n >= 1 & n <= 2 ~ "1-2",
#                      n > 2 & n <= 6 ~ "3-6",
#                      n > 6 & n <= 10 ~ "7-10",
#                      n > 10 ~ "> 10")) |> 
#   group_by(`Lieux et édifices municipaux accessibles (n)`) |> 
#   summarize(`Population (n)` = sum(Population, na.rm = TRUE),
#             `Population (%)` = `Population (n)` / sum(DA_demo$Population, na.rm = TRUE),
#             `Immigrants (n)` = sum(immigrant, na.rm = TRUE),
#             `Immigrants (%)` = `Immigrants (n)` / sum(DA_demo$immigrant, na.rm = TRUE),
#             `Faible revenu (n)` = sum(low, na.rm = TRUE),
#             `Faible revenu (%)` = `Faible revenu (n)` / sum(DA_demo$low, na.rm = TRUE))
# 
# 
# municipal_table <- equipment_demo |> 
#   gt() |> 
#   fmt(columns = c(2,4,6), fns = convert_number_tens) |> 
#   fmt(columns = c(3,5,7), fns = convert_pct) |> 
#   data_color(
#     columns = c(3,5,7),
#     colors = scales::col_numeric(
#       palette = c("white", color_theme("purpletransport")),
#       domain = NULL
#     )
#   ) |> 
#   # Appliquer le style de la police à toute la table
#   tab_style(
#     style = cell_text(
#       font = "KMR-Apparat-Regular"
#     ),
#     locations = cells_body()
#   ) |> 
#   tab_style(
#     style = cell_text(
#       font = "KMR-Apparat-Regular"
#     ),
#     locations = cells_column_labels()
#   ) |> 
#   # Options générales pour la table
#   tab_options(
#     table.font.size = 12,
#     row_group.font.size = 12,
#     table.width = px(6 * 96)
#   )

table_sf <- get_census(dataset = "CA21", 
                       regions = list(CSD = 2465005), 
                       level = "DA",
                       vectors = c("low" = "v_CA21_1025"),  # Removed immigrant vector
                       geo_format = "sf")

names(table_sf)[names(table_sf) == "Population"] <- "Pop_DA"
table_sf <- left_join(DBs[c("GeoUID", "DA_UID", "Population")], 
                      sf::st_drop_geometry(table_sf), by = c("DA_UID" = "GeoUID"))
table_sf <- sf::st_drop_geometry(table_sf)
table_sf$pop_ratio <- table_sf$Population / table_sf$Pop_DA
table_sf$low <- as.numeric(table_sf$low) * table_sf$pop_ratio

# Keep only Population 
DA_demo <- table_sf[c("GeoUID", "Population")]

# Join with equipment data
equipment_demo_j <- left_join(DA_demo, sf::st_drop_geometry(equipment_count), by = c("GeoUID" = "to"))

# Remove immigrant and low income calculations
equipment_demo <- 
  equipment_demo_j |> 
  mutate(`Lieux et édifices municipaux accessibles (n)` = 
           case_when(n == 0 ~ "0",
                     n >= 1 & n <= 2 ~ "1-2",
                     n > 2 & n <= 6 ~ "3-6",
                     n > 6 & n <= 10 ~ "7-10",
                     n > 10 ~ "> 10")) |> 
  group_by(`Lieux et édifices municipaux accessibles (n)`) |> 
  summarize(`Population (n)` = sum(Population, na.rm = TRUE),
            `Population (%)` = `Population (n)` / sum(DA_demo$Population, na.rm = TRUE))

# Generate the final table without immigrant columns
municipal_table <- equipment_demo |> 
  gt() |> 
  fmt(columns = `Population (n)`, fns = convert_number_tens) |>  
  fmt(columns = `Population (%)`, fns = convert_pct) |> 
  data_color(
    columns = `Population (%)`,
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = NULL
    )
  ) |> 
  # Apply font style
  tab_style(
    style = cell_text(
      font = "KMR-Apparat-Regular"
    ),
    locations = cells_body()
  ) |> 
  tab_style(
    style = cell_text(
      font = "KMR-Apparat-Regular"
    ),
    locations = cells_column_labels()
  ) |> 
  # General table settings
  tab_options(
    table.font.size = 12,
    row_group.font.size = 12,
    table.width = px(6 * 96)
  )


gtsave(municipal_table, "output/axe3/municipal_table.png", zoom = 3)


municipal_population <- sum(DA_demo$Population, na.rm = TRUE)
municipal_lowincome <- sum(DA_demo$low, na.rm = TRUE)
municipal_immigrant <- sum(DA_demo$immigrant, na.rm = TRUE)

municipal_access <- equipment_demo |> 
  filter(`Lieux et édifices municipaux accessibles (n)` != 0) |> 
  summarise(Population = sum(`Population (n)`, na.rm = TRUE),
            Population = convert_pct(Population / municipal_population)) |> 
  pull(Population)

municipal_access_three <- equipment_demo |> 
  filter(!`Lieux et édifices municipaux accessibles (n)` %in% c(0, "1-2")) |> 
  summarise(Population = sum(`Population (n)`, na.rm = TRUE),
            Population = convert_pct(Population / municipal_population)) |> 
  pull(Population)

municipal_immigrant_ratio <- weighted_mean(equipment_demo_j$n, equipment_demo_j$immigrant, na.rm = TRUE) |> 
  round(digits = 2)
municipal_population_ratio <- weighted_mean(equipment_demo_j$n, equipment_demo_j$Population, na.rm = TRUE) |> 
  round(digits = 2)
municipal_lowincome_ratio <- weighted_mean(equipment_demo_j$n, equipment_demo_j$low, na.rm = TRUE) |> 
  round(digits = 2)

municipal_lowincome_lowaccess <- equipment_demo_j |> 
  filter(n < 4) |> 
  summarise(low = sum(low, na.rm = TRUE)) |> 
  mutate(low = convert_pct(low / municipal_lowincome)) |> 
  pull(low)

municipal_immigrant_lowaccess <- equipment_demo_j |> 
  filter(n < 4) |> 
  summarise(imm = sum(immigrant, na.rm = TRUE)) |> 
  mutate(imm = convert_pct(imm / municipal_immigrant)) |> 
  pull(imm)

municipal_pop_lowaccess <- equipment_demo_j |> 
  filter(n < 4) |> 
  summarise(pop = sum(Population, na.rm = TRUE)) |> 
  mutate(pop = convert_pct(pop / municipal_population)) |> 
  pull(pop)

# Table Breakdown ---------------------------------------------------------
ttm_60 <- qread("data/ttm_foot_60.qs")

access_breakdown <- left_join(sf::st_drop_geometry(municipal_csv), ttm_60, 
                       by = c("GeoUID" = "to"),
                       relationship = "many-to-many") |> 
  mutate(
    breakdown = cut(
      travel_seconds,
      breaks = c(-Inf, 900, 1800, 2700, 3600, Inf),
      labels = c("0–15", "15–30", "30–45", "45–60", "60+"),
      right = FALSE,
      ordered_result = TRUE   # <-- ensures it’s ordered
    )
  ) |> 
  group_by(from) |>
  summarise(breakdown = min(breakdown)) |> 
  ungroup()

access_sector <- DBs |> 
  left_join(access_breakdown, by = c("GeoUID" = "from")) |> 
  mutate(
    breakdown = ifelse(is.na(breakdown), "60+", as.character(breakdown)),
    breakdown = factor(breakdown, levels = c("0–15", "15–30", "30–45", "45–60", "60+"), ordered = TRUE)
  ) |> 
  select(GeoUID, Population, breakdown) |> 
  st_join(laval_sectors |> select(name), join = st_intersects)

access_60 <- access_sector |> 
  st_drop_geometry() |> 
  group_by(name, breakdown) |> 
  summarise(total_population = sum(Population, na.rm = TRUE), .groups = "drop") |> 
  complete(
    name,
    breakdown = factor(c("0–15", "15–30", "30–45", "45–60", "60+"),
                       levels = c("0–15", "15–30", "30–45", "45–60", "60+"),
                       ordered = TRUE),
    fill = list(total_population = 0)
  ) |> 
  group_by(name) |> 
  mutate(percentage = (total_population / sum(total_population))) |> 
  arrange(name, breakdown) |> 
  ungroup() |> 
  pivot_wider(
    names_from = breakdown,
    values_from = c(total_population, percentage),
    names_glue = "{breakdown} ({.value})"
  ) |> 
  rename(
    "0 à 15 minutes (n)"  = `0–15 (total_population)`,
    "0 à 15 minutes (%)"  = `0–15 (percentage)`,
    "15 à 30 minutes (n)" = `15–30 (total_population)`,
    "15 à 30 minutes (%)" = `15–30 (percentage)`,
    "30 à 45 minutes (n)" = `30–45 (total_population)`,
    "30 à 45 minutes (%)" = `30–45 (percentage)`,
    "45 à 60 minutes (n)" = `45–60 (total_population)`,
    "45 à 60 minutes (%)" = `45–60 (percentage)`,
    "60+ minutes (n)"     = `60+ (total_population)`,
    "60+ minutes (%)"     = `60+ (percentage)`
  ) |> 
  select(name, `0 à 15 minutes (n)`, `0 à 15 minutes (%)`, `15 à 30 minutes (n)`, `15 à 30 minutes (%)`,
         `30 à 45 minutes (n)`, `30 à 45 minutes (%)`, `45 à 60 minutes (n)`, `45 à 60 minutes (%)`,
         `60+ minutes (n)`,  `60+ minutes (%)`)

municipal_access_table <- access_60 |> 
  gt() |> 
  cols_label(name = "") |> 
  fmt(columns = c(2,4,6,8,10), fns = convert_number_tens) |>  
  fmt(columns = c(3, 5, 7, 9, 11), fns = convert_pct) |> 
  data_color(
    columns = c(3, 5, 7, 9, 11),
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = NULL
    )
  ) |>
  tab_style(
    style = cell_text(
      font = "KMR Apparat"
    ),
    locations = cells_body()
  ) |> 
  tab_style(
    style = cell_text(
      font = "KMR Apparat"
    ),
    locations = cells_column_labels()
  ) |> 
  # General table settings
  tab_options(
    table.font.size = 12,
    row_group.font.size = 12
  ) |> 
  cols_width(1 ~ px(168),
             2:11 ~ px(80))

gtsave(municipal_access_table, "output/axe3/municipal_access_table.png", zoom = 3)

# 15 minute sector --------------------------------------------------------
#Run lines 94-191 first
access_15_sector <- equipment_demo_j |> 
  left_join(DBs, by = "GeoUID") |> 
  st_as_sf() |>
  select(GeoUID, Population.x, n) |> 
  st_join(laval_sectors |> select(name), join = st_intersects) |> 
  mutate(
    breakdown = cut(
      n,
      breaks = c(-Inf, 0.1, 2.1, 6.1, 10.1, Inf),
      labels = c("0", "1–2", "3–6", "7–10", "> 10"),
      right = FALSE,
      ordered_result = TRUE   # <-- ensures it’s ordered
    )
  ) |> 
  st_drop_geometry() |> 
  group_by(name, breakdown) |> 
  summarise(total_population = sum(Population.x, na.rm = TRUE), .groups = "drop") |> 
  complete(
    name,
    breakdown = factor(c("0", "1–2", "3–6", "7–10", "> 10"),
                       levels = c("0", "1–2", "3–6", "7–10", "> 10"),
                       ordered = TRUE),
    fill = list(total_population = 0)
  ) |> 
  group_by(name) |> 
  mutate(percentage = (total_population / sum(total_population))) |> 
  arrange(name, breakdown) |> 
  ungroup() |> 
  pivot_wider(
    names_from = breakdown,
    values_from = c(total_population, percentage),
    names_glue = "{breakdown} ({.value})"
  ) |> 
  rename(
    "0 (n)"  = `0 (total_population)`,
    "0 (%)"  = `0 (percentage)`,
    "1-2 (n)" = `1–2 (total_population)`,
    "1-2 (%)" = `1–2 (percentage)`,
    "3-6 (n)" = `3–6 (total_population)`,
    "3-6 (%)" = `3–6 (percentage)`,
    "7-10 (n)" = `7–10 (total_population)`,
    "7-10 (%)" = `7–10 (percentage)`,
    "> 10 (n)" = `> 10 (total_population)`,
    "> 10 (%)" = `> 10 (percentage)`
  ) |> 
  select(
    name,
    `0 (n)`, `0 (%)`,
    `1-2 (n)`, `1-2 (%)`,
    `3-6 (n)`, `3-6 (%)`,
    `7-10 (n)`,  `7-10 (%)`,
    `> 10 (n)`, `> 10 (%)`
  )

municipal_access_15_table <- access_15_sector |> 
  gt() |> 
  cols_label(name = "") |> 
  fmt(columns = c(2,4,6,8,10), fns = convert_number_tens) |>  
  fmt(columns = c(3, 5, 7, 9, 11), fns = convert_pct) |> 
  data_color(
    columns = c(3, 5, 7, 9, 11),
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = NULL
    )
  ) |>
  tab_style(
    style = cell_text(
      font = "KMR Apparat"
    ),
    locations = cells_body()
  ) |> 
  tab_style(
    style = cell_text(
      font = "KMR Apparat"
    ),
    locations = cells_column_labels()
  ) |> 
  # General table settings
  tab_options(
    table.font.size = 12,
    row_group.font.size = 12
  ) |> 
  cols_width(1 ~ px(168),
             2:11 ~ px(80))

gtsave(municipal_access_15_table, "output/axe3/municipal_access_15_table.png", zoom = 3)
# R Markdown --------------------------------------------------------------


qs::qsavem(municipal_map, municipal_table, municipal_access, municipal_access_three,
           municipal_immigrant_ratio, municipal_population_ratio, municipal_lowincome_ratio,
           municipal_lowincome_lowaccess, municipal_immigrant_lowaccess, municipal_pop_lowaccess,
           file = "data/axe3/municipal.qsm")

