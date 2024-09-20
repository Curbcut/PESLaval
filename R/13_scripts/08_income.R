### INCOME #####################################################################

source("R/01_startup.R")

# CPI values --------------------------------------------------------------

# CPI for october of all the years
CPI <- tibble::as_tibble(read.csv("data/CPI.csv")) |> 
  select(REF_DATE, GEO, product = `Products.and.product.groups`, UOM, 
         value = VALUE) |>
  suppressWarnings()

CPI <- CPI[CPI$GEO == "Quebec" & CPI$product == "All-items", ] |> 
  mutate(month = lubridate::ym(REF_DATE)) |> 
  select(month, value) |> 
  filter(month(month) == 01) |> 
  mutate(year = year(month)) |> 
  select(year, value) |> 
  mutate(value = value / value[year == "2020"])


# Total Household Income --------------------------------------------------

#Census grabber for median household income for 2020 to start up the graph table
mhh_census_grabber_20 <- function(region, geolevel, geoname, filter_v){
  regions_list <- list()
  regions_list[[geolevel]] <- region
  
  get_census(dataset = "CA21",
             regions = regions_list,
             level = geolevel,
             vectors = "v_CA21_906"
  ) |> 
    select(-filter_v) |> 
    mutate(Geography = geoname, .before = 1)
}

#Census grabber for median household income for all other years
mhh_census_grabber <- function(cyear, region, geolevel, vectorid, filter_v, ryear){
  regions_list <- list()
  regions_list[[geolevel]] <- region
  
  get_census(dataset = cyear,
             regions = regions_list,
             level = geolevel,
             vectors = vectorid
  ) |> 
    select(-filter_v) |> 
    rename(!!ryear := last_col())
}

#Grabbing median individual income for Laval for years 2000-2020
mhh_inc20_lvl <- mhh_census_grabber_20("2465005", "CSD", "Laval", (1:10)) |> 
  rename("2020" := last_col())
mhh_inc15_lvl <- mhh_census_grabber("CA16","2465005", "CSD", "v_CA16_2397", (1:10), "2015")
mhh_inc10_lvl <- mhh_census_grabber("CA11", "2465005", "CSD", "v_CA11N_2562", (1:10), "2010") |> 
  select(-1)
mhh_inc05_lvl <- mhh_census_grabber("CA06", "2465005", "CSD", "v_CA06_2000", (1:10), "2005")
mhh_inc00_lvl <- mhh_census_grabber("CA01", "2465005", "CSD", "v_CA01_1634", (1:10), "2000")
#Binding the tables together
mhh_lvl_graph <- bind_cols(mhh_inc20_lvl, mhh_inc15_lvl, mhh_inc10_lvl, mhh_inc05_lvl, mhh_inc00_lvl)

#Grabbing median individual income for Quebec for years 2000-2020
mhh_inc20_qc <- mhh_census_grabber_20("24", "PR", "Quebec", (1:8)) |> 
  rename("2020" := last_col())
mhh_inc15_qc <- mhh_census_grabber("CA16", "24", "PR", "v_CA16_2397", (1:8), "2015")
mhh_inc10_qc <- mhh_census_grabber("CA11", "24", "PR", "v_CA11N_2562", (1:8), "2010") |> 
  select(-1)
mhh_inc05_qc <- mhh_census_grabber("CA06", "24", "PR", "v_CA06_2000", (1:8), "2005")
mhh_inc00_qc <- mhh_census_grabber("CA01", "24", "PR", "v_CA01_1634", (1:8), "2000")
#Bind the tables together
mhh_qc_graph <- bind_cols(mhh_inc20_qc, mhh_inc15_qc, mhh_inc10_qc, mhh_inc05_qc, mhh_inc00_qc)

#Bind all geography graphs together
mhh_graph <- bind_rows(mhh_lvl_graph, mhh_qc_graph) |> 
  pivot_longer(cols = -Geography, names_to = "year", values_to = "income")

# Adjust with inflation
mhh_graph <- mutate(mhh_graph, income = income / CPI$value[match(year, CPI$year)])


rev_med <- mhh_graph$income[mhh_graph$year == 2020 & mhh_graph$Geography == "Laval"]
rev_med_2015 <- mhh_graph$income[mhh_graph$year == 2015 & mhh_graph$Geography == "Laval"]
rev_med_aug <- convert_pct((rev_med - rev_med_2015) / rev_med_2015)

rev_med <- convert_number_tens(rev_med)
rev_med_2015 <- convert_number(rev_med_2015)

rev_med_QC <- mhh_graph$income[mhh_graph$year == 2020 & mhh_graph$Geography == "Quebec"]
rev_med_2015_QC <- mhh_graph$income[mhh_graph$year == 2015 & mhh_graph$Geography == "Quebec"]
rev_med_aug_QC <- convert_pct((rev_med_QC - rev_med_2015_QC) / rev_med_2015_QC)


names(mhh_graph) <- c("Région", "Année", "Revenu médian des ménages")
mhh_graph <- mhh_graph %>%
  pivot_wider(names_from = Région, 
              values_from = c(`Revenu médian des ménages`))

names(mhh_graph) <- c("Année", "Ville de Laval", "Ensemble du Québec")

mhh_plot <-
  gt(mhh_graph) |> 
  fmt(columns = 2:3, fns = \(x) paste(convert_number(x), "$")) |> 
  # Apply font style to the whole table
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
  tab_style(
    style = cell_text(
      font = "KMR Apparat Regular"
    ),
    locations = cells_row_groups()
  ) |>
  tab_style(
    style = cell_fill(color = "#F0F0F0"),
    locations = cells_row_groups()
  ) |> 
  tab_spanner(
    label = "Revenu médian des ménages",
    columns = c("Ville de Laval", "Ensemble du Québec")
  ) |> 
  tab_options(
    table.font.size = indesign_fontsize,
    row_group.font.size = indesign_title_fontsize
  )

gtsave(mhh_plot, "output/axe1/income/mhh_plot.png")


# Household median income DA / Sector -------------------------------------

median_income_sf <- get_census(dataset = "CA21",
                               regions = list(CSD = 2465005),
                               level = "DA",
                               vectors = c(median_income = "v_CA21_906",
                                           priv_hh = "v_CA21_905"),
                               geo_format = "sf")

# Map loyer médian par DA
labels <- c("< 60K $", "60K $ - 80K $", "80K $ - 100K $", "100K $ - 120K $", "> 120K $")
t <- add_bins(df = median_income_sf,
              variable = "median_income",
              breaks = c(-Inf, 60000, 80000, 100000, 120000, Inf),
              labels = labels
)
# t <- Reduce(rbind,
#             split(t, t$binned_variable, drop = FALSE) |>
#               lapply(\(x) {
#                 out <- tibble::tibble(x$binned_variable)
#                 out$geometry <- sf::st_union(x)
#                 sf::st_as_sf(out, crs = 4326)[1, ]
#               })
# ) |> sf::st_as_sf()
# names(t)[1] <- "binned_variable"

lvls <- levels(t$binned_variable)
lvls <- c(lvls[length(lvls)], lvls[-length(lvls)])

median_income_sf_plot <-
  ggplot(data = t) +
  gg_cc_tiles +
  geom_sf(aes(fill = binned_variable), color = "transparent", lwd = 0) +
  scale_fill_manual(
    values = curbcut_colors$left_5$fill[c(1:6)],  # Include NA color first
    na.value = curbcut_colors$left_5$fill[1],
    name = element_blank(),
    breaks = lvls,  # NA first
    labels = lvls,  # NA label first
    guide = guide_legend(
      title.position = "top",
      label.position = "bottom",
      keywidth = 4,
      keyheight = 0.5
    )
  ) +
  gg_cc_theme

ggplot2::ggsave(filename = here::here("output/axe1/income/median_income_sf_plot.pdf"),
                plot = median_income_sf_plot, width = 6.5, height = 4)


# Tableau des loyers médians par secteurs
median_income_sf_2021 <- median_income_sf
z <- sf::st_intersects(sf::st_centroid(median_income_sf_2021), laval_sectors)
median_income_sf_2021$secteur <- sapply(z, \(x) {
  if (length(x) == 0) return(NA)
  laval_sectors$name[x]
})
median_income_sf_2021 <-
  median_income_sf_2021 |>
  group_by(secteur) |>
  summarize(median_income = weighted_mean(median_income, priv_hh, na.rm = TRUE))
median_income_sf_2021 <- median_income_sf_2021[1:6, ]
median_income_sf_2021 <- sf::st_drop_geometry(median_income_sf_2021)
names(median_income_sf_2021) <- c("Secteur", "Revenu médian des ménages")

median_income_table <-
  gt(median_income_sf_2021) |> 
  # Appliquer une mise en couleur sur les colonnes médianes
  data_color(
    columns = 2,
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = NULL
    )
  ) |> 
  fmt(columns = 2, fns = \(x) paste(convert_number_tens(x), "$")) |> 
  # Ajouter des bordures en haut de chaque groupe de ligne
  tab_style(
    style = cell_borders(
      sides = c("top"),
      color = "white",
      weight = px(10)
    ),
    locations = cells_row_groups()
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

gtsave(median_income_table, "output/axe1/housing/median_income_table.png")


# # Répartition des revenus des ménages -------------------------------------
# 
# #Setting the names for the vectors
# tot_hh_names <- c("Total", "< 5.000 $", "5 à 9.999 $", "10 à 14.999 $", "15 à 19.999 $",
#                   "20 à 24.999 $", "25 à 29.999 $", "30 à 34.999 $", "35 à 39.999 $", "40 à 44.999 $",
#                   "45 à 49.999 $", "50 à 59.999 $", "60 à 69.999 $", "70 à 79.999 $", "80 à 89.999 $",
#                   "90 à 99.999 $", "=> 100.000 $", "100 à 124.999 $", "125 à 149.999 $",
#                   "150 à 199.999 $", "=> 200.000 $")
# 
# #Grabbing vectors for total household income and renaming them with tot_hh_names
# tot_hh_vectors <- can21$vector[which(can21$vector == "v_CA21_923"):which(can21$vector == "v_CA21_943")] |> 
#   set_names(tot_hh_names)
# 
# #Grabbing total household income
# tot_hh_lvl  <- get_census(dataset = "CA21", 
#                           regions = list(CSD = 2465005), 
#                           level = "CSD",
#                           vectors = tot_hh_vectors) |> 
#   select(all_of(tot_hh_names)) |> 
#   select(everything())
# 
# #Preparing the bar graph for "regular" intervals
# tot_hh_bar <- tot_hh_lvl |> 
#   mutate("< 10.000 $" = `< 5.000 $` + `5 à 9.999 $`,
#          "10 à 19.999 $" = `10 à 14.999 $` + `15 à 19.999 $`,
#          "20 à 29.999 $" = `20 à 24.999 $` + `25 à 29.999 $`,
#          "30 à 39.999 $" = `30 à 34.999 $` + `35 à 39.999 $`,
#          "40 à 49.999 $" = `40 à 44.999 $` + `45 à 49.999 $`) |> 
#   rename("> 200.000 $" = "=> 200.000 $") |> 
#   select(-"< 5.000 $", -"5 à 9.999 $", -"10 à 14.999 $", -"15 à 19.999 $", -"20 à 24.999 $",
#          -"25 à 29.999 $", -"30 à 34.999 $", -"35 à 39.999 $", -"40 à 44.999 $", -"45 à 49.999 $",
#          -Total, -"=> 100.000 $") |> 
#   pivot_longer(cols = everything(), names_to = "Bracket", values_to = "Counts") |>
#   mutate(Bracket = factor(Bracket, levels = c("> 200.000 $", "150 à 199.999 $", "125 à 149.999 $",
#                                               "100 à 124.999 $", "90 à 99.999 $", "80 à 89.999 $",
#                                               "70 à 79.999 $", "60 à 69.999 $", "50 à 59.999 $",
#                                               "40 à 49.999 $", "30 à 39.999 $", "20 à 29.999 $",
#                                               "10 à 19.999 $", "< 10.000 $")))
# 
# #Preparing the bar graph table for $50k intervals
# tot_hh_bar_50 <- tot_hh_lvl |> 
#   mutate("< 50.000 $"= `< 5.000 $` + `5 à 9.999 $` + `10 à 14.999 $` + `15 à 19.999 $` + `20 à 24.999 $` +
#            `25 à 29.999 $` + `30 à 34.999 $` + `35 à 39.999 $` +`40 à 44.999 $` + `45 à 49.999 $`,
#          "50 à 99.999 $" = `50 à 59.999 $` + `60 à 69.999 $` + `70 à 79.999 $` + `80 à 89.999 $` + `90 à 99.999 $`,
#          "100 à 149.999 $" = `100 à 124.999 $` + `125 à 149.999 $`) |> 
#   rename("> 200.000 $" = "=> 200.000 $") |> 
#   select("< 50.000 $", "50 à 99.999 $", "100 à 149.999 $", "150 à 199.999 $", "> 200.000 $") |> 
#   pivot_longer(cols = everything(), names_to = "Bracket", values_to = "Counts") |> 
#   mutate(Bracket = factor(Bracket, levels = c("> 200.000 $", "150 à 199.999 $", "100 à 149.999 $",
#                                               "50 à 99.999 $", "< 50.000 $")))
# 
# #Bar graph for tot_hh_bar_50
# household_rev_plot <-
#   ggplot(tot_hh_bar_50, aes(x = Bracket, y = Counts, fill = "")) +
#   geom_bar(stat = "identity", position = "dodge", width = 0.7, fill = color_theme("greenecology")) +
#   labs(title = element_blank(), 
#        x = "Revenu total du ménage", 
#        y = "Nombre de ménages") +
#   scale_y_continuous(label = convert_number) +
#   coord_flip() +
#   gg_cc_theme_no_sf +
#   theme(legend.position = "none",
#         plot.margin = margin(5, 20, 5, 5))
#   theme(legend.position = "none",
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.title = element_text(hjust = 0.5),
#         plot.margin = margin(5, 15, 5, 5, "pt"))
# 
# 
# ggplot2::ggsave(filename = here::here("output/axe1/income/household_rev_plot.pdf"),
#                 plot = household_rev_plot, width = 5, height = 2)
# 
# # After-Tax Household Income ----------------------------------------------
# #Setting the names for the vectors
# at_hh_names <- c("Total", "< 5.000 $", "5 à 9.999 $", "10 à 14.999 $", "15 à 19.999 $",
#                  "20 à 24.999 $", "25 à 29.999 $", "30 à 34.999 $", "35 à 39.999 $", "40 à 44.999 $",
#                  "45 à 49.999 $", "50 à 59.999 $", "60 à 69.999 $", "70 à 79.999 $", "80 à 89.999 $",
#                  "90 à 99.999 $", "=> 100.000 $", "100 à 124.999 $", "125 à 149.999 $",
#                  "> 150.000 $")
# 
# #Grabbing vectors for AT household income and renaming them with at_hh_names
# at_hh_vectors <- can21$vector[which(can21$vector == "v_CA21_944"):which(can21$vector == "v_CA21_963")] |> 
#   set_names(at_hh_names)
# 
# #Grabbing AT household income
# at_hh_lvl  <- get_census(dataset = "CA21", 
#                          regions = list(CSD = 2465005), 
#                          level = "CSD",
#                          vectors = at_hh_vectors) |> 
#   select(all_of(at_hh_names)) |> 
#   select(everything())
# 
# #Preparing the bar graph for "regular" intervals
# at_hh_bar <- at_hh_lvl |> 
#   mutate("< 10.000 $" = `< 5.000 $` + `5 à 9.999 $`,
#          "10 à 19.999 $" = `10 à 14.999 $` + `15 à 19.999 $`,
#          "20 à 29.999 $" = `20 à 24.999 $` + `25 à 29.999 $`,
#          "30 à 39.999 $" = `30 à 34.999 $` + `35 à 39.999 $`,
#          "40 à 49.999 $" = `40 à 44.999 $` + `45 à 49.999 $`) |> 
#   select(-"< 5.000 $", -"5 à 9.999 $", -"10 à 14.999 $", -"15 à 19.999 $", -"20 à 24.999 $",
#          -"25 à 29.999 $", -"30 à 34.999 $", -"35 à 39.999 $", -"40 à 44.999 $", -"45 à 49.999 $",
#          -Total, -"=> 100.000 $") |> 
#   pivot_longer(cols = everything(), names_to = "Bracket", values_to = "Counts") |>
#   mutate(Bracket = factor(Bracket, levels = c("> 150.000 $", "125 à 149.999 $",
#                                               "100 à 124.999 $", "90 à 99.999 $", "80 à 89.999 $",
#                                               "70 à 79.999 $", "60 à 69.999 $", "50 à 59.999 $",
#                                               "40 à 49.999 $", "30 à 39.999 $", "20 à 29.999 $",
#                                               "10 à 19.999 $", "< 10.000 $")))
# 
# #Creating the sideways bar graph for tot_hh_bar
# ggplot(at_hh_bar, aes(x = Bracket, y = Counts, fill = "")) +
#   geom_bar(stat = "identity", position = "dodge", width = 0.7, fill = "royalblue2") +
#   labs(title = "Revenu des ménages après impôt 2020 à Laval", x = "Revenu des ménages après impôt",
#        y = "Nombre de ménages") +
#   theme_minimal() +
#   theme(legend.position = "none",
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.title = element_text(hjust = 0.5))+
#   coord_flip()
# 
# #Preparing the bar graph table for $50k intervals
# at_hh_bar_50 <- at_hh_lvl |> 
#   mutate("< 50.000 $"= `< 5.000 $` + `5 à 9.999 $` + `10 à 14.999 $` + `15 à 19.999 $` + `20 à 24.999 $` +
#            `25 à 29.999 $` + `30 à 34.999 $` + `35 à 39.999 $` +`40 à 44.999 $` + `45 à 49.999 $`,
#          "50 à 99.999 $" = `50 à 59.999 $` + `60 à 69.999 $` + `70 à 79.999 $` + `80 à 89.999 $` + `90 à 99.999 $`,
#          "100 à 149.999 $" = `100 à 124.999 $` + `125 à 149.999 $`) |> 
#   select("< 50.000 $", "50 à 99.999 $", "100 à 149.999 $", "> 150.000 $") |> 
#   pivot_longer(cols = everything(), names_to = "Bracket", values_to = "Counts") |> 
#   mutate(Bracket = factor(Bracket, levels = c("> 150.000 $", "100 à 149.999 $",
#                                               "50 à 99.999 $", "< 50.000 $")))
# 
# #Bar graph for at_hh_bar_50
# ggplot(at_hh_bar_50, aes(x = Bracket, y = Counts, fill = "")) +
#   geom_bar(stat = "identity", position = "dodge", width = 0.7, fill = "royalblue2") +
#   labs(title = "Revenu des ménages après impôt 2020 à Laval", x = "Revenu des ménages après impôt",
#        y = "Nombre de ménages") +
#   theme_minimal() +
#   theme(legend.position = "none",
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.title = element_text(hjust = 0.5))+
#   coord_flip()
# 
# # Household Income in 2020 ------------------------------------------------
# #Pulling vectors for household income in 2020
# hh_inc20_vectors <- c("v_CA21_906","v_CA21_907", "v_CA21_909", "v_CA21_910",
#                       "v_CA21_912", "v_CA21_913") |> 
#   set_names(c("total", "at_total", "one_p", "at_one_p", "two_p", "at_two_p"))
# 
# #Creating a function for cancensus so multiple uses aren't needed
# census_grabber <- function(region, geolevel, geoname){
#   regions_list <- list()
#   regions_list[[geolevel]] <- region
#   
#   get_census(dataset = "CA21",
#              regions = regions_list,
#              level = geolevel,
#              vectors = hh_inc20_vectors
#   ) |> 
#     mutate(Geography = geoname) |> 
#     select(Geography, total, at_total, one_p, at_one_p, two_p, at_two_p)
# }
# 
# #Fetching census data for each of the geographies for hh_inc20_vectors
# hh_inc20_lvl <- census_grabber("2465005", "CSD", "Laval")
# hh_inc20_mtl <- census_grabber("2466023", "CSD", "Montreal")
# hh_inc20_qc <- census_grabber("24", "PR", "Quebec")
# 
# #Preparing the table for the group stacked bar chart
# hh_inc20_graph <- bind_rows(hh_inc20_lvl, hh_inc20_mtl, hh_inc20_qc) |> 
#   mutate(total_diff = total - at_total, one_p_diff = one_p - at_one_p,
#          two_p_diff = two_p - at_two_p) |> 
#   select(Geography, at_total, total_diff, at_one_p, one_p_diff, at_two_p, two_p_diff) |> 
#   pivot_longer(cols = -Geography, names_to = "income_type", values_to = "income") |> 
#   mutate("Household Type" = case_when(
#     income_type %in% c("at_total", "total_diff") ~ "Private",
#     income_type %in% c("at_one_p", "one_p_diff") ~ "One Person",
#     income_type %in% c("at_two_p", "two_p_diff") ~ "Two+ Persons")) |>
#   group_by(`Household Type`) |>
#   mutate(income_type = factor(income_type, levels = c("one_p_diff", "at_one_p",
#                                                       "two_p_diff", "at_two_p",
#                                                       "total_diff", "at_total"))) |>
#   ungroup() |> 
#   mutate(`Household Type` = factor(`Household Type`, levels = c("Private", "One Person", "Two+ Persons")))
# 
# #Create a grouped stacked bar graph
# ggplot(hh_inc20_graph, aes(x = Geography, y = income, fill = income_type)) +
#   geom_bar(stat = "identity", position = "stack") +
#   facet_wrap(~`Household Type`, scales = "fixed", nrow = 1) +
#   labs(title = "Revenu des ménages privés 2020", x = "", y = "Revenu des ménages privés") +
#   theme_minimal() +
#   scale_fill_manual(values = c("total_diff" = "#aec7e8", "at_total" = "#1f77b4",
#                                "one_p_diff" = "#ffbb78", "at_one_p" = "#ff7f0e",
#                                "two_p_diff" = "#98df8a", "at_two_p" = "#2ca02c"),
#                     labels = c("total_diff" = "Revenu total du ménage privé",
#                                "at_total" = "Revenu après impôt des ménages privés",
#                                "one_p_diff" = "Revenu total d'une personne",
#                                "at_one_p" = "Revenu après impôt d'une personne",
#                                "two_p_diff" = "Revenu total pour deux personnes et plus",
#                                "at_two_p" = "Revenu après impôt pour deux personnes et plus")) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
#         legend.position = "bottom")
# 
# 
# # Economic Household Income -----------------------------------------------
# 
# ehh_inc20_vectors <- c("v_CA21_965","v_CA21_966", "v_CA21_969", "v_CA21_970",
#                        "v_CA21_973", "v_CA21_974", "v_CA21_977", "v_CA21_978") |> 
#   set_names(c("total", "at_total", "couple", "at_couple", "couple_c",
#               "at_couple_c", "one_p", "at_one_p"))
# 
# #Census grabber for economic household income
# ehh_census_grabber <- function(region, geolevel, geoname){
#   regions_list <- list()
#   regions_list[[geolevel]] <- region
#   
#   get_census(dataset = "CA21",
#              regions = regions_list,
#              level = geolevel,
#              vectors = ehh_inc20_vectors
#   ) |> 
#     mutate(Geography = geoname) |> 
#     select(Geography, total, at_total, couple, at_couple,
#            couple_c, at_couple_c, one_p, at_one_p)
# }
# 
# #Grabbing census data for economic household income
# ehh_inc20_lvl <- ehh_census_grabber("2465005", "CSD", "Laval")
# ehh_inc20_mtl <- ehh_census_grabber("2466023", "CSD", "Montreal")
# ehh_inc20_qc <- ehh_census_grabber("24", "PR", "Quebec")
# 
# #Preparing the table for the group stacked bar chart
# ehh_inc20_graph <- bind_rows(ehh_inc20_lvl, ehh_inc20_mtl, ehh_inc20_qc) |> 
#   mutate(total_diff = total - at_total, couple_diff = couple - at_couple,
#          couple_c_diff = couple_c - at_couple_c, one_p_diff = one_p - at_one_p) |> 
#   select(Geography, at_total, total_diff, at_one_p, one_p_diff,
#          at_couple, couple_diff, at_couple_c, couple_c_diff) |> 
#   pivot_longer(cols = -Geography, names_to = "income_type", values_to = "income") |> 
#   mutate("Household Type" = case_when(
#     income_type %in% c("at_total", "total_diff") ~ "Economic",
#     income_type %in% c("at_one_p", "one_p_diff") ~ "One Parent",
#     income_type %in% c("at_couple", "couple_diff") ~ "Couple",
#     income_type %in% c("at_couple_c", "couple_c_diff") ~ "Couple with Children")) |>
#   group_by(`Household Type`) |>
#   mutate(income_type = factor(income_type, levels = c("total_diff", "at_total",
#                                                       "one_p_diff", "at_one_p",
#                                                       "couple_diff", "at_couple",
#                                                       "couple_c_diff", "at_couple_c"))) |>
#   ungroup() |> 
#   mutate(`Household Type` = factor(`Household Type`, levels = c("Economic", "One Parent",
#                                                                 "Couple", "Couple with Children")))
# 
# #Graphing out the data
# ggplot(ehh_inc20_graph, aes(x = Geography, y = income, fill = income_type)) +
#   geom_bar(stat = "identity", position = "stack") +
#   facet_wrap(~`Household Type`, scales = "fixed", nrow = 1) +
#   labs(title = "Revenu économique des ménages 2020 à Laval", x = "", y = "Revenu économique des ménages") +
#   theme_minimal() +
#   scale_fill_manual(values = c("total_diff" = "#aec7e8", "at_total" = "#1f77b4",
#                                "one_p_diff" = "#ffbb78", "at_one_p" = "#ff7f0e",
#                                "couple_diff" = "#98df8a", "at_couple" = "#2ca02c",
#                                "couple_c_diff" = "#ff9896", "at_couple_c" = "#d62728"),
#                     labels = c("total_diff" = "Revenu total du ménage économique",
#                                "at_total" = "Revenu économique des ménages après impôt",
#                                "one_p_diff" = "Revenu total d'un seul parent",
#                                "at_one_p" = "Revenu après impôt d'un seul parent",
#                                "couple_diff" = "Revenu total du couple",
#                                "at_couple" = "Revenu du couple après impôt",
#                                "couple_c_diff" = "Revenu total d'un couple avec enfants",
#                                "at_couple_c" = "Couple avec enfants Revenus après impôts")) +
#   scale_y_continuous(labels = scales::comma) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
#         legend.position = "bottom", strip.text.x = element_blank()) +
#   guides(fill = guide_legend(nrow = 4))
#
#
# Personal Income Brackets ---------------------------------------------------------

# Evolution of Individual Income ------------------------------------------
# Census grabber for median individual income for 2020 to start up the graph table
mii_census_grabber_20 <- function(region, geolevel, geoname, filter_v){
  regions_list <- list()
  regions_list[[geolevel]] <- region
  
  get_census(dataset = "CA21",
             regions = regions_list,
             level = geolevel,
             vectors = "v_CA21_560"
  ) |> 
    select(-filter_v) |> 
    mutate(Geography = geoname, .before = 1)
}

# Census grabber for years other than 2020
mii_census_grabber <- function(cyear, region, geolevel, vectorid, filter_v, ryear){
  regions_list <- list()
  regions_list[[geolevel]] <- region
  
  get_census(dataset = cyear,
             regions = regions_list,
             level = geolevel,
             vectors = vectorid
  ) |> 
    select(-filter_v) |> 
    rename(!!ryear := last_col())
}


#Grabbing median individual income for Laval for years 2000-2020
mii_inc20_lvl <- mii_census_grabber_20("2465005", "CSD", "Laval", (1:10)) |> 
  rename("2020" := last_col())
mii_inc15_lvl <- mii_census_grabber("CA16","2465005", "CSD", "v_CA16_2207", (1:10), "2015")
mii_inc10_lvl <- mii_census_grabber("CA11", "2465005", "CSD", "v_CA11N_2341", (1:10), "2010") |> 
  select(-1)
mii_inc05_lvl <- mii_census_grabber("CA06", "2465005", "CSD", "v_CA06_1583", (1:10), "2005")
mii_inc00_lvl <- mii_census_grabber("CA01", "2465005", "CSD", "v_CA01_1449", (1:10), "2000")
#Binding the tables together
mii_lvl_graph <- bind_cols(mii_inc20_lvl, mii_inc15_lvl, mii_inc10_lvl, mii_inc05_lvl, mii_inc00_lvl)

#Grabbing median individual income for Quebec for years 2000-2020
mii_inc20_qc <- mii_census_grabber_20("24", "PR", "Quebec", (1:8)) |> 
  rename("2020" := last_col())
mii_inc15_qc <- mii_census_grabber("CA16", "24", "PR", "v_CA16_2207", (1:8), "2015")
mii_inc10_qc <- mii_census_grabber("CA11", "24", "PR", "v_CA11N_2341", (1:8), "2010") |> 
  select(-1)
mii_inc05_qc <- mii_census_grabber("CA06", "24", "PR", "v_CA06_1583", (1:8), "2005")
mii_inc00_qc <- mii_census_grabber("CA01", "24", "PR", "v_CA01_1449", (1:8), "2000")

#Bind the tables together
mii_qc_graph <- bind_cols(mii_inc20_qc, mii_inc15_qc, mii_inc10_qc, mii_inc05_qc, mii_inc00_qc)

#Bind all geography tables together to make the line graph
mii_graph <- bind_rows(mii_lvl_graph, mii_qc_graph) |> 
  pivot_longer(cols = -Geography, names_to = "year", values_to = "income")

# Adjust with inflation
mii_graph <- mutate(mii_graph, income = income / CPI$value[match(year, CPI$year)])

rev_mid_ind <- mii_graph$income[mii_graph$year == 2020 & mii_graph$Geography == "Laval"]
rev_mid_ind_2015 <- mii_graph$income[mii_graph$year == 2015 & mii_graph$Geography == "Laval"]
rev_mid_ind_aug <- convert_pct((rev_mid_ind - rev_mid_ind_2015) / rev_mid_ind_2015)

rev_mid_ind <- convert_number_tens(rev_mid_ind)
rev_mid_ind_2015 <- convert_number(rev_mid_ind_2015)

rev_mid_ind_QC <- mii_graph$income[mii_graph$year == 2020 & mii_graph$Geography == "Quebec"]
rev_mid_ind_2015_QC <- mii_graph$income[mii_graph$year == 2015 & mii_graph$Geography == "Quebec"]
rev_mid_ind_aug_QC <- convert_pct((rev_mid_ind_QC - rev_mid_ind_2015_QC) / rev_mid_ind_2015_QC)


names(mii_graph) <- c("Région", "Année", "Revenu médian des ménages")
mii_graph <- mii_graph %>%
  pivot_wider(names_from = Région, 
              values_from = c(`Revenu médian des ménages`))

names(mii_graph) <- c("Année", "Ville de Laval", "Ensemble du Québec")

mii_plot <-
  gt(mii_graph) |> 
  fmt(columns = 2:3, fns = \(x) paste(convert_number(x), "$")) |> 
  # Apply font style to the whole table
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
  tab_style(
    style = cell_text(
      font = "KMR Apparat Regular"
    ),
    locations = cells_row_groups()
  ) |>
  tab_style(
    style = cell_fill(color = "#F0F0F0"),
    locations = cells_row_groups()
  ) |> 
  tab_spanner(
    label = "Revenu médian des ménages",
    columns = c("Ville de Laval", "Ensemble du Québec")
  ) |> 
  tab_options(
    table.font.size = indesign_fontsize,
    row_group.font.size = indesign_title_fontsize
  )

gtsave(mii_plot, "output/axe1/income/mii_plot.png")







#Pulling all vectors for total individual income
can21 <- cancensus::list_census_vectors("CA21")
indtotinc_vectors <- can21$vector[which(can21$vector == "v_CA21_665"):which(can21$vector == "v_CA21_712")]

#Naming the rows for the vectors
indtotinc_names <- c("total_count", "wo_total_income", "w_total_income", "under_10",
                     "10_20", "20_30", "30_40", "40_50", "50_60", "60_70", "70_80",
                     "80_90", "90_100", "over_100", "100_150", "over_150")

#Filter for vectors for Total
inc21_vectors_total <- can21 |> 
  filter(vector %in% indtotinc_vectors & type == "Total") |> 
  pull(vector) |> 
  set_names(indtotinc_names)

#Filter for vectors for men
inc21_vectors_men <- can21 |> 
  filter(vector %in% indtotinc_vectors & type == "Male") |> 
  pull(vector) |> 
  set_names(indtotinc_names)

#Filter for vectors for women
inc21_vectors_women <- can21 |> 
  filter(vector %in% indtotinc_vectors & type == "Female") |> 
  pull(vector) |> 
  set_names(indtotinc_names)

#Pull individual total income brackets for Laval
indtotinc_lvl_total <- get_census(dataset = "CA21", 
                                  regions = list(CSD = 2465005), 
                                  level = "CSD",
                                  vectors = inc21_vectors_total) |> 
  select(all_of(indtotinc_names)) |> 
  mutate(Geography = c("Laval")) |> 
  select(Geography, everything())

#Pull individual total income brackets for men
indtotinc_lvl_men  <- get_census(dataset = "CA21", 
                                 regions = list(CSD = 2465005), 
                                 level = "CSD",
                                 vectors = inc21_vectors_men) |> 
  select(all_of(indtotinc_names)) |> 
  mutate(Geography = c("Men")) |> 
  select(Geography, everything())

#Pull individual total income brackets for women
indtotinc_lvl_women  <- get_census(dataset = "CA21", 
                                   regions = list(CSD = 2465005), 
                                   level = "CSD",
                                   vectors = inc21_vectors_women) |> 
  select(all_of(indtotinc_names)) |> 
  mutate(Geography = c("Women")) |> 
  select(Geography, everything())

#Bind Laval, Men, and Women together
ind_total_income <- bind_rows(indtotinc_lvl_total, indtotinc_lvl_men, indtotinc_lvl_women)

#Prepare table for individual income brackets grouped bar graph
ind_total_bar <- ind_total_income |> 
  select(-total_count, -wo_total_income, -w_total_income, -over_100) |> 
  rename("< 10.000 $" = "under_10", "10 à 19.999 $" = "10_20", "20 à 29.999 $" = "20_30",
         "30 à 39.999 $" = "30_40", "40 à 49.999 $" = "40_50", "50 à 59.999 $" = "50_60",
         "60 à 69.999 $" = "60_70", "70 à 79.999 $" = "70_80", "80 à 89.999 $" = "80_90",
         "90 à 99.999 $" = "90_100", "100 à 150.000 $" = "100_150", "> 150.000 $" = "over_150") |> 
  pivot_longer(cols = -Geography, names_to = "Bracket", values_to = "Value") |> 
  mutate(Bracket = factor(Bracket, levels = c("< 10.000 $", "10 à 19.999 $", "20 à 29.999 $",
                                              "30 à 39.999 $", "40 à 49.999 $", "50 à 59.999 $",
                                              "60 à 69.999 $", "70 à 79.999 $", "80 à 89.999 $",
                                              "90 à 99.999 $", "100 à 150.000 $", "> 150.000 $")))

#Grouped bar graph for individual brackets
revenue_annuel_plot <- 
ggplot(ind_total_bar[ind_total_bar$Geography != "Laval", ], 
       aes(x = Bracket, y = Value, fill = Geography)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = element_blank(),
       x = "", y = "Individus") +
  scale_fill_manual(values = c(#"Laval" = "royalblue2", 
    "Men" = "#A3B0D1", 
    "Women" = "#CD718C"),
    name = "Status",
    labels = c(#"Laval" = "Laval", 
      "Men" = "Hommes", "Women" = "Femmes")) +
  scale_x_discrete(labels = function(x) gsub(",", " ", x)) +
  scale_y_continuous(labels = convert_number) +
  theme_minimal() +
  gg_cc_theme_no_sf +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.box.margin = margin(t = -20))

ggplot2::ggsave(filename = here::here("output/axe1/income/revenue_annuel_plot.pdf"),
                plot = revenue_annuel_plot, width = 6.5, height = 3.5)



ind_total_laval <- ind_total_bar[ind_total_bar$Geography == "Laval", ]

inc_ind_less50 <- sum(ind_total_laval$Value[1:5])
inc_ind_all <- sum(ind_total_laval$Value)
inc_ind_less50_pct <- convert_pct(inc_ind_less50 / inc_ind_all)
inc_ind_less50 <- convert_number(inc_ind_less50)

inc_ind_between_20k30k <- sum(ind_total_laval$Value[3])
inc_ind_less50_pct <- convert_pct(inc_ind_between_20k30k / inc_ind_all)
inc_ind_between_20k30k <- convert_number(inc_ind_between_20k30k)


# Household median income DA / Sector -------------------------------------

median_income_ind_sf <- get_census(dataset = "CA21",
                                   regions = list(CSD = 2465005),
                                   level = "DA",
                                   vectors = c(median_income = "v_CA21_560",
                                               tot_rec = "v_CA21_557"),
                                   geo_format = "sf")

# Map loyer médian par DA
labels <- c("< 25K $", "25K $ - 35K $", "35K $ - 45K $", "45K $ - 55K $", "> 55K $")
t <- add_bins(df = median_income_ind_sf,
              variable = "median_income",
              breaks = c(-Inf, 25000, 35000, 45000, 55000, Inf),
              labels = labels
)
# t <- Reduce(rbind,
#             split(t, t$binned_variable, drop = FALSE) |>
#               lapply(\(x) {
#                 out <- tibble::tibble(x$binned_variable)
#                 out$geometry <- sf::st_union(x)
#                 sf::st_as_sf(out, crs = 4326)[1, ]
#               })
# ) |> sf::st_as_sf()
# names(t)[1] <- "binned_variable"

lvls <- levels(t$binned_variable)
lvls <- c(lvls[length(lvls)], lvls[-length(lvls)])

median_income_ind_plot <-
  ggplot(data = t) +
  gg_cc_tiles +
  geom_sf(aes(fill = binned_variable), color = "transparent", lwd = 0) +
  scale_fill_manual(
    values = curbcut_colors$left_5$fill[c(1:6)],  # Include NA color first
    na.value = curbcut_colors$left_5$fill[1],
    name = element_blank(),
    breaks = lvls,  # NA first
    labels = lvls,  # NA label first
    guide = guide_legend(
      title.position = "top",
      label.position = "bottom",
      keywidth = 4,
      keyheight = 0.5
    )
  ) +
  gg_cc_theme

ggplot2::ggsave(filename = here::here("output/axe1/income/median_income_ind_plot.pdf"),
                plot = median_income_ind_plot, width = 6.5, height = 4)


# Tableau des loyers médians par secteurs
median_income_sf_2021 <- median_income_ind_sf
z <- sf::st_intersects(sf::st_centroid(median_income_sf_2021), laval_sectors)
median_income_sf_2021$secteur <- sapply(z, \(x) {
  if (length(x) == 0) return(NA)
  laval_sectors$name[x]
})
median_income_sf_2021 <-
  median_income_sf_2021 |>
  group_by(secteur) |>
  summarize(median_income = weighted_mean(median_income, tot_rec, na.rm = TRUE))
median_income_sf_2021 <- median_income_sf_2021[1:6, ]
median_income_sf_2021 <- sf::st_drop_geometry(median_income_sf_2021)
names(median_income_sf_2021) <- c("Secteur", "Revenu médian des individus")

median_income_ind_table <-
  gt(median_income_sf_2021) |> 
  # Appliquer une mise en couleur sur les colonnes médianes
  data_color(
    columns = 2,
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = NULL
    )
  ) |> 
  fmt(columns = 2, fns = \(x) paste(convert_number_tens(x), "$")) |> 
  # Ajouter des bordures en haut de chaque groupe de ligne
  tab_style(
    style = cell_borders(
      sides = c("top"),
      color = "white",
      weight = px(10)
    ),
    locations = cells_row_groups()
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

gtsave(median_income_table, "output/axe1/housing/median_income_table.png")




median_income_gender <- get_census(dataset = "CA21",
                                   regions = list(CSD = 2465005),
                                   level = "CSD",
                                   vectors = c(median_income_homme = "v_CA21_561",
                                               median_income_femme = "v_CA21_562"))
median_income_hommes <- median_income_gender$median_income_homme
median_income_femmes <- median_income_gender$median_income_femme
inc_f_x_h <- round(median_income_femmes / median_income_hommes * 100) / 100

median_income_gender_2016 <- get_census(dataset = "CA16",
                                   regions = list(CSD = 2465005),
                                   level = "CSD",
                                   vectors = c(median_income_homme = "v_CA16_2208",
                                               median_income_femme = "v_CA16_2209"))
median_income_hommes_2016 <- median_income_gender_2016$median_income_homme
median_income_femmes_2016 <- median_income_gender_2016$median_income_femme
inc_f_x_h_2016 <- round(median_income_femmes_2016 / median_income_hommes_2016 * 100) / 100

med_inc_femmes_aug <- (median_income_femmes - median_income_femmes_2016) / median_income_femmes_2016
med_inc_femmes_aug <- convert_pct(med_inc_femmes_aug)
med_inc_hommes_aug <- (median_income_hommes - median_income_hommes_2016) / median_income_hommes_2016
med_inc_hommes_aug <- convert_pct(med_inc_hommes_aug)

median_income_hommes <- convert_number(median_income_hommes)
median_income_femmes <- convert_number(median_income_femmes)
median_income_hommes_2016 <- convert_number(median_income_hommes_2016)
median_income_femmes_2016 <- convert_number(median_income_femmes_2016)


# #Year over year growth table
# mii_yoygraph <- bind_rows(mii_lvl_graph, mii_mtl_graph, mii_qc_graph) |> 
#   mutate("2020" = round(100 * `2020` / `2015` - 100, 2),
#          "2015" = round(100 * `2015` / `2010` - 100, 2),
#          "2010" = round(100 * `2010` / `2005` - 100, 2),
#          "2005" = round(100 * `2005` / `2000` - 100, 2)) |> 
#   select(Geography, "2005", "2010", "2015", "2020") |> 
#   pivot_longer(cols = -Geography, names_to = "Year", values_to = "Growth")
# 
# #Creating a table with CPI index values from 2000 to 2020
# cpi <- data.frame(
#   Geography = "CPI",
#   Year = c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008",
#            "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017",
#            "2018", "2019", "2020", "2021", "2022", "2023", "2024"),
#   Index = c(100, 103.8, 104.3, 107.4, 109.9, 111.7, 114.6, 116.4, 119.1, 119.2, 120.4,
#             124.6, 127, 127.8, 129.9, 131.5, 132.4, 133.3, 135.6, 138.8, 138.3, 144,
#             154.8, 160.9, 165.9)
# )
# 
# #Creating and binding yoy growth with CPI src:https://www150.statcan.gc.ca/n1/pub/71-607-x/2018016/cpilg-ipcgl-eng.htm
# mii_yoygraph_cpi <- bind_rows(mii_lvl_graph, mii_mtl_graph, mii_qc_graph) |> 
#   mutate("2020y" = `2020` / `2015`,
#          "2015y" = `2015` / `2010`,
#          "2010y" = `2010` / `2005`,
#          "2005y" = `2005` / `2000`) |> 
#   mutate("2000yoy" = 100,
#          "2005yoy" = 100 * `2005y`,
#          "2010yoy" = 100 * `2005y` * `2010y`,
#          "2015yoy" = 100 * `2005y` * `2010y` * `2015y`,
#          "2020yoy" = 100 * `2005y` * `2010y` * `2015y` * `2020y`) |> 
#   select(Geography, "2000yoy", "2005yoy", "2010yoy", "2015yoy", "2020yoy") |> 
#   rename("2000" = "2000yoy", "2005" = "2005yoy", "2010" = "2010yoy",
#          "2015" = "2015yoy", "2020" = "2020yoy") |> 
#   pivot_longer(cols = -Geography, names_to = "Year", values_to = "Index") |> 
#   bind_rows(cpi)
# 
# #Creating the line graph for median individual income
# ggplot(mii_graph, aes(x = Year, y = Income, color = Geography, group = Geography)) +
#   geom_line(linewidth = 1.5) +
#   labs(title = "Revenu médian individuel 2000-2020",
#        x = "Année",
#        y = "Revenu médian individuel ($)") +
#   theme_minimal() +
#   theme(legend.position = "bottom", legend.box = "horizontal",
#         legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
# 
# #Line graph for mii yoy growth
# ggplot(mii_yoygraph, aes(x = Year, y = Growth, color = Geography, group = Geography)) +
#   geom_line(linewidth = 1.5) +
#   scale_color_manual(values = c("CPI" = "grey", "Laval" = "royalblue2",
#                                 "Montreal" = "indianred3", "Quebec" = "gold2")) +
#   labs(title = "Individual Median Income Growth 2005-2020",
#        x = "Année",
#        y = "Income Growth (%)") +
#   theme_minimal() +
#   theme(legend.position = "bottom", legend.title = element_blank(),
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.title = element_text(hjust = 0.5))
# 
# #Plot for year over year growth
# ggplot(mii_yoygraph, aes(x = Year, y = Growth, fill = Geography)) +
#   geom_bar(stat = "identity", position = "dodge", width = 0.7) +
#   labs(title = "Croissance du revenu médian individuel 2005-2020",
#        x = "Année", y = "Croissance des revenus (%)") +
#   theme_minimal() +
#   theme(legend.position = "bottom",
#         legend.title = element_blank(),
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.title = element_text(hjust = 0.5))
# 
# #Line graph for CPI and mii index
# ggplot(mii_yoygraph_cpi, aes(x = Year, y = Index, color = Geography, group = Geography)) +
#   geom_line(linewidth = 1.5) +
#   scale_color_manual(values = c("CPI" = "grey", "Laval" = "royalblue2",
#                                 "Montreal" = "indianred3", "Quebec" = "gold2"),
#                      labels = c("IPC", "Laval", "Montreal", "Quebec")) +
#   labs(title = "Revenu médian individuel par rapport à l'IPC 2000-2020",
#        x = "Année",
#        y = "Revenu médian individuel normalisé") +
#   theme_minimal() +
#   theme(legend.position = "bottom", legend.title = element_blank(),
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.title = element_text(hjust = 0.5))
























































# #Preparing table for bar graph for total income brackets by every $50,000 
# ind_total_bar_50 <- ind_total_income |> 
#   select(-total_count, -wo_total_income, -w_total_income, -over_100) |> 
#   mutate("< 50.000 $" = under_10 + `10_20` + `20_30` + `30_40` + `40_50`,
#          "50 à 99.999 $" = `50_60` + `60_70` + `70_80` + `80_90` + `90_100`,
#          "100 à 150.000 $" = `100_150`, "> 150.000$" = over_150) |> 
#   select(-under_10, -`10_20`, -`20_30`, -`30_40`, -`40_50`, -`50_60`,
#          -`60_70`, -`70_80`, -`80_90`, -`90_100`, -`100_150`, -over_150) |> 
#   pivot_longer(cols = -Geography, names_to = "Bracket", values_to = "Value") |>
#   mutate(Bracket = factor(Bracket, levels = c("< 50.000 $", "50 à 99.999 $",
#                                               "100 à 150.000 $", "> 150.000$")))
# 
# #Bar graph for total individual income by $50,000 intervals
# ggplot(ind_total_bar_50[ind_total_bar_50$Geography != "Laval",], aes(x = Bracket, y = Value, fill = Geography)) +
#   geom_bar(stat = "identity", position = "dodge", width = 0.7) +
#   labs(title = element_blank(),
#        x = "", y = "Individus") +
#   scale_fill_manual(values = c(#"Laval" = "royalblue2", 
#     "Men" = "#A3B0D1", 
#     "Women" = "#CD718C"),
#     name = "Status",
#     labels = c(#"Laval" = "Laval", 
#       "Men" = "Hommes", "Women" = "Femmes")) +
#   scale_x_discrete(labels = function(x) gsub(",", " ", x)) +
#   scale_y_continuous(labels = convert_number) +
#   theme_minimal() +
#   gg_cc_theme_no_sf +
#   theme(legend.position = "bottom",
#         legend.title = element_blank(),
#         axis.text.x = element_text(angle = 45, hjust = 1))

# # Individual After-Tax ----------------------------------------------------
# #Pulling all vectors for after-tax (at) individual income
# indatinc_vectors <- can21$vector[which(can21$vector == "v_CA21_713"):which(can21$vector == "v_CA21_760")]
# 
# #Naming the rows for the vectors
# indatinc_names <- c("total_count", "wo_at_income", "w_at_income", "under_10",
#                     "10_20", "20_30", "30_40", "40_50", "50_60", "60_70", "70_80",
#                     "80_90", "90_100", "over_100", "100_125", "over_125")
# 
# #Filter for vectors for Total
# indat21_vectors_total <- can21 |> 
#   filter(vector %in% indatinc_vectors & type == "Total") |> 
#   pull(vector) |> 
#   set_names(indatinc_names)
# 
# #Filter for vectors for men
# indat21_vectors_men <- can21 |> 
#   filter(vector %in% indatinc_vectors & type == "Male") |> 
#   pull(vector) |> 
#   set_names(indatinc_names)
# 
# #Filter for vectors for women
# indat21_vectors_women <- can21 |> 
#   filter(vector %in% indatinc_vectors & type == "Female") |> 
#   pull(vector) |> 
#   set_names(indatinc_names)
# 
# #Pull individual AT income brackets for Laval
# indat21_lvl_total  <- get_census(dataset = "CA21", 
#                                  regions = list(CSD = 2465005), 
#                                  level = "CSD",
#                                  vectors = indat21_vectors_total) |> 
#   select(all_of(indatinc_names)) |> 
#   mutate(Geography = c("Laval")) |> 
#   select(Geography, everything())
# 
# #Pull individual AT income brackets for men
# indat21_lvl_men  <- get_census(dataset = "CA21", 
#                                regions = list(CSD = 2465005), 
#                                level = "CSD",
#                                vectors = indat21_vectors_men) |> 
#   select(all_of(indatinc_names)) |> 
#   mutate(Geography = c("Men")) |> 
#   select(Geography, everything())
# 
# #Pull individual AT income brackets for women
# indat21_lvl_women  <- get_census(dataset = "CA21", 
#                                  regions = list(CSD = 2465005), 
#                                  level = "CSD",
#                                  vectors = indat21_vectors_women) |> 
#   select(all_of(indatinc_names)) |> 
#   mutate(Geography = c("Women")) |> 
#   select(Geography, everything())
# 
# #Bind tables together
# ind_at_income <- bind_rows(indat21_lvl_total, indat21_lvl_men, indat21_lvl_women)
# 
# #Prepping table for bar graph
# ind_at_bar <- ind_at_income |> 
#   select(-total_count, -wo_at_income, -w_at_income, -over_100) |> 
#   rename("< 10.000 $" = "under_10", "10 à 19.999 $" = "10_20", "20 à 29.999 $" = "20_30",
#          "30 à 39.999 $" = "30_40", "40 à 49.999 $" = "40_50", "50 à 59.999 $" = "50_60",
#          "60 à 69.999 $" = "60_70", "70 à 79.999 $" = "70_80", "80 à 89.999 $" = "80_90",
#          "90 à 99.999 $" = "90_100", "100 à 125.000 $" = "100_125", "> 125.000 $" = "over_125") |> 
#   pivot_longer(cols = -Geography, names_to = "Bracket", values_to = "Value") |> 
#   mutate(Bracket = factor(Bracket, levels = c("< 10.000 $", "10 à 19.999 $", "20 à 29.999 $",
#                                               "30 à 39.999 $", "40 à 49.999 $", "50 à 59.999 $",
#                                               "60 à 69.999 $", "70 à 79.999 $", "80 à 89.999 $",
#                                               "90 à 99.999 $", "100 à 125.000 $", "> 125.000 $")))
# 
# #Bar Graph for individual AT income
# ggplot(ind_at_bar, aes(x = Bracket, y = Value, fill = Geography)) +
#   geom_bar(stat = "identity", position = "dodge", width = 0.7) +
#   labs(title = "Tranches de revenu individuel après impôt à Laval 2020",
#        x = "", y = "Personnes") +
#   scale_fill_manual(values = c("Laval" = "royalblue2", 
#                                "Men" = "indianred", 
#                                "Women" = "gold2"),
#                     name = "Status",
#                     labels = c("Laval" = "Laval", "Men" = "Hommes", "Women" = "Femmes")) +
#   theme_minimal() +
#   theme(legend.position = "bottom",
#         legend.title = element_blank(),
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.title = element_text(hjust = 0.5))

# # Individual Employment Brackets ------------------------------------------
# #Grabbing vectors for employment brackets
# indeminc_vectors <- can21$vector[which(can21$vector == "v_CA21_761"):which(can21$vector == "v_CA21_811")]
# 
# #Naming the rows for the vectors
# indeminc_names <- c("total_count", "wo_em_income", "w_em_income", "under_5", "5_10",
#                     "10_20", "20_30", "30_40", "40_50", "50_60", "60_70", "70_80",
#                     "80_90", "90_100", "over_100", "100_125", "over_125")
# 
# #Filter for vectors for Total
# indeminc_vectors_total <- can21 |> 
#   filter(vector %in% indeminc_vectors & type == "Total") |> 
#   pull(vector) |> 
#   set_names(indeminc_names)
# 
# #Filter for vectors for men
# indeminc_vectors_men <- can21 |> 
#   filter(vector %in% indeminc_vectors & type == "Male") |> 
#   pull(vector) |> 
#   set_names(indeminc_names)
# 
# #Filter for vectors for women
# indeminc_vectors_women <- can21 |> 
#   filter(vector %in% indeminc_vectors & type == "Female") |> 
#   pull(vector) |> 
#   set_names(indeminc_names)
# 
# #Pull individual employment income brackets for Laval
# indeminc_lvl_total  <- get_census(dataset = "CA21", 
#                                   regions = list(CSD = 2465005), 
#                                   level = "CSD",
#                                   vectors = indeminc_vectors_total) |> 
#   select(all_of(indeminc_names)) |> 
#   mutate(Geography = c("Laval")) |> 
#   select(Geography, everything())
# 
# #Pull individual employment income brackets for men
# indeminc_lvl_men  <- get_census(dataset = "CA21", 
#                                 regions = list(CSD = 2465005), 
#                                 level = "CSD",
#                                 vectors = indeminc_vectors_men) |> 
#   select(all_of(indeminc_names)) |> 
#   mutate(Geography = c("Men")) |> 
#   select(Geography, everything())
# 
# #Pull individual employment income brackets for women
# indeminc_lvl_women  <- get_census(dataset = "CA21", 
#                                   regions = list(CSD = 2465005), 
#                                   level = "CSD",
#                                   vectors = indeminc_vectors_women) |> 
#   select(all_of(indeminc_names)) |> 
#   mutate(Geography = c("Women")) |> 
#   select(Geography, everything())
# 
# #Bind tables together
# indeminc <- bind_rows(indeminc_lvl_total, indeminc_lvl_men, indeminc_lvl_women)
# 
# #Prepare table for normal interval bar graph
# indeminc_bar <- indeminc |> 
#   mutate("< 10.000 $" = under_5 + `5_10`) |> 
#   rename("10 à 19.999 $" = "10_20", "20 à 29.999 $" = "20_30",
#          "30 à 39.999 $" = "30_40", "40 à 49.999 $" = "40_50", "50 à 59.999 $" = "50_60",
#          "60 à 69.999 $" = "60_70", "70 à 79.999 $" = "70_80", "80 à 89.999 $" = "80_90",
#          "90 à 99.999 $" = "90_100", "100 à 125.000 $" = "100_125", "> 125.000 $" = "over_125") |> 
#   select(-total_count, -w_em_income, -wo_em_income, -under_5, -`5_10`, -over_100) |> 
#   pivot_longer(cols = -Geography, names_to = "Bracket", values_to = "Count") |> 
#   mutate(Bracket = factor(Bracket, levels = c("< 10.000 $", "10 à 19.999 $", "20 à 29.999 $",
#                                               "30 à 39.999 $", "40 à 49.999 $", "50 à 59.999 $",
#                                               "60 à 69.999 $", "70 à 79.999 $", "80 à 89.999 $",
#                                               "90 à 99.999 $", "100 à 125.000 $", "> 125.000 $")))
# 
# #Bar Graph for individual employment income
# ggplot(indeminc_bar, aes(x = Bracket, y = Count, fill = Geography)) +
#   geom_bar(stat = "identity", position = "dodge", width = 0.7) +
#   labs(title = "Tranches de revenu d’emploi pour les particuliers de Laval 2020",
#        x = "", y = "Personnes") +
#   scale_fill_manual(values = c("Laval" = "royalblue2", 
#                                "Men" = "indianred", 
#                                "Women" = "gold2"),
#                     name = "Status",
#                     labels = c("Laval" = "Laval", "Men" = "Hommes", "Women" = "Femmes")) +
#   theme_minimal() +
#   theme(legend.position = "bottom",
#         legend.title = element_blank(),
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.title = element_text(hjust = 0.5))


# # Evolution of Individual Income ------------------------------------------
# #Census grabber for median individual income for 2020 to start up the graph table
# mii_census_grabber_20 <- function(region, geolevel, geoname, filter_v){
#   regions_list <- list()
#   regions_list[[geolevel]] <- region
#   
#   get_census(dataset = "CA21",
#              regions = regions_list,
#              level = geolevel,
#              vectors = "v_CA21_560"
#   ) |> 
#     select(-filter_v) |> 
#     mutate(Geography = geoname, .before = 1)
# }
# 
# #Census grabber for years other than 2020
# mii_census_grabber <- function(cyear, region, geolevel, vectorid, filter_v, ryear){
#   regions_list <- list()
#   regions_list[[geolevel]] <- region
#   
#   get_census(dataset = cyear,
#              regions = regions_list,
#              level = geolevel,
#              vectors = vectorid
#   ) |> 
#     select(-filter_v) |> 
#     rename(!!ryear := last_col())
# }
# 
# 
# #Grabbing median individual income for Laval for years 2000-2020
# mii_inc20_lvl <- mii_census_grabber_20("2465005", "CSD", "Laval", (1:10)) |> 
#   rename("2020" := last_col())
# mii_inc15_lvl <- mii_census_grabber("CA16","2465005", "CSD", "v_CA16_2207", (1:10), "2015")
# mii_inc10_lvl <- mii_census_grabber("CA11", "2465005", "CSD", "v_CA11N_2341", (1:10), "2010") |> 
#   select(-1)
# mii_inc05_lvl <- mii_census_grabber("CA06", "2465005", "CSD", "v_CA06_1583", (1:10), "2005")
# mii_inc00_lvl <- mii_census_grabber("CA01", "2465005", "CSD", "v_CA01_1449", (1:10), "2000")
# #Binding the tables together
# mii_lvl_graph <- bind_cols(mii_inc20_lvl, mii_inc15_lvl, mii_inc10_lvl, mii_inc05_lvl, mii_inc00_lvl)
# 
# #Grabbing median individual income for Montreal CMA for years 2000-2020
# mii_inc20_mtl <- mii_census_grabber_20("2466023", "CSD", "Montreal", (1:10)) |> 
#   rename("2020" := last_col())
# mii_inc15_mtl <- mii_census_grabber("CA16", "2466023", "CSD", "v_CA16_2207", (1:10), "2015")
# mii_inc10_mtl <- mii_census_grabber("CA11", "2466023", "CSD", "v_CA11N_2341", (1:10), "2010") |> 
#   select(-1)
# mii_inc05_mtl <- mii_census_grabber("CA06", "2466023", "CSD", "v_CA06_1583", (1:10), "2005")
# mii_inc00_mtl <- mii_census_grabber("CA01", "2466025", "CSD", "v_CA01_1449", (1:10), "2000")
# #Bind the tables together
# mii_mtl_graph <- bind_cols(mii_inc20_mtl, mii_inc15_mtl, mii_inc10_mtl, mii_inc05_mtl, mii_inc00_mtl)
# 
# #Grabbing median individual income for Quebec for years 2000-2020
# mii_inc20_qc <- mii_census_grabber_20("24", "PR", "Quebec", (1:8)) |> 
#   rename("2020" := last_col())
# mii_inc15_qc <- mii_census_grabber("CA16", "24", "PR", "v_CA16_2207", (1:8), "2015")
# mii_inc10_qc <- mii_census_grabber("CA11", "24", "PR", "v_CA11N_2341", (1:8), "2010") |> 
#   select(-1)
# mii_inc05_qc <- mii_census_grabber("CA06", "24", "PR", "v_CA06_1583", (1:8), "2005")
# mii_inc00_qc <- mii_census_grabber("CA01", "24", "PR", "v_CA01_1449", (1:8), "2000")
# 
# #Bind the tables together
# mii_qc_graph <- bind_cols(mii_inc20_qc, mii_inc15_qc, mii_inc10_qc, mii_inc05_qc, mii_inc00_qc)
# 
# #Bind all geography tables together to make the line graph
# mii_graph <- bind_rows(mii_lvl_graph, mii_mtl_graph, mii_qc_graph) |> 
#   pivot_longer(cols = -Geography, names_to = "Year", values_to = "Income")
# 
# #Year over year growth table
# mii_yoygraph <- bind_rows(mii_lvl_graph, mii_mtl_graph, mii_qc_graph) |> 
#   mutate("2020" = round(100 * `2020` / `2015` - 100, 2),
#          "2015" = round(100 * `2015` / `2010` - 100, 2),
#          "2010" = round(100 * `2010` / `2005` - 100, 2),
#          "2005" = round(100 * `2005` / `2000` - 100, 2)) |> 
#   select(Geography, "2005", "2010", "2015", "2020") |> 
#   pivot_longer(cols = -Geography, names_to = "Year", values_to = "Growth")
# 
# #Creating a table with CPI index values from 2000 to 2020
# cpi <- data.frame(
#   Geography = "CPI",
#   Year = c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008",
#            "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017",
#            "2018", "2019", "2020", "2021", "2022", "2023", "2024"),
#   Index = c(100, 103.8, 104.3, 107.4, 109.9, 111.7, 114.6, 116.4, 119.1, 119.2, 120.4,
#             124.6, 127, 127.8, 129.9, 131.5, 132.4, 133.3, 135.6, 138.8, 138.3, 144,
#             154.8, 160.9, 165.9)
# )
# 
# #Creating and binding yoy growth with CPI src:https://www150.statcan.gc.ca/n1/pub/71-607-x/2018016/cpilg-ipcgl-eng.htm
# mii_yoygraph_cpi <- bind_rows(mii_lvl_graph, mii_mtl_graph, mii_qc_graph) |> 
#   mutate("2020y" = `2020` / `2015`,
#          "2015y" = `2015` / `2010`,
#          "2010y" = `2010` / `2005`,
#          "2005y" = `2005` / `2000`) |> 
#   mutate("2000yoy" = 100,
#          "2005yoy" = 100 * `2005y`,
#          "2010yoy" = 100 * `2005y` * `2010y`,
#          "2015yoy" = 100 * `2005y` * `2010y` * `2015y`,
#          "2020yoy" = 100 * `2005y` * `2010y` * `2015y` * `2020y`) |> 
#   select(Geography, "2000yoy", "2005yoy", "2010yoy", "2015yoy", "2020yoy") |> 
#   rename("2000" = "2000yoy", "2005" = "2005yoy", "2010" = "2010yoy",
#          "2015" = "2015yoy", "2020" = "2020yoy") |> 
#   pivot_longer(cols = -Geography, names_to = "Year", values_to = "Index") |> 
#   bind_rows(cpi)
# 
# #Creating the line graph for median individual income
# ggplot(mii_graph, aes(x = Year, y = Income, color = Geography, group = Geography)) +
#   geom_line(linewidth = 1.5) +
#   labs(title = "Revenu médian individuel 2000-2020",
#        x = "Année",
#        y = "Revenu médian individuel ($)") +
#   theme_minimal() +
#   theme(legend.position = "bottom", legend.box = "horizontal",
#         legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
# 
# #Line graph for mii yoy growth
# ggplot(mii_yoygraph, aes(x = Year, y = Growth, color = Geography, group = Geography)) +
#   geom_line(linewidth = 1.5) +
#   scale_color_manual(values = c("CPI" = "grey", "Laval" = "royalblue2",
#                                 "Montreal" = "indianred3", "Quebec" = "gold2")) +
#   labs(title = "Individual Median Income Growth 2005-2020",
#        x = "Année",
#        y = "Income Growth (%)") +
#   theme_minimal() +
#   theme(legend.position = "bottom", legend.title = element_blank(),
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.title = element_text(hjust = 0.5))
# 
# #Plot for year over year growth
# ggplot(mii_yoygraph, aes(x = Year, y = Growth, fill = Geography)) +
#   geom_bar(stat = "identity", position = "dodge", width = 0.7) +
#   labs(title = "Croissance du revenu médian individuel 2005-2020",
#        x = "Année", y = "Croissance des revenus (%)") +
#   theme_minimal() +
#   theme(legend.position = "bottom",
#         legend.title = element_blank(),
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.title = element_text(hjust = 0.5))
# 
# #Line graph for CPI and mii index
# ggplot(mii_yoygraph_cpi, aes(x = Year, y = Index, color = Geography, group = Geography)) +
#   geom_line(linewidth = 1.5) +
#   scale_color_manual(values = c("CPI" = "grey", "Laval" = "royalblue2",
#                                 "Montreal" = "indianred3", "Quebec" = "gold2"),
#                      labels = c("IPC", "Laval", "Montreal", "Quebec")) +
#   labs(title = "Revenu médian individuel par rapport à l'IPC 2000-2020",
#        x = "Année",
#        y = "Revenu médian individuel normalisé") +
#   theme_minimal() +
#   theme(legend.position = "bottom", legend.title = element_blank(),
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.title = element_text(hjust = 0.5))

# # Maps --------------------------------------------------------------------
# #Grabbing the data for median income in shapefile format
# laval_medinc <- get_census(dataset = "CA21", 
#                            regions = list(CSD = 2465005), 
#                            vectors = c("med_inc" = "v_CA21_560"),
#                            level = "CT",
#                            geo_format = "sf")
# 
# #Mapping median income
# ggplot(data = laval_medinc) +
#   geom_sf(aes(fill = med_inc), color = NA) +
#   labs(title = "Revenu individuel médian à Laval 2020", color = "Dollars $", fill = "Revenu médian $") +
#   scale_fill_gradientn(colors = curbcut_scale, na.value = curbcut_na) +
#   theme_minimal() +
#   theme(axis.line = element_blank(), axis.text = element_blank(),
#         axis.title = element_blank(), axis.ticks = element_blank(),
#         panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
#         legend.position = "bottom", legend.justification = "center") +
#   guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
#                                barwidth = 10, barheight = 1))
# 
# #Grabbing the data for hh median income in shapefile format
# laval_hhmedinc <- get_census(dataset = "CA21", 
#                              regions = list(CSD = 2465005), 
#                              vectors = c("med_inc" = "v_CA21_906"),
#                              level = "CT",
#                              geo_format = "sf")
# 
# #Mapping household median income
# ggplot(data = laval_hhmedinc) +
#   geom_sf(aes(fill = med_inc), color = NA) +
#   labs(title = "Revenu médian des ménages à Laval 2020", color = "Dollars $", fill = "Revenu médian $") +
#   scale_fill_gradientn(colors = curbcut_scale, na.value = curbcut_na) +
#   theme_minimal() +
#   theme(axis.line = element_blank(), axis.text = element_blank(),
#         axis.title = element_blank(), axis.ticks = element_blank(),
#         panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
#         legend.position = "bottom", legend.justification = "center") +
#   guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
#                                barwidth = 15, barheight = 1))



# Faible revenu -----------------------------------------------------------

vecs_frev <- c(limat_all_pct = "v_CA16_2540", limat_0_17_pct =  "v_CA16_2543", 
               limat_18_64_pct = "v_CA16_2549", limat_65_plus_pct = "v_CA16_2552")

frev <- get_census(dataset = "CA16",
                   regions = list(CSD = 2465005),
                   level = "CSD",
                   vectors = vecs_frev) |> 
  mutate(Région = "Ville de Laval") |> 
  select(c("Région", names(vecs_frev)))
frev_QC <- get_census(dataset = "CA16",
                      regions = list(PR = 24),
                      level = "PR",
                      vectors = vecs_frev) |> 
  mutate(Région = "Ensemble du Québec") |> 
  select(c("Région", names(vecs_frev)))
frev <- rbind(frev, frev_QC) |> 
  mutate(across(ends_with("_pct"), \(x) x/100))

# Transformation en format long
frev_long <- frev |> 
  pivot_longer(cols = -Région, names_to = "variable", values_to = "valeur")

# Séparer les données en absolu et en pourcentage
frev_pct <- frev_long |> 
  filter(str_ends(variable, "_pct")) |> 
  mutate(variable = str_replace(variable, "_pct", ""),
         variable = str_replace(variable, "limat_", ""))

frev_pct_old_2015 <- frev_pct$valeur[frev_pct$Région == "Ville de Laval" &
                                       frev_pct$variable == "65_plus"] |> 
  convert_pct()

frev_pct_2015 <- frev_pct$valeur[frev_pct$Région == "Ville de Laval" &
                                   frev_pct$variable == "all"] |> 
  convert_pct()


vecs_frev <- c(limat_all = "v_CA21_1025", limat_0_17 = "v_CA21_1028", 
               limat_18_64 = "v_CA21_1034", limat_65_plus = "v_CA21_1037", 
               limat_all_pct = "v_CA21_1040", limat_0_17_pct =  "v_CA21_1043", 
               limat_18_64_pct = "v_CA21_1049", limat_65_plus_pct = "v_CA21_1052")

frev <- get_census(dataset = "CA21",
                   regions = list(CSD = 2465005),
                   level = "CSD",
                   vectors = vecs_frev) |> 
  mutate(Région = "Ville de Laval") |> 
  select(c("Région", names(vecs_frev)))
frev_QC <- get_census(dataset = "CA21",
                      regions = list(PR = 24),
                      level = "PR",
                      vectors = vecs_frev) |> 
  mutate(Région = "Ensemble du Québec") |> 
  select(c("Région", names(vecs_frev)))
frev <- rbind(frev, frev_QC) |> 
  mutate(across(ends_with("_pct"), \(x) x/100))

# Transformation en format long
frev_long <- frev |> 
  pivot_longer(cols = -Région, names_to = "variable", values_to = "valeur")

# Séparer les données en absolu et en pourcentage
frev_abs <- frev_long |> 
  filter(!str_ends(variable, "_pct")) |> 
  mutate(variable = str_replace(variable, "limat_", ""))
frev_pct <- frev_long |> 
  filter(str_ends(variable, "_pct")) |> 
  mutate(variable = str_replace(variable, "_pct", ""),
         variable = str_replace(variable, "limat_", ""))

# Réorganisation en format large
frev_wide <- frev_abs |> 
  left_join(frev_pct, by = c("variable", "Région")) |> 
  pivot_wider(names_from = Région, values_from = c(valeur.x, valeur.y))

frev_wide$variable <- gsub("all", "Total", frev_wide$variable)
frev_wide$variable <- gsub("0_17", "0 à 17 ans", frev_wide$variable)
frev_wide$variable <- gsub("18_64", "18 à 64 ans", frev_wide$variable)
frev_wide$variable <- gsub("65_plus", "65+ ans", frev_wide$variable)

# Création de la table GT
frev_wide_table <- 
frev_wide |> 
  gt(rowname_col = "variable") |> 
  cols_label(
    `valeur.x_Ville de Laval` = "Ville de Laval (n)",
    `valeur.y_Ville de Laval` = "Ville de Laval (%)",
    `valeur.x_Ensemble du Québec` = "Ensemble du Québec (n)",
    `valeur.y_Ensemble du Québec` = "Ensemble du Québec (%)"
  ) |> 
  fmt(columns = 2:3, fns = convert_number) |>
  fmt(columns = 4:5, fns = convert_pct) |> 
  data_color(
    columns = 4:5,
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = NULL
    )
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

gtsave(frev_wide_table, "output/axe1/income/frev_wide_table.png")

frev_total <- convert_pct(frev_wide$`valeur.y_Ville de Laval`[frev_wide$variable == "Total"])
frev_total_QC <- convert_pct(frev_wide$`valeur.y_Ensemble du Québec`[frev_wide$variable == "Total"])

frev_total_old <- convert_pct(frev_wide$`valeur.y_Ville de Laval`[frev_wide$variable == "65+ ans"])
frev_total_old_QC <- convert_pct(frev_wide$`valeur.y_Ensemble du Québec`[frev_wide$variable == "65+ ans"])

# SF
frev_sf <- get_census(dataset = "CA21",
                      regions = list(CSD = 2465005),
                      level = "DA",
                      vectors = vecs_frev,
                      geo_format = "sf") |> 
  mutate(across(ends_with("_pct"), \(x) x/100))

# Map loyer médian par DA
labels <- c("0 % - 4 %", "4 % - 8 %", "8 % - 12 %", "12 % - 16 %", "+16 %")
t <- add_bins(df = frev_sf,
              variable = "limat_all_pct",
              breaks = c(-Inf, 0.04, 0.08, 0.12, 0.16, Inf),
              labels = labels
)

lvls <- levels(t$binned_variable)
lvls <- c(lvls[length(lvls)], lvls[-length(lvls)])

frev_sf_plot <-
  ggplot(data = t) +
  gg_cc_tiles +
  geom_sf(aes(fill = binned_variable), color = "transparent", lwd = 0) +
  scale_fill_manual(
    values = curbcut_colors$left_5$fill[c(1:6)],  # Include NA color first
    na.value = curbcut_colors$left_5$fill[1],
    name = element_blank(),
    breaks = lvls,  # NA first
    labels = lvls,  # NA label first
    guide = guide_legend(
      title.position = "top",
      label.position = "bottom",
      keywidth = 4,
      keyheight = 0.5
    )
  ) +
  gg_cc_theme

ggplot2::ggsave(filename = here::here("output/axe1/income/frev_sf_plot.pdf"),
                plot = frev_sf_plot, width = 6.5, height = 4)


# Tableau des loyers médians par secteurs
frev_sf_2021 <- frev_sf
z <- sf::st_intersects(sf::st_centroid(frev_sf_2021), laval_sectors)
frev_sf_2021$secteur <- sapply(z, \(x) {
  if (length(x) == 0) return(NA)
  laval_sectors$name[x]
})
frev_sf_2021 <-
  frev_sf_2021 |>
  group_by(secteur) |>
  summarize(limat_all_pct = weighted_mean(limat_all_pct, Households, na.rm = TRUE))
frev_sf_2021 <- frev_sf_2021[1:6, ]
frev_sf_2021 <- sf::st_drop_geometry(frev_sf_2021)
names(frev_sf_2021) <- c("Secteur", "Faible revenu (%)")

frev_sector_table <-
  gt(frev_sf_2021) |> 
  # Appliquer une mise en couleur sur les colonnes médianes
  data_color(
    columns = 2,
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = NULL
    )
  ) |> 
  fmt(columns = 2, fns = convert_pct) |> 
  # Ajouter des bordures en haut de chaque groupe de ligne
  tab_style(
    style = cell_borders(
      sides = c("top"),
      color = "white",
      weight = px(10)
    ),
    locations = cells_row_groups()
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

gtsave(frev_sector_table, "output/axe1/housing/frev_sector_table.png")










# Défavorisation materielle et sociale ------------------------------------

library(httr)
library(sf)

fetch_all_pages_defav <- function(base_url) {
  # Function to fetch data with pagination and error handling
  # Set initial query parameters
  query_params <- list(
    where = "1=1", # to get all the data; no filter
    outFields = "*", # to get all fields
    outSR = "4326", # output spatial reference; EPSG:4326 is WGS84 lat/long
    f = "geojson", # output format
    returnGeometry = "true" # to ensure geometry is included
  )
  
  all_data <- list()
  offset <- 0
  keep_fetching <- TRUE
  
  while (keep_fetching) {
    # Update the query parameters with the current offset and limit
    query_params$resultOffset <- offset
    query_params$resultRecordCount <- 1000
    
    # Make the GET request
    response <- httr::GET(url = base_url, query = query_params)
    
    # Check if the request was successful
    if (httr::status_code(response) != 200) {
      stop("Failed to retrieve data: ", httr::status_code(response))
    }
    
    # Inspect the content type to ensure it's GeoJSON
    content_type <- httr::headers(response)$`content-type`
    if (!grepl("application/geo", content_type, ignore.case = TRUE)) {
      break
    }
    
    # Read the content as geojson and convert to an sf object
    page_data <- tryCatch(
      sf::st_read(httr::content(response, "text"), quiet = TRUE),
      error = function(e) {
        message("Error reading data: ", e$message)
        return(NULL)
      }
    )
    
    # Check if data was successfully read
    if (is.null(page_data) || nrow(page_data) == 0) {
      keep_fetching <- FALSE
    } else {
      # Store the fetched data
      all_data[[length(all_data) + 1]] <- page_data
      # Increment the offset for the next page
      offset <- offset + 1000
      print(length(all_data))
    }
  }
  
  all_data <- Reduce(rbind, all_data)
  all_data <- sf::st_make_valid(all_data)
  
  names(all_data) <- tolower(names(all_data))
  sf::st_filter(all_data, lvl)[c("adidu", tolower("QuintMatSocRSS"))] 
}

defav_2021 <- fetch_all_pages_defav(
  "https://public.arcgis.msss.rtss.qc.ca/arcgis/rest/services/Defavorisation/defavorisation_2006_2011_2016_2021/MapServer/2/query"
)
defav_2016 <- fetch_all_pages_defav(
  "https://public.arcgis.msss.rtss.qc.ca/arcgis/rest/services/Defavorisation/defavorisation_2006_2011_2016_2021/MapServer/10/query"
)

defav_2021 <- defav_2021[order(defav_2021$adidu),]
defav_2016 <- defav_2016[order(defav_2016$adidu),]

names(defav_2016)[1:2] <- c("adidu", "quintmatsocrss_2016")
defav <- left_join(defav_2021, sf::st_drop_geometry(defav_2016))
defav$defav_var <- defav$quintmatsocrss - defav$quintmatsocrss_2016

defav <- defav_2021 |> 
  mutate(defav = case_when(quintmatsocrss %in% c(1,2) ~ "Favorisés",
                           quintmatsocrss %in% c(3) ~ "Intermediaires",
                           quintmatsocrss %in% c(4,5) ~ "Défavorisés (mat.)",
                           quintmatsocrss %in% c(6,7) ~ "Défavorisés (soc.)",
                           quintmatsocrss %in% c(8,9) ~ "Défavorisés (mat. et soc.)"))

defav$defav <- factor(defav$defav, labels = c("Défavorisés (mat. et soc.)",
                                              "Défavorisés (soc.)",
                                              "Défavorisés (mat.)",
                                              "Intermediaires",
                                              "Favorisés"))
defav$defav <- addNA(defav$defav)
lvls <- levels(defav$defav)
lvls <- c(lvls[length(lvls)], rev(lvls[-length(lvls)]))

defav_plot <- 
  ggplot(data = defav) +
  gg_cc_tiles +
  geom_sf(aes(fill = defav), color = "transparent", lwd = 0) +
  scale_fill_manual(
    values = c(curbcut_colors$left_5$fill[1], 
               c("#E8E8E8", "#B7CBCC", "#6C83B5", "#73AE80", "#2A5A5B")),
    na.value = curbcut_colors$left_5$fill[1],
    name = element_blank(),
    breaks = lvls,
    labels = lvls,
    guide = guide_legend(
      title.position = "top",
      label.position = "bottom",
      keywidth = 4,
      keyheight = 0.5
    )
  ) +
  gg_cc_theme


ggplot2::ggsave(filename = here::here("output/axe1/income/defav_plot.pdf"),
                plot = defav_plot, width = 6.5, height = 4)


qs::qsavem(rev_med, rev_med_2015, rev_med_aug, rev_med_aug_QC, mhh_plot,
           median_income_sf_plot, median_income_table, inc_ind_less50,
           inc_ind_all, inc_ind_less50_pct, inc_ind_less50, inc_ind_between_20k30k,
           inc_ind_less50_pct, inc_ind_between_20k30k, revenue_annuel_plot,
           mii_plot, median_income_ind_plot, median_income_ind_table,
           median_income_hommes, median_income_femmes, inc_f_x_h,
           median_income_hommes_2016, median_income_femmes_2016, inc_f_x_h_2016,
           med_inc_femmes_aug, med_inc_hommes_aug, defav_plot, frev_pct_old_2015, 
           frev_pct_2015, frev_wide_table, frev_total, frev_total_QC, 
           frev_total_old, frev_total_old_QC, frev_sf_plot, frev_sector_table,
           rev_mid_ind_2015, rev_mid_ind, rev_mid_ind_aug, rev_mid_ind_QC, 
           rev_mid_ind_2015_QC, rev_mid_ind_aug_QC,
           file = "data/axe1/income.qsm")




# socio21vmap <- read_xlsx("/Users/justin/Documents/R/curbcut/socio21.xlsx") |> 
#   filter(MUNIC == 2465005) |> 
#   na.omit() |> 
#   select(AD, NOTEMAT, NOTESOC) |> 
#   mutate(AD = as.character(AD))
# socio21 <- read_xlsx("D://Mcgill/can_cache/socio21.xlsx")
# socio16 <- read_xlsx("D://Mcgill/can_cache/socio16.xlsx", sheet = 2)
# 
# #Joining the dissemination area data with socio21vmap
# socio21map <- left_join(laval_da, socio21vmap, join_by("name" == "AD"))
# 
# #Finding the Jenks breaks for the binned maps
# material_jenks <- classInt::classIntervals(socio21map$NOTEMAT, n = 5, style = "jenks")$brks
# social_jenks <- classInt::classIntervals(socio21map$NOTESOC, n = 5, style = "jenks")$brks
# 
# #Creating own breaks for binned maps
# material_breaks <- c(-Inf, -0.034, -0.005, 0.019, 0.049, Inf)
# social_breaks <- c(-Inf, -0.072, -0.035, -0.001, 0.038, Inf)
# 
# #Labels for binned material map
# material_labels <- c("< -0,034", "-0,034 à -0,005", "-0,005 à 0,019",
#                      "0,019 à 0,049", "> 0,049")
# social_labels <- c("< -0,072", "-0,072 à -0,035", "-0,035 à -0,001",
#                    "-0,001 à 0,038", "> 0,154")
# 
# #Plotting material deprivation (continuous)
# ggplot(data = socio21map) +
#   geom_sf(aes(fill = NOTEMAT), color = NA) +
#   labs(title = "La défavorisation matérielle à Laval 2021",
#        fill = "Score de défavorisation matérielle") +
#   scale_fill_gradientn(colors = curbcut_scale, na.value = curbcut_na) +
#   theme_minimal() +
#   theme(axis.line = element_blank(), axis.text = element_blank(),
#         axis.title = element_blank(), axis.ticks = element_blank(),
#         panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
#         legend.position = "bottom", legend.justification = "center") +
#   guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
#                                barwidth = 10, barheight = 1))
# 
# #Binned material deprivation map
# ggplot(data = socio21map) +
#   geom_sf(aes(fill = cut(NOTEMAT, breaks = material_breaks, labels = material_labels)), color = NA) +
#   labs(title = "La défavorisation matérielle à Laval 2021",
#        fill = "Score de défavorisation matérielle") +
#   scale_fill_manual(values = curbcut_scale, na.value = curbcut_na) +
#   theme_minimal() +
#   theme(axis.line = element_blank(), axis.text = element_blank(),
#         axis.title = element_blank(), axis.ticks = element_blank(),
#         panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
#         legend.position = "bottom", legend.justification = "center") +
#   guides(fill = guide_legend(title.position = "top", title.hjust = 0.5,
#                              barwidth = 1, barheight = 1, nrow = 1))
# 
# #Mapping social deprivation
# ggplot(data = socio21map) +
#   geom_sf(aes(fill = SCORESOC), color = NA) +
#   labs(title = "La défavorisation sociale à Laval 2021",
#        fill = "Score de défavorisation sociale") +
#   scale_fill_gradientn(colors = curbcut_scale, na.value = curbcut_na) +
#   theme_minimal() +
#   theme(axis.line = element_blank(), axis.text = element_blank(),
#         axis.title = element_blank(), axis.ticks = element_blank(),
#         panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
#         legend.position = "bottom", legend.justification = "center") +
#   guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
#                                barwidth = 10, barheight = 1))
# 
# #Social deprivation binned
# ggplot(data = socio21map) +
#   geom_sf(aes(fill = cut(NOTESOC, breaks = social_breaks, labels = social_labels)), color = NA) +
#   labs(title = "La défavorisation sociale à Laval 2021",
#        fill = "Score de défavorisation sociale") +
#   scale_fill_manual(values = curbcut_scale, na.value = curbcut_na) + # Use custom palette
#   theme_minimal() +
#   theme(axis.line = element_blank(), axis.text = element_blank(),
#         axis.title = element_blank(), axis.ticks = element_blank(),
#         panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
#         legend.position = "bottom", legend.justification = "center") +
#   guides(fill = guide_legend(title.position = "top", title.hjust = 0.5,
#                              barwidth = 1, barheight = 1, nrow = 1))
# 
# # YoY for median individual income ---------------------------------------
# #Grabbing vectors for median individual income
# inc21v <- c("total" = "v_CA21_560", "men" = "v_CA21_561", "women" = "v_CA21_562")
# inc16v <- c("total" = "v_CA16_2207", "men" = "v_CA16_2208", "women" = "v_CA16_2209")
# inc11v <- c("total" = "v_CA11N_2341", "men" = "v_CA11N_2342", "women" = "v_CA11N_2343")
# inc06v <- c("total" = "v_CA06_1583", "men" = "v_CA06_1605", "women" = "v_CA06_1627")
# inc01v <- c("total" = "v_CA01_1449", "men" = "v_CA01_1471", "women" = "v_CA01_1493")
# 
# #Creating a function to grab census data
# inc_grabber <- function(dyear, vector, year){
#   get_census(dataset = dyear, 
#              regions = list(CSD = 2465005), 
#              level = "CSD",
#              vectors = vector) |> 
#     mutate(Year = year) |> 
#     select(Year, total, men, women)
# }
# 
# inc21 <- inc_grabber("CA21", inc21v, "2021")
# inc16 <- inc_grabber("CA16", inc16v, "2016")
# inc11 <- inc_grabber("CA11", inc11v, "2011")
# inc06 <- inc_grabber("CA06", inc06v, "2006")
# inc01 <- inc_grabber("CA01", inc01v, "2001")
# 
# income <- bind_rows(inc01, inc06, inc11, inc16, inc21) |> 
#   arrange(Year) |> 
#   mutate(
#     total_growth = (total / lag(total) - 1) * 100,
#     men_growth = (men / lag(men) - 1) * 100,
#     women_growth = (women / lag(women) - 1) * 100
#   ) |> 
#   select(-total, -men, -women) |> 
#   slice(-1) |> 
#   rename("Laval" = "total_growth", "Hommes" = "men_growth", "Femmes" = "women_growth") |> 
#   pivot_longer(-Year, names_to = "type", values_to = "growth") |> 
#   mutate(type = factor(type, levels = c("Laval", "Hommes", "Femmes")))
# 
# ggplot(income, aes(x = Year, y = growth, fill = type)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   facet_wrap(~type) +
#   scale_fill_manual(values = c("Laval" = "chartreuse3", "Hommes" = "royalblue3", "Femmes" = "indianred2")) +
#   labs(title = "Croissance du revenu médian individuel à Laval 2001-2021",
#        x = "Année", y = "Croissance du revenu sur 5 ans (%)") +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
#         legend.position = "bottom", legend.justification = "center")
