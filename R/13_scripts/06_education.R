### Scolarité ##################################################################

source("R/01_startup.R")

# Nombre d'élèves ---------------------------------------------------------

student_data <- data.frame(
  annee = c("2014-2015", "2015-2016", "2016-2017", "2017-2018", 
                       "2018-2019", "2019-2020", "2020-2021", "2021-2022", 
                       "2022-2023"),
  eleves = c(67303, 68259, 69259, 68175, 68870, 68926, 69568, 
                        69028, 70839)
)
student_data$region <- "Laval"
quebec_student_data <- data.frame(
  annee = c("2014-2015", "2015-2016", "2016-2017", "2017-2018", 
                       "2018-2019", "2019-2020", "2020-2021", "2021-2022", 
                       "2022-2023"),
  eleves = c(1318225, 1327559, 1341966, 1349416, 1366432, 
                        1371788, 1372844, 1389998, 1414934)
)
quebec_student_data$region <- "Québec"

# Combine the two data frames
students <- bind_rows(student_data, quebec_student_data)

# Normalize the data so that the first year starts at 100
students <- students %>%
  group_by(region) %>%
  mutate(normalized_eleves = (eleves / eleves[1]) * 100)

# Create the line graph with normalized data
education_indice_plot <- 
  ggplot(students, aes(x = annee, y = normalized_eleves, color = region, group = region)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Laval" = color_theme("greenecology"), "Québec" = color_theme("blueexplorer"))) +
  labs(title = NULL,
       x = "Année scolaire",
       y = "Nombre d'élèves (index base 100)",
       color = "Région") +
  gg_cc_theme_no_sf +
  theme(legend.title = element_blank())

education_eleves_var <- convert_pct(students$normalized_eleves[
  students$annee == "2022-2023" & students$region == "Laval"]/100-1)
education_eleves_var_qc <- convert_pct(students$normalized_eleves[
  students$annee == "2022-2023" & students$region == "Québec"]/100-1)


# Sorties sans diplôme ----------------------------------------------------

library(ggplot2)
library(dplyr)

# Création des données pour Laval
laval_dropout <- data.frame(
  annee = c("2013-2014", "2014-2015", "2015-2016", "2016-2017", 
            "2017-2018", "2018-2019", "2019-2020", "2020-2021", 
            "2021-2022"),
  Total = c(16.1, 14.8, 13.4, 14.3, 14.3, 13.3, 12.3, 14.0, 14.0),
  Masculin = c(19.8, 18.3, 15.6, 18.0, 16.9, 15.4, 14.2, 16.2, 17.9),
  `Féminin` = c(12.5, 11.4, 11.3, 10.2, 11.2, 10.9, 10.2, 11.6, 10.3)
)
laval_dropout$region <- "Laval"

# Création des données pour l'ensemble du Québec
quebec_dropout <- data.frame(
  annee = c("2013-2014", "2014-2015", "2015-2016", "2016-2017", 
            "2017-2018", "2018-2019", "2019-2020", "2020-2021", 
            "2021-2022"),
  Total = c(16.2, 15.5, 14.6, 15.1, 15.4, 16.2, 14.9, 15.6, 18.2),
  Masculin = c(19.8, 19.2, 18.1, 18.8, 19.1, 20.4, 18.1, 19.3, 22.4),
  `Féminin` = c(12.6, 11.9, 11.2, 11.5, 11.8, 12.1, 11.7, 11.9, 14.1)
)
quebec_dropout$region <- "Québec"

# Combine the two data frames
dropout_data <- bind_rows(laval_dropout, quebec_dropout)

# Reshape the data for ggplot
dropout_data_long <- dropout_data %>%
  pivot_longer(cols = c("Total", "Masculin", "Féminin"), 
               names_to = "gender", 
               values_to = "taux")
dropout_data_long$taux <- dropout_data_long$taux / 100

# Créer le graphique des taux de sortie sans diplôme par genre et par région
dropout_plot <- ggplot(dropout_data_long, 
       aes(x = annee, y = taux, group = interaction(gender, region),
           color = gender, alpha = region)) +
  geom_line(size = 1) +
  geom_point(data = dropout_data_long[dropout_data_long$region == "Laval",], size = 2) +
  scale_color_manual(values = c("Total" = color_theme("browndemographics"), 
                                "Masculin" = color_theme("blueexplorer"), 
                                "Féminin" = color_theme("pinkhealth")),
                     guide = guide_legend(override.aes = list(size = 5, shape = 15,
                                                              linetype = 0))) +
  scale_alpha_manual(values = c(Laval = 1, Québec = 0.3)) +
  scale_y_continuous(labels = convert_pct) +
  labs(title = element_blank(),
       x = "Année scolaire",
       y = "Taux de sortie sans diplôme (%)",
       color = "Genre", 
       alpha = "Région") +
  gg_cc_theme_no_sf +
  theme(legend.title.position = "top")

ggplot2::ggsave(filename = here::here("output/axe1/education/dropout_plot.pdf"), 
                plot = dropout_plot, width = 7.5, height = 4)


# High school diploma -----------------------------------------------------
# Grabbing necessary vectors for 2006-2021
hs_21v <- c("total" = "v_CA21_5865", 
            "no_diploma" = "v_CA21_5868", 
            "highschool" = "v_CA21_5871",
            "postsecondary" = "v_CA21_5874",
            "bachelor_higher" = "v_CA21_5895")

hs_16v <- c("total" = "v_CA16_5096", 
            "no_diploma" = "v_CA16_5099", 
            # "postsecondary" = "v_CA16_5105",
            "bachelor_higher" = "v_CA16_5123")

hs_11v <- c("total" = "v_CA11N_1801", 
            "no_diploma" = "v_CA11N_1804", 
            # "postsecondary" = "v_CA11N_1810",
            "bachelor_higher" = "v_CA11N_1822")

hs_06v <- c("total" = "v_CA06_1248", 
            "no_diploma" = "v_CA06_1249", 
            # "postsecondary" = "",
            "bachelor_higher" = "v_CA06_1256")


# Creating the functions to grab the actual data
census_grab <- function(year, region, level, vector_list, region_name){
  cyear <- gsub("20", "CA", year)
  get_census(dataset = cyear,
             regions = region,
             level = level,
             vectors = vector_list) |> 
    mutate(region = region_name, year = year) |> 
    select(year, region, total, no_diploma, bachelor_higher)
}

# Grabbing the data using the functions
educ_21 <- census_grab(year = "2021", 
                       region = list(CSD = 2465005), 
                       level = "CSD", 
                       vector_list = hs_21v,
                       region_name = "Laval")
educ_16 <- census_grab(year = "2016", 
                       region = list(CSD = 2465005), 
                       level = "CSD", 
                       vector_list = hs_16v,
                       region_name = "Laval")
educ_11 <- census_grab(year = "2011", 
                       region = list(CSD = 2465005), 
                       level = "CSD", 
                       vector_list = hs_11v,
                       region_name = "Laval")
educ_06 <- census_grab(year = "2006", 
                       region = list(CSD = 2465005), 
                       level = "CSD", 
                       vector_list = hs_06v,
                       region_name = "Laval")
educ_laval <- rbind(educ_21, educ_16, educ_11, educ_06)

# Same for Quebec
educ_21 <- census_grab(year = "2021", 
                       region = list(PR = 24), 
                       level = "PR", 
                       vector_list = hs_21v,
                       region_name = "Québec")
educ_16 <- census_grab(year = "2016", 
                       region = list(PR = 24), 
                       level = "PR", 
                       vector_list = hs_16v,
                       region_name = "Québec")
educ_11 <- census_grab(year = "2011", 
                       region = list(PR = 24), 
                       level = "PR", 
                       vector_list = hs_11v,
                       region_name = "Québec")
educ_06 <- census_grab(year = "2006", 
                       region = list(PR = 24), 
                       level = "PR", 
                       vector_list = hs_06v,
                       region_name = "Québec")
educ_qc <- rbind(educ_21, educ_16, educ_11, educ_06)

educ <- rbind(educ_laval, educ_qc)
educ$no_diploma_pct <- educ$no_diploma / educ$total
educ$bachelor_higher_pct <- educ$bachelor_higher / educ$total

# Créer les pourcentages pour Laval en 2006 et 2021
no_diploma_pct_2006_laval <- convert_pct(educ %>% filter(year == 2006 & region == "Laval") %>% pull(no_diploma_pct))
no_diploma_pct_2021_laval <- convert_pct(educ %>% filter(year == 2021 & region == "Laval") %>% pull(no_diploma_pct))
bachelor_higher_pct_2006_laval <- convert_pct(educ %>% filter(year == 2006 & region == "Laval") %>% pull(bachelor_higher_pct))
bachelor_higher_pct_2021_laval <- convert_pct(educ %>% filter(year == 2021 & region == "Laval") %>% pull(bachelor_higher_pct))

# Créer les pourcentages pour le Québec en 2006 et 2021
no_diploma_pct_2006_qc <- convert_pct(educ %>% filter(year == 2006 & region == "Québec") %>% pull(no_diploma_pct))
no_diploma_pct_2021_qc <- convert_pct(educ %>% filter(year == 2021 & region == "Québec") %>% pull(no_diploma_pct))
bachelor_higher_pct_2006_qc <- convert_pct(educ %>% filter(year == 2006 & region == "Québec") %>% pull(bachelor_higher_pct))
bachelor_higher_pct_2021_qc <- convert_pct(educ %>% filter(year == 2021 & region == "Québec") %>% pull(bachelor_higher_pct))


educ_CT_21 <- get_census(dataset = "CA21",
                         regions = list(CSD = 2465005),
                         level = "DA",
                         vectors = hs_21v, 
                         geo_format = "sf")

educ_CT_21$no_diploma_pct <- educ_CT_21$no_diploma / educ_CT_21$total
educ_CT_21$bachelor_higher_pct <- educ_CT_21$bachelor_higher / educ_CT_21$total

t <- add_bins(df = educ_CT_21,
              variable = "bachelor_higher_pct",
              breaks = c(-Inf, 0.1, 0.2, 0.3, .4, Inf),
              labels = c("0 % - 10%", "10 % - 20 %", "20 % - 30 %", "30 % - 40 %", "+40 %")
)

t <- Reduce(rbind,
            split(t, t$binned_variable) |>
              lapply(\(x) {
                out <- tibble::tibble(x$binned_variable)
                out$geometry <- sf::st_union(x)
                sf::st_as_sf(out, crs = 4326)[1, ]
              })
) |> sf::st_as_sf()
names(t)[1] <- "binned_variable"

diplome_uni <- 
  ggplot(data = t) +
  gg_cc_tiles +
  geom_sf(aes(fill = binned_variable), color = "transparent", lwd = 0) +
  scale_fill_manual(
    values = curbcut_colors$left_5$fill[2:6],
    name = "Diplôme universitaire (%)",
    guide = guide_legend(
      title.position = "top",
      label.position = "bottom",
      keywidth = 1,
      keyheight = 0.5
    )
  ) +
  gg_cc_theme

t <- add_bins(df = educ_CT_21,
              variable = "no_diploma_pct",
              breaks = c(-Inf, 0.05, 0.1, 0.15, 0.2, Inf),
              labels = c("0 % - 2.5 %", "2.5 % - 5 %", "5 % - 7.5 %", "7.5 % - 10 %", "+10 %")
)

t <- Reduce(rbind,
            split(t, t$binned_variable) |>
              lapply(\(x) {
                out <- tibble::tibble(x$binned_variable)
                out$geometry <- sf::st_union(x)
                sf::st_as_sf(out, crs = 4326)[1, ]
              })
) |> sf::st_as_sf()
names(t)[1] <- "binned_variable"

aucun_diplome <-
  ggplot(data = t) +
  gg_cc_tiles +
  geom_sf(aes(fill = binned_variable), color = "transparent", lwd = 0) +
  scale_fill_manual(
    values = curbcut_colors$left_5$fill[2:6],
    name = "Aucun diplôme (%)",
    guide = guide_legend(
      title.position = "top",
      label.position = "bottom",
      keywidth = 1,
      keyheight = 0.5
    )
  ) +
  gg_cc_theme

library(patchwork)

education_uni_aucun_plot <- diplome_uni + patchwork::plot_spacer() + aucun_diplome +
  patchwork::plot_layout(widths = c(1, 0.01, 1))

ggplot2::ggsave(filename = here::here("output/axe1/education/education_uni_aucun_plot.pdf"), 
                plot = education_uni_aucun_plot, width = 7.5, height = 3)

# Séparer par secteurs
educ_DA_21 <- get_census(dataset = "CA21",
                         regions = list(CSD = 2465005),
                         level = "DA",
                         vectors = hs_21v,
                         geo_format = "sf")
educ_CSD_21 <- get_census(dataset = "CA21",
                          regions = list(CSD = 2465005),
                          level = "CSD",
                          vectors = hs_21v,
                          geo_format = "sf")

educ_PR_21 <- get_census(dataset = "CA21",
                          regions = list(PR = 24),
                          level = "PR",
                          vectors = hs_21v,
                          geo_format = "sf")


educ_DA_21$highschool <- educ_DA_21$highschool + educ_DA_21$postsecondary
educ_CSD_21$highschool <- educ_CSD_21$highschool + educ_CSD_21$postsecondary
educ_PR_21$highschool <- educ_PR_21$highschool + educ_PR_21$postsecondary

# Interpolate DAs to sectors
educ_sectors <- cc.buildr::interpolate_from_area(to = laval_sectors, from = educ_DA_21, 
                                                 additive_vars = names(hs_21v),
                                                 crs = 32618)

educ_sectors$no_diploma_pct <- educ_sectors$no_diploma / educ_sectors$total
educ_sectors$highschool_pct <- educ_sectors$highschool / educ_sectors$total
educ_sectors$postsecondary_pct <- educ_sectors$postsecondary / educ_sectors$total
educ_sectors$bachelor_higher_pct <- educ_sectors$bachelor_higher / educ_sectors$total
educ_CSD_21$no_diploma_pct <- educ_CSD_21$no_diploma / educ_CSD_21$total
educ_CSD_21$highschool_pct <- educ_CSD_21$highschool / educ_CSD_21$total
educ_CSD_21$postsecondary_pct <- educ_CSD_21$postsecondary / educ_CSD_21$total
educ_CSD_21$bachelor_higher_pct <- educ_CSD_21$bachelor_higher / educ_CSD_21$total
educ_PR_21$no_diploma_pct <- educ_PR_21$no_diploma / educ_PR_21$total
educ_PR_21$highschool_pct <- educ_PR_21$highschool / educ_PR_21$total
educ_PR_21$postsecondary_pct <- educ_PR_21$postsecondary / educ_PR_21$total
educ_PR_21$bachelor_higher_pct <- educ_PR_21$bachelor_higher / educ_PR_21$total

educ_sectors <- educ_sectors[c("name", "no_diploma", "no_diploma_pct", 
                               "highschool", "highschool_pct", "postsecondary",
                               "postsecondary_pct", "bachelor_higher", 
                               "bachelor_higher_pct")]
educ_CSD_21 <- educ_CSD_21[c("name", "no_diploma", "no_diploma_pct", 
                             "highschool", "highschool_pct", "postsecondary",
                             "postsecondary_pct", "bachelor_higher", 
                             "bachelor_higher_pct")]
educ_PR_21 <- educ_PR_21[c("name", "no_diploma", "no_diploma_pct", 
                             "highschool", "highschool_pct", "postsecondary",
                             "postsecondary_pct", "bachelor_higher", 
                             "bachelor_higher_pct")]
educ_sectors <- rbind(educ_PR_21, educ_CSD_21, educ_sectors)

educ_sectors <- sf::st_drop_geometry(educ_sectors)
names(educ_sectors) <- c("Zone géographique", 
                         "Aucun diplôme (n)", 
                         "Aucun diplôme (%)", 
                         "DES ou équivalent (n)", 
                         "DES ou équivalent (%)", 
                         "DPS ou équivalent (n)", 
                         "DPS ou équivalent (%)", 
                         "Diplôme uni. (n)", 
                         "Diplôme uni. (%)")
educ_sectors$`Aucun diplôme (n)` <- convert_number(educ_sectors$`Aucun diplôme (n)`)
educ_sectors$`DES ou équivalent (n)` <- convert_number(educ_sectors$`DES ou équivalent (n)`)
educ_sectors$`DPS ou équivalent (n)` <- convert_number(educ_sectors$`DPS ou équivalent (n)`)
educ_sectors$`Diplôme uni. (n)` <- convert_number(educ_sectors$`Diplôme uni. (n)`)

educ_sectors$`Zone géographique`[1] <- "Ensemble du Québec"
educ_sectors$`Zone géographique`[2] <- "Ville de Laval"

educ_sectors_table <- 
  gt(educ_sectors) |> 
  data_color(
    columns = ends_with("%)"),
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = NULL
    )
  ) |> 
  fmt(
    columns = ends_with("%)"),
    fns = convert_pct
  ) |>
  # Adding the row group for "Secteur"
  tab_row_group(
    label = "Secteur",
    rows = 3:nrow(educ_sectors)
  ) |>
  # Adding the row group for "Région"
  tab_row_group(
    label = "Région",
    rows = 1:2
  ) |>
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = 2
    )
  ) |>
  tab_style(
    style = cell_borders(
      sides = c("top"),
      color = "white",
      weight = px(10)
    ),
    locations = cells_row_groups()
  ) |>
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
  tab_options(
    table.font.size = indesign_fontsize,
    row_group.font.size = indesign_fontsize
    
  )

gtsave(educ_sectors_table, "output/axe1/education/educ_sectors_table.png")

# Composition 2006-2021 ---------------------------------------------------

comp21v <- c("total" = "v_CA21_5865", "none" = "v_CA21_5868", "sec" = "v_CA21_5871",
             "psec" = "v_CA21_5877", "uni" = "v_CA21_5895")
comp16v <- c("total" = "v_CA16_5096", "none" = "v_CA16_5099", "sec" = "v_CA16_5102",
             "psec" = "v_CA16_5105", "uni" = "v_CA16_5123")
comp11v <- c("total" = "v_CA11N_1801", "none" = "v_CA11N_1804", "sec" = "v_CA11N_1807",
             "psec" = "v_CA11N_1810", "uni" = "v_CA11N_1822")
comp06v <- c("total" = "v_CA06_1248", "none" = "v_CA06_1249", "sec" = "v_CA06_1251",
             "cert" = "v_CA06_1250", "uni" = "v_CA06_1256")

comp21 <- get_census(dataset = "CA21",
                     regions = list(CSD = (2465005)),
                     level = "CSD",
                     vectors = comp21v) |> 
  mutate(Year = 2021) |> 
  select(Year, total, none, sec, psec, uni)

comp16 <- get_census(dataset = "CA16",
                     regions = list(CSD = (2465005)),
                     level = "CSD",
                     vectors = comp16v) |> 
  mutate(psec = psec - uni) |> 
  mutate(Year = 2016) |> 
  select(Year, total, none, sec, psec, uni)

comp11 <- get_census(dataset = "CA11",
                     regions = list(CSD = (2465005)),
                     level = "CSD",
                     vectors = comp11v) |> 
  mutate(psec = psec - uni) |> 
  mutate(Year = 2011) |> 
  select(Year, total, none, sec, psec, uni)

comp06 <- get_census(dataset = "CA06",
                     regions = list(CSD = (2465005)),
                     level = "CSD",
                     vectors = comp06v) |> 
  mutate(psec = cert - sec - uni) |> 
  mutate(Year = 2006) |> 
  select(Year, total, none, sec, psec, uni)

comp <- bind_rows(comp06, comp11, comp16, comp21) |> 
  mutate(Year = as.factor(Year),
         none = none / total,
         sec = sec / total,
         psec = psec / total,
         uni = uni / total) |> 
  select(-total) |> 
  pivot_longer(cols = -Year, names_to = "level", values_to = "prop") |> 
  mutate(prop_per = convert_pct(x = prop)) |> 
  mutate(level = factor(level, levels = c("uni", "psec", "sec", "none"))) |> 
  group_by(Year) |> 
  mutate(cumulative = cumsum(prop) - 0.5 * prop) |> 
  ungroup()

edu_comp_graph <-
  ggplot(comp, aes(x = Year, y = prop, fill = level)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = prop_per, y = cumulative), color = "white",
            size = ggplot_fontsize) +
  scale_y_continuous(labels = convert_pct) +
  labs(x = element_blank(), y = "Proportion de la population \nâgée de 25 à 64 ans (%)") +
  scale_fill_manual(values = c("none" = "#98A8CB", "sec" = "#6C83B5",
                               "psec" = "#3d4a66", "uni" = "#252c3d"),
                    labels = c("none" = "Aucun certificat, diplôme ou grade",
                               "sec" = "Diplôme d'études secondaires ou\nattestation d'équivalence",
                               "psec" = "Certificat ou diplôme d’études\npostsecondaires inférieur au baccalauréat",
                               "uni" = "Baccalauréat ou grade supérieur")) +
  gg_cc_theme_no_sf +
  theme(legend.title = element_blank()) +
  guides(fill = guide_legend(ncol = 2))

ggplot2::ggsave(filename = here::here("output/axe1/education/edu_comp_graph.pdf"),
                plot = edu_comp_graph, width = 7.5, height = 3)

# Par genre ---------------------------------------------------------------

edus_v <- c(m_total = "v_CA21_5866", f_total = "v_CA21_5867",
            "m_none" = "v_CA21_5869", "f_none" = "v_CA21_5870",
            "m_sec" = "v_CA21_5872", "f_sec" = "v_CA21_5873",
            "m_psec" = "v_CA21_5878", "f_psec" = "v_CA21_5879",
            "m_uni" = "v_CA21_5896", "f_uni" = "v_CA21_5897")

edus <- get_census(dataset = "CA21",
                   regions = list(CSD = (2465005)),
                   level = "CSD",
                   vectors = edus_v) |> 
  mutate(m_none = m_none / m_total, f_none = f_none / f_total,
         m_sec = m_sec / m_total, f_sec = f_sec / f_total,
         m_psec = m_psec / m_total, f_psec = f_psec / f_total,
         m_uni = m_uni / m_total, f_uni = f_uni / f_total) |> 
  select(m_none, f_none, m_sec, f_sec, m_psec, f_psec, m_uni, f_uni) |> 
  pivot_longer(everything(), names_to = "level", values_to = "prop") |> 
  mutate(education = case_when(str_ends(level, "none") ~ "none",
                               str_ends(level, "_sec") ~ "sec",
                               str_ends(level, "psec") ~ "psec",
                               str_ends(level, "uni") ~ "uni",
                               TRUE ~ "Other"),
         sex = case_when(str_starts(level, "m") ~ "Hommes",
                         str_starts(level, "f") ~ "Femmes",
                         TRUE ~ "Total"),
         percent = convert_pct(x = prop)) |> 
  select(-level) |> 
  mutate(education = factor(education, levels = c("none", "sec", "psec", "uni")))

edu_gender_graph <- ggplot(edus, aes(x = education, y = prop, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = percent), vjust = 2, position = position_dodge(width = 0.9),
            color = "white") +
  scale_y_continuous(labels = convert_pct) +
  scale_fill_manual(values = c("Femmes" = "#CD718C", "Hommes" = "#A3B0D1")) +
  scale_x_discrete(labels = c("Aucun certificat, diplôme\nou grade",
                              "Diplôme d'études secondaires\nou attestation d'équivalence",
                              "Certificat ou diplôme\nd’études postsecondaires\ninférieur au baccalauréat",
                              "Baccalauréat ou grade supérieur")) +
  gg_cc_theme_no_sf +
  ylab("Proportion, par genre, de \nla population de 25 à 64 ans (%)")+
  xlab(NULL) +
  theme(legend.title = element_blank())

ggplot2::ggsave(filename = here::here("output/axe1/education/edu_gender_graph.pdf"), 
                plot = edu_gender_graph, width = 7.5, height = 4)


# Save --------------------------------------------------------------------


qs::qsavem(education_indice_plot, education_eleves_var, education_eleves_var_qc,
           no_diploma_pct_2006_laval,
           no_diploma_pct_2021_laval, bachelor_higher_pct_2006_laval,
           bachelor_higher_pct_2021_laval, no_diploma_pct_2006_qc,
           no_diploma_pct_2021_qc, bachelor_higher_pct_2006_qc,
           bachelor_higher_pct_2021_qc, education_uni_aucun_plot,
           educ_sectors_table, edu_comp_graph, edu_gender_graph, dropout_plot,
           file = "data/axe1/education.qsm")
