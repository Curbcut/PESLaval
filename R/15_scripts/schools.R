#Loading up libraries
source("R/01_startup.R")

CT <- cancensus::get_census(dataset = "CA21", 
                            regions = list(CSD = 2465005), 
                            level = "CT",
                            geo_format = "sf")
DB <- cancensus::get_census(dataset = "CA21", 
                            regions = list(CSD = 2465005), 
                            level = "DB",
                            geo_format = "sf")

#Setting the Laval bound box for maps
laval_bbox <- st_bbox(CT)


# Importing and Cleaning Data for DBs ---------------------------------------------

ttm_walk_15 <- ttm()

# ttm_walk_15 <- qs::qread("data/axe3/CT_laval_foot_ttm.qs") |> 
#   enframe(name = "GeoUID", value = "to") |> 
#   unnest(to) |> 
#   rowwise() %>%
#   unite(time, 3:83, na.rm = TRUE, sep = ", ") |> 
#   mutate(time = as.numeric(time)) |> 
#   select(GeoUID, DA_ID, time) |> 
#   rename("to" = DA_ID) |> 
#   filter(time <= 900) |> 
#   transmute(from = GeoUID, to) |> 
#   # Add self
#   rbind(tibble::tibble(from = CT$GeoUID, to = CT$GeoUID)) |>
#   unique()

school_public <- read_sf("data/axe3/schools/PPS_Public_Ecole.shp") |> 
  filter(ADULTE != 1, FORM_PRO != 1, ORDRE_ENS != "Préscolaire") |> 
  sf::st_filter(laval_sectors) |> 
  select(OBJECTID, PRIM, SEC, TYPE_CS)
school_private <- read_sf("data/axe3/schools/PPS_Prive_Installation.shp") |> 
  filter(ADULTE != 1, FORM_PRO != 1, ORDRE_ENS != "Préscolaire") |> 
  sf::st_filter(laval_sectors) |> 
  transmute(OBJECTID, PRIM, SEC, TYPE_CS = "Franco")
school <- rbind(school_public, school_private)

## Primary school
primaire_sf <- school[school$PRIM == 1, ]
primaire <- sf::st_join(primaire_sf, DB["GeoUID"])
primaire <- sf::st_drop_geometry(primaire[c("OBJECTID", "GeoUID", "TYPE_CS")])
primaire <- left_join(primaire, ttm_walk_15, by = c("GeoUID" = "from"), 
                      relationship = "many-to-many")

primaire <- 
  primaire |> 
  group_by(to, TYPE_CS) |> 
  count() |> 
  ungroup() |> 
  transmute(GeoUID = to, lang = TYPE_CS)

bilingual <- table(primaire$GeoUID)
bilingual <- names(bilingual)[bilingual == 2]

primare_access <- sapply(DB$GeoUID, \(ID) {
  if (!ID %in% primaire$GeoUID) return("Aucun accès")
  if (ID %in% bilingual) return("Accès à au moins une\n école des deux langues")
  lang <- primaire$lang[primaire$GeoUID == ID]
  if (lang == "Franco") return("Accès à au moins une\n école francophone")
  if (lang == "Anglo") return("Accès à au moins une\n école anglophone")
})

primaire <- tibble::tibble(GeoUID = names(primare_access), 
                           access = unname(primare_access)) |> 
  left_join(DB["GeoUID"]) |> 
  sf::st_as_sf()

primaire_plot <- 
  ggplot(primaire) +
  gg_cc_tiles +
  geom_sf(aes(fill = access), color = "transparent") +
  scale_fill_manual(values = c("Aucun accès" = "#E8E8E8", 
                               "Accès à au moins une\n école francophone" = "#6C83B5",
                               "Accès à au moins une\n école anglophone" = "#73AE80",
                               "Accès à au moins une\n école des deux langues" = "#2A5A5B")) +
  geom_sf(data = primaire_sf, aes(color = TYPE_CS), size = 0.75) +
  scale_color_manual(values = c("Anglo" = "#F5D574", 
                                "Franco" = "#CD718C"),
                     labels = c("Anglo" = "École anglophone", 
                                "Franco" = "École francophone")) +
  gg_cc_theme +
  theme(legend.title = element_blank(),
        legend.spacing = unit(0.75, "cm"),
        legend.box.spacing = unit(0.75, "cm"))+
  guides(fill = guide_legend(ncol = 2),
         color = guide_legend(ncol = 1, override.aes = list(size = 3)))


ggplot2::ggsave(filename = here::here("output/axe3/primaire_plot.pdf"), 
                plot = primaire_plot, width = 7, height = 5, bg = "transparent")

# Combien d'enfants plutôt que combien de Toutes les familles
DA_children <- cancensus::get_census(dataset = "CA21", 
                                     regions = list(CSD = 2465005), 
                                     level = "DA",
                                     vectors = c("v_CA21_38", "v_CA21_41", "v_CA21_44",
                                                 "v_CA21_47", "v_CA21_53", "v_CA21_56",
                                                 "v_CA21_59"),
                                     geo_format = "sf")
DA_children$children <- {DA_children$`v_CA21_38: 6` +  DA_children$`v_CA21_41: 7` + 
    DA_children$`v_CA21_44: 8` + DA_children$`v_CA21_47: 9` + DA_children$`v_CA21_53: 10` + 
    DA_children$`v_CA21_56: 11` + DA_children$`v_CA21_59: 12`}
DA_children <- DA_children[c("GeoUID", "Population", "children")]
names(DA_children)[names(DA_children) == "Population"] <- "Pop_DA"

DB_pop <- left_join(DB[c("GeoUID", "DA_UID", "Population")], 
                    sf::st_drop_geometry(DA_children), by = c("DA_UID" = "GeoUID"))
DB_pop <- sf::st_drop_geometry(DB_pop)
DB_pop$pop_ratio <- DB_pop$Population / DB_pop$Pop_DA
DB_pop$children <- as.numeric(DB_pop$children) * DB_pop$pop_ratio


DB_pop <- left_join(DB_pop, sf::st_drop_geometry(primaire), by = "GeoUID")
children_with_access <- sum(DB_pop$children[DB_pop$access != "Aucun accès"], na.rm = TRUE)
children_with_access_pct <- convert_pct(children_with_access / sum(DB_pop$children, na.rm = TRUE))
children_with_access_fr <- sum(DB_pop$children[
  DB_pop$access %in% c("Accès à au moins une\n école francophone", 
                       "Accès à au moins une\n école des deux langues")], 
  na.rm = TRUE)
children_with_access_fr_pct <- convert_pct(children_with_access_fr / sum(DB_pop$children, na.rm = TRUE))
children_with_access_en <- sum(DB_pop$children[
  DB_pop$access %in% c("Accès à au moins une\n école anglophone", 
                       "Accès à au moins une\n école des deux langues")], 
  na.rm = TRUE)
children_with_access_en_pct <- convert_pct(children_with_access_en / sum(DB_pop$children, na.rm = TRUE))


## Secondary school
secondaire_sf <- school[school$SEC == 1, ]
secondaire <- sf::st_join(secondaire_sf, DB["GeoUID"])
secondaire <- sf::st_drop_geometry(secondaire[c("OBJECTID", "GeoUID", "TYPE_CS")])
secondaire <- left_join(secondaire, ttm_walk_15, by = c("GeoUID" = "from"), 
                      relationship = "many-to-many")

secondaire <- 
  secondaire |> 
  group_by(to, TYPE_CS) |> 
  count() |> 
  ungroup() |> 
  transmute(GeoUID = to, lang = TYPE_CS)

bilingual <- table(secondaire$GeoUID)
bilingual <- names(bilingual)[bilingual == 2]

secondary_access <- sapply(DB$GeoUID, \(ID) {
  if (!ID %in% secondaire$GeoUID) return("Aucun accès")
  if (ID %in% bilingual) return("Accès à au moins une\n école des deux langues")
  lang <- secondaire$lang[secondaire$GeoUID == ID]
  if (lang == "Franco") return("Accès à au moins une\n école francophone")
  if (lang == "Anglo") return("Accès à au moins une\n école anglophone")
})

secondaire <- tibble::tibble(GeoUID = names(secondary_access), 
                           access = unname(secondary_access)) |> 
  left_join(DB["GeoUID"]) |> 
  sf::st_as_sf()

secondaire_plot <- 
  ggplot(secondaire) +
  gg_cc_tiles +
  geom_sf(aes(fill = access), color = "transparent") +
  scale_fill_manual(values = c("Aucun accès" = "#E8E8E8", 
                               "Accès à au moins une\n école francophone" = "#6C83B5",
                               "Accès à au moins une\n école anglophone" = "#73AE80",
                               "Accès à au moins une\n école des deux langues" = "#2A5A5B")) +
  geom_sf(data = secondaire_sf, aes(color = TYPE_CS), size = 0.75) +
  scale_color_manual(values = c("Anglo" = "#F5D574", 
                                "Franco" = "#CD718C"),
                     labels = c("Anglo" = "École anglophone", 
                                "Franco" = "École francophone")) +
  gg_cc_theme +
  theme(legend.title = element_blank(),
        legend.spacing = unit(0.75, "cm"),
        legend.box.spacing = unit(0.75, "cm"))+
  guides(fill = guide_legend(ncol = 2),
         color = guide_legend(ncol = 1, override.aes = list(size = 3)))


ggplot2::ggsave(filename = here::here("output/axe3/secondaire_plot.pdf"), 
                plot = secondaire_plot, width = 7, height = 5, bg = "transparent")

# Combien d'enfants plutôt que combien de Toutes les familles
DA_children <- cancensus::get_census(dataset = "CA21", 
                                     regions = list(CSD = 2465005), 
                                     level = "DA",
                                     vectors = c("v_CA21_59", "v_CA21_62", "v_CA21_65", 
                                                 "v_CA21_74", "v_CA21_77", "v_CA21_80"),
                                     geo_format = "sf")
DA_children$children <- {DA_children$`v_CA21_59: 12` + DA_children$`v_CA21_62: 13` +
  DA_children$`v_CA21_65: 14` + DA_children$`v_CA21_74: 15` +
  DA_children$`v_CA21_77: 16` + DA_children$`v_CA21_80: 17`}
DA_children <- DA_children[c("GeoUID", "Population", "children")]
names(DA_children)[names(DA_children) == "Population"] <- "Pop_DA"

DB_pop <- left_join(DB[c("GeoUID", "DA_UID", "Population")], 
                    sf::st_drop_geometry(DA_children), by = c("DA_UID" = "GeoUID"))
DB_pop <- sf::st_drop_geometry(DB_pop)
DB_pop$pop_ratio <- DB_pop$Population / DB_pop$Pop_DA
DB_pop$children <- as.numeric(DB_pop$children) * DB_pop$pop_ratio


DB_pop <- left_join(DB_pop, sf::st_drop_geometry(secondaire), by = "GeoUID")
children_with_access_sec <- sum(DB_pop$children[DB_pop$access != "Aucun accès"], na.rm = TRUE)
children_with_access_sec_pct <- convert_pct(children_with_access_sec / sum(DB_pop$children, na.rm = TRUE))
children_with_access_sec_fr <- sum(DB_pop$children[
  DB_pop$access %in% c("Accès à au moins une\n école francophone", 
                       "Accès à au moins une\n école des deux langues")], 
  na.rm = TRUE)
children_with_access_sec_fr_pct <- convert_pct(children_with_access_sec_fr / sum(DB_pop$children, na.rm = TRUE))
children_with_access_sec_en <- sum(DB_pop$children[
  DB_pop$access %in% c("Accès à au moins une\n école anglophone", 
                       "Accès à au moins une\n école des deux langues")], 
  na.rm = TRUE)
children_with_access_sec_en_pct <- convert_pct(children_with_access_sec_en / sum(DB_pop$children, na.rm = TRUE))


# Demo CT numbers ---------------------------------------------------------

table5 <- read.csv("data/axe3/table5.csv") |> 
  rename("notimm_low_child" = "With.children",
         "imm_low_child" = "With.children.1",
         "notimm_notlow_child" = "With.children.2",
         "imm_notlow_child" = "With.children.3",
         "GeoUID" = "X") |> 
  slice(-1:-5)

table6 <- read.csv("data/axe3/table6.csv") |> 
  rename("notimm_low_child" = "With.children",
         "imm_low_child" = "With.children.1",
         "notimm_notlow_child" = "With.children.2",
         "imm_notlow_child" = "With.children.3",
         "GeoUID" = "X") |> 
  slice(-1:-2) |> 
  mutate(GeoUID = substr(GeoUID, 1, 10))

table7 <- read.csv("data/axe3/table7.csv") |> 
  rename("notimm_low_child" = "With.children",
         "imm_low_child" = "With.children.1",
         "notimm_notlow_child" = "With.children.2",
         "imm_notlow_child" = "With.children.3",
         "GeoUID" = "X") |> 
  slice(-1:-2) |> 
  mutate(GeoUID = substr(GeoUID, 1, 10))

table8a <- read.csv("data/axe3/table8a.csv") |> 
  rename("notimm_low_child" = "With.children",
         "imm_low_child" = "With.children.1",
         "notimm_notlow_child" = "With.children.2",
         "imm_notlow_child" = "With.children.3",
         "GeoUID" = "X") |> 
  slice(-1:-2) |> 
  mutate(GeoUID = substr(GeoUID, 1, 10))

table8b <- read.csv("data/axe3/table8b.csv") |> 
  rename("notimm_low_child" = "With.children",
         "imm_low_child" = "With.children.1",
         "notimm_notlow_child" = "With.children.2",
         "imm_notlow_child" = "With.children.3",
         "GeoUID" = "X") |> 
  slice(-1:-2) |> 
  slice(1:7) |> 
  mutate(GeoUID = substr(GeoUID, 1, 10))

lowincome_imm <- bind_rows(table5, table6, table7, table8a, table8b) |> 
  mutate_all(~ na_if(., "x"))

CT_demo <- right_join(lowincome_imm, CT[c("Households", "GeoUID")], by = "GeoUID") |> 
  sf::st_as_sf()
names(CT_demo)[names(CT_demo) == "Households"] <- "Hou_CT"

DB_hou <- left_join(DB[c("GeoUID", "CT_UID", "Households")], 
                    sf::st_drop_geometry(CT_demo), by = c("CT_UID" = "GeoUID"))
DB_hou <- sf::st_drop_geometry(DB_hou)
DB_hou$hou_ratio <- DB_hou$Households / DB_hou$Hou_CT
DB_hou$notimm_low_child <- as.numeric(DB_hou$notimm_low_child) * DB_hou$hou_ratio
DB_hou$imm_low_child <- as.numeric(DB_hou$imm_low_child) * DB_hou$hou_ratio
DB_hou$notimm_notlow_child <- as.numeric(DB_hou$notimm_notlow_child) * DB_hou$hou_ratio
DB_hou$imm_notlow_child <- as.numeric(DB_hou$imm_notlow_child) * DB_hou$hou_ratio


# Calculate accessibility to schools combined
schools_access <- sf::st_join(school, DB["GeoUID"])
schools_access <- sf::st_drop_geometry(schools_access[c("OBJECTID", "GeoUID", "PRIM", "SEC")])

# Quels DB peuvent se rendre à chaque école?
DB_with_primaire <- schools_access$GeoUID[schools_access$PRIM > 0]
DB_can_reach_primaire <- ttm_walk_15$to[ttm_walk_15$from %in% DB_with_primaire]
DB_can_reach_primaire <- unique(DB_can_reach_primaire)
DB_with_secondaire <- schools_access$GeoUID[schools_access$SEC > 0]
DB_can_reach_secondaire <- ttm_walk_15$to[ttm_walk_15$from %in% DB_with_secondaire]
DB_can_reach_secondaire <- unique(DB_can_reach_secondaire)

sum_households <- colSums(DB_hou[c("notimm_low_child", "imm_low_child", 
                                   "notimm_notlow_child", "imm_notlow_child")])



df_summarized <- DB_hou |>
  filter(GeoUID %in% DB_can_reach_primaire) |> 
  summarize(
    school = "Primaire",
    notimm_low_child = sum(notimm_low_child),
    ratio_notimm_low_child = notimm_low_child / sum_households[[1]],
    imm_low_child = sum(imm_low_child),
    ratio_imm_low_child = imm_low_child / sum_households[[2]],
    notimm_notlow_child = sum(notimm_notlow_child),
    ratio_notimm_notlow_child = notimm_notlow_child / sum_households[[3]],
    imm_notlow_child = sum(imm_notlow_child),
    ratio_imm_notlow_child = imm_notlow_child / sum_households[[4]],
    families = sum(notimm_low_child, imm_low_child, notimm_notlow_child, imm_notlow_child),
    ratio_families = families / sum(sum_households)
  ) |> 
  rbind({
    DB_hou |>
      filter(GeoUID %in% DB_can_reach_secondaire) |> 
      summarize(
        school = "Secondaire",
        notimm_low_child = sum(notimm_low_child),
        ratio_notimm_low_child = notimm_low_child / sum_households[[1]],
        imm_low_child = sum(imm_low_child),
        ratio_imm_low_child = imm_low_child / sum_households[[2]],
        notimm_notlow_child = sum(notimm_notlow_child),
        ratio_notimm_notlow_child = notimm_notlow_child / sum_households[[3]],
        imm_notlow_child = sum(imm_notlow_child),
        ratio_imm_notlow_child = imm_notlow_child / sum_households[[4]],
        families = sum(notimm_low_child, imm_low_child, notimm_notlow_child, imm_notlow_child),
        ratio_families = families / sum(sum_households)
      )
  }) |> 
  rename("Familles non-immigrantes, faible revenu (n)" = "notimm_low_child",
         "Familles non-immigrantes, faible revenu (%)" = "ratio_notimm_low_child",
         "Familles immigrantes, faible revenu (n)" = "imm_low_child",
         "Familles immigrantes, faible revenu (%)" = "ratio_imm_low_child",
         "Familles non-immigrantes (n)" = "notimm_notlow_child",
         "Familles non-immigrantes (%)" = "ratio_notimm_notlow_child",
         "Familles immigrantes (n)" = "imm_notlow_child",
         "Familles immigrantes (%)" = "ratio_imm_notlow_child",
         "Toutes les familles (n)" = "families",
         "Toutes les familles (%)" = "ratio_families") |>
  select("school", "Toutes les familles (n)", "Toutes les familles (%)",
         "Familles non-immigrantes (n)", "Familles non-immigrantes (%)",
         "Familles immigrantes (n)", "Familles immigrantes (%)",
         "Familles non-immigrantes, faible revenu (n)", "Familles non-immigrantes, faible revenu (%)",
         "Familles immigrantes, faible revenu (n)", "Familles immigrantes, faible revenu (%)")

df_wide <- df_summarized %>%
  pivot_wider(names_from = school, 
              values_from = c(
                `Toutes les familles (n)`, `Toutes les familles (%)`,
                `Familles non-immigrantes (n)`, `Familles non-immigrantes (%)`,
                `Familles immigrantes (n)`, `Familles immigrantes (%)`,
                `Familles non-immigrantes, faible revenu (n)`, `Familles non-immigrantes, faible revenu (%)`,
                `Familles immigrantes, faible revenu (n)`, `Familles immigrantes, faible revenu (%)`
              ))

df_long <- df_wide %>%
  pivot_longer(
    cols = starts_with("Familles") | starts_with("Toutes"),
    names_to = c("Categorie", ".value"),
    names_pattern = "(.*)_(.*)"
  )

school_table_data <- df_long[!grepl("%", df_long$Categorie), ]
school_table_data$Proportion_Primaire <- df_long[grepl("%", df_long$Categorie), ]$Primaire
school_table_data$Proportion_Secondaire <- df_long[grepl("%", df_long$Categorie), ]$Secondaire
names(school_table_data)[1:3] <- c(" ", "Familles_Primaire", "Familles_Secondaire")
school_table_data$` ` <- gsub(" \\(n\\)", "", school_table_data$` `)
school_table_data <- school_table_data[c(5,2,1,4,3),]

school_table <- 
  gt(school_table_data) |> 
  cols_label(
    ` ` = " ",
    `Familles_Primaire` = "Familles",
    `Familles_Secondaire` = "Familles",
    `Proportion_Primaire` = "Proportion",
    `Proportion_Secondaire` = "Proportion"
  ) |> 
  tab_spanner(
    label = "Primaire",
    columns = c(`Familles_Primaire`, `Proportion_Primaire`)
  ) |> 
  tab_spanner(
    label = "Secondaire",
    columns = c(`Familles_Secondaire`, `Proportion_Secondaire`)
  ) |> 
  fmt(columns = c(2,3), fns = convert_number_tens) |> 
  fmt(columns = c(4,5), fns = convert_pct) |> 
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
  # Options générales pour la table
  tab_options(
    table.font.size = 12,
    row_group.font.size = 12,
    table.width = px(6 * 96)
  )

gtsave(school_table, "output/axe3/school_table.png", zoom = 1)



qs::qsavem(primary_school_total, secondary_school_total,primary_franco, secondary_franco,
           children_with_access_pct, children_with_access_fr_pct, children_with_access_en_pct,
           children_with_access_sec_pct, children_with_access_sec_fr_pct, 
           children_with_access_sec_en_pct, primaire_plot, secondaire_plot, school_table,
           file = "data/axe3/schools.qsm")
