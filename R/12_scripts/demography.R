### DEMOGRAPHY - POPULATION AND POPULATION DENSITY #############################

source("R/01_startup.R")

#Importing data from Laval and Beyond2020
en_2270_25 <- read.csv("data/new/2270_25_en.csv", fileEncoding = "latin1")
en_2270_100 <- read.csv("data/new/2270_100_en.csv", fileEncoding = "latin1")

#importing data from estimates and projections(use the library readxl) https://publications.msss.gouv.qc.ca/msss/document-001617/
pop <- read_excel("data/new/estimate.xlsx", sheet = "Groupe d'âge") |> 
  slice(-1:-3) |> 
  row_to_names(row_number = 1)

#importing data from the isq rmr https://statistique.quebec.ca/fr/document/projections-de-population-regions-administratives-et-regions-metropolitaines-rmr
rmr_laval <- read_excel("data/new/PopAS_28P_base_2024.xlsx", sheet = 1, skip = 3) |> 
  row_to_names(row_number = 1) |> 
  clean_names() |> 
  filter(na_3 == "Laval (dans RMRM)" & na == "Référence A2024") |> 
  select(-1:-3)

cma_mtl <- read_excel("data/new/PopAS_RMR_base_2024.xlsx", sheet = 1, skip = 3) |> 
  row_to_names(row_number = 1) |> 
  clean_names() |> 
  filter(na_3 == "RMR de Montréal" & na == "Référence A2024") |> 
  select(-1:-3)

# Population count --------------------------------------------------------

# To get raw census data, we use the cancensus package. At this link, there is 
# documentation on how to get and save an API key: https://mountainmath.github.io/cancensus/

# We grab census information from Laval. This small call lets us retrieve 
# population counts for the whole city.
laval_census <- cancensus::get_census(dataset = "CA21", 
                                      regions = list(CSD = 2465005), 
                                      level = "CSD",
                                      geo_format = "sf")

# convert_number lets us convert any type and number to 'Pretty' number. 
# To use curbcut functions, install the package through `devtools::install_github("Curbcut/curbcut")`
laval_population_census <- laval_census$Population
laval_population_census_pretty <- convert_number(x = laval_population_census)

#laval_population_ISQ <- 454990
#laval_population_ISQ_pretty <- convert_number(x = laval_population_ISQ)

laval_population_2024 <- pop |> 
  filter(`Code du territoire` == "13") |> 
  filter(`Sexe` == "Total") |> 
  filter(Année == 2024) |> 
  pull(`Tous les âges`) |> 
  as.integer()
laval_population_2024_pretty <- convert_number(x = laval_population_2024)

laval_population_2025 <- pop |> 
  filter(`Code du territoire` == "13") |> 
  filter(`Sexe` == "Total") |> 
  filter(Année == 2025) |> 
  pull(`Tous les âges`) |> 
  as.integer()
laval_population_2025_pretty <- convert_number(x = laval_population_2025)


laval_size <- (cc.buildr::get_area(laval_census) / 1e6)
laval_size_pretty <- convert_number(x = laval_size)

density <- laval_population_2025 / laval_size
density_pretty <- convert_number(x = density)


# Population density graph ------------------------------------------------

CT <- cancensus::get_census(dataset = "CA21", 
                            regions = list(CSD = 2465005), 
                            level = "DA", 
                            geo_format = "sf")

# Prepare a function that calculates population density. We will use it many
# times in this chapter.
pop_density_fun <- function(x, col_name = "pop_density") {
  # Calculate the area, in meters, of every feature
  x$area <- cc.buildr::get_area(x) / 1000000 # Converting square meters to square kilometers
  
  # Get population density
  x[[col_name]] <- x$Population / x$area
  
  # Return the df
  x
}

# Get population density
CT <- pop_density_fun(CT)

labels <- c("0 - 1 500", "1 500 - 3 000", "3 000 - 4 500", "4 500 - 6 000", 
            "+ 6 000")

t <- add_bins(df = CT,
              variable = "pop_density",
              breaks = c(0, 1500, 3000, 4500, 6000, Inf),
              labels = labels
)

# # Union the features so the polygons don't show their borders. Might revisit
# # with the addition of streets!
# t <- Reduce(rbind,
#             split(t, t$binned_variable) |>
#               lapply(\(x) {
#                 out <- tibble::tibble(x$binned_variable)
#                 out$geometry <- sf::st_union(x)
#                 sf::st_as_sf(out, crs = 4326)[1, ]
#               })
# ) |> sf::st_as_sf()
# names(t)[1] <- "binned_variable"

# Make a plot of density
pop_density_plot <-
  ggplot(data = t) +
  gg_cc_tiles +
  geom_sf(aes(fill = binned_variable), color = "transparent", lwd = 0) +
  scale_fill_manual(
    values = curbcut_colors$left_5$fill[2:6],
    name = element_blank(),
    breaks = labels,
    labels = labels,
    guide = guide_legend(
      title.position = "top",
      label.position = "bottom",
      keywidth = 4,
      keyheight = 0.5
    )
  ) +
  gg_cc_theme


# Save the plot so it's in a good quality
ggplot2::ggsave(filename = here::here("output/0_demography/CT_pop_density.pdf"), 
                plot = pop_density_plot, width = 6.5, height = 5)


# Population Distribution by Age and Gender------------------------------------

### Age values for text
# laval_age_moyen <- get_census(dataset = "CA21", 
#                               regions = list(CSD = 2465005), 
#                               level = "CSD",
#                               vectors = c("age_moyen" = "v_CA21_386"))$age_moyen
# prov_age_moyen <- get_census(dataset = "CA21", 
#                              regions =  list(PR = 24), 
#                              level = "PR",
#                              vectors = c("age_moyen" = "v_CA21_386"))$age_moyen
# CMA_age_moyen <- get_census(dataset = "CA21", 
#                             regions = list(CMA = 24462), 
#                             level = "CMA",
#                             vectors = c("age_moyen" = "v_CA21_386"))$age_moyen
# age_moyen_pretty <- convert_number(laval_age_moyen)
# age_moyen_prov_pretty <- convert_number(prov_age_moyen)
# age_moyen_cma_pretty <- convert_number(CMA_age_moyen)
# 
# 
# laval_moins_18 <- get_census(dataset = "CA21", 
#                              regions = list(CSD = 2465005), 
#                              level = "CSD",
#                              vectors = c("v_CA21_11", 
#                                          "v_CA21_74", "v_CA21_77", "v_CA21_80", 
#                                          "v_CA21_83"))
# laval_moins_18 <- {laval_moins_18[11:15] |> unlist() |> sum()} / laval_moins_18$Population
# prov_moins_18 <- get_census(dataset = "CA21", 
#                             regions =  list(PR = 24), 
#                             level = "PR",
#                              vectors = c("v_CA21_11", 
#                                          "v_CA21_74", "v_CA21_77", "v_CA21_80", 
#                                          "v_CA21_83"))
# prov_moins_18 <- {prov_moins_18[9:13] |> unlist() |> sum()} / prov_moins_18$Population
# 
# moins_18_pretty <- convert_pct(laval_moins_18)
# moins_18_prov_pretty <- convert_pct(prov_moins_18)
# 
# laval_sixtyfive <- get_census(dataset = "CA21", 
#                               regions = list(CSD = 2465005), 
#                               level = "CSD",
#                               vectors = c("sixtyfive+" = "v_CA21_251"))
# prov_sixtyfive <- get_census(dataset = "CA21", 
#                              regions =  list(PR = 24), 
#                              level = "PR",
#                              vectors = c("sixtyfive+" = "v_CA21_251"))
# 
# sixtyfive_pretty <- convert_pct(laval_sixtyfive$`sixtyfive+` / laval_sixtyfive$Population)
# sixtyfive_prov_pretty <- convert_pct(prov_sixtyfive$`sixtyfive+` / prov_sixtyfive$Population)

#Average age
laval_age_moyen <- rmr_laval |> 
  filter(na_4 == 2025 & na_5 == 3) |> 
  pull(age_moyen) |> 
  as.double()
age_moyen_pretty <- convert_number(laval_age_moyen)
  

CMA_age_moyen <- cma_mtl |> 
  filter(na_4 == 2025 & na_5 == 3) |> 
  pull(age_moyen) |> 
  as.double()
age_moyen_cma_pretty <- convert_number(CMA_age_moyen)

prov_age_moyen <- 42.8
age_moyen_prov_pretty <- convert_number(prov_age_moyen)

#Under 18
laval_moins_18 <- pop |> 
  filter(`Code du territoire` == "13") |> 
  filter(`Sexe` == "Total") |> 
  filter(Année == 2025) |> 
  mutate(moin_18 = as.integer(`0-17 ans`) / as.integer(`Tous les âges`)) |> 
  pull(moin_18)

moins_18_pretty <- laval_moins_18 |> 
  convert_pct()

moins_18_prov <- pop |> 
  filter(`Code du territoire` == "99") |> 
  filter(`Sexe` == "Total") |> 
  filter(Année == 2025) |> 
  mutate(moin_18 = as.integer(`0-17 ans`) / as.integer(`Tous les âges`)) |> 
  pull(moin_18)

moins_18_prov_pretty <- convert_pct(moins_18_prov)

#over 65
laval_sixtyfive <- pop |> 
  filter(`Code du territoire` == "13") |> 
  filter(`Sexe` == "Total") |> 
  filter(Année == 2025) |> 
  mutate(plus_65 = as.integer(`65 ans ou plus`) / as.integer(`Tous les âges`)) |> 
  pull(plus_65)

sixtyfive_pretty <- convert_pct(laval_sixtyfive)

prov_sixtyfive <- pop |> 
  filter(`Code du territoire` == "99") |> 
  filter(`Sexe` == "Total") |> 
  filter(Année == 2025) |> 
  mutate(plus_65 = as.integer(`65 ans ou plus`) / as.integer(`Tous les âges`)) |> 
  pull(plus_65)
sixtyfive_prov_pretty <- convert_pct(prov_sixtyfive)

#Adult distribution
laval_adult <- pop |> 
  filter(`Code du territoire` == "13") |> 
  filter(`Sexe` == "Total") |> 
  filter(Année == 2025) |> 
  mutate(young = as.integer(`18-44 ans`) / as.integer(`Tous les âges`),
         old = as.integer(`45-64 ans`) / as.integer(`Tous les âges`))

laval_young_pretty <- laval_adult |> 
  pull(young) |> 
  convert_pct()

laval_old_pretty <- laval_adult |> 
  pull(old) |> 
  convert_pct()

qc_adult <- pop |> 
  filter(`Code du territoire` == "99") |> 
  filter(`Sexe` == "Total") |> 
  filter(Année == 2025) |> 
  mutate(young = as.integer(`18-44 ans`) / as.integer(`Tous les âges`),
         old = as.integer(`45-64 ans`) / as.integer(`Tous les âges`))

qc_young_pretty <- qc_adult |> 
  pull(young) |> 
  convert_pct()

qc_old_pretty <- qc_adult |> 
  pull(old) |> 
  convert_pct()

# Get all the age/gender variables for the 2021 census
pop_dist_vector <- c(
  "Homme 0-4" = "v_CA21_15", "Femme 0-4" = "v_CA21_16", 
  "Homme 5-9" = "v_CA21_33", "Femme 5-9" = "v_CA21_34", 
  "Homme 10-14" = "v_CA21_51", "Femme 10-14" = "v_CA21_52", 
  "Homme 15-19" = "v_CA21_72", "Femme 15-19" = "v_CA21_73", 
  "Homme 20-24" = "v_CA21_90", "Femme 20-24" = "v_CA21_91", 
  "Homme 25-29" = "v_CA21_108", "Femme 25-29" = "v_CA21_109", 
  "Homme 30-34" = "v_CA21_126", "Femme 30-34" = "v_CA21_127", 
  "Homme 35-39" = "v_CA21_144", "Femme 35-39" = "v_CA21_145", 
  "Homme 40-44" = "v_CA21_162", "Femme 40-44" = "v_CA21_163",
  "Homme 45-49" = "v_CA21_180", "Femme 45-49" = "v_CA21_181", 
  "Homme 50-54" = "v_CA21_198", "Femme 50-54" = "v_CA21_199", 
  "Homme 55-59" = "v_CA21_216", "Femme 55-59" = "v_CA21_217", 
  "Homme 60-64" = "v_CA21_234", "Femme 60-64" = "v_CA21_235", 
  "Homme 65-69" = "v_CA21_255", "Femme 65-69" = "v_CA21_256", 
  "Homme 70-74" = "v_CA21_273", "Femme 70-74" = "v_CA21_274", 
  "Homme 75-79" = "v_CA21_291", "Femme 75-79" = "v_CA21_292", 
  "Homme 80-84" = "v_CA21_309", "Femme 80-84" = "v_CA21_310", 
  "Homme 85+" = "v_CA21_327", "Femme 85+" = "v_CA21_328")

# Vector for sorting
sort_vec <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", 
              "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", 
              "70-74", "75-79", "80-84", "85+")

# Get these variables for Laval
# pop_dist_laval  <- get_census(dataset = "CA21", 
#                               regions = list(CSD = 2465005), 
#                               level = "CSD",
#                               vectors = pop_dist_vector)
# 
# # Get the variables for the province
# pop_dist_qc  <- get_census(dataset = "CA21", 
#                            regions = list(PR = 24), 
#                            level = "PR",
#                            vectors = pop_dist_vector)

pop_dist_one <- pop |> 
  filter(`Code du territoire` %in% c("13", "99") & `Année` == 2025 & Sexe != "Total") |> 
  select(-Statut, -`Niveau géographique`, -`Type de données`, -`Tous les âges`, -Territoire, -Année, -`90 ans ou plus`,
         -`75 ans ou plus`, -`0-17 ans`, -`85-89 ans`, -`18-64 ans`, -`20-64 ans`, -`6-11 ans`, -`12-17 ans`,
         -`65 ans ou plus`, -`0-19 ans`, -`18-44 ans`, -`45-64 ans`, -`65-74 ans`, -`75-84 ans`, -`0-5 ans`) |> 
  mutate(across(-Sexe, ~ as.integer(gsub("[^0-9]", "", .)))) |> 
  mutate("0-4" = `1-4 ans` + `Moins un an`,
         Sexe = recode(Sexe, "Masculin" = "Hommes", "Féminin" = "Femmes")) |> 
  rename("5-9" = `5-9 ans`, "10-14" = `10-14 ans`, "15-19" = `15-19 ans`, "20-24" = `20-24 ans`,
         "25-29" = `25-29 ans`, `30-34` = `30-34 ans`, `35-39` = `35-39 ans`, `40-44` = `40-44 ans`,
         `45-49` = `45-49 ans`, `50-54` = `50-54 ans`, `55-59` = `55-59 ans`, `60-64` = `60-64 ans`,
         `65-69` = `65-69 ans`, `70-74` = `70-74 ans`, `75-79` = `75-79 ans`, `80-84` = `80-84 ans`, 
         `85+` = `85 ans ou plus`) |> 
  select(`Code du territoire`, Sexe, `0-4`, `5-9`, `10-14`, `15-19`, `20-24`, `25-29`, `30-34`, `35-39`,
         `40-44`, `45-49`, `50-54`, `55-59`, `60-64`, `65-69`, `70-74`, `75-79`,
         `80-84`, `85+`)

pop_dist_laval <- pop_dist_one |> 
  filter(`Code du territoire` == 13) |> 
  select(-`Code du territoire`) |> 
  pivot_longer(-Sexe, names_to = "age_group", values_to = "value") |> 
  mutate(new_col = paste(Sexe, age_group)) |> 
  select(new_col, value) |> 
  pivot_wider(names_from = new_col, values_from = value) |> 
  mutate(name = "Laval")

pop_dist_qc <- pop_dist_one |> 
  filter(`Code du territoire` == 99) |> 
  select(-`Code du territoire`) |> 
  pivot_longer(-Sexe, names_to = "age_group", values_to = "value") |> 
  mutate(new_col = paste(Sexe, age_group)) |> 
  select(new_col, value) |> 
  pivot_wider(names_from = new_col, values_from = value) |> 
  mutate(name = "Ensemble du Québec")

library(ggpattern)

# Merge the two datasets
# pop_dist <-
#   bind_rows(pop_dist_laval, pop_dist_qc) |> 
#   #mutate(name = c("Laval", "Ensemble du Québec"), .before = GeoUID) |> 
#   #select(-c(GeoUID:CMA_UID, C_UID)) |> 
#   pivot_longer(-name, names_to = "category") |> 
#   # Split gender off into its own variable
#   mutate(gender = str_extract(category, "(Homme|Femme)"),
#          category = str_remove(category, "(Homme |Femme )")) |> 
#   group_by(name, gender) |> 
#   mutate(pct = value / sum(value)) |> 
#   ungroup() |> 
#   # Make Femme values negative to facilitate easier population pyramids
#   mutate(pct = if_else(gender == "Femme", pct * -1, pct)) |> 
#   mutate(category = factor(category, levels = sort_vec))
# 
# # Reorder the facet levels as needed
# pop_dist$name <- factor(pop_dist$name, levels = c("Laval", "Ensemble du Québec"))
# 
# age_pyramid <- pop_dist |> 
#   ggplot(aes(x = pct, y = category, fill = gender)) +
#   geom_col() +
#   facet_wrap(~name, nrow = 1) +
#   scale_fill_manual(name = element_blank(), values = c("Femme" = color_theme("pinkhealth"), "Homme" = color_theme("blueexplorer"))) +
#   scale_x_continuous(breaks = -2:2 * 0.04,
#                      labels = c("8 %", "4 %", "0", "4 %", "8 %")) +
#   gg_cc_theme_no_sf +
#   xlab(element_blank())+
#   ylab(element_blank()) +
#   theme(strip.text = element_text(size = 11),
#         legend.text = element_text(size = 9),
#         axis.text.x = element_text(size = 9),
#         axis.text.y = element_text(size = 9)) +
#   geom_text(aes(label = ifelse(abs(pct) > 0.06, "*", "")), 
#             size = 6)



# Adjust dataset
pop_dist <-
  bind_rows(pop_dist_laval, pop_dist_qc) |> 
  #mutate(name = c("Laval", "Ensemble du Québec"), .before = GeoUID) |> 
  #select(-c(GeoUID:CMA_UID, C_UID)) |> 
  pivot_longer(-name, names_to = "category") |> 
  mutate(gender = str_extract(category, "(Homme|Femme)"),
         category = str_remove(category, "(Hommes|Femmes)")) |> 
  group_by(name, gender) |> 
  mutate(pct = value / sum(value)) |> 
  ungroup() |> 
  mutate(pct = if_else(gender == "Femme", pct * -1, pct)) |> 
  mutate(category = str_trim(category),
         category = factor(category, levels = sort_vec))

# Age pyramid with tightly packed bars
age_pyramid <- 
pop_dist |> 
  ggplot(aes(x = pct, y = category, fill = name)) +
  geom_col(
    position = position_dodge(width = 1),  # Laval & Québec side by side for each gender
    width = 1                                # Max width to fully occupy the space
  ) +
  scale_fill_manual(
    name = "Population",
    values = c("Laval" = color_theme("greenecology"), "Ensemble du Québec" = color_theme("blueexplorer"))
  ) +
  scale_x_continuous(
    breaks = -2:2 * 0.04,
    labels = c("8 %", "4 %", "0", "4 %", "8 %")
  ) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1) +  # Add central black line
  annotate("text", x = -0.05, y = "85+", label = "Femmes", size = 4, 
           family="KMR-Apparat-Regular") +  # Label for Femmes
  annotate("text", x = 0.05, y = "85+", label = "Hommes", size = 4, 
           family="KMR-Apparat-Regular") +  # Label for Hommes
  gg_cc_theme_no_sf +
  xlab("Proportion de la population (%)") +
  ylab("Groupe d'âge") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    strip.text = element_text(size = 11),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    legend.title = element_blank()
  )


ggplot2::ggsave(filename = here::here("output/0_demography/age_pyramid.pdf"), 
                plot = age_pyramid, width = 9, height = 4.5)


# Répartition par genre ---------------------------------------------------

# h_f  <- get_census(dataset = "CA21", 
#                    regions = list(CSD = 2465005), 
#                    level = "CSD",
#                    vectors = c(homme = "v_CA21_9", femme = "v_CA21_10"))
# 
# homme_pct_pretty <- convert_pct(h_f$homme / (h_f$homme + h_f$femme))
# femme_pct_pretty <- convert_pct(h_f$femme / (h_f$femme + h_f$homme))

h_f <- pop |> 
  filter(`Code du territoire` == "13") |> 
  filter(Année == 2025) |> 
  select(Sexe, `Tous les âges`) |> 
  pivot_wider(names_from = Sexe, values_from = `Tous les âges`) |> 
  mutate(across(everything(), ~ as.integer(gsub("[^0-9]", "", .))))

homme_pct_pretty <- convert_pct(h_f$Masculin / (h_f$Total))
femme_pct_pretty <- convert_pct(h_f$Féminin / (h_f$Total))
  

#ethnic origin for the population ----------------------------------------------

ethnic_origins <- read.csv("data/demography/Ethnic Origin Total Pop V2.csv", skip = 12)[1:10, 1:2] |> 
  tibble::as_tibble()
names(ethnic_origins) <- c("response", "total")
ethnic_origins$total <- gsub(",", "", ethnic_origins$total) |> as.numeric()

ethnic_origins <- sapply(ethnic_origins$response[3:10], \(x) {
  val <- ethnic_origins$total[ethnic_origins$response == x] / 
    ethnic_origins$total[ethnic_origins$response == "Total - Ethnic or cultural origin 9"]
}, simplify = FALSE, USE.NAMES = TRUE)

ethnic_origins$`caraibe_latin` <- ethnic_origins$`Latin, Central and South American origins` + ethnic_origins$`Caribbean origins`
ethnic_origins$`other` <- ethnic_origins$`Other ethnic and cultural origins` + ethnic_origins$`Oceanian origins`

ethnic_origins <- lapply(ethnic_origins, convert_pct)
ethnic_origins$na <- ethnic_origins$`North American origins`
ethnic_origins$europe <- ethnic_origins$`European origins`
ethnic_origins$asian <- ethnic_origins$`Asian origins`
ethnic_origins$african <- ethnic_origins$`African origins`

# How many ethnic groups?
ethnic_groups <- cancensus::list_census_vectors("CA21")$vector[
  cancensus::list_census_vectors("CA21")$parent_vector == "v_CA21_4917"
]
vecs <- ethnic_groups[!is.na(ethnic_groups)]
ethnic_groups <- cancensus::get_census(dataset = "CA21", 
                                       regions = list(CSD = 2465005), 
                                       level = "CSD",
                                       vectors = vecs)
ethnic_groups <- ethnic_groups[11:ncol(ethnic_groups)]
ethnic_groups <- pivot_longer(ethnic_groups, cols = names(ethnic_groups))
groupe_ethniques_diff <- ethnic_groups[ethnic_groups$value != 0, ] |> nrow()


# Percent population change -----------------------------------------------

population_change <- cancensus::get_census(dataset = "CA21", 
                                           regions = list(CSD = 2465005), 
                                           level = "CSD",
                                           vectors = c("population_change" = "v_CA21_3"))
population_change <- population_change$population_change
population_change <- convert_pct(population_change / 100)

quebec_pop_change <- cancensus::get_census(dataset = "CA21", 
                                           regions = list(PR = 24), 
                                           level = "PR",
                                           vectors = c("population_change" = "v_CA21_3"))
quebec_pop_change <- quebec_pop_change$population_change
quebec_pop_change <- convert_pct(quebec_pop_change / 100)


# Population evolution ----------------------------------------------------

#insert data set from folder sourced from different census' years
# pop_evolution <- read.csv("data/demography/Population Evolution.csv", skip = 3) |> 
#   tibble::as_tibble()
#pop_evolution_tidy <- pop_evolution[1:16, ]
#names(pop_evolution_tidy)[1] <- "Année"

#Manually inputting data from https://web.archive.org/web/20131006173408/http://www.stat.gouv.qc.ca/donstat/societe/demographie/dons_regnl/regional/Tableau_top_10.htm
pre_2001 <- tibble::tibble(
  Année = c(1966, 1971, 1976, 1981, 1986, 1991, 1996),
  `Type de données` = c("Estimation", "Estimation", "Estimation", "Estimation", "Estimation", "Estimation", "Estimation"),
  `Tous les âges` = c("196088", "228010", "246243", "268335", "284164", "314398", "330393"),
  Population = c(196088, 228010, 246243, 268335, 284164, 314398, 330393))

pop_evolution_1 <- pop |> 
  filter(`Code du territoire` == 13 & Sexe == "Total") |> 
  select(Année, `Type de données`, `Tous les âges`) |> 
  mutate(Année = as.integer(Année),
         `Population` = as.integer(`Tous les âges`))

pop_evolution <- pre_2001 |> 
  bind_rows(pop_evolution_1)

# the last four points need to be distinct because those are the future projections
pop_evolution_tidy <- pop_evolution_tidy %>%
  mutate(PointType = ifelse(row_number() > n() - 4, "Projection (ISQ)", "Valeur du recensement canadien"))

# allows for the end of the line to be dotted based on projection points
# solid_data <- pop_evolution_tidy %>% filter(PointType == "Valeur du recensement canadien" | row_number() == n() - 4)
# dotted_data <- pop_evolution_tidy %>% filter(PointType == "Projection (ISQ)" | row_number() == n() - 4)

solid_data <- pop_evolution %>% filter(`Type de données` == "Estimation")
dotted_data <- pop_evolution %>% filter(`Type de données` == "Projection")

# create the visual 
# pop_et_proj <-
#   ggplot(data = pop_evolution_tidy, aes(x = Année, y = Population)) +
#   geom_point(aes(color = PointType), size = 5) +
#   geom_line(data = solid_data, aes(x = Année, y = Population, group = 1), linetype = "solid",
#             linewidth = 1.5) +
#   geom_line(data = dotted_data, aes(x = Année, y = Population, group = 1), linetype = "dotted",
#             linewidth = 1.5) +
#   ylim(0, max(pop_evolution_tidy$Population)) +
#   scale_color_manual(values = c("Projection (ISQ)" = color_theme("pinkhealth"), "Valeur du recensement canadien" = "black")) +
#   labs(color = element_blank(), title = element_blank()) + 
#   scale_y_continuous(labels = convert_number, limits = c(0,500000)) +
#   gg_cc_theme_no_sf +
#   xlab(NULL)

laval_pop_2025 <- pop_evolution |> 
  filter(Année == 2025) |> 
  pull(Population) |> 
  convert_number()

laval_pop_2051 <- pop_evolution |> 
  filter(Année == 2051) |> 
  pull(Population) |> 
  convert_number()

laval_pop_growth <- pop_evolution |> 
  filter(Année %in% c(2025, 2051)) |> 
  arrange(Année) |> 
  pull(Population) |> 
  {\(x) x[2] / x[1] - 1}() |> 
  convert_pct()

pop_et_proj <-
  ggplot(data = pop_evolution, aes(x = Année, y = `Population`)) +
  #geom_point(aes(color = `Type de données`), size = 2) +
  geom_line(data = solid_data, aes(x = Année, y = `Population`, group = 1, color = `Type de données`), linetype = "solid",
            linewidth = 1.5) +
  geom_line(data = dotted_data, aes(x = Année, y = `Population`, group = 1, color = `Type de données`), linetype = "dotted",
            linewidth = 1.5) +
  #ylim(0, max(pop_evolution$`Tous les âges`)) +
  scale_color_manual(values = c("Projection" = color_theme("pinkhealth"), "Estimation" = "black")) +
  labs(color = element_blank(), title = element_blank()) + 
  scale_y_continuous(labels = convert_number, limits = c(200000,500000)) +
  gg_cc_theme_no_sf +
  xlab("Année")



# NEW DATA
# library(dplyr)
# pop_projections <- 
# readxl::read_excel("data/demography/PopGrAS_RA_base_2024.xlsx", skip = 5) |> 
#   filter(`...3` == "Laval",
#          `...1` == "Référence A2024",
#          `...5` == 3) |> 
#   rename(`Année` = `...4`,) |> 
#   mutate_all(as.numeric)


# laval21 <- cancensus::get_census(dataset = "CA21", 
#                                  regions = list(CSD = 2465005), 
#                                  level = "CSD")
# laval16 <- cancensus::get_census(dataset = "CA16", 
#                                  regions = list(CSD = 2465005), 
#                                  level = "CSD")
# laval11 <- cancensus::get_census(dataset = "CA11", 
#                                  regions = list(CSD = 2465005), 
#                                  level = "CSD")
# laval06 <- cancensus::get_census(dataset = "CA06", 
#                                  regions = list(CSD = 2465005), 
#                                  level = "CSD")
# laval01 <- cancensus::get_census(dataset = "CA01", 
#                                  regions = list(CSD = 2465005), 
#                                  level = "CSD")
# laval1996 <- cancensus::get_census(dataset = "CA1996", 
#                                  regions = list(CSD = 2465005), 
#                                  level = "CSD")
# 
# pop_evol_recensement <- 
# tibble::tibble(Année = c(1996, 2001, 2006, 2011, 2016, 2021),
#                Population = c(laval1996$Population, laval01$Population, laval06$Population, laval11$Population, laval16$Population, laval21$Population),
#                PointType = "Valeur du recensement canadien")

# pop_evolution <- pop_evolution[pop_evolution$Year %in% c(1966:2021), c("Year", "Population")] |> 
#   mutate(PointType = "Valeur du recensement canadien")
# 
# pop_evolution_tidy <- 
#   rbind(pop_evolution, 
#         pop_projections |> 
#           transmute(Year = `Année`, Population = TOTAL, PointType = "Projection (ISQ)")
#   )
# pop_evolution_tidy$Year <- as.numeric(pop_evolution_tidy$Year)
# # allows for the end of the line to be dotted based on projection points
# solid_data <- pop_evolution_tidy %>% filter(PointType == "Valeur du recensement canadien")
# dotted_data <- pop_evolution_tidy %>% filter(PointType == "Projection (ISQ)" | row_number() == n() - 4)
# 
# 
# pop_et_proj <-
# ggplot(data = pop_evolution_tidy, aes(x = Year, y = Population, linetype = PointType, color = PointType)) +
#   geom_point(data = pop_evolution_tidy[pop_evolution_tidy$PointType == "Valeur du recensement canadien",], 
#              size = 5) +
#   geom_line(linewidth = 1.5) +
#   ylim(0, max(pop_evolution_tidy$Population)) +
#   scale_linetype_manual(values = c("Projection (ISQ)" = "dotted", 
#                                    "Valeur du recensement canadien" = "solid")) +
#   scale_color_manual(values = c("Projection (ISQ)" = color_theme("pinkhealth"), 
#                                 "Valeur du recensement canadien" = "black")) +
#   labs(color = NULL, linetype = NULL, title = NULL) + 
#   scale_y_continuous(labels = convert_number, limits = c(0, 500000)) +
#   gg_cc_theme_no_sf +
#   xlab(NULL) +
#  theme(legend.title = element_blank())

ggplot2::ggsave(filename = here::here("output/0_demography/pop_et_proj.pdf"), 
                plot = pop_et_proj, width = 8, height = 5)

# Naissances --------------------------------------------------------------

# Births -----------------------------------------------------------------------
# this is the data source, filter to just look at Laval
### https://statistique.quebec.ca/fr/document/naissances-regions-administratives/tableau/naissances-deces-accroissement-naturel-mariages-par-region-administrative-quebec#tri_phe=10&tri_ra=13

#insert downloaded data set from linked source

# Laval_Births <- read.csv("data/demography/Laval_Births.csv", skip = 5) |> 
#   tibble::as_tibble()
# Laval_Births <- pivot_longer(Laval_Births[3, ], cols = names(Laval_Births)[4:41], )[4:5]
# names(Laval_Births) <- c("year", "naissances")
# Laval_Births$year <- as.numeric(gsub("^X|ᵖ", "", Laval_Births$year))
# Laval_Births$naissances <- as.numeric(Laval_Births$naissances)
# 
# 
# # Get numbner of births 2023
# naissances_laval <- Laval_Births$naissances[Laval_Births$year == 2023]
# naissances_laval <- convert_number(naissances_laval)
# 
# # Inspect the specific rows 35 to 39
# naissances_laval_moyenne_5 <- 
#   convert_number(Laval_Births$naissances[Laval_Births$year %in% 2019:2023] |> mean())

#https://statistique.quebec.ca/fr/document/naissances-regions-administratives/tableau/naissances-deces-accroissement-naturel-mariages-par-region-administrative-quebec#tri_phe=10&tri_ra=13
birth_rate <- read_xlsx("data/new/Fichier_complet_704_205.xlsx", skip = 2) |> 
  slice(1:19) |> 
  row_to_names(row_number = 1) |> 
  clean_names() |> 
  filter(`natalite2` == "Laval") |> 
  select(-1, -2) |> 
  setNames(as.integer(1986:2023)) |> 
  mutate(across(everything(), ~ as.numeric(as.character(.)))) |> 
  pivot_longer(cols = everything(), names_to = "Year", values_to = "Naissances pour 1 000") |> 
  mutate(Year = as.integer(Year))

laval_births <- read_xlsx("data/new/naissance.xlsx", skip = 4) |> 
  filter(`...2` %in% c("Code RA", "13")) |> 
  select(-`...1`, -`...2`, -`...41`) |> 
  mutate(`...3` = replace(`...3`, 1, "Years")) |>
  row_to_names(row_number = 1) |>
  pivot_longer(cols = -Years, names_to = "Year", values_to = "Naissances") |> 
  select(-Years) |> 
  mutate(Year = as.integer(Year),
         Naissances = as.integer(Naissances)) |> 
  left_join(birth_rate, by = "Year") |> 
  mutate(
    Naissances = as.numeric(Naissances),
    `Naissances pour 1 000` = as.numeric(`Naissances pour 1 000`)
  )

laval_avgbirths <- laval_births |> 
  filter(Year >= 2018, Year <= 2022) |> 
  summarise(avg_births = mean(Naissances, na.rm = TRUE)) |> 
  pull(avg_births) |> 
  convert_number()

laval_avgbirthrate <- laval_births |> 
  filter(Year >= 2018, Year <= 2022) |> 
  summarise(avg_births = mean(`Naissances pour 1 000`, na.rm = TRUE)) |> 
  pull(avg_births) |> 
  convert_number()

# laval_births_graph <- 
#   ggplot(laval_births) + 
#   geom_line(aes(x = Year, y = Naissances), color = color_theme("browndemographics"),
#             linewidth = 1.5) + 
#   scale_y_continuous(labels = convert_number, limits = c(2500,4800)) +
#   gg_cc_theme_no_sf +
#   xlab(NULL) +
#   ylab("Naissances")

scale_factor <- max(laval_births$Naissances, na.rm = TRUE) / max(laval_births$`Naissances pour 1 000`, na.rm = TRUE)

laval_births_graph <- 
  ggplot(laval_births) + 
  geom_bar(aes(x = Year, y = Naissances, fill = "Naissances"), stat = "identity") + 
  geom_line(aes(x = Year, y = `Naissances pour 1 000` * scale_factor, color = "Naissances pour 1 000"),
            linewidth = 1.5) +
  scale_y_continuous(
    name = "Naissances", 
    labels = convert_number,
    sec.axis = sec_axis(~ . / scale_factor, name = "Naissances pour 1 000")
  ) +
  scale_fill_manual(values = c("Naissances" = color_theme("browndemographics"))) +
  scale_color_manual(values = c("Naissances pour 1 000" = color_theme("purpletransport"))) +
  labs(fill = NULL, color = NULL) +
  theme(legend.position = "bottom") +
  gg_cc_theme_no_sf +
  xlab(NULL)

laval_births_2022 <- laval_births |> 
  filter(Year == 2022) |> 
  pull(Naissances) |> 
  convert_number()

laval_births_2010 <- laval_births |> 
  filter(Year == 2010) |> 
  pull(Naissances) |> 
  convert_number()

laval_birthrate_2022 <- laval_births |> 
  filter(Year == 2022) |> 
  pull(`Naissances pour 1 000`) |> 
  convert_number()

laval_birthrate_2009 <- laval_births |> 
  filter(Year == 2009) |> 
  pull(`Naissances pour 1 000`) |> 
  convert_number()


ggplot2::ggsave(filename = here::here("output/0_demography/laval_births_graph.pdf"), 
                plot = laval_births_graph, width = 6.5, height = 4)

qs::qsavem(#laval_population_ISQ_pretty, laval_population_2024_pretty,
           laval_population_2025_pretty, laval_size_pretty, density_pretty,
           pop_density_plot, age_pyramid, age_moyen_pretty,
           age_moyen_prov_pretty, age_moyen_cma_pretty, moins_18_pretty, 
           moins_18_prov_pretty, sixtyfive_pretty, sixtyfive_prov_pretty, 
           homme_pct_pretty, femme_pct_pretty, ethnic_origins, groupe_ethniques_diff,
           population_change, quebec_pop_change, pop_et_proj, #naissances_laval,
           #naissances_laval_moyenne_5, 
           laval_births_graph, laval_population_census_pretty,
           laval_pop_2051, laval_pop_2025, laval_pop_growth, laval_avgbirths, laval_births_2022,
           laval_births_2010, laval_birthrate_2009, laval_birthrate_2022, laval_avgbirthrate,
           file = "data/demography/demo.qsm")

                                    
