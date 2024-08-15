### DEMOGRAPHY - POPULATION AND POPULATION DENSITY #############################
source("R/01_startup.R")
#geom_context <- qs::qread("data/geom_context/geom_context.qs")

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

laval_population_ISQ <- 454990
laval_population_ISQ_pretty <- convert_number(x = laval_population_ISQ)

laval_size <- (cc.buildr::get_area(laval_census) / 1e6)
laval_size_pretty <- convert_number(x = laval_size)

density <- laval_population_ISQ / laval_size
density_pretty <- convert_number(x = density)


# Population density graph ------------------------------------------------

CT <- cancensus::get_census(dataset = "CA21", 
                            regions = list(CSD = 2465005), 
                            level = "CT", 
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

labels <- c("0 - 1 000", "1 000 - 2 000", "2 000 - 3 000", "3 000 - 4 000", 
            "+ 4 000")

t <- add_bins(df = CT,
              variable = "pop_density",
              breaks = c(0, 1000, 2000, 3000, 4000, Inf),
              labels = labels
)

# Union the features so the polygons don't show their borders. Might revisit
# with the addition of streets!
t <- Reduce(rbind,
            split(t, t$binned_variable) |>
              lapply(\(x) {
                out <- tibble::tibble(x$binned_variable)
                out$geometry <- sf::st_union(x)
                sf::st_as_sf(out, crs = 4326)[1, ]
              })
) |> sf::st_as_sf()
names(t)[1] <- "binned_variable"

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
                plot = pop_density_plot, width = 6, height = 4)


# Population Distribution by Age and Gender------------------------------------

### Age values for text
laval_age_moyen <- get_census(dataset = "CA21", 
                              regions = list(CSD = 2465005), 
                              level = "CSD",
                              vectors = c("age_moyen" = "v_CA21_386"))$age_moyen
prov_age_moyen <- get_census(dataset = "CA21", 
                             regions =  list(PR = 24), 
                             level = "PR",
                             vectors = c("age_moyen" = "v_CA21_386"))$age_moyen
CMA_age_moyen <- get_census(dataset = "CA21", 
                            regions = list(CMA = 24462), 
                            level = "CMA",
                            vectors = c("age_moyen" = "v_CA21_386"))$age_moyen
age_moyen_pretty <- convert_number(laval_age_moyen)
age_moyen_prov_pretty <- convert_number(prov_age_moyen)
age_moyen_cma_pretty <- convert_number(CMA_age_moyen)


laval_moins_18 <- get_census(dataset = "CA21", 
                             regions = list(CSD = 2465005), 
                             level = "CSD",
                             vectors = c("v_CA21_11", 
                                         "v_CA21_74", "v_CA21_77", "v_CA21_80", 
                                         "v_CA21_83"))
laval_moins_18 <- {laval_moins_18[11:15] |> unlist() |> sum()} / laval_moins_18$Population
prov_moins_18 <- get_census(dataset = "CA21", 
                            regions =  list(PR = 24), 
                            level = "PR",
                             vectors = c("v_CA21_11", 
                                         "v_CA21_74", "v_CA21_77", "v_CA21_80", 
                                         "v_CA21_83"))
prov_moins_18 <- {prov_moins_18[9:13] |> unlist() |> sum()} / prov_moins_18$Population

moins_18_pretty <- convert_pct(laval_moins_18)
moins_18_prov_pretty <- convert_pct(prov_moins_18)

laval_sixtyfive <- get_census(dataset = "CA21", 
                              regions = list(CSD = 2465005), 
                              level = "CSD",
                              vectors = c("sixtyfive+" = "v_CA21_251"))
prov_sixtyfive <- get_census(dataset = "CA21", 
                             regions =  list(PR = 24), 
                             level = "PR",
                             vectors = c("sixtyfive+" = "v_CA21_251"))

sixtyfive_pretty <- convert_pct(laval_sixtyfive$`sixtyfive+` / laval_sixtyfive$Population)
sixtyfive_prov_pretty <- convert_pct(prov_sixtyfive$`sixtyfive+` / prov_sixtyfive$Population)


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
pop_dist_laval  <- get_census(dataset = "CA21", 
                              regions = list(CSD = 2465005), 
                              level = "CSD",
                              vectors = pop_dist_vector)

# Get the variables for the province
pop_dist_qc  <- get_census(dataset = "CA21", 
                           regions = list(PR = 24), 
                           level = "PR",
                           vectors = pop_dist_vector)

# Merge the two datasets
pop_dist <-
  bind_rows(pop_dist_laval, pop_dist_qc) |> 
  mutate(name = c("Laval", "Ensemble du Québec"), .before = GeoUID) |> 
  select(-c(GeoUID:CMA_UID, C_UID)) |> 
  pivot_longer(-name, names_to = "category") |> 
  # Split gender off into its own variable
  mutate(gender = str_extract(category, "(Homme|Femme)"),
         category = str_remove(category, "(Homme |Femme )")) |> 
  group_by(name, gender) |> 
  mutate(pct = value / sum(value)) |> 
  ungroup() |> 
  # Make Femme values negative to facilitate easier population pyramids
  mutate(pct = if_else(gender == "Femme", pct * -1, pct)) |> 
  mutate(category = factor(category, levels = sort_vec))

# Reorder the facet levels as needed
pop_dist$name <- factor(pop_dist$name, levels = c("Laval", "Ensemble du Québec"))

age_pyramid <- pop_dist |> 
  ggplot(aes(x = pct, y = category, fill = gender)) +
  geom_col() +
  facet_wrap(~name, nrow = 1) +
  scale_fill_manual(name = element_blank(), values = c("Femme" = color_theme("pinkhealth"), "Homme" = color_theme("blueexplorer"))) +
  scale_x_continuous(breaks = -2:2 * 0.04,
                     labels = c("8 %", "4 %", "0", "4 %", "8 %")) +
  gg_cc_theme_no_sf +
  xlab(element_blank())+
  ylab(element_blank()) +
  theme(strip.text = element_text(size = 12),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7)) +
  geom_text(aes(label = ifelse(abs(pct) > 0.06, "*", "")), 
            size = 6)

ggplot2::ggsave(filename = here::here("output/0_demography/age_pyramid.pdf"), 
                plot = age_pyramid, width = 6, height = 4)


# Répartition par genre ---------------------------------------------------

h_f  <- get_census(dataset = "CA21", 
                   regions = list(CSD = 2465005), 
                   level = "CSD",
                   vectors = c(homme = "v_CA21_9", femme = "v_CA21_10"))

homme_pct_pretty <- convert_pct(h_f$homme / (h_f$homme + h_f$femme))
femme_pct_pretty <- convert_pct(h_f$femme / (h_f$femme + h_f$homme))


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
pop_evolution <- read.csv("data/demography/Population Evolution.csv", skip = 3) |> 
  tibble::as_tibble()
pop_evolution_tidy <- pop_evolution[1:16, ]
names(pop_evolution_tidy)[1] <- "Année"

# the last four points need to be distinct because those are the future projections
pop_evolution_tidy <- pop_evolution_tidy %>%
  mutate(PointType = ifelse(row_number() > n() - 4, "Projection (ISQ)", "Valeur du recensement canadien"))

# allows for the end of the line to be dotted based on projection points
solid_data <- pop_evolution_tidy %>% filter(PointType == "Valeur du recensement canadien" | row_number() == n() - 4)
dotted_data <- pop_evolution_tidy %>% filter(PointType == "Projection (ISQ)" | row_number() == n() - 4)

# create the visual 
pop_et_proj <-
  ggplot(data = pop_evolution_tidy, aes(x = Année, y = Population)) +
  geom_point(aes(color = PointType), size = 5) +
  geom_line(data = solid_data, aes(x = Année, y = Population, group = 1), linetype = "solid",
            linewidth = 2) +
  geom_line(data = dotted_data, aes(x = Année, y = Population, group = 1), linetype = "dotted",
            linewidth = 1.5) +
  ylim(0, max(pop_evolution_tidy$Population)) +
  scale_color_manual(values = c("Projection (ISQ)" = color_theme("pinkhealth"), "Valeur du recensement canadien" = "black")) +
  labs(color = element_blank(), title = element_blank()) + 
  scale_y_continuous(labels = convert_number, limits = c(0,500000)) +
  gg_cc_theme_no_sf

ggplot2::ggsave(filename = here::here("output/0_demography/pop_et_proj.pdf"), 
                plot = pop_et_proj, width = 7, height = 3)



# Naissances --------------------------------------------------------------

# Births -----------------------------------------------------------------------
# this is the data source, filter to just look at Laval
### https://statistique.quebec.ca/fr/document/naissances-regions-administratives/tableau/naissances-deces-accroissement-naturel-mariages-par-region-administrative-quebec#tri_phe=10&tri_ra=13

#insert downloaded data set from linked source

Laval_Births <- read.csv("data/demography/Laval_Births.csv", skip = 5) |> 
  tibble::as_tibble()
Laval_Births <- pivot_longer(Laval_Births[3, ], cols = names(Laval_Births)[4:41], )[4:5]
names(Laval_Births) <- c("year", "naissances")
Laval_Births$year <- as.numeric(gsub("^X|ᵖ", "", Laval_Births$year))
Laval_Births$naissances <- as.numeric(Laval_Births$naissances)


# Get numbner of births 2023
naissances_laval <- Laval_Births$naissances[Laval_Births$year == 2023]
naissances_laval <- convert_number(naissances_laval)

# Inspect the specific rows 35 to 39
naissances_laval_moyenne_5 <- 
  convert_number(Laval_Births$naissances[Laval_Births$year %in% 2019:2023] |> mean())

laval_births_graph <- 
  ggplot(Laval_Births) + 
  geom_line(aes(x = year, y = naissances), color = color_theme("browndemographics"),
            linewidth = 2) + 
  scale_y_continuous(labels = convert_number, limits = c(2500,4800)) +
  gg_cc_theme_no_sf +
  xlab("Année") +
  ylab("Naissances")

ggplot2::ggsave(filename = here::here("output/0_demography/laval_births_graph.pdf"), 
                plot = laval_births_graph, width = 4, height = 3)

qs::qsavem(laval_population_ISQ_pretty, laval_size_pretty, density_pretty,
           pop_density_plot, age_pyramid, age_moyen_pretty,
           age_moyen_prov_pretty, age_moyen_cma_pretty, moins_18_pretty, 
           moins_18_prov_pretty, sixtyfive_pretty, sixtyfive_prov_pretty, 
           homme_pct_pretty, femme_pct_pretty, ethnic_origins, groupe_ethniques_diff,
           population_change, quebec_pop_change, pop_et_proj, naissances_laval,
           naissances_laval_moyenne_5, laval_births_graph, laval_population_census_pretty,
           file = "data/demography/demo.qsm")

                                    
