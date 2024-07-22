#bridget working:
### DEMOGRAPHY - POPULATION AND POPULATION DENSITY #############################
source("R/01_startup")
library(ggplot2)
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

# curbcut::convert_unit lets us convert any type and number to 'Pretty' number. 
# To use curbcut functions, install the package through `devtools::install_github("Curbcut/curbcut")`
laval_population_census <- laval_census$Population
laval_population_census_pretty <- curbcut::convert_unit(x = laval_population)

laval_population_ISQ <- 454990
laval_population_ISQ_pretty <- curbcut::convert_unit(x = laval_population)

laval_size <- (cc.buildr::get_area(laval_census) / 1e6)
laval_size_pretty <- curbcut::convert_unit(x = laval_size)

density <- laval_population_ISQ / laval_size
density_pretty <- curbcut::convert_unit(x = density)


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
ggplot2::ggsave(filename = here::here("output/0_demography/CT_pop_density.png"), 
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

ggplot2::ggsave(filename = here::here("output/0_demography/age_pyramid.png"), 
                plot = age_pyramid, width = 6, height = 4)


# Another way to accomplish the same result, to show a different workflow
# Temporary example to show the other way to get our data
pop_dist_qc_CSD <- get_census(dataset = "CA21", 
                              regions = list(PR = 24), 
                              level = "CSD",
                              vectors = pop_dist_vector)

pop_dist_qc_CSD <- 
  pop_dist_qc_CSD |> 
  # All rows with any NAs have all NAs, so safe to just filter on one column
  filter(!is.na(`Femme 45-49`)) |> 
  mutate(city = case_when(
    GeoUID == "2466023" ~ "Montreal",
    GeoUID == "2465005" ~ "Laval",
    .default = NA),
    region = if_else(CMA_UID == "24462", "Montreal", NA),
    .before = Type) |> 
  group_by(city, region) |> 
  summarize(across(c(`Homme 0-4`:`Femme 85+`), sum),
            .groups = "drop")

pop_dist_qc_CSD_1 <- 
  pop_dist_qc_CSD |> 
  filter(!is.na(city)) |> 
  select(-region)

pop_dist_qc_CSD_2 <- 
  pop_dist_qc_CSD |> 
  filter(region == "Montreal") |> 
  summarize(city = "Montreal CMA", across(c(`Homme 0-4`:`Femme 85+`), sum))

pop_dist_qc_CSD_3 <- 
  pop_dist_qc_CSD |>
  summarize(city = "Province", across(c(`Homme 0-4`:`Femme 85+`), sum))

bind_rows(pop_dist_qc_CSD_1, pop_dist_qc_CSD_2, pop_dist_qc_CSD_3) |> 
  pivot_longer(-city, names_to = "category") |> 
  rename(name = city) |> 
  # Split gender off into its own variable
  mutate(gender = str_extract(category, "(Homme|Femme)"),
         category = str_remove(category, "(Homme |Femme )")) |> 
  group_by(name, gender) |> 
  mutate(pct = value / sum(value)) |> 
  ungroup() |> 
  # Make Femme values negative to facilitate easier population pyramids
  mutate(pct = if_else(gender == "Femme", pct * -1, pct)) |> 
  mutate(category = factor(category, levels = sort_vec)) |> 
  ggplot(aes(x = pct, y = category, fill = gender)) +
  geom_col() +
  facet_wrap(~name, nrow = 2) +
  scale_fill_manual(values = c("Femme" = "pink", "Homme" = "blue")) +
  scale_x_continuous(breaks = -2:2 * 0.04,
                     labels = c("8 %", "4 %", "0", "4 %", "8 %")) +
  theme(legend.position = "bottom")


#average and median age--------------------------------------------------------

average_age <- cancensus::get_census(dataset = "CA21", 
                                      regions = list(CSD = 2465005), 
                                      level = "CSD",
                                      vectors = c("AverageAge" = "v_CA21_386",
                                                  "AverageAgeHomme" = "v_CA21_387",
                                                  "AverageAgeFemme" = "v_CA21_388"))

median_age <- cancensus::get_census(dataset = "CA21", 
                                     regions = list(CSD = 2465005), 
                                     level = "CSD",
                                     vectors = c("MedianAge" = "v_CA21_389",
                                                 "MedianAgeHomme" = "v_CA21_390",
                                                 "MedianAgeFemme" = "v_CA21_391"))

#percent population change------------------------------------------------------

population_change <- cancensus::get_census(dataset = "CA21", 
                                           regions = list(CSD = 2465005), 
                                           level = "CSD",
                                           vectors = c("population_change" = "v_CA21_3"))

#compare population change to rest of province

View(list_census_regions("CA21"))
View(list_census_vectors("CA21"))

quebec_pop_change <- cancensus::get_census(dataset = "CA21", 
                                           regions = list(PR = 8501833), 
                                           level = "PR",
                                           vectors = c("population_change" = "v_CA21_3"))

#ethnic origin for the population ----------------------------------------------
#using Total - Place of birth for the immigrant population in private households
####but then this doesn't account for non-immigrants?

ethnic_origin <- cancensus::get_census(dataset = "CA21", 
                                       regions = list(CSD = 2465005), 
                                       level = "CSD",
                                       vectors = c("Total" = "v_CA21_4455",
                                                   "Americas" = "v_CA21_4458",
                                                   "Europe" = "v_CA21_4494",
                                                   "Africa" = "v_CA21_4545",
                                                   "Asia" = "v_CA21_4578",
                                                   "Oceana and other" = "v_CA21_4632"))

ethnic_origin <- ethnic_origin |> mutate(PercentAmericas = Americas/Total) |> 
  mutate(PercentEurope = Europe/Total,
         PercentAfrica = Africa/Total,
         PercentAsia = Asia/Total,
         PercentOceana_Other = `Oceana and other`/Total)

#see if this is significantly different than previous years or not

View(list_census_vectors("CA06"))

ethnic_origin2006 <- cancensus::get_census(dataset = "CA06",
                                           regions = list(CSD = 2465005), 
                                           level = "CSD")
                                           


qs::qsavem(laval_population_ISQ_pretty, laval_size_pretty, density_pretty,
           pop_density_plot, age_pyramid, age_moyen_pretty,
           age_moyen_prov_pretty, age_moyen_cma_pretty, moins_18_pretty, 
           moins_18_prov_pretty, sixtyfive_pretty, sixtyfive_prov_pretty, 
           file = "data/demography/demo.qsm")

                                    
