#bridget working:
### DEMOGRAPHY - POPULATION AND POPULATION DENSITY #############################

library(ggplot2)
#geom_context <- qs::qread("data/geom_context/geom_context.qs")

# Population count --------------------------------------------------------

# To get raw census data, we use the cancensus package. At this link, there is 
# documentation on how to get and save an API key: https://mountainmath.github.io/cancensus/

# We grab census information from Laval. This small call lets us retrieve 
# population counts for the whole city.
laval_census <- cancensus::get_census(dataset = "CA21", 
                                      regions = list(CSD = 2465005), 
                                      level = "CSD")

# curbcut::convert_unit lets us convert any type and number to 'Pretty' number. 
# To use curbcut functions, install the package through `devtools::install_github("Curbcut/curbcut")`
laval_population <- laval_census$Population
laval_population_pretty <- curbcut::convert_unit(x = laval_population)

# Another example is to use its method pct, or dollar
curbcut:::convert_unit.pct(x = 0.4555, decimal = 1)
curbcut:::convert_unit.dollar(x = 102512)


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


# Make a plot of density
pop_density_plot <- 
  ggplot(data = CT) +
  geom_sf(aes(fill = pop_density), color = "transparent") +
  scale_fill_viridis_c(
    option = "viridis", 
    direction = -1, 
    name = "Population density\n(per km²)",
    labels = \(x) curbcut::convert_unit(x = x),
    guide = guide_colourbar(barwidth = 15),
    limits = c(1000, 6000),
    oob = scales::oob_squish
  ) +
  theme(legend.position = "bottom")

#geom_context(sf_focus = CT) +
theme(legend.position = "bottom")

# Save the plot so it's in a good quality
ggplot2::ggsave(filename = here::here("output/0_demography/CT_pop_density.png"), 
                plot = pop_density_plot, width = 6, height = 4)


# Population density change -----------------------------------------------

# Get 2016 and 2021 population density in dissemination blocks
CT <- cancensus::get_census(dataset = "CA21", 
                            regions = list(CSD = 2465005), 
                            level = "CT", 
                            geo_format = "sf")
CT_2016 <- cancensus::get_census(dataset = "CA16", 
                                 regions = list(CSD = 2465005), 
                                 level = "CT", 
                                 geo_format = "sf")

# Interpolate 2016 population to 2021 spatial features
names(CT)[names(CT) == "GeoUID"] <- "ID"
names(CT_2016)[names(CT_2016) == "GeoUID"] <- "ID"
CT_2016 <- cc.buildr::interpolate_from_area(to = CT, 
                                            from = CT_2016, 
                                            additive = "Population", 
                                            crs = 32618)

# Calculate population density on the two years
CT <- pop_density_fun(CT)
CT_2016 <- pop_density_fun(CT_2016, col_name = "pop_density_2016")

# Merge 2016 population density with the 2021
CT <- merge(CT, sf::st_drop_geometry(CT_2016)[c("ID", "pop_density_2016")], by = "ID")

# Get change in population density
CT$pop_density_change <- CT$pop_density - CT$pop_density_2016

# Prepare the plot
# Define the new breaks with symmetry and without empty ranges
breaks <- c(-Inf, -300, -100, -50, -25, 25, 50, 100, 300, Inf)

# Define the new labels reflecting the breaks without empty categories
labels <- c("Perte de plus de 300", 
            "Perte de 100 à 300", 
            "Perte de 50 à 100",
            "Perte de 25 à 50", 
            "Environ identique",
            "Gain de 25 à 50", 
            "Gain de 50 à 100",
            "Gain de 100 à 300", 
            "Gain de plus de 300")

# Define the colors, making sure there is a color for each category
# Assuming viridis gives a range of colors from which we can take a subset
colors <- rev(viridis::viridis(length(labels)))
colors[which(labels == "Environ identique")] <- "white" # Set 'About the same' to white


# Create a factor variable based on the defined breaks
CT$pop_density_change_cat <- cut(CT$pop_density_change, breaks = breaks, 
                                 labels = labels, right = FALSE)
CT$pop_density_change_cat <- factor(CT$pop_density_change_cat, levels = labels)

# Switch to full geo (spanning under water!)
CT <- cc.data::census_switch_full_geo(CT, scale_name = "CT")
CT <- sf::st_transform(CT, crs = 32618)

# Generate the plot with manual color scaling
pop_density_change_plot <- 
  ggplot(data = CT) +
  geom_sf(aes(fill = pop_density_change_cat), linewidth=0.01, 
          color = "transparent") +
  scale_fill_manual(
    values = colors,
    name = "Population density\nchange (per km²)",
    labels = labels,
    limits = labels, 
    drop = FALSE,
    guide = guide_legend(title.position = "top", title.hjust = 0.5)
  ) +
  theme(legend.position = "right")
#geom_context(sf_focus = CT) +
theme(legend.position = "right")

# Save the plot so it's in a good quality
ggplot2::ggsave(filename = here::here("output/0_demography/CT_pop_density_change.png"), 
                plot = pop_density_change_plot, width = 6, height = 4)


# Save the objects that will be referred in the text ----------------------

qs::qsavem(laval_population_pretty, pop_density_plot, 
           pop_density_change_plot,
           file = "data/demography/pop_and_density.qsm")



### Population Gender------------------------------------------------------------

View(list_census_vectors("CA21"))

pop_gender <- cancensus::get_census(dataset = "CA21", 
                                        regions = list(CSD = 2465005), 
                                        level = "CSD",
                                        vectors = c("Total" = "v_CA21_8",
                                                    "Male" = "v_CA21_9",
                                                    "Female" = "v_CA21_10"))

pop_gender <- pop_gender |>
  mutate(PercentMale = Male / Total * 100) |> mutate(PercentFemale = Female/Total * 100)


# Population Distribution by Age and Gender------------------------------------

# Get all the age/gender variables for the 2021 census
pop_dist_vector <- c(
  "Male 0-4" = "v_CA21_15", "Female 0-4" = "v_CA21_16", 
  "Male 5-9" = "v_CA21_33", "Female 5-9" = "v_CA21_34", 
  "Male 10-14" = "v_CA21_51", "Female 10-14" = "v_CA21_52", 
  "Male 15-19" = "v_CA21_72", "Female 15-19" = "v_CA21_73", 
  "Male 20-24" = "v_CA21_90", "Female 20-24" = "v_CA21_91", 
  "Male 25-29" = "v_CA21_108", "Female 25-29" = "v_CA21_109", 
  "Male 30-34" = "v_CA21_126", "Female 30-34" = "v_CA21_127", 
  "Male 35-39" = "v_CA21_144", "Female 35-39" = "v_CA21_145", 
  "Male 40-44" = "v_CA21_162", "Female 40-44" = "v_CA21_163",
  "Male 45-49" = "v_CA21_180", "Female 45-49" = "v_CA21_181", 
  "Male 50-54" = "v_CA21_198", "Female 50-54" = "v_CA21_199", 
  "Male 55-59" = "v_CA21_216", "Female 55-59" = "v_CA21_217", 
  "Male 60-64" = "v_CA21_234", "Female 60-64" = "v_CA21_235", 
  "Male 65-69" = "v_CA21_255", "Female 65-69" = "v_CA21_256", 
  "Male 70-74" = "v_CA21_273", "Female 70-74" = "v_CA21_274", 
  "Male 75-79" = "v_CA21_291", "Female 75-79" = "v_CA21_292", 
  "Male 80-84" = "v_CA21_309", "Female 80-84" = "v_CA21_310", 
  "Male 85 and over" = "v_CA21_327", "Female 85 and over" = "v_CA21_328")

# Vector for sorting
sort_vec <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", 
              "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", 
              "70-74", "75-79", "80-84", "85 and over")

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
  bind_rows(pop_distribution_laval, pop_distribution_qc) |> 
  mutate(name = c("Laval", "Quebec"), .before = GeoUID) |> 
  select(-c(GeoUID:CMA_UID, C_UID)) |> 
  pivot_longer(-name, names_to = "category") |> 
  # Split gender off into its own variable
  mutate(gender = str_extract(category, "(Male|Female)"),
         category = str_remove(category, "(Male |Female )")) |> 
  group_by(name, gender) |> 
  mutate(pct = value / sum(value)) |> 
  ungroup() |> 
  # Make female values negative to facilitate easier population pyramids
  mutate(pct = if_else(gender == "Female", pct * -1, pct)) |> 
  mutate(category = factor(category, levels = sort_vec))
  
pop_dist |> 
  ggplot(aes(x = pct, y = category, fill = gender)) +
  geom_col() +
  facet_wrap(~name, nrow = 1) +
  scale_fill_manual(values = c("Female" = "pink", "Male" = "blue")) +
  scale_x_continuous(breaks = -2:2 * 0.04,
                     labels = c("8 %", "4 %", "0", "4 %", "8 %")) +
  theme(legend.position = "bottom")

# Another way to accomplish the same result, to show a different workflow
# Temporary example to show the other way to get our data
pop_dist_qc_CSD <- get_census(dataset = "CA21", 
                              regions = list(PR = 24), 
                              level = "CSD",
                              vectors = pop_dist_vector)

pop_dist_qc_CSD <- 
  pop_dist_qc_CSD |> 
  # All rows with any NAs have all NAs, so safe to just filter on one column
  filter(!is.na(`Female 45-49`)) |> 
  mutate(city = case_when(
    GeoUID == "2466023" ~ "Montreal",
    GeoUID == "2465005" ~ "Laval",
    .default = NA),
    region = if_else(CMA_UID == "24462", "Montreal", NA),
    .before = Type) |> 
  group_by(city, region) |> 
  summarize(across(c(`Male 0-4`:`Female 85 and over`), sum),
            .groups = "drop")

pop_dist_qc_CSD_1 <- 
  pop_dist_qc_CSD |> 
  filter(!is.na(city)) |> 
  select(-region)

pop_dist_qc_CSD_2 <- 
  pop_dist_qc_CSD |> 
  filter(region == "Montreal") |> 
  summarize(city = "Montreal CMA", across(c(`Male 0-4`:`Female 85 and over`), sum))

pop_dist_qc_CSD_3 <- 
  pop_dist_qc_CSD |>
  summarize(city = "Province", across(c(`Male 0-4`:`Female 85 and over`), sum))

bind_rows(pop_dist_qc_CSD_1, pop_dist_qc_CSD_2, pop_dist_qc_CSD_3) |> 
  pivot_longer(-city, names_to = "category") |> 
  rename(name = city) |> 
  # Split gender off into its own variable
  mutate(gender = str_extract(category, "(Male|Female)"),
         category = str_remove(category, "(Male |Female )")) |> 
  group_by(name, gender) |> 
  mutate(pct = value / sum(value)) |> 
  ungroup() |> 
  # Make female values negative to facilitate easier population pyramids
  mutate(pct = if_else(gender == "Female", pct * -1, pct)) |> 
  mutate(category = factor(category, levels = sort_vec)) |> 
  ggplot(aes(x = pct, y = category, fill = gender)) +
  geom_col() +
  facet_wrap(~name, nrow = 2) +
  scale_fill_manual(values = c("Female" = "pink", "Male" = "blue")) +
  scale_x_continuous(breaks = -2:2 * 0.04,
                     labels = c("8 %", "4 %", "0", "4 %", "8 %")) +
  theme(legend.position = "bottom")


#average and median age--------------------------------------------------------

average_age <- cancensus::get_census(dataset = "CA21", 
                                      regions = list(CSD = 2465005), 
                                      level = "CSD",
                                      vectors = c("AverageAge" = "v_CA21_386",
                                                  "AverageAgeMale" = "v_CA21_387",
                                                  "AverageAgeFemale" = "v_CA21_388"))

median_age <- cancensus::get_census(dataset = "CA21", 
                                     regions = list(CSD = 2465005), 
                                     level = "CSD",
                                     vectors = c("MedianAge" = "v_CA21_389",
                                                 "MedianAgeMale" = "v_CA21_390",
                                                 "MedianAgeFemale" = "v_CA21_391"))

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
                                           









                                    