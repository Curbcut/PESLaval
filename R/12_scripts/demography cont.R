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


#broad population distribution-------------------------------------------------

pop_dist_broad_laval <- data.frame(
  Category = c("Total", "0-19", "20-64", "65 plus"),
  Year = c("2021", "2021", "2021", "2021"),
  Value = c("438370", "101240","255195", "81930"),
  Percent = c("100", "23.09", "58.21", "18.69")
)
                                                          



# updated with David's code:
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
  bind_rows(pop_dist_laval, pop_dist_qc) |> 
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


average_age_Montreal <- cancensus::get_census(dataset = "CA21", 
                                              regions = list(CSD = 2466023),
                                              level = "CSD",
                                              vectors = c("AverageAge" = "v_CA21_386"))
                                                               
average_age_Quebec <-  cancensus::get_census(dataset = "CA21", 
                                             regions = list(PR = 24),
                                             level = "PR",
                                             vectors = c("AverageAge" = "v_CA21_386"))                                                              


#percent population change------------------------------------------------------

population_change <- cancensus::get_census(dataset = "CA21", 
                                           regions = list(CSD = 2465005), 
                                           level = "CSD",
                                           vectors = c("population_change" = "v_CA21_3"))

#compare population change to rest of province

View(list_census_regions("CA21"))
View(list_census_vectors("CA21"))

quebec_pop_change <- cancensus::get_census(dataset = "CA21", 
                                           regions = list(PR = 24), 
                                           level = "PR",
                                           vectors = c("population_change" = "v_CA21_3"))

#ethnic origin for the population ----------------------------------------------
# using Total - Ethnic or cultural origin for the population in private households

ethnic_origin <- cancensus::get_census(dataset = "CA21", 
                                       regions = list(CSD = 2465005), 
                                       level = "CSD",
                                       vectors = c("Total" = "v_CA21_4917",
                                                   "Canadian" = "v_CA21_4920"
                                                   ))


# Ethnic Origin Just for immigrants - 2021 -------------------------------------
# using Total - Place of birth for the immigrant population in private households

ethnic_origin_immigrants <- cancensus::get_census(dataset = "CA21", 
                                                  regions = list(CSD = 2465005), 
                                                  level = "CSD",
                                                  vectors = c("Total" = "v_CA21_4455",
                                                              "Americas" = "v_CA21_4458",
                                                              "Europe" = "v_CA21_4494",
                                                              "Africa" = "v_CA21_4545",
                                                              "Asia" = "v_CA21_4578",
                                                              "Oceana and other" = "v_CA21_4632"))

ethnic_origin_immigrants_aspercent <- ethnic_origin |> mutate(PercentAmericas = Americas/Total) |> 
  mutate(PercentEurope = Europe/Total,
         PercentAfrica = Africa/Total,
         PercentAsia = Asia/Total,
         PercentOceana_Other = `Oceana and other`/Total)



# Immigrant Status--------------------------------------------------------------

total_immigrants <- cancensus::get_census(dataset = "CA21", 
                                          regions = list(CSD = 2465005), 
                                          level = "CSD",
                                          vectors = c("Total" = "v_CA21_4404",
                                                      "Non-Immigrants" = "v_CA21_4407",
                                                      "Immigrants" = "v_CA21_4410"))

total_immigrants_as_percent <- total_immigrants |> 
  mutate(PercentImmigrant = Immigrants/Total*100,
         PercentNonImmigrant = `Non-Immigrants`/Total)

# Immigrant Status 2006 for comparison
# using total pop by immigrant status

total_immigrants2006 <- cancensus::get_census(dataset = "CA21", 
                                              regions = list(CSD = 2465005), 
                                              level = "CSD",
                                              vectors = c("Total" = "v_CA06_474",
                                                          "Non-Immigrants" = "v_CA06_475",
                                                          "Immigrants" = "v_CA06_478"))

total_immigrants2006_aspercent <- total_immigrants2006 |> 
  mutate(PercentImmigrant = Immigrants/Total *100,
         PercentNonImmigrant = `Non-Immigrants`/Total)


# Immigrant Status 2016 for comparison
# using total pop by immigrant status

total_immigrants2016 <- cancensus::get_census(dataset = "CA16", 
                                              regions = list(CSD = 2465005), 
                                              level = "CSD",
                                              vectors = c("Total" = "v_CA16_3405",
                                                          "Non-Immigrants" = "v_CA16_3408",
                                                          "Immigrants" = "v_CA16_3411"))

total_immigrants2016_aspercent <- total_immigrants2016 |> 
  mutate(PercentImmigrant = Immigrants/Total * 100,
         PercentNonImmigrant = `Non-Immigrants`/Total)


# 2001 

View(list_census_vectors("CA01"))


total_immigrants2001 <- cancensus::get_census(dataset = "CA01", 
                                              regions = list(CSD = 2465005), 
                                              level = "CSD",
                                              vectors = c("Total" = "v_CA01_402",
                                                          "Immigrants" = "v_CA01_406"))


total_immigrants2001_aspercent <- total_immigrants2001 |> 
  mutate(PercentImmigrant = Immigrants/Total*100)


# 1996 

View(list_census_vectors("CA1996"))

total_immigrants1996 <- cancensus::get_census(dataset = "CA1996", 
                                              regions = list(CSD = 2465005), 
                                              level = "CSD",
                                              vectors = c("Total" = "v_CA1996_125",
                                                          "Immigrants" = "v_CA1996_128"))

total_immigrants1996_aspercent <- total_immigrants1996 |> 
  mutate(PercentImmigrant = Immigrants/Total*100)


# visualise change in immigrant population 
#combine tibbles

combined_immigrantdata <- bind_rows(total_immigrants_as_percent, total_immigrants2016_aspercent, total_immigrants2006_aspercent, total_immigrants2001_aspercent, total_immigrants1996_aspercent)

#add years

combined_immigrantdata <- combined_immigrantdata |> 
  mutate(year = c(2021, 2016, 2006, 2001, 1996)) |> 
  mutate(Region = "Laval")



#ggplot(combined_immigrantdata, aes(x = `year`, y = `PercentImmigrant`)) +
  #geom_col()+ 
  #labs(
    #title = "Percentage of Immigrants by Year",
    #x = "Year",
    #y = "Percentage of Immigrants"
 # )

#get rid of space between bars: 

ggplot(combined_immigrantdata, aes(x = year, y = PercentImmigrant)) +
  geom_line()+
  geom_point() +
  labs(x = "Year", y = "Immigrant Percent of Population") +
  coord_cartesian(ylim = c(0, max(combined_immigrantdata$PercentImmigrant) * 1.1))




library(tidyverse)
library(ggplot2)
 
# immigrant status queb and mtl --------------------------------------------------------

total_immigrants_mtl <- cancensus::get_census(dataset = "CA21", 
                                          regions = list(CSD = 2466023), 
                                          level = "CSD",
                                          vectors = c("Total" = "v_CA21_4404",
                                                      "Non-Immigrants" = "v_CA21_4407",
                                                      "Immigrants" = "v_CA21_4410"))

total_immigrants_mtl_as_percent <- total_immigrants_mtl |> 
  mutate(PercentImmigrant = Immigrants/Total*100,
         PercentNonImmigrant = `Non-Immigrants`/Total*100)
  




# plot of immigrant status over years for province and laval and Montreal-------------------


total_immigrants_queb <- cancensus::get_census(dataset = "CA21", 
                                          regions = list(PR = 24), 
                                          level = "PR",
                                          vectors = c("Total" = "v_CA21_4404",
                                                      "Non-Immigrants" = "v_CA21_4407",
                                                      "Immigrants" = "v_CA21_4410"))

total_immigrants_queb_as_percent <- total_immigrants_queb |> 
  mutate(PercentImmigrant = Immigrants/Total*100,
         PercentNonImmigrant = `Non-Immigrants`/Total*100)

# Immigrant Status 2006 for comparison
# using total pop by immigrant status

total_immigrants_queb2006 <- cancensus::get_census(dataset = "CA06", 
                                              regions = list(PR = 24), 
                                              level = "PR",
                                              vectors = c("Total" = "v_CA06_474",
                                                          "Non-Immigrants" = "v_CA06_475",
                                                          "Immigrants" = "v_CA06_478"))

total_immigrants_queb2006_aspercent <- total_immigrants_queb2006 |> 
  mutate(PercentImmigrant = Immigrants/Total *100,
         PercentNonImmigrant = `Non-Immigrants`/Total*100)


# Immigrant Status 2016 for comparison
# using total pop by immigrant status

total_immigrants_queb2016 <- cancensus::get_census(dataset = "CA16", 
                                              regions = list(PR = 24), 
                                              level = "PR",
                                              vectors = c("Total" = "v_CA16_3405",
                                                          "Non-Immigrants" = "v_CA16_3408",
                                                          "Immigrants" = "v_CA16_3411"))

total_immigrants_queb2016_aspercent <- total_immigrants_queb2016 |> 
  mutate(PercentImmigrant = Immigrants/Total * 100,
         PercentNonImmigrant = `Non-Immigrants`/Total*100)


# 2001 

View(list_census_vectors("CA01"))


total_immigrants_queb2001 <- cancensus::get_census(dataset = "CA01", 
                                              regions = list(PR = 24), 
                                              level = "PR",
                                              vectors = c("Total" = "v_CA01_402",
                                                          "Immigrants" = "v_CA01_406"))


total_immigrants_queb2001_aspercent <- total_immigrants_queb2001 |> 
  mutate(PercentImmigrant = Immigrants/Total*100)


# 1996 

View(list_census_vectors("CA1996"))

total_immigrants_queb1996 <- cancensus::get_census(dataset = "CA1996", 
                                              regions = list(PR = 24), 
                                              level = "PR",
                                              vectors = c("Total" = "v_CA1996_125",
                                                          "Immigrants" = "v_CA1996_128"))

total_immigrants_queb1996_aspercent <- total_immigrants_queb1996 |> 
  mutate(PercentImmigrant = Immigrants/Total*100)


# visualise change in immigrant population 
#combine tibbles

combined_immigrantdata_queb <- bind_rows(total_immigrants_queb_as_percent, total_immigrants_queb2016_aspercent, total_immigrants_queb2006_aspercent, total_immigrants_queb2001_aspercent, total_immigrants_queb1996_aspercent)

#add years

combined_immigrantdata_queb <- combined_immigrantdata_queb |> 
  mutate(year = c(2021, 2016, 2006, 2001, 1996)) |> 
  mutate(Region = "Quebec")



#get rid of space between bars: 

ggplot(combined_immigrantdata_queb, aes(x = year, y = PercentImmigrant)) +
  geom_line()+
  geom_point() +
  labs(x = "Year", y = "Immigrant Percent of Population") +
  coord_cartesian(ylim = c(0, max(combined_immigrantdata_queb$PercentImmigrant) * 1.1))


# combine Laval and Province 

immigrant_data_queb_laval <- bind_rows(combined_immigrantdata_queb, combined_immigrantdata)


ggplot(immigrant_data_queb_laval, aes(x = year, y = PercentImmigrant, colour = `Region`)) +
  geom_line()+
  geom_point() +
  labs(x = "Year", y = "Immigrant Percent of Population", title = "Immigrant Population as percent of Total Population")
  
  


# Montreal Data #### ran into issues and did not work

total_immigrants_mtl <- cancensus::get_census(dataset = "CA21", 
                                               regions = list(CSD = 2466023), 
                                               level = "CSD",
                                               vectors = c("Total" = "v_CA21_4404",
                                                           "Non-Immigrants" = "v_CA21_4407",
                                                           "Immigrants" = "v_CA21_4410"))

total_immigrants_mtl_as_percent <- total_immigrants_mtl |> 
  mutate(PercentImmigrant = Immigrants/Total*100,
         PercentNonImmigrant = `Non-Immigrants`/Total*100)

# Immigrant Status 2006 for comparison
# using total pop by immigrant status

total_immigrants_mtl2006 <- cancensus::get_census(dataset = "CA06", 
                                                   regions = list(CSD = 2466023), 
                                                   level = "CSD",
                                                   vectors = c("Total" = "v_CA06_474",
                                                               "Non-Immigrants" = "v_CA06_475",
                                                               "Immigrants" = "v_CA06_478"))

total_immigrants_mtl2006_aspercent <- total_immigrants_mtl2006 |> 
  mutate(PercentImmigrant = Immigrants/Total *100,
         PercentNonImmigrant = `Non-Immigrants`/Total*100)


# Immigrant Status 2016 for comparison
# using total pop by immigrant status

total_immigrants_mtl2016 <- cancensus::get_census(dataset = "CA16", 
                                                   regions = list(CSD = 2466023), 
                                                   level = "CSD",
                                                   vectors = c("Total" = "v_CA16_3405",
                                                               "Non-Immigrants" = "v_CA16_3408",
                                                               "Immigrants" = "v_CA16_3411"))

total_immigrants_mtl2016_aspercent <- total_immigrants_mtl2016 |> 
  mutate(PercentImmigrant = Immigrants/Total * 100,
         PercentNonImmigrant = `Non-Immigrants`/Total*100)


# 2001 

View(list_census_vectors("CA01"))


total_immigrants_mtl2001 <- cancensus::get_census(dataset = "CA01", 
                                                   regions = list(CSD = 2466023), 
                                                   level = "CSD",
                                                   vectors = c("Total" = "v_CA01_402",
                                                               "Immigrants" = "v_CA01_406"))


total_immigrants_mtl2001_aspercent <- total_immigrants_mtl2001 |> 
  mutate(PercentImmigrant = Immigrants/Total*100)


# 1996 

View(list_census_vectors("CA1996"))

total_immigrants_mtl1996 <- cancensus::get_census(dataset = "CA1996", 
                                                   regions = list(CSD = 2466023), 
                                                   level = "CSD",
                                                   vectors = c("Total" = "v_CA1996_125",
                                                               "Immigrants" = "v_CA1996_128"))

total_immigrants_mtl1996_aspercent <- total_immigrants_mtl1996 |> 
  mutate(PercentImmigrant = Immigrants/Total*100)


# visualise change in immigrant population 
#combine tibbles

combined_immigrantdata_mtl <- bind_rows(total_immigrants_mtl_as_percent, total_immigrants_mtl2016_aspercent, total_immigrants_mtl2006_aspercent, total_immigrants_mtl2001_aspercent, total_immigrants_mtl1996_aspercent)

#add years

combined_immigrantdata_mtl <- combined_immigrantdata_mtl |> 
  mutate(year = c(2021, 2016, 2006, 2001, 1996)) |> 
  mutate(Region = "Montreal")


#########issues with Montreal Data = unable to combine with province and laval



 

# Visible Minority  -----------------------------------------------------

visible_minority <- cancensus::get_census(dataset = "CA21", 
                                          regions = list(CSD = 2465005), 
                                          level = "CSD",
                                          vectors = c("TotalPop" = "v_CA21_4872",
                                                      "TotalVisibleMinority" = "v_CA21_4875",
                                                      "SouthAsian" = "v_CA21_4878",
                                                      "Chinese" = "v_CA21_4881",
                                                      "Black" = "v_CA21_4884",
                                                      "Filipino" = "v_CA21_4887",
                                                      "Arab" = "v_CA21_4890",
                                                      "LatinAmerican" = "v_CA21_4893",
                                                      "SoutheastAsian" = "v_CA21_4896",
                                                      "WestAsian" = "v_CA21_4899",
                                                      "Korean" = "v_CA21_4902",
                                                      "Japanese" = "v_CA21_4905",
                                                      "n.i.e" = "v_CA21_4908",
                                                      "NotVisibleMinorty" = "v_CA21_4914",
                                                      "MultipleVisibleMinorities" = "v_CA21_4911" ))
 
percent_visible_minority <- visible_minority |> mutate(PercentVisibleMinority = TotalVisibleMinority/TotalPop)
 
#pivot 

visible_minority_tidy <- pivot_longer(visible_minority, cols = -c(GeoUID:CMA_UID), names_to = "Category", values_to = "Count")

#filter data to remove total columns

filtered_visible_minority_tidy <- visible_minority_tidy |> 
  filter(!(Category %in% c("MultipleVisibleMinorities", "NotVisibleMinorty", "TotalVisibleMinority", "TotalPop")))


# make plot
ggplot(filtered_visible_minority_tidy, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  labs(title = "Visible Minority Distribution", x = "Category", y = "Count", fill = "Category") +
  theme(legend.position = "none")                                


# compare to Quebec

visible_minority_Queb <- cancensus::get_census(dataset = "CA21", 
                                          regions = list(PR = 24), 
                                          level = "PR",
                                          vectors = c("TotalPop" = "v_CA21_4872",
                                                      "TotalVisibleMinority" = "v_CA21_4875",
                                                      "SouthAsian" = "v_CA21_4878",
                                                      "Chinese" = "v_CA21_4881",
                                                      "Black" = "v_CA21_4884",
                                                      "Filipino" = "v_CA21_4887",
                                                      "Arab" = "v_CA21_4890",
                                                      "LatinAmerican" = "v_CA21_4893",
                                                      "SoutheastAsian" = "v_CA21_4896",
                                                      "WestAsian" = "v_CA21_4899",
                                                      "Korean" = "v_CA21_4902",
                                                      "Japanese" = "v_CA21_4905",
                                                      "n.i.e" = "v_CA21_4908",
                                                      "NotVisibleMinorty" = "v_CA21_4914",
                                                      "MultipleVisibleMinorities" = "v_CA21_4911" ))

percent_visible_minority_Queb <- visible_minority_Queb |> mutate(PercentVisibleMinority = TotalVisibleMinority/TotalPop)









# Annual Growth Rate - Housing - In Pop Growth Section of Text --------------------------------------------------
# using Total private dwellings
## in text = amount of total private dwellings 

PrivateDwellings2021 <- cancensus::get_census(dataset = "CA21",
                                            regions = list(CSD = 2465005), 
                                            level = "CSD",
                                            vectors = c("Total_Private_Dwellings" = "v_CA21_4"))

View(list_census_vectors("CA16"))

PrivateDwellings2016 <- cancensus::get_census(dataset = "CA16",
                                              regions = list(CSD = 2465005), 
                                              level = "CSD",
                                              vectors = c("Total_Private_Dwellings" = "v_CA16_404"))

# look at 2021 value
privatedwellingsLaval <- PrivateDwellings2021[1, 'Total_Private_Dwellings']

print(privatedwellingsLaval)

# look at 2016 value

privatedwellingsLaval2016 <- PrivateDwellings2016[1,'Total_Private_Dwellings']
print(privatedwellingsLaval2016)

#increase in total private dwellings as count

dwellingincrease <- privatedwellingsLaval - privatedwellingsLaval2016
print(dwellingincrease)

# increase as percent

dwellingincreasepercent <- (privatedwellingsLaval - privatedwellingsLaval2016)/privatedwellingsLaval2016*100
print(dwellingincreasepercent)



# compare to Province

PrivateDwellings2021_qc <- cancensus::get_census(dataset = "CA21",
                                              regions = list(PR = 24), 
                                              level = "PR",
                                              vectors = c("Total_Private_Dwellings" = "v_CA21_4"))

View(list_census_vectors("CA16"))

PrivateDwellings2016_qc <- cancensus::get_census(dataset = "CA16",
                                              regions = list(PR = 24), 
                                              level = "PR",
                                              vectors = c("Total_Private_Dwellings" = "v_CA16_404"))








### Population Evolution 1966 - 2041 ----------------------------------------------------------

#insert data set from folder sourced from different census' years

pop_evolution <- read.csv("/Users/bridgetbuglioni/Documents/GitHub/PESLaval/data/Population Evolution.csv", skip = 3) |> 
  tibble::as_tibble()
 

str(pop_evolution)  

#choose only data to plot

pop_evolution_tidy <- pop_evolution |> 
  slice(1:16)

ggplot(data = pop_evolution_tidy, aes(x = Year, y = Population))+
  geom_point()+
  ylim(0, max(pop_evolution_tidy$Population))


# the last four points need to be distinct because those are the future projections
pop_evolution_tidy <- pop_evolution_tidy %>%
  mutate(PointType = ifelse(row_number() > n() - 4, "Projection", "Estimate"))

# allows for the end of the line to be dotted based on projection points
solid_data <- pop_evolution_tidy %>% filter(PointType == "Estimate" | row_number() == n() - 4)
dotted_data <- pop_evolution_tidy %>% filter(PointType == "Projection" | row_number() == n() - 4)

# create the visual 
ggplot(data = pop_evolution_tidy, aes(x = Year, y = Population)) +
  geom_point(aes(color = PointType), size = 3) +
  geom_line(data = solid_data, aes(x = Year, y = Population, group = 1), linetype = "dotted") +  # Solid line for initial points
  geom_line(data = dotted_data, aes(x = Year, y = Population, group = 1), linetype = "solid") +  # Dotted line for last four points
  ylim(0, max(pop_evolution_tidy$Population)) +
  scale_color_manual(values = c("Projection" = "pink", "Estimate" = "black")) +
  labs(color = "Point Type", title = "Evolution of the Laval Population 1966 to 2041")




# Births -----------------------------------------------------------------------
# this is the data source, filter to just look at Laval
### https://statistique.quebec.ca/fr/document/naissances-regions-administratives/tableau/naissances-deces-accroissement-naturel-mariages-par-region-administrative-quebec#tri_phe=10&tri_ra=13

#insert downloaded data set from linked source

Laval_Births <- read.csv("/Users/bridgetbuglioni/Documents/GitHub/PESLaval/data/Laval_Births.csv", skip = 5) |> 
  tibble::as_tibble()

#convert row with data to numeric
Laval_Births[3, ] <- lapply(Laval_Births[3, ], function(x) as.numeric(as.character(x)))

str(Laval_Births_sliced)

# look at just the birth data

Laval_Births_sliced <- Laval_Births |> 
  slice(3)


Laval_Births_sliced_long <- Laval_Births_sliced |> 
  pivot_longer(-c(X:Code.RA))

str(Laval_Births_sliced_long$value)
# Ensure the 'value' column is numeric
Laval_Births_sliced_long$value <- as.numeric(as.character(Laval_Births_sliced_long$value))

str(Laval_Births_sliced_long)

# Get numbner of births 2023
births2023 <- Laval_Births_sliced_long[39, 'value']

# see the value 
print(births2023)

# Inspect the specific rows 35 to 39
subset_values <- Laval_Births_sliced_long[35:39, 'value']
print(subset_values)

numeric_values <- as.numeric(subset_values$value)
print(subset_values)

# Calculate the mean of the numeric vector
averagebirth_past5yrs <- mean(numeric_values, na.rm = TRUE)

# Print the result - average births over last five years 
print(averagebirth_past5yrs)



