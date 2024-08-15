### Child development ##########################################################

source("R/01_startup.R")

# # Place of Birth Outside Canada ------------------------------------------------
# # Children age 0-14, immigrated between 2006 and 2021
# 
# immigrant_children <- read_csv("data/axe1/children/0_19_Children_Place of birth and period of immigration.csv", skip = 12) |> 
#   tibble::as_tibble() 
# 
# # Remove commas and convert to numeric
# immigrant_children$`Total immigrant population 0 to 14` <- 
#   as.numeric(gsub(",", "", immigrant_children$`Total immigrant population 0 to 14`))
# 
# # select desired rows only
# immigrant_children_filtered <- immigrant_children |> 
#   slice(2:3,7:10)
# 
# ggplot(data = immigrant_children_filtered, aes(x = `Birth Place`, y = Total_0to19)) +
#   geom_col() +
#   labs(title = "Immigrant Children age 0-19 Place of Birth", x = "Country", y = "Count")
# 
# # percent of immigrant children vs born in Canada:
# percent_immigrant_children <- data.frame(`Total Children` = c(101245), 
#                                          `Immigrant Children` = c(11440))
# 
# percent_immigrant_children <- percent_immigrant_children |> 
#   mutate(PercentImmigrant = Immigrant.Children/Total.Children*100)
# 
# 
# # Language -------------------------------------------------------------
#   
# language_children <- read_csv("/data/axe1/children/children mother tongue.csv", skip = 10) |> 
#   tibble::as_tibble() 
# 
# #convert characters to numbers
# language_children$Total <- as.numeric(gsub(",", "", language_children$Total))
# language_children_total <- language_children |> 
#   slice(2)
# 
# # combine some categories
# language_children_total_grouped <- language_children_total |> 
#   mutate(`Non-Official` = perNon_official + perMultipleOther,
#          `English and Other` = perEnglish + perEng_andother,
#          `Both Official and other` = perBoth + per_EnglishFrench_andother,
#          `French and Other` = perFrench + perFrench_andother)
# 
# 
# language_children_total_grouped_long <- language_children_total_grouped |> 
#   pivot_longer(-c(Age)) |> 
#   mutate(Geography = "Laval")
# 
# #select data
# language_children_total_grouped_long_sliced <- language_children_total_grouped_long |> 
#   slice(18:21)
# 
# # so columns are in the correct order
# language_children_total_grouped_long_sliced$name <- 
#   factor(language_children_total_grouped_long_sliced$name, levels = unique(language_children_total_grouped_long_sliced$name))
# 
# 
# # stacked visualisation
# ggplot(data = language_children_total_grouped_long_sliced, aes(x = Geography, y = value, fill = name)) +
#   geom_col()
# 
# ### compare to quebec
# # download and insert data file for province
# language_children_queb <- read_csv("/data/axe1/children/children mother tongue quebec.csv", skip = 10) |> 
#   tibble::as_tibble() 
# 
# #convert characters to numbers
# language_children_queb$Total <- as.numeric(gsub(",", "", language_children_queb$Total))
# 
# language_children_queb_total <- language_children_queb |> 
#   slice(2)
# 
# 
# # combine some categories
# language_children_queb_total_grouped <- language_children_queb_total |> 
#   mutate(`Non-Official` = perNon_official + perMultipleOther,
#          `English and Other` = perEnglish + perEng_andother,
#          `Both Official and other` = perBoth + per_EnglishFrench_andother, 
#          `French and Other` = perFrench + perFrench_andother)
# 
# language_children_queb_total_grouped_long <- language_children_queb_total_grouped |> 
#   pivot_longer(-c(Age)) |> 
#   mutate(Geography = "Quebec")
# 
# 
# #select data
# language_children_queb_total_grouped_long_sliced <- language_children_queb_total_grouped_long  |> 
#   slice(18:21) 
# 
# # so columns are in the correct order
# 
# language_children_queb_total_grouped_long_sliced$name <- 
#   factor(language_children_queb_total_grouped_long_sliced$name, levels = unique(language_children_queb_total_grouped_long_sliced$name))
# 
# #check what data looks like
# ggplot(data = language_children_queb_total_grouped_long_sliced, aes(x = name, y = value)) +
#   geom_col()
# 
# # combine queb and laval
# 
# combined_language <- bind_rows(language_children_total_grouped_long_sliced,language_children_queb_total_grouped_long_sliced)
# 
# 
# # Check the class of combined_language
# print(class(combined_language))
# 
# # Inspect the structure of combined_language
# print(str(combined_language))
# 
# 
# ## plot with bars stacked vertically 
# ggplot(data = combined_language, aes(x = Geography, y = value, fill = name)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Children 0 to 19 Mother Tongue", y = "Percent", x = "Response") +
#   geom_text(aes(label = paste0(value, "%")), position = position_stack(vjust = 0.5), color = "white")
# 
#  
# # Age and Gender ---------------------------------------------------------------
# 
# # Get all the age/gender variables for the 2021 census
# children_pop_dist_vector <- c("Male Under 1" = "v_CA21_18",
#                      "Female Under 1" = "v_CA21_19",
#                      "Male 1" = "v_CA21_21",
#                      "Female 1" = "v_CA21_22",
#                      "Male 2" = "v_CA21_24",
#                      "Female 2" = "v_CA21_25",
#                      "Male 3" = "v_CA21_27",
#                      "Female 3" = "v_CA21_28",
#                      "Male 4" = "v_CA21_30",
#                      "Female 4" = "v_CA21_31",
#                      "Male 5" = "v_CA21_36",
#                      "Female 5" = "v_CA21_37",
#                      "Male 6" = "v_CA21_39",
#                      "Female 6" = "v_CA21_40",
#                      "Male 7" = "v_CA21_42",
#                      "Female 7" = "v_CA21_43",
#                      "Male 8" = "v_CA21_45",
#                      "Female 8" = "v_CA21_46",
#                      "Male 9" = "v_CA21_48",
#                      "Female 9" = "v_CA21_49",
#                      "Male 10" = "v_CA21_54",
#                      "Female 10" = "v_CA21_55",
#                      "Male 11" = "v_CA21_57",
#                      "Female 11" = "v_CA21_58",
#                      "Male 12" = "v_CA21_60",
#                      "Female 12" = "v_CA21_61",
#                      "Male 13" = "v_CA21_63",
#                      "Female 13" = "v_CA21_64",
#                      "Male 14" = "v_CA21_66",
#                      "Female 14" = "v_CA21_67",
#                      "Male 15" = "v_CA21_75",
#                      "Female 15" = "v_CA21_76",
#                      "Male 16" = "v_CA21_78",
#                      "Female 16" = "v_CA21_79",
#                      "Male 17" = "v_CA21_81",
#                      "Female 17" = "v_CA21_82",
#                      "Male 18" = "v_CA21_84",
#                      "Female 18" = "v_CA21_85",
#                      "Male 19" = "v_CA21_87",
#                      "Female 19" = "v_CA21_88")
# 
# # Vector for sorting
# children_sort_vec <- c("Under 1", "1", "2", "3", "4", "5", "6", 
#               "7", "8", "9", "10", "11", "12", "13", 
#               "14", "15", "16", "17", "18", "19")
# 
# 
# # Get these variables for Laval
# children_pop_dist_laval  <- get_census(dataset = "CA21", 
#                               regions = list(CSD = 2465005), 
#                               level = "CSD",
#                               vectors = children_pop_dist_vector)
# 
# # Get the variables for the province
# children_pop_dist_qc  <- get_census(dataset = "CA21", 
#                            regions = list(PR = 24), 
#                            level = "PR",
#                            vectors = children_pop_dist_vector)
# 
# # Merge the two datasets
# children_pop_dist <-
#   bind_rows(children_pop_dist_laval, children_pop_dist_qc) |> 
#   mutate(name = c("Laval", "Quebec"), .before = GeoUID) |> 
#   select(-c(GeoUID:CMA_UID, C_UID)) |> 
#   pivot_longer(-name, names_to = "category") |> 
#   # Split gender off into its own variable
#   mutate(gender = str_extract(category, "(Male|Female)"),
#          category = str_remove(category, "(Male |Female )")) |> 
#   group_by(name, gender) |> 
#   mutate(pct = value / sum(value)) |> 
#   ungroup() |> 
#   # Make female values negative to facilitate easier population pyramids
#   mutate(pct = if_else(gender == "Female", pct * -1, pct)) |> 
#   mutate(category = factor(category, levels = children_sort_vec))
# 
# children_pop_dist |> 
#   ggplot(aes(x = pct, y = category, fill = gender)) +
#   geom_col() +
#   facet_wrap(~name, nrow = 1) +
#   scale_fill_manual(values = c("Female" = "pink", "Male" = "blue")) +
#   scale_x_continuous(breaks = -2:2 * 0.04,
#                      labels = c("8 %", "4 %", "0", "4 %", "8 %")) +
#   theme(legend.position = "bottom")
# 
# 
# # gender distribution
# gender_distribution_laval <- children_pop_dist_laval |> 
#   pivot_longer(-c(GeoUID:CMA_UID)) |> 
#   
# # Filter columns for males and females
# 
# male_columns <- grep("Male", names(children_pop_dist_laval), value = TRUE) 
# female_columns <- grep("Female", names(children_pop_dist_laval), value = TRUE)
# 
# # Sum counts for males and females
# total_male_count <- children_pop_dist_laval %>%
#   select(all_of(male_columns)) %>%
#   rowSums(na.rm = TRUE)
# 
# total_female_count <- children_pop_dist_laval %>%
#   select(all_of(female_columns)) %>%
#   rowSums(na.rm = TRUE)
# 
# # Calculate percentage of males and females
# total_population <- total_male_count + total_female_count
# percentage_male <- (total_male_count / total_population) * 100
# percentage_female <- (total_female_count / total_population) * 100
# 
# # Create a data frame with the results
# gender_distribution <- data.frame(Age_Group = children_sort_vec,
#                                   Percentage_Male = percentage_male,
#                                   Percentage_Female = percentage_female)
# 
# 
# 
# # see the distribution of children by age group -- results not interesting 
# children_dist <- cancensus::get_census(dataset = "CA21", 
#                                               regions = list(CSD = 2465005),
#                                               level = "CSD",
#                                               vectors = c("Total 0-14" = "v_CA21_11",
#                                                        "Total 0-4" = "v_CA21_14",
#                                                        "Total 5-9" = "v_CA21_32",
#                                                        "Total 10-14" = "v_CA21_50",
#                                                        "Total 15-19" = "v_CA21_71"))
# 
# children_dist <- children_dist |> 
#   mutate(`Total 0-19` = `Total 0-14` + `Total 15-19`) |> 
#   select(-`Total 0-14`) 
# 
# children_dist_percent <- children_dist |> 
#   mutate(across(`Total 0-4`:`Total 15-19`, ~. /`Total 0-19` *100)) |> 
#   select(-`Total 0-19`) 
# 
# children_dist_percent <- children_dist_percent |> 
#   pivot_longer(-c(GeoUID:CMA_UID)) 
#  
# 
# ggplot(data = children_dist_percent, aes(x = name, y = value, fill = name)) +
#   geom_col()
# 
# 
# 
# # quebec
# children_dist_qc <- cancensus::get_census(dataset = "CA21", 
#                                        regions = list(PR = 24),
#                                        level = "PR",
#                                        vectors = c("Total 0-14" = "v_CA21_11",
#                                                    "Total 0-4" = "v_CA21_14",
#                                                    "Total 5-9" = "v_CA21_32",
#                                                    "Total 10-14" = "v_CA21_50",
#                                                    "Total 15-19" = "v_CA21_71"))   
# 
# children_dist_qc <- children_dist_qc |> 
#   mutate(`Total 0-19` = `Total 0-14` + `Total 15-19`) |> 
#   select(-`Total 0-14`) 
# 
# children_dist_percent_qc <- children_dist_qc |> 
#   mutate(across(`Total 0-4`:`Total 15-19`, ~. /`Total 0-19` *100)) |> 
#   select(-`Total 0-19`) 
# 
# children_dist_percent_qc <- children_dist_percent_qc |> 
#   pivot_longer(-c(GeoUID:C_UID)) 
# 
# ggplot(data = children_dist_percent_qc, aes(x = name, y = value, fill = name)) +
#   geom_col()
# 
# 
### Children Activity - SCREEN TIME -------------------------------------------
# more than 2 hours per day
# data from here : https://transformation-numerique.ulaval.ca/enquetes-et-mesures/netendances/la-famille-numerique-2023/

#data frame of young children screen time age 6-12
youngchildren_screentime <- data.frame(`Total` = c(100),
                                       `Plus de 15` = c(20),
                                       `Entre 11 et 15` = c(13),
                                       `Moins de 10`= c(59),
                                       `Jamais`= c(3),
                                       `Ne savent pas` = c(5))

youngchildren_screentime <- youngchildren_screentime |>
  pivot_longer(-c(Total))

youngchildren_screentime <- youngchildren_screentime |>
  mutate("Age" = "6-12")

#data frame of children screen time age 13 - 17
adolescant_screentime <- data.frame(`Total` = c(100),
                                    `Plus de 15` = c(45),
                                    `Entre 11 et 15` = c(14),
                                    `Moins de 10`= c(35),
                                    `Jamais`= c(2),
                                    `Ne savent pas` = c(4))

adolescant_screentime <- adolescant_screentime |>
  pivot_longer(-c(Total))

adolescant_screentime <- adolescant_screentime |>
  mutate("Age" = "13-17")

# combine into one df
combined_screentime <- bind_rows(youngchildren_screentime, adolescant_screentime)
combined_screentime$name <- gsub("\\.", " ", combined_screentime$name)

combined_screentime$name <-
  factor(combined_screentime$name, levels = unique(combined_screentime$name))

combined_screentime$Age <- factor(combined_screentime$Age,
                                  levels = c("6-12", "13-17"))

children_internet_usage <- 
ggplot(data = combined_screentime, aes(x = Age, y = rev(value), fill = name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = curbcut_colors$left_5$fill[6:2],
                    guide = guide_legend(title.position = "top", label.position = "bottom", nrow = 1)) +
  geom_text(aes(label = paste0(rev(value), " %")), position = position_stack(vjust = 0.5), color = "white") +
  labs(x = "Âge", y = "Enfants (%)", fill = "Heures par semaine", 
       title = NULL) +
  gg_cc_theme_no_sf


ggplot2::ggsave(filename = here::here("output/axe1/children/children_internet_usage.pdf"), 
                plot = children_internet_usage, width = 3, height = 6, bg = "transparent")

# Kindergarten children vulnerability --------------------------------------------
# data = quebec survery on child development in kindergarten 2022

children_vulnerability <- data.frame(`Region` = c("Laval"),
                                     `Santé physique et bien-être` = c(11.6),
                                     `Compétences sociales` = c(12.8),
                                     `Maturité affective` = c(12.7),
                                     `Développement cognitif et langagier` = c(14.4),
                                     `Habiletés de communication et connaissances générales` = c(17.1),
                                     `Au moins un domaine` = c(34.0))
children_vulnerability <- children_vulnerability |>
  pivot_longer(-c(Region))
# children_vulnerability$value <- convert_pct(children_vulnerability$value / 100)
# children_vulnerability$value[[6]] <- "34,0 %"

children_vulnerability_qc <- data.frame(`Region` = c("Québec"),
                                         `Santé physique et bien-être` = c(10.3),
                                         `Compétences sociales` = c(10.6),
                                         `Maturité affective` = c(11.7),
                                         `Développement cognitif et langagier` = c(12.1),
                                         `Habiletés de communication et connaissances générales` = c(11.5),
                                         `Au moins un domaine` = c(28.7))
children_vulnerability_qc <- children_vulnerability_qc |>
  pivot_longer(-c(Region))
# children_vulnerability_qc$value <- convert_pct(children_vulnerability_qc$value / 100)

combined_child_vulnerability <- bind_rows(children_vulnerability, children_vulnerability_qc)
combined_child_vulnerability$name <- gsub("\\.", " ", combined_child_vulnerability$name)

children_vulnerability_plot <- 
ggplot(data = combined_child_vulnerability, aes(x = name, y = value, fill = Region))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = NULL, x = NULL, y = "Enfants (%)") + 
  gg_cc_theme_no_sf +
  geom_text(aes(label = convert_pct(value / 100)),
            position = position_dodge(width = 0.9),
            vjust = 2, color = "white") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 16)) +
  scale_fill_manual(values = c("Laval" = color_theme("greenecology"), "Québec" = color_theme("blueexplorer"))) +
  theme(legend.title = element_blank())

ggplot2::ggsave(filename = here::here("output/axe1/children/children_vulnerability_plot.pdf"), 
                plot = children_vulnerability_plot, width = 6, height = 3, bg = "transparent")

# # Vulnerability over the years 
# female_change_vulnerability <- data.frame(`Gender` = c("Female"),
#                                           `Vulnerable_2012` = c(21.3),
#                                           `Vulnerable_2017` = c(23.0),
#                                           `Vulnerable_2022` = c(25.2))
#                                   
# male_change_vulnerability <- data.frame(`Gender` = c("Male"),
#                                           `Vulnerable_2012` = c(37.8),
#                                           `Vulnerable_2017` = c(39.8),
#                                           `Vulnerable_2022` = c(42.7))
# 
# female_change_vulnerability <- female_change_vulnerability |> 
#   pivot_longer(-c(Gender))
# 
# male_change_vulnerability <- male_change_vulnerability |> 
#   pivot_longer(-c(Gender))
# 
# combined_child_vulnerability_change <- bind_rows(male_change_vulnerability, female_change_vulnerability)
# 
# ggplot(data = combined_child_vulnerability_change, aes(x = name, y = value, fill = Gender))+
#   geom_bar(stat = "identity", position = "dodge") +
#   scale_fill_manual(values = c("Female" = "pink", "Male" = "blue")) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   labs(title = "Vulnerability in Kindergarten Age Children", x = "Year", y = "Percent")


# Save --------------------------------------------------------------------

qs::qsavem(children_vulnerability_plot, children_internet_usage, 
           file = "data/axe1/children.qsm")
