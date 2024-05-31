#Loading libraries
source("R/01_startup.R")
library(sf)

#Setting CensusMapper API Key because it won't save
set_cancensus_api_key("CensusMapper_4308d496f011429cf814385050f083dc")

#Grabbing all cancensus vector
can21 <- list_census_vectors(dataset = "CA21")

#Caching census data to reduce amount of calls and speed up process.
#Personal use only, change the folder to your own folder if you want to use it
set_cancensus_cache_path("D:/McGill/can_cache", install = TRUE, overwrite = TRUE)

#Grabbing Laval's shapefile by census tract
laval_ct <- cancensus::get_census(dataset = "CA21", 
                                  regions = list(CSD = 2465005), 
                                  level = "CT", 
                                  geo_format = "sf")
# Personal Income Brackets ---------------------------------------------------------
#Pulling all vectors for total individual income
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
indtotinc_lvl_total  <- get_census(dataset = "CA21", 
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
  rename("< $10,000" = "under_10", "$10-19,999" = "10_20", "$20-29,999" = "20_30",
         "$30-39,999" = "30_40", "$40-$49,999" = "40_50", "$50-59,999" = "50_60",
         "$60-69,999" = "60_70", "$70-79,999" = "70_80", "$80-89,999" = "80_90",
         "$90-99,999" = "90_100", "$100-150,000" = "100_150", "> $150,000" = "over_150") |> 
  pivot_longer(cols = -Geography, names_to = "Bracket", values_to = "Value") |> 
  mutate(Bracket = factor(Bracket, levels = c("< $10,000", "$10-19,999", "$20-29,999",
                                              "$30-39,999", "$40-$49,999", "$50-59,999",
                                              "$60-69,999", "$70-79,999", "$80-89,999",
                                              "$90-99,999", "$100-150,000", "> $150,000")))

#Grouped bar graph for individual brackets
ggplot(ind_total_bar, aes(x = Bracket, y = Value, fill = Geography)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Total Income Brackets in Laval in 2020 for Individuals Age 15 and Over in Private Households",
       x = "", y = "Value") +
  scale_fill_manual(values = c("Laval" = "royalblue2", 
                               "Men" = "indianred", 
                               "Women" = "gold2"),
                    name = "Status",
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

#Preparing table for bar graph for total income brackets by every $50,000 
ind_total_bar_50 <- ind_total_income |> 
  select(-total_count, -wo_total_income, -w_total_income, -over_100) |> 
  mutate("< $50,000" = under_10 + `10_20` + `20_30` + `30_40` + `40_50`,
         "$50-99,999" = `50_60` + `60_70` + `70_80` + `80_90` + `90_100`,
         "$100-150,000" = `100_150`, "> $150,000" = over_150) |> 
  select(-under_10, -`10_20`, -`20_30`, -`30_40`, -`40_50`, -`50_60`,
         -`60_70`, -`70_80`, -`80_90`, -`90_100`, -`100_150`, -over_150) |> 
  pivot_longer(cols = -Geography, names_to = "Bracket", values_to = "Value") |>
  mutate(Bracket = factor(Bracket, levels = c("< $50,000", "$50-99,999",
                                              "$100-150,000", "> $150,000")))

#Bar graph for total individual income by $50,000 intervals
ggplot(ind_total_bar_50, aes(x = Bracket, y = Value, fill = Geography)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Total Income Brackets in Laval in 2020 for Individuals Age 15 and Over in Private Households",
       x = "", y = "Value") +
  scale_fill_manual(values = c("Laval" = "royalblue2", 
                               "Men" = "indianred", 
                               "Women" = "gold2"),
                    name = "Status",
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Individual After-Tax ----------------------------------------------------
#Pulling all vectors for after-tax (at) individual income
indatinc_vectors <- can21$vector[which(can21$vector == "v_CA21_713"):which(can21$vector == "v_CA21_760")]

#Naming the rows for the vectors
indatinc_names <- c("total_count", "wo_at_income", "w_at_income", "under_10",
                      "10_20", "20_30", "30_40", "40_50", "50_60", "60_70", "70_80",
                      "80_90", "90_100", "over_100", "100_125", "over_125")

#Filter for vectors for Total
indat21_vectors_total <- can21 |> 
  filter(vector %in% indatinc_vectors & type == "Total") |> 
  pull(vector) |> 
  set_names(indatinc_names)

#Filter for vectors for men
indat21_vectors_men <- can21 |> 
  filter(vector %in% indatinc_vectors & type == "Male") |> 
  pull(vector) |> 
  set_names(indatinc_names)

#Filter for vectors for women
indat21_vectors_women <- can21 |> 
  filter(vector %in% indatinc_vectors & type == "Female") |> 
  pull(vector) |> 
  set_names(indatinc_names)

#Pull individual AT income brackets for Laval
indat21_lvl_total  <- get_census(dataset = "CA21", 
                                   regions = list(CSD = 2465005), 
                                   level = "CSD",
                                   vectors = indat21_vectors_total) |> 
  select(all_of(indatinc_names)) |> 
  mutate(Geography = c("Laval")) |> 
  select(Geography, everything())

#Pull individual AT income brackets for men
indat21_lvl_men  <- get_census(dataset = "CA21", 
                                 regions = list(CSD = 2465005), 
                                 level = "CSD",
                                 vectors = indat21_vectors_men) |> 
  select(all_of(indatinc_names)) |> 
  mutate(Geography = c("Men")) |> 
  select(Geography, everything())

#Pull individual AT income brackets for women
indat21_lvl_women  <- get_census(dataset = "CA21", 
                                   regions = list(CSD = 2465005), 
                                   level = "CSD",
                                   vectors = indat21_vectors_women) |> 
  select(all_of(indatinc_names)) |> 
  mutate(Geography = c("Women")) |> 
  select(Geography, everything())

#Bind tables together
ind_at_income <- bind_rows(indat21_lvl_total, indat21_lvl_men, indat21_lvl_women)

#Prepping table for bar graph
ind_at_bar <- ind_at_income |> 
  select(-total_count, -wo_at_income, -w_at_income, -over_100) |> 
  rename("< $10,000" = "under_10", "$10-19,999" = "10_20", "$20-29,999" = "20_30",
         "$30-39,999" = "30_40", "$40-$49,999" = "40_50", "$50-59,999" = "50_60",
         "$60-69,999" = "60_70", "$70-79,999" = "70_80", "$80-89,999" = "80_90",
         "$90-99,999" = "90_100", "$100-125,000" = "100_125", "> $125,000" = "over_125") |> 
  pivot_longer(cols = -Geography, names_to = "Bracket", values_to = "Value") |> 
  mutate(Bracket = factor(Bracket, levels = c("< $10,000", "$10-19,999", "$20-29,999",
                                              "$30-39,999", "$40-$49,999", "$50-59,999",
                                              "$60-69,999", "$70-79,999", "$80-89,999",
                                              "$90-99,999", "$100-125,000", "> $125,000")))

#Bar Graph for individual AT income
ggplot(ind_at_bar, aes(x = Bracket, y = Value, fill = Geography)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "After-Tax Income Brackets in Laval in 2020 for Individuals Age 15 and Over in Private Households",
       x = "", y = "Value") +
  scale_fill_manual(values = c("Laval" = "royalblue2", 
                               "Men" = "indianred", 
                               "Women" = "gold2"),
                    name = "Status",
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Individual Employment Brackets ------------------------------------------
#Grabbing vectors for employment brackets
indeminc_vectors <- can21$vector[which(can21$vector == "v_CA21_761"):which(can21$vector == "v_CA21_811")]

#Naming the rows for the vectors
indeminc_names <- c("total_count", "wo_em_income", "w_em_income", "under_5", "5_10",
                    "10_20", "20_30", "30_40", "40_50", "50_60", "60_70", "70_80",
                    "80_90", "90_100", "over_100", "100_125", "over_125")

#Filter for vectors for Total
indeminc_vectors_total <- can21 |> 
  filter(vector %in% indeminc_vectors & type == "Total") |> 
  pull(vector) |> 
  set_names(indeminc_names)

#Filter for vectors for men
indeminc_vectors_men <- can21 |> 
  filter(vector %in% indeminc_vectors & type == "Male") |> 
  pull(vector) |> 
  set_names(indeminc_names)

#Filter for vectors for women
indeminc_vectors_women <- can21 |> 
  filter(vector %in% indeminc_vectors & type == "Female") |> 
  pull(vector) |> 
  set_names(indeminc_names)

#Pull individual employment income brackets for Laval
indeminc_lvl_total  <- get_census(dataset = "CA21", 
                                 regions = list(CSD = 2465005), 
                                 level = "CSD",
                                 vectors = indeminc_vectors_total) |> 
  select(all_of(indeminc_names)) |> 
  mutate(Geography = c("Laval")) |> 
  select(Geography, everything())

#Pull individual employment income brackets for men
indeminc_lvl_men  <- get_census(dataset = "CA21", 
                               regions = list(CSD = 2465005), 
                               level = "CSD",
                               vectors = indeminc_vectors_men) |> 
  select(all_of(indeminc_names)) |> 
  mutate(Geography = c("Men")) |> 
  select(Geography, everything())

#Pull individual employment income brackets for women
indeminc_lvl_women  <- get_census(dataset = "CA21", 
                                 regions = list(CSD = 2465005), 
                                 level = "CSD",
                                 vectors = indeminc_vectors_women) |> 
  select(all_of(indeminc_names)) |> 
  mutate(Geography = c("Women")) |> 
  select(Geography, everything())

#Bind tables together
indeminc <- bind_rows(indeminc_lvl_total, indeminc_lvl_men, indeminc_lvl_women)

#Prepare table for normal interval bar graph
indeminc_bar <- indeminc |> 
  mutate("< $10,000" = under_5 + `5_10` + `10_20` + `20_30` + `30_40` + `40_50`,
         "$50-99,999" = `50_60` + `60_70` + `70_80` + `80_90` + `90_100`,
         "$100-150,000" = `100_150`, "> $150,000" = over_150) |> 
  rename("$10-19,999" = "10_20", "$20-29,999" = "20_30", "$30-39,999" = "30_40",
         "$40-$49,999" = "40_50", "$50-59,999" = "50_60", "$60-69,999" = "60_70",
         "$70-79,999" = "70_80", "$80-89,999" = "80_90", "$90-99,999" = "90_100",
         "$100-125,000" = "100_125", "> $125,000" = "over_125") |> 
  select(-total_count, -w_em_income, -wo_em_income, -under_5, -`5_10`, -over_100) |> 
  pivot_longer(cols = -Geography, names_to = "Bracket", values_to = "Count") |> 
  mutate(Bracket = factor(Bracket, levels = c("< $10,000", "$10-19,999", "$20-29,999",
                                              "$30-39,999", "$40-$49,999", "$50-59,999",
                                              "$60-69,999", "$70-79,999", "$80-89,999",
                                              "$90-99,999", "$100-125,000", "> $125,000")))

#Bar Graph for individual employment income
ggplot(indeminc_bar, aes(x = Bracket, y = Count, fill = Geography)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Employment Income Brackets in Laval in 2020 for Individuals Age 15 and Over in Private Households",
       x = "", y = "Value") +
  scale_fill_manual(values = c("Laval" = "royalblue2", 
                               "Men" = "indianred", 
                               "Women" = "gold2"),
                    name = "Status",
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Total Household Income --------------------------------------------------
#Setting the names for the vectors
tot_hh_names <- c("Total", "< $5,000", "$5-9,999", "$10-14,999", "$15-19,999",
                  "$20-24,999", "$25-29,999", "$30-34,999", "$35-39,999", "$40-44,999",
                  "$45-49,999", "$50-59,999", "$60-69,999", "$70-79,999", "$80-89,999",
                  "$90-99,999", "=> $100,000", "$100-124,999", "$125-149,999",
                  "$150-199,999", "=> $200,000")

#Grabbing vectors for total household income and renaming them with tot_hh_names
tot_hh_vectors <- can21$vector[which(can21$vector == "v_CA21_923"):which(can21$vector == "v_CA21_943")] |> 
  set_names(tot_hh_names)

#Grabbing total household income
tot_hh_lvl  <- get_census(dataset = "CA21", 
                                  regions = list(CSD = 2465005), 
                                  level = "CSD",
                                  vectors = tot_hh_vectors) |> 
  select(all_of(tot_hh_names)) |> 
  select(everything())

#Preparing the bar graph for "regular" intervals
tot_hh_bar <- tot_hh_lvl |> 
  mutate("< $10,000" = `< $5,000` + `$5-9,999`,
         "$10-19,999" = `$10-14,999` + `$15-19,999`,
         "$20-29,999" = `$20-24,999` + `$25-29,999`,
         "$30-39,999" = `$30-34,999` + `$35-39,999`,
         "$40-49,999" = `$40-44,999` + `$45-49,999`) |> 
  select(-"< $5,000", -"$5-9,999", -"$10-14,999", -"$15-19,999", -"$20-24,999",
         -"$25-29,999", -"$30-34,999", -"$35-39,999", -"$40-44,999", -"$45-49,999",
         -Total, -"=> $100,000") |> 
  pivot_longer(cols = everything(), names_to = "Bracket", values_to = "Counts") |>
  mutate(Bracket = factor(Bracket, levels = c("=> $200,000", "$150-199,999", "$125-149,999",
                                              "$100-124,999", "$90-99,999", "$80-89,999",
                                              "$70-79,999", "$60-69,999", "$50-59,999",
                                              "$40-49,999", "$30-39,999", "$20-29,999",
                                              "$10-19,999", "< $10,000")))

#Creating the sideways bar graph for tot_hh_bar
ggplot(tot_hh_bar, aes(x = Bracket, y = Counts, fill = "")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, fill = "royalblue2") +
  labs(title = "2020 Total Household Income", x = "Total Household Income", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_flip()

#Preparing the bar graph table for $50k intervals
tot_hh_bar_50 <- tot_hh_lvl |> 
  mutate("< $50,000"= `< $5,000` + `$5-9,999` + `$10-14,999` + `$15-19,999` + `$20-24,999` +
           `$25-29,999` + `$30-34,999` + `$35-39,999` +`$40-44,999` + `$45-49,999`,
         "$50-99,999" = `$50-59,999` + `$60-69,999` + `$70-79,999` + `$80-89,999` + `$90-99,999`,
         "$100-149,999" = `$100-124,999` + `$125-149,999`) |> 
  select("< $50,000", "$50-99,999", "$100-149,999", "$150-199,999", "=> $200,000") |> 
  pivot_longer(cols = everything(), names_to = "Bracket", values_to = "Counts") |> 
  mutate(Bracket = factor(Bracket, levels = c("=> $200,000", "$150-199,999", "$100-149,999",
                                              "$50-99,999", "< $50,000")))

#Bar graph for tot_hh_bar_50
ggplot(tot_hh_bar_50, aes(x = Bracket, y = Counts, fill = "")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, fill = "royalblue2") +
  labs(title = "2020 Total Household Income", x = "Total Household Income", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_flip()

# After-Tax Household Income ----------------------------------------------
#Setting the names for the vectors
at_hh_names <- c("Total", "< $5,000", "$5-9,999", "$10-14,999", "$15-19,999",
                  "$20-24,999", "$25-29,999", "$30-34,999", "$35-39,999", "$40-44,999",
                  "$45-49,999", "$50-59,999", "$60-69,999", "$70-79,999", "$80-89,999",
                  "$90-99,999", "=> $100,000", "$100-124,999", "$125-149,999", "=> $150,000")

#Grabbing vectors for AT household income and renaming them with at_hh_names
at_hh_vectors <- can21$vector[which(can21$vector == "v_CA21_944"):which(can21$vector == "v_CA21_963")] |> 
  set_names(at_hh_names)

#Grabbing AT household income
at_hh_lvl  <- get_census(dataset = "CA21", 
                          regions = list(CSD = 2465005), 
                          level = "CSD",
                          vectors = at_hh_vectors) |> 
  select(all_of(at_hh_names)) |> 
  select(everything())

#Preparing the bar graph for "regular" intervals
at_hh_bar <- at_hh_lvl |> 
  mutate("< $10,000" = `< $5,000` + `$5-9,999`,
         "$10-19,999" = `$10-14,999` + `$15-19,999`,
         "$20-29,999" = `$20-24,999` + `$25-29,999`,
         "$30-39,999" = `$30-34,999` + `$35-39,999`,
         "$40-49,999" = `$40-44,999` + `$45-49,999`) |> 
  select(-"< $5,000", -"$5-9,999", -"$10-14,999", -"$15-19,999", -"$20-24,999",
         -"$25-29,999", -"$30-34,999", -"$35-39,999", -"$40-44,999", -"$45-49,999",
         -Total, -"=> $100,000") |> 
  pivot_longer(cols = everything(), names_to = "Bracket", values_to = "Counts") |>
  mutate(Bracket = factor(Bracket, levels = c("=> $150,000", "$125-149,999",
                                              "$100-124,999", "$90-99,999", "$80-89,999",
                                              "$70-79,999", "$60-69,999", "$50-59,999",
                                              "$40-49,999", "$30-39,999", "$20-29,999",
                                              "$10-19,999", "< $10,000")))

#Creating the sideways bar graph for tot_hh_bar
ggplot(at_hh_bar, aes(x = Bracket, y = Counts, fill = "")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, fill = "royalblue2") +
  labs(title = "2020 After-Tax Household Income", x = "Total Household Income", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_flip()

#Preparing the bar graph table for $50k intervals
at_hh_bar_50 <- at_hh_lvl |> 
  mutate("< $50,000"= `< $5,000` + `$5-9,999` + `$10-14,999` + `$15-19,999` + `$20-24,999` +
           `$25-29,999` + `$30-34,999` + `$35-39,999` +`$40-44,999` + `$45-49,999`,
         "$50-99,999" = `$50-59,999` + `$60-69,999` + `$70-79,999` + `$80-89,999` + `$90-99,999`,
         "$100-149,999" = `$100-124,999` + `$125-149,999`) |> 
  select("< $50,000", "$50-99,999", "$100-149,999", "=> $150,000") |> 
  pivot_longer(cols = everything(), names_to = "Bracket", values_to = "Counts") |> 
  mutate(Bracket = factor(Bracket, levels = c("=> $150,000", "$100-149,999",
                                              "$50-99,999", "< $50,000")))

#Bar graph for at_hh_bar_50
ggplot(at_hh_bar_50, aes(x = Bracket, y = Counts, fill = "")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, fill = "royalblue2") +
  labs(title = "2020 After-Tax Household Income", x = "Total Household Income", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_flip()

# Household Income in 2020 ------------------------------------------------
#Pulling vectors for household income in 2020
hh_inc20_vectors <- c("v_CA21_906","v_CA21_907", "v_CA21_909", "v_CA21_910",
                      "v_CA21_912", "v_CA21_913") |> 
  set_names(c("total", "at_total", "one_p", "at_one_p", "two_p", "at_two_p"))

#Creating a function for cancensus so multiple uses aren't needed
census_grabber <- function(region, geolevel, geoname){
  regions_list <- list()
  regions_list[[geolevel]] <- region
  
  get_census(dataset = "CA21",
    regions = regions_list,
    level = geolevel,
    vectors = hh_inc20_vectors
  ) |> 
    mutate(Geography = geoname) |> 
    select(Geography, total, at_total, one_p, at_one_p, two_p, at_two_p)
}

#Fetching census data for each of the geographies for hh_inc20_vectors
hh_inc20_lvl <- census_grabber("2465005", "CSD", "Laval")
hh_inc20_mtlcma <- census_grabber("24462", "CMA", "Montreal CMA")
hh_inc20_qc <- census_grabber("24", "PR", "Quebec")

#Preparing the table for the group stacked bar chart
hh_inc20_graph <- bind_rows(hh_inc20_lvl, hh_inc20_mtlcma, hh_inc20_qc) |> 
  mutate(total_diff = total - at_total, one_p_diff = one_p - at_one_p,
         two_p_diff = two_p - at_two_p) |> 
  select(Geography, at_total, total_diff, at_one_p, one_p_diff, at_two_p, two_p_diff) |> 
  pivot_longer(cols = -Geography, names_to = "income_type", values_to = "income") |> 
  mutate("Household Type" = case_when(
    income_type %in% c("at_total", "total_diff") ~ "Private",
    income_type %in% c("at_one_p", "one_p_diff") ~ "One Person",
    income_type %in% c("at_two_p", "two_p_diff") ~ "Two+ Persons")) |>
  group_by(`Household Type`) |>
  mutate(income_type = factor(income_type, levels = c("one_p_diff", "at_one_p",
                                                      "two_p_diff", "at_two_p",
                                                      "total_diff", "at_total"))) |>
  ungroup() |> 
  mutate(`Household Type` = factor(`Household Type`, levels = c("Private", "One Person", "Two+ Persons")))

#Create a grouped stacked bar graph
ggplot(hh_inc20_graph, aes(x = Geography, y = income, fill = income_type)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~`Household Type`, scales = "fixed", nrow = 1) +
  labs(title = "2020 Private Household Income", x = "Geography", y = "Income") +
  theme_minimal() +
  scale_fill_manual(values = c("total_diff" = "#aec7e8", "at_total" = "#1f77b4",
                               "one_p_diff" = "#ffbb78", "at_one_p" = "#ff7f0e",
                               "two_p_diff" = "#98df8a", "at_two_p" = "#2ca02c"),
                    labels = c("total_diff" = "Private Household Total Income",
                               "at_total" = "Private Household After-Tax Income",
                               "one_p_diff" = "1 Person Total Income",
                               "at_one_p" = "1 Person After-Tax Income",
                               "two_p_diff" = "2+ Person Total Income",
                               "at_two_p" = "2+ Person After Income")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Economic Household Income -----------------------------------------------

ehh_inc20_vectors <- c("v_CA21_965","v_CA21_966", "v_CA21_969", "v_CA21_970",
                      "v_CA21_973", "v_CA21_974", "v_CA21_977", "v_CA21_978") |> 
  set_names(c("total", "at_total", "couple", "at_couple", "couple_c",
              "at_couple_c", "one_p", "at_one_p"))

#Census grabber for economic household income
ehh_census_grabber <- function(region, geolevel, geoname){
  regions_list <- list()
  regions_list[[geolevel]] <- region
  
  get_census(dataset = "CA21",
             regions = regions_list,
             level = geolevel,
             vectors = ehh_inc20_vectors
  ) |> 
    mutate(Geography = geoname) |> 
    select(Geography, total, at_total, couple, at_couple,
           couple_c, at_couple_c, one_p, at_one_p)
}

#Grabbing census data for economic household income
ehh_inc20_lvl <- ehh_census_grabber("2465005", "CSD", "Laval")
ehh_inc20_mtlcma <- ehh_census_grabber("24462", "CMA", "Montreal CMA")
ehh_inc20_qc <- ehh_census_grabber("24", "PR", "Quebec")

#Preparing the table for the group stacked bar chart
ehh_inc20_graph <- bind_rows(ehh_inc20_lvl, ehh_inc20_mtlcma, ehh_inc20_qc) |> 
  mutate(total_diff = total - at_total, couple_diff = couple - at_couple,
         couple_c_diff = couple_c - at_couple_c, one_p_diff = one_p - at_one_p) |> 
  select(Geography, at_total, total_diff, at_one_p, one_p_diff,
         at_couple, couple_diff, at_couple_c, couple_c_diff) |> 
  pivot_longer(cols = -Geography, names_to = "income_type", values_to = "income") |> 
  mutate("Household Type" = case_when(
    income_type %in% c("at_total", "total_diff") ~ "Economic",
    income_type %in% c("at_one_p", "one_p_diff") ~ "One Parent",
    income_type %in% c("at_couple", "couple_diff") ~ "Couple",
    income_type %in% c("at_couple_c", "couple_c_diff") ~ "Couple with Children")) |>
  group_by(`Household Type`) |>
  mutate(income_type = factor(income_type, levels = c("total_diff", "at_total",
                                                      "one_p_diff", "at_one_p",
                                                      "couple_diff", "at_couple",
                                                      "couple_c_diff", "at_couple_c"))) |>
  ungroup() |> 
  mutate(`Household Type` = factor(`Household Type`, levels = c("Economic", "One Parent",
                                                                "Couple", "Couple with Children")))

ggplot(ehh_inc20_graph, aes(x = Geography, y = income, fill = income_type)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~`Household Type`, scales = "fixed", nrow = 1) +
  labs(title = "2020 Economic Household Income", x = "Geography", y = "Income") +
  theme_minimal() +
  scale_fill_manual(values = c("total_diff" = "#aec7e8", "at_total" = "#1f77b4",
                               "one_p_diff" = "#ffbb78", "at_one_p" = "#ff7f0e",
                               "couple_diff" = "#98df8a", "at_couple" = "#2ca02c",
                               "couple_c_diff" = "#ff9896", "at_couple_c" = "#d62728"),
                    labels = c("total_diff" = "Economic Household Total Income",
                               "at_total" = "Economic Household After-Tax Income",
                               "one_p_diff" = "1 Parent Total Income",
                               "at_one_p" = "1 Parent After-Tax Income",
                               "couple_diff" = "Couple Total Income",
                               "at_couple" = "Couple After-Tax Income",
                               "couple_c_diff" = "Couple w/ Children Total Income",
                               "at_couple_c" = "Couple w/ Children After Tax Income")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Evolution of Individual Income ------------------------------------------
#Census grabber for median individual income for 2020 to start up the graph table
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

#Census grabber for years other than 2020
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

#Grabbing median individual income for Montreal CMA for years 2000-2020
mii_inc20_mtl <- mii_census_grabber_20("2466023", "CSD", "Montreal", (1:10)) |> 
  rename("2020" := last_col())
mii_inc15_mtl <- mii_census_grabber("CA16", "2466023", "CSD", "v_CA16_2207", (1:10), "2015")
mii_inc10_mtl <- mii_census_grabber("CA11", "2466023", "CSD", "v_CA11N_2341", (1:10), "2010") |> 
  select(-1)
mii_inc05_mtl <- mii_census_grabber("CA06", "2466023", "CSD", "v_CA06_1583", (1:10), "2005")
mii_inc00_mtl <- mii_census_grabber("CA01", "2466025", "CSD", "v_CA01_1449", (1:10), "2000")
#Bind the tables together
mii_mtl_graph <- bind_cols(mii_inc20_mtl, mii_inc15_mtl, mii_inc10_mtl, mii_inc05_mtl, mii_inc00_mtl)

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
mii_graph <- bind_rows(mii_lvl_graph, mii_mtl_graph, mii_qc_graph) |> 
  pivot_longer(cols = -Geography, names_to = "Year", values_to = "Income")

#Creating the line graph for median individual income
ggplot(mii_graph, aes(x = Year, y = Income, color = Geography, group = Geography)) +
  geom_line() +
  labs(title = "Individual Median Income 2000-2020",
       x = "Year",
       y = "Individual Median Income ($)") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal",
        legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

# Evolution of Household Income ------------------------------------------
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

#Grabbing median individual income for Montreal CMA for years 2000-2020
mhh_inc20_mtl <- mhh_census_grabber_20("2466023", "CSD", "Montreal", (1:10)) |> 
  rename("2020" := last_col())
mhh_inc15_mtl <- mhh_census_grabber("CA16", "2466023", "CSD", "v_CA16_2397", (1:10), "2015")
mhh_inc10_mtl <- mhh_census_grabber("CA11", "2466023", "CSD", "v_CA11N_2562", (1:10), "2010") |> 
  select(-1)
mhh_inc05_mtl <- mhh_census_grabber("CA06", "2466023", "CSD", "v_CA06_2000", (1:10), "2005")
mhh_inc00_mtl <- mhh_census_grabber("CA01", "2466025", "CSD", "v_CA01_1634", (1:10), "2000")
#Bind the tables together
mhh_mtl_graph <- bind_cols(mhh_inc20_mtl, mhh_inc15_mtl, mhh_inc10_mtl, mhh_inc05_mtl, mhh_inc00_mtl)

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
mhh_graph <- bind_rows(mhh_lvl_graph, mhh_mtl_graph, mhh_qc_graph) |> 
  pivot_longer(cols = -Geography, names_to = "Year", values_to = "Income")

#Creating the line graph for median household income
ggplot(mhh_graph, aes(x = Year, y = Income, color = Geography, group = Geography)) +
  geom_line() +
  labs(title = "Median Household Income 2000-2020",
       x = "Year",
       y = "Median Household Income ($)") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal",
        legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

# Maps --------------------------------------------------------------------
laval_medinc <- get_census(dataset = "CA21", 
                         regions = list(CSD = 2465005), 
                         vectors = c("med_inc" = "v_CA21_560"),
                         level = "CT",
                         geo_format = "sf")

ggplot(data = laval_medinc) +
  geom_sf(aes(fill = med_inc)) +
  labs(title = "Median Income in Laval") +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

laval_hhmedinc <- get_census(dataset = "CA21", 
                           regions = list(CSD = 2465005), 
                           vectors = c("med_inc" = "v_CA21_906"),
                           level = "CT",
                           geo_format = "sf")

ggplot(data = laval_hhmedinc) +
  geom_sf(aes(fill = med_inc)) +
  labs(title = "Median Household Income in Laval") +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
