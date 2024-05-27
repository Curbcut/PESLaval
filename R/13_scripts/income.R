#Loading libraries
source("R/01_startup.R")

#Setting CensusMapper API Key because it won't save
set_cancensus_api_key("CensusMapper_4308d496f011429cf814385050f083dc")

#Grabbing all cancensus vector
can21 <- list_census_vectors(dataset = "CA21")

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
