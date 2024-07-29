source("R/01_startup.R")
library(scales)
library(readxl)
library(gt)
library(forcats)
library(extrafont)
library(scales)

#Setting CensusMapper API Key because it won't save
set_cancensus_api_key("CensusMapper_4308d496f011429cf814385050f083dc")

#Caching census data to reduce amount of calls and speed up process.
#Personal use only, change the folder to your own folder if you want to use it
set_cancensus_cache_path("D:/McGill/can_cache", install = TRUE, overwrite = TRUE)
set_cancensus_cache_path("/Users/justin/Documents/R/CurbCutSelf")

#Curbcut scale
curbcut_scale <- c("#C4CDE1", "#98A8CB", "#6C83B5", "#4C5C7F", "#2B3448")
curbcut_fill <- c("#A3B0D1", "#73AD80", "#E08565", "#CD718C", "#C9C3FA",
                  "#F5D574", "#ADB033", "#9E9090")

# Immigration & Diversity -------------------------------------------------
#Grabbing immigration numbers and total population for each census year
imm_21v <- c("total" = "v_CA21_4404", "imm" = "v_CA21_4410")
imm_16v <- c("total" = "v_CA16_3405", "imm" = "v_CA16_3411")
imm_11v <- c("total" = "v_CA11N_16", "imm" = "v_CA11N_22")
imm_06v <- c("total" = "v_CA06_474", "imm" = "v_CA06_478")
imm_01v <- c("total" = "v_CA01_402", "imm" = "v_CA01_406")
imm_96v <- c("non" = "v_CA1996_126", "imm" = "v_CA1996_128")

#Grabbing the data from census
imm_lvl_mtl_21  <- get_census(dataset = "CA21", 
                             regions = list(CSD = c(2465005, 2466023)), 
                             level = "CSD",
                             vectors = imm_21v)

imm_qc_21  <- get_census(dataset = "CA21", 
                             regions = list(PR = c(24)), 
                             level = "PR",
                             vectors = imm_21v) |> 
  mutate(`Region Name` = "Québec")

imm_lvl_mtl_16  <- get_census(dataset = "CA16", 
                              regions = list(CSD = c(2465005, 2466023)), 
                              level = "CSD",
                              vectors = imm_16v)

imm_qc_16  <- get_census(dataset = "CA16", 
                         regions = list(PR = c(24)), 
                         level = "PR",
                         vectors = imm_16v) |> 
  mutate(`Region Name` = "Québec")

imm_lvl_mtl_11  <- get_census(dataset = "CA11", 
                              regions = list(CSD = c(2465005, 2466023)), 
                              level = "CSD",
                              vectors = imm_11v)

imm_qc_11  <- get_census(dataset = "CA11", 
                         regions = list(PR = c(24)), 
                         level = "PR",
                         vectors = imm_11v) |> 
  mutate(`Region Name` = "Québec")

imm_lvl_mtl_06  <- get_census(dataset = "CA06", 
                              regions = list(CSD = c(2465005, 2466023)), 
                              level = "CSD",
                              vectors = imm_06v)

imm_qc_06  <- get_census(dataset = "CA06", 
                         regions = list(PR = c(24)), 
                         level = "PR",
                         vectors = imm_06v) |> 
  mutate(`Region Name` = "Québec")

imm_lvl_mtl_01  <- get_census(dataset = "CA01", 
                              regions = list(CSD = c(2465005, 2466025)), 
                              level = "CSD",
                              vectors = imm_01v)

imm_qc_01  <- get_census(dataset = "CA01", 
                         regions = list(PR = c(24)), 
                         level = "PR",
                         vectors = imm_01v) |> 
  mutate(`Region Name` = "Québec")

imm_lvl_mtl_96  <- get_census(dataset = "CA1996", 
                              regions = list(CSD = c(2465005, 2466025)), 
                              level = "CSD",
                              vectors = imm_96v)

imm_qc_96  <- get_census(dataset = "CA1996", 
                         regions = list(PR = c(24)), 
                         level = "PR",
                         vectors = imm_96v) |> 
  mutate(`Region Name` = "Québec")

#Binding and cleaning up the data
imm_21 <- bind_rows(imm_lvl_mtl_21, imm_qc_21) |> 
  mutate(year = 2021,
         percentage = imm / total) |> 
  select(`Region Name`, year, percentage)

imm_16 <- bind_rows(imm_lvl_mtl_16, imm_qc_16) |> 
  mutate(year = 2016,
         percentage = imm / total) |> 
  select(`Region Name`, year, percentage)

imm_11 <- bind_rows(imm_lvl_mtl_11, imm_qc_11) |> 
  mutate(year = 2011,
         percentage = imm / total) |> 
  select(`Region Name`, year, percentage)

imm_06 <- bind_rows(imm_lvl_mtl_06, imm_qc_06) |> 
  mutate(year = 2006,
         percentage = imm / total) |> 
  select(`Region Name`, year, percentage)

imm_01 <- bind_rows(imm_lvl_mtl_01, imm_qc_01) |> 
  mutate(year = 2001,
         percentage = imm / total) |> 
  select(`Region Name`, year, percentage)

imm_96 <- bind_rows(imm_lvl_mtl_96, imm_qc_96) |> 
  mutate(year = 1996,
         percentage = imm / (non + imm)) |> 
  select(`Region Name`, year, percentage)

#Binding the rows together
imm <- bind_rows(imm_96, imm_01, imm_06, imm_11, imm_16, imm_21) |> 
  mutate(`Region Name` = if_else(`Region Name` == "Laval (V)", "Laval", `Region Name`),
         `Region Name` = if_else(`Region Name` == "Montréal (V)", "Montréal", `Region Name`))

imm_evol_graph <- ggplot(data = imm, aes(x = factor(year), y = percentage, color = `Region Name`, group = `Region Name`)) +
  geom_line(linewidth = 1.35) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = convert_pct) +
  labs(y = "Proportion de la population") +
  scale_color_manual(values = c("Laval" = "#A3B0D1", "Montréal" = "#E08565",
                                "Québec" = "#73AD80")) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal", axis.title.x = element_blank(),
        legend.title = element_blank(), text=element_text(family="KMR Apparat Regular"))

#Grabbing specific numbers for the text
imm_21_lvl_prop <- get_census(dataset = "CA21", 
                              regions = list(CSD = c(2465005)), 
                              level = "CSD",
                              vectors = imm_21v) |> 
  mutate(percentage = convert_pct(imm / total)) |> 
  pull(percentage)

imm_21_mtl_prop <- get_census(dataset = "CA21", 
                              regions = list(CSD = c(2466023)), 
                              level = "CSD",
                              vectors = imm_21v) |> 
  mutate(percentage = convert_pct(imm / total)) |> 
  pull(percentage)

imm_21_qc_prop  <- get_census(dataset = "CA21",
                              regions = list(PR = c(24)),
                              level = "PR",
                              vectors = imm_21v) |> 
  mutate(percentage = convert_pct(imm / total)) |> 
  pull(percentage)

#Prepping data to map it
breaks <- c(-Inf, 0.2, 0.3, 0.4, 0.5, Inf)

imm_lvl_21_ct <- get_census(dataset = "CA21", 
                              regions = list(CSD = c(2465005)), 
                              level = "DA",
                              vectors = imm_21v, geo_format = "sf") |> 
  mutate(percentage = imm / total) |> 
  mutate(percentage_category = cut(percentage, 
                                   breaks = breaks, 
                                   labels = c("< 20 %", "20-30 %", "30-40 %", "40-50 %", "> 50%"), 
                                   include.lowest = TRUE))

#Mapping the data
imm_prop_map <- ggplot(data = imm_lvl_21_ct) +
  gg_cc_tiles +
  geom_sf(aes(fill = percentage_category), color = NA) +
  geom_sf(data = laval_sectors, fill = "transparent", color = "black") +
  scale_fill_manual(values = curbcut_scale, na.value = "#B3B3BB") +
  labs(fill = "Proportion de la population") +
  theme_void() +
  theme(legend.position = "bottom", legend.box = "horizontal",
        text=element_text(family="KMR Apparat Regular")) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5,
                             nrow = 1))

#Data Table
imm_table_21v <- c("Population (n)"= "v_CA21_4404", "Immigrants totaux (n)" = "v_CA21_4410",
                   "Immigrants récents (n)" = "v_CA21_4635", "Résident non permanents (n)" = "v_CA21_4434",
                   "Non-immigrants (n)" = "v_CA21_4407")

imm_table_lvl <- get_census(dataset = "CA21", 
                              regions = list(CSD = c(2465005)), 
                              level = "CSD",
                              vectors = imm_table_21v) |> 
  mutate(Région = "Laval")

imm_table_mtl <- get_census(dataset = "CA21", 
                              regions = list(CSD = c(2466023)), 
                              level = "CSD",
                              vectors = imm_table_21v) |> 
  mutate(Région = "Montréal")

imm_table_qc  <- get_census(dataset = "CA21",
                              regions = list(PR = c(24)),
                              level = "PR",
                              vectors = imm_table_21v) |> 
  mutate(Région = "Québec")

#Formatting the numbers
imm_table_data <- bind_rows(imm_table_lvl, imm_table_mtl, imm_table_qc) |> 
  mutate(`Immigrants totaux (%)` = convert_pct(`Immigrants totaux (n)` / `Population (n)`),
         `Immigrants récents (%)` = convert_pct(`Immigrants récents (n)` / `Population (n)`),
         `Résident non permanents (%)` = convert_pct(`Résident non permanents (n)` / `Population (n)`),
         `Non-immigrants (%)` = convert_pct(`Non-immigrants (n)` / `Population (n)`)) |>
  mutate(`Population (n)` = convert_number(`Population (n)`),
         `Immigrants totaux (n)` = convert_number(`Immigrants totaux (n)`),
         `Immigrants récents (n)` = convert_number(`Immigrants récents (n)`),
         `Résident non permanents (n)` = convert_number(`Résident non permanents (n)`),
         `Non-immigrants (n)` = convert_number(`Non-immigrants (n)`)) |> 
  select(Région, `Population (n)`, `Immigrants totaux (n)`, `Immigrants totaux (%)`,
         `Immigrants récents (n)`, `Immigrants récents (%)`, `Résident non permanents (n)`,
         `Résident non permanents (%)`, `Non-immigrants (n)`, `Non-immigrants (%)`)

#Creating the table
imm_table <- imm_table_data |> gt() |> 
  tab_style(
    style = cell_fill(color = "#F0F0F0"),
    locations = cells_body(rows = 2, columns = everything())
  ) |> 
  tab_style(
    style = cell_borders(sides = "right", color = "darkgrey",weight = px(2)),
    locations = cells_body(columns = 2)
  ) |> 
  tab_style(
    style = cell_borders(sides = "right", color = "darkgrey",weight = px(2)),
    locations = cells_body(columns = 4)
  ) |> 
  tab_style(
    style = cell_borders(sides = "right", color = "darkgrey",weight = px(2)),
    locations = cells_body(columns = 6)
  ) |> 
  tab_style(
    style = cell_borders(sides = "right", color = "darkgrey",weight = px(2)),
    locations = cells_body(columns = 8)
  )

# Immigrant Status --------------------------------------------------------
#Pulling the number for the non-resident population
non_res_prop <- get_census(dataset = "CA21", 
                            regions = list(CSD = c(2465005)), 
                            level = "CSD",
                            vectors = c("Pop"= "v_CA21_4404", "nonres" = "v_CA21_4434")) |> 
  mutate(test = convert_pct(nonres / Pop)) |> 
  pull(test)

#Recent immigrant map
#Prepping data to map it
breaks <- c(-Inf, 0.025, 0.05, 0.075, 0.1, Inf)

recimm_lvl_21_da <- get_census(dataset = "CA21", 
                            regions = list(CSD = c(2465005)), 
                            level = "DA",
                            vectors = c("pop"= "v_CA21_4404", "recent" = "v_CA21_4635"),
                            geo_format = "sf") |> 
  mutate(percentage = recent / pop) |> 
  replace_na(list(percentage = 0)) |> 
  mutate(percentage_category = cut(percentage, 
                                   breaks = breaks, 
                                   labels = c("< 2,5 %", "2,5-5 %", "5-7,5 %", "7,5-20 %", "> 20%"), 
                                   include.lowest = TRUE))

#Mapping the data
recimm_prop_map <- ggplot(data = recimm_lvl_21_da) +
  gg_cc_tiles +
  geom_sf(aes(fill = percentage_category), color = NA) +
  geom_sf(data = laval_sectors, fill = "transparent", color = "black") +
  scale_fill_manual(values = curbcut_scale, na.value = "#B3B3BB") +
  labs(fill = "Proportion de la population") +
  theme_void() +
  theme(legend.position = "bottom", legend.box = "horizontal",
        text=element_text(family="KMR Apparat Regular")) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5,
                             nrow = 1))

#Citizen proportion
Citizenship <- cancensus::get_census(dataset = "CA21", 
                                     regions = list(CSD = 2465005), 
                                     level = "CSD",
                                     vectors = c("Total" = "v_CA21_4389",
                                                 "Canadian" = "v_CA21_4392",
                                                 "NotCanadian" = "v_CA21_4401"))

Citizenship <- Citizenship |> mutate(PercentCanadian = Canadian/Total)

CanadianCitizensLaval <- Citizenship[1,"PercentCanadian"] |> 
  mutate(PercentCanadian = convert_pct(PercentCanadian)) |> 
  pull(PercentCanadian)

Citizenship_Quebec <- cancensus::get_census(dataset = "CA21", 
                                            regions = list(PR = 24), 
                                            level = "PR",
                                            vectors = c("Total" = "v_CA21_4389",
                                                        "Canadian" = "v_CA21_4392",
                                                        "NotCanadian" = "v_CA21_4401"))

Citizenship_Quebec <- Citizenship_Quebec |> mutate(PercentCanadian = Canadian/Total)

CanadianCitizensQc <- Citizenship_Quebec[1, "PercentCanadian"] |> 
  mutate(PercentCanadian = convert_pct(PercentCanadian)) |> 
  pull(PercentCanadian)

Citizenship_mtl <- cancensus::get_census(dataset = "CA21", 
                                         regions = list(CSD = 2466023), 
                                         level = "CSD",
                                         vectors = c("Total" = "v_CA21_4389",
                                                     "Canadian" = "v_CA21_4392",
                                                     "NotCanadian" = "v_CA21_4401"))

Citizenship_mtl <- Citizenship_mtl |> mutate(PercentCanadian = Canadian/Total)

CanadianCitizensMtl <- Citizenship_mtl[1, "PercentCanadian"] |> 
  mutate(PercentCanadian = convert_pct(PercentCanadian)) |> 
  pull(PercentCanadian)

# Period of Immigration ---------------------------------------------------
#Pulling data and making it usable for laval
immigrant_decade <- cancensus::get_census(dataset = "CA21", 
                                          regions = list(CSD = 2465005), 
                                          level = "CSD",
                                          vectors = c("Total" = "v_CA21_4404",
                                                      "Avant 1980" = "v_CA21_4413",
                                                      "1980 à 1990" = "v_CA21_4416",
                                                      "1991 à 2000" = "v_CA21_4419",
                                                      "2001 à 2010" = "v_CA21_4422",
                                                      "2011 à 2021" = "v_CA21_4425"))

immigrant_decade_percent <- immigrant_decade |> 
  mutate(across(`Avant 1980`:`2011 à 2021`, ~. /Total)) |> 
  select(-Total) |> 
  pivot_longer(-c(GeoUID:CMA_UID)) |> 
  mutate(Region = "Laval")

immigrant_decade_percent$name <- 
  factor(immigrant_decade_percent$name, levels = unique(immigrant_decade_percent$name))

ggplot(data = immigrant_decade_percent, aes(x = name, y = value)) +
  geom_bar(stat = "identity") +
  labs(x = "Decade", y = "Percent", title = "Period of Immigration Laval")

#Now for Quebec
immigrant_decade_queb <- cancensus::get_census(dataset = "CA21", 
                                               regions = list(PR = 24), 
                                               level = "PR",
                                               vectors = c("Total" = "v_CA21_4404",
                                                           "Avant 1980" = "v_CA21_4413",
                                                           "1980 à 1990" = "v_CA21_4416",
                                                           "1991 à 2000" = "v_CA21_4419",
                                                           "2001 à 2010" = "v_CA21_4422",
                                                           "2011 à 2021" = "v_CA21_4425"))

immigrant_decade_queb_percent <- immigrant_decade_queb |> 
  mutate(across(`Avant 1980`:`2011 à 2021`, ~. /Total)) |> 
  select(-Total) |> 
  pivot_longer(-c(GeoUID:C_UID)) |> 
  mutate(Region = "Québec")

immigrant_decade_queb_percent$name <- 
  factor(immigrant_decade_queb_percent$name, levels = unique(immigrant_decade_queb_percent$name))

# combine percentage plots together
combined_decade_data <- bind_rows(immigrant_decade_percent, immigrant_decade_queb_percent) |> 
  mutate(percentage = convert_pct(value))

period_imm_graph <- ggplot(data = combined_decade_data, aes(x = name, y = value, fill = Region)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = percentage), position = position_dodge(width = 0.9),
            vjust = 2.5, size = 4, color = "white") +
  labs(x = "Decade", y = "Proportion de la population", title = "Period of Immigration: Laval vs. Quebec") +
  scale_fill_manual(values = c("Laval" = "#A3B0D1", "Québec" = "#73AD80")) +
  scale_y_continuous(labels = function(x) paste0(scales::percent(x, accuracy = 1), " ")) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_blank(), axis.title.x = element_blank(),
        legend.title = element_blank(), text = element_text(family = "KMR Apparat Regular"))

# Immigration Admission Category ------------------------------------------
immigrant_admissioncat <- cancensus::get_census(dataset = "CA21", 
                                                regions = list(CSD = 2465005), 
                                                level = "CSD",
                                                vectors = c("Total" = "v_CA21_4830",
                                                            "Économique" = "v_CA21_4833",
                                                            "Famille" = "v_CA21_4842",
                                                            "Réfugiés" = "v_CA21_4845",
                                                            "Autres" = "v_CA21_4848"))

immigrant_admissioncat_percent <- immigrant_admissioncat |> 
  mutate(Region = "Laval",
         Économique = `Économique`/ Total,
         Famille = `Famille`/Total,
         Réfugiés = `Réfugiés`/Total,
         Autres = Autres/Total) |> 
  select(Region, Économique, Famille, Réfugiés, Autres)

# compare to quebec

immigrant_admissioncat_qc <- cancensus::get_census(dataset = "CA21", 
                                                   regions = list(CSD = 24), 
                                                   level = "PR",
                                                   vectors = c("Total" = "v_CA21_4830",
                                                               "Économique" = "v_CA21_4833",
                                                               "Famille" = "v_CA21_4842",
                                                               "Réfugiés" = "v_CA21_4845",
                                                               "Autres" = "v_CA21_4848"))

immigrant_admissioncat_percent_qc <- immigrant_admissioncat_qc |> 
  mutate(Region = "Québec",
         Économique = `Économique`/ Total,
         Famille = `Famille`/Total,
         Réfugiés = `Réfugiés`/Total,
         Autres = Autres/Total) |> 
  select(Region, Économique, Famille, Réfugiés, Autres)

# combine with laval

admission_cat_combined <- bind_rows(immigrant_admissioncat_percent, immigrant_admissioncat_percent_qc) |> 
  pivot_longer(cols = -Region, names_to = "Type", values_to = "Percent") |> 
  mutate(perc = convert_pct(Percent)) |> 
  mutate(Type = factor(Type, levels = c("Économique", "Famille", "Réfugiés", "Autres")))

ad_cat_graph <- ggplot(data = admission_cat_combined, aes(x = Type, y = Percent, fill = Region)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = perc), position = position_dodge(width = 0.9),
            vjust = -0.5, size = 4, color = "black") +
  scale_y_continuous(labels = function(x) paste0(scales::percent(x, accuracy = 1), " ")) +
  scale_fill_manual(values = c("Laval" = "#A3B0D1", "Québec" = "#73AD80")) +
  labs(y = "Proportion d'immigrants", x = "Catégorie d'admission") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_blank(),
        legend.title = element_blank(), text = element_text(family = "KMR Apparat Regular"))

#Grabbing specific numbers for markdown
laval_ad_cat <- admission_cat_combined |> 
  filter(Region == "Laval" & Type == "Économique") |> 
  pull(perc)

# Age of Immigration ------------------------------------------------------
#Grabbing vectors for data
age_21v <- c("m_under_5" = "v_CA21_4441", "m_5_14" = "v_CA21_4444", "m_15_24" = "v_CA21_4447",
             "m_25_44" = "v_CA21_4450", "m_45_over" = "v_CA21_4453",
             "f_under_5" = "v_CA21_4442", "f_5_14" = "v_CA21_4445", "f_15_24" = "v_CA21_4448",
             "f_25_44" = "v_CA21_4451", "f_45_over" = "v_CA21_4454")

#Grabbing totals for each sex
age_21_gender <- get_census(dataset = "CA21",
                            regions = list(CSD = 2465005),
                            level = "CSD",
                            vectors = c("male" = "v_CA21_4438", "female" = "v_CA21_4439"))

age_21_male <- age_21_gender |> pull(male)
age_21_female <- age_21_gender |> pull(female)
age_21_total <- age_21_male + age_21_female

#Grabbing actual data for age of immigration and cleaning it up
age_21 <- get_census(dataset = "CA21",
                     regions = list(CSD = 2465005),
                     level = "CSD",
                     vectors = age_21v) |> 
  select("m_under_5", "m_5_14", "m_15_24", "m_25_44", "m_45_over", "f_under_5",
         "f_5_14", "f_15_24", "f_25_44", "f_45_over") |> 
  pivot_longer(cols = everything(), names_to = "Age", values_to = "count") |> 
  mutate(gender = if_else(str_starts(Age, "m_"), "Homme", "Femme"),
         Age = case_when(
           str_ends(Age, "_5") ~ "Moins de 5 ans",
           str_ends(Age, "_14") ~ "5 à 14",
           str_ends(Age, "_24") ~ "15 à 24",
           str_ends(Age, "_44") ~ "25 à 44",
           str_ends(Age, "_over") ~ "45 et plus"
         )) |> 
  mutate(Age = factor(Age, levels = c("45 et plus", "25 à 44", "15 à 24", "5 à 14",
                                      "Moins de 5 ans")),
         prop = if_else(gender == "Homme", count / age_21_male, count / age_21_female)) |> 
  mutate(percentage = convert_pct(prop))

#Reversing the factor for use in other graphs
age_21_rev <- get_census(dataset = "CA21",
                     regions = list(CSD = 2465005),
                     level = "CSD",
                     vectors = age_21v) |> 
  select("m_under_5", "m_5_14", "m_15_24", "m_25_44", "m_45_over", "f_under_5",
         "f_5_14", "f_15_24", "f_25_44", "f_45_over") |> 
  pivot_longer(cols = everything(), names_to = "Age", values_to = "count") |> 
  mutate(gender = if_else(str_starts(Age, "m_"), "Homme", "Femme"),
         Age = case_when(
           str_ends(Age, "_5") ~ "Moins de 5 ans",
           str_ends(Age, "_14") ~ "5 à 14",
           str_ends(Age, "_24") ~ "15 à 24",
           str_ends(Age, "_44") ~ "25 à 44",
           str_ends(Age, "_over") ~ "45 et plus"
         )) |> 
  mutate(Age = factor(Age, levels = c("Moins de 5 ans", "5 à 14", "15 à 24", "25 à 44",
                                      "45 et plus")),
         prop = if_else(gender == "Homme", count / age_21_total, count / age_21_total)) |> 
  mutate(percentage = convert_pct(prop),
         gender = factor(gender, levels = c("Homme", "Femme")))

#Graphing age of immigration by sex by numbers
imm_age_sex_graph <- ggplot(data = age_21, aes(x = gender, y = count, fill = Age)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percentage), 
            position = position_stack(vjust = 0.5), 
            color = "white") +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  scale_fill_manual(values = curbcut_scale) +
  labs(y = "Personnes",
       fill = "Age Group") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_blank(),
        legend.title = element_blank(), text = element_text(family = "KMR Apparat Regular"),
        axis.title.x = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE))

#Doing the same above except by proportion
imm_age_sex_prop_graph <- ggplot(data = age_21, aes(x = gender, y = prop, fill = Age)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percentage), 
            position = position_stack(vjust = 0.5), 
            color = "white") +
  scale_fill_manual(values = curbcut_scale) +
  scale_y_continuous(labels = convert_pct) +
  labs(x = "Tranche d'âge",
       y = "Proportion d'immigrants",
       fill = "Age Group") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_blank(),
        legend.title = element_blank(), text = element_text(family = "KMR Apparat Regular"),
        axis.title.x = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE))

#Graphing numbers by age group
imm_age_graph <- ggplot(data = age_21_rev, aes(x = Age, y = count, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = percentage), position = position_dodge(width = 0.9),
            vjust = 2.5, size = 4, color = "white") +
  scale_fill_manual(values = c("Homme" = "#A3B0D1", "Femme" = "#CD718C")) +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  labs(x = "Gender",
       y = "Count",
       fill = "Age Group") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_blank(),
        legend.title = element_blank(), text = element_text(family = "KMR Apparat Regular"),
        axis.title.x = element_blank())

imm_stackedage_graph <- ggplot(data = age_21, aes(x = Age, y = count, fill = gender)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = percentage), 
            position = position_stack(vjust = 0.5), 
            color = "white") +
  scale_fill_manual(values = c("Homme" = "#A3B0D1", "Femme" = "#CD718C")) +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  labs(x = "Gender",
       y = "Personnes",
       fill = "Age Group") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_blank(),
        legend.title = element_blank(), text = element_text(family = "KMR Apparat Regular"),
        axis.title.x = element_blank())

# Ethnic Origins ----------------------------------------------------------
#Grabbing total immigration numbers
imm_origin_total <- get_census(dataset = "CA21",
                               regions = list(CSD = 2465005),
                               level = "CSD",
                               vectors = c("Brazil" = "v_CA21_4461",
                                           "Colombia" = "v_CA21_4464",
                                           "El Salvador" = "v_CA21_4467",
                                           "Guyana" = "v_CA21_4470",
                                           "Haiti" = "v_CA21_4473",
                                           "Jamaica" = "v_CA21_4476",
                                           "Mexico" = "v_CA21_4479",
                                           "Peru" = "v_CA21_4482",
                                           "Trinidad and Tobago" = "v_CA21_4485",
                                           "Other in Americas" = "v_CA21_4491",
                                           "États-Unis" = "v_CA21_4488",
                                           "L'Europe" = "v_CA21_4494",
                                           "Afrique" = "v_CA21_4545",
                                           "Asie" = "v_CA21_4578",
                                           "Océanie et autres" = "v_CA21_4632")) |>
  mutate(`Les Caraïbes, l'Amérique centrale, l'Amérique du Sud et l'Amérique latine` = Brazil + Mexico +
           + Colombia + `El Salvador` + Guyana + Haiti + Jamaica + Peru + `Trinidad and Tobago` +
           `Other in Americas`,
         `Type` = "Total") |> 
  select(`Type`, `États-Unis`, `Les Caraïbes, l'Amérique centrale, l'Amérique du Sud et l'Amérique latine`, `L'Europe`,
         `Afrique`, `Asie`, `Océanie et autres`) |> 
  pivot_longer(cols = -Type, names_to = "origin", values_to = "count")

#Grabbing recent numbers
imm_origin_recent <- get_census(dataset = "CA21",
                                regions = list(CSD = 2465005),
                                level = "CSD",
                                vectors = c("Brazil" = "v_CA21_4641",
                                            "Colombia" = "v_CA21_4644",
                                            "Haiti" = "v_CA21_4647",
                                            "Jamaica" = "v_CA21_4650",
                                            "Mexico" = "v_CA21_4653",
                                            "Venezuela" = "v_CA21_4659",
                                            "Other in Americas" = "v_CA21_4662",
                                            "États-Unis" = "v_CA21_4656",
                                            "L'Europe" = "v_CA21_4665",
                                            "Afrique" = "v_CA21_4692",
                                            "Asie" = "v_CA21_4740",
                                            "Océanie et autres" = "v_CA21_4809")) |>
  mutate(`Les Caraïbes, l'Amérique centrale, l'Amérique du Sud et l'Amérique latine` = Brazil + Mexico +
           + Colombia + Haiti + Jamaica + `Other in Americas`,
         `Type` = "Récent") |> 
  select(`Type`, `États-Unis`, `Les Caraïbes, l'Amérique centrale, l'Amérique du Sud et l'Amérique latine`, `L'Europe`,
         `Afrique`, `Asie`, `Océanie et autres`) |> 
  pivot_longer(cols = -Type, names_to = "origin", values_to = "count")

#Grabbing totals for later use
imm_origin_total_count <- get_census(dataset = "CA21",
                                     regions = list(CSD = 2465005),
                                     level = "CSD",
                                     vectors = c("total" = "v_CA21_4455")) |> 
  pull(total)

imm_origin_recent_count <- get_census(dataset = "CA21",
                                     regions = list(CSD = 2465005),
                                     level = "CSD",
                                     vectors = c("total" = "v_CA21_4635")) |> 
  pull(total)

#Binding the data together and calculating the proportion
imm_origin <- bind_rows(imm_origin_recent, imm_origin_total) |> 
  mutate(origin = ifelse(origin == "Les Caraïbes, l'Amérique centrale, l'Amérique du Sud et l'Amérique latine",
                         "Les Caraïbes, l'Amérique du Sud,\nl'Amérique centrale et\nl'Amérique latine", origin),
         proportion = ifelse(Type == "Total", count / imm_origin_total_count, count / imm_origin_recent_count)) |> 
  mutate(origin = factor(origin, levels = c("Asie", "Afrique", "Les Caraïbes, l'Amérique du Sud,\nl'Amérique centrale et\nl'Amérique latine",
                                            "L'Europe", "États-Unis", "Océanie et autres"))) |> 
  mutate(perc = convert_pct(proportion))

# plot of Recent vs Total Immigrant Population Origins
imm_origin_graph <- ggplot(data = imm_origin, aes(x = origin, y = proportion, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = perc), position = position_dodge(width = 0.9),
            vjust = -0.5, size = 4, color = "black") +
  scale_y_continuous(labels = convert_pct) +
  scale_fill_manual(values = c("Total" = "#A3B0D1", "Récent" = "#CD718C")) +
  labs(x = "Lieu de naissance",
       y = "Proportion d'immigrants") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_blank(),
        legend.title = element_blank(), text = element_text(family = "KMR Apparat Regular"),
        axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_text(margin = margin(t = -10)),
        legend.margin = margin(t = -5))

#Grabbing specific percentages for the text
imm_asia <- get_census(dataset = "CA21",
                       regions = list(CSD = 2465005),
                       level = "CSD",
                       vectors = c("Asie" = "v_CA21_4578")) |> 
  mutate(number = convert_pct(Asie / imm_origin_total_count)) |> 
  pull(number)

imm_africa <- get_census(dataset = "CA21",
                         regions = list(CSD = 2465005),
                         level = "CSD",
                         vectors = c("Africa" = "v_CA21_4545")) |> 
  mutate(number = convert_pct(Africa / imm_origin_total_count)) |> 
  pull(number)

imm_europe <- get_census(dataset = "CA21",
                         regions = list(CSD = 2465005),
                         level = "CSD",
                         vectors = c("Europe" = "v_CA21_4494")) |> 
  mutate(number = convert_pct(Europe / imm_origin_total_count)) |> 
  pull(number)

imm_CCSLA <- get_census(dataset = "CA21",
                        regions = list(CSD = 2465005),
                        level = "CSD",
                        vectors = c("Brazil" = "v_CA21_4461",
                                    "Colombia" = "v_CA21_4464",
                                    "El Salvador" = "v_CA21_4467",
                                    "Guyana" = "v_CA21_4470",
                                    "Haiti" = "v_CA21_4473",
                                    "Jamaica" = "v_CA21_4476",
                                    "Mexico" = "v_CA21_4479",
                                    "Peru" = "v_CA21_4482",
                                    "Trinidad and Tobago" = "v_CA21_4485",
                                    "Other in Americas" = "v_CA21_4491")) |>
  mutate(number = convert_pct((Brazil + Mexico + Colombia + `El Salvador` + Guyana + Haiti +
           Jamaica + Peru + `Trinidad and Tobago` + `Other in Americas`) / imm_origin_total_count)) |> 
  pull(number)

recent_asia <- get_census(dataset = "CA21",
                          regions = list(CSD = 2465005),
                          level = "CSD",
                          vectors = c("Asie" = "v_CA21_4740")) |> 
  mutate(number = convert_pct(Asie / imm_origin_recent_count)) |> 
  pull(number)

recent_africa <- get_census(dataset = "CA21",
                          regions = list(CSD = 2465005),
                          level = "CSD",
                          vectors = c("Africa" = "v_CA21_4692")) |> 
  mutate(number = convert_pct(Africa / imm_origin_recent_count)) |> 
  pull(number)

recent_syria <- get_census(dataset = "CA21",
                           regions = list(CSD = 2465005),
                           level = "CSD",
                           vectors = c("country" = "v_CA21_4794")) |> 
  mutate(number = convert_pct(country / imm_origin_recent_count)) |> 
  pull(number)

recent_lebanon <- get_census(dataset = "CA21",
                             regions = list(CSD = 2465005),
                             level = "CSD",
                             vectors = c("country" = "v_CA21_4776")) |> 
  mutate(number = convert_pct(country / imm_origin_recent_count)) |> 
  pull(number)

recent_algeria <- get_census(dataset = "CA21",
                             regions = list(CSD = 2465005),
                             level = "CSD",
                             vectors = c("country" = "v_CA21_4695")) |> 
  mutate(number = convert_pct(country / imm_origin_recent_count)) |> 
  pull(number)

recent_haiti <- get_census(dataset = "CA21",
                             regions = list(CSD = 2465005),
                             level = "CSD",
                             vectors = c("country" = "v_CA21_4647")) |> 
  mutate(number = convert_pct(country / imm_origin_recent_count)) |> 
  pull(number)

recent_morocco <- get_census(dataset = "CA21",
                           regions = list(CSD = 2465005),
                           level = "CSD",
                           vectors = c("country" = "v_CA21_4719")) |> 
  mutate(number = convert_pct(country / imm_origin_recent_count)) |> 
  pull(number)

# Visible Minorities ------------------------------------------------------
#Grabbing the total for later proportion calculation
vis_total <- get_census(dataset = "CA21",
                        regions = list(CSD = 2465005),
                        level = "CSD",
                        vectors = c("total" = "v_CA21_4875")) |> 
  pull(total)

#Grabbing and cleaning the actual data
vis_min <- get_census(dataset = "CA21",
                      regions = list(CSD = 2465005),
                      level = "CSD",
                      vectors = c("Sud-Asiatique" = "v_CA21_4878",
                                  "Chinois" = "v_CA21_4881",
                                  "Noir" = "v_CA21_4884",
                                  "Philippin" = "v_CA21_4887",
                                  "Arabe" = "v_CA21_4890",
                                  "Latino-Américain" = "v_CA21_4893",
                                  "Asiatique du Sud-Est" = "v_CA21_4896",
                                  "Asiatique occidental" = "v_CA21_4899",
                                  "Coréen" = "v_CA21_4902",
                                  "Japonais" = "v_CA21_4905",
                                  "n.i.a." = "v_CA21_4908",
                                  "Multiples" = "v_CA21_4911")) |> 
  select("Sud-Asiatique", "Chinois", "Noir", "Philippin", "Arabe",
         "Latino-Américain", "Asiatique du Sud-Est", "Asiatique occidental",
         "Coréen", "Japonais", "n.i.a.", "Multiples") |> 
  pivot_longer(cols = everything(), names_to = "type", values_to = "count") |> 
  mutate(perc = convert_pct(count / vis_total),
         type = fct_reorder(type, count, .desc = TRUE))

#Graphing the data
vis_min_graph <- ggplot(data = vis_min, aes(x = type, y = count, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(), fill = "#A3B0D1") +
  geom_text(aes(label = perc), position = position_dodge(width = 0.9),
            vjust = -0.5, size = 4, color = "black") +
  scale_y_continuous(labels = convert_number) +
  labs(x = "Lieu de naissance",
       y = "Personnes") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_blank(),
        legend.title = element_blank(), text = element_text(family = "KMR Apparat Regular"),
        axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())

#Grabbing numbers for the text
vis_min_laval <- get_census(dataset = "CA21",
                      regions = list(CSD = 2465005),
                      level = "CSD",
                      vectors = c("total" = "v_CA21_4872", "vis" = "v_CA21_4875")) |> 
  mutate(number = convert_pct(vis / total)) |> 
  pull(number)

vis_min_quebec <- get_census(dataset = "CA21",
                             regions = list(PR = 24),
                             level = "PR",
                             vectors = c("total" = "v_CA21_4872", "vis" = "v_CA21_4875")) |> 
  mutate(number = convert_pct(vis / total)) |> 
  pull(number)

vis_min_arab <- get_census(dataset = "CA21",
                           regions = list(CSD = 2465005),
                           level = "CSD",
                           vectors = c("total" = "v_CA21_4872", "vis" = "v_CA21_4890")) |> 
  mutate(number = convert_pct(vis / total)) |> 
  pull(number)

vis_min_black <- get_census(dataset = "CA21",
                           regions = list(CSD = 2465005),
                           level = "CSD",
                           vectors = c("total" = "v_CA21_4872", "vis" = "v_CA21_4884")) |> 
  mutate(number = convert_pct(vis / total)) |> 
  pull(number)

# Religion ----------------------------------------------------------------
lvl_secular <- get_census(dataset = "CA21",
                          regions = list(CSD = 2465005),
                          level = "CSD",
                          vectors = c("total" = "v_CA21_5670", "religion" = "v_CA21_5742")) |> 
  mutate(number = convert_pct(religion / total)) |> 
  pull(number)

qc_secular <- get_census(dataset = "CA21",
                          regions = list(PR = 24),
                          level = "PR",
                          vectors = c("total" = "v_CA21_5670", "religion" = "v_CA21_5742")) |> 
  mutate(number = convert_pct(religion / total)) |> 
  pull(number)

lvl_christ <- get_census(dataset = "CA21",
                          regions = list(CSD = 2465005),
                          level = "CSD",
                          vectors = c("total" = "v_CA21_5670", "religion" = "v_CA21_5676")) |> 
  mutate(number = convert_pct(religion / total)) |> 
  pull(number)

qc_christ <- get_census(dataset = "CA21",
                         regions = list(PR = 24),
                         level = "PR",
                         vectors = c("total" = "v_CA21_5670", "religion" = "v_CA21_5676")) |> 
  mutate(number = convert_pct(religion / total)) |> 
  pull(number)

lvl_islam <- get_census(dataset = "CA21",
                         regions = list(CSD = 2465005),
                         level = "CSD",
                         vectors = c("total" = "v_CA21_5670", "religion" = "v_CA21_5730")) |> 
  mutate(number = convert_pct(religion / total)) |> 
  pull(number)

qc_islam <- get_census(dataset = "CA21",
                        regions = list(PR = 24),
                        level = "PR",
                        vectors = c("total" = "v_CA21_5670", "religion" = "v_CA21_5730")) |> 
  mutate(number = convert_pct(religion / total)) |> 
  pull(number)

# R Markdown --------------------------------------------------------------
ggplot2::ggsave(filename = here::here("output/axe1/immigration/imm_evol_graph.png"), 
                plot = imm_evol_graph, width = 8, height = 6)
ggplot2::ggsave(filename = here::here("output/axe1/immigration/imm_prop_map.png"), 
                plot = imm_prop_map, width = 8, height = 6)
ggplot2::ggsave(filename = here::here("output/axe1/immigration/recimm_prop_map.png"), 
                plot = recimm_prop_map, width = 8, height = 6)
ggplot2::ggsave(filename = here::here("output/axe1/immigration/period_imm_graph.png"), 
                plot = period_imm_graph, width = 8, height = 6)
ggplot2::ggsave(filename = here::here("output/axe1/immigration/ad_cat_graph.png"), 
                plot = ad_cat_graph, width = 8, height = 6)
ggplot2::ggsave(filename = here::here("output/axe1/immigration/imm_age_sex_graph.png"), 
                plot = imm_age_sex_graph, width = 8, height = 6)
ggplot2::ggsave(filename = here::here("output/axe1/immigration/imm_age_sex_prop_graph.png"), 
                plot = imm_age_sex_prop_graph, width = 8, height = 6)
ggplot2::ggsave(filename = here::here("output/axe1/immigration/imm_age_graph.png"), 
                plot = imm_age_graph, width = 8, height = 6)
ggplot2::ggsave(filename = here::here("output/axe1/immigration/imm_origin_graph.png"), 
                plot = imm_origin_graph, width = 8, height = 6)
ggplot2::ggsave(filename = here::here("output/axe1/immigration/vis_min_graph.png"), 
                plot = vis_min_graph, width = 8, height = 6)
ggplot2::ggsave(filename = here::here("output/axe1/immigration/imm_stackedage_graph.png"), 
                plot = imm_stackedage_graph, width = 8, height = 6)

qs::qsavem(imm_evol_graph, imm_21_lvl_prop, imm_21_mtl_prop, imm_21_qc_prop,
           imm_prop_map, imm_table, non_res_prop, recimm_prop_map, CanadianCitizensMtl,
           CanadianCitizensQc, CanadianCitizensLaval, period_imm_graph, ad_cat_graph,
           laval_ad_cat, imm_age_sex_graph, imm_age_sex_prop_graph, imm_age_graph, imm_stackedage_graph,
           imm_origin_graph, imm_asia, imm_africa, imm_europe, imm_CCSLA, recent_asia,
           recent_africa, recent_syria, recent_lebanon, recent_algeria, recent_haiti,
           recent_morocco, vis_min_graph, vis_min_laval, vis_min_quebec, vis_min_arab,
           vis_min_black, lvl_secular, qc_secular, lvl_christ, qc_christ, lvl_islam,
           qc_islam,
           file = "D://McGill/can_cache/data/immigration.qsm")
