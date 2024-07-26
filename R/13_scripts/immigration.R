source("R/01_startup.R")
library(scales)
library(readxl)
library(gt)

#Setting CensusMapper API Key because it won't save
set_cancensus_api_key("CensusMapper_4308d496f011429cf814385050f083dc")

#Caching census data to reduce amount of calls and speed up process.
#Personal use only, change the folder to your own folder if you want to use it
set_cancensus_cache_path("D:/McGill/can_cache", install = TRUE, overwrite = TRUE)
set_cancensus_cache_path("/Users/justin/Documents/R/CurbCutSelf")

#Curbcut scale
curbcut_scale <- c("#C4CDE1", "#98A8CB", "#6C83B5", "#4C5C7F", "#2B3448")

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

ggplot(data = immigrant_decade_queb_percent, aes(x = name, y = value)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Decade", y = "Percent", title = "Period of Immigration Quebec")

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
  mutate(Economic = `Economic Immigrant`/ Total,
         Family = `Family Sponsored`/Total,
         Refugee = `Refugees`/Total,
         Other = Other/Total) |> 
  pivot_longer(-c(GeoUID:CMA_UID))


#slice data to just select percent columns
immigrant_admissioncat_percent_plot <- immigrant_admissioncat_percent  |> 
  slice(c(5:8))


immigrant_admissioncat_percent_plot|> 
  ggplot(aes(x = name, y = value)) +
  geom_col() +
  labs(x = "Admission Category", y = "Count")


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
  mutate(Economic = `Economic Immigrant`/ Total*100,
         Family = `Family Sponsored`/Total*100,
         Refugee = `Refugees`/Total*100,
         Other = Other/Total*100)

immigrant_admissioncat_percent_qc <- immigrant_admissioncat_percent_qc |> 
  pivot_longer(-c(GeoUID:C_UID))



#slice data to just select percent columns
immigrant_admissioncat_percent_plot_qc <- immigrant_admissioncat_percent_qc  |> 
  slice(c(5:8))


immigrant_admissioncat_percent_plot_qc|> 
  ggplot(aes(x = name, y = value)) +
  geom_col() +
  labs(x = "Admission Category", y = "Count")

# combine with laval

admission_cat_combined <- bind_rows(immigrant_admissioncat_percent_plot, immigrant_admissioncat_percent_plot_qc)

ggplot(data = admission_cat_combined, aes(x = name, y = value, fill = `Region Name`)) +
  geom_col(position = "dodge") +
  labs(y = "Percent", x = "Admission Category", title = "Percentage of Immigrants by Admission Category")

# R Markdown --------------------------------------------------------------
ggplot2::ggsave(filename = here::here("output/axe1/immigration/imm_evol_graph.png"), 
                plot = imm_evol_graph, width = 8, height = 6)
ggplot2::ggsave(filename = here::here("output/axe1/immigration/imm_prop_map.png"), 
                plot = imm_prop_map, width = 8, height = 6)
ggplot2::ggsave(filename = here::here("output/axe1/immigration/recimm_prop_map.png"), 
                plot = recimm_prop_map, width = 8, height = 6)
ggplot2::ggsave(filename = here::here("output/axe1/immigration/period_imm_graph.png"), 
                plot = period_imm_graph, width = 8, height = 6)

qs::qsavem(imm_evol_graph, imm_21_lvl_prop, imm_21_mtl_prop, imm_21_qc_prop,
           imm_prop_map, imm_table, non_res_prop, recimm_prop_map, CanadianCitizensMtl,
           CanadianCitizensQc, CanadianCitizensLaval, period_imm_graph,
           file = "D://McGill/can_cache/data/immigration.qsm")
