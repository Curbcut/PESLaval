#Loading libraries

source("R/01_startup.R")

#Setting CensusMapper API Key because it won't save
set_cancensus_api_key("CensusMapper_4308d496f011429cf814385050f083dc")

# Employment Data 2021 ---------------------------------------------------------
#Vectors for 2021 total population 15+ by labour force (lf) status, employed, and unemployed (25% sample data)
lf_vectors_21 <- c("lf_pop" = "v_CA21_6492", "tot_empl" = "v_CA21_6498", "tot_unempl" = "v_CA21_6501")

#Vector with the given CanCensus names from lf_vectors_21
lf_names_21 <- c("lf_pop", "tot_empl", "tot_unempl")

#Grab lf_vectors_21 data for Laval and Montreal and cleans up the table
lf_lvl_mtl_21  <- get_census(dataset = "CA21", 
                              regions = list(CSD = c(2465005, 2466023)), 
                              level = "CSD",
                              vectors = lf_vectors_21) |> 
  select(all_of(lf_names_21)) |> 
  mutate(Geography = c("Laval", "Montreal")) |> 
  select(Geography, everything())

#Grab lf_vectors_21 data for the Montreal CMA and cleans up the table
lf_mtl_cma_21 <- get_census(dataset = "CA21", 
                                  regions = list(CMA = 24462), 
                                  level = "CMA",
                                  vectors = lf_vectors_21) |> 
  select(all_of(lf_names_21)) |> 
  mutate(Geography = "Montreal CMA") |> 
  select(Geography, everything())

#Grab lf_vectors_21 data for the province of Quebec and cleans up the table
lf_qc_21  <- get_census(dataset = "CA21", 
                           regions = list(PR = 24), 
                           level = "PR",
                           vectors = lf_vectors_21) |> 
  select(all_of(lf_names_21)) |> 
  mutate(Geography = "Quebec") |> 
  select(Geography, everything())

#Combines the data and calculates (un)employment rate, and then cleans up the table
lf_combined_21 <- bind_rows(lf_lvl_mtl_21, lf_mtl_cma_21, lf_qc_21) |> 
  mutate(empl_rate_21 = round(tot_empl * 100 / lf_pop, 1),
         unempl_rate_21 = round(tot_unempl*100 / lf_pop, 1)) |> 
  select(Geography, empl_rate_21, unempl_rate_21)

# Employment data 2016 --------------------------------------------------
#Vectors for 2016 employment and unemployment rate
lf_vectors_16 <- c("empl_rate_16" = "v_CA16_5615",  "unempl_rate_16" = "v_CA16_5618")

#2016 variable names for censuses that have employment and unemployment rate
lf_names_16 <- c("empl_rate_16", "unempl_rate_16")

#Grab lf_vectors_16 data for Laval and Montreal and cleans up the table
lf_lvl_mtl_16  <- get_census(dataset = "CA16", 
                           regions = list(CSD = c(2465005, 2466023)), 
                           level = "CSD",
                           vectors = lf_vectors_16) |> 
  select(all_of(lf_names_16)) |> 
  mutate(Geography = c("Laval", "Montreal")) |> 
  select(Geography, everything())

#Grab lf_vectors_16 data for the Montreal CMA and cleans up the table
lf_mtl_cma_16 <- get_census(dataset = "CA16", 
                            regions = list(CMA = 24462), 
                            level = "CMA",
                            vectors = lf_vectors_16) |> 
  select(all_of(lf_names_16)) |> 
  mutate(Geography = "Montreal CMA") |> 
  select(Geography, everything())

#Grab lf_vectors_16 data for the province of Quebec and cleans up the table
lf_qc_16  <- get_census(dataset = "CA16", 
                        regions = list(PR = 24), 
                        level = "PR",
                        vectors = lf_vectors_16) |> 
  select(all_of(lf_names_16)) |> 
  mutate(Geography = "Quebec") |> 
  select(Geography, everything())

#Combine 2016 data into one table
lf_combined_16 <- bind_rows(lf_lvl_mtl_16, lf_mtl_cma_16, lf_qc_16)

# Employment data 2011 --------------------------------------------------
#Vectors for 2011 employment and unemployment rate
lf_vectors_11 <- c("empl_rate_11" = "v_CA11N_2005", "unempl_rate_11" = "v_CA11N_2008")

#2011 variable names for censuses that have employment and unemployment rate
lf_names_11 <- c("empl_rate_11", "unempl_rate_11")

#Grab lf_vectors_11 data for Laval and Montreal and cleans up the table
lf_lvl_mtl_11  <- get_census(dataset = "CA11", 
                             regions = list(CSD = c(2465005, 2466023)), 
                             level = "CSD",
                             vectors = lf_vectors_11) |> 
  select(all_of(lf_names_11)) |> 
  mutate(Geography = c("Laval", "Montreal")) |> 
  select(Geography, everything())

#Grab lf_vectors_11 data for the Montreal CMA and cleans up the table
lf_mtl_cma_11 <- get_census(dataset = "CA11", 
                            regions = list(CMA = 24462), 
                            level = "CMA",
                            vectors = lf_vectors_11) |> 
  select(all_of(lf_names_11)) |> 
  mutate(Geography = "Montreal CMA") |> 
  select(Geography, everything())

#Grab lf_vectors_11 data for the province of Quebec and cleans up the table
lf_qc_11  <- get_census(dataset = "CA11", 
                        regions = list(PR = 24), 
                        level = "PR",
                        vectors = lf_vectors_11) |> 
  select(all_of(lf_names_11)) |> 
  mutate(Geography = "Quebec") |> 
  select(Geography, everything())

#Combine 2011 data into one table
lf_combined_11 <- bind_rows(lf_lvl_mtl_11, lf_mtl_cma_11, lf_qc_11)

# Employment data 2006 --------------------------------------------------
#Vectors for 2006 employment and unemployment rate
lf_vectors_06 <- c("empl_rate_06" = "v_CA06_581", "unempl_rate_06" = "v_CA06_582")

#2006 variable names for censuses that have employment and unemployment rate
lf_names_06 <- c("empl_rate_06", "unempl_rate_06")

#Grab lf_vectors_06 data for Laval and Montreal and cleans up the table
lf_lvl_mtl_06  <- get_census(dataset = "CA06", 
                             regions = list(CSD = c(2465005, 2466023)), 
                             level = "CSD",
                             vectors = lf_vectors_06) |> 
  select(all_of(lf_names_06)) |> 
  mutate(Geography = c("Laval", "Montreal")) |> 
  select(Geography, everything())

#Grab lf_vectors_06 data for the Montreal CMA and cleans up the table
lf_mtl_cma_06 <- get_census(dataset = "CA06", 
                            regions = list(CMA = 24462), 
                            level = "CMA",
                            vectors = lf_vectors_06) |> 
  select(all_of(lf_names_06)) |> 
  mutate(Geography = "Montreal CMA") |> 
  select(Geography, everything())

#Grab lf_vectors_06 data for the province of Quebec and cleans up the table
lf_qc_06  <- get_census(dataset = "CA06", 
                        regions = list(PR = 24), 
                        level = "PR",
                        vectors = lf_vectors_06) |> 
  select(all_of(lf_names_06)) |> 
  mutate(Geography = "Quebec") |> 
  select(Geography, everything())

#Combine 2006 data into one table
lf_combined_06 <- bind_rows(lf_lvl_mtl_06, lf_mtl_cma_06, lf_qc_06)

# Employment data 2001 --------------------------------------------------
#Vectors for 2001 employment and unemployment rate
lf_vectors_01 <- c("empl_rate_01" = "v_CA01_741", "unempl_rate_01" = "v_CA01_742")

#2001 variable names for censuses that have employment and unemployment rate
lf_names_01 <- c("empl_rate_01", "unempl_rate_01")

#Grab lf_vectors_01 data for Laval and Montreal and cleans up the table
lf_lvl_mtl_01  <- get_census(dataset = "CA01", 
                             regions = list(CSD = c(2465005, 2466025)), 
                             level = "CSD",
                             vectors = lf_vectors_01) |> 
  select(all_of(lf_names_01)) |> 
  mutate(Geography = c("Laval", "Montreal")) |> 
  select(Geography, everything())

#Grab lf_vectors_01 data for the Montreal CMA and cleans up the table
lf_mtl_cma_01 <- get_census(dataset = "CA01", 
                            regions = list(CMA = 24462), 
                            level = "CMA",
                            vectors = lf_vectors_01) |> 
  select(all_of(lf_names_01)) |> 
  mutate(Geography = "Montreal CMA") |> 
  select(Geography, everything())

#Grab lf_vectors_01 data for the province of Quebec and cleans up the table
lf_qc_01  <- get_census(dataset = "CA01", 
                        regions = list(PR = 24), 
                        level = "PR",
                        vectors = lf_vectors_01) |> 
  select(all_of(lf_names_01)) |> 
  mutate(Geography = "Quebec") |> 
  select(Geography, everything())

#Combine 2001 data into one table
lf_combined_01 <- bind_rows(lf_lvl_mtl_01, lf_mtl_cma_01, lf_qc_01)

# Employment Table and Graphs ---------------------------------------------
#Combines 2001-2021 employment data into one table
lf_combined <- lf_combined_01 |> 
  full_join(lf_combined_06, by = "Geography") |> 
  full_join(lf_combined_11, by = "Geography") |> 
  full_join(lf_combined_16, by = "Geography") |> 
  full_join(lf_combined_21, by = "Geography")

#Clean-up now unused variables
rm(lf_combined_01, lf_combined_06, lf_combined_11, lf_combined_16, lf_combined_21,
   lf_lvl_mtl_01, lf_lvl_mtl_06, lf_lvl_mtl_11, lf_lvl_mtl_16, lf_lvl_mtl_21,
   lf_mtl_cma_01, lf_mtl_cma_06, lf_mtl_cma_11, lf_mtl_cma_16, lf_mtl_cma_21,
   lf_qc_01, lf_qc_06, lf_qc_11, lf_qc_16, lf_qc_21,
   lf_names_01, lf_names_06, lf_names_11, lf_names_16, lf_names_21,
   lf_vectors_01, lf_vectors_06, lf_vectors_11, lf_vectors_16, lf_vectors_21)

#Creating the employment rate table and pivoting it to make it longer
employment_rate <- lf_combined |> 
  select(Geography, empl_rate_01, empl_rate_06, empl_rate_11, empl_rate_16, empl_rate_21) |> 
  rename("2001" = empl_rate_01, "2006" = empl_rate_06, "2011" = empl_rate_11, "2016" = empl_rate_16, "2021" = empl_rate_21) |> 
  pivot_longer(cols = -Geography, names_to = "Year", values_to = "Value")


#Creating the line graph for employment rate
ggplot(employment_rate, aes(x = as.factor(Year), y = Value, color = Geography, group = Geography)) +
  geom_line() +
  labs(title = "Employment Rate from 2001 to 2006", x = "Year", y = "Employment Rate (%)", color = "Geography") +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank())

#Creating the unemployment rate table and pivoting it to make it longer
unemployment_rate <- lf_combined |> 
  select(Geography, unempl_rate_01, unempl_rate_06, unempl_rate_11, unempl_rate_16, unempl_rate_21) |> 
  rename("2001" = unempl_rate_01, "2006" = unempl_rate_06, "2011" = unempl_rate_11, "2016" = unempl_rate_16, "2021" = unempl_rate_21) |> 
  pivot_longer(cols = -Geography, names_to = "Year", values_to = "Value")


#Creating the line graph for employment rate
ggplot(unemployment_rate, aes(x = as.factor(Year), y = Value, color = Geography, group = Geography)) +
  geom_line() +
  labs(title = "Unemployment Rate from 2001 to 2006", x = "Year", y = "Unemployment Rate (%)", color = "Geography") +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank())

# Workplace category ------------------------------------------------------
#Grab 2021 workplace category data for Laval, clean it up, and pivot it into a longer format
workplace_lvl  <- get_census(dataset = "CA21", 
                             regions = list(CSD = 2465005), 
                             level = "CSD",
                             vectors = c("total" = "v_CA21_7602", "home" = "v_CA21_7605", "outside_can" = "v_CA21_7608",
                                         "no_address" = "v_CA21_7611", "fixed_address" = "v_CA21_7614")) |> 
  select(all_of(c("home", "outside_can", "no_address", "fixed_address"))) |>
  select(everything()) |> 
  pivot_longer(everything(), names_to = "location", values_to = "count")

#Legend Labels
workplace_labels <- c("home" = "Worked at Home",
                      "outside_can" = "Worked Outside of Canada",
                      "no_address" = "No Fixed Workplace Address",
                      "fixed_address" = "Usual Place of Work")

#Creating the pie chart
ggplot(workplace_lvl, aes(x = "", y = count, fill = location)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a"),
    labels = workplace_labels) +
  labs(title = "2021 Workplace Category") +
  theme(legend.position = "right", legend.title = element_blank())