#Loading libraries
source("R/01_startup.R")
library(scales)
library(readxl)

#Setting CensusMapper API Key because it won't save
set_cancensus_api_key("CensusMapper_4308d496f011429cf814385050f083dc")

#Caching census data to reduce amount of calls and speed up process.
#Personal use only, change the folder to your own folder if you want to use it
set_cancensus_cache_path("D:/McGill/can_cache", install = TRUE, overwrite = TRUE)
set_cancensus_cache_path("/Users/justin/Documents/R/CurbCutSelf")

# Employment Data 2021 ---------------------------------------------------------
#Vectors for 2021 total population 15+ by labour force (lf) status, employed, and unemployed (25% sample data)
lf_vectors_21 <- c("tot_pop" = "v_CA21_6492", "lf_pop" = "v_CA21_6495",
                   "tot_empl" = "v_CA21_6498", "tot_unempl" = "v_CA21_6501")

#Vector with the given CanCensus names from lf_vectors_21
lf_names_21 <- c("tot_pop", "lf_pop", "tot_empl", "tot_unempl")

#Grab lf_vectors_21 data for Laval and Montreal and cleans up the table
lf_lvl_mtl_21  <- get_census(dataset = "CA21", 
                              regions = list(CSD = c(2465005, 2466023)), 
                              level = "CSD",
                              vectors = lf_vectors_21) |> 
  select(all_of(lf_names_21)) |> 
  mutate(Geography = c("Laval", "Montreal")) |> 
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
lf_combined_21 <- bind_rows(lf_lvl_mtl_21, lf_qc_21) |> 
  mutate(empl_rate_21 = round(tot_empl * 100 / tot_pop, 1),
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
lf_combined_16 <- bind_rows(lf_lvl_mtl_16, lf_qc_16)

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
lf_combined_11 <- bind_rows(lf_lvl_mtl_11, lf_qc_11)

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
lf_combined_06 <- bind_rows(lf_lvl_mtl_06, lf_qc_06)

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
lf_combined_01 <- bind_rows(lf_lvl_mtl_01, lf_qc_01)

# Employment Table and Graphs ---------------------------------------------
#Combines 2001-2021 employment data into one table
lf_combined <- lf_combined_01 |> 
  full_join(lf_combined_06, by = "Geography") |> 
  full_join(lf_combined_11, by = "Geography") |> 
  full_join(lf_combined_16, by = "Geography") |> 
  full_join(lf_combined_21, by = "Geography")

#Creating the employment rate table and pivoting it to make it longer
employment_rate <- lf_combined |> 
  select(Geography, empl_rate_01, empl_rate_06, empl_rate_11, empl_rate_16, empl_rate_21) |> 
  rename("2001" = empl_rate_01, "2006" = empl_rate_06, "2011" = empl_rate_11, "2016" = empl_rate_16, "2021" = empl_rate_21) |> 
  pivot_longer(cols = -Geography, names_to = "Year", values_to = "Value")


#Creating the line graph for employment rate
ggplot(employment_rate, aes(x = as.factor(Year), y = Value, color = Geography, group = Geography)) +
  geom_line(size = 1.5) +
  labs(title = "Taux d'emploi de 2001 à 2021", x = "Année",
       y = "Taux d'emploi (%)", color = "Geography") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal",
        legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

#Creating the unemployment rate table and pivoting it to make it longer
unemployment_rate <- lf_combined |> 
  select(Geography, unempl_rate_01, unempl_rate_06, unempl_rate_11, unempl_rate_16, unempl_rate_21) |> 
  rename("2001" = unempl_rate_01, "2006" = unempl_rate_06, "2011" = unempl_rate_11, "2016" = unempl_rate_16, "2021" = unempl_rate_21) |> 
  pivot_longer(cols = -Geography, names_to = "Year", values_to = "Value")


#Creating the line graph for unemployment rate
ggplot(unemployment_rate, aes(x = as.factor(Year), y = Value, color = Geography, group = Geography)) +
  geom_line(size = 1.5) +
  labs(title = "Taux de chômage de 2001 à 2021", x = "Année",
       y = "Taux de chômage (%)", color = "Geography") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal",
        legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

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
  scale_fill_manual(values = c("aquamarine4", "sienna3", "mediumpurple3", "hotpink2"),
    labels = workplace_labels) +
  labs(title = "2021 Workplace Category") +
  theme(legend.position = "right", legend.title = element_blank())

# Activity Situation ------------------------------------------------------
#Note this section is only for reference. Only a table will be created
#Vectors for 2021 activity situation (total, in labour force(lf), employed, unemployed, not in lf)
act_total <- c("total" = "v_CA21_6492", "in_lf" = "v_CA21_6495", "employed" = "v_CA21_6498",
               "unemployed" = "v_CA21_6501", "not_lf" = "v_CA21_6504")
act_men <- c("total" = "v_CA21_6493", "in_lf" = "v_CA21_6496", "employed" = "v_CA21_6499",
             "unemployed" = "v_CA21_6502", "not_lf" = "v_CA21_6505")
act_women <- c("total" = "v_CA21_6494", "in_lf" = "v_CA21_6497", "employed" = "v_CA21_6500",
               "unemployed" = "v_CA21_6503", "not_lf" = "v_CA21_6506")

#Vector with the given names for activity vectors above
act_names <- c("total", "in_lf", "employed", "unemployed", "not_lf")

#Grabbing Laval-wide activity situation data
act_lvl_total  <- get_census(dataset = "CA21", 
                             regions = list(CSD = 2465005), 
                             level = "CSD",
                             vectors = act_total) |> 
  select(all_of(act_names)) |> 
  mutate(Type = "Laval") |> 
  select(Type, everything())

#Grabbing men-only activity situation data
act_lvl_men  <- get_census(dataset = "CA21", 
                             regions = list(CSD = 2465005), 
                             level = "CSD",
                             vectors = act_men) |> 
  select(all_of(act_names)) |> 
  mutate(Type = "Men") |> 
  select(Type, everything())

#Grabbing women-only activity situation data
act_lvl_women  <- get_census(dataset = "CA21", 
                           regions = list(CSD = 2465005), 
                           level = "CSD",
                           vectors = act_women) |> 
  select(all_of(act_names)) |> 
  mutate(Type = "Women") |> 
  select(Type, everything())

#Bind the three tables together
activity_situation <- bind_rows(act_lvl_total, act_lvl_men, act_lvl_women)

# Working During Reference Year -------------------------------------------
#Vectors for 2021 working during reference year, for total count, did not work, worked,
#full time and full year, part time or part year, and average weeks worked
wref_total <- c("total" = "v_CA21_6516", "no_work" = "v_CA21_6519", "worked" = "v_CA21_6522",
                "full" = "v_CA21_6525", "part" = "v_CA21_6528", "avg_wks" = "v_CA21_6531")
wref_men <- c("total" = "v_CA21_6517", "no_work" = "v_CA21_6520", "worked" = "v_CA21_6523",
              "full" = "v_CA21_6526", "part" = "v_CA21_6529", "avg_wks" = "v_CA21_6532")
wref_women <- c("total" = "v_CA21_6518", "no_work" = "v_CA21_6521", "worked" = "v_CA21_6524",
                "full" = "v_CA21_6527", "part" = "v_CA21_6530", "avg_wks" = "v_CA21_6533")

#Vector with the given names for reference year vectors above
wref_names <- c("total", "no_work", "worked", "full", "part", "avg_wks")

#Grabbing Laval-wide work during reference year data
wref_lvl_total  <- get_census(dataset = "CA21", 
                             regions = list(CSD = 2465005), 
                             level = "CSD",
                             vectors = wref_total) |> 
  select(all_of(wref_names)) |> 
  mutate(Type = "Laval") |> 
  select(Type, everything())

#Grabbing men work during reference year data
wref_lvl_men  <- get_census(dataset = "CA21", 
                              regions = list(CSD = 2465005), 
                              level = "CSD",
                              vectors = wref_men) |> 
  select(all_of(wref_names)) |> 
  mutate(Type = "Men") |> 
  select(Type, everything())

#Grabbing women work during reference year data
wref_lvl_women  <- get_census(dataset = "CA21", 
                            regions = list(CSD = 2465005), 
                            level = "CSD",
                            vectors = wref_women) |> 
  select(all_of(wref_names)) |> 
  mutate(Type = "Women") |> 
  select(Type, everything())

#Bind the three tables together
work_ref_year <- bind_rows(wref_lvl_total, wref_lvl_men, wref_lvl_women)

#Prepares work_ref_year to be used in creating the graph
wref_table_data <- work_ref_year |> 
  select(-total, -worked, -avg_wks) |> 
  pivot_longer(cols = -Type, names_to = "Status", values_to = "Count") |> 
  mutate(Status = factor(Status, levels = c("full", "part", "no_work")))

#Graph creation
ggplot(wref_table_data, aes(x = Status, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Work Status by Type", x = "", y = "Count") +
  scale_fill_manual(values = c("Laval" = "royalblue2", 
                               "Men" = "indianred", 
                               "Women" = "gold2"),
                    name = "Status",
                    ) +
  scale_x_discrete(labels = c("full" = "Full-Time", 
                              "part" = "Part-Time", 
                              "no_work" = "No Work")) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())

# Category of Worker ------------------------------------------------------
#Vectors for 2021 work category, for total, n/a, all category of workers,
#employees, permanent workers, temporary workers, fixed term, seasonal workers,
#and self-employed
cat_total <- c("total" = "v_CA21_6534", "na" = "v_CA21_6537", "allworkers" = "v_CA21_6540",
               "employee" = "v_CA21_6543", "permanent" = "v_CA21_6546", "temp" = "v_CA21_6549",
               "fixed" = "v_CA21_6552", "seasonal" = "v_CA21_6555", "self" = "v_CA21_6558")
cat_men <- c("total" = "v_CA21_6535", "na" = "v_CA21_6538", "allworkers" = "v_CA21_6541",
               "employee" = "v_CA21_6544", "permanent" = "v_CA21_6547", "temp" = "v_CA21_6550",
               "fixed" = "v_CA21_6553", "seasonal" = "v_CA21_6556", "self" = "v_CA21_6559")
cat_women <- c("total" = "v_CA21_6536", "na" = "v_CA21_6539", "allworkers" = "v_CA21_6542",
               "employee" = "v_CA21_6545", "permanent" = "v_CA21_6548", "temp" = "v_CA21_6551",
               "fixed" = "v_CA21_6554", "seasonal" = "v_CA21_6557", "self" = "v_CA21_6560")

#Vector with the given names for category of worker vectors
cat_names <- c("total", "na", "allworkers", "employee", "permanent",
               "temp", "fixed", "seasonal", "self")

#Grabbing Laval-wide category of workers
cat_lvl_total  <- get_census(dataset = "CA21", 
                            regions = list(CSD = 2465005), 
                            level = "CSD",
                            vectors = cat_total) |> 
  select(all_of(cat_names)) |> 
  mutate(Type = "Total") |> 
  select(Type, everything())

#Grabbing men category of workers
cat_lvl_men  <- get_census(dataset = "CA21", 
                             regions = list(CSD = 2465005), 
                             level = "CSD",
                             vectors = cat_men) |> 
  select(all_of(cat_names)) |> 
  mutate(Type = "Men") |> 
  select(Type, everything())

#Grabbing women category of workers
cat_lvl_women  <- get_census(dataset = "CA21", 
                             regions = list(CSD = 2465005), 
                             level = "CSD",
                             vectors = cat_women) |> 
  select(all_of(cat_names)) |> 
  mutate(Type = "Women") |> 
  select(Type, everything())

#Bind the three tables together
worker_category <- bind_rows(cat_lvl_total, cat_lvl_men, cat_lvl_women)

#Prepping the data to create graphs
work_cat_table <- worker_category |> 
  select(-total, -allworkers, -employee, -fixed, -seasonal, -Type) |> 
  slice(-c(2, 3)) |> 
  pivot_longer(cols = everything(), names_to = "category", values_to = "count")

#Pie chart for category of workers
ggplot(work_cat_table, aes(x = "", y = count, fill = category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "2021 Category of Workers including Permaence of Employment") +
  scale_fill_discrete(labels = c("na" = "Not Applicable",
                                 "permanent" = "Permanent",
                                 "temp" = "Temporary",
                                 "self" = "Self Employed")) +
  theme(
    legend.position = "bottom", legend.title = element_blank()
  )

# Occupation Major Category -----------------------------------------------
#Vectors for 2021 NOC categories
noc_total <- c("total" = "v_CA21_6561", "na" = "v_CA21_6564", "all" = "v_CA21_6567",
               "0" = "v_CA21_6570", "1" = "v_CA21_6573", "2" = "v_CA21_6576",
               "3" = "v_CA21_6579", "4" = "v_CA21_6582", "5" = "v_CA21_6585",
               "6" = "v_CA21_6588", "7" = "v_CA21_6591", "8" = "v_CA21_6594",
               "9" = "v_CA21_6597")
noc_men <- c("total" = "v_CA21_6562", "na" = "v_CA21_6565", "all" = "v_CA21_6568",
               "0" = "v_CA21_6571", "1" = "v_CA21_6574", "2" = "v_CA21_6577",
               "3" = "v_CA21_6580", "4" = "v_CA21_6583", "5" = "v_CA21_6586",
               "6" = "v_CA21_6589", "7" = "v_CA21_6592", "8" = "v_CA21_6595",
               "9" = "v_CA21_6598")
noc_women <- c("total" = "v_CA21_6563", "na" = "v_CA21_6566", "all" = "v_CA21_6569",
               "0" = "v_CA21_6572", "1" = "v_CA21_6575", "2" = "v_CA21_6578",
               "3" = "v_CA21_6582", "4" = "v_CA21_6584", "5" = "v_CA21_6587",
               "6" = "v_CA21_6590", "7" = "v_CA21_6593", "8" = "v_CA21_6596",
               "9" = "v_CA21_6599")


noc_total16 <- c("total" = "v_CA16_5654", "na" = "v_CA16_5657", "all" = "v_CA16_5660",
                "0" = "v_CA16_5663", "1" = "v_CA16_5666", "2" = "v_CA16_5669",
                "3" = "v_CA16_5672", "4" = "v_CA16_5675", "5" = "v_CA16_5678",
                "6" = "v_CA16_5681", "7" = "v_CA16_5684", "8" = "v_CA16_5687",
                "9" = "v_CA16_5690")
noc_men16 <- c("total" = "v_CA16_5655", "na" = "v_CA16_5658", "all" = "v_CA16_5661",
                "0" = "v_CA16_5664", "1" = "v_CA16_5667", "2" = "v_CA16_5670",
                "3" = "v_CA16_5673", "4" = "v_CA16_5676", "5" = "v_CA16_5679",
                "6" = "v_CA16_5682", "7" = "v_CA16_5685", "8" = "v_CA16_5688",
                "9" = "v_CA16_5691")
noc_women16 <- c("total" = "v_CA16_5656", "na" = "v_CA16_5659", "all" = "v_CA16_5662",
                 "0" = "v_CA16_5665", "1" = "v_CA16_5668", "2" = "v_CA16_5671",
                 "3" = "v_CA16_5674", "4" = "v_CA16_5677", "5" = "v_CA16_5680",
                 "6" = "v_CA16_5683", "7" = "v_CA16_5686", "8" = "v_CA16_5689",
                 "9" = "v_CA16_5692")


#Vector with the given names for the NOC occupations vectors above
noc_names <- c("total", "na", "all", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")

#Grabbing Laval-wide NOC occupations
noc_lvl_total  <- get_census(dataset = "CA21", 
                             regions = list(CSD = 2465005), 
                             level = "CSD",
                             vectors = noc_total) |> 
  select(all_of(noc_names)) |> 
  mutate(Type = "Total") |> 
  select(Type, everything())

#Grabbing men NOC occupations
noc_lvl_men  <- get_census(dataset = "CA21", 
                             regions = list(CSD = 2465005), 
                             level = "CSD",
                             vectors = noc_men) |> 
  select(all_of(noc_names)) |> 
  mutate(Type = "Men") |> 
  select(Type, everything())

#Grabbing women NOC occupations
noc_lvl_women  <- get_census(dataset = "CA21", 
                           regions = list(CSD = 2465005), 
                           level = "CSD",
                           vectors = noc_women) |> 
  select(all_of(noc_names)) |> 
  mutate(Type = "Women") |> 
  select(Type, everything())

#Grabbing Laval-wide NOC occupations 2016
noc_lvl_total16 <- get_census(dataset = "CA16", 
                             regions = list(CSD = 2465005), 
                             level = "CSD",
                             vectors = noc_total16) |> 
  select(all_of(noc_names)) |> 
  mutate(Type = "Total") |> 
  select(Type, everything())

#Grabbing men NOC occupations 2016
noc_lvl_men16 <- get_census(dataset = "CA16", 
                           regions = list(CSD = 2465005), 
                           level = "CSD",
                           vectors = noc_men16) |> 
  select(all_of(noc_names)) |> 
  mutate(Type = "Men") |> 
  select(Type, everything())

#Grabbing women NOC occupations
noc_lvl_women16 <- get_census(dataset = "CA16", 
                             regions = list(CSD = 2465005), 
                             level = "CSD",
                             vectors = noc_women16) |> 
  select(all_of(noc_names)) |> 
  mutate(Type = "Women") |> 
  select(Type, everything())

#Binding the NOC tables together
noc_occupation <- bind_rows(noc_lvl_total, noc_lvl_men, noc_lvl_women)
noc_occupation16 <- bind_rows(noc_lvl_total16, noc_lvl_men16, noc_lvl_women16)

#Exporting the tables so proportion can be calculated using excel
write.csv(noc_occupation, "D://McGill/can_cache/noc_occupation.csv", row.names = FALSE)
write.csv(noc_occupation16, "D://McGill/can_cache/noc_occupation16.csv", row.names = FALSE)

#Set up table to create a grouped bar graph
noc_occupation_table <- noc_occupation |> 
  select(-total, -all) |> 
  pivot_longer(cols = -Type, names_to = "category", values_to = "count") |> 
  mutate(Type = factor(Type, levels = c("Total", "Men", "Women"))) |> 
  mutate(category = factor(category, levels = c("na", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")))

noc_occupation_table16 <- noc_occupation16 |> 
  select(-total, -all) |> 
  pivot_longer(cols = -Type, names_to = "category", values_to = "count") |> 
  mutate(Type = factor(Type, levels = c("Total", "Men", "Women"))) |> 
  mutate(category = factor(category, levels = c("na", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")))

#Creating the grouped bar graph
ggplot(noc_occupation_table, aes(x = category, y = count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_x_discrete(labels = c("Sans objet", "Membres des corps législatifs et\n cadres supérieurs/cadres supérieures",
                              "Affaires, finance et administration", "Sciences naturelles et appliquées\n et domaines apparentés",
                              "Secteur de la santé", "Enseignement, droit et services sociaux,\n communautaires et gouvernementaux",
                              "Arts, culture, sports et loisirs", "Vente et services",
                              "Métiers, transport, machinerie et\n domaines apparentés",
                              "Ressources naturelles, agriculture et\n production connexe",
                              "Fabrication et services d'utilité publique")) +
  labs(title = "Classification nationale des professions (CNP) 2021", x = "", y = "Personnes") +
  scale_fill_manual(values = c("Total" = "royalblue2", 
                               "Men" = "indianred", 
                               "Women" = "gold2"),
                    name = "Status",
                    labels = c("Total" = "Total", "Men" = "Hommes", "Women" = "Femmes")) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# North American Industry Classification System ---------------------------
#Vectors for 2021 NAICS categories
naics_total <- c("total" = "v_CA21_6600", "na" = "v_CA21_6603", "all" = "v_CA21_6606",
                 "11" = "v_CA21_6609", "21" = "v_CA21_6612", "22" = "v_CA21_6615",
                 "23" = "v_CA21_6618", "31-33" = "v_CA21_6621", "41" = "v_CA21_6624",
                 "44-45" = "v_CA21_6627", "48-49" = "v_CA21_6630", "51" = "v_CA21_6633",
                 "52" = "v_CA21_6636", "53" = "v_CA21_6639", "54" = "v_CA21_6642",
                 "55" = "v_CA21_6645", "56" = "v_CA21_6648", "61" = "v_CA21_6651",
                 "62" = "v_CA21_6654", "71" = "v_CA21_6657", "72" = "v_CA21_6660",
                 "81" = "v_CA21_6663", "91" = "v_CA21_6666")
naics_men <- c("total" = "v_CA21_6601", "na" = "v_CA21_6604", "all" = "v_CA21_6607",
                 "11" = "v_CA21_6610", "21" = "v_CA21_6613", "22" = "v_CA21_6616",
                 "23" = "v_CA21_6619", "31-33" = "v_CA21_6622", "41" = "v_CA21_6625",
                 "44-45" = "v_CA21_6628", "48-49" = "v_CA21_6631", "51" = "v_CA21_6634",
                 "52" = "v_CA21_6637", "53" = "v_CA21_6640", "54" = "v_CA21_6643",
                 "55" = "v_CA21_6646", "56" = "v_CA21_6649", "61" = "v_CA21_6652",
                 "62" = "v_CA21_6655", "71" = "v_CA21_6658", "72" = "v_CA21_6661",
                 "81" = "v_CA21_6664", "91" = "v_CA21_6667")
naics_women <- c("total" = "v_CA21_6602", "na" = "v_CA21_6605", "all" = "v_CA21_6608",
               "11" = "v_CA21_6611", "21" = "v_CA21_6614", "22" = "v_CA21_6617",
               "23" = "v_CA21_6620", "31-33" = "v_CA21_6623", "41" = "v_CA21_6626",
               "44-45" = "v_CA21_6629", "48-49" = "v_CA21_6632", "51" = "v_CA21_6635",
               "52" = "v_CA21_6638", "53" = "v_CA21_6641", "54" = "v_CA21_6644",
               "55" = "v_CA21_6647", "56" = "v_CA21_6650", "61" = "v_CA21_6653",
               "62" = "v_CA21_6656", "71" = "v_CA21_6659", "72" = "v_CA21_6662",
               "81" = "v_CA21_6665", "91" = "v_CA21_6668")

#Grabbing the names of the vectors above
naics_names <- c("total", "na", "all", "11", "21", "22", "23", "31-33", "41", "44-45",
                 "48-49", "51", "52", "53", "54", "55", "56", "61", "62", "71", "72",
                 "81", "91")

#Grabbing 2021 NAICS data for Laval
naics_lvl_total  <- get_census(dataset = "CA21", 
                             regions = list(CSD = 2465005), 
                             level = "CSD",
                             vectors = naics_total) |> 
  select(all_of(naics_names)) |> 
  mutate(Type = "Total") |> 
  select(Type, everything())

#Grabbing 2021 NAICS data for Laval men
naics_lvl_men  <- get_census(dataset = "CA21", 
                               regions = list(CSD = 2465005), 
                               level = "CSD",
                               vectors = naics_men) |> 
  select(all_of(naics_names)) |> 
  mutate(Type = "Men") |> 
  select(Type, everything())

#Grabbing 2021 NAICS data for Laval women
naics_lvl_women  <- get_census(dataset = "CA21", 
                             regions = list(CSD = 2465005), 
                             level = "CSD",
                             vectors = naics_women) |> 
  select(all_of(naics_names)) |> 
  mutate(Type = "Women") |> 
  select(Type, everything())

#Binding the NAICS tables together
naics_table <- bind_rows(naics_lvl_total, naics_lvl_men, naics_lvl_women)

#Set up table to create a grouped bar graph
naics_bar <- naics_table |> 
  select(-total, -all) |> 
  pivot_longer(cols = -Type, names_to = "category", values_to = "count") |> 
  mutate(Type = factor(Type, levels = c("Total", "Men", "Women"))) |> 
  mutate(category = factor(category, levels = c("na", "11", "21", "22", "23", "31-33", "41",
                                                "44-45", "48-49", "51", "52", "53", "54",
                                                "55", "56", "61", "62", "71", "72","81", "91")))

#Bar graph
ggplot(naics_bar, aes(x = category, y = count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "North American Industry Classification System 2021", x = "", y = "Count") +
  scale_fill_manual(values = c("Total" = "royalblue2", 
                               "Men" = "indianred", 
                               "Women" = "gold2"),
                    name = "Status",
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())

#Table for pie chart
naics_pie <- naics_table |> 
  select(-Type, -total, -all) |> 
  slice(c(-2, -3)) |> 
  pivot_longer(cols = everything(), names_to = "category", values_to = "count")

#Pie Chart
ggplot(naics_pie, aes(x = "", y = count, fill = category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "North American Industry Classification System 2021") +
  scale_fill_discrete(labels = c("na" = "Not Applicable")) +
  theme(
    legend.position = "right", legend.title = element_blank()
  )

# Commuting Destination ---------------------------------------------------
#Vectors for commuting destination
comdes_total <- c("total" = "v_CA21_7617", "same_csd" = "v_CA21_7620", "diff_csd_incd" = "v_CA21_7623",
                  "diff_csd" = "v_CA21_7626", "diff_prov" = "v_CA21_7629")
comdes_men <- c("total" = "v_CA21_7618", "same_csd" = "v_CA21_7621", "diff_csd_incd" = "v_CA21_7624",
                  "diff_csd" = "v_CA21_7627", "diff_prov" = "v_CA21_7630")
comdes_women <- c("total" = "v_CA21_7619", "same_csd" = "v_CA21_7622", "diff_csd_incd" = "v_CA21_7625",
                "diff_csd" = "v_CA21_7628", "diff_prov" = "v_CA21_7631")

#Names for the vectors above
comdes_names <- c("total", "same_csd", "diff_csd_incd", "diff_csd", "diff_prov")

#Commuting Destination for Laval
comdes_lvl_total  <- get_census(dataset = "CA21", 
                               regions = list(CSD = 2465005), 
                               level = "CSD",
                               vectors = comdes_total) |> 
  select(all_of(comdes_names)) |> 
  mutate(Type = "Total") |> 
  select(Type, everything())

#Commuting Destination for Laval men
comdes_lvl_men  <- get_census(dataset = "CA21", 
                                regions = list(CSD = 2465005), 
                                level = "CSD",
                                vectors = comdes_men) |> 
  select(all_of(comdes_names)) |> 
  mutate(Type = "Men") |> 
  select(Type, everything())

#Commuting Destination for Laval women
comdes_lvl_women  <- get_census(dataset = "CA21", 
                                regions = list(CSD = 2465005), 
                                level = "CSD",
                                vectors = comdes_women) |> 
  select(all_of(comdes_names)) |> 
  mutate(Type = "Women") |> 
  select(Type, everything())

#Bind tables together
comdes_table <- bind_rows(comdes_lvl_total, comdes_lvl_men, comdes_lvl_women)

#Prepare table for grouped bar graph
comdes_bar <- comdes_table |> 
  select(-total, -diff_csd_incd) |> 
  pivot_longer(cols = -Type, names_to = "destination", values_to = "count") |> 
  mutate(Type = factor(Type, levels = c("Total", "Men", "Women")),
         destination = factor(destination, levels = c("same_csd", "diff_csd", "diff_prov"))) |> 
  arrange(Type, destination)

#Grouped bar graph
ggplot(comdes_bar, aes(x = destination, y = count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "2021 Laval Commuting Destination", x = "", y = "Count") +
  scale_fill_manual(values = c("Total" = "royalblue2", 
                               "Men" = "indianred", 
                               "Women" = "gold2"),
                    name = "Status",
  ) +
  scale_x_discrete(labels = c(
    "same_csd" = "À Laval",
    "diff_csd" = "Hors de Laval mais au Québec",
    "diff_prov" = "Hors Québec"
  )) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())

# Commute Destination v2 -----------------------------------------------------
#Grabbing the vectors for the commuting destinations in 2016 and 2021
commute21v <- c("total" = "v_CA21_7617", "same_csd" = "v_CA21_7620",
               "diff_csd" = "v_CA21_7626", "diff_prov" = "v_CA21_7629")
commute16v <- c("total" = "v_CA16_5777", "same_csd" = "v_CA16_5780",
                "diff_csd" = "v_CA16_5786", "diff_prov" = "v_CA16_5789")

#Creating a function to grab data for commute destination and to calculate new vectors to be used
commute_grabber <- function(dyear, cvector, cyear){
  get_census(dataset = dyear, 
             regions = list(CSD = 2465005), 
             level = "CSD",
             vectors = cvector) #|>
    #mutate(Year = cyear, "Within Laval" = round(same_csd * 100 / total, 1),
           #"Outside Laval" = round((diff_csd + diff_prov) * 100 / total, 1)) |> 
    #select(Year, "Within Laval", "Outside Laval")
}

#Grabbing the data for years 2016 and 2021
commute21 <- commute_grabber("CA21", commute21v, "2021")
commute16 <- commute_grabber("CA16", commute16v, "2016")

#Prepping the data to create the graph
commute_dest <- bind_rows(commute21, commute16) |> 
  pivot_longer(cols = -Year, names_to = "destination", values_to = "percentage") |> 
  mutate(destination = factor(destination, levels = c("Within Laval",
                                                      "Outside Laval")))

#Creating the bar graph for commute destination
ggplot(commute_dest, aes(x = destination, y = percentage, fill = factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Destinations de déplacement à Laval en 2016 et 2021",
       x = "Destination de déplacement",
       y = "Proportion de navetteurs (%)",
       fill = "Year") +
  scale_x_discrete(labels = c(
    "Within Laval" = "À Laval",
    "Outside Laval" = "À l'extérieur de Laval")) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

# Place of Work -----------------------------------------------------------
#Grabbing the vectors for place of work in 2016 and 2011
place21v <- c("total" = "v_CA21_7602", "wfh" = "v_CA21_7605", "outside_ca" = "v_CA21_7608",
              "no_fix" = "v_CA21_7611", "usual" = "v_CA21_7614")
place16v <- c("total" = "v_CA16_5762", "wfh" = "v_CA16_5765", "outside_ca" = "v_CA16_5768",
              "no_fix" = "v_CA16_5771", "usual" = "v_CA16_5774")
place11v <- c("total" = "v_CA11N_2176", "wfh" = "v_CA11N_2179", "outside_ca" = "v_CA11N_2182",
              "no_fix" = "v_CA11N_2185", "usual" = "v_CA11N_2188")
place06v <- c("total" = "v_CA06_1076", "wfh" = "v_CA06_1081", "outside_ca" = "v_CA06_1082",
              "no_fix" = "v_CA06_1083", "usual" = "v_CA06_1077")
place01v <- c("total" = "v_CA06_1076", "wfh" = "v_CA06_1081", "outside_ca" = "v_CA06_1082",
              "no_fix" = "v_CA06_1083", "usual" = "v_CA06_1077")

#Function to grab the data from the census and to calculate new vectors to be used
place_grabber <- function(dyear, cvector, cyear){
  get_census(dataset = dyear, 
             regions = list(CSD = 2465005), 
             level = "CSD",
             vectors = cvector) |>
    mutate(Year = cyear, "Work from Home" = round(wfh * 100 / total, 1),
           "Outside Canada" = round(outside_ca * 100 / total, 1),
           "No Fixed Address" = round(no_fix * 100 / total, 1),
           "Usual Place of Work" = round(usual * 100 / total, 1)) |> 
    select(Year, "Usual Place of Work", "Work from Home",
           "No Fixed Address", "Outside Canada")
}

#grabbing the census data
place21 <- place_grabber("CA21", place21v, "2021")
place16 <- place_grabber("CA16", place16v, "2016")
place11 <- place_grabber("CA11", place11v, "2011")
place06 <- place_grabber("CA06", place11v, "2006")
place01 <- get_census(dataset = "CA01", 
                      regions = list(CSD = 2465005), 
                      level = "CSD",
                      vectors = c("total" = "v_CA01_1236", "wfh_m" = "v_CA01_1242",
                                  "wfh_f" = "v_CA01_1250", "outside_ca_m" = "v_CA01_1243",
                                  "outside_ca_f" = "v_CA01_1251", "no_fix_m" = "v_CA01_1244",
                                  "no_fix_f" = "v_CA01_1252", "usual_m" = "v_CA01_1238",
                                  "usual_f" = "v_CA01_1246")) |> 
  mutate(Year = "2001", "Usual Place of Work" = (usual_m + usual_f) * 100 / total,
         "Work from Home" = (wfh_m + wfh_f) * 100 / total,
         "No Fixed Address" = (no_fix_m + no_fix_f) * 100 / total,
         "Outside Canada" = (outside_ca_m + outside_ca_f) * 100 / total) |> 
  select(Year, "Usual Place of Work", "Work from Home", "No Fixed Address", "Outside Canada")

#Prepping the data to be made into a graph
place <- bind_rows(place21, place16, place11, place06, place01) |> 
  pivot_longer(cols = -Year, names_to = "destination", values_to = "percentage") |> 
  mutate(destination = factor(destination, levels = c("Usual Place of Work", "Work from Home",
                                                      "No Fixed Address", "Outside Canada")))

#Creating the bar graph for place of work
ggplot(place, aes(x = destination, y = percentage, fill = factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Catégorie du lieu de travail de Laval 2016 et 2021",
       x = "Catégorie du lieu de travail",
       y = "Proportion de résidents employés (%)",
       fill = "Year") +
  scale_x_discrete(labels = c(
    "Usual Place of Work" = "Lieu habituel de travail",
    "Work from Home" = "À domicile",
    "No Fixed Address" = "Sans adresse de travail fixe",
    "Outside Canada" = "À l'extérieur du Canada")) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

#Line graph for place of work
ggplot(place, aes(x = Year, y = percentage, color = destination, group = destination)) +
  geom_line(linewidth = 1.5) +
  labs(title = "Catégorie de lieu de travail à Laval de 2001 à 2021", x = "Année",
       y = "Proportion de résidents employés (%)") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal",
        legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

# Activity Situation Historical -------------------------------------------
#Grabbing vectors for activity situation
act21v <- c("total" = "v_CA21_6492", "in_lf" = "v_CA21_6495", "not_lf" = "v_CA21_6504")
act16v <- c("total" = "v_CA16_5597", "in_lf" = "v_CA16_5600", "not_lf" = "v_CA16_5609")
act11v <- c("total" = "v_CA11N_1987", "in_lf" = "v_CA11N_1990", "not_lf" = "v_CA11N_1999")
act06v <- c("total" = "v_CA06_575", "in_lf" = "v_CA06_576", "not_lf" = "v_CA06_579")
act01v <- c("total" = "v_CA01_735", "in_lf" = "v_CA01_736", "not_lf" = "v_CA01_739")

#Function to grab activity situation data from census
activity_grabber <- function(dyear, cvector, cyear){
  get_census(dataset = dyear, 
             regions = list(CSD = 2465005), 
             level = "CSD",
             vectors = cvector) |> 
    mutate(Year = cyear)
}

#Grabbing the data for each census year
act21 <- activity_grabber("CA21", act21v, "2021")
act16 <- activity_grabber("CA16", act16v, "2016")
act11 <- activity_grabber("CA11", act11v, "2011")
act06 <- activity_grabber("CA06", act06v, "2006")
act01 <- activity_grabber("CA01", act01v, "2001")

#Preparing the raw number data for the line graph
act_raw <- bind_rows(act21, act16, act11, act06, act01) |> 
  select(Year, total, in_lf, not_lf) |> 
  rename("Total" = total, "In Labour Force" = in_lf, "Not in Labour Force" = not_lf) |> 
  pivot_longer(cols = -Year, values_to = "count", names_to = "type") |> 
  mutate(type = factor(type, levels = c("Total", "In Labour Force",
                                        "Not in Labour Force")))

#Preparing the proportionate data for the line graph
act_prop <- bind_rows(act21, act16, act11, act06, act01) |> 
  mutate(lf_prop = round(in_lf * 100 / total, 1),
         nlf_prop = round(not_lf * 100 / total, 1)) |> 
  select(Year, lf_prop, nlf_prop) |> 
  pivot_longer(cols = -Year, values_to = "percentage", names_to = "type")

#Line graph for raw numbers
ggplot(act_raw, aes(x = as.factor(Year), y = count, color = type, group = type)) +
  geom_line(size = 1.5) +
  labs(title = "Situation d'activité de Laval 2001-2021", x = "Année",
       y = "Personnes", color = "Geography") +
  scale_color_manual(labels = c("Total" = "Total", "In Labour Force" = "Population Active",
                               "Not in Labour Force" = "Population Inactive"),
                     values = c("Total" = "indianred2",
                       "In Labour Force" = "royalblue3",
                       "Not in Labour Force" = "darkgreen"
                     )) +
  scale_y_continuous(labels = label_number(big.mark = ".")) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal",
        legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

#Line graph for proportion
ggplot(act_prop, aes(x = as.factor(Year), y = percentage, color = type, group = type)) +
  geom_line(size = 1.5) +
  labs(title = "Activity Situation* in Laval 2001-2021", x = "Year",
       y = "Count (Persons)", color = "Geography") +
  scale_y_continuous(labels = comma_format()) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal",
        legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

# Work During Reference Year v2 -------------------------------------------
#Pulling vectors for work during reference year
wref21v <- c("total" = "v_CA21_6516", "no_work" = "v_CA21_6519",
                   "full" = "v_CA21_6525", "part" = "v_CA21_6528")
wref16v <- c("total" = "v_CA16_5621", "no_work" = "v_CA16_5624",
             "full" = "v_CA16_5630", "part" = "v_CA16_5633")

#Function to pull data from the census
wref_grabber <- function(dyear, cvector, cyear){
  get_census(dataset = dyear, 
             regions = list(CSD = 2465005), 
             level = "CSD",
             vectors = cvector) |> 
    mutate(Year = cyear)
}

#Grabbing the data and then binding them together for easier comparison
wref21 <- wref_grabber("CA21", wref21v, "2021")
wref16 <- wref_grabber("CA16", wref16v, "2016")
wref <- bind_rows(wref21, wref16)

# social assistance rate --------------------------------------------------
#Pulling a modified version of assistance_sociale_QC_et_Laval.xlsx for social assistance
#in Quebec and modifying it to be easier to work with
social_assist_qc <- read_excel("D://Mcgill/can_cache/assistance_sociale_QC_et_Laval.xlsx", sheet = 1) |> 
  rename(Year = 1) |> 
  select(Year, Change, `Social Rate`) |> 
  mutate(across(c(Change, `Social Rate`), ~ . * 100),
         Geography = "Quebec")

#Pulling a modified version of assistance_sociale_QC_et_Laval.xlsx for social assistance
#in Laval and modifying it to be easier to work with
social_assist_lvl <- read_excel("D://Mcgill/can_cache/assistance_sociale_QC_et_Laval.xlsx", sheet = 2) |> 
  rename(Year = 1) |> 
  select(Year, Change, `Social Rate`) |> 
  filter(!is.na(Year)) |> 
  mutate(
    Change = as.numeric(Change),
    `Social Rate` = as.numeric(`Social Rate`)
  ) |> 
  mutate(across(c(Change, `Social Rate`), ~ . * 100),
         Geography = "Laval")

#Binding the data together
social_assist_rate <- bind_rows(social_assist_qc, social_assist_lvl) |> 
  select(-Change)

#Graphing the social assistance rate
ggplot(social_assist_rate, aes(x = Year, y = `Social Rate`, color = Geography, group = Geography)) +
  geom_line(linewidth = 1.5) +
  labs(title = "Taux d'assistance sociale de 1993 à 2023", x = "Année",
       y = "Taux annuel moyen d’aide sociale (%)", color = "Geography") +
  scale_y_continuous(labels = label_number(decimal.mark = ",")) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal",
        legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
