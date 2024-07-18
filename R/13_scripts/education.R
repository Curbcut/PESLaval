source("R/01_startup.R")
library(scales)
library(readxl)

#Setting CensusMapper API Key because it won't save
set_cancensus_api_key("CensusMapper_4308d496f011429cf814385050f083dc")

#Caching census data to reduce amount of calls and speed up process.
#Personal use only, change the folder to your own folder if you want to use it
set_cancensus_cache_path("D:/McGill/can_cache", install = TRUE, overwrite = TRUE)

# High school diploma -----------------------------------------------------
#Grabbing necessary vectors for 2006-2021
hs_21v <- c("total" = "v_CA21_5817", "total_m" = "v_CA21_5818", "total_f" = "v_CA21_5819",
            "hs_total" = "v_CA21_5805", "hs_m" = "v_CA21_5806", "hs_f" = "v_CA21_5807")
hs_16v <- c("total" = "v_CA16_5051", "nohs_total" = "v_CA16_5054")
hs_11v <- c("total" = "v_CA11N_1771", "nohs_total" = "v_CA11N_1774")
hs_06v <- c("total15" = "v_CA06_1234", "nohs15_total" = "v_CA06_1235",
            "total25" = "v_CA06_1248", "nohs25_total" = "v_CA06_1249",
            "total65" = "v_CA06_1262", "nohs65_total" = "v_CA06_1263")

#Creating the functions to grab the actual data
csd_grabber_21 <- function(dyear, region, vector_list, region_name){
  get_census(dataset = dyear,
             regions = list(CSD = c(region)),
             level = "CSD",
             vectors = vector_list) |> 
    mutate(Region = region_name) |> 
    select(Region, total, total_m, total_f, hs_total, hs_m, hs_f)
}

pr_grabber_21 <- function(dyear, vector_list, region_name){
  get_census(dataset = dyear,
             regions = list(PR = c(24)),
             level = "PR",
             vectors = vector_list) |> 
    mutate(Region = region_name) |> 
    select(Region, total, total_m, total_f, hs_total, hs_m, hs_f)
}

csd_grabber_16_11 <- function(dyear, region, vector_list, region_name){
  get_census(dataset = dyear,
             regions = list(CSD = c(region)),
             level = "CSD",
             vectors = vector_list) |> 
    mutate(Region = region_name) |> 
    mutate(`hs_total` = `total` - `nohs_total`) |> 
    select(Region, total, hs_total)
}

pr_grabber_16_11 <- function(dyear, vector_list, region_name){
  get_census(dataset = dyear,
             regions = list(PR = c(24)),
             level = "PR",
             vectors = vector_list) |> 
    mutate(Region = region_name) |> 
    mutate(`hs_total` = `total` - `nohs_total`) |> 
    select(Region, total, hs_total)
}

csd_grabber_06 <- function(dyear, region, vector_list, region_name){
  get_census(dataset = dyear,
             regions = list(CSD = c(region)),
             level = "CSD",
             vectors = vector_list) |> 
    mutate(Region = region_name,
           total = `total15` + `total25` + `total65`) |> 
    mutate(`hs_total` = `total` - `nohs15_total` - nohs25_total - nohs65_total) |> 
    select(Region, total, hs_total)
}

pr_grabber_06 <- function(dyear, vector_list, region_name){
  get_census(dataset = dyear,
             regions = list(PR = c(24)),
             level = "PR",
             vectors = vector_list) |> 
    mutate(Region = region_name,
           total = `total15` + `total25` + `total65`) |> 
    mutate(`hs_total` = `total` - `nohs15_total` - nohs25_total - nohs65_total) |> 
    select(Region, total, hs_total)
}

#Grabbing the data using the functions
hs_21_lvl <- csd_grabber_21("CA21", 2465005, hs_21v, "Laval") |> mutate(Year = "2021")
hs_21_mtl <- csd_grabber_21("CA21", 2466023, hs_21v, "Montreal") |> mutate(Year = "2021")
hs_21_qc <- pr_grabber_21("CA21", hs_21v, "Quebec") |> mutate(Year = "2021")

hs_16_lvl <- csd_grabber_16_11("CA16", 2465005, hs_16v, "Laval") |> mutate(Year = "2016")
hs_16_mtl <- csd_grabber_16_11("CA16", 2466023, hs_16v, "Montreal") |> mutate(Year = "2016")
hs_16_qc <- pr_grabber_16_11("CA16", hs_16v, "Quebec") |> mutate(Year = "2016")

hs_11_lvl <- csd_grabber_16_11("CA11", 2465005, hs_11v, "Laval") |> mutate(Year = "2011")
hs_11_mtl <- csd_grabber_16_11("CA11", 2466023, hs_11v, "Montreal") |> mutate(Year = "2011")
hs_11_qc <- pr_grabber_16_11("CA11", hs_11v, "Quebec") |> mutate(Year = "2011")

hs_06_lvl <- csd_grabber_06("CA06", 2465005, hs_06v, "Laval") |> mutate(Year = "2006")
hs_06_mtl <- csd_grabber_06("CA06", 2466023, hs_06v, "Montreal") |> mutate(Year = "2006")
hs_06_qc <- pr_grabber_06("CA06", hs_06v, "Quebec") |> mutate(Year = "2006")

# High School Diploma Graphs ----------------------------------------------
#Binding and cleaning up the data to be used to make a graph
hs <- bind_rows(hs_21_lvl, hs_21_mtl, hs_21_qc,
                hs_16_lvl, hs_16_mtl, hs_16_qc,
                hs_11_lvl, hs_11_mtl, hs_11_qc,
                hs_06_lvl, hs_06_mtl, hs_06_qc) |> 
  mutate(hs_prop = hs_total/total * 100) |> 
  select(Region, Year, hs_prop)

#Creating the line graph
ggplot(hs, aes(x = Year, y = hs_prop, color = Region, group = Region)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = c("Laval" = "#A3B0D1",
                                "Montreal" = "#E08565",
                                "Quebec" = "#73AD80")) +
  labs(x = "Année", y = "Population titulaire d'un diplôme d'études\nsecondaires ou équivalent (%)") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())

# Highest Certificate 15+ -------------------------------------------------
#Grabbing desired vectors for highest certificate for 15+
hc15_21v <- c("total" = "v_CA21_5817", "total_m" = "v_CA21_5818", "total_f" = "v_CA21_5819",
              "none" = "v_CA21_5820", "none_m" = "v_CA21_5821", "none_f" = "v_CA21_5822",
              "sec" = "v_CA21_5823", "sec_m" = "v_CA21_5824", "sec_f" = "v_CA21_5825",
              "app" = "v_CA21_5832", "app_m" = "v_CA21_5833", "app_f" = "v_CA21_5834",
              "col" = "v_CA21_5841", "col_m" = "v_CA21_5842", "col_f" = "v_CA21_5843",
              "uni" = "v_CA21_5844", "uni_m" = "v_CA21_5845", "uni_f" = "v_CA21_5846",
              "bac" = "v_CA21_5847", "bac_m" = "v_CA21_5848", "bac_f" = "v_CA21_5849")

#Grabbing actual data, and cleaning it up for use
hc15 <- get_census(
  dataset = "CA21",
  regions = list(CSD = (2465005)),
  level = "CSD",
  vectors = hc15_21v) |> 
  select(none, none_m, none_f, sec, sec_m, sec_f, app,
         app_m, app_f, col, col_m, col_f, uni, uni_m, uni_f, bac, bac_m, bac_f) |> 
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "count") |> 
  mutate(education = case_when(str_starts(variable, "none") ~ "Aucun certificat, diplôme ou grade",
                               str_starts(variable, "sec") ~ "Diplôme d'études secondaires",
                               str_starts(variable, "app") ~ "Certificat ou diplôme d'apprenti",
                               str_starts(variable, "col") ~ "Diplôme d'un collège",
                               str_starts(variable, "uni") ~ "Diplôme universitaire inférieur au baccalauréat",
                               str_starts(variable, "bac") ~ "Diplôme universitaire",
                               TRUE ~ "Other"),
         Type = case_when(str_ends(variable, "_m") ~ "Male",
                          str_ends(variable, "_f") ~ "Female",
                          TRUE ~ "Total")) |> 
  mutate(Type = factor(Type, levels = c("Total", "Female", "Male")),
         education = factor(education,
                            levels = c("Aucun certificat, diplôme ou grade", "Diplôme d'études secondaires",
                                       "Certificat ou diplôme d'apprenti", "Diplôme d'un collège",
                                       "Diplôme universitaire inférieur au baccalauréat",
                                       "Diplôme universitaire")),
         prop = paste0(round(count / sum(count) * 100, 1), "%"))
#Autowrapping the text so it's readable
hc15$education <- str_wrap(hc15$education, width = 25)


#Creating to bar graph
ggplot(hc15, aes(x = education, y = count, fill = factor(Type, levels = c("Total", "Male", "Female")))) +
  geom_bar(stat = "identity", position = "dodge", aes(group = variable)) +
  labs(x = "", y = "Personnes") +
  scale_fill_manual(values = c("Total" = "#73AD80",
                               "Male" = "#A3B0D1",
                               "Female" = "#CD718C")) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Highest Certificate 25-64 -----------------------------------------------
hc25v <- c("total" = "v_CA21_5865", "total_m" = "v_CA21_5866", "total_f" = "v_CA21_5867",
              "none" = "v_CA21_5868", "none_m" = "v_CA21_5869", "none_f" = "v_CA21_5870",
              "sec" = "v_CA21_5871", "sec_m" = "v_CA21_5872", "sec_f" = "v_CA21_5873",
              "app" = "v_CA21_5880", "app_m" = "v_CA21_5881", "app_f" = "v_CA21_5882",
              "col" = "v_CA21_5889", "col_m" = "v_CA21_5890", "col_f" = "v_CA21_5891",
              "uni" = "v_CA21_5892", "uni_m" = "v_CA21_5893", "uni_f" = "v_CA21_5894",
              "bac" = "v_CA21_5895", "bac_m" = "v_CA21_5896", "bac_f" = "v_CA21_5897")

#Grabbing actual data, and cleaning it up for use
hc25 <- get_census(
  dataset = "CA21",
  regions = list(CSD = (2465005)),
  level = "CSD",
  vectors = hc25v) |> 
  select(none, none_m, none_f, sec, sec_m, sec_f, app,
         app_m, app_f, col, col_m, col_f, uni, uni_m, uni_f, bac, bac_m, bac_f) |> 
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "count") |> 
  mutate(education = case_when(str_starts(variable, "none") ~ "Aucun certificat, diplôme ou grade",
                               str_starts(variable, "sec") ~ "Diplôme d'études secondaires",
                               str_starts(variable, "app") ~ "Certificat ou diplôme d'apprenti",
                               str_starts(variable, "col") ~ "Diplôme d'un collège",
                               str_starts(variable, "uni") ~ "Diplôme universitaire inférieur au baccalauréat",
                               str_starts(variable, "bac") ~ "Diplôme universitaire",
                               TRUE ~ "Other"),
         Type = case_when(str_ends(variable, "_m") ~ "Male",
                          str_ends(variable, "_f") ~ "Female",
                          TRUE ~ "Total")) |> 
  mutate(Type = factor(Type, levels = c("Total", "Female", "Male")),
         education = factor(education,
                            levels = c("Aucun certificat, diplôme ou grade", "Diplôme d'études secondaires",
                                       "Certificat ou diplôme d'apprenti", "Diplôme d'un collège",
                                       "Diplôme universitaire inférieur au baccalauréat",
                                       "Diplôme universitaire")),
         prop = paste0(round(count / sum(count) * 100, 1), "%"))
#Autowrapping the text so it's readable
hc25$education <- str_wrap(hc25$education, width = 25)


#Creating to bar graph
ggplot(hc25, aes(x = education, y = count, fill = factor(Type, levels = c("Total", "Male", "Female")))) +
  geom_bar(stat = "identity", position = "dodge", aes(group = variable)) +
  labs(x = "", y = "Personnes") +
  scale_fill_manual(values = c("Total" = "#73AD80",
                               "Male" = "#A3B0D1",
                               "Female" = "#CD718C")) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
