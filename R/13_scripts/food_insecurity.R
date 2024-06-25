#Loading libraries
source("R/01_startup.R")
library(sf)
library(readxl)
library(cmhc)
library(scales)

#Setting CensusMapper API Key because it won't save
set_cancensus_api_key("CensusMapper_4308d496f011429cf814385050f083dc")

#Grabbing all cancensus vector
can21 <- list_census_vectors(dataset = "CA21")

#Caching census and CMHC data to reduce amount of calls and speed up process.
#Personal use only, change the folder to your own folder if you want to use it
set_cancensus_cache_path("D:/McGill/can_cache", install = TRUE, overwrite = TRUE)
set_cmhc_cache_path("D:/McGill/can_cache", install = TRUE, overwrite = TRUE)

#Grabbing Laval's shapefile by census tract
laval_ct <- cancensus::get_census(dataset = "CA21", 
                                  regions = list(CSD = 2465005), 
                                  level = "CT", 
                                  geo_format = "sf")

#Grabbing Laval shapefile by dissemination area
laval_da <- cancensus::get_census(dataset = "CA21", 
                                  regions = list(CSD = 2465005), 
                                  level = "DA", 
                                  geo_format = "sf")

#Loading the colors for the curbcut scale
curbcut_scale <- c("#C4CDE1", "#98A8CB", "#6C83B5", "#4C5C7F", "#2B3448")
curbcut_na <- "#B3B3BB"

# Risk Factors ------------------------------------------------------------
#Grabbing shapefile data for immigration
imm_lvl <- get_census(dataset = "CA21", 
                           regions = list(CSD = 2465005), 
                           vectors = c("total" = "v_CA21_4404", "imm" = "v_CA21_4410"),
                           level = "CT",
                           geo_format = "sf") |> 
  mutate(imm_prop = imm * 100 / total)

ggplot(data = imm_lvl) +
  geom_sf(aes(fill = imm_prop), color = NA) +
  labs(title = "Population immigrante à Laval 2021", fill = "Proportion d'immigrants (%)") +
  scale_fill_gradientn(colors = curbcut_scale, na.value = curbcut_na) +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.justification = "center") +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
                               barwidth = 15, barheight = 1))

#Grabbing shapefile data for one parent families
onep_lvl <- get_census(dataset = "CA21", 
                       regions = list(CSD = 2465005), 
                       vectors = c("total" = "v_CA21_499", "onep" = "v_CA21_507"),
                       level = "CT",
                       geo_format = "sf") |> 
  mutate(onep_prop = onep * 100 / total)

ggplot(data = onep_lvl) +
  geom_sf(aes(fill = onep_prop), color = NA) +
  labs(title = "Familles monoparentales à Laval 2021", fill = "Proportion de familles monoparentales (%)") +
  scale_fill_gradientn(colors = curbcut_scale, na.value = curbcut_na) +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.justification = "center") +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
                               barwidth = 15, barheight = 1))

alone_lvl <- get_census(dataset = "CA21", 
                      regions = list(CSD = 2465005), 
                      vectors = c("total" = "v_CA21_510", "alone" = "v_CA21_534"),
                      level = "CT",
                      geo_format = "sf") |> 
  mutate(alone_prop = alone * 100 / total)

ggplot(data = alone_lvl) +
  geom_sf(aes(fill = alone_prop), color = NA) +
  labs(title = "Population vivant seule à Laval 2021",
       fill = "Proportion de ménages composés d'une seule personne (%)") +
  scale_fill_gradientn(colors = curbcut_scale, na.value = curbcut_na) +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.justification = "center") +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
                               barwidth = 15, barheight = 1))

older75_lvl <- get_census(dataset = "CA21", 
                          regions = list(CSD = 2465005), 
                          vectors = c("total" = "v_CA21_8", "75_79" = "v_CA21_290",
                                      "80_84" = "v_CA21_308", "85_up" = "v_CA21_326"),
                          level = "CT",
                          geo_format = "sf") |> 
  mutate(older75_prop = (`75_79` + `80_84` + `85_up`) * 100 / total)

ggplot(data = older75_lvl) +
  geom_sf(aes(fill = older75_prop), color = NA) +
  labs(title = "Population de 75 ans et plus à Laval 2021",
       fill = "Proportion de la population de 75 ans et plus (%)") +
  scale_fill_gradientn(colors = curbcut_scale, na.value = curbcut_na) +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.justification = "center") +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
                               barwidth = 15, barheight = 1))

une_lvl <- get_census(dataset = "CA21", 
                        regions = list(CSD = 2465005), 
                        vectors = c("total" = "v_CA21_5817", "une" = "v_CA21_5820"),
                        level = "CT",
                        geo_format = "sf") |> 
  mutate(une_prop = une * 100 / total)

ggplot(data = une_lvl) +
  geom_sf(aes(fill = une_prop), color = NA) +
  labs(title = "Population sous-éduqué à Laval 2021",
       fill = "Proportion de la population sous-éduqué (%)") +
  scale_fill_gradientn(colors = curbcut_scale, na.value = curbcut_na) +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.justification = "center") +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
                               barwidth = 15, barheight = 1))

lim_lvl <- get_census(dataset = "CA21", 
                      regions = list(CSD = 2465005), 
                      vectors = c("lico" = "v_CA21_1040"),
                      level = "CT",
                      geo_format = "sf")

ggplot(data = lim_lvl) +
  geom_sf(aes(fill = lico), color = NA) +
  labs(title = "Population à faible revenu à Laval 2021",
       fill = "Prévalence du faible revenu (MFR-ApI)(%)") +
  scale_fill_gradientn(colors = curbcut_scale, na.value = curbcut_na) +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.justification = "center") +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
                               barwidth = 15, barheight = 1))

#Grabbing vectors for evolution chart
rf21v <- c("imm_total" = "v_CA21_4404", "imm" = "v_CA21_4410",
           "onep_total" = "v_CA21_499", "onep" = "v_CA21_507",
           "alone_total" = "v_CA21_510", "alone" = "v_CA21_534",
           "older_total" = "v_CA21_8", "75_79" = "v_CA21_290",
           "80_84" = "v_CA21_308", "85_up" = "v_CA21_326",
           "une_total" = "v_CA21_5817", "une" = "v_CA21_5820",
           "Low Income*" = "v_CA21_1040")
rf16v <- c("imm_total" = "v_CA16_3405", "imm" = "v_CA16_3411",
            "onep_total" = "v_CA16_484", "onep" = "v_CA16_488",
            "alone_total" = "v_CA16_424", "alone" = "v_CA16_510",
            "older_total" = "v_CA16_1", "75_79" = "v_CA16_283",
            "80_84" = "v_CA16_301", "85_up" = "v_CA16_319",
            "une_total" = "v_CA16_5051", "une" = "v_CA16_5054",
            "Low Income*" = "v_CA16_2540")
rf11v <- c("imm_total" = "v_CA11N_16", "imm" = "v_CA11N_22",
           "onep_total" = "v_CA11F_115", "onep" = "v_CA11F_129",
           "alone_total" = "v_CA11F_145", "alone" = "v_CA11F_157",
           "older_total" = "v_CA11F_5", "75_79" = "v_CA11F_68",
           "80_84" = "v_CA11F_71", "85_up" = "v_CA11F_74",
           "une_total" = "v_CA11N_1771", "une" = "v_CA11N_1774",
           "Low Income*" = "v_CA11N_2606")

#Census grabbing function for immigration
rf_grabber <- function(dyear, vector, year){
  get_census(dataset = dyear, 
             regions = list(CSD = 2465005), 
             level = "CSD",
             vectors = vector) |> 
    mutate(Year = year, Immigration = imm * 100 / imm_total,
           `One Parent Families` = onep * 100 / onep_total,
           `Living Alone` = alone * 100 / alone_total,
           `Older than 75` = (`75_79` + `80_84` + `85_up`) * 100 / older_total,
           `Under-Educated` = une * 100 / une_total) |> 
    select(Year, Immigration, `One Parent Families`, `Living Alone`, `Older than 75`,
           `Under-Educated`, `Low Income*`)
}

#Grabbing data with the function and vectors above
rf21 <- rf_grabber("CA21", rf21v, "2021")
rf16 <- rf_grabber("CA16", rf16v, "2016")
rf11 <- rf_grabber("CA11", rf11v, "2011")
#Grabbing data for 2001 and2006 with it's own function as age and education
#aren't together but are divided by age
rf06 <- get_census(dataset = "CA06", regions = list(CSD = 2465005), level = "CSD",
                   vectors = c("imm_total" = "v_CA06_474", "imm" = "v_CA06_478",
                   "onep_total" = "v_CA06_55", "onep" = "v_CA06_69",
                   "alone_total" = "v_CA06_85", "alone" = "v_CA06_89",
                   "older_total" = "v_CA06_2", "75_79m" = "v_CA06_19",
                   "80_84m" = "v_CA06_20", "85_upm" = "v_CA06_21",
                   "75_79w" = "v_CA06_38", "80_84w" = "v_CA06_39",
                   "85_upw" = "v_CA06_40",
                   "une_total15" = "v_CA06_1234", "une15" = "v_CA06_1235",
                   "une_total24" = "v_CA06_1248", "une24" = "v_CA06_1249",
                   "une_total65" = "v_CA06_1262", "une65" = "v_CA06_1263",
                   "Low Income*" = "v_CA06_1981")) |> 
  mutate(Year = "2006", Immigration = imm * 100 / imm_total,
         `One Parent Families` = onep * 100 / onep_total,
         `Living Alone` = alone * 100 / alone_total,
         `Older than 75` = (`75_79m` + `80_84m` + `85_upm` + `75_79w` + `80_84w` + `85_upw`) * 100 / older_total,
         `Under-Educated` = (`une15` + `une24` + `une65`) * 100 /(`une_total15` + `une_total24` + `une_total65`)) |> 
  select(Year, Immigration, `One Parent Families`, `Living Alone`, `Older than 75`,
         `Under-Educated`, `Low Income*`)

rf01 <- get_census(dataset = "CA01", regions = list(CSD = 2465005), level = "CSD",
                   vectors = c("imm_total" = "v_CA01_402", "imm" = "v_CA01_406",
                               "onep_total" = "v_CA01_53", "onep" = "v_CA01_67",
                               "alone_total" = "v_CA01_83", "alone" = "v_CA01_87",
                               "older_total" = "v_CA01_5", "75_79m" = "v_CA01_22",
                               "80_84m" = "v_CA01_23", "85_upm" = "v_CA01_24",
                               "75_79w" = "v_CA01_41", "80_84w" = "v_CA01_42",
                               "85_upw" = "v_CA01_43",
                               "Low Income*" = "v_CA01_1620")) |> 
  mutate(Year = "2001", Immigration = imm * 100 / imm_total,
         `One Parent Families` = onep * 100 / onep_total,
         `Living Alone` = alone * 100 / alone_total,
         `Older than 75` = (`75_79m` + `80_84m` + `85_upm` + `75_79w` + `80_84w` + `85_upw`) * 100 / older_total,
         `Under-Educated` = NA) |> 
  select(Year, Immigration, `One Parent Families`, `Living Alone`, `Older than 75`,
         `Under-Educated`, `Low Income*`)

#Binding the risk factor data together
rf <- bind_rows(rf21, rf16, rf11, rf06, rf01) |> 
  pivot_longer(cols = -Year, names_to = "Risk Factor", values_to = "Proportion")

#Plotting the line graph
ggplot(rf, aes(x = Year, y = Proportion, color = `Risk Factor`, group = `Risk Factor`)) +
  geom_line(linewidth = 1.25) +
  scale_color_manual(values = c("Immigration" = "#66c2a5", "One Parent Families" = "#fc8d62",
                                "Living Alone" = "#8da0cb", "Older than 75" = "#e78ac3",
                                "Under-Educated" = "#a6d854", "Low Income*" = "#ffd92f"),
                     labels = c("Immigrant", "Familles monoparentales", "Vivre seul",
                                "Plus de 75 ans", "Sous-éduqué", "Faible revenu")) +
  labs(title = "Proportion de la population présentant des facteurs de risque\n
                pour Insécurité alimentaire à Laval 2001-2021",
       x = "Année",
       y = "Proportion de la population (%)",
       caption = "*Les années 2011 à 2021 utilisent la définition MFR-ApI du faible revenu") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))
