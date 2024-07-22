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
              "psec" = "v_CA21_5877", "psec_m" = "v_CA21_5878", "psec_f" = "v_CA21_5879",
              "uni" = "v_CA21_5895", "uni_m" = "v_CA21_5896", "uni_f" = "v_CA21_5897")

#Grabbing actual data, and cleaning it up for use
hc25 <- get_census(
  dataset = "CA21",
  regions = list(CSD = (2465005)),
  level = "CSD",
  vectors = hc25v) |> 
  select(none, none_m, none_f, sec, sec_m, sec_f, psec, psec_m, psec_f, uni, uni_m, uni_f) |> 
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "count") |> 
  mutate(education = case_when(str_starts(variable, "none") ~ "Aucun certificat, diplôme ou grade",
                               str_starts(variable, "sec") ~ "Diplôme d'études secondaires",
                               str_starts(variable, "psec") ~ "Certificat ou diplôme postsecondaire inférieur au baccalauréat",
                               str_starts(variable, "uni") ~ "Diplôme universitaire",
                               TRUE ~ "Other"),
         Type = case_when(str_ends(variable, "_m") ~ "Hommes",
                          str_ends(variable, "_f") ~ "Femmes",
                          TRUE ~ "Total")) |>
  mutate(Type = factor(Type, levels = c("Total", "Femmes", "Hommes")),
         education = factor(education,
                            levels = c("Aucun certificat, diplôme ou grade", "Diplôme d'études secondaires",
                                       "Certificat ou diplôme postsecondaire inférieur au baccalauréat",
                                       "Diplôme universitaire")),
         prop = paste0(round(count / 228815 * 100, 1), "%"))


#Creating to bar graph
ggplot(hc25, aes(x = education, y = count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", aes(group = variable)) +
  geom_text(aes(label = prop, y = count), position = position_dodge(width = 0.9), vjust = 1.5, size = 3.5, color = "white") +
  labs(x = "", y = "Personnes") +
  scale_fill_manual(values = c("Total" = "#73AD80",
                               "Hommes" = "#A3B0D1",
                               "Femmes" = "#CD718C")) +
  scale_x_discrete(labels = c("Aucun certificat, diplôme ou\ngrade",
                              "Diplôme d'études secondaires",
                              "Certificat ou diplôme\npostsecondaire inférieur\nau baccalauréat",
                              "Diplôme universitaire")) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank(),
        axis.text.x = element_text(angle = 40, hjust = 1))

# Composition 2006-2021 ---------------------------------------------------
comp21v <- c("total" = "v_CA21_5865", "none" = "v_CA21_5868", "sec" = "v_CA21_5871",
             "psec" = "v_CA21_5877", "uni" = "v_CA21_5895")
comp16v <- c("total" = "v_CA16_5096", "none" = "v_CA16_5099", "sec" = "v_CA16_5102",
             "psec" = "v_CA16_5105", "uni" = "v_CA16_5123")
comp11v <- c("total" = "v_CA11N_1801", "none" = "v_CA11N_1804", "sec" = "v_CA11N_1807",
             "psec" = "v_CA11N_1810", "uni" = "v_CA11N_1822")
comp06v <- c("total" = "v_CA06_1248", "none" = "v_CA06_1249", "sec" = "v_CA06_1251",
             "cert" = "v_CA06_1250", "uni" = "v_CA06_1256")

comp21 <- get_census(dataset = "CA21",
                      regions = list(CSD = (2465005)),
                      level = "CSD",
                      vectors = comp21v) |> 
  mutate(Year = 2021) |> 
  select(Year, total, none, sec, psec, uni)

comp16 <- get_census(dataset = "CA16",
                     regions = list(CSD = (2465005)),
                     level = "CSD",
                     vectors = comp16v) |> 
  mutate(psec = psec - uni) |> 
  mutate(Year = 2016) |> 
  select(Year, total, none, sec, psec, uni)

comp11 <- get_census(dataset = "CA11",
                     regions = list(CSD = (2465005)),
                     level = "CSD",
                     vectors = comp11v) |> 
  mutate(psec = psec - uni) |> 
  mutate(Year = 2011) |> 
  select(Year, total, none, sec, psec, uni)

comp06 <- get_census(dataset = "CA06",
                     regions = list(CSD = (2465005)),
                     level = "CSD",
                     vectors = comp06v) |> 
  mutate(psec = cert - sec - uni) |> 
  mutate(Year = 2006) |> 
  select(Year, total, none, sec, psec, uni)

comp <- bind_rows(comp06, comp11, comp16, comp21) |> 
  mutate(Year = as.factor(Year),
         none = round(none / total * 100, 1),
         sec = round(sec / total * 100, 1),
         psec = round(psec / total * 100, 1),
         uni = round(uni / total * 100, 1)) |> 
  select(-total) |> 
  pivot_longer(cols = -Year, names_to = "level", values_to = "prop") |> 
  mutate(prop_per = paste0(prop, "%")) |> 
  mutate(level = factor(level, levels = c("uni", "psec", "sec", "none"))) |> 
  group_by(Year) |> 
  mutate(cumulative = cumsum(prop) - 0.5 * prop) |> 
  ungroup()

ggplot(comp, aes(x = Year, y = prop, fill = level)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = prop_per, y = cumulative), vjust = 0.5, color = "white", size = 3) +
  labs(x = "Année") +
  scale_fill_manual(values = c("none" = "#252c3d", "sec" = "#3d4a66",
                               "psec" = "#6C83B5", "uni" = "#98A8CB"),
                    labels = c("none" = "Aucun certificat, diplôme ou grade",
                               "sec" = "Diplôme d'études secondaires ou\nattestation d'équivalence",
                               "psec" = "Certificat ou diplôme d’études\npostsecondaires inférieur au baccalauréat",
                               "uni" = "Baccalauréat ou grade supérieur")) +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "bottom",
        plot.title = element_blank(), axis.title.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.x = element_blank(), axis.ticks.x = element_blank()) +
  guides(fill = guide_legend(ncol = 2))

# Education by Sex --------------------------------------------------------
edus_v <- c("m15_64" = "v_CA21_69", "f15_64" = "v_CA21_70",
            "m15_19" = "v_CA21_72", "f15_19" = "v_CA21_73",
            "m20_24" = "v_CA21_90", "f20_24" = "v_CA21_91",
            "m_none" = "v_CA21_5869", "f_none" = "v_CA21_5870",
            "m_sec" = "v_CA21_5872", "f_sec" = "v_CA21_5873",
            "m_psec" = "v_CA21_5878", "f_psec" = "v_CA21_5879",
            "m_uni" = "v_CA21_5896", "f_uni" = "v_CA21_5897")

edus <- get_census(dataset = "CA21",
                   regions = list(CSD = (2465005)),
                   level = "CSD",
                   vectors = edus_v) |> 
  mutate(m_total = m15_64 - m15_19 - m20_24, f_total = f15_64 - f15_19 - f20_24) |> 
  mutate(m_none = round(m_none / m_total * 100, 1), f_none = round(f_none / f_total * 100, 1),
         m_sec = round(m_sec / m_total * 100, 1), f_sec = round(f_sec / f_total * 100, 1),
         m_psec = round(m_psec / m_total * 100, 1), f_psec = round(f_psec / f_total * 100, 1),
         m_uni = round(m_uni / m_total * 100, 1), f_uni = round(f_uni / f_total * 100, 1)) |> 
  select(m_none, f_none, m_sec, f_sec, m_psec, f_psec, m_uni, f_uni) |> 
  pivot_longer(everything(), names_to = "level", values_to = "prop") |> 
  mutate(education = case_when(str_ends(level, "none") ~ "none",
                               str_ends(level, "_sec") ~ "sec",
                               str_ends(level, "psec") ~ "psec",
                               str_ends(level, "uni") ~ "uni",
                               TRUE ~ "Other"),
         sex = case_when(str_starts(level, "m") ~ "Hommes",
                          str_starts(level, "f") ~ "Femmes",
                          TRUE ~ "Total"),
         percent = paste0(prop, "%")) |> 
  select(-level) |> 
  mutate(education = factor(education, levels = c("none", "sec", "psec", "uni")))

ggplot(edus, aes(x = education, y = prop, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = percent, y = 1), vjust = -0.5, color = "white", size = 4, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("Femmes" = "#CD718C", "Hommes" = "#A3B0D1")) +
  scale_x_discrete(labels = c("Aucun certificat, diplôme\nou grade",
                              "Diplôme d'études secondaires\nou attestation d'équivalence",
                              "Certificat ou diplôme\nd’études postsecondaires\ninférieur au baccalauréat",
                              "Baccalauréat ou grade supérieur")) +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "bottom",
        plot.title = element_blank(), axis.title.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.x = element_blank(), axis.ticks.x = element_blank()) +
  guides(fill = guide_legend(ncol = 2))
