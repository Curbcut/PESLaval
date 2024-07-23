### LANGUES ####################################################################
source("R/01_startup.R")

# 1996 Census Data - Knowledge of official languages
LOffLang96 <- cancensus::get_census(
  dataset = "CA1996",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(
    "Total" = "v_CA1996_310",
    "English" = "v_CA1996_311",
    "French" = "v_CA1996_312",
    "Both" = "v_CA1996_313",
    "Neither" = "v_CA1996_314"))

# 2001 Census Data - Knowledge of official languages
LOffLang01 <- cancensus::get_census(
  dataset = "CA01",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(
    "Total" = "v_CA01_213",
    "English" = "v_CA01_214",
    "French" = "v_CA01_215",
    "Both" = "v_CA01_216",
    "Neither" = "v_CA01_217"))

# 2006 Census Data - Knowledge of Official languages
LOffLan06 <- cancensus::get_census(
  dataset = "CA06",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(
    "Total" = "v_CA06_243",
    "English" = "v_CA06_244",
    "French" = "v_CA06_245",
    "Both" = "v_CA06_246",
    "Neither" = "v_CA06_247"))

# 2016 Census Data - Knowledge of Official Languages in Laval
LOffLang16 <- cancensus::get_census(
  dataset = "CA16",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(
    "Total" = "v_CA16_512",
    "English" = "v_CA16_515",
    "French" = "v_CA16_518",
    "Both" = "v_CA16_521",
    "Neither" = "v_CA16_524"))

# 2016 Census Data - Knowledge of Official Languages in Laval
LOffLang11 <- cancensus::get_census(
  dataset = "CA11",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(
    "Total" = "v_CA11F_551",
    "English" = "v_CA11F_554",
    "French" = "v_CA11F_557",
    "Both" = "v_CA11F_560",
    "Neither" = "v_CA11F_563"))

# 2021 Census Data - Knowledge of Official Languages in Laval
LOffLang21 <- cancensus::get_census(
  dataset = "CA21",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(
    "Total" = "v_CA21_1144",
    "English" = "v_CA21_1147",
    "French" = "v_CA21_1150",
    "Both" = "v_CA21_1153",
    "Neither" = "v_CA21_1156"))

# Defining the needed census variables 
# 2016 Census
# v_CA16_512 Total Total - Knowledge of official languages
# v_CA16_515 Total English only
# v_CA16_518 Total French only
# v_CA16_521 Total English and French
# v_CA16_524 Total Neither English nor French 

#census Data for QC 2021
QCoffLang21 <- cancensus::get_census(
  dataset = "CA21",
  regions = list(PR = 24),
  level = "PR",
  vectors = c(
    "Total" = "v_CA21_1144",
    "English" = "v_CA21_1147",
    "French" = "v_CA21_1150",
    "Both" = "v_CA21_1153",
    "Neither" = "v_CA21_1156"))

#Define function to calculate percentages
calculate_percentages <- function(data) {
  total_population <- data$Total
  data <- data %>%
    mutate(across(English:Neither, ~. / total_population*100))
  return(data)
}

#calculate percentages for each region and year
PerLoffLang96 <- calculate_percentages(LOffLang96)
PercLoffLang01 <- calculate_percentages(LOffLang01)
PercLoffLang06 <- calculate_percentages(LOffLan06)
PercLoffLang11 <- calculate_percentages(LOffLang11)
PerLoffLang16 <- calculate_percentages(LOffLang16)
PerLoffLang21 <-  calculate_percentages(LOffLang21)
PerQCoffLang21 <- calculate_percentages(QCoffLang21)

# tidy the dataframes of the percentages by doing pivot_longer 
LtidyOffLang96 <- pivot_longer(PerLoffLang96, cols = c(English:Neither), 
                               names_to = "Language", values_to = "Percentage")
LtidyOffLang01 <- pivot_longer(PercLoffLang01, cols = c(English:Neither),
                               names_to = "Language", values_to = "Percentage")
LtidyOffLang06 <- pivot_longer(PercLoffLang06, cols = c(English:Neither),
                               names_to = "Language", values_to = "Percentage")
LtidyOffLang11 <- pivot_longer(PercLoffLang11, cols = c(English:Neither),
                               names_to = "Language", values_to = "Percentage")
LtidyOffLang16 <-  pivot_longer(PerLoffLang16, cols = c(English:Neither), 
                                names_to = "Language", values_to = "Percentage")
LtidyOffLang21 <- pivot_longer(PerLoffLang21, cols = c(English:Neither),
                               names_to = "Language", values_to = "Percentage")
QCtidyOffLang21 <- pivot_longer(PerQCoffLang21, cols = c(English:Neither),
                                names_to = "Language", values_to = "Percentage")

# Mutate to add the years to each data frame by adding a column
tidyLOLang96 <- LtidyOffLang96 |> 
  mutate(Year = "1996")
tidyLOlang01 <- LtidyOffLang01 |> 
  mutate(Year = "2001")
tidyLOLang06 <- LtidyOffLang06 |> 
  mutate(Year= "2006")
tidyLOLang11 <- LtidyOffLang11 |> 
  mutate(Year = "2011")
tidyLOLang16 <- LtidyOffLang16 |> 
  mutate(Year = "2016")
tidyLOLang21 <- LtidyOffLang21 |> 
  mutate(Year = "2021")

# need to add laval and quebec as the region
tidyLOLang21 <- tidyLOLang21 |> 
  mutate(Region = "Laval")
tidyQCLang21 <- QCtidyOffLang21 |> 
  mutate(Region = "Quebec")

# Combine the pivot tables - use bind_rows function [comb2021offlang <- bind_rows(TidyPerLoffLang16, TidyPerLoffLang21)]
combinedKnowOffLang <- bind_rows(tidyLOLang96, tidyLOlang01, tidyLOLang06, tidyLOLang11, tidyLOLang16, tidyLOLang21)

# combine the pivot tables using bind rows
knowledgeLang21 <- bind_rows(tidyLOLang21, tidyQCLang21)

combinedKnowOffLang$Language <- factor(combinedKnowOffLang$Language,
                                       levels = c("Neither", "English", "French", "Both"),
                                       labels = c("Aucun", "Anglais", "Français", "Les deux"))

knowledge_official <- 
  ggplot(combinedKnowOffLang, aes(x = Year, y = Percentage, fill = Language)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = paste0(round(Percentage),"%")),
            position = position_fill(vjust = 0.5),
            size = 3,
            color = "white") +
  ylab(NULL) +
  xlab(NULL) +
  scale_fill_manual(values = c("Aucun" = color_theme("browndemographics"), 
                               "Anglais" = color_theme("pinkhealth"), 
                               "Français" = color_theme("blueexplorer"), 
                               "Les deux" = color_theme("greenecology"))) +
  guides(fill = guide_legend(reverse = TRUE)) +
  gg_cc_theme_no_sf +
  theme(legend.title = element_blank())

ggplot2::ggsave(filename = here::here("output/axe1/knowledge_official.png"), 
                plot = knowledge_official, width = 6, height = 4)

# Values for the text
bilingual_1996 <- combinedKnowOffLang$Percentage[
  combinedKnowOffLang$Year == 1996 & combinedKnowOffLang$Language == "Les deux"
]
bilingual_1996 <- convert_pct(bilingual_1996 / 100)
bilingual_2021 <- combinedKnowOffLang$Percentage[
  combinedKnowOffLang$Year == 2021 & combinedKnowOffLang$Language == "Les deux"
]
bilingual_2021 <- convert_pct(bilingual_2021 / 100)

# Plot the difference with Quebec
reorder21 <- knowledgeLang21 %>%
  mutate(Language = fct_relevel(Language,
                                "Neither", "English", "French", "Both"))

# now to plot and compare laval to quebec in 2021 
ggplot(reorder21, aes(x=Region, y=Percentage, fill= Language)) +
  geom_bar(stat="identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = paste0(round(Percentage),"%")),
            position = position_fill(vjust = 0.5),
            size = 3,
            color = "white") +
  labs(y = "Percentage", fill = "Language") +
  scale_fill_discrete(
    labels = c("Neither" = "Neither",
               "English" = "English Only",
               "French" = "French Only",
               "Both" = "Both")) 

# First Language - Knowledge of offical Language -------------------------
#First language()

#Need to find all the census vectors: 
find_census_vectors("first language", dataset = "CA1996", type = "total", 
                    query_type = "keyword", interactive = FALSE)

# 1996
# v_CA1996_315 - Total
# v_CA1996_319 - Neither
# v_CA1996_316 - English
# v_CA1996_318 - Both
# v_CA1996_317 - French 

find_census_vectors("first language", dataset = "CA01", type = "total", 
                    query_type = "keyword", interactive = FALSE)
# 2001 First language spoken
# v_CA01_218 - Total
# v_CA01_222 - Neither
# v_CA01_219 - English 
# v_CA01_221 - Both
# v_CA01_220 - French

find_census_vectors("first language", dataset = "CA06", type = "total", 
                    query_type = "keyword", interactive = FALSE)
# 2006 First Language spoken
# v_CA06_248 - Total
# v_CA06_252 - Neither
# v_CA06_249 - English
# v_CA06_251 - Both
# v_CA06_250 - French

find_census_vectors("first language", dataset = "CA16", type = "total", 
                    query_type = "keyword", interactive = FALSE)
# 2016 First Language Spoken
# v_CA16_527 - Total
# v_CA16_539 - Neither 
# v_CA16_530 - English
# v_CA16_536 - Both
# v_CA16_533 - French

find_census_vectors("first language", dataset = "CA21", type = "total", 
                    query_type = "keyword", interactive = FALSE)
# 2021 First Language Spoken 
# v_CA21_1159 - Total
# v_CA21_1171 - Neither
# v_CA21_1162 - English
# v_CA21_1168 - Both
# v_CA21_1165 - French 

# 1996 Census Data - First official language spoken
FirLang96 <- cancensus::get_census(
  dataset = "CA1996",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(
    "Total" = "v_CA1996_315",
    "Neither" = "v_CA1996_319",
    "English" = "v_CA1996_316",
    "Both" = "v_CA1996_318",
    "French" = "v_CA1996_317"))

# 2001 - First Official Langauge spoken - get census data
FirLang01 <- cancensus::get_census(
  dataset = "CA01", 
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(
    "Total" = "v_CA01_218",
    "Neither" = "v_CA01_222",
    "English" = "v_CA01_219",
    "Both" = "v_CA01_221",
    "French" = "v_CA01_220")
)  

# 2006 - First Official Language Spoken 
FirLang06 <-  cancensus::get_census(
  dataset = "CA06",
  regions = list(CSD= 2465005),
  level = "CSD",
  vectors = c(
    "Total" = "v_CA06_248",
    "Neither" = "v_CA06_252",
    "English" = "v_CA06_249",
    "Both" = "v_CA06_251",
    "French" = "v_CA06_250"))

# 2016 - First Official Language Spoken 
FirLan16 <-  cancensus::get_census(
  dataset = "CA16",
  regions = list(CSD= 2465005),
  level = "CSD",
  vectors = c(
    "Total" = "v_CA16_527",
    "Neither" = "v_CA16_539",
    "English" = "v_CA16_530",
    "Both" = "v_CA16_536",
    "French" = "v_CA16_533"))

# 2021 - First Official Language Spoken
FirLang21 <-  cancensus::get_census(
  dataset = "CA21",
  regions = list(CSD= 2465005),
  level = "CSD",
  vectors = c(
    "Total" = "v_CA21_1159",
    "Neither" = "v_CA21_1171",
    "English" = "v_CA21_1162",
    "Both" = "v_CA21_1168",
    "French" = "v_CA21_1165"))

FirLang21qc <- cancensus::get_census(
  dataset = "CA21",
  regions = list(PR= 24),
  level = "PR",
  vectors = c(
    "Total" = "v_CA21_1159",
    "Neither" = "v_CA21_1171",
    "English" = "v_CA21_1162",
    "Both" = "v_CA21_1168",
    "French" = "v_CA21_1165"))

#ensure that data is data frame
FirLang96 <- as.data.frame(FirLang96)
library(dplyr)

#Need to calculate percentages for each dataset
calc_percentages <- function(data) {
  total_population <- data$Total
  data <- data %>%
    mutate(across(c(Neither, English, Both, French), ~. / total_population*100))
  return(data)
}

#Check data structure: 
str(FirLang96)
# Percent of first official languages spoken
PerFirLang96 <- calc_percentages(FirLang96)
PerFirLang01 <- calc_percentages(FirLang01)
PerFirLang06 <- calc_percentages(FirLang06)
PerFirLang16 <- calc_percentages(FirLan16)
PerFirLang21 <- calc_percentages(FirLang21)
PerFirLangqc21 <- calc_percentages(FirLang21qc)

## tidy the dataframes of the percentages by doing pivot_longer 
tidyFirLang96 <- pivot_longer(PerFirLang96, cols = c(Neither, English, Both, French), 
                              names_to = "Language", values_to = "Percentage")
tidyFirLang01 <- pivot_longer(PerFirLang01, cols = c(Neither, English, Both, French),
                              names_to = "Language", values_to = "Percentage")
tidyFirLang06 <- pivot_longer(PerFirLang06, cols = c(Neither, English, Both, French),
                              names_to = "Language", values_to = "Percentage")
tidyFirLang16 <- pivot_longer(PerFirLang16, cols = c(Neither, English, Both, French),
                              names_to = "Language", values_to = "Percentage")
tidyFirLang21 <- pivot_longer(PerFirLang21, cols = c(Neither, English, Both, French),
                              names_to = "Language", values_to = "Percentage")
tidyFirLang21qc <- pivot_longer(PerFirLangqc21, cols = c(Neither, English, Both, French),
                                names_to = "Language", values_to = "Percentage")

# Mutate to add the years to each data frame by adding a column
tidyFirLang96 <- tidyFirLang96 |> 
  mutate(Year = "1996")
tidyFirLang01 <- tidyFirLang01 |> 
  mutate(Year = "2001")
tidyFirLang06 <- tidyFirLang06 |> 
  mutate(Year = "2006")
tidyFirLang16 <- tidyFirLang16 |> 
  mutate(Year = "2016")
tidyFirLang21 <- tidyFirLang21 |> 
  mutate(Year = "2021", region = "Laval")
tidyFirLang21qc <- tidyFirLang21qc |> 
  mutate(Year = "2021", region = "Quebec")

# Combine all the pivot tables - use bind_rows function 
# ex: combinedKnowOffLang <- bind_rows(tidyLOLang96, tidyLOlang01, tidyLOLang06, tidyLOLang16, tidyLOLang21)
combinedFirLang <- bind_rows(tidyFirLang96, tidyFirLang01, tidyFirLang06, tidyFirLang16, tidyFirLang21)

#going to compare Laval to Quebec
combinedFir21 <- 
  bind_rows(tidyFirLang21, tidyFirLang21qc)

#Now to plot
#ggplot(combinedKnowOffLang, aes(x = Year, y = Percentage, fill = Language)) +
ggplot(combinedFirLang, aes(x= Year, y = Percentage, fill = Language)) +
  geom_bar(stat = "identity", position = "fill")

ggplot(combinedFir21, aes(region, Percentage, fill = Language)) +
  geom_bar(stat = "identity", position = "fill")
#need to reorder
## Reorder following a precise order

pFirLang21 <- combinedFir21 %>%
  mutate(Language = fct_relevel(Language, 
                                "Neither", "English", "Both", 
                                "French")) 
pirFirLang <- combinedFirLang |> 
  mutate(Language = fct_relevel(Language,
                                "Neither", "English", "Both", "French"))
#install.packages("scales")
library(scales) #this allows the labelpercent function to display as percent on y axis

ggplot(pFirLang21, aes(x=region, y=Percentage, fill= Language)) +
  geom_bar(stat="identity", position = "fill") +
  labs(title = "First official Language Spoken 2021", x = "region", y = "Percentage") +
  scale_y_continuous(labels = label_percent()) + 
  theme_minimal()

ggplot(pirFirLang, aes(x=Year, y=Percentage, fill= Language)) +
  geom_bar(stat="identity", position = "fill") +
  labs(title = "First official Language Spoken 2021", x = "region", y = "Percentage") +
  scale_y_continuous(labels = label_percent()) + 
  theme_minimal()

# add percentages to the bars
ggplot(pirFirLang, aes(x = Year, y = Percentage, fill = Language)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = paste0(round(Percentage),"%")),
            position = position_fill(vjust = 0.5),
            size = 3,
            color = "white") +
  labs(title = "First official Language Spoken 2021", y = "Percentage") 

# Languages spoken at Home ------------------------------------------------
find_census_vectors("language spoken at home", dataset = "CA21", type = "total", 
                    query_type = "keyword", interactive = FALSE)
blip <-  cancensus::get_census(
  dataset = "CA16",
  regions = list(CSD= 2465005),
  level = "CSD",
  vectors = c(
    "Parent" = "v_CA16_1358"))

language_parent16 <- find_census_vectors("Single Responses", "CA16") %>% 
  filter(vector == "v_CA16_1358") 
# This is the parent vector for language spoken most often at home in Laval in 2016

#Select all leaf nodes of this vector. The parameter TRUE returns only the finite leaves among child nodes. 

language_children16 <- language_parent16 %>%
  child_census_vectors(TRUE)

# Store the vector list for later
newlanguage_vectors16 <- language_children16 %>% pull(vector)
View(newlanguage_vectors16)

#now I will try to calculate the top 10 languages spoken at home in Laval
# grab census data for laval 2016
laval10_ca16 <- get_census(dataset = "CA16",
                           regions = list(CSD= 2465005),
                           level = "CSD",
                           vectors = c(language_vectors16), 
                           geo_format = NA, 
                           labels = "short")

# now i need to figure out the top 10 languages
newtestlaval10_ca16 <- laval10_ca16 |> 
  tidyr::gather(key = language, value = lang_count, v_CA16_1364:v_CA16_1937) |> 
  top_n(10, lang_count) |> 
  inner_join(list_census_vectors(dataset = "CA16"), by =c("language" = "vector")) |> 
  select(language, label, lang_count) |> 
  arrange(desc(lang_count))

#will need to calculate percent based on the parent vector
homelang10_ca16 <- newtestlaval10_ca16 |> pull(language)
#need to add the parent vector to this to calculate share and percentages
homelang10_ca16 <- c(language_parent16 |> pull(vector), language_vectors16)

# Can try to plot these by CT
laval10_2016CT <- get_census(dataset = "CA16", level = "CT", 
                             regions = list(CSD= 2465005),
                             vectors = homelang10_ca16, 
                             geo_format = "sf",
                             labels = "short")

# Need to convert this data to long format
laval10_2016CTtidy <- laval10_2016CT |> 
  tidyr::gather(key = language, value = lang_count, v_CA16_1364:v_CA16_1937) |> 
  rename('Single Responses' = v_CA16_1358) |> 
  filter(language !="v_CA16_1364") |> 
  mutate(lang_share_sr16 = lang_count/'Single Responses',
         lange_share_sr16 = ifelse(is.na(lang_share_sr16), 0,
                                   lang_share_sr16)) |> 
  inner_join(list_census_vectors(dataset = "CA16"), by =c("language" = "vector"))

#trying again with a diff script
#perlaval2016.10 <- laval10_2016CT |> 
# mutate(laval10_2016CT(across(c(v_CA16_1364:v_CA16_1937), /v_CA16_1358*100)))
#below from chatgpt
perlaval2016.10 <- laval10_2016CT %>%
  mutate(across(starts_with("v_CA16_"), ~ . / v_CA16_1358 * 100, .names = "percent_{col}"))

tidylavalper2016 <-  pivot_longer(perlaval2016.10, 
                                  cols = c(percent_v_CA16_1367,
                                           percent_v_CA16_1364, 
                                           percent_v_CA16_1658, 
                                           percent_v_CA16_1958, 
                                           percent_v_CA16_1883),
                                  names_to = "Language", values_to = "Percentage")
#percent_
# v_CA16_1367 - French
# v_CA16_1364 - English
# v_CA16_1658 - Arabic
# v_CA16_1958 - Spanish
# v_CA16_1883 - Greek
# v_CA16_1775 - Armenian
# v_CA16_1955 - Romanian
# v_CA16_1949 - Italian
# v_CA16_1742 - Creole, n.o.s.
# v_CA16_1937 - Persian (Farsi)

#variables to plot - just 5 languages
lang.toplot2016 <- c("percent_v_CA16_1367", "percent_v_CA16_1364", 
                     "percent_v_CA16_1658", 
                     "percent_v_CA16_1958", 
                     "percent_v_CA16_1883")
#filter the data set to just include these variables
filterlaval2016.5 <- perlaval2016.10 |> 
  select("percent_v_CA16_1367", "percent_v_CA16_1364","percent_v_CA16_1658", 
         "percent_v_CA16_1958",
         "percent_v_CA16_1883")

#need to rename the columns: 
filterlaval2016.5 <- rename(filterlaval2016.5, "French" = "percent_v_CA16_1367", 
                            "English" = "percent_v_CA16_1364",
                            "Arabic" = "percent_v_CA16_1658",
                            "Spanish" = "percent_v_CA16_1958",
                            "Greek" = "percent_v_CA16_1883")


# ggplot
map_theme <- theme_void() + 
  theme(plot.title=element_text(face="bold", hjust = 0.5)) + 
  theme(plot.subtitle=element_text(hjust = 0.5)) + 
  theme(plot.caption=element_text(size=8, margin=margin(t=10), hjust = 0.95))

#plot - Arabic

ggplot(filterlaval2016.5)+
  geom_sf(aes(fill = Arabic)) +
  map_theme + scale_fill_viridis_c()

# french 
ggplot(filterlaval2016.5)+
  geom_sf(aes(fill = French)) +
  map_theme + scale_fill_viridis_c()
# English 
ggplot(filterlaval2016.5)+
  geom_sf(aes(fill = English)) +
  map_theme + scale_fill_viridis_c()

# Spanish
ggplot(filterlaval2016.5)+
  geom_sf(aes(fill = Spanish)) +
  map_theme + scale_fill_viridis_c()

#Greek 
ggplot(filterlaval2016.5)+
  geom_sf(aes(fill = Greek)) +
  map_theme + scale_fill_viridis_c()

#setting up the laval census info
# Grabbing census information from Laval. 
library(ggplot2)
library(cancensus)

laval_census <- 
  cancensus::get_census(dataset = "CA21", 
                                      regions = list(CSD = 2465005), 
                                      level = "CSD")

set_cancensus_cache_path('C:/Users/Dell7510/Documents/MUP 2/RA/Curbcut', FALSE)
#"C:/Users/Dell7510/Documents/MUP 2/RA/Curbcut", install=TRUE
#get help to cache it
set_cancensus_cache_path("C:/Users/Dell7510/Documents/MUP 2/RA/Curbcut", install = TRUE)

#calculate population density
CT <- cancensus::get_census(dataset = "CA21", 
                            regions = list(CSD = 2465005), 
                            level = "CT", 
                            geo_format = "sf")
library(sf)
library(dplyr)

dataset <-  "CA21"

#define official language
#to look for vector can run a keyword search ie:
#find_census_vectors('commuting duration', dataset = 'CA11', type = 'female', 
# query_type = 'keyword'

languag.off <- 
  find_census_vectors('languages', dataset = 'CA21', type = 'all', query_type = 'keyword')

#going to generate a table using the census data frame including language vectors
# key variables for knowledge of official languages for 2021 census are:
# v_CA21_1144 - total - total Knowledge of official languages for the total population excluding institutional residents
# v_CA21_1147 - english only - total
# v_CA21_1150 - french only - total 
# v_CA21_1153 - english and french - total
# v_CA21_1156 - Neither English nor French - total

#langs_off.know <- get_census(dataset, level = "CSD", regions = csd100_list, vectors = language_vectors, geo_format = NA, labels = "short")

## PLOT CONTEXT FUNCTION #######################################################
# Get the Laval CSD to filter the rest
laval <- cancensus::get_census(dataset = "CA21",
                               regions = list(CSD = 2465005),
                               level = "CSD",
                               geo_format = "sf")
# Montreal CMA
CSDs <- cancensus::get_census(dataset = "CA21", 
                              regions = list(CMA = 24462), 
                              level = "CSD",
                              geo_format = "sf")
CSDs <- CSDs[CSDs$GeoUID != "2465005", ]
CSDs <- sf::st_union(CSDs)  

##devtools::install_github("mountainmath/cancensus") look at income statistics for the 10 most populous cities
#library(cancensus)
#dataset='CA16'
#level="CSD"
#median_income_vectors <- list_census_vectors(dataset, quiet=TRUE) %>% 
 # filter(type=="Total",grepl("Median",label),grepl("income",label)) %>% pull("vector") 
#regions <- list_census_regions(dataset) %>% filter(level==!!level) %>% top_n(10,pop) %>% as_census_region_list


# Official Languages Vector -----------------------------------------------

# v_CA21_1144 - total - total Knowledge of official languages for the total population excluding institutional residents
# v_CA21_1147 - english only - total
# v_CA21_1150 - french only - total 
# v_CA21_1153 - english and french - total
# v_CA21_1156 - Neither English nor French - total

langs_off.know <- 
  cancensus::get_census(dataset = "CA21",
                        regions = list(CSD = 2465005),
                        level = "CT",
                        vectors=c("v_CA21_1147", "v_CA21_1150", "v_CA21_1153", "v_CA21_1156"),


# Data Frame for CSD Official languages 2021 - Laval -----------------------------------

CSD.off.lang <- 
  cancensus::get_census(dataset = "CA21",
                        regions = list(CSD = 2465005),
                        level = "CSD",
                        vectors = c("Total" = "v_CA21_1144",
                                    "English" = "v_CA21_1147",
                                    "French" = "v_CA21_1150",
                                    "English and French" = "v_CA21_1153",
                                    "Neither English nor French" = "v_CA21_1156"))
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)

CSD.off.lang <- 
  CSD.off.lang |> 
  pivot_longer(-c(GeoUID:CMA_UID))
  
# visualizing this total 

ggplot(CSD.off.lang, aes(x = name, y = value)) +
  geom_col()

#filter data to remove total columns

filterCSD.off.lang <- 
  CSD.off.lang |> 
  filter(!(name %in% c("Total")))

#Visualize filtered data 
ggplot(filterCSD.off.lang, aes(x = name, y = value)) + 
  geom_col

#make numbers normal not scientific notation

ggplot(filterCSD.off.lang, aes(x = name, y = value)) + 
  geom_col() + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#Need to mutate to find percentages? easier comparison, and then run this for QC
##NOt sure how to do this#
#CSD.off.lang_aspercent <- CSD.off.lang |> 
 # mutate("PercentEnglish" = "English"/"Total",
  #"PercentFrench" = "French"/"Total",
  #"PercentEnglish_French" = "English and French"/"Total", 
  #"PercentNeither" = "Neither English nor French"/"Total")

#trying again to figure out percentage
off.lang.CSD <- 
  cancensus::get_census(dataset = "CA21",
                        regions = list(CSD = 2465005),
                        level = "CSD",
                        vectors = c("Total" = "v_CA21_1144",
                                    "English" = "v_CA21_1147",
                                    "French" = "v_CA21_1150",
                                    "English and French" = "v_CA21_1153",
                                    "Neither English nor French" = "v_CA21_1156"))
#Trying to get the percent
Percentoff.lang.CSD <- off.lang.CSD |> 
 mutate(PercentEnglish = as.numeric(English)/as.numeric(Total)   
          #mutate(PercentFrench = "French"/"Total") |>    
          #mutate(PercentEnglish_French" = "English and French"/"Total") |> 
          #mutate(PercentNeither = "Neither English nor French"/"Total")

# Convert columns to numeric
#off.lang.CSD |> 
#data$English <- as.numeric(data$English)
#data$
#data$Total <- as.numeric(data$Total)

off.lang.CSD

#trying again to get percent
Percentoff.lang.CSD <- off.lang.CSD |> 
  mutate(PercentEnglish = English/Total, 
           PercentFrench = French/Total,
         PercentBoth = `English and French`/Total,
         PercentNeither = `Neither English nor French`/Total) 

#pivot longer
Percentoff.lang.CSD <- 
  Percentoff.lang.CSD |> 
  pivot_longer(-c(GeoUID:CMA_UID))

#filter data to remove total columns

filterPercent.off.lang.CSD <- 
  Percentoff.lang.CSD |> 
  filter(!(name %in% c("Total")))

#visualize
#ggplot(filterPercent.off.lang.CSD, aes(x = name, y = value)) + 
#geom_col() + 
  #scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


# Good Script -------------------------------------------------------------

#Knowledge of official Languages

# Compile all the vectors and data sources --------------------------------


find_census_vectors("Knowledge official language", dataset = "CA01", type = "total", 
                    query_type = "keyword", interactive = FALSE)
find_census_vectors("Knowledge official language", dataset = "CA1996", type = "total", 
                    query_type = "keyword", interactive = FALSE)
find_census_vectors("Knowledge official language", dataset = "CA06", type = "total", 
                    query_type = "keyword", interactive = FALSE)

# 1996 Knowledge vectors -Official Languages (20 percent sample data)
# v_CA1996_310 - Total
# v_CA1996_311 - English
# v_CA1996_312 - French
# v_CA1996_313 - Both - English and French
# v_CA1996_314 - Neither - English and French


# 2001 knowledge vectors - Official Languages (20 percent sample data)
# v_CA01_213 - Total
# v_CA01_214 - English
# v_CA01_215 - French
# v_CA01_216 - Both - English and French
# v_CA01_217 - Neither - English and French

# 2006 Knowledge vectors - Official Languages (20 percent data)
# v_CA06_243 - Total
# v_CA06_244 - English
# v_CA06_245 - French
# v_CA06_246 - Both - English and French
# v_CA06_247 - Neither

# 2016 Census
# v_CA16_512 Total Total - Knowledge of official languages
# v_CA16_515 Total English only
# v_CA16_518 Total French only
# v_CA16_521 Total English and French
# v_CA16_524 Total Neither English nor French 

# 2021 Census
# v_CA21_1144 Total Knowledge of official languages for the total population
# v_CA21_1147 Total English only
# v_CA21_1150 Total French only
# v_CA21_1153 Total English and French
# v_CA21_1156 Total Neither English nor French

library(cancensus)
library(ggplot2)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)

# 1996 Census Data - Knowledge of official languages
LOffLang96 <- cancensus::get_census(
  dataset = "CA1996",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(
    "Total" = "v_CA1996_310",
    "English" = "v_CA1996_311",
    "French" = "v_CA1996_312",
    "Both" = "v_CA1996_313",
    "Neither" = "v_CA1996_314"))

# 2001 Census Data - Knowledge of official languages
LOffLang01 <- cancensus::get_census(
  dataset = "CA01",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(
    "Total" = "v_CA01_213",
    "English" = "v_CA01_214",
    "French" = "v_CA01_215",
    "Both" = "v_CA01_216",
    "Neither" = "v_CA01_217"))

# 2006 Census Data - Knowledge of Official languages
LOffLan06 <- cancensus::get_census(
  dataset = "CA06",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(
    "Total" = "v_CA06_243",
    "English" = "v_CA06_244",
    "French" = "v_CA06_245",
    "Both" = "v_CA06_246",
    "Neither" = "v_CA06_247"))

# 2016 Census Data - Knowledge of Official Languages in Laval
LOffLang16 <- cancensus::get_census(
  dataset = "CA16",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(
    "Total" = "v_CA16_512",
    "English" = "v_CA16_515",
    "French" = "v_CA16_518",
    "Both" = "v_CA16_521",
    "Neither" = "v_CA16_524"))

# 2021 Census Data - Knowledge of Official Languages in Laval
LOffLang21 <- cancensus::get_census(
  dataset = "CA21",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(
    "Total" = "v_CA21_1144",
    "English" = "v_CA21_1147",
    "French" = "v_CA21_1150",
    "Both" = "v_CA21_1153",
    "Neither" = "v_CA21_1156"))

# 2021 Census Data - knowledge of offical languages in QC
qcOffLang21 <- cancensus::get_census(
  dataset = "CA21",
  regions = list(PR = 24),
  level = "PR",
  vectors = c(
    "Total" = "v_CA21_1144",
    "English" = "v_CA21_1147",
    "French" = "v_CA21_1150",
    "Both" = "v_CA21_1153",
    "Neither" = "v_CA21_1156"))

#Define function to calculate percentages
calculate_percentages <- function(data) {
  total_population <- data$Total
  data <- data %>%
    mutate(across(English:Neither, ~. / total_population*100))
  return(data)
}

#calculate percentages for each region and year
PerLoffLang96 <- calculate_percentages(LOffLang96)
PercLoffLang01 <- calculate_percentages(LOffLang01)
PercLoffLang06 <- calculate_percentages(LOffLan06)
PerLoffLang16 <- calculate_percentages(LOffLang16)
PerLoffLang21 <-  calculate_percentages(LOffLang21)
PerqcoffLang21 <- calculate_percentages(qcOffLang21)

# tidy the dataframes of the percentages by doing pivot_longer 
LtidyOffLang96 <- pivot_longer(PerLoffLang96, cols = c(English:Neither), 
                               names_to = "Language", values_to = "Percentage")
LtidyOffLang01 <- pivot_longer(PercLoffLang01, cols = c(English:Neither),
                               names_to = "Language", values_to = "Percentage")
LtidyOffLang06 <- pivot_longer(PercLoffLang06, cols = c(English:Neither),
                               names_to = "Language", values_to = "Percentage")
LtidyOffLang16 <-  pivot_longer(PerLoffLang16, cols = c(English:Neither), 
                                names_to = "Language", values_to = "Percentage")
LtidyOffLang21 <- pivot_longer(PerLoffLang21, cols = c(English:Neither),
                               names_to = "Language", values_to = "Percentage")
QctidyoffLang21 <- pivot_longer(PerqcoffLang21, cols = c(English:Neither),
                                names_to = "Language", values_to = "Percentage")

# Mutate to add the years to each data frame by adding a column
tidyLOLang96 <- LtidyOffLang96 |> 
  mutate(Year = "1996")
tidyLOlang01 <- LtidyOffLang01 |> 
  mutate(Year = "2001")
tidyLOLang06 <- LtidyOffLang06 |> 
  mutate(Year= "2006")
tidyLOLang16 <- LtidyOffLang16 |> 
  mutate(Year = "2016")
tidyLOLang21 <- LtidyOffLang21 |> 
  mutate(Year = "2021", region = "Laval")
tidyQCOLang21 <- QctidyoffLang21 |> 
  mutate(Year = "2021", region = "Quebec")

# Combine the pivot tables - use bind_rows function [comb2021offlang <- bind_rows(TidyPerLoffLang16, TidyPerLoffLang21)]
combinedKnowOffLang <- bind_rows(tidyLOLang96, tidyLOlang01, tidyLOLang06, tidyLOLang16, tidyLOLang21)
combined2021 <- 
  bind_rows(tidyLOLang21, tidyQCOLang21)

#Now plot - Test print
ggplot(combinedKnowOffLang, aes(Year, y = Percentage, fill = Language)) +
  geom_bar(stat = "identity", position = "dodge")+
  theme_minimal()
#should delete above

#trying different method
ggplot(combinedKnowOffLang, aes(x = Year, y = Percentage, fill = Language)) +
  geom_bar(stat = "identity", position = "fill")

#plot laval to province for 2021
ggplot(combined2021, aes(x=region, y= Percentage, fill = Language)) +
  geom_bar(stat = "identity", position = "fill")

# reorder to show french, both, english, neither in the stacked bar graph
# need to install.packages("forcats")
install.packages("forcats")
library(forcats)
#can reorder
#This didn't work
#TestOffLangAll$Language <- fct_relevel(combinedKnowOffLang$Language, "Neither", "English," "Both", "French")

## Reorder following a precise order
reorder <- combined2021 %>%
  mutate(Language = fct_relevel(Language, 
                                "Neither", "English", "Both", 
                                "French")) 
ggplot(reorder, aes(x=region, y=Percentage, fill= Language)) +
  geom_bar(stat="identity", position = "fill") 



# Save the objects --------------------------------------------------------

qs::qsavem(knowledge_official, bilingual_1996, bilingual_2021,
           file = "data/axe1/language.qsm")
