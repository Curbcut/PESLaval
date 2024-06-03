library(cancensus)
library(ggplot2)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)

# Languages spoken at work  -----------------------------------------------
# need to determine language most spoken at work and all languages used at work

#get 2021 Census Data
View(list_census_vectors("CA21"))

#all languages spoken at work - will compare Laval and Quebec variables
# v_CA21_6669 - Total - All languages used at work for the population 
## note: this is not single response so need way more
# v_CA21_6693 - Total - French and non-official language(s)
# v_CA21_6696 - Total - English, French and non-official language(s)
# v_CA21_6699 - Total - Multiple non-official languages

allworklangvector21 <- c(
  "Total" = "v_CA21_6669",
  "English" = "v_CA21_6672",
  "French" = "v_CA21_6675",
  "NonOff" = "v_CA21_6678",
  "EngFr" = "v_CA21_6687",
  "EngNonOff" = "v_CA21_6690",
  "FrNonOff" = "v_CA21_6693",
  "EngFrNonOff" = "v_CA21_6696",
  "MulNonOff" = "v_CA21_6699")


#get the data for Laval in 2021 (all languages used at work)
allworklanglav_21 <- 
  get_census(dataset = "CA21",
             regions = list(CSD = 2465005),
             level = "CSD",
             vectors = worklang21vector_all)

# get the data for QC in 2021 (all languages used at work)
allworklangqc_21 <- 
  get_census(dataset = "CA21",
             regions = list(PR = 24),
             level = "PR",
             vectors = allworklangvector21)

# merge the all languages spoken at work datasets 
allworklang21 <- 
  bind_rows(
    allworklanglv_21,
    allworklangqc_21) |> 
  mutate(region = c("laval", "quebec"))

#troubleshooting to combine data, look up names of columns 
print(names(allworklang21))
# reformat to combine and group language columns
com_allworklang21 <- allworklang21 |> 
  mutate(
    french_comb = rowSums(across(c(French, EngFr, FrNonOff, EngFrNonOff))),
    english_comb = rowSums(across(c(English, EngFr, EngNonOff, EngFrNonOff))),
    non_official_comb = rowSums(across(c(NonOff, EngNonOff, FrNonOff, EngFrNonOff, MulNonOff)))
    )

# Need a total to be able to calculate percentages
com_allworklang21 <- com_allworklang21 |> 
  mutate(
    total_comb = rowSums(across(c(french_comb, english_comb, non_official_comb)))
  )
  
# now we need to convert the totals to percentages
Perallworklang21 <- com_allworklang21 |> 
  group_by(region) |> 
  mutate(across(c(french_comb, english_comb, non_official_comb), ~./ total_comb*100))

# tidy the dataframes of the percentages by doing pivot_longer 
tidyallworklang21 <- 
  pivot_longer(Perallworklang21, cols = c(french_comb:non_official_comb),
               names_to = "language", values_to = "percentage")
#trying non percent 
testtidyworklang <- 
  pivot_longer(com_allworklang21, cols = c(french_comb:non_official_comb),
               names_to = "language", values_to = "people")

# test plot 

ggplot(tidyallworklang21, aes(x = region, y = percentage, fill = language)) + 
  geom_bar(stat = "identity", position = "stack") 

ggplot(testtidyworklang, aes(x = region, y = people, fill = language))+
  geom_bar(stat = "identity", position = "stack")

# I want to reorder to show french, english, non offical in the stacked bar graph 
# need to load forcats
library(forcats)

retidyallworklang21 <- tidyallworklang21 |> 
  mutate(language = fct_relevel(language, 
                                "non_official_comb","english_comb","french_comb")) 

# try plotting again
ggplot(retidyallworklang21, aes(x = region, y = percentage, fill = language)) + 
  geom_bar(stat = "identity", position = "stack")

# try to label with percentages 

#define the work language variable
worklang01vector <- c(
  "Total" = "v_CA01_226",
  "English" = "v_CA01_227",
  "French" = "v_CA01_228",
  "Non-official" = "v_CA01_229")

# want to look at the top 10 languages used at work in Laval
# This is the parent vector for language spoken most often at home in Laval in 2016
worklang_parent21 <- find_census_vectors("Single Responses", "CA21") %>% 
  filter(vector == "v_CA21_6717") 

#Select all leaf nodes of this vector. The parameter TRUE returns only the finite leaves among child nodes. 

language_children21 <- worklang_parent21 %>%
  child_census_vectors(TRUE)

# Store the vector list for later
language_vectors21 <- language_children21 %>% pull(vector)
View(language_vectors21)

# now I will try to calculate the top 10 languages spoken at work in Laval
# grab census data for laval 2021
laval10_21 <- get_census(dataset = "CA21",
                           regions = list(CSD= 2465005),
                           level = "CSD",
                           vectors = c(language_vectors21), 
                           geo_format = "sf", 
                           labels = "short")

# now i need to figure out the top 10 languages
worklaval10_ca21 <- laval10_21 |> 
  tidyr::gather(key = language, value = lang_count, v_CA21_6867:v_CA21_7341) |> 
  top_n(10, lang_count) |> 
  inner_join(list_census_vectors(dataset = "CA21"), by =c("language" = "vector")) |> 
  select(language, label, lang_count) |> 
  arrange(desc(lang_count))

#variables

#	v_CA21_6705 - total single responses 
# v_CA21_6717 - total non-official languages 


# Language used most often at work ----------------------------------------


# v_CA21_6702 -Total - Language used most often at work for the population aged 15 years and over who worked since January 1, 2020, in private households, 2021 Census
# v_CA21_6705 - total of single responses

worklangvector_21 <- c(
  "total" = "v_CA21_6705",
  "official" = "v_CA21_6708",
  "english" = "v_CA21_6711", 
  "french" = "v_CA21_6714",
  "non_off" = "v_CA21_6717")
  
# get data for laval and quebec: first laval

#get the data for Laval in 2021 (language used most at work)
worklanglav_21 <- 
  get_census(dataset = "CA21",
             regions = list(CSD = 2465005),
             level = "CSD",
             vectors = worklangvector_21)

# get the data for QC in 2021 (all languages used at work)

worklangqc_21 <- 
  get_census(dataset = "CA21",
             regions = list(PR = 24),
             level = "PR",
             vectors = worklangvector_21)

# merge the laval and quebec data into one dataset

worklang21 <- 
  bind_rows(
    worklanglav_21,
    worklangqc_21) |> 
  mutate(region = c("laval", "quebec"))

# convert to percentages so they can be compared
# now we need to convert the totals to percentages
Perworklang21 <- worklang21|> 
  group_by(region) |> 
  mutate(across(c(english, french, non_off), ~./ total*100))

# tidy the dataframes of the percentages by doing pivot_longer 

tidyworklang21 <- 
  pivot_longer(Perworklang21, cols = c(english:non_off),
               names_to = "language", values_to = "percentage")

# test print 
ggplot(tidyworklang21, aes(x = region, y = percentage, fill = language))+
geom_bar(stat = "identity", position = "stack")

install.packages("treemap")
library(treemap)

#want to try to plot as tree map

treemap(tidyworklang21, 
        index = "language",
        vSize = "percentage",
        type = "index")


# I want to reorder to show french, english, non offical in the stacked bar graph 
# need to load forcats
library(forcats)

retidyworklang21 <- tidyworklang21 |> 
  mutate(language = fct_relevel(language, 
                                "non_off","english","french")) 
# try plotting again
ggplot(retidyworklang21, aes(x = region, y = percentage, fill = language)) +
  geom_bar(stat = "identity", position = "stack")

#