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

