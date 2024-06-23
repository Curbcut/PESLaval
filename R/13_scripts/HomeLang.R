library(cancensus)
library(ggplot2)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)

# Home Languages ----------------------------------------------------------


# Languages spoken (most) at Home ------------------------------------------------

# 1996
CT96 <- cancensus::get_census(dataset = "CA1996", 
                            regions = list(CSD = 2465005),
                            level = "CT",
                            geo_format = "sf")

View(list_census_vectors("CA1996"))

#Total population by home language 1996
# v_CA1996_322 - Total population by home language
# v_CA1996_323 - Single responses
# v_CA1996_324 - English
# v_CA1996_325 - French
# v_CA1996_326 -Non-official languages

#get all the variables for language spoken at home 1996
homelang96vector <- c(
  #"Total" = "v_CA1996_322", #I should use total of single responses which would be 323
  "Total" = "v_CA1996_323",
  "English" = "v_CA1996_324",
"French" = "v_CA1996_325",
"Non-official" = "v_CA1996_326")

#get the data for laval 1996
homelang96_laval <- get_census(dataset = "CA1996",
                               regions = list(CSD = 2465005),
                               level = "CSD",
                               vectors = homelang96vector)

#now for 2001
CT01 <- cancensus::get_census(dataset = "CA01", 
                            regions = list(CSD = 2465005),
                            level = "CSD", 
                            geo_format = "sf")
                            
View(list_census_vectors("CA01"))

homelang01vector <- c(
  "Total" = "v_CA01_226",
  "English" = "v_CA01_227",
  "French" = "v_CA01_228",
  "Non-official" = "v_CA01_229")

#Label as non

# now pull the data using the vectors

homelang01_laval <- get_census(dataset = "CA01",
                               regions = list(CSD = 2465005),
                               level = "CSD",
                               vectors = homelang01vector)

#now for 2006 - pulling total population by language spoken most often at home
View(list_census_vectors("CA06"))

homelang06vector <-  c(
  "Total" = "v_CA06_256",
  "English" = "v_CA06_257",
  "French" = "v_CA06_258",
  "Non-official" = "v_CA06_259")

# now pull the data 
homelang06_laval <- get_census(dataset = "CA06",
                               regions = list(CSD = 2465005),
                               level = "CSD",
                               vectors = homelang06vector)
#now for 2016
View(list_census_vectors("CA16"))

homelang16vector <-  c(
  "Total" = "v_CA16_1358",
  "English" = "v_CA16_1364",
  "French" = "v_CA16_1367",
  "Non-official" = "v_CA16_1370") 

homelang16_laval <- get_census(dataset = "CA16",
                               regions = list(CSD = 2465005),
                               level = "CSD",
                               vectors = homelang06vector)
#lastly data for 2021 (Total - Language spoken most often at home for the total population)
View(list_census_vectors("CA21"))
     
homelang21vector <- c(
  "Total" = "v_CA21_2203",
  "English" = "v_CA21_2209",
  "French" = "v_CA21_2212",
  "Non-official" = "v_CA21_2215")  

homelang21_laval <- get_census(dataset = "CA21",
                            regions = list(CSD = 2465005),
     level = "CSD",
     vectors = homelang21vector)

# also show for the province
homelang21_qc <- cancensus::get_census(
  dataset = "CA21",
  regions = list(PR = 24),
  level = "PR",
  vectors = c(homelang21vector))

#merge the laval datasets 
#bind_rows(pop_distribution_laval, pop_distribution_qc) |> mutate(name = c("Laval", "Quebec"), .before = GeoUID) |> select(-c(GeoUID:CMA_UID, C_UID)) |> 
  #pivot_longer(-name, names_to = "category") |> 
lavalhomelang <- 
  bind_rows(homelang96_laval, 
            homelang01_laval, 
            homelang06_laval, 
            homelang16_laval, 
            homelang21_laval) |> 
 mutate(year = c("1996", "2001", "2006", "2016", "2021"))

#merge the 2021 datasetst to compare Laval and Quebec
homelang21 <- 
  bind_rows(
    homelang21_laval,
    homelang21_qc) |> 
  mutate(region = c("Laval", "Quebec"))

#need to calculate percentage of each language relative to the total within each year
# check the column names of data frame
colnames(lavalhomelang)
print(colnames(lavalhomelang))
# make sure that all the values are numeric
lavalhomelang <- lavalhomelang %>%
  mutate(across(c(Total, English, French, "Non-official"), ~ as.numeric(gsub("[^0-9.]", "", .))))

# Check for any NA values after conversion
sum(is.na(lavalhomelang$Total))
sum(is.na(lavalhomelang$English))
sum(is.na(lavalhomelang$French))
sum(is.na(lavalhomelang$Non_official))

#need to convert the totals to percentages

lavalhomelang_percent <- lavalhomelang %>%
  group_by(year) %>%
  mutate(across(c(English, French, 'Non-official'), ~./ Total*100))
 View(lavalhomelang_percent)
 
 homelang21percent <- homelang21 |> 
   group_by(region) |> 
   mutate(across(c(English, French, 'Non-official'), ~./ Total*100))

 # tidy the dataframes of the percentages by doing pivot_longer 
 tidyhomelang <- 
   pivot_longer(lavalhomelang_percent, cols = c(English:'Non-official'),
                names_to = "Language", values_to = "Percentage")
 
 tidyhomelang21 <- 
   pivot_longer(homelang21percent, cols = c(English:'Non-official'),
                names_to = "Language", values_to = "Percentage")

 #now plot to see whats up!

 ggplot(tidyhomelang, aes(x = year, y = Percentage, fill = Language)) + 
   geom_bar(stat = "identity", position = "fill")
 
 # reorder to show french then english then non-official 
 library(forcats)
 retidyhomelang <- tidyhomelang |> 
   mutate(Language = fct_relevel(Language, "Non-official", "English", "French"))
 
 #trying again but adding percentages
 ggplot(retidyhomelang, aes(x=year, y=Percentage, fill= Language)) +
   geom_bar(stat="identity", position = "fill") +
   ggtitle("Languages spoken most often at home in Laval") +
   scale_y_continuous(labels = scales::percent) +
   geom_text(aes(label = paste0(round(Percentage),"%")),
             position = position_fill(vjust = 0.5),
             size = 3,
             color = "white") +
   labs(y = "Percentage")

#  geom_text(aes(label = Percentage,"%"), 
           position = position_stack(vjust = 0.5), size = 3)
 
#now compare Laval to Quebec in 2021 

 ggplot(retidyhomelang21, aes(x = region, y = Percentage, fill = Language)) + 
   geom_bar(stat = "identity", position = "fill") +
   ggtitle("Languages spoken most often at home in 2021") +
   scale_y_continuous(labels = scales::percent) +
   geom_text(aes(label = paste0(round(Percentage), "%")),
             position = position_fill(vjust = 0.5), 
             size = 3,
             color = "white") +
               labs(y="Percentage")
 
 # need to reorder to show the same order: non-official, english, and french
 retidyhomelang21 <- tidyhomelang21 |> 
   mutate(Language = fct_relevel(Language, "Non-official", "English", "French"))

 # I want to reorder to show french, english, non offical in the stacked bar graph  
 retidyhomelang <- tidyhomelang |> 
   mutate(Language = fct_relevel(Language, 
                                "Non-official","English","French")) 
 # now try plotting again
 ggplot(retidyhomelang, aes(x = year, y = Percentage, fill = Language)) + 
   geom_bar(stat = "identity", position = "fill")

#to do a stacked bar area I need to make sure the language column is a factor
 tidyhomelang$Language <- 
   factor(tidyhomelang$Language,
   levels = c("English", "French","Non-official"))
 
 #also need to make sure that the other values are numeric
 tidyhomelang$year <- as.numeric(tidyhomelang$year)
 tidyhomelang$Percentage <- as.numeric(tidyhomelang$Percentage)
 
 print(tidyhomelang)
 
#trying another type of visualization
 ggplot(tidyhomelang, aes(x= year, y=Percentage, fill = Language)) + 
   geom_area() 
 
 ggplot(tidyhomelang, aes(x = year, y = Percentage, fill = Language)) +
 geom_area() +
   scale_x_continuous(breaks = c(1996, 2001, 2006, 2016, 2021)) +
   labs(title = "Language most spoken at home",
        x = "Year",
        y = "Percentage",
        fill = "Language") +
   theme_minimal()
 

# going to try replotting by by reordering
 # Give a specific order:
tidyhomelang$Language <- 
  factor(tidyhomelang$Language,levels=c("English","French", "Non-official"))

#try replotting
ggplot(tidyhomelang, aes(x = year, y = Percentage, fill = Language)) +
  geom_area(position = "identity", alpha = 1) +
  scale_x_continuous(breaks = c(1996, 2001, 2006, 2016, 2021)) +
  labs(title = "Language most spoken at home",
       x = "Year",
       y = "Percentage",
       fill = "Language") +
  theme_minimal() 

ggplot(tidyhomelang, aes(x = year, y = Percentage, fill = Language)) +
  geom_area()+
  scale_x_continuous(breaks = c(1996, 2001, 2006, 2016, 2021)) +
  labs(title = "Language most spoken at home",
       x = "Year",
       y = "Percentage",
       fill = "Language") +
  theme_minimal() 


# checking values 
unique(tidyhomelang$Language)
unique(tidyhomelang$Percentage)
summary(tidyhomelang)


# All languages spoken at home --------------------------------------------
##get 2021 Census Data
View(list_census_vectors("CA21"))

# make a vector for all languages spoken at home in Laval 2021

allhomelangvector21 <- c(
  "total" = "v_CA21_2167",
  "english" = "v_CA21_2170",
  "french" = "v_CA21_2173",
  "non_off" = "v_CA21_2176",
  "indig" = "v_CA21_2179",
  "non_ind"  = "v_CA21_2182",
  "eng_fr" = "v_CA21_2185",
  "eng_nonoff" = "v_CA21_2188",
  "fr_nonoff" = "v_CA21_2191",
  "engfr_nonoff" = "v_CA21_2194",
  "mul_nonoff" = "v_CA21_2197") #multiple non-official

##get the data for Laval in 2021 (all languages used at home)
allhomelanglav_21 <- 
  get_census(dataset = "CA21",
             regions = list(CSD = 2465005),
             level = "CSD",
             vectors = allhomelangvector21)

# get the data for QC in 2021 (all languages used at work)
allhomelangqc_21 <- 
  get_census(dataset = "CA21",
             regions = list(PR = 24),
             level = "PR",
             vectors = allhomelangvector21)

#bind the 2 datasets together

allhomelang21 <- 
  bind_rows(
    allhomelanglav_21,
    allhomelangqc_21) |> 
  mutate(region = c("laval", "quebec"))

# reformat to combine and group language columns (English, French, and non-official)
com_allhomelang21 <- allhomelang21 |> 
  mutate(
    french_all = rowSums(across(c(french, fr_nonoff, eng_fr, engfr_nonoff))),
    english_all = rowSums(across(c(english, eng_nonoff, eng_fr, engfr_nonoff))),
    non_off_all = rowSums(across(c(non_off, eng_nonoff, fr_nonoff, engfr_nonoff, mul_nonoff)))
    )

#now i need a total column to be able to calculate percentages
com_allhomelang21 <- com_allhomelang21 |> 
  mutate(
    total_comb = rowSums(across(c(french_all, english_all, non_off_all)))
  )

# now we need to convert the totals to percentages
Perallhomelang21 <- com_allhomelang21 |> 
  group_by(region) |> 
  mutate(across(c(french_all, english_all, non_off_all), ~./ total_comb*100))

# format the table to plot it using pivot longer - tidy the dataframes of the percentages 
tidyallhomelang21 <- 
  pivot_longer(Perallhomelang21, cols = c(french_all:non_off_all),
               names_to = "language", values_to = "percentage")

# try plotting 
ggplot(tidyallhomelang21, aes(x = region, y = percentage, fill = language)) + 
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = paste0(round(percentage),"%")),
            position = position_stack(vjust = 0.5),
            size = 4,
            color = "white") +
  labs(y = "percentage")


# percentages are not showing up correctly so going to do some conversions 
prophomelang21 <- tidyallhomelang21 |> 
  mutate(percentage = percentage/ 100)

#rename column
prophomelang21 <-  prophomelang21 |> 
  rename(prop = c(percentage/ 100))

#retry plotting 
ggplot(prophomelang21, aes(x = region, y = percentage, fill = language)) + 
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = paste0(round(percentage*100),"%")),
            position = position_stack(vjust = 0.5),
            size = 4,
            color = "white") +
  labs(y = "percentage", title = "All languages spoken at Home 2021")

#replot to have the bars beside each other
ggplot(prophomelang21, aes(x = region, y = percentage, fill = language)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = paste0(round(percentage*100),"%")),
            position = position_dodge(vjust = 0.5),
            size = 4,
            color = "white") +
  labs(y = "percentage", title = "All languages spoken at Home 2021")

# I also want to see what the 10 most common non-official languages spoken at home
# this uses the single response, we need to build a vector for this

# 	v_CA21_2215 = total non-offical languages - Language spoken most often at home 

homelangparent21 <- find_census_vectors("Single Responses", "CA21") %>% 
  filter(vector == "v_CA21_2215")

#Select all leaf nodes of this vector. The parameter TRUE returns only the finite leaves among child nodes. 

homelang_children21 <- homelangparent21 %>%
  child_census_vectors(TRUE)

# Store the vector list for later
homelangvector21 <- homelang_children21 %>% pull(vector)
View(homelangvector21)

# now I will try to calculate the top 10 languages spoken at home in Laval
# grab census data for laval 2021
lavalhome21 <- get_census(dataset = "CA21",
                         regions = list(CSD= 2465005),
                         level = "CSD",
                         vectors = c(homelangvector21), 
                         geo_format = "sf", 
                         labels = "short")

# there are 169785 households in Laval 

#find the top 5 non-official languages will do this for the top 5 languages 
lavalhome21_tidy <- lavalhome21 |> 
  pivot_longer(cols = v_CA21_2371:v_CA21_2881, 
               names_to = "language", 
               values_to = "lang_count")

# get the top 5 languages
lang5_laval <- lavalhome21_tidy |> 
  slice_max(order_by = lang_count, n = 5)

# join with labels
lang5_laval <- lang5_laval |> 
  left_join(list_census_vectors(dataset = "CA21"),
            by = c("language" = "vector")) 

# test plot 
ggplot(lang5_laval, aes(x = reorder(label, -lang_count), y = lang_count, fill = label)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Top 5 Non-official Languages Spoken at Home in Laval",
       x = "Language",
       y = "Number of Speakers") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability

# for the report- helpful to see the number as proportion of households 
# # there are 169785 households in Laval 
# Arabic : 20220 / 169785 =  12 %
# Spanish : 9660 / 169785 = 5.6%
# Armenian : 7280 / 169785 = 4.2 
# Greek: 5460 / 169785 = 3.2
# Romanian: 4530 / 169785 = 2.6


 
 
 