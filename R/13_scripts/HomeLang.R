library(cancensus)
library(ggplot2)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)

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
 
 #trying again but adding percentages
 ggplot(tidyhomelang, aes(x=year, y=Percentage, fill= Language)) +
   geom_bar(stat="identity", position = "fill") +
   scale_y_continuous(labels = scales::percent) +
   geom_text(aes(label = paste0(round(Percentage),"%")),
             position = position_fill(vjust = 0.5),
             size = 4,
             color = "white") +
   labs(y = "Percentage")

#  geom_text(aes(label = Percentage,"%"), 
           position = position_stack(vjust = 0.5), size = 3)
 
#now compare Laval to Quebec in 2021 
#something crazy is happening with this code... 
 ggplot(tidyhomelang21, aes(x = region, y = Percentage, fill = Language)) + 
   geom_bar(stat = "identity", position = "fill") +
   scale_y_continuous(labels = scales::percent) +
   geom_text(aes(label = paste0(round(Percentage), "%")),
             position = position_fill(vjust = 0.5), 
             size = 3,
             color = "white") +
               labs(y="Percentage")

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
 
# Now I will compare Laval to Quebec for 2021, languages most spoken at home
# I also want to see what the 5 most common non-official languages spoken at home
 
# v_CA21_2200 = Total - Language spoken most often at home for the total population excluding institutional residents - 100% data
# (CA 2021 Census; 100% data; Language; Total - Language spoken most often at home 
 #for the total population excluding institutional residents - 100% data)
# v_CA21_2203 - single responses - Total
# v_CA21_2215 - Non-official languages (total)
 
 
 