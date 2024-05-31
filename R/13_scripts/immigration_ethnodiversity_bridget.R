##### Immigration and ethnocultural diversity #####

library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(gridExtra)

# Canadian Citizens -----------------------------------------------------

Citizenship <- cancensus::get_census(dataset = "CA21", 
                                       regions = list(CSD = 2465005), 
                                       level = "CSD",
                                       vectors = c("Total" = "v_CA21_4389",
                                                   "Canadian" = "v_CA21_4392",
                                                   "NotCanadian" = "v_CA21_4401"))

Citizenship <- Citizenship |> mutate(PercentCanadian = Canadian/Total)

# compare to Quebec 

Citizenship_Quebec <- cancensus::get_census(dataset = "CA21", 
                                            regions = list(PR = 24), 
                                            level = "PR",
                                            vectors = c("Total" = "v_CA21_4389",
                                                        "Canadian" = "v_CA21_4392",
                                                        "NotCanadian" = "v_CA21_4401"))

Citizenship_Quebec <- Citizenship_Quebec |> mutate(PercentCanadian = Canadian/Total)


# compare to mtl 

Citizenship_mtl <- cancensus::get_census(dataset = "CA21", 
                                     regions = list(CSD = 2466023), 
                                     level = "CSD",
                                     vectors = c("Total" = "v_CA21_4389",
                                                 "Canadian" = "v_CA21_4392",
                                                 "NotCanadian" = "v_CA21_4401"))
Citizenship_mtl <- Citizenship_mtl |> mutate(PercentCanadian = Canadian/Total)

#ethnic origin ----------------------------------------------

# first for the entire Laval population 
# data file is saved on my computer, comes from cancensus: https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810035801&pickMembers%5B0%5D=1.90&pickMembers%5B1%5D=2.1&pickMembers%5B2%5D=3.1&pickMembers%5B3%5D=4.1

ethnic_origin_Laval <- read_csv("/Users/bridgetbuglioni/Documents/GitHub/PESLaval/data/Ethnic Origin Total Pop V2.csv", skip = 12) |> 
  tibble::as_tibble()
names(ethnic_origin_Laval) <- c("ethnic_or_cultural", "total", "single", "multiple")

# some columns are NA
names(ethnic_origin_Laval)

# give them filler names so I can pivot 
colnames(ethnic_origin_Laval) <- ifelse(is.na(colnames(ethnic_origin_Laval)), paste0("V", seq_along(colnames(ethnic_origin_Laval))), colnames(ethnic_origin_Laval))

ethnic_origin_Laval <- ethnic_origin_Laval |> 
  select(-single:-V14)
  
# Remove commas and convert to numeric
ethnic_origin_Laval$total <- as.numeric(gsub(",", "", ethnic_origin_Laval$total))


ethnic_origin_Laval_long <- ethnic_origin_Laval |> 
  pivot_longer(cols = -ethnic_or_cultural)
 

 
# Subset rows 2 to 10
selected_rows <- ethnic_origin_Laval_long[3:10, ]

unique(selected_rows$value)



# Plot using ggplot2
ggplot(data = selected_rows, aes(x = ethnic_or_cultural, y = value)) +
  geom_col() +
  labs(title = "Ethnic or Cultural Origins of Laval Population")



# make my own df to plot as percents -- using 2021 census data counts 

ethnic_origin <- data.frame(
  `North American origins` = c(122805),
  `European origins` = c(159120),
  `Asian origins` = c(78415),
  `African origins` = c(46120),
  `CCSLA origins` = c(45320),
  `Oceanic and other` = c(31070),
  `Total` = c(429555))

ethnic_origin_percents <- ethnic_origin|> 
  mutate(across(`North.American.origins`:`Oceanic.and.other`, ~. /Total *100)) |> 
  select(-Total) 

ethnic_origin_long <- ethnic_origin_percents %>%
  pivot_longer(cols = everything(), names_to = "origin", values_to = "percent")

ggplot(ethnic_origin_long, aes(x = origin, y = percent)) +
  geom_col(fill = "skyblue") +
  labs(title = "Ethnic or Cultural Origins of Laval Population",
       x = "Origin",
       y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = round(percent, 2)), position = position_dodge(width = 0.9), vjust = -0.5)



#ethnic origin of immigrant pop (Place of Birth) -----------------------------------------------
# using 6 categories: North America (USA); 
                      #Europe; 
                      #Latin A, Central and South A, Caribbean, Mex; 
                      #Africa; 
                      #Asia; 
                      #Oceanian and Other

ethnic_origin_immigrants_categorised <- cancensus::get_census(dataset = "CA21", 
                                                  regions = list(CSD = 2465005), 
                                                  level = "CSD",
                                                  vectors = c("Total" = "v_CA21_4455",
                                                              "Brazil" = "v_CA21_4461",
                                                              "Colombia" = "v_CA21_4464",
                                                              "El Salvador" = "v_CA21_4467",
                                                              "Guyana" = "v_CA21_4470",
                                                              "Haiti" = "v_CA21_4473",
                                                              "Jamaica" = "v_CA21_4476",
                                                              "Mexico" = "v_CA21_4479",
                                                              "Peru" = "v_CA21_4482",
                                                              "Trinidad and Tobago" = "v_CA21_4485",
                                                              "Other in Americas" = "v_CA21_4491",
                                                              "USA" = "v_CA21_4488",
                                                              "Europe" = "v_CA21_4494",
                                                              "Africa" = "v_CA21_4545",
                                                              "Asia" = "v_CA21_4578",
                                                              "Oceania and Other" = "v_CA21_4632"))

#mutate categories to split North America from other Americas

ethnic_origin_immigrants_categorised <- ethnic_origin_immigrants_categorised |> 
  mutate(`North America` = USA) |> 
  mutate(`Caribbean, Central, South and Latin America` = Brazil + Mexico +
         + Colombia + `El Salvador` + Guyana + Haiti + Jamaica + Peru + `Trinidad and Tobago` +
           `Other in Americas`) |> 
  select(-c(USA, Mexico, Brazil, Colombia, `El Salvador`, Guyana, Haiti, Jamaica, Peru,
            `Trinidad and Tobago`, `Other in Americas`))


#pivot 

ethnic_origin_immigrants_categorised <- pivot_longer(ethnic_origin_immigrants_categorised, 
                                                          cols = -c(GeoUID:CMA_UID), 
                                                          names_to = "Category", values_to = "Count")

#plot

filtered_data <- ethnic_origin_immigrants_categorised |> 
  filter(Category != "Total") 
  
filtered_data |>   
  ggplot(aes(x = Category, y = Count))+
  geom_col() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Immigrant Population Place of Birth")


# view percentages

ethnic_origin_immigrants_categorised_percent <- ethnic_origin_immigrants_categorised |> 
  mutate(`perNA` = `North America`/Total * 100,
         perCCSLA = `Caribbean, Central, South and Latin America`/Total*100,
         perAsia = Asia/Total*100,
         perEurope = Europe/Total*100,
         perAfrica = Africa/Total*100,
         `perOceania and Other` = `Oceania and Other`/Total*100)


#ethnic origin of RECENT immigrant pop (Place of Birth) -----------------------------------------------
# using 6 categories: North America USA); 
#Europe; 
#Latin A, Central and South A, Caribbean, Mex; 
#Africa; 
#Asia; 
#Oceanian and Other

recentim_ethnic_origin <- cancensus::get_census(dataset = "CA21", 
                                                              regions = list(CSD = 2465005), 
                                                              level = "CSD",
                                                              vectors = c("Total" = "v_CA21_4635",
                                                                          "Brazil" = "v_CA21_4641",
                                                                          "Colombia" = "v_CA21_4644",
                                                                          "Haiti" = "v_CA21_4647",
                                                                          "Jamaica" = "v_CA21_4650",
                                                                          "Mexico" = "v_CA21_4653",
                                                                          "Venezuela" = "v_CA21_4659",
                                                                          "Other in Americas" = "v_CA21_4662",
                                                                          "USA" = "v_CA21_4656",
                                                                          "Europe" = "v_CA21_4665",
                                                                          "Africa" = "v_CA21_4692",
                                                                          "Asia" = "v_CA21_4740",
                                                                          "Oceania and Other" = "v_CA21_4809"))


#mutate categories to split North America from other Americas

recentim_ethnic_origin <- recentim_ethnic_origin |> 
  mutate(`North America` = USA) |> 
  mutate(`Caribbean, Central, South and Latin America` = Brazil + Mexico +
         + Colombia + Haiti + Jamaica + Venezuela +
           `Other in Americas`) |> 
  select(-c(USA, Mexico, Brazil, Colombia, Haiti, Jamaica, Venezuela,
             `Other in Americas`))

#pivot

recentim_ethnic_origin <- recentim_ethnic_origin |> 
  pivot_longer(-c(GeoUID:CMA_UID), 
               names_to = "Category", values_to = "Count")


filter_recentim_ethnic_origin <- recentim_ethnic_origin |> 
  filter(Category != "Total") 

filter_recentim_ethnic_origin |>   
  ggplot(aes(x = Category, y = Count))+
  geom_col() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Recent Immigrant Population Place of Birth")

# view percentages

recentim_ethnic_origin_percent <- recentim_ethnic_origin|> 
  mutate(`perNA` = `North America`/Total* 100,
         perCCSLA = `Caribbean, Central, South and Latin America`/Total*100,
         perAsia = Asia/Total*100,
         perEurope = Europe/Total*100,
         perAfrica = Africa/Total*100,
         `perOceania and Other` = `Oceania and Other`/Total*100)



# put the data together 
# make a plot of origins of recent immigrant vs total immigrants as percents 

# stop scientific notation
options(scipen = 999)

str(recentim_ethnic_origin_percent)
str(ethnic_origin_immigrants_categorised_percent)

# pivot data and add 'year'
recentim_ethnic_origin_percent <- recentim_ethnic_origin_percent |> 
  pivot_longer(-c(GeoUID:CMA_UID)) |> 
  mutate(Year = "Recent Immigrants")
  

ethnic_origin_immigrants_categorised_percent <- ethnic_origin_immigrants_categorised_percent |> 
  pivot_longer(-c(GeoUID:CMA_UID)) |> 
  mutate(Year = "All Immigrants")

# combine percentage plots together

combined_immigrantorigin_data <- bind_rows(recentim_ethnic_origin_percent, ethnic_origin_immigrants_categorised_percent)

# need to remove rows or select certain rows 

rows_to_plot <- combined_immigrantorigin_data %>%
  slice(c(8:13, 21:26))



ggplot(data = rows_to_plot, aes(x = name, y = value, fill = Year)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Origin", y = "Percent", title = "Recent vs Total Immigrant Population Origins") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("All Immigrants" = "skyblue", "Recent Immigrants" = "lightgreen")) +
  theme_minimal()







# immigration status of total population -------------------------------------
# immigrant and non-immgrant status - map by CT


immigrant_status <-  cancensus::get_census(dataset = "CA21", 
                                           regions = list(CSD = 2465005), 
                                           level = "CT",
                                           geo_format = "sf",
                                           vectors = c("Total" = "v_CA21_4404",
                                                       "Non Immigrants" = "v_CA21_4407",
                                                       "Immigrants" = "v_CA21_4410"))
                                                       

immigrant_status_aspercent <- immigrant_status |> 
  mutate(`Non Immigrant`= `Non Immigrants`/Total,
         Immigrant = Immigrants/Total)



ggplot(data = immigrant_status_aspercent) +
 geom_sf(aes(fill = Immigrant))  +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Percentage") 



# immigrant status for Laval as whole

immigrant_statusLaval <-  cancensus::get_census(dataset = "CA21", 
                                           regions = list(CSD = 2465005), 
                                           level = "CSD",
                                           geo_format = "sf",
                                           vectors = c("Total" = "v_CA21_4404",
                                                       "Non Immigrants" = "v_CA21_4407",
                                                       "Immigrants" = "v_CA21_4410",
                                                       "Non-Permanent Residents" = "v_CA21_4434"))

immigrant_statusLaval_aspercent <- immigrant_statusLaval |> 
  mutate(`Non Immigrant`= `Non Immigrants`/Total,
         Immigrant = Immigrants/Total,
         `Non-Permanent Residents` = `Non-Permanent Residents`/ Total)


# compare to Quebec (PR) and Montreal

immigrant_status_queb <-  cancensus::get_census(dataset = "CA21", 
                                           regions = list(PR = 24), 
                                           level = "PR",
                                           vectors = c("Total" = "v_CA21_4404",
                                                       "Non Immigrants" = "v_CA21_4407",
                                                       "Immigrants" = "v_CA21_4410"))
immigrant_status_queb <- immigrant_status_queb |> 
  mutate(`Non Immigrant`= `Non Immigrants`/Total,
         Immigrant = Immigrants/Total)


# immigration by Age Group -----------------------------------------------------

immigration_by_age_gender <- cancensus::get_census(dataset = "CA21", 
                                            regions = list(CSD = 2465005), 
                                            level = "CSD",
                                            vectors = c("Male Under 5" = "v_CA21_4441",
                                                        "Female Under 5" = "v_CA21_4442",
                                                        "Male 5 to 14" = "v_CA21_4444",
                                                        "Female 5 to 14" = "v_CA21_4445",
                                                        "Male 15 to 24" = "v_CA21_4447",
                                                        "Female 15 to 24" = "v_CA21_4448",
                                                        "Male 25 to 44" = "v_CA21_4450",
                                                        "Female 25 to 44" = "v_CA21_4451",
                                                        "Male 45 and over" = "v_CA21_4453",
                                                        "Female 45 and over" = "v_CA21_4454"))

# Calculate total population
immigration_by_age_gender$total_population <- rowSums(immigration_by_age_gender[, c("Male Under 5", "Female Under 5", 
                                                                                    "Male 5 to 14", "Female 5 to 14",
                                                                                    "Male 15 to 24", "Female 15 to 24",
                                                                                    "Male 25 to 44", "Female 25 to 44",
                                                                                    "Male 45 and over", "Female 45 and over")])

# Calculate percentage for each age-gender group
immigration_by_age_gender$Male_Under_5_Percent <- (immigration_by_age_gender$"Male Under 5" / immigration_by_age_gender$total_population) * 100
immigration_by_age_gender$Female_Under_5_Percent <- (immigration_by_age_gender$"Female Under 5" / immigration_by_age_gender$total_population) * 100
immigration_by_age_gender$Male_5_to_14_Percent <- (immigration_by_age_gender$"Male 5 to 14" / immigration_by_age_gender$total_population) * 100
immigration_by_age_gender$Female_5_to_14_Percent <- (immigration_by_age_gender$"Female 5 to 14" / immigration_by_age_gender$total_population) * 100
immigration_by_age_gender$Male_15_to_24_Percent <- (immigration_by_age_gender$"Male 15 to 24" / immigration_by_age_gender$total_population) * 100
immigration_by_age_gender$Female_15_to_24_Percent <- (immigration_by_age_gender$"Female 15 to 24" / immigration_by_age_gender$total_population) * 100
immigration_by_age_gender$Male_25_to_44_Percent <- (immigration_by_age_gender$"Male 25 to 44" / immigration_by_age_gender$total_population) * 100
immigration_by_age_gender$Female_25_to_44_Percent <- (immigration_by_age_gender$"Female 25 to 44" / immigration_by_age_gender$total_population) * 100
immigration_by_age_gender$Male_45_and_over_Percent <- (immigration_by_age_gender$"Male 45 and over" / immigration_by_age_gender$total_population) * 100
immigration_by_age_gender$Female_45_and_over_Percent <- (immigration_by_age_gender$"Female 45 and over" / immigration_by_age_gender$total_population) * 100


# pivot longer

immigration_by_age_gender <- 
  immigration_by_age_gender |> 
  mutate(Region = "Laval") |> 
  pivot_longer(-c(GeoUID:CMA_UID, Region)) |> 
  mutate(gender = str_extract(name, "(Male|Female)"),
         name = str_remove(name, "(Male |Female )"))

# so columns are in the correct order

immigration_by_age_gender$name <- 
  factor(immigration_by_age_gender$name, levels = unique(immigration_by_age_gender$name))



# slice if using percent data 
immigration_by_age_gender_plot <- immigration_by_age_gender |> 
  slice(c(12:21))

#plot 

immigration_by_age_gender_plot <-
immigration_by_age_gender_plot|> 
  ggplot(aes(x = name, y = value, fill = gender)) +  # Set fill to gender
  geom_col(position = "dodge") +  # Dodge bars for side-by-side visualization
  labs(x = "Age", y = "Percent", title = "Laval") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels by 45 degrees
  ) +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "pink"))+
  scale_y_continuous(labels = scales::comma) 





# compare to Quebec

immigration_by_age_gender_queb <- cancensus::get_census(dataset = "CA21", 
                                                   regions = list(PR = 24), 
                                                   level = "PR",
                                                   vectors = c("Male Under 5" = "v_CA21_4441",
                                                               "Female Under 5" = "v_CA21_4442",
                                                               "Male 5 to 14" = "v_CA21_4444",
                                                               "Female 5 to 14" = "v_CA21_4445",
                                                               "Male 15 to 24" = "v_CA21_4447",
                                                               "Female 15 to 24" = "v_CA21_4448",
                                                               "Male 25 to 44" = "v_CA21_4450",
                                                               "Female 25 to 44" = "v_CA21_4451",
                                                               "Male 45 and over" = "v_CA21_4453",
                                                               "Female 45 and over" = "v_CA21_4454"))

# Calculate total population
immigration_by_age_gender_queb$total_population <- rowSums(immigration_by_age_gender_queb[, c("Male Under 5", "Female Under 5", 
                                                                                              "Male 5 to 14", "Female 5 to 14",
                                                                                              "Male 15 to 24", "Female 15 to 24",
                                                                                              "Male 25 to 44", "Female 25 to 44",
                                                                                              "Male 45 and over", "Female 45 and over")])

# Calculate percentage for each age-gender group
immigration_by_age_gender_queb$Male_Under_5_Percent <- (immigration_by_age_gender_queb$"Male Under 5" / immigration_by_age_gender_queb$total_population) * 100
immigration_by_age_gender_queb$Female_Under_5_Percent <- (immigration_by_age_gender_queb$"Female Under 5" / immigration_by_age_gender_queb$total_population) * 100
immigration_by_age_gender_queb$Male_5_to_14_Percent <- (immigration_by_age_gender_queb$"Male 5 to 14" / immigration_by_age_gender_queb$total_population) * 100
immigration_by_age_gender_queb$Female_5_to_14_Percent <- (immigration_by_age_gender_queb$"Female 5 to 14" / immigration_by_age_gender_queb$total_population) * 100
immigration_by_age_gender_queb$Male_15_to_24_Percent <- (immigration_by_age_gender_queb$"Male 15 to 24" / immigration_by_age_gender_queb$total_population) * 100
immigration_by_age_gender_queb$Female_15_to_24_Percent <- (immigration_by_age_gender_queb$"Female 15 to 24" / immigration_by_age_gender_queb$total_population) * 100
immigration_by_age_gender_queb$Male_25_to_44_Percent <- (immigration_by_age_gender_queb$"Male 25 to 44" / immigration_by_age_gender_queb$total_population) * 100
immigration_by_age_gender_queb$Female_25_to_44_Percent <- (immigration_by_age_gender_queb$"Female 25 to 44" / immigration_by_age_gender_queb$total_population) * 100
immigration_by_age_gender_queb$Male_45_and_over_Percent <- (immigration_by_age_gender_queb$"Male 45 and over" / immigration_by_age_gender_queb$total_population) * 100
immigration_by_age_gender_queb$Female_45_and_over_Percent <- (immigration_by_age_gender_queb$"Female 45 and over" / immigration_by_age_gender_queb$total_population) * 100


# pivot longer

immigration_by_age_gender_queb <- 
  immigration_by_age_gender_queb |> 
  mutate(Region = "Quebec") |> 
  pivot_longer(-c(GeoUID:C_UID, Region)) |> 
  mutate(gender = str_extract(name, "(Male|Female)"),
         name = str_remove(name, "(Male |Female )")) 

# so columns are in the correct order

immigration_by_age_gender_queb$name <- 
  factor(immigration_by_age_gender_queb$name, levels = unique(immigration_by_age_gender_queb$name))


# slice if using percent data 
immigration_by_age_gender_queb_plot <- immigration_by_age_gender_queb |> 
  slice(c(12:21))


# plot
immigration_by_age_gender_queb_plot <-
immigration_by_age_gender_queb_plot|> 
  ggplot(aes(x = name, y = value, fill = gender)) +  # Set fill to gender
  geom_col(position = "dodge") +  # Dodge bars for side-by-side visualization
  labs(x = "Age", y = "Percent", title = "Quebec") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels by 45 degrees
  ) +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "pink"))+
  scale_y_continuous(labels = scales::comma) 


# combine qc and laval on one output
print(immigration_by_age_gender_plot)


grid.arrange(immigration_by_age_gender_plot, immigration_by_age_gender_queb_plot, ncol = 2) 







# Period of Immigration -----------------------------------------------

immigrant_decade <- cancensus::get_census(dataset = "CA21", 
                                              regions = list(CSD = 2465005), 
                                              level = "CSD",
                                              vectors = c("Total" = "v_CA21_4410",
                                                "Before 1980" = "v_CA21_4413",
                                                          "1980 to 1990" = "v_CA21_4416",
                                                          "1991 to 2000" = "v_CA21_4419",
                                                          "2001 to 2010" = "v_CA21_4422",
                                                          "2011 to 2021" = "v_CA21_4425"))
immigrant_decade_percent <- immigrant_decade |> 
  mutate(across(`Before 1980`:`2011 to 2021`, ~. /Total *100)) |> 
  select(-Total) 
  



immigrant_decade_percent <- immigrant_decade_percent |> 
  pivot_longer(-c(GeoUID:CMA_UID)) |> 
  mutate(Region = "Laval")


# put columns in correct order 

immigrant_decade_percent$name <- 
  factor(immigrant_decade_percent$name, levels = unique(immigrant_decade_percent$name))



#plot 

ggplot(data = immigrant_decade_percent, aes(x = name, y = value)) +
  geom_bar(stat = "identity") +
  labs(x = "Decade", y = "Percent", title = "Period of Immigration Laval")


# status by decade Quebec

immigrant_decade_queb <- cancensus::get_census(dataset = "CA21", 
                                               regions = list(PR = 24), 
                                               level = "PR",
                                               vectors = c("Total" = "v_CA21_4410",
                                                           "Before 1980" = "v_CA21_4413",
                                                           "1980 to 1990" = "v_CA21_4416",
                                                           "1991 to 2000" = "v_CA21_4419",
                                                           "2001 to 2010" = "v_CA21_4422",
                                                           "2011 to 2021" = "v_CA21_4425"))

immigrant_decade_queb_percent <- immigrant_decade_queb |> 
  mutate(across(`Before 1980`:`2011 to 2021`, ~. /Total *100)) |> 
  select(-Total)

immigrant_decade_queb_percent <- immigrant_decade_queb_percent |> 
  pivot_longer(-c(GeoUID:C_UID)) |> 
  mutate(Region = "Quebec")

immigrant_decade_queb_percent$name <- 
  factor(immigrant_decade_queb_percent$name, levels = unique(immigrant_decade_queb_percent$name))

ggplot(data = immigrant_decade_queb_percent, aes(x = name, y = value)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Decade", y = "Percent", title = "Period of Immigration Quebec")


# combine percentage plots together

combined_decade_data <- bind_rows(immigrant_decade_percent, immigrant_decade_queb_percent)

ggplot(data = combined_decade_data, aes(x = name, y = value, fill = Region)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Decade", y = "Percent", title = "Period of Immigration: Laval vs. Quebec") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()





# Generation -----------------------------------------------

immigrant_generation <- cancensus::get_census(dataset = "CA21", 
                                              regions = list(CSD = 2465005), 
                                              level = "CSD",
                                              vectors = c("First Generation" = "v_CA21_4821",
                                                          "Second Generation" = "v_CA21_4824",
                                                          "Third Generation or more" = "v_CA21_4827"))

immigrant_generation <- immigrant_generation |> 
  pivot_longer(-c(GeoUID:CMA_UID))


immigrant_generation$name <- 
  factor(immigrant_generation$name, levels = unique(immigrant_generation$name))

ggplot(data = immigrant_generation, aes(x = name, y = value)) +
  geom_bar(stat = "identity") +
  labs(title = "Laval", x = "Generation", y = "Count")


# as percentage

immigrant_generation_percent <- cancensus::get_census(dataset = "CA21", 
                                              regions = list(CSD = 2465005), 
                                              level = "CSD",
                                              vectors = c("Total" = "v_CA21_4818",
                                                          "First Generation" = "v_CA21_4821",
                                                          "Second Generation" = "v_CA21_4824",
                                                          "Third Generation or more" = "v_CA21_4827"))

immigrant_generation_percent <- immigrant_generation_percent |> 
  mutate(`First Generation` = `First Generation`/ Total,
         `Second Generation` = `Second Generation`/Total,
          `Third Generation or more` = `Third Generation or more`/Total)


# compared to Quebec

immigrant_generation_queb <- cancensus::get_census(dataset = "CA21", 
                                              regions = list(PR = 24), 
                                              level = "PR",
                                              vectors = c("Total" = "v_CA21_4818",
                                                          "First Generation" = "v_CA21_4821",
                                                          "Second Generation" = "v_CA21_4824",
                                                          "Third Generation or more" = "v_CA21_4827"))

immigrant_generation_queb <- immigrant_generation_queb |> 
  pivot_longer(-c(GeoUID:C_UID))


immigrant_generation_queb$name <- 
  factor(immigrant_generation_queb$name, levels = unique(immigrant_generation_queb$name))

ggplot(data = immigrant_generation_queb, aes(x = name, y = value)) +
  geom_bar(stat = "identity") +
  labs(title = "Quebec", x = "Generation", y = "Count")


immigrant_generation_queb <- immigrant_generation_queb |> 
  mutate(`First Generation` = `First Generation`/ Total,
         `Second Generation` = `Second Generation`/Total,
         `Third Generation or more` = `Third Generation or more`/Total)



#combine on one graph

immigrant_generation_percent <- mutate(immigrant_generation_percent, "Year" = 2021)

immigrant_generation_queb <- mutate(immigrant_generation_queb, "Year" = 2016)

combined_generation_data <- bind_rows(immigrant_generation_percent, immigrant_generation_queb)


### unable to pivot correctly 


#ggplot(combined_generation_data, aes(x = name, y = value, fill = as.factor(Year))) +
  #geom_bar(stat = "identity", position = "dodge") +
  #labs(x = "Category",
       y = "Value",
       fill = "Census Year")



# Immigrant Admission Category -------------------------------------------------

immigrant_admissioncat <- cancensus::get_census(dataset = "CA21", 
                                                regions = list(CSD = 2465005), 
                                                level = "CSD",
                                                vectors = c("Total" = "v_CA21_4830",
                                                            "Economic Immigrant" = "v_CA21_4833",
                                                            "Family Sponsored" = "v_CA21_4842",
                                                            "Refugees" = "v_CA21_4845",
                                                            "Other" = "v_CA21_4848"))

immigrant_admissioncat_percent <- immigrant_admissioncat |> 
  mutate(Economic = `Economic Immigrant`/ Total*100,
         Family = `Family Sponsored`/Total*100,
         Refugee = `Refugees`/Total*100,
         Other = Other/Total*100)

immigrant_admissioncat_percent <- immigrant_admissioncat_percent |> 
  pivot_longer(-c(GeoUID:CMA_UID))


immigrant_admissioncat_percent <- immigrant_admissioncat_percent |> 
  pivot_longer(-c(GeoUID:CMA_UID))
  
  
#slice data to just select percent columns
immigrant_admissioncat_percent_plot <- immigrant_admissioncat_percent  |> 
  slice(c(5:8))


immigrant_admissioncat_percent_plot|> 
  ggplot(aes(x = name, y = value)) +
  geom_col() +
  labs(x = "Admission Category", y = "Count")


# compare to quebec

immigrant_admissioncat_qc <- cancensus::get_census(dataset = "CA21", 
                                                regions = list(CSD = 24), 
                                                level = "PR",
                                                vectors = c("Total" = "v_CA21_4830",
                                                            "Economic Immigrant" = "v_CA21_4833",
                                                            "Family Sponsored" = "v_CA21_4842",
                                                            "Refugees" = "v_CA21_4845",
                                                            "Other" = "v_CA21_4848"))

immigrant_admissioncat_percent_qc <- immigrant_admissioncat_qc |> 
  mutate(Economic = `Economic Immigrant`/ Total*100,
         Family = `Family Sponsored`/Total*100,
         Refugee = `Refugees`/Total*100,
         Other = Other/Total*100)

immigrant_admissioncat_percent_qc <- immigrant_admissioncat_percent_qc |> 
  pivot_longer(-c(GeoUID:C_UID))



#slice data to just select percent columns
immigrant_admissioncat_percent_plot_qc <- immigrant_admissioncat_percent_qc  |> 
  slice(c(5:8))


immigrant_admissioncat_percent_plot_qc|> 
  ggplot(aes(x = name, y = value)) +
  geom_col() +
  labs(x = "Admission Category", y = "Count")

# combine with laval

admission_cat_combined <- bind_rows(immigrant_admissioncat_percent_plot, immigrant_admissioncat_percent_plot_qc)

ggplot(data = admission_cat_combined, aes(x = name, y = value, fill = `Region Name`)) +
  geom_col(position = "dodge") +
  labs(y = "Percent", x = "Admission Category", title = "Percentage of Immigrants by Admission Category")







#immigration admission category change over last 5 years 
library(cancensus)
View(list_census_vectors("CA16"))

immigrant_admissioncat_2016 <- cancensus::get_census(dataset = "CA16", 
                                                regions = list(CSD = 2465005), 
                                                level = "CSD",
                                                vectors = c("Total" = "v_CA16_3831",
                                                            "Economic Immigrant" = "v_CA16_3834",
                                                            "Family Sponsored" = "v_CA16_3843",
                                                            "Refugees" = "v_CA16_3846",
                                                            "Other" = "v_CA16_3849"))

immigrant_admissioncat_2016_percent <- immigrant_admissioncat_2016 |> 
  mutate(Economic = `Economic Immigrant`/ Total,
         Family = `Family Sponsored`/Total,
         Refugee = `Refugees`/Total)


immigrant_admissioncat_2016 <- immigrant_admissioncat_2016 |> 
  pivot_longer(-c(GeoUID:CMA_UID))

filtered_immigrant_admissioncat_2016 <- immigrant_admissioncat_2016 |> 
  filter(!(`name` %in% c("Total")))

filtered_immigrant_admissioncat_2016$name <- 
  factor(filtered_immigrant_admissioncat_2016$name, levels = unique(filtered_immigrant_admissioncat_2016$name))
                                                            
ggplot(data = filtered_immigrant_admissioncat_2016, aes(x = name, y = value)) +
  geom_bar(stat = "identity") +
  labs(x = "Admission Category", y = "Count")


## make a plot that shows changes between 2016 and 2021

filtered_immigrant_admissioncat <- mutate(filtered_immigrant_admissioncat, "Year" = 2021)

filtered_immigrant_admissioncat_2016 <- mutate(filtered_immigrant_admissioncat_2016, "Year" = 2016)

combined_data <- bind_rows(filtered_immigrant_admissioncat, filtered_immigrant_admissioncat_2016)

ggplot(combined_data, aes(x = name, y = value, fill = as.factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Category",
       y = "Value",
       fill = "Census Year")

# Pre Admission Experience ----------------------------------------------------

immigrant_experience <- cancensus::get_census(dataset = "CA21", 
                                                     regions = list(CSD = 2465005), 
                                                     level = "CSD",
                                                     vectors = c("Total" = "v_CA21_4851",
                                                                 "Asylum Claim" = "v_CA21_4854",
                                                                 "Work Permit" = "v_CA21_4857",
                                                                 "Study Permit" = "v_CA21_4860",
                                                                 "Work and Study Permit" = "v_CA21_4863",
                                                                 "Other Permit" = "v_CA21_4866",
                                                                 "No Experience" = "v_CA21_4869"))

immigrant_experience <- immigrant_experience |> 
  pivot_longer(-c(GeoUID:CMA_UID))



# view percentages
immigrant_experience <- immigrant_experience |> 
  mutate(`Asylum Claim` = `Asylum Claim`/ Total,
         `Work Permit` = `Work Permit`/Total,
         `Study Permit` = `Study Permit`/Total,
         `Work and Study Permit` = `Work and Study Permit`/Total,
         `Other Permit` = `Other Permit`/Total,
         `No Experience` = `No Experience` / Total)


immigrant_experience_subgrouped <- immigrant_experience |> 
  filter(`name` != "Total")

immigrant_experience_subgrouped <- immigrant_experience_subgrouped |> 
  mutate(subgroup = `name`)

immigrant_experience_subgrouped$subgroup <- factor(immigrant_experience_subgrouped$subgroup, levels = unique(immigrant_experience_subgrouped$subgroup))

immigrant_experience_subgrouped <- immigrant_experience_subgrouped |> 
  mutate(constant_value = 1)


# plot 

ggplot(data = immigrant_experience_subgrouped, aes(x = constant_value, y = value, fill = subgroup))+
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Immigrant Experience",
       x = NULL, y = "Value",
       fill = "Subgroup") +
  theme(axis.text.x = element_blank())


# compared to Quebec

immigrant_experience_queb <- cancensus::get_census(dataset = "CA21", 
                                                   regions = list(PR = 24), 
                                                   level = "PR",
                                                   vectors = c("Total" = "v_CA21_4851",
                                                               "Asylum Claim" = "v_CA21_4854",
                                                               "Work Permit" = "v_CA21_4857",
                                                               "Study Permit" = "v_CA21_4860",
                                                               "Work and Study Permit" = "v_CA21_4863",
                                                               "Other Permit" = "v_CA21_4866",
                                                               "No Experience" = "v_CA21_4869"))

# view percentages
immigrant_experience_queb <- immigrant_experience_queb |> 
  mutate(`Asylum Claim` = `Asylum Claim`/ Total,
         `Work Permit` = `Work Permit`/Total,
         `Study Permit` = `Study Permit`/Total,
         `Work and Study Permit` = `Work and Study Permit`/Total,
         `Other Permit` = `Other Permit`/Total,
         `No Experience` = `No Experience` / Total)



immigrant_experience_queb <- immigrant_experience_queb |> 
  pivot_longer(-c(GeoUID:C_UID))


immigrant_experience_queb_subgrouped <- immigrant_experience_queb |> 
  filter(`name` != "Total")

immigrant_experience_queb_subgrouped <- immigrant_experience_queb_subgrouped |> 
  mutate(subgroup = `name`)

immigrant_experience_queb_subgrouped$subgroup <- factor(immigrant_experience_queb_subgrouped$subgroup, levels = unique(immigrant_experience_queb_subgrouped$subgroup))

immigrant_experience_queb_subgrouped <- immigrant_experience_queb_subgrouped |> 
  mutate(constant_value = 1)


ggplot(data = immigrant_experience_queb_subgrouped, aes(x = constant_value, y = value, fill = subgroup))+
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Immigrant Experience Quebec",
       x = NULL, y = "Value",
       fill = "Subgroup") +
  theme(axis.text.x = element_blank())






# Religion ---------------------------------------------------------------------

religion <- cancensus::get_census(dataset = "CA21", 
                                  regions = list(CSD = 2465005), 
                                  level = "CSD",
                                  vectors = c("Total" = "v_CA21_5670",
                                              "Buddhist" = "v_CA21_5673",
                                              "Christian" = "v_CA21_5676",
                                              "Hindu" = "v_CA21_5724",
                                              "Jewish" = "v_CA21_5727",
                                              "Muslim" = "v_CA21_5730",
                                              "Sikh" = "v_CA21_5733",
                                              "Traditional (North American Indigenous) spirituality" = "v_CA21_5736",
                                              "Other religion" = "v_CA21_5739",
                                              "No Religion and Secular" = "v_CA21_5742"))



religion_percentages <- religion |> 
  mutate(`perBuddhist` = `Buddhist` / Total * 100,
         `perChristian` = `Christian` / Total * 100,
         `perHindu` = `Hindu` / Total * 100,
         `perJewish` = `Jewish` / Total * 100,
         `perMuslim` = `Muslim` / Total * 100,
         `perSikh` = `Sikh` / Total * 100,
        `perIndigenous` = `Traditional (North American Indigenous) spirituality` / Total * 100,
         `perOther` = `Other religion` / Total * 100,
         `perNoReligion` = `No Religion and Secular` / Total * 100)


religion_long <- religion_percentages %>%
  select(GeoUID, `Region Name`, perBuddhist, perChristian, perHindu, perJewish, perMuslim, perSikh, perIndigenous, perOther, perNoReligion) %>%
  pivot_longer(cols = starts_with("per"), names_to = "Religion", values_to = "Percentage")
  

ggplot(religion_long, aes(x = Religion, y = Percentage, fill = Religion)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Different Religions", x = "Religion", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = FALSE)

# Create the pie chart
ggplot(religion_long, aes(x = "", y = Percentage, fill = Religion)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Percentage of Different Religions") +
  theme_void() +  # Use a void theme to remove axes and background
  theme(legend.position = "right") +
  guides(fill = guide_legend(title = "Religion"))  +
  geom_text(aes(y = Percentage, label = sprintf("%.2f%%", Percentage)), color = "white")
  




# compare to Quebec

religion_queb <- cancensus::get_census(dataset = "CA21", 
                                  regions = list(PR = 24), 
                                  level = "PR",
                                  vectors = c("Total" = "v_CA21_5670",
                                              "Buddhist" = "v_CA21_5673",
                                              "Christian" = "v_CA21_5676",
                                              "Hindu" = "v_CA21_5724",
                                              "Jewish" = "v_CA21_5727",
                                              "Muslim" = "v_CA21_5730",
                                              "Sikh" = "v_CA21_5733",
                                              "Traditional (North American Indigenous) spirituality" = "v_CA21_5736",
                                              "Other religion" = "v_CA21_5739",
                                              "No Religion and Secular" = "v_CA21_5742"))

religion_queb_percentages <- religion_queb |> 
  mutate(`perBuddhist` = `Buddhist` / Total * 100,
         `perChristian` = `Christian` / Total * 100,
         `perHindu` = `Hindu` / Total * 100,
         `perJewish` = `Jewish` / Total * 100,
         `perMuslim` = `Muslim` / Total * 100,
         `perSikh` = `Sikh` / Total * 100,
         `perIndigenous` = `Traditional (North American Indigenous) spirituality` / Total * 100,
         `perOther` = `Other religion` / Total * 100,
         `perNoReligion` = `No Religion and Secular` / Total * 100)
  

religion_queb_long <- religion_queb_percentages %>%
  select(GeoUID, `Region Name`, perBuddhist, perChristian, perHindu, perJewish, perMuslim, perSikh, perIndigenous, perOther, perNoReligion) %>%
  pivot_longer(cols = starts_with("per"), names_to = "Religion", values_to = "Percentage")

ggplot(religion_queb_long, aes(x = "", y = Percentage, fill = Religion)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Percentage of Different Religions") +
  theme_void() +  # Use a void theme to remove axes and background
  theme(legend.position = "right") +
  guides(fill = guide_legend(title = "Religion"))





# Évolution des naissances -----------------------------------------------------
#Data - Birth by birthplace of Mother from: https://statistique.quebec.ca/en/document/births-administrative-regions/tableau/naissances-selon-le-lieu-de-naissance-de-la-mere-par-region-administrative-quebec#tri_tertr=13&tri_annee=2500

# import downloaded data set

births_df <- read.csv("/Users/bridgetbuglioni/Documents/GitHub/PESLaval/data/Births by birthplace of mother.csv", skip = 5) |> 
  tibble::as_tibble()

str(births_df)

births_df_rows_to_plot <- births_df %>%
  slice(c(1:17))


ggplot(births_df_rows_to_plot, aes(x = Année, y = perÉtranger)) +
  geom_point() +
  geom_line(aes(group=1))+
  labs(x = "Year", y = "Percent of Births from mothers born outside Canada", title = "Evolution of Births - Mother's birthplace outside Canada")

ggplot(births_df_rows_to_plot, aes(x = Année, y = perÉtranger)) +
  geom_col() +
  labs(x = "Year", y = "Percent of Births from mothers born outside Canada", title = "Evolution of Births - Mother's birthplace outside Canada") +
  geom_text(aes(label = round(perÉtranger, 2)), position = position_dodge(width = 0.9), vjust = -0.5)

