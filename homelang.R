library(cancensus)
library(ggplot2)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)


# Languages spoken at Home ------------------------------------------------

# 1996
CT96 <- cancensus::get_census(dataset = "CA1996", 
                            regions = list(CSD = 2465005), 
                            level = "CT", 
                            geo_format = "sf"

View(list_census_vectors("CA1996"))

#Total population by home language 1996
# v_CA1996_322 - Total population by home language
# v_CA1996_323 - Single responses
# v_CA1996_324 - English
# v_CA1996_325 - French
# v_CA1996_326 -Non-official languages

#get all the variables for language spoken at home 1996
homelang96vector <- c(
  "Total" = "v_CA1996_322",
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
                            level = "CT", 
                            geo_format = "sf"
                            
View(list_census_vectors("CA01"))

homelang01vector <- c(
  "Total" = "v_CA01_226",
  "English" = "v_CA01_227",
  "French" = "v_CA01_228",
  "Non-official" = "v_CA01_229")
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
            homelang21_laval) #|> 
 # mutate(name = c("1996", "2001", "2006", "2016", "2021"), .before = GeoUID) |> 
  #pivot_longer(-name, names_to = "languages", values_to = "number") 
  
  #percentages <- (values / values["Total"]) * 100
  
