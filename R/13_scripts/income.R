#Loading libraries
source("R/01_startup.R")

#Setting CensusMapper API Key because it won't save
set_cancensus_api_key("CensusMapper_4308d496f011429cf814385050f083dc")

#Grabbing all cancensus vector
can21 <- list_census_vectors(dataset = "CA21")

# Personal Income Brackets ---------------------------------------------------------

indtotinc_vectors <- can21$vector[which(can21$vector == "v_CA21_665"):which(can21$vector == "v_CA21_712")]

indtoinc_names <- c("total_count", "wo_total_income")

inc21_vectors_total <- can21 |> 
  filter(vector %in% indtotinc_vectors & type == "Total") |> 
  pull(vector)

#Pull individual total income brackets for Laval
indtotinc_lvl_total  <- get_census(dataset = "CA21", 
                             regions = list(CSD = 2465005), 
                             level = "CSD",
                             vectors = inc21_vectors_total) |> 
  select(all_of(lf_names_21)) |> 
  mutate(Geography = c("Laval")) |> 
  select(Geography, everything())