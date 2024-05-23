#Loading libraries
source("R/01_startup.R")

#Setting CensusMapper API Key because it's annoying and won't save
set_cancensus_api_key("CensusMapper_4308d496f011429cf814385050f083dc")

# Labour force data 2021 ---------------------------------------------------------
#Vectors for 2021 total population 15+ by labour force (lf) status, employed, and unemployed (25% sample data)
lf_vectors_21 <- c("lf_pop" = "v_CA21_6492", "tot_empl" = "v_CA21_6498", "tot_unempl" = "v_CA21_6501")

#Vector with the given CanCensus names from employ_rate_vectors
lf_names_21 <- c("lf_pop", "tot_empl", "tot_unempl")

#Grab employ_rate_vectors_21 data for Laval and cleans up the table
lf_laval_21  <- get_census(dataset = "CA21", 
                              regions = list(CSD = 2465005), 
                              level = "CSD",
                              vectors = lf_vectors_21) |> 
  select(all_of(lf_names_21)) |> 
  mutate(Geography = "Laval") |> 
  select(Geography, everything())

#Grab employ_rate_vectors_21 data for Montreal and cleans up the table
lf_mtl_21  <- get_census(dataset = "CA21", 
                                 regions = list(CSD = 2466023), 
                                 level = "CSD",
                                 vectors = lf_vectors_21) |> 
  select(all_of(lf_names_21)) |> 
  mutate(Geography = "Montreal") |> 
  select(Geography, everything())

#Grab employ_rate_vectors_21 data for the Montreal CMA and cleans up the table
lf_mtl_cma_21 <- get_census(dataset = "CA21", 
                                  regions = list(CMA = 24462), 
                                  level = "CMA",
                                  vectors = lf_vectors_21) |> 
  select(all_of(lf_names_21)) |> 
  mutate(Geography = "Montreal CMA") |> 
  select(Geography, everything())

#Grab employ_rate_vectors_21 data for the province of Quebec and cleans up the table
lf_qc_21  <- get_census(dataset = "CA21", 
                           regions = list(PR = 24), 
                           level = "PR",
                           vectors = lf_vectors_21) |> 
  select(all_of(lf_names_21)) |> 
  mutate(Geography = "Quebec") |> 
  select(Geography, everything())

#Combine 2021 data into one table
lf_combined_21 <- bind_rows(lf_laval_21, lf_mtl_21, lf_mtl_cma_21, lf_qc_21)

# Labour force data 2016 --------------------------------------------------
#Vectors for 2016 total population 15+ by labour force (lf) status, employed, and unemployed (25% sample data)
lf_vectors_16 <- c("empl_rate" = "v_CA16_5615",  "unempl_rate" = "v_CA16_5618")


lf_names_16 <- c("empl_rate", "unempl_rate")

#Grab employ_rate_vectors_16 data for Laval and cleans up the table
lf_laval_16  <- get_census(dataset = "CA16", 
                           regions = list(CSD = 2465005), 
                           level = "CSD",
                           vectors = lf_vectors_16) |> 
  select(all_of(lf_names_16)) |> 
  mutate(Geography = "Laval") |> 
  select(Geography, everything())

#Grab employ_rate_vectors_16 data for Montreal and cleans up the table
lf_mtl_16  <- get_census(dataset = "CA16", 
                         regions = list(CSD = 2466023), 
                         level = "CSD",
                         vectors = lf_vectors_16) |> 
  select(all_of(lf_names_16)) |> 
  mutate(Geography = "Montreal") |> 
  select(Geography, everything())

#Grab employ_rate_vectors_16 data for the Montreal CMA and cleans up the table
lf_mtl_cma_16 <- get_census(dataset = "CA16", 
                            regions = list(CMA = 24462), 
                            level = "CMA",
                            vectors = lf_vectors_16) |> 
  select(all_of(lf_names_16)) |> 
  mutate(Geography = "Montreal CMA") |> 
  select(Geography, everything())

#Grab employ_rate_vectors_16 data for the province of Quebec and cleans up the table
lf_qc_16  <- get_census(dataset = "CA16", 
                        regions = list(PR = 24), 
                        level = "PR",
                        vectors = lf_vectors_16) |> 
  select(all_of(lf_names_16)) |> 
  mutate(Geography = "Quebec") |> 
  select(Geography, everything())

#Combine 2016 data into one table
lf_combined_16 <- bind_rows(lf_laval_16, lf_mtl_16, lf_mtl_cma_16, lf_qc_16)
