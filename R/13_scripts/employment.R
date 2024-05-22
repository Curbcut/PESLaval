#Loading libraries
source("R/01_startup.R")

#Setting CensusMapper API Key because it's annoying and won't save
set_cancensus_api_key("CensusMapper_4308d496f011429cf814385050f083dc")

# Employment Rate ---------------------------------------------------------
#Vectors for total population 15+ by labor force (lf) status, employed, and unemployed (25% sample data) by total (tot), men (m), and women (f)
employ_rate_vectors <- c("lf_pop" = "v_CA21_6492", "lf_pop_m" = "v_CA21_6493", "lf_pop_f" =  "v_CA21_6494",
                         "tot_empl" = "v_CA21_6498", "empl_m" = "v_CA21_6499", "empl_f" = "v_CA21_6500",
                         "tot_unempl" = "v_CA21_6501", "unempl_m" = "v_CA21_6502", "unempl_f" = "v_CA21_6503")

#Vector with the given CanCensus names from employ_rate_vectors
vector_names <- c("lf_pop", "lf_pop_m", "lf_pop_f",
                  "tot_empl", "empl_m", "empl_f",
                  "tot_unempl", "unempl_m", "unempl_f")

#Grab employ_rate_vectors data for Laval and cleans up the table
employ_rate_laval  <- get_census(dataset = "CA21", 
                              regions = list(CSD = 2465005), 
                              level = "CSD",
                              vectors = employ_rate_vectors) |> 
  select(all_of(vector_names)) |> 
  mutate(Geography = "Laval") |> 
  select(Geography, everything())

#Grab employ_rate_vectors data for Montreal and cleans up the table
employ_rate_mtl  <- get_census(dataset = "CA21", 
                                 regions = list(CSD = 2466023), 
                                 level = "CSD",
                                 vectors = employ_rate_vectors) |> 
  select(all_of(vector_names)) |> 
  mutate(Geography = "Montreal") |> 
  select(Geography, everything())

#Grab employ_rate_vectors data for the Montreal CMA and cleans up the table
employ_rate_mtl_cma <- get_census(dataset = "CA21", 
                                  regions = list(CMA = 24462), 
                                  level = "CMA",
                                  vectors = employ_rate_vectors) |> 
  select(all_of(vector_names)) |> 
  mutate(Geography = "Montreal CMA") |> 
  select(Geography, everything())

#Grab employ_rate_vectors data for the province of Quebec and cleans up the table
employ_rate_qc  <- get_census(dataset = "CA21", 
                           regions = list(PR = 24), 
                           level = "PR",
                           vectors = employ_rate_vectors) |> 
  select(all_of(vector_names)) |> 
  mutate(Geography = "Quebec") |> 
  select(Geography, everything())
