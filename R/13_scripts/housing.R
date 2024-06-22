#Loading libraries
source("R/01_startup.R")
library(cmhc)

#Setting CensusMapper API Key because it won't save
set_cancensus_api_key("CensusMapper_4308d496f011429cf814385050f083dc")

#Caching census/CMHC data to reduce amount of calls and speed up process.
#Personal use only, change the folder to your own folder if you want to use it
set_cancensus_cache_path("D:/McGill/can_cache", install = TRUE, overwrite = TRUE)
set_cmhc_cache_path("D:/McGill/can_cache", install = TRUE, overwrite = TRUE)
cmhc::set_cmhc_cache_path("/Users/justin/Documents/R/CurbCutSelf", install = TRUE, overwrite = TRUE)

#See values of CMHC
cmhc_breakdown <- list_cmhc_breakdowns()

#Grabbing Laval's shapefile by census tract
laval_ct <- cancensus::get_census(dataset = "CA21", 
                                  regions = list(CSD = 2465005), 
                                  level = "CT", 
                                  geo_format = "sf")

#Using curbcut colors
curbcut_scale <- c("#C4CDE1", "#98A8CB", "#6C83B5", "#4C5C7F", "#2B3448")

# Annual Monthly Tenant Cost (Laval, Montreal, Quebec) ----------------------------
#Setting the years to pull data from
years <- 2010:2023

#Pulling average rent data
avg_rent_cmhc <- function(geoid, years, geoname) {
  map_dfr(years, function(cyear) {
    get_cmhc(survey = "Rms", series = "Average Rent", dimension = "Bedroom Type",
             breakdown = "Census Subdivision", geo_uid = geoid, year = cyear) |> 
      mutate(Geography = geoname) |> 
      filter(str_detect(`Bedroom Type`, "Total")) |> 
      select(Geography, Year, Value)
  })
}

#Grabbing annual average rent data from 2010 to 2023
avg_rent_lvl <- avg_rent_cmhc(2465005, years, "Laval")
avg_rent_mtl <- avg_rent_cmhc(2466023, years, "Montreal")

test <- get_cmhc(survey = "Rms", series = "Average Rent", dimension = "Bedroom Type",
                 breakdown = "Survey Zones", geo_uid = 2465005, year = 2022)

#Manually inputting the province of Quebec's data as it's unavailable using the CMHC package
#src = https://www.cmhc-schl.gc.ca/professionals/housing-markets-data-and-research/housing-data/data-tables/rental-market/rental-market-report-data-tables
avg_rent_qc <- data.frame(
  Geography = "Quebec",
  Year = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019,
           2020, 2021, 2022, 2023),
  Value = c(648, 666, 693, 679, 691, 712, 727, 736, 761, 800, 845, 874, 952, 1022)
)

#YoY growth calculation
avg_rent_yoy <- bind_rows(avg_rent_lvl, avg_rent_mtl, avg_rent_qc) |> 
  pivot_wider(id_cols = "Geography", names_from = Year, values_from = Value) |> 
  mutate(`2010_2011` = (`2011` / `2010` - 1) * 100,
         `2011_2012` = (`2012` / `2011` - 1) * 100,
         `2012_2013` = (`2013` / `2012` - 1) * 100,
         `2013_2014` = (`2014` / `2013` - 1) * 100,
         `2014_2015` = (`2015` / `2014` - 1) * 100,
         `2015_2016` = (`2016` / `2015` - 1) * 100,
         `2016_2017` = (`2017` / `2016` - 1) * 100,
         `2017_2018` = (`2018` / `2017` - 1) * 100,
         `2018_2019` = (`2019` / `2018` - 1) * 100,
         `2019_2020` = (`2020` / `2019` - 1) * 100,
         `2020_2021` = (`2021` / `2020` - 1) * 100,
         `2021_2022` = (`2022` / `2021` - 1) * 100,
         `2022_2023` = (`2023` / `2022` - 1) * 100) |> 
  select(Geography, `2010_2011`,`2011_2012`, `2012_2013`, `2013_2014`, `2014_2015`,
         `2015_2016`, `2016_2017`, `2017_2018`, `2018_2019`, `2019_2020`, `2020_2021`,
         `2021_2022`, `2022_2023`) |> 
  rename("2011" = `2010_2011`, "2012" = `2011_2012`, "2013" = `2012_2013`,
         "2014" = `2013_2014`, "2015" = `2014_2015`, "2016" = `2015_2016`,
         "2017" = `2016_2017`, "2018" = `2017_2018`, "2019" = `2018_2019`,
         "2020" = `2019_2020`, "2021" = `2020_2021`, "2022" = `2021_2022`,
         "2023" =  `2022_2023`) |> 
  pivot_longer(cols = -Geography, names_to = "Year", values_to = "Growth")

#Preparing the table for the line graph
avg_rent_annual <- bind_rows(avg_rent_lvl, avg_rent_mtl, avg_rent_qc) |> 
  mutate(Geography = factor(Geography, levels = c("Laval", "Montreal", "Quebec")))

#Line graph
ggplot(avg_rent_annual, aes(x = Year, y = `Value`, group = Geography, color = Geography)) +
  geom_line(linewidth = 1.25) +
  labs(title = "Loyer mensuel moyen 2010-2023",
       x = "Année",
       y = "Loyer mensuel moyen ($)") +
  scale_color_manual(values = c("Laval" = "royalblue2", "Montreal" = "indianred2",
                                "Quebec" = "gold3"),
                     labels = c("Laval", "Montreal", "Quebec")) +
  theme_minimal() +
  theme(
    legend.position = "bottom", legend.box = "horizontal",
    legend.title = element_blank(), plot.title = element_text(hjust = 0.5)
  )

ggplot(avg_rent_yoy, aes(x = Year, y = Growth, fill = Geography)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(title = "Variation d’une année sur l’autre du loyer moyen 2010-2023",
       x = "Année",
       y = "Variation du loyer moyen (%)") +
  theme(legend.position = "bottom", legend.box = "horizontal",
        legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
# Median Rent -------------------------------------------------------------
#Choosing years to pull data from
years <- 2010:2023

#Pulling median rent data
med_rent_cmhc <- function(geoid, years, geoname) {
  map_dfr(years, function(cyear) {
    get_cmhc(survey = "Rms", series = "Median Rent", dimension = "Bedroom Type",
             breakdown = "Census Subdivision", geo_uid = geoid, year = cyear) |> 
      mutate(Geography = geoname) |> 
      filter(str_detect(`Bedroom Type`, "Total")) |> 
      select(Geography, Year, Value)
  })
}

#Grabbing annual average rent data from 2010 to 2023
med_rent_lvl <- med_rent_cmhc(2465005, years, "Laval")
med_rent_mtl <- med_rent_cmhc(2466023, years, "Montreal")

#Manually inputting the province of Quebec's data as it's unavailable using the CMHC package
#src = https://www.cmhc-schl.gc.ca/professionals/housing-markets-data-and-research/housing-data/data-tables/rental-market/rental-market-report-data-tables
med_rent_qc <- data.frame(
  Geography = "Quebec",
  Year = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019,
           2020, 2021, 2022, 2023),
  Value = c(610, 629, 625, 645, 650, 665, 675, 693, 705, 745, 773, 800, 860, 939)
)

#YoY growth calculation
med_rent_yoy <- bind_rows(med_rent_lvl, med_rent_mtl, med_rent_qc) |> 
  pivot_wider(id_cols = "Geography", names_from = Year, values_from = Value) |> 
  mutate(`2010_2011` = (`2011` / `2010` - 1) * 100,
         `2011_2012` = (`2012` / `2011` - 1) * 100,
         `2012_2013` = (`2013` / `2012` - 1) * 100,
         `2013_2014` = (`2014` / `2013` - 1) * 100,
         `2014_2015` = (`2015` / `2014` - 1) * 100,
         `2015_2016` = (`2016` / `2015` - 1) * 100,
         `2016_2017` = (`2017` / `2016` - 1) * 100,
         `2017_2018` = (`2018` / `2017` - 1) * 100,
         `2018_2019` = (`2019` / `2018` - 1) * 100,
         `2019_2020` = (`2020` / `2019` - 1) * 100,
         `2020_2021` = (`2021` / `2020` - 1) * 100,
         `2021_2022` = (`2022` / `2021` - 1) * 100,
         `2022_2023` = (`2023` / `2022` - 1) * 100) |> 
  select(Geography, `2010_2011`,`2011_2012`, `2012_2013`, `2013_2014`, `2014_2015`,
         `2015_2016`, `2016_2017`, `2017_2018`, `2018_2019`, `2019_2020`, `2020_2021`,
         `2021_2022`, `2022_2023`) |> 
  rename("2011" = `2010_2011`, "2012" = `2011_2012`, "2013" = `2012_2013`,
         "2014" = `2013_2014`, "2015" = `2014_2015`, "2016" = `2015_2016`,
         "2017" = `2016_2017`, "2018" = `2017_2018`, "2019" = `2018_2019`,
         "2020" = `2019_2020`, "2021" = `2020_2021`, "2022" = `2021_2022`,
         "2023" =  `2022_2023`) |> 
  pivot_longer(cols = -Geography, names_to = "Year", values_to = "Growth")

#Preparing the table for the line graph
med_rent_annual <- bind_rows(med_rent_lvl, med_rent_mtl, med_rent_qc) |> 
  mutate(Geography = factor(Geography, levels = c("Laval", "Montreal", "Quebec")))

ggplot(med_rent_annual, aes(x = Year, y = `Value`, group = Geography, color = Geography)) +
  geom_line(linewidth = 1.25) +
  labs(title = "Loyer mensuel médian 2010-2023",
       x = "Année",
       y = "Loyer mensuel médian ($)") +
  scale_color_manual(values = c("Laval" = "royalblue2", "Montreal" = "indianred2",
                                "Quebec" = "gold3"),
                     labels = c("Laval", "Montreal", "Quebec")) +
  theme_minimal() +
  theme(
    legend.position = "bottom", legend.box = "horizontal",
    legend.title = element_blank(), plot.title = element_text(hjust = 0.5)
  )

ggplot(med_rent_yoy, aes(x = Year, y = Growth, fill = Geography)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(title = "Variation d’une année sur l’autre du loyer médian 2010-2023",
       x = "Année",
       y = "Variation du loyer médian (%)") +
  theme(legend.position = "bottom", legend.box = "horizontal",
        legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

# Monthly Tenant Cost by Laval Zone -----------------------------------------------------
# CMHC zones
zones <- cmhc::get_cmhc_geography(level = "NBHD")
zones <- sf::st_transform(zones, crs = 32618)

# Laval
laval <- cancensus::get_census("CA21", regions = list(CSD = 2465005), level = "CSD", geo_format = "sf")
laval <- sf::st_transform(laval, crs = 32618)

# Which CMHC zones are in Laval
laval_zones <- sf::st_filter(zones, laval)

#Setting the years to pull data
years <- 2010:2023

# Get the average rent for year 2023
avg_rent <- lapply(years, \(year) {
  avg_r <- cmhc::get_cmhc(survey = "Rms", series = "Average Rent", breakdown = "Neighbourhoods", 
                          dimension = "Bedroom Type", geo_uid = "24462", year = year) 
  # Only keep Laval's
  avg_r[avg_r$`Neighbourhoods` %in% laval_zones$ZONE_NAME_EN, ]
})

#Pulling average rent data for each zone and each bedroom type
avg_rent <- lapply(avg_rent, `[`, c("Neighbourhoods", "Bedroom Type", "Value"))
avg_rent <- mapply(\(year, table) {
  names(table)[3] <- paste0("avg_rent_", year)
  table
}, years, avg_rent, SIMPLIFY = FALSE)
avg_rent <- Reduce(merge, avg_rent)

#Calculating change in rent from 2018 to 2023
avg_rent5 <- avg_rent |> 
  filter(`Bedroom Type` == "Total") |> 
  mutate(year5 = (avg_rent_2023 / avg_rent_2018 - 1) * 100) |> 
  select(`Survey Zones`, `year5`) |> 
  rename("avg_rent_2023" = year5)

#Mapping the average rent by zone onto Laval
avg_rent[avg_rent$`Bedroom Type` == "Total", ] |> 
  merge(laval_zones[c("ZONE_NAME_EN")], by.x = "Neighbourhoods", by.y = "ZONE_NAME_EN") |> 
  sf::st_as_sf() |> 
  ggplot2::ggplot() +
  ggplot2::labs(title = "Loyer moyen à Laval 2023",
                fill = "Loyer moyen") +
  ggplot2::geom_sf(ggplot2::aes(fill = avg_rent_2023)) +
  ggplot2::theme_minimal() +
  ggplot2::scale_fill_gradientn(colors = curbcut_scale) +
  ggplot2::theme(axis.line = element_blank(), axis.text = element_blank(),
                       axis.title = element_blank(), axis.ticks = element_blank(),
                       panel.grid = element_blank(), legend.position = "bottom",
                       plot.title = element_text(hjust = 0.5))

#Plotting change in rent
avg_rent5 |> 
  merge(laval_zones[c("ZONE_NAME_EN")], by.x = "Survey Zones", by.y = "ZONE_NAME_EN") |> 
  sf::st_as_sf() |> 
  ggplot2::ggplot() +
  ggplot2::labs(title = "Variation mensuelle moyenne du loyer à Laval (2018-2023)",
                fill = "Variation du loyer (%)") +
  ggplot2::geom_sf(ggplot2::aes(fill = avg_rent_2023)) +
  ggplot2::theme_minimal() +
  ggplot2::scale_fill_gradientn(colors = curbcut_scale) +
  ggplot2::theme(axis.line = ggplot2::element_blank(), axis.text = ggplot2::element_blank(),
                 axis.title = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                 panel.grid = ggplot2::element_blank(), legend.position = "bottom",
                 plot.title = ggplot2::element_text(hjust = 0.5)) +
  ggplot2::guides(fill = ggplot2::guide_colorbar(title.position = "top", title.hjust = 0.5))

# YoY Growth for Rent -----------------------------------------------------
#Run this only after you run average and median rent above

#Calculating YoY change
avg_yoy <- avg_rent_annual |> 
  group_by(Geography) |> 
  arrange(Geography, Year) |> 
  mutate(PercentChange = (Value / lag(Value) - 1) * 100) |> 
  filter(Year != 2010) |> 
  select(-Value)

#Graphing YoY change for average monthly rent
ggplot(avg_yoy, aes(x = Year, y = `PercentChange`, group = Geography, color = Geography)) +
  geom_hline(yintercept = 0, size = 0.5, color = "black") +
  geom_line(size = 1.25) +
  labs(title = "Change in Average Monthly Rent 2011-2023",
       x = "Year",
       y = "Change in Average Rent (%)") +
  scale_color_manual(values = c("Laval" = "royalblue2", "Montreal" = "indianred2",
                                "Quebec" = "gold3"),
                     labels = c("Laval", "Montreal", "Quebec")) +
  scale_x_continuous(breaks = 2011:2023) +
  theme_minimal() +
  theme(
    legend.position = "bottom", legend.box = "horizontal",
    legend.title = element_blank(), plot.title = element_text(hjust = 0.5)
  )

#Calculating YoY change for median rent
med_yoy <- med_rent_annual |> 
  group_by(Geography) |> 
  arrange(Geography, Year) |> 
  mutate(PercentChange = (Value / lag(Value) - 1) * 100) |> 
  filter(Year != 2010) |> 
  select(-Value)

#Graphing median rent YoY Change
ggplot(med_yoy, aes(x = Year, y = `PercentChange`, group = Geography, color = Geography)) +
  geom_hline(yintercept = 0, size = 0.5, color = "black") +
  geom_line(size = 1.25) +
  labs(title = "Change in Median Monthly Rent 2011-2023",
       x = "Year",
       y = "Change in Median Rent (%)") +
  scale_color_manual(values = c("Laval" = "royalblue2", "Montreal" = "indianred2",
                                "Quebec" = "gold3"),
                     labels = c("Laval", "Montreal", "Quebec")) +
  scale_x_continuous(breaks = 2011:2023) +
  theme_minimal() +
  theme(
    legend.position = "bottom", legend.box = "horizontal",
    legend.title = element_blank(), plot.title = element_text(hjust = 0.5)
  )

#Census Monthly Shelter Cost ----------------------------------------------
#Grabbing monthly owner shelter cost vectors for 2006-2021
osc_21v <- c("med_owner" = "v_CA21_4309", "avg_owner" = "v_CA21_4310")
osc_16v <- c("med_owner" = "v_CA16_4893", "avg_owner" = "v_CA16_4894")
osc_11v <- c("med_owner" = "v_CA11N_2284", "avg_owner" = "v_CA11N_2285")

#Function to grab data for the above vectors for 2011-2021
osc_census <- function(region, geolevel, geoname, datayear, osc_year, cyear){
  regions_list <- list()
  regions_list[[geolevel]] <- region
  
  get_census(dataset = datayear,
             regions = regions_list,
             level = geolevel,
             vectors = osc_year
  ) |> 
    mutate(Geography = geoname, Year = cyear) |> 
    select(Geography, Year, med_owner, avg_owner)
}

#Same function as above but specific for 2006
osc_census06 <- function(region, geolevel, geoname){
  regions_list <- list()
  regions_list[[geolevel]] <- region
  
  get_census(dataset = "CA06",
             regions = regions_list,
             level = geolevel,
             vectors = c("avg_owner" = "v_CA06_2055", "avg_tenant" = "v_CA06_2050")
  ) |> 
    mutate(Geography = geoname, Year = "2006") |> 
    select(Geography, Year, avg_owner)
}

#Grabbing monthly shelter cost for 2021
osc_lvl_21 <- osc_census(2465005, "CSD", "Laval", "CA21", osc_21v, "2021")
osc_mtl_21 <- osc_census(2466023, "CSD", "Montreal", "CA21", osc_21v, "2021")
osc_qc_21 <- osc_census(24, "PR", "Quebec", "CA21", osc_21v, "2021")
osc_21 <- bind_rows(osc_lvl_21, osc_mtl_21, osc_qc_21)

#Grabbing monthly shelter cost for 2016
osc_lvl_16 <- osc_census(2465005, "CSD", "Laval", "CA16", osc_16v, "2016")
osc_mtl_16 <- osc_census(2466023, "CSD", "Montreal", "CA16", osc_16v, "2016")
osc_qc_16 <- osc_census(24, "PR", "Quebec", "CA16", osc_16v, "2016")
osc_16 <- bind_rows(osc_lvl_16, osc_mtl_16, osc_qc_16)

#Grabbing monthly shelter cost for 2011
osc_lvl_11 <- osc_census(2465005, "CSD", "Laval", "CA11", osc_11v, "2011")
osc_mtl_11 <- osc_census(2466023, "CSD", "Montreal", "CA11", osc_11v, "2011")
osc_qc_11 <- osc_census(24, "PR", "Quebec", "CA11", osc_11v, "2011")
osc_11 <- bind_rows(osc_lvl_11, osc_mtl_11, osc_qc_11)

#Grabbing monthly shelter cost for 2006
osc_lvl_06 <- osc_census06(2465005, "CSD", "Laval")
osc_mtl_06 <- osc_census06(2466023, "CSD", "Montreal")
osc_qc_06 <- osc_census06(24, "PR", "Quebec")
osc_06 <- bind_rows(osc_lvl_06, osc_mtl_06, osc_qc_06) |> 
  rename("owner" = avg_owner)

#Preparing the data for the median graph
osc_med_graph <- bind_rows(osc_21, osc_16, osc_11) |> 
  select(-avg_owner) |> 
  rename("owner" = med_owner) |> 
  pivot_longer(cols = c(owner),
               names_to = "Rent_Type",
               values_to = "Rent") |> 
  mutate(Geography = paste0(Geography, " (", Rent_Type, ")")) |> 
  select(-Rent_Type)

#Median line graph
ggplot(osc_med_graph, aes(x = Year, y = `Rent`, group = Geography, color = Geography)) +
  geom_line(linewidth = 1.25) +
  labs(title = "Coût de logement mensuel médian pour les ménages propriétaires 2011-2021",
       x = "Année",
       y = "Coût de logement mensuel médian ($)") +
  scale_color_manual(labels = c("Laval (owner)" = "Laval", 
                                "Montreal (owner)" = "Montreal",
                                "Quebec (owner)" = "Quebec"),
                     values = c("Laval (owner)" = "royalblue3", 
                                "Montreal (owner)" = "indianred3",
                                "Quebec (owner)" = "gold3")) +
  theme_minimal() +
  theme(
    legend.position = "bottom", legend.box = "horizontal",
    legend.title = element_blank(), plot.title = element_text(hjust = 0.5)
  )

#Preparing the data to create the average line graph
osc_avg_graph <- bind_rows(bind_rows(osc_21, osc_16, osc_11)) |> 
  select(-med_owner) |> 
  rename("owner" = avg_owner) |> 
  bind_rows(osc_06) |> 
  pivot_longer(cols = c(owner),
               names_to = "Rent_Type",
               values_to = "Rent") |> 
  mutate(Geography = paste0(Geography, " (", Rent_Type, ")")) |> 
  select(-Rent_Type)

#Line graph for average monthly shelter cost
ggplot(osc_avg_graph, aes(x = Year, y = `Rent`, group = Geography, color = Geography)) +
  geom_line(linewidth = 1.25) +
  labs(title = "Coût de logement mensuel moyen pour les ménages propriétaires 2006-2021",
       x = "Année",
       y = "Coût mensuel moyen du logement ($)") +
  scale_color_manual(labels = c("Laval (owner)" = "Laval", 
                                "Montreal (owner)" = "Montreal",
                                "Quebec (owner)" = "Quebec"),
                     values = c("Laval (owner)" = "royalblue3", 
                                "Montreal (owner)" = "indianred3",
                                "Quebec (owner)" = "gold3")) +
  theme_minimal() +
  theme(
    legend.position = "bottom", legend.box = "horizontal",
    legend.title = element_blank(), plot.title = element_text(hjust = 0.5)
  )

# Affordability 30% -------------------------------------------------------
#Grabbing vectors for total, owner, and tenants spending >30% on shelter costs for 2001-2021
aff_21v <- c("total" = "v_CA21_4288", "total_30" = "v_CA21_4290",
             "owner_30" = "v_CA21_4307", "tenant_30" = "v_CA21_4315")
aff_16v <- c("total" = "v_CA16_4886", "total_30" = "v_CA16_4888",
             "owner_30" = "v_CA16_4892", "tenant_30" = "v_CA16_4899")
aff_11v <- c("total" = "v_CA11N_2277", "total_30" = "v_CA11N_2279",
             "owner_30" = "v_CA11N_2283", "tenant_30" = "v_CA11N_2290")

#Creating a census grabber function for years 2011-2021 and cleans it up
aff_census_1121 <- function(dset, vectors, cyear){
  get_census(dataset = dset,
             regions = list(CSD = 2465005),
             level = "CSD",
             vectors = vectors) |> 
    mutate(Year = cyear,
           laval_30 = round(total_30 *100 / total, 1)) |> 
    select(Year, laval_30, owner_30, tenant_30)
}

#Grabbing data for 2011-2021
aff_21 <- aff_census_1121("CA21", aff_21v, "2021")
aff_16 <- aff_census_1121("CA16", aff_16v, "2016")
aff_11 <- aff_census_1121("CA11", aff_11v, "2011")

#Grabbing data and cleans it up for 2006
aff_06 <- get_census(dataset = "CA06",
                     regions = list(CSD = 2465005),
                     level = "CSD",
                     vectors = c("total" = "v_CA06_2048", "owner" = "v_CA06_2053", "owner_p30" = "v_CA06_2056",
                                 "tenant" = "v_CA06_2049", "tenant_p30" = "v_CA06_2051")) |> 
  mutate(Year = "2006",
         laval_30 = round((tenant_p30 + owner_p30) * 100 / total, 1),
         owner_30 = round(owner_p30 * 100 / owner, 1),
         tenant_30 = round(tenant_p30 * 100 / tenant, 1)) |> 
  select(Year, laval_30, owner_30, tenant_30)

#Grabbing data and cleans it up for 2001
aff_01 <- get_census(dataset = "CA01",
                     regions = list(CSD = 2465005),
                     level = "CSD",
                     vectors = c("owner" = "v_CA01_1670", "owner_p30" = "v_CA01_1672",
                                 "tenant" = "v_CA01_1666", "tenant_p30" = "v_CA01_1668")) |> 
  mutate(Year = "2001",
         laval_30 = round((tenant_p30 + owner_p30) * 100 / (owner + tenant), 1),
         owner_30 = round(owner_p30 * 100 / owner, 1),
         tenant_30 = round(tenant_p30 * 100 / tenant, 1)) |> 
  select(Year, laval_30, owner_30, tenant_30)

#Prepping data to create a line graph
aff_graph <- bind_rows(aff_21, aff_16, aff_11, aff_06, aff_01) |> 
  pivot_longer(cols = -Year, names_to = "hh_type", values_to = "proportion")

#Creating the line graph
ggplot(aff_graph, aes(x = Year, y = proportion, group = hh_type, color = hh_type)) +
  geom_line(linewidth = 1.25) +
  labs(title = "% de ménages consacrant plus de 30 % de leur revenu aux frais de logement (2001-2021)",
       x = "Année",
       y = "Proportion de ménages (%)") +
  scale_y_continuous(limits = c(0, 40),
                     breaks = seq(0, 40, by = 10)) +
  scale_color_manual(values = c("royalblue3", "indianred2", "gold3"),
                     labels = c("Tous les ménages", "Ménages propriétaires", "Ménages locataires")) +
  theme_minimal() +
  theme(
    legend.position = "bottom", legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5), legend.title = element_blank()
  )
# Core Housing Need --------------------------------------------
#Grab core housing need percentage for Laval in 2021
chn_lvl_21year <- get_census(dataset = "CA21", 
                             regions = list(CSD = 2465005), 
                             level = "CSD",
                             vectors = c("Total" = "v_CA21_4302", "Core" = "v_CA21_4303")) |> 
  select(all_of(c("Total", "Core"))) |> 
  mutate(`% Core Need` = round((Core*100) / Total, 1),
         "Year" = "2021") |> 
  select(Year, `% Core Need`)

#Grab core housing need for Laval from 2006-2016 and filter out the data and then
#binding it with chn_lvl_21year
chn_lvl <- get_cmhc("Core Housing Need", "Housing Standards",
                    "% of Households in Core Housing Need", "Historical Time Periods",
                    geoFilter = "Default", geo_uid = "2465005") |> 
  filter(`% of Households in Core Housing Need` == "Total") |> 
  select(DateString, Value) |> 
  rename("Year" = "DateString",
         "% Core Need" = "Value") |> 
  bind_rows(chn_lvl_21year)

#Creating a line graph for chn_lvl
ggplot(chn_lvl, aes(x = Year, y = `% Core Need`, group = "")) +
  geom_line(linewidth = 1.25) +
  labs(title = "Pourcentage de ménages ayant des besoins impérieux en matière de logement 2006-2021",
       x = "Année",
       y = "Ménages ayant des besoins impérieux en matière de logement (%)") +
  scale_y_continuous(limits = c(0, 20),
                     breaks = seq(0, 20, by = 10)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Function to grab census data for core housing need
chn_census_grabber <- function(cyear, region, geolevel, filter_v, geography){
  regions_list <- list()
  regions_list[[geolevel]] <- region
  
  get_census(dataset = cyear,
             regions = regions_list,
             level = geolevel,
             vectors = c("v_CA21_4302", "v_CA21_4304", "v_CA21_4308",
                         "v_CA21_4316", "v_CA21_4305", "v_CA21_4313")
  ) |> 
    select(-filter_v)
}

#Giving each column easier names to refer back to
bar_graph_names <- c("total", "no_chn","owner_chn", "tenant_chn", "owner_total", "tenant_total")

#Grabbing core housing need data for 2021 for each of the geographies from the
#census and calculating the proportion of those in core need
chn_lvl21 <- chn_census_grabber("CA21","2465005", "CSD", (1:10), "Laval") |> 
  setNames(bar_graph_names) |> 
  mutate("Geography" = "Laval",
         "No Core Need" = no_chn / total,
         "Owner Core Need" = (owner_total * owner_chn) / total,
         "Tenant Core Need" = (tenant_total * tenant_chn) / total) |> 
  select("Geography", "No Core Need", "Owner Core Need", "Tenant Core Need")
chn_mtl21 <- chn_census_grabber("CA21","2466023", "CSD", (1:10), "Montreal CMA") |> 
  setNames(bar_graph_names) |> 
  mutate("Geography" = "Montreal",
         "No Core Need" = no_chn / total,
         "Owner Core Need" = (owner_total * owner_chn) / total,
         "Tenant Core Need" = (tenant_total * tenant_chn) / total) |> 
  select("Geography", "No Core Need", "Owner Core Need", "Tenant Core Need")
chn_qc21 <- chn_census_grabber("CA21","24", "PR", (1:8), "Quebec") |> 
  setNames(bar_graph_names) |> 
  mutate("Geography" = "Quebec",
         "No Core Need" = no_chn / total,
         "Owner Core Need" = (owner_total * owner_chn) / total,
         "Tenant Core Need" = (tenant_total * tenant_chn) / total) |> 
  select("Geography", "No Core Need", "Owner Core Need", "Tenant Core Need")

#Preparing the data to create a grouped bar graph
chn <- bind_rows(chn_lvl21, chn_mtl21, chn_qc21) |> 
  pivot_longer(cols = -Geography, names_to = "Need", values_to = "Percentage") |> 
  filter(Need != "No Core Need") |> 
  mutate(Need = factor(Need, levels = c("Owner Core Need", "Tenant Core Need")))

#Creating the grouped bar graph
ggplot(chn, aes(x = Geography, y = Percentage, fill = Need)) +
  geom_bar(stat = "identity") +
  labs(title = "Core Housing Need 2021",
       x = "Geography",
       y = "Households in Core Need (%)") +
  scale_fill_manual(values = c("gold2", "indianred3", "dodgerblue3")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.title = element_blank())

# Core Housing Need using the Municipality QoL Dashboard ------------------
#Manually inputting data from Statistics Canada Municipality Quality of Life
#Dashboard
#src = https://www150.statcan.gc.ca/n1/pub/71-607-x/71-607-x2023025-eng.htm
chn_qol <- data.frame(
  Geography = c("Laval", "Montreal", "Quebec"),
  Overall = as.numeric(c("6.4", "10.5", "6.0")),
  Owner = as.numeric(c("2.2", "3.5", "2.1")),
  Tenant = as.numeric(c("14.8", "14.6", "11.9")),
  Subsidized = as.numeric(c("21.5", "18.5", "15.9"))) |> 
  pivot_longer(cols = -Geography, names_to = "Type", values_to = "Proportion") |> 
  mutate(Type = factor(Type, levels = c("Overall", "Owner", "Tenant", "Subsidized")))

#Creating the plot showing proportions of core housing need
ggplot(chn_qol, aes(x = Geography, y = Proportion, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Besoins impérieux de logement par type de ménage 2021",
       x = "",
       y = "Proportion de ménages (%)",
       fill = "Type of Household") +
  scale_fill_manual(values = c("Overall" = "royalblue2", "Owner" = "indianred2",
                               "Tenant" = "gold3", "Subsidized" = "chartreuse3"),
                    labels = c("Overall" = "Dans l'ensemble", "Owner" = "Propriétaire",
                               "Tenant" = "Locataire", "Subsidized" = "Subventionné")) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

# Proportion of Tenants and Owners ---------------------------------------------
pto_21v <- c("total" = "v_CA21_4288", "owner" = "v_CA21_4305", "tenant" = "v_CA21_4313")
pto_16v <- c("total" = "v_CA16_4886", "owner" = "v_CA16_4890", "tenant" = "v_CA16_4897")
pto_11v <- c("total" = "v_CA11N_2277", "owner" = "v_CA11N_2281", "tenant" = "v_CA11N_2288")
pto_06v <- c("total" = "v_CA06_2048", "owner" = "v_CA06_2053", "tenant" = "v_CA06_2049")
pto_01v <- c("owner" = "v_CA01_1670", "tenant" = "v_CA01_1666")

#Census Grabbing Function for Laval
pto_census <- function(datayear, pto_year, cyear){
  get_census(dataset = datayear,
             regions = list(CSD = 2465005),
             level = "CSD",
             vectors = pto_year
  ) |> 
    mutate(Year = cyear) |> 
    select(-GeoUID, -Type, -`Region Name`, -`Area (sq km)`, -Population,
           -Dwellings, -Households, -CD_UID, -PR_UID, -CMA_UID)
}

#Census Grabbing Function for Quebec for Comparison
pto_census_QC <- function(datayear, pto_year, cyear){
  get_census(dataset = datayear,
             regions = list(PR = 24),
             level = "PR",
             vectors = pto_year
  ) |> 
    mutate(Year = cyear) |> 
    select(-GeoUID, -Type, -`Region Name`, -`Area (sq km)`, -Population,
           -Dwellings, -Households, -C_UID)
}

#Census Grabbing Function for Canada for Comparison
pto_census_CA <- function(datayear, pto_year, cyear){
  get_census(dataset = datayear,
             regions = list(C = 01),
             level = "C",
             vectors = pto_year
  ) |> 
    mutate(Year = cyear) |> 
    select(-GeoUID, -Type, -`Region Name`, -`Area (sq km)`, -Population,
           -Dwellings, -Households)
}

#Grabbing data for 2001-2021 for Laval
pto_21 <- pto_census("CA21", pto_21v, "2021")
pto_16 <- pto_census("CA16", pto_16v, "2016")
pto_11 <- pto_census("CA11", pto_11v, "2011") |> 
  select(-`NHS Non Return Rate`)
pto_06 <- pto_census("CA06", pto_06v, "2006")
pto_01 <- pto_census("CA01", pto_01v, "2001") |> 
  mutate(total = owner + tenant)

#Grabbing data for 2001-2021 for Quebec and Canada
pto_21_QC <- pto_census_QC("CA21", pto_21v, "2021")
pto_16_QC <- pto_census_QC("CA16", pto_16v, "2016")
pto_11_QC <- pto_census_QC("CA11", pto_11v, "2011") |> 
  select(-`NHS Non Return Rate`)
pto_06_QC <- pto_census_QC("CA06", pto_06v, "2006")
pto_01_QC <- pto_census_QC("CA01", pto_01v, "2001") |> 
  mutate(total = owner + tenant)
pto_graph_QC <- bind_rows(pto_21_QC, pto_16_QC, pto_11_QC, pto_06_QC, pto_01_QC) |> 
  mutate("Owner Households" = owner * 100 / total,
         "Tenant Households" = tenant * 100 / total)

pto_21_CA <- pto_census_CA("CA21", pto_21v, "2021")
pto_16_CA <- pto_census_CA("CA16", pto_16v, "2016")
pto_11_CA <- pto_census_CA("CA11", pto_11v, "2011") |> 
  select(-`NHS Non Return Rate`)
pto_06_CA <- pto_census_CA("CA06", pto_06v, "2006")
pto_01_CA <- pto_census_CA("CA01", pto_01v, "2001") |> 
  mutate(total = owner + tenant)
pto_graph_CA <- bind_rows(pto_21_CA, pto_16_CA, pto_11_CA, pto_06_CA, pto_01_CA) |> 
  mutate("Owner Households" = owner * 100 / total,
         "Tenant Households" = tenant * 100 / total)

#Grabbing Montreal and Quebec for comparison
pto_mtl_21 <- get_census(dataset = "CA21",
                         regions = list(CSD = 2466023),
                         level = "CSD",
                         vectors = pto_21v) |> 
  mutate(rent = tenant * 100 / total, own = owner * 100 / total,
         Geography = "Montreal") |> 
  select(Geography, rent, own)
pto_qc_21 <- get_census(dataset = "CA21",
                         regions = list(PR = 24),
                         level = "PR",
                         vectors = pto_21v) |> 
  mutate(rent = tenant * 100 / total, own = owner * 100 / total,
         Geography = "Quebec") |> 
  select(Geography, rent, own)

#Making the data usable to graph
pto_graph <- bind_rows(pto_21, pto_16, pto_11, pto_06, pto_01) |> 
  pivot_longer(cols = -Year, names_to = "Type", values_to = "Households") |> 
  mutate(Type = factor(Type, levels = c("total", "owner", "tenant")))

#Proportion Graph
pto_graph_prop <- bind_rows(pto_21, pto_16, pto_11, pto_06, pto_01) |> 
  mutate("Owner Households" = owner * 100 / total,
         "Tenant Households" = tenant * 100 / total) |> 
  select(Year, "Owner Households", "Tenant Households") |> 
  pivot_longer(cols = -Year, names_to = "Type", values_to = "Households") |> 
  mutate(Type = factor(Type, levels = c("Owner Households", "Tenant Households")))

#Graphing the data out
ggplot(pto_graph, aes(x = Year, y = Households, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Total, ménages propriétaires et locataires à Laval 2001-2021",
       x = "Année",
       y = "Nombre de ménages",
       fill = "Type of Household") +
  scale_fill_manual(values = c("total" = "royalblue2", "owner" = "indianred2",
                               "tenant" = "gold3"),
                    labels = c("Total", "Propriétaire", "Locataire")) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal",
        legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

#Creating a line chart of proportion over years
ggplot(pto_graph_prop, aes(x = Year, y = Households, color = Type, group = Type)) +
  geom_line(linewidth = 1.5) +
  labs(title = "Proportion de ménages propriétaires et locataires à Laval 2001-2021",
       x = "Année",
       y = "Proportion de ménages (%)") +
  scale_color_manual(values = c("Owner Households" = "royalblue2", "Tenant Households" = "indianred2"),
                     labels = c("Ménages propriétaires", "Ménages locataire")) +
  theme_minimal() +
  theme(
    legend.position = "bottom", legend.box = "horizontal",
    legend.title = element_blank(), plot.title = element_text(hjust = 0.5)
  )

# Suitable Housing --------------------------------------------------------
#Grabbing data from the municipial qol dashboard
sh_lvl <- data.frame(
  "Geography" = "Laval",
  "Total" = 26.1,
  "Owner" = 19,
  "Tenant" = 40.1
)
sh_mtl <- data.frame(
  "Geography" = "Montreal",
  "Total" = 36.9,
  "Owner" = 27.1,
  "Tenant" = 42.6
)
sh_qc <- data.frame(
  "Geography" = "Quebec",
  "Total" = 24,
  "Owner" = 16.2,
  "Tenant" = 35.7
)

#Binding the rows together
sh <- bind_rows(sh_lvl, sh_mtl, sh_qc) %>%
  pivot_longer(cols = -Geography, values_to = "proportion", names_to = "Type") |> 
  mutate(Type = factor(Type, levels = c("Total", "Owner", "Tenant")))

#Creating a bar graph of suitable housing
ggplot(sh, aes(x = Geography, y = proportion, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Logement de taille inadaptée 2021",
       x = "",
       y = "Proportion de ménages (%)",
       fill = "Type of Household") +
  scale_fill_manual(values = c("Total" = "royalblue2", "Owner" = "indianred2",
                               "Tenant" = "gold3"),
                    labels = c("Total", "Propriétaire", "Locataire")) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal",
        legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
# Housing Starts ----------------------------------------------------------

#Grabbing and manipulating the data to be easier to use and graph
starts_lvl <- get_cmhc(survey = "Scss", series = "Starts", dimension = "Intended Market",
         breakdown = "Historical Time Periods", geo_uid = 2465005, year = 2009) |> 
  mutate(Date = dmy(paste0("01 ", DateString)), Year = as.factor(year(Date))) |> 
  select(Year, `Intended Market`, Value) |> 
  group_by(Year, `Intended Market`) |> 
  summarize(Units = sum(Value), .groups = "drop") |> 
  filter(`Intended Market` != "Unknown", `Intended Market` != "Co-Op",
         Year != "2024", Year != "2009") |> 
  mutate(`Intended Market` = factor(`Intended Market`, levels = c("All", "Homeowner", "Rental",
                                                                  "Condo")))
starts_lvl_line <- starts_lvl |> 
  filter(`Intended Market` != "All") |> 
  mutate(`Intended Market` = gsub("Homeowner", "Homeowner Starts", `Intended Market`)) |> 
  mutate(`Intended Market` = gsub("Rental", "Rental Starts", `Intended Market`)) |> 
  mutate(`Intended Market` = gsub("Condo", "Condo Starts", `Intended Market`))

#Graphing the data
ggplot(starts_lvl, aes(x = Year, y = Units, fill = `Intended Market`)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Mises en chantier d’unités d’habitation à Laval 2010-2023",
       x = "Année",
       y = "Nombre de mises en chantier") +
  scale_fill_manual(values = c("All" = "indianred2", "Homeowner" = "chartreuse3",
                               "Rental" = "royalblue3", "Condo" = "gold3"),
                    labels = c("Total", "Propriétaire", "De location", "Copropriété")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank()
  )

#Calculating the proportion of housing starts
startsp_lvl <- starts_lvl |> 
  pivot_wider(names_from = `Intended Market`, values_from = Units) |> 
  mutate(`Rental Starts` = Rental * 100 / All,
         `Owner Starts` = (Homeowner + Condo) * 100 / All) |> 
  select(-All, -Condo, -Homeowner, -Rental) |> 
  pivot_longer(cols = -Year, names_to = "Type", values_to = "Count")

#Graphing the proportion of housing starts
ggplot(startsp_lvl, aes(x = Year, y = `Count`, group = Type, color = Type)) +
  geom_line(size = 1.25) +
  labs(title = "Proportion des mises en chantier 2010-2023",
       x = "Année",
       y = "Proportion de types de mises en chantier (%)") +
  scale_color_manual(values = c("Owner Starts" = "indianred2", "Rental Starts" = "chartreuse3"),
                    labels = c("Démarrage par le propriétaire", "Débuts de location")) +
  theme_minimal() +
  theme(
    legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(starts_lvl_line, aes(x = Year, y = `Units`, group = `Intended Market`, color = `Intended Market`)) +
  geom_line(size = 1.25) +
  labs(title = "Mises en chantier à Laval 2010-2023",
       x = "Année",
       y = "Nombre de logements") +
  scale_color_manual(values = c("Homeowner Starts" = "#333366",
                                "Condo Starts" = "#cd5c5c",
                                "Rental Starts" = "#478547"),
                     labels = c("Démarrage par le propriétaire",
                                "Mises en chantier de condos",
                                "Débuts de location")) +
  theme_minimal() +
  theme(
    legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1)
  )
# Housing Completions -----------------------------------------------------
completions_lvl <- get_cmhc(survey = "Scss", series = "Completions", dimension = "Intended Market",
                       breakdown = "Historical Time Periods", geo_uid = 2465005, year = 2009) |> 
  mutate(Date = dmy(paste0("01 ", DateString)), Year = as.factor(year(Date))) |> 
  select(Year, `Intended Market`, Value) |> 
  group_by(Year, `Intended Market`) |> 
  summarize(Units = sum(Value), .groups = "drop") |> 
  filter(`Intended Market` != "Unknown", `Intended Market` != "Co-Op",
         Year != "2024", Year != "2009") |> 
  mutate(`Intended Market` = factor(`Intended Market`, levels = c("All", "Homeowner", "Rental",
                                                                  "Condo")))

completions_lvl_line <- completions_lvl |> 
  filter(`Intended Market` != "All") |> 
  mutate(`Intended Market` = gsub("Homeowner", "Homeowner Completions", `Intended Market`)) |> 
  mutate(`Intended Market` = gsub("Rental", "Rental Completions", `Intended Market`)) |> 
  mutate(`Intended Market` = gsub("Condo", "Condo Completions", `Intended Market`))

starts_completions <- bind_rows(starts_lvl_line, completions_lvl_line)

ggplot(starts_completions, aes(x = Year, y = `Units`, group = `Intended Market`, color = `Intended Market`)) +
  geom_line(size = 1.25) +
  labs(title = "Mises en chantier et achèvements d'habitations à Laval 2010-2023",
       x = "Année",
       y = "Nombre de logements") +
  scale_color_manual(values = c("Homeowner Starts" = "#333366", "Homeowner Completions" = "#707094",
                                "Condo Starts" = "#cd5c5c", "Condo Completions" = "#dc8d8d",
                                "Rental Starts" = "#478547", "Rental Completions" = "#77DD77"),
                     labels = c("Démarrage par le propriétaire", "Achèvements par le propriétaire",
                                "Mises en chantier de condos", "Achèvements de condos",
                                "Débuts de location", "Achèvements de location")) +
  theme_minimal() +
  theme(
    legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(completions_lvl_line, aes(x = Year, y = `Units`, group = `Intended Market`, color = `Intended Market`)) +
  geom_line(size = 1.25) +
  labs(title = "Logements achevés à Laval 2010-2023",
       x = "Année",
       y = "Nombre de logements") +
  scale_color_manual(values = c("Homeowner Completions" = "#707094",
                                "Condo Completions" = "#dc8d8d",
                                "Rental Completions" = "#77DD77"),
                     labels = c("Achèvements par le propriétaire",
                                "Achèvements de condos",
                                "Achèvements de location")) +
  theme_minimal() +
  theme(
    legend.position = "bottom", legend.box = "horizontal", legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1)
  )
# Maps for shelter cost change ----------------------------------------------------
#Grabbing shelter cost data for 2021
shelter_21 <- get_census(dataset = "CA21",
                       regions = list(CSD = 2465005),
                       level = "CT",
                       vectors = c("avg_rent21" = "v_CA21_4318", "med_rent21" = "v_CA21_4317",
                                   "avg_owner21" = "v_CA21_4310", "med_owner21" = "v_CA21_4309")) |> 
  select(GeoUID, avg_rent21, med_rent21, avg_owner21, med_owner21)

#Grabbing shelter cost data for 2016
shelter_16 <- get_census(dataset = "CA16",
                         regions = list(CSD = 2465005),
                         level = "CT",
                         vectors = c("avg_rent16" = "v_CA16_4901", "med_rent16" = "v_CA16_4900",
                                     "avg_owner16" = "v_CA16_4894", "med_owner16" = "v_CA16_4893")) |> 
  select(GeoUID, avg_rent16, med_rent16, avg_owner16, med_owner16)

#Calculating the change between 2021 and 2016
change_5 <- left_join(shelter_21, shelter_16, by = "GeoUID") |> 
  mutate(avg_rent = as.numeric(as.character((avg_rent21 / avg_rent16 - 1) * 100)),
         med_rent = as.numeric(as.character((med_rent21 / med_rent16 - 1) * 100)),
         avg_own = as.numeric(as.character((avg_owner21 / avg_owner16 - 1) * 100)),
         med_own = as.numeric(as.character((med_owner21 / med_owner16 - 1) * 100))) |> 
  select(GeoUID, avg_rent, med_rent, avg_own, med_own)

#Prepping the data to make a map
change_5_map <- left_join(laval_ct, change_5, join_by(GeoUID)) |> 
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .)))

#Plotting the change in average rent (more graphs can be made if needed)
ggplot(data = change_5_map) +
  geom_sf(aes(fill = avg_rent)) +
  labs(title = "% Average Rent Change in Laval 2016-2021",
       fill = "% Average Rent Change") +
  scale_fill_gradientn(colors = curbcut_scale, na.value = "#B3B3BB",
                       guide = guide_colorbar(barheight = 1, barwidth = 10,
                                              title.position = "top",
                                              title.hjust = 0.5)) +  # Center the title
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

# 30, 50, and 80% of income -----------------------------------------------
#Reading affordability data from CurbCut. Change filepath if needed
afford30 <- read_csv("D:/McGill/can_cache/afford30.csv") |> 
  mutate(name = as.character(name)) |> 
  mutate(percentage = affordhou_total_sc30_total_total_pct_2021 * 100) |> 
  mutate(name = sprintf("%.2f", as.numeric(name)))
afford50 <- read_csv("D:/McGill/can_cache/afford50.csv") |> 
  mutate(name = as.character(name)) |> 
  mutate(percentage = affordhou_total_sc50_total_total_pct_2021 * 100) |> 
  mutate(name = sprintf("%.2f", as.numeric(name)))
afford80 <- read_csv("D:/McGill/can_cache/afford80.csv") |> 
  mutate(name = as.character(name)) |> 
  mutate(percentage = affordhou_total_sc80_total_total_pct_2021 * 100) |> 
  mutate(name = sprintf("%.2f", as.numeric(name)))

afford30map <- left_join(laval_ct, afford30, join_by("GeoUID" == "name")) |> 
  mutate(percentage_bins = cut(percentage, breaks = c(5, 12.5, 20, 27.5, 35, 42.5)))
afford50map <- left_join(laval_ct, afford50, join_by("GeoUID" == "name")) |> 
  mutate(percentage_bins = cut(percentage, breaks = c(-Inf, 3.5, 7, 10.5, 14, 17.5)))
afford80map <- left_join(laval_ct, afford80, join_by("GeoUID" == "name")) |> 
  mutate(percentage_bins = cut(percentage, breaks = c(-Inf, 0.75, 1.5, 2.25, 3, Inf)))

ggplot(data = afford30map) +
  geom_sf(aes(fill = percentage_bins)) +
  labs(title = "Abordabilité du logement à Laval 2021 (>30%)",
       fill = "Proportion de ménages consacrant plus de 30% de leur revenu au logement") +
  scale_fill_manual(values = curbcut_scale, na.value = "#B3B3BB",
                    labels = c("5-12.5%", "12.5-20%", "20-27.5%", "27.5-35%", "> 35%")) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, 
                             barheight = 1, barwidth = 10)) +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

ggplot(data = afford50map) +
  geom_sf(aes(fill = percentage_bins)) +
  labs(title = "Abordabilité du logement à Laval 2021 (>50%)",
       fill = "Proportion de ménages consacrant plus de 50% de leur revenu au logement") +
  scale_fill_manual(values = curbcut_scale, na.value = "#B3B3BB",
                    labels = c("< 3.5%", "3.5-7%", "7-10.5%", "10.5-14%", "> 14%")) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, 
                             barheight = 1, barwidth = 10)) +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

ggplot(data = afford80map) +
  geom_sf(aes(fill = percentage_bins)) +
  labs(title = "Abordabilité du logement à Laval 2021 (>80%)",
       fill = "Proportion de ménages consacrant plus de 80% de leur revenu au logement") +
  scale_fill_manual(values = curbcut_scale, na.value = "#B3B3BB",
                    labels = c("< 0.75%", "0.75-1.5%", "1.5-2.25%", "2.25-3%", "> 3%")) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, 
                             barheight = 1, barwidth = 10)) +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

# Service aide au logement ----------------------------------------
#service_aide_au_logement_OMH_laval.pdf in the data folder