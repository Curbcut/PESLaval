### LOGEMENT ###################################################################

source("R/01_startup.R")
library(cmhc)


# Statut d'occupation -----------------------------------------------------

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
  mutate("Owner Households QC" = owner * 100 / total,
         "Tenant Households QC" = tenant * 100 / total) |> 
  select(Year, "Owner Households QC", "Tenant Households QC")

pto_21_CA <- pto_census_CA("CA21", pto_21v, "2021")
pto_16_CA <- pto_census_CA("CA16", pto_16v, "2016")
pto_11_CA <- pto_census_CA("CA11", pto_11v, "2011") |> 
  select(-`NHS Non Return Rate`)
pto_06_CA <- pto_census_CA("CA06", pto_06v, "2006")
pto_01_CA <- pto_census_CA("CA01", pto_01v, "2001") |> 
  mutate(total = owner + tenant)
pto_graph_CA <- bind_rows(pto_21_CA, pto_16_CA, pto_11_CA, pto_06_CA, pto_01_CA) |> 
  mutate("Owner Households CA" = owner * 100 / total,
         "Tenant Households CA" = tenant * 100 / total) |> 
  select(Year, "Owner Households CA", "Tenant Households CA")

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
  select(-total) |> 
  pivot_longer(cols = -Year, names_to = "Type", values_to = "Households") |> 
  group_by(Year) %>%
  mutate(Proportion = Households / sum(Households))


pto_graph <- bind_rows(pto_21, pto_16, pto_11, pto_06, pto_01) |> 
  select(-total) |> 
  pivot_longer(cols = -Year, names_to = "Type", values_to = "Households") |> 
  group_by(Year) %>%
  mutate(Proportion = Households / sum(Households),
         ProportionLabel = convert_pct(Proportion)) |> 
  ungroup() |> 
  mutate(Type = factor(Type, levels = c("owner", "tenant"))) |> 
  arrange(Year) |> 
  group_by(Type) |> 
  mutate(Increase = c(FALSE, diff(Proportion) > 0)) |> 
  ungroup() |> 
  mutate(ProportionLabel = case_when(
    Year == 2001 ~ ProportionLabel,
    Increase == TRUE ~ paste0(ProportionLabel, " ▲"),
    Increase == FALSE ~ paste0(ProportionLabel, " ▼"),
    TRUE ~ ProportionLabel
  ))

# Graphique pour le nombre de ménages
g1 <- ggplot(pto_graph, aes(x = Year, y = Households, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(y = "Nombre de ménages (n)") +
  scale_fill_manual(values = c("owner" = color_theme("redhousing"), "tenant" = color_theme("greenecology")),
                    labels = c("Propriétaire", "Locataire")) +
  scale_y_continuous(labels = convert_number) +
  xlab(NULL) +
  gg_cc_theme_no_sf +
  theme(legend.title = element_blank())

# Graphique pour la proportion des ménages
g2 <- ggplot(pto_graph, aes(x = Year, y = Proportion, fill = Type)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = ProportionLabel, group = Type),
            vjust = 2, color = "black", size = 2.75) +
  labs(y = "Proportion des ménages (%)") +
  scale_fill_manual(values = c("owner" = color_theme("redhousing"), "tenant" = color_theme("greenecology")),
                    labels = c("Propriétaire", "Locataire")) +
  scale_y_continuous(labels = convert_pct) +
  xlab(NULL) +
  gg_cc_theme_no_sf +
  theme(legend.title = element_blank())

# Combinaison des deux graphiques avec facet_wrap et partage de la légende
library(patchwork)

logement_statutoccupation <- (g1 + g2) + plot_layout(guides = 'collect') &
  theme(legend.position = "bottom")

ggplot2::ggsave(filename = here::here("output/axe1/housing/logement_statutoccupation.pdf"),
                plot = logement_statutoccupation, width = 6.5, height = 3.5)

# Valeurs pour le graph

housing_statut_fun <- function(occ, year) {
  x <- pto_graph$Proportion[pto_graph$Year == year & pto_graph$Type == occ]
  convert_pct(x)
}

housing_owner_2016_pct <- housing_statut_fun("owner", 2016)
housing_owner_2001_pct <- housing_statut_fun("owner", 2001)
housing_owner_2021_pct <- housing_statut_fun("owner", 2021)

housing_statut_evol_fun <- function(year, type) {
  pto_graph$Households[pto_graph$Type == type & pto_graph$Year == year]
}
housing_owner_2021 <- housing_statut_evol_fun(2021, "owner")
housing_owner_2016 <- housing_statut_evol_fun(2016, "owner")
housing_evol_owner_1621 <- (housing_owner_2021 - housing_owner_2016) / housing_owner_2016
housing_evol_owner_1621 <- convert_pct(housing_evol_owner_1621)
housing_owner_2021 <- convert_number(housing_owner_2021)
housing_owner_2016 <- convert_number(housing_owner_2016)

housing_tenant_2021 <- housing_statut_evol_fun(2021, "tenant")
housing_tenant_2016 <- housing_statut_evol_fun(2016, "tenant")
housing_evol_tenant_1621 <- (housing_tenant_2021 - housing_tenant_2016) / housing_tenant_2016
housing_evol_tenant_1621 <- convert_pct(housing_evol_tenant_1621)
housing_tenant_2021 <- convert_number(housing_tenant_2021)
housing_tenant_2016 <- convert_number(housing_tenant_2016)

housing_statut_prop_fun_cA <- function(year, type) {
  pto_graph_CA <- bind_rows(pto_21_CA, pto_16_CA, pto_11_CA, pto_06_CA, pto_01_CA) |> 
    pivot_longer(cols = -Year, names_to = "Type", values_to = "Households")
  out <- pto_graph_CA$Households[pto_graph_CA$Type == type & pto_graph_CA$Year == year] / 
    pto_graph_CA$Households[pto_graph_CA$Type == "total" & pto_graph_CA$Year == year]
  convert_pct(out)
}

housing_prop_tenant_2021_CA <- housing_statut_prop_fun_cA(2021, "tenant")
housing_prop_tenant_2016_CA <- housing_statut_prop_fun_cA(2016, "tenant")
housing_prop_tenant_2001_CA <- housing_statut_prop_fun_cA(2001, "tenant")

housing_statut_prop_fun_QC <- function(year, type) {
  pto_graph_QC <- bind_rows(pto_21_QC, pto_16_QC, pto_11_QC, pto_06_QC, pto_01_QC) |> 
    pivot_longer(cols = -Year, names_to = "Type", values_to = "Households")
  out <- pto_graph_QC$Households[pto_graph_QC$Type == type & pto_graph_QC$Year == year] / 
    pto_graph_QC$Households[pto_graph_QC$Type == "total" & pto_graph_QC$Year == year]
  convert_pct(out)
}

housing_prop_tenant_2021_QC <- housing_statut_prop_fun_QC(2021, "tenant")
housing_prop_tenant_2016_QC <- housing_statut_prop_fun_QC(2016, "tenant")
housing_prop_tenant_2001_QC <- housing_statut_prop_fun_QC(2001, "tenant")


# # CMHC values -------------------------------------------------------------
# 
# #See values of CMHC
# cmhc_breakdown <- list_cmhc_breakdowns()
# 
# #Grabbing Laval's shapefile by census tract
# laval_ct <- cancensus::get_census(dataset = "CA21", 
#                                   regions = list(CSD = 2465005), 
#                                   level = "CT", 
#                                   geo_format = "sf")
# 
# #Using curbcut colors
# curbcut_scale <- c("#C4CDE1", "#98A8CB", "#6C83B5", "#4C5C7F", "#2B3448")
# curbcut_scale_afford <- c("#f9fafc", "#C4CDE1", "#98A8CB", "#6C83B5", "#4C5C7F", "#2B3448")
# 
# #Grabbing Montreal CMA shapefile
# mtlcma_sf <- cancensus::get_census(dataset = "CA21",
#                                    regions = list(CMA = 24462),
#                                    level = "CMA",
#                                    geo_format = "sf")
# 
# #Grabbing Laval CSD shapefile
# laval_csd <- cancensus::get_census(dataset = "CA21", 
#                                    regions = list(CSD = 2465005), 
#                                    level = "CSD", 
#                                    geo_format = "sf")
# 
# #Setting up bounding box
# laval_bbox <- st_bbox(laval_csd)


# Loyer médian ------------------------------------------------------------

# Setting the years to pull data from
years <- 2010:2023

# CPI for october of all the years
CPI <- tibble::as_tibble(read.csv("data/CPI.csv")) |> 
  select(REF_DATE, GEO, product = `Products.and.product.groups`, UOM, 
         value = VALUE) |>
  suppressWarnings()

CPI <- CPI[CPI$GEO == "Quebec" & CPI$product == "Shelter", ] |> 
  mutate(month = ym(REF_DATE)) |> 
  select(month, value) |> 
  filter(month >= ym(sprintf("%s-10", years[1])))

CPI_rent <- 
  CPI |> 
  filter(month(month) == 10) |> 
  mutate(year = year(month)) |> 
  select(year, value) |> 
  mutate(value = value / value[year == years[length(years)]])

# Pulling average rent data
avg_rent_cmhc <- function(geoid, years, breakdown = "Census Subdivision") {
  map_dfr(years, function(cyear) {
    cmhc::get_cmhc(survey = "Rms", series = "Median Rent", dimension = "Bedroom Type",
             breakdown = breakdown, geo_uid = geoid, year = cyear,
             frequency = "Annual") |> 
      filter(`Bedroom Type` == "2 Bedroom")
  })
}

#Grabbing annual average rent data from 2010 to 2023
avg_rent_lvl <- avg_rent_cmhc(2465005, years) |> 
  mutate(Geography = "Laval") |> 
  select(Geography, Value, Year)
avg_rent_qc <- avg_rent_cmhc("01", years, "Provinces") |> 
  filter(Provinces == "Quebec") |> 
  mutate(Geography = "Québec") |> 
  select(Geography, Value, Year)

# Bring dollar values to 2023 $
avg_rent_lvl_inf <- avg_rent_lvl
avg_rent_lvl_inf$Value <- avg_rent_lvl_inf$Value / CPI_rent$value

# # Manually inputting the province of Quebec's data as it's unavailable using the CMHC package
# # src = https://www.cmhc-schl.gc.ca/professionals/housing-markets-data-and-research/housing-data/data-tables/rental-market/rental-market-report-data-tables
# avg_rent_qc <- data.frame(
#   Geography = "Québec",
#   Year = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019,
#            2020, 2021, 2022, 2023),
#   Value = c(648, 666, 693, 679, 691, 712, 727, 736, 761, 800, 845, 874, 952, 1022)
# )

avg_rent_qc_inf <- avg_rent_qc
avg_rent_qc_inf$Value <- avg_rent_qc_inf$Value / CPI_rent$value

# Preparing the table for the line graph
avg_rent_annual <- bind_rows(avg_rent_lvl, avg_rent_qc) |> 
  mutate(Geography = factor(Geography, levels = c("Laval", "Québec")))
avg_rent_annual_inf <- bind_rows(avg_rent_lvl_inf, avg_rent_qc_inf) |> 
  mutate(Geography = factor(Geography, levels = c("Laval", "Québec")))

# avg_rent_annual$indexed <- "Dollars courants (non ajusté)"
# avg_rent_annual_inf$indexed <- "Ajusté à l'inflation ($ de 2023)"
# avg_rent_annual <- rbind(avg_rent_annual_inf, avg_rent_annual)

# Line graph
housing_loyermed_plot <-
ggplot(avg_rent_annual, aes(x = Year, y = `Value`, group = Geography, color = Geography)) +
  geom_line(linewidth = 1.5) +
  labs(title = element_blank(),
       x = NULL,
       y = "Loyer mensuel médian ($)") +
  scale_color_manual(values = c("Laval" = color_theme("greenecology"), "Québec" = color_theme("blueexplorer"))) +
  scale_y_continuous(labels = convert_number) +
  gg_cc_theme_no_sf +
  theme(legend.title = element_blank())# +
  # facet_wrap(~indexed)

ggplot2::ggsave(filename = here::here("output/axe1/housing/housing_loyermed_plot.pdf"),
                plot = housing_loyermed_plot, width = 3, height = 3)

housing_loyer_2023 <- avg_rent_annual$Value[
  avg_rent_annual$Year == 2023 & avg_rent_annual$Geography == "Laval"]
housing_loyer_2010 <- avg_rent_annual$Value[
  avg_rent_annual$Year == 2010 & avg_rent_annual$Geography == "Laval"]
housing_loyer_var <- convert_pct((housing_loyer_2023 - housing_loyer_2010) / housing_loyer_2010)

housing_loyer_2010_noinf <- avg_rent_annual$Value[
  avg_rent_annual$Year == 2010 & avg_rent_annual$Geography == "Laval"]
housing_loyer_var_noinf <- convert_pct((housing_loyer_2023 - housing_loyer_2010_noinf) / housing_loyer_2010_noinf)


housing_loyer_2023 <- convert_number(housing_loyer_2023)
housing_loyer_2010 <- convert_number(housing_loyer_2010)
housing_loyer_2010_noinf <- convert_number(housing_loyer_2010_noinf)

housing_loyer_2023_QC <- avg_rent_annual$Value[
  avg_rent_annual$Year == 2023 & avg_rent_annual$Geography == "Québec"]
housing_loyer_2010_QC <- avg_rent_annual$Value[
  avg_rent_annual$Year == 2010 & avg_rent_annual$Geography == "Québec"]

housing_loyer_var_QC <- convert_pct((housing_loyer_2023_QC - housing_loyer_2010_QC) / housing_loyer_2010_QC)

housing_loyer_2023_QC <- convert_number_noround(housing_loyer_2023_QC)


# # Median Rent -------------------------------------------------------------
# #Choosing years to pull data from
# years <- 2010:2023
# 
# #Pulling median rent data
# med_rent_cmhc <- function(geoid, years, geoname) {
#   map_dfr(years, function(cyear) {
#     get_cmhc(survey = "Rms", series = "Median Rent", dimension = "Bedroom Type",
#              breakdown = "Census Subdivision", geo_uid = geoid, year = cyear) |> 
#       mutate(Geography = geoname) |> 
#       filter(str_detect(`Bedroom Type`, "Total")) |> 
#       select(Geography, Year, Value)
#   })
# }
# 
# #Grabbing annual median rent data from 2010 to 2023
# med_rent_lvl <- med_rent_cmhc(2465005, years, "Laval")
# med_rent_mtl <- med_rent_cmhc(2466023, years, "Montréal")
# 
# #Manually inputting the province of Quebec's data as it's unavailable using the CMHC package
# #src = https://www.cmhc-schl.gc.ca/professionals/housing-markets-data-and-research/housing-data/data-tables/rental-market/rental-market-report-data-tables
# med_rent_qc <- data.frame(
#   Geography = "Québec",
#   Year = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019,
#            2020, 2021, 2022, 2023),
#   Value = c(610, 629, 625, 645, 650, 665, 675, 693, 705, 745, 773, 800, 860, 939)
# )
# 
# #YoY growth calculation
# med_rent_yoy <- bind_rows(med_rent_lvl, med_rent_mtl, med_rent_qc) |> 
#   pivot_wider(id_cols = "Geography", names_from = Year, values_from = Value) |> 
#   mutate(`2010_2011` = (`2011` / `2010` - 1),
#          `2011_2012` = (`2012` / `2011` - 1),
#          `2012_2013` = (`2013` / `2012` - 1),
#          `2013_2014` = (`2014` / `2013` - 1),
#          `2014_2015` = (`2015` / `2014` - 1),
#          `2015_2016` = (`2016` / `2015` - 1),
#          `2016_2017` = (`2017` / `2016` - 1),
#          `2017_2018` = (`2018` / `2017` - 1),
#          `2018_2019` = (`2019` / `2018` - 1),
#          `2019_2020` = (`2020` / `2019` - 1),
#          `2020_2021` = (`2021` / `2020` - 1),
#          `2021_2022` = (`2022` / `2021` - 1),
#          `2022_2023` = (`2023` / `2022` - 1)) |> 
#   select(Geography, `2010_2011`,`2011_2012`, `2012_2013`, `2013_2014`, `2014_2015`,
#          `2015_2016`, `2016_2017`, `2017_2018`, `2018_2019`, `2019_2020`, `2020_2021`,
#          `2021_2022`, `2022_2023`) |> 
#   rename("2011" = `2010_2011`, "2012" = `2011_2012`, "2013" = `2012_2013`,
#          "2014" = `2013_2014`, "2015" = `2014_2015`, "2016" = `2015_2016`,
#          "2017" = `2016_2017`, "2018" = `2017_2018`, "2019" = `2018_2019`,
#          "2020" = `2019_2020`, "2021" = `2020_2021`, "2022" = `2021_2022`,
#          "2023" =  `2022_2023`) |> 
#   pivot_longer(cols = -Geography, names_to = "Year", values_to = "Growth")
# 
# #Preparing the table for the line graph
# med_rent_annual <- bind_rows(med_rent_lvl, med_rent_mtl, med_rent_qc) |> 
#   mutate(Geography = factor(Geography, levels = c("Laval", "Montréal", "Québec")))
# 
# med_rent_graph <- ggplot(med_rent_annual, aes(x = Year, y = `Value`, group = Geography, color = Geography)) +
#   geom_line(linewidth = 1.5) +
#   labs(y = "Loyer mensuel médian ($)") +
#   scale_color_manual(values = c("Laval" = "#A3B0D1", "Montréal" = "#E08565",
#                                 "Québec" = "#73AD80"),
#                      labels = c("Laval", "Montréal", "Québec")) +
#   theme_minimal() +
#   theme(
#     legend.position = "bottom", legend.box = "horizontal", axis.title.x = element_blank(),
#     legend.title = element_blank(), plot.title = element_text(hjust = 0.5),
#     text=element_text(family="KMR Apparat Regular"))
# 
# med_yoy_growth_graph <- ggplot(med_rent_yoy, aes(x = Year, y = Growth, fill = Geography)) +
#   geom_bar(stat = "identity", position = position_dodge()) +
#   facet_wrap(~Geography) +
#   scale_fill_manual(values = c("Laval" = "#A3B0D1", "Montréal" = "#E08565", "Québec" = "#73AD80")) +
#   scale_y_continuous(labels = convert_pct) +
#   theme_minimal() +
#   labs(y = "Variation du loyer médian") +
#   theme(legend.position = "none", plot.title = element_text(hjust = 0.5),
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         text=element_text(family="KMR Apparat Regular"), axis.title.x = element_blank())
# 
# # Monthly Tenant Cost by Laval Neighborhood -------------------------------
# nbhd_mtlcma <- cmhc::get_cmhc(survey = "Rms", series = "Median Rent", 
#                        dimension = "Bedroom Type", breakdown = "Neighbourhoods", 
#                        geo_uid = 24462, year = 2023)
# 
# zones <- cmhc::get_cmhc_geography(level = "NBHD")
# zones <- sf::st_transform(zones, crs = 32618)
# 
# laval <- cancensus::get_census("CA21", regions = list(CSD = 2465005), level = "CSD", geo_format = "sf")
# laval <- sf::st_transform(laval, crs = 32618)
# 
# laval_zones <- sf::st_filter(zones, laval) |> 
#   select(NBHD_NAME_EN)
# 
# med_rent_lvl <- nbhd_mtlcma |> 
#   semi_join(laval_zones, by = join_by("Neighbourhoods" == "NBHD_NAME_EN")) |> 
#   filter(`Bedroom Type` == "2 Bedroom")
# 
# med_rent_lvl_sf <- laval_zones |> 
#   left_join(med_rent_lvl, join_by(NBHD_NAME_EN == Neighbourhoods)) |>
#   mutate(breaks = cut(Value, breaks = c(-Inf, 800, 875, 950, 1025, Inf),
#                          labels = c("< 800", "800-875", "875-950", "950-1 025", "> 1 025"), 
#                          right = FALSE)) |> 
#   st_transform(4326)
# 
# mtlcma_sf_32618 <- st_transform(mtlcma_sf, 32618)
# laval_csd_32618 <- st_transform(laval_csd, 32618)
# laval_bbox_32618 <- st_bbox(laval_csd_32618)
# 
# med_rent_map <- ggplot(data = med_rent_lvl_sf) +
#   gg_cc_tiles +
#   geom_sf(aes(fill = breaks), color = "black") +
#   scale_fill_manual(values = curbcut_scale, na.value = "#B3B3BB") +
#   labs(fill = "Loyer médian ($)") +
#   theme_void() +
#   theme(legend.position = "bottom",  legend.justification = "center",
#         text=element_text(family="KMR Apparat Regular"),
#         legend.box.margin = margin(t = -20)) +
#   guides(fill = guide_legend(title.position = "top", title.hjust = 0.5,
#                              barwidth = 1, barheight = 1, nrow = 1))
# 
# nbhd_mtlcma_18 <- cmhc::get_cmhc(survey = "Rms", series = "Median Rent", 
#                                  dimension = "Bedroom Type", breakdown = "Neighbourhoods", 
#                                  geo_uid = 24462, year = 2018) |> 
#   semi_join(laval_zones, by = join_by("Neighbourhoods" == "NBHD_NAME_EN")) |> 
#   filter(`Bedroom Type` == "2 Bedroom") |> 
#   rename(Value2018 = Value) |> 
#   select(Neighbourhoods, Value2018)
# 
# med5 <- med_rent_lvl_sf |> 
#   left_join(nbhd_mtlcma_18, by = join_by(NBHD_NAME_EN == `Neighbourhoods`)) |> 
#   mutate(Change = (Value / Value2018 - 1) * 100)
# 
# ggplot(data = med5) +
#   geom_sf(data = mtlcma_sf, fill = "lightgrey") +
#   geom_sf(aes(fill = Change)) +
#   scale_fill_gradientn(colors = curbcut_scale, na.value = "#B3B3BB") +
#   labs(title = "Variation en pourcentage du loyer médian* à Laval 2018-2023",
#        subtitle = "*Pour un appartement de deux chambres",
#        fill = "Variation du loyer (%)") +
#   theme_minimal() +
#   theme(axis.line = element_blank(), axis.text = element_blank(),
#         axis.title = element_blank(), axis.ticks = element_blank(),
#         panel.grid = element_blank(), legend.position = "bottom",
#         plot.title = element_text(hjust = 0.5), legend.justification = "center",
#         plot.subtitle = element_text(hjust = 0.5),
#         panel.background = element_rect(fill = "lightblue")) +
#   guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
#                                barwidth = 10, barheight = 1)) +
#   coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
#            ylim = c(laval_bbox$ymin, laval_bbox$ymax))

# # Monthly Tenant Cost by Laval Zone -----------------------------------------------------
# # CMHC zones
# zones <- cmhc::get_cmhc_geography(level = "NBHD")
# zones <- sf::st_transform(zones, crs = 32618)
# 
# # Laval
# laval <- cancensus::get_census("CA21", regions = list(CSD = 2465005), level = "CSD", geo_format = "sf")
# laval <- sf::st_transform(laval, crs = 32618)
# 
# # Which CMHC zones are in Laval
# laval_zones <- sf::st_filter(zones, laval)
# 
# #Setting the years to pull data
# years <- 2010:2023
# 
# # Get the average rent for year 2023
# avg_rent <- lapply(years, \(year) {
#   avg_r <- cmhc::get_cmhc(survey = "Rms", series = "Average Rent", breakdown = "Neighbourhoods", 
#                           dimension = "Bedroom Type", geo_uid = "24462", year = year) 
#   # Only keep Laval's
#   avg_r[avg_r$`Neighbourhoods` %in% laval_zones$ZONE_NAME_EN, ]
# })
# 
# #Pulling average rent data for each zone and each bedroom type
# avg_rent <- lapply(avg_rent, `[`, c("Neighbourhoods", "Bedroom Type", "Value"))
# avg_rent <- mapply(\(year, table) {
#   names(table)[3] <- paste0("avg_rent_", year)
#   table
# }, years, avg_rent, SIMPLIFY = FALSE)
# avg_rent <- Reduce(merge, avg_rent)
# 
# #Calculating change in rent from 2018 to 2023
# avg_rent5 <- avg_rent |> 
#   filter(`Bedroom Type` == "Total") |> 
#   mutate(year5 = (avg_rent_2023 / avg_rent_2018 - 1) * 100) |> 
#   select(`Survey Zones`, `year5`) |> 
#   rename("avg_rent_2023" = year5)
# 
# #Mapping the average rent by zone onto Laval
# avg_rent[avg_rent$`Bedroom Type` == "Total", ] |> 
#   merge(laval_zones[c("ZONE_NAME_EN")], by.x = "Neighbourhoods", by.y = "ZONE_NAME_EN") |> 
#   sf::st_as_sf() |> 
#   ggplot2::ggplot() +
#   ggplot2::labs(title = "Loyer moyen à Laval 2023",
#                 fill = "Loyer moyen") +
#   ggplot2::geom_sf(ggplot2::aes(fill = avg_rent_2023)) +
#   ggplot2::theme_minimal() +
#   ggplot2::scale_fill_gradientn(colors = curbcut_scale) +
#   ggplot2::theme(axis.line = element_blank(), axis.text = element_blank(),
#                        axis.title = element_blank(), axis.ticks = element_blank(),
#                        panel.grid = element_blank(), legend.position = "bottom",
#                        plot.title = element_text(hjust = 0.5))
# 
# #Plotting change in rent
# avg_rent5 |> 
#   merge(laval_zones[c("ZONE_NAME_EN")], by.x = "Survey Zones", by.y = "ZONE_NAME_EN") |> 
#   sf::st_as_sf() |> 
#   ggplot2::ggplot() +
#   ggplot2::labs(title = "Variation mensuelle moyenne du loyer à Laval (2018-2023)",
#                 fill = "Variation du loyer (%)") +
#   ggplot2::geom_sf(ggplot2::aes(fill = avg_rent_2023)) +
#   ggplot2::theme_minimal() +
#   ggplot2::scale_fill_gradientn(colors = curbcut_scale) +
#   ggplot2::theme(axis.line = ggplot2::element_blank(), axis.text = ggplot2::element_blank(),
#                  axis.title = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
#                  panel.grid = ggplot2::element_blank(), legend.position = "bottom",
#                  plot.title = ggplot2::element_text(hjust = 0.5)) +
#   ggplot2::guides(fill = ggplot2::guide_colorbar(title.position = "top", title.hjust = 0.5))

# # YoY Growth for Rent -----------------------------------------------------
# #Run this only after you run average and median rent above
# 
# #Calculating YoY change
# avg_yoy <- avg_rent_annual |> 
#   group_by(Geography) |> 
#   arrange(Geography, Year) |> 
#   mutate(PercentChange = (Value / lag(Value) - 1) * 100) |> 
#   filter(Year != 2010) |> 
#   select(-Value)
# 
# #Graphing YoY change for average monthly rent
# ggplot(avg_yoy, aes(x = Year, y = `PercentChange`, group = Geography, color = Geography)) +
#   geom_hline(yintercept = 0, size = 0.5, color = "black") +
#   geom_line(size = 1.25) +
#   labs(title = "Change in Average Monthly Rent 2011-2023",
#        x = "Year",
#        y = "Change in Average Rent (%)") +
#   scale_color_manual(values = c("Laval" = "royalblue2", "Montreal" = "indianred2",
#                                 "Quebec" = "gold3"),
#                      labels = c("Laval", "Montreal", "Quebec")) +
#   scale_x_continuous(breaks = 2011:2023) +
#   theme_minimal() +
#   theme(
#     legend.position = "bottom", legend.box = "horizontal",
#     legend.title = element_blank(), plot.title = element_text(hjust = 0.5)
#   )
# 
# #Calculating YoY change for median rent
# med_yoy <- med_rent_annual |> 
#   group_by(Geography) |> 
#   arrange(Geography, Year) |> 
#   mutate(PercentChange = (Value / lag(Value) - 1) * 100) |> 
#   filter(Year != 2010) |> 
#   select(-Value)
# 
# #Graphing median rent YoY Change
# ggplot(med_yoy, aes(x = Year, y = `PercentChange`, group = Geography, color = Geography)) +
#   geom_hline(yintercept = 0, size = 0.5, color = "black") +
#   geom_line(size = 1.25) +
#   labs(title = "Change in Median Monthly Rent 2011-2023",
#        x = "Year",
#        y = "Change in Median Rent (%)") +
#   scale_color_manual(values = c("Laval" = "royalblue2", "Montreal" = "indianred2",
#                                 "Quebec" = "gold3"),
#                      labels = c("Laval", "Montreal", "Quebec")) +
#   scale_x_continuous(breaks = 2011:2023) +
#   theme_minimal() +
#   theme(
#     legend.position = "bottom", legend.box = "horizontal",
#     legend.title = element_blank(), plot.title = element_text(hjust = 0.5)
#   )

# Loyer médian ------------------------------------------------------------

osc_21v <- c("med_owner" = "v_CA21_4309", 
             "med_tenant" = "v_CA21_4317",
             "avg_owner" = "v_CA21_4310")
osc_16v <- c("med_owner" = "v_CA16_4893",
             "med_tenant" = "v_CA16_4900",
             "avg_owner" = "v_CA16_4894")
osc_11v <- c("med_owner" = "v_CA11N_2284", 
             "avg_owner" = "v_CA11N_2285")

owner_tenant_sf <- get_census(dataset = "CA21",
                              regions = list(CSD = 2465005),
                              level = "CT",
                              vectors = c(osc_21v, owner_hou = "v_CA21_4305",
                                          tenant_hou = "v_CA21_4313"),
                              geo_format = "sf")

# Map loyer médian par DA
labels <- c("300 $ - 600 $", "600 $ - 900 $", "900 $ - 1 200 $", "+ 1 200 $")
t <- add_bins(df = owner_tenant_sf,
              variable = "med_tenant",
              breaks = c(-Inf, 600, 900, 1200, Inf),
              labels = labels
)
# t <- Reduce(rbind,
#             split(t, t$binned_variable, drop = FALSE) |>
#               lapply(\(x) {
#                 out <- tibble::tibble(x$binned_variable)
#                 out$geometry <- sf::st_union(x)
#                 sf::st_as_sf(out, crs = 4326)[1, ]
#               })
# ) |> sf::st_as_sf()
# names(t)[1] <- "binned_variable"

housing_median_rent_plot <-
  ggplot(data = t) +
  gg_cc_tiles +
  geom_sf(aes(fill = binned_variable), color = "transparent", lwd = 0) +
  scale_fill_manual(
    values = curbcut_colors$left_5$fill[2:5],  # Include NA color first
    # na.value = curbcut_colors$left_5$fill[1],
    name = element_blank(),
    breaks = levels(t$binned_variable),
    labels = levels(t$binned_variable),
    guide = guide_legend(
      title.position = "top",
      label.position = "bottom",
      keywidth = 4,
      keyheight = 0.5
    )
  ) +
  gg_cc_theme

ggplot2::ggsave(filename = here::here("output/axe1/housing/housing_median_rent_plot.pdf"),
                plot = housing_median_rent_plot, width = 6.5, height = 4)


# Tableau des loyers médians par secteurs
owner_tenant_sf_2021 <- owner_tenant_sf
z <- sf::st_intersects(sf::st_centroid(owner_tenant_sf_2021), laval_sectors)
owner_tenant_sf_2021$secteur <- sapply(z, \(x) {
  if (length(x) == 0) return(NA)
  laval_sectors$name[x]
})
owner_tenant_sf_2021 <-
  owner_tenant_sf_2021 |>
  group_by(secteur) |>
  summarize(med_tenant = weighted_mean(med_tenant, tenant_hou, na.rm = TRUE),
            med_owner = weighted_mean(med_owner, owner_hou, na.rm = TRUE))
owner_tenant_sf_2021 <- owner_tenant_sf_2021[1:6, ]
owner_tenant <- sf::st_drop_geometry(owner_tenant_sf_2021)
names(owner_tenant) <- c("Secteur", "Loyer médian mensuel", "Frais de logement mensuels médians (propriétaires)")

housing_loyer_med_table <-
  gt(owner_tenant[1:6, 1:2]) |> 
  # Appliquer une mise en couleur sur les colonnes médianes
  data_color(
    columns = 2,
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = NULL
    )
  ) |> 
  fmt(columns = 2, fns = \(x) paste(convert_number_tens(x), "$")) |> 
  # Ajouter des bordures en haut de chaque groupe de ligne
  tab_style(
    style = cell_borders(
      sides = c("top"),
      color = "white",
      weight = px(10)
    ),
    locations = cells_row_groups()
  ) |> 
  # Appliquer le style de la police à toute la table
  tab_style(
    style = cell_text(
      font = "KMR Apparat Regular"
    ),
    locations = cells_body()
  ) |> 
  tab_style(
    style = cell_text(
      font = "KMR Apparat Regular"
    ),
    locations = cells_column_labels()
  ) |> 
  # Options générales pour la table
  tab_options(
    table.font.size = 12,
    row_group.font.size = 12,
    table.width = px(6 * 96)
  )

gtsave(housing_loyer_med_table, "output/axe1/housing/housing_loyer_med_table.png", zoom = 3)


lavalouest_loyer <- owner_tenant$`Loyer médian mensuel`[owner_tenant$Secteur == "Laval-Ouest, Sainte-Dorothée, Laval-sur-le-Lac"]
lavalouest_loyer <- convert_number_tens(lavalouest_loyer)
chomedey_loyer <- owner_tenant$`Loyer médian mensuel`[owner_tenant$Secteur == "Chomedey"]
chomedey_loyer <- convert_number_tens(chomedey_loyer)

mobility_status <- get_census(dataset = "CA21",
                              regions = list(CSD = 2465005),
                              level = "DA",
                              vectors = c(mobility_5 = "v_CA21_5778",
                                          total = "v_CA21_5772"),
                              geo_format = "sf")

z <- sf::st_intersects(sf::st_centroid(mobility_status), laval_sectors)
mobility_status$secteur <- sapply(z, \(x) {
  if (length(x) == 0) return(NA)
  laval_sectors$name[x]
})
mobility_status <-
  mobility_status |>
  group_by(secteur) |>
  summarize(mobility_5 = sum(mobility_5, na.rm = TRUE) / sum(total, na.rm = TRUE))
mobility_status <- mobility_status[1:6, ]
mobility_status_chomedey <- convert_pct(mobility_status$mobility_5[mobility_status$secteur == "Chomedey"])

mobility_status_ldr <- convert_pct(mobility_status$mobility_5[mobility_status$secteur == "Laval-des-Rapides, Pont-Viau"])


mobility_status_CSD <- get_census(dataset = "CA21",
                              regions = list(CSD = 2465005),
                              level = "CSD",
                              vectors = c(mobility_5 = "v_CA21_5778",
                                          total = "v_CA21_5772"),
                              geo_format = "sf") |> 
  summarize(mobility_5 = sum(mobility_5, na.rm = TRUE) / sum(total, na.rm = TRUE)) |> 
  pull(mobility_5) |> 
  convert_pct()


# Changement des deux derniers recensements
vars <- names(c(osc_16v, owner_hou = "v_CA16_4890",
                tenant_hou = "v_CA16_4897"))

# 2016 in sectors
owner_tenant_sf_2016 <- get_census(dataset = "CA16",
                                   regions = list(CSD = 2465005),
                                   level = "CT",
                                   vectors = c(osc_16v, owner_hou = "v_CA16_4890",
                                               tenant_hou = "v_CA16_4897"),
                                   geo_format = "sf")
z <- sf::st_intersects(sf::st_centroid(owner_tenant_sf_2016), laval_sectors)
owner_tenant_sf_2016$secteur <- sapply(z, \(x) {
  if (length(x) == 0) return(NA)
  laval_sectors$name[x]
})
owner_tenant_sf_2016 <- sf::st_drop_geometry(owner_tenant_sf_2016)
owner_tenant_sf_2016 <-
  owner_tenant_sf_2016 |>
  group_by(secteur) |>
  summarize(med_tenant_2016 = weighted_mean(med_tenant, tenant_hou, na.rm = TRUE),
            med_owner_2016 = weighted_mean(med_owner, owner_hou, na.rm = TRUE))
owner_tenant_sf_2016 <- owner_tenant_sf_2016[1:6,]

# Bind both years and calculate variations
owner_tenant_sf_var <- left_join(sf::st_drop_geometry(owner_tenant_sf_2021), owner_tenant_sf_2016,
                                 by = "secteur")
owner_tenant_sf_var$var_tenant <- (owner_tenant_sf_var$med_tenant - owner_tenant_sf_var$med_tenant_2016) / owner_tenant_sf_var$med_tenant_2016
owner_tenant_sf_var$var_owner <- (owner_tenant_sf_var$med_owner - owner_tenant_sf_var$med_owner_2016) / owner_tenant_sf_var$med_owner_2016


owner_tenant_sf_var_table <- owner_tenant_sf_var[c("secteur", "var_tenant", "var_owner")]
# Add Quebec and Laval
housing_owner_CSD_cost <- get_census(dataset = "CA21",
                                     regions = list(CSD = 2465005),
                                     level = "CSD",
                                     vectors = c(osc_21v),
                                     geo_format = "sf") |> 
  pull(med_owner)
housing_owner_CSD_cost_2016 <- get_census(dataset = "CA21",
                                          regions = list(CSD = 2465005),
                                          level = "CSD",
                                          vectors = c(osc_16v),
                                          geo_format = "sf") |> 
  pull(med_owner)
housing_owner_PR_cost <- get_census(dataset = "CA21",
                                    regions = list(PR = 24),
                                    level = "PR",
                                    vectors = c(osc_21v),
                                    geo_format = "sf") |> 
  pull(med_owner)
housing_owner_PR_cost_2016 <- get_census(dataset = "CA21",
                                         regions = list(PR = 24),
                                         level = "PR",
                                         vectors = c(osc_16v),
                                         geo_format = "sf") |> 
  pull(med_owner)
housing_tenant_CSD_rent <- get_census(dataset = "CA21",
                                     regions = list(CSD = 2465005),
                                     level = "CSD",
                                     vectors = c(osc_21v),
                                     geo_format = "sf") |> 
  pull(med_tenant)
housing_tenant_CSD_rent_2016 <- get_census(dataset = "CA21",
                                          regions = list(CSD = 2465005),
                                          level = "CSD",
                                          vectors = c(osc_16v),
                                          geo_format = "sf") |> 
  pull(med_tenant)
housing_tenant_PR_rent <- get_census(dataset = "CA21",
                                    regions = list(PR = 24),
                                    level = "PR",
                                    vectors = c(osc_21v),
                                    geo_format = "sf") |> 
  pull(med_tenant)
housing_tenant_PR_rent_2016 <- get_census(dataset = "CA21",
                                         regions = list(PR = 24),
                                         level = "PR",
                                         vectors = c(osc_16v),
                                         geo_format = "sf") |> 
  pull(med_tenant)

owner_tenant_sf_var_table <- rbind(
  rbind(tibble::tibble(secteur = "Ensemble du Québec", 
                       var_tenant = (housing_tenant_PR_rent - housing_tenant_PR_rent_2016) / housing_tenant_PR_rent_2016, 
                       var_owner = (housing_owner_PR_cost - housing_owner_PR_cost_2016) / housing_owner_PR_cost_2016),
        
        tibble::tibble(secteur = "Ville de Laval", 
                       var_tenant = (housing_tenant_CSD_rent - housing_tenant_CSD_rent_2016) / housing_tenant_CSD_rent_2016, 
                       var_owner = (housing_owner_CSD_cost - housing_owner_CSD_cost_2016) / housing_owner_CSD_cost_2016)
  ), owner_tenant_sf_var_table
)

names(owner_tenant_sf_var_table) <- c(" ", "Augmentation du loyer mensuel médian (2016 - 2021)", 
                                      "Augmentation des frais de logements mensuels médians (propriétaires) (2016 - 2021)")


loyer_med_var_table <-
  gt(owner_tenant_sf_var_table[1:2]) |> 
  # Appliquer une mise en couleur sur les colonnes médianes
  data_color(
    columns = 2,
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = NULL
    )
  ) |> 
  fmt(columns = 2, fns = convert_pct) |> 
  # Adding the row group for "Secteur"
  tab_row_group(
    label = "Secteur",
    rows = 3:nrow(owner_tenant_sf_var_table)
  ) |>
  # Adding the row group for "Région"
  tab_row_group(
    label = "Région",
    rows = 1:2
  ) |>
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        rows = 2
      )
    ) |>
    tab_style(
      style = cell_borders(
        sides = c("top"),
        color = "white",
        weight = px(10)
      ),
      locations = cells_row_groups()
    ) |>
    # Apply font style to the whole table
    tab_style(
      style = cell_text(
        font = "KMR Apparat Regular"
      ),
      locations = cells_body()
    ) |>
    tab_style(
      style = cell_text(
        font = "KMR Apparat Regular"
      ),
      locations = cells_column_labels()
    ) |>
    tab_style(
      style = cell_text(
        font = "KMR Apparat Regular"
      ),
      locations = cells_row_groups()
    ) |>
    tab_style(
      style = cell_fill(color = "#F0F0F0"),
      locations = cells_row_groups()
    ) |> 
    tab_options(
      table.font.size = 12,
      row_group.font.size = 12,
      table.width = px(6 * 96)
    )
  

gtsave(loyer_med_var_table, "output/axe1/housing/loyer_med_var_table.png", zoom = 3)

losd <- "Laval-Ouest, Sainte-Dorothée, Laval-sur-le-Lac"
housing_rent_losd <- owner_tenant_sf_var$med_tenant[owner_tenant_sf_var == losd]
housing_rent_losd <- convert_number_tens(housing_rent_losd)
housing_rent_losd_2016 <- owner_tenant_sf_var$med_tenant_2016[owner_tenant_sf_var == losd]
housing_rent_losd_2016 <- convert_number_tens(housing_rent_losd_2016)

housing_rent_var_losd <- owner_tenant_sf_var_table$`Augmentation du loyer mensuel médian (2016 - 2021)`[
  owner_tenant_sf_var_table$` ` == losd
] |> convert_pct()

srfb <- "Chomedey"
housing_rent_srfb <- owner_tenant_sf_var$med_tenant[owner_tenant_sf_var == srfb]
housing_rent_srfb <- convert_number_tens(housing_rent_srfb)
housing_rent_srfb_2016 <- owner_tenant_sf_var$med_tenant_2016[owner_tenant_sf_var == srfb]
housing_rent_srfb_2016 <- convert_number_tens(housing_rent_srfb_2016)
housing_rent_var_srfb <- owner_tenant_sf_var_table$`Augmentation du loyer mensuel médian (2016 - 2021)`[
  owner_tenant_sf_var_table$` ` == srfb
] |> convert_pct()

svpsf <- "Duvernay, Saint-Vincent-de-Paul, Saint-François"
housing_rent_svpsf <- owner_tenant_sf_var$med_tenant[owner_tenant_sf_var == svpsf]
housing_rent_svpsf <- convert_number_tens(housing_rent_svpsf)
housing_rent_svpsf_2016 <- owner_tenant_sf_var$med_tenant_2016[owner_tenant_sf_var == svpsf]
housing_rent_svpsf_2016 <- convert_number_tens(housing_rent_svpsf_2016)
housing_rent_var_svpsf <- owner_tenant_sf_var_table$`Augmentation du loyer mensuel médian (2016 - 2021)`[
  owner_tenant_sf_var_table$` ` == svpsf
] |> convert_pct()


# Coût des logements ------------------------------------------------------

housing_owner_CSD_cost_var <- (housing_owner_CSD_cost - housing_owner_CSD_cost_2016) / housing_owner_CSD_cost_2016
housing_owner_CSD_cost_var <- convert_pct(housing_owner_CSD_cost_var)
housing_owner_CSD_cost <- convert_number(housing_owner_CSD_cost)
housing_owner_CSD_cost_2016 <- convert_number(housing_owner_CSD_cost_2016)

housing_owner_PR_cost_var <- (housing_owner_PR_cost - housing_owner_PR_cost_2016) / housing_owner_PR_cost_2016
housing_owner_PR_cost_var <- convert_pct(housing_owner_PR_cost_var)
housing_owner_PR_cost <- convert_number(housing_owner_PR_cost)
housing_owner_PR_cost_2016 <- convert_number(housing_owner_PR_cost_2016)

# Map shelter cost par DA
labels <- c("900 $ - 1 200 $", "1 200 $ - 1 500 $", "1 500 $ - 1 800 $", "+ 1 800 $")
t <- add_bins(df = owner_tenant_sf,
              variable = "med_owner",
              breaks = c(-Inf, 1200, 1500, 1800, Inf),
              labels = labels
)
# t <- Reduce(rbind,
#             split(t, t$binned_variable, drop = FALSE) |>
#               lapply(\(x) {
#                 out <- tibble::tibble(x$binned_variable)
#                 out$geometry <- sf::st_union(x)
#                 sf::st_as_sf(out, crs = 4326)[1, ]
#               })
# ) |> sf::st_as_sf()
names(t)[1] <- "binned_variable"

housing_median_cost_plot <-
  ggplot(data = t) +
  gg_cc_tiles +
  geom_sf(aes(fill = binned_variable), color = "transparent", lwd = 0) +
  scale_fill_manual(
    values = curbcut_colors$left_5$fill[2:5],
    name = element_blank(),
    breaks = levels(t$binned_variable),
    labels = levels(t$binned_variable),
    guide = guide_legend(
      title.position = "top",
      label.position = "bottom",
      keywidth = 4,
      keyheight = 0.5
    )
  ) +
  gg_cc_theme

ggplot2::ggsave(filename = here::here("output/axe1/housing/housing_median_cost_plot.pdf"),
                plot = housing_median_rent_plot, width = 6.5, height = 4)

housing_cost_med_table <-
  gt(owner_tenant[1:6, c(1,3)]) |> 
  # Appliquer une mise en couleur sur les colonnes médianes
  data_color(
    columns = 2,
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = NULL
    )
  ) |> 
  fmt(columns = 2, fns = \(x) paste(convert_number_tens(x), "$")) |> 
  # Ajouter des bordures en haut de chaque groupe de ligne
  tab_style(
    style = cell_borders(
      sides = c("top"),
      color = "white",
      weight = px(10)
    ),
    locations = cells_row_groups()
  ) |> 
  # Appliquer le style de la police à toute la table
  tab_style(
    style = cell_text(
      font = "KMR Apparat Regular"
    ),
    locations = cells_body()
  ) |> 
  tab_style(
    style = cell_text(
      font = "KMR Apparat Regular"
    ),
    locations = cells_column_labels()
  ) |> 
  # Options générales pour la table
  tab_options(
    table.font.size = 12,
    row_group.font.size = 12,
    table.width = px(6 * 96)
  )

gtsave(housing_cost_med_table, "output/axe1/housing/housing_cost_med_table.png", zoom = 3)


housing_cost_med_table_var <-
  gt(owner_tenant_sf_var_table[c(1, 3)]) |> 
  # Appliquer une mise en couleur sur les colonnes médianes
  data_color(
    columns = 2,
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = NULL
    )
  ) |> 
  fmt(columns = 2, fns = convert_pct) |> 
  # Adding the row group for "Secteur"
  tab_row_group(
    label = "Secteur",
    rows = 3:nrow(owner_tenant_sf_var_table)
  ) |>
  # Adding the row group for "Région"
  tab_row_group(
    label = "Région",
    rows = 1:2
  ) |>
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = 2
    )
  ) |>
  tab_style(
    style = cell_borders(
      sides = c("top"),
      color = "white",
      weight = px(10)
    ),
    locations = cells_row_groups()
  ) |>
  # Apply font style to the whole table
  tab_style(
    style = cell_text(
      font = "KMR Apparat Regular"
    ),
    locations = cells_body()
  ) |>
  tab_style(
    style = cell_text(
      font = "KMR Apparat Regular"
    ),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_text(
      font = "KMR Apparat Regular"
    ),
    locations = cells_row_groups()
  ) |>
  tab_style(
    style = cell_fill(color = "#F0F0F0"),
    locations = cells_row_groups()
  ) |> 
  tab_options(
    table.font.size = 12,
    row_group.font.size = 12,
    table.width = px(6 * 96)
  )

gtsave(housing_cost_med_table_var, "output/axe1/housing/housing_cost_med_table_var.png", zoom = 3)

losd <- "Laval-Ouest, Sainte-Dorothée, Laval-sur-le-Lac"
housing_cost_losd <- owner_tenant_sf_var$med_owner[owner_tenant_sf_var == losd]
housing_cost_losd <- convert_number_tens(housing_cost_losd)
housing_cost_losd_2016 <- owner_tenant_sf_var$med_owner_2016[owner_tenant_sf_var == losd]
housing_cost_losd_2016 <- convert_number_tens(housing_cost_losd_2016)
housing_cost_var_losd <- owner_tenant_sf_var_table$`Augmentation des frais de logements mensuels médians (propriétaires) (2016 - 2021)`[
  owner_tenant_sf_var_table$` ` == losd
] |> convert_pct()

lrpv <- "Laval-des-Rapides, Pont-Viau"
housing_cost_lrpv <- owner_tenant_sf_var$med_owner[owner_tenant_sf_var == lrpv]
housing_cost_lrpv <- convert_number_tens(housing_cost_lrpv)
housing_cost_lrpv_2016 <- owner_tenant_sf_var$med_owner_2016[owner_tenant_sf_var == lrpv]
housing_cost_lrpv_2016 <- convert_number_tens(housing_cost_lrpv_2016)
housing_cost_var_lrpv <- owner_tenant_sf_var_table$`Augmentation des frais de logements mensuels médians (propriétaires) (2016 - 2021)`[
  owner_tenant_sf_var_table$` ` == lrpv
] |> convert_pct()

srfb <- "Vimont, Auteuil"
housing_cost_srfb <- owner_tenant_sf_var$med_owner[owner_tenant_sf_var == srfb]
housing_cost_srfb <- convert_number_tens(housing_cost_srfb)
housing_cost_srfb_2016 <- owner_tenant_sf_var$med_owner_2016[owner_tenant_sf_var == srfb]
housing_cost_srfb_2016 <- convert_number_tens(housing_cost_srfb_2016)
housing_cost_var_srfb <- owner_tenant_sf_var_table$`Augmentation des frais de logements mensuels médians (propriétaires) (2016 - 2021)`[
  owner_tenant_sf_var_table$` ` == srfb
] |> convert_pct()





# Locataires et fluctuations du marché ------------------------------------

housing_tenant_CSD_rent_var <- (housing_tenant_CSD_rent - housing_tenant_CSD_rent_2016) / housing_tenant_CSD_rent_2016
housing_tenant_CSD_rent_var <- convert_pct(housing_tenant_CSD_rent_var)

housing_tenant_CSD_rent <- convert_number(housing_tenant_CSD_rent)



# Mise en chantier et achèvement ------------------------------------------

# Starts
starts_lvl <- get_cmhc(survey = "Scss", series = "Starts", dimension = "Intended Market",
                       breakdown = "Historical Time Periods", geo_uid = 2465005, year = 2009) |> 
  mutate(Date = dmy(paste0("01 ", DateString)), Year = as.factor(year(Date)),
         `Marché visé` = `Intended Market`) |> 
  select(Year, `Marché visé`, Value) |> 
  group_by(Year, `Marché visé`) |> 
  summarize(Units = sum(Value), .groups = "drop") |> 
  filter(`Marché visé` != "Unknown", `Marché visé` != "Co-Op",
         Year != "2024", Year != "2009") |> 
  mutate(`Marché visé` = case_when(`Marché visé` == "All" ~ "Total",
                                   `Marché visé` == "Homeowner" ~ "Propriétaire-occupant",
                                   `Marché visé` == "Rental" ~ "Locatif",
                                   `Marché visé` == "Condo" ~ "Copropriété"
                                   )) |> 
  mutate(`Marché visé` = factor(`Marché visé`, levels = c("Total", "Propriétaire-occupant", "Locatif",
                                                                  "Copropriété")))

housing_starts <- 
  ggplot(starts_lvl, aes(x = Year, y = `Units`, group = `Marché visé`, color = `Marché visé`)) +
  geom_line(aes(size = `Marché visé`, alpha = `Marché visé`)) +
  labs(title = element_blank(),
       x = element_blank(),
       y = "Nombre de logements") +
  scale_color_manual(values = c("Total" = color_theme("browndemographics"),
                                "Propriétaire-occupant" = color_theme("redhousing"),
                                "Copropriété" = color_theme("purpletransport"),
                                "Locatif" = color_theme("greenurbanlife"))) +
  scale_size_manual(values = c("Total" = 1.25, "Propriétaire-occupant" = 1.75, 
                               "Copropriété" = 1.75, "Locatif" = 1.75)) +
  scale_alpha_manual(values = c("Total" = 0.25, "Propriétaire-occupant" = 0.9, 
                                "Copropriété" = 0.9, "Locatif" = 0.9)) +
  scale_y_continuous(label = convert_number) +
  gg_cc_theme_no_sf +
  theme(legend.title.position = "top")

ggplot2::ggsave(filename = here::here("output/axe1/housing/housing_starts.pdf"), 
                plot = housing_starts, width = 9, height = 3, bg = "transparent")

# Completions
completions_lvl <- get_cmhc(survey = "Scss", series = "Completions", dimension = "Intended Market",
                            breakdown = "Historical Time Periods", geo_uid = 2465005, year = 2009,
                            frequency = "Annual") |> 
  mutate(Date = dmy(paste0("01 ", DateString)), Year = as.factor(year(Date)),
         `Marché visé` = `Intended Market`) |> 
  select(Year, `Marché visé`, Value) |> 
  group_by(Year, `Marché visé`) |> 
  summarize(Units = sum(Value), .groups = "drop") |> 
  filter(`Marché visé` != "Unknown", `Marché visé` != "Co-Op",
         Year != "2024", Year != "2009") |> 
  mutate(`Marché visé` = case_when(`Marché visé` == "All" ~ "Total",
                                   `Marché visé` == "Homeowner" ~ "Propriétaire-occupant",
                                   `Marché visé` == "Rental" ~ "Locatif",
                                   `Marché visé` == "Condo" ~ "Copropriété"
  )) |> 
  mutate(`Marché visé` = factor(`Marché visé`, levels = c("Total", "Propriétaire-occupant", "Locatif",
                                                          "Copropriété")))

housing_completions <- 
  ggplot(completions_lvl, aes(x = Year, y = `Units`, group = `Marché visé`, color = `Marché visé`)) +
  geom_line(aes(size = `Marché visé`, alpha = `Marché visé`)) +
  labs(title = element_blank(),
       x = element_blank(),
       y = "Nombre de logements") +
  scale_color_manual(values = c("Total" = color_theme("browndemographics"),
                                "Propriétaire-occupant" = color_theme("redhousing"),
                                "Copropriété" = color_theme("purpletransport"),
                                "Locatif" = color_theme("greenurbanlife"))) +
  scale_size_manual(values = c("Total" = 1.25, "Propriétaire-occupant" = 1.75, 
                               "Copropriété" = 1.75, "Locatif" = 1.75)) +
  scale_alpha_manual(values = c("Total" = 0.25, "Propriétaire-occupant" = 0.9, 
                                "Copropriété" = 0.9, "Locatif" = 0.9)) +
  scale_y_continuous(label = convert_number) +
  gg_cc_theme_no_sf +
  theme(legend.title.position = "top")

ggplot2::ggsave(filename = here::here("output/axe1/housing/housing_completions.pdf"), 
                plot = housing_completions, width = 9, height = 3, bg = "transparent")


# Ratio
names(starts_lvl)[3] <- "starts_units"
names(completions_lvl)[3] <- "completions_units"

starts_completions <- left_join(starts_lvl, completions_lvl, by = c("Year", "Marché visé"))
starts_completions$ratio <- starts_completions$starts_units / starts_completions$completions_units

housing_starts_completions_ratio <- 
ggplot(starts_completions[starts_completions$`Marché visé` != "Total", ], 
       aes(x = Year, y = `ratio`, group = `Marché visé`, color = `Marché visé`)) +
  geom_line(aes(size = `Marché visé`, alpha = `Marché visé`)) +
  labs(title = element_blank(),
       x = element_blank(),
       y = "Nombre de logements") +
  scale_color_manual(values = c("Total" = color_theme("browndemographics"),
                                "Propriétaire-occupant" = color_theme("redhousing"),
                                "Copropriété" = color_theme("purpletransport"),
                                "Locatif" = color_theme("greenurbanlife"))) +
  scale_size_manual(values = c("Total" = 1.25, "Propriétaire-occupant" = 1.75, 
                               "Copropriété" = 1.75, "Locatif" = 1.75)) +
  scale_alpha_manual(values = c("Total" = 0.25, "Propriétaire-occupant" = 0.9, 
                                "Copropriété" = 0.9, "Locatif" = 0.9)) +
  scale_y_continuous(label = convert_number) +
  gg_cc_theme_no_sf +
  theme(legend.title.position = "top")

ggplot2::ggsave(filename = here::here("output/axe1/housing/housing_starts_completions_ratio.pdf"), 
                plot = housing_starts_completions_ratio, width = 9, height = 3, bg = "transparent")


# Affordability 30% -------------------------------------------------------
#Grabbing vectors for total, owner, and tenants spending >30% on shelter costs for 2001-2021
aff_21v <- c("total" = "v_CA21_4288", "total_30" = "v_CA21_4290",
             "owner_30" = "v_CA21_4307", owner_total = "v_CA21_4305",
             "tenant_30" = "v_CA21_4315", tenant_total = "v_CA21_4313")
aff_16v <- c("total" = "v_CA16_4886", "total_30" = "v_CA16_4888",
             "owner_30" = "v_CA16_4892", owner_total = "v_CA16_4890",
             "tenant_30" = "v_CA16_4899", tenant_total = "v_CA16_4897")
aff_11v <- c("total" = "v_CA11N_2277", "total_30" = "v_CA11N_2279",
             "owner_30" = "v_CA11N_2283", "tenant_30" = "v_CA11N_2290")

#Creating a census grabber function for years 2011-2021 and cleans it up
aff_census_1121 <- function(dset, vectors, cyear){
  get_census(dataset = dset,
             regions = list(CSD = 2465005),
             level = "CSD",
             vectors = vectors) |> 
    mutate(Year = cyear,
           total_30 = total_30 / total) |> 
    transmute(Year, total_30, owner_30 = owner_30 / 100, 
              tenant_30 = tenant_30 / 100)
}

#Grabbing data for 2011-2021
aff_21 <- aff_census_1121("CA21", aff_21v, "2021")
aff_16 <- aff_census_1121("CA16", aff_16v, "2016")
aff_11 <- aff_census_1121("CA11", aff_11v, "2011")

housing_te <- convert_pct(aff_21$total_30)
housing_te_tenant <- convert_pct(aff_21$tenant_30)
housing_te_owner <- convert_pct(aff_21$owner_30)
housing_te_2016 <- convert_pct(aff_16$total_30)
housing_te_CMA <- get_census(dataset = "CA21",
           regions = list(CMA = 24462),
           level = "CMA",
           vectors = aff_21v) |> 
  mutate(Year = 2021,
         total_30 = total_30 / total) |> 
  transmute(Year, total_30, owner_30 = owner_30 / 100, 
            tenant_30 = tenant_30 / 100) |> 
  pull(total_30) |> convert_pct()


# Tableau des taux d'efforts

taux_efforts <- mapply(\(dataset, year, vectors) {
  
  taux_efforts <- get_census(dataset = dataset,
                             regions = list(CSD = 2465005),
                             level = "DA",
                             vectors = vectors,
                             geo_format = "sf") |> 
    mutate(Year = year,
           total_30 = total_30 / total) |> 
    mutate(owner_30 = owner_30 / 100, 
           tenant_30 = tenant_30 / 100)
  
  # Prendre par secteur
  z <- sf::st_intersects(sf::st_centroid(taux_efforts), laval_sectors)
  taux_efforts$secteur <- sapply(z, \(x) {
    if (length(x) == 0) return(NA)
    laval_sectors$name[x]
  })
  taux_efforts <-
    taux_efforts |>
    group_by(secteur) |>
    summarize(total_30 = weighted_mean(total_30, total, na.rm = TRUE),
              owner_30 = weighted_mean(owner_30, owner_total, na.rm = TRUE),
              tenant_30 = weighted_mean(tenant_30, tenant_total, na.rm = TRUE))
  taux_efforts <- taux_efforts[1:6, ]
  taux_efforts <- sf::st_drop_geometry(taux_efforts)
  
  
  # Province et tout Laval
  taux_efforts_PR <- get_census(dataset = dataset,
                                regions = list(PR = 24),
                                level = "PR",
                                vectors = vectors) |> 
    mutate(Year = year,
           total_30 = total_30 / total) |> 
    transmute(secteur = "Ensemble du Québec",
              owner_30 = owner_30 / 100, 
              tenant_30 = tenant_30 / 100,
              total_30)
  taux_efforts_CSD <- get_census(dataset = dataset,
                                 regions = list(CSD = 2465005),
                                 level = "CSD",
                                 vectors = vectors) |> 
    mutate(Year = year,
           total_30 = total_30 / total) |> 
    transmute(secteur = "Ville de Laval",
              owner_30 = owner_30 / 100, 
              tenant_30 = tenant_30 / 100,
              total_30)
  taux_efforts <- rbind(taux_efforts_PR, taux_efforts_CSD, taux_efforts)
  taux_efforts
}, c("CA16", "CA21"), c("2016", "2021"), list(aff_16v, aff_21v), SIMPLIFY = FALSE)

taux_efforts$variation <- data.frame(secteur = taux_efforts$CA21$secteur)
taux_efforts$variation$owner_30 <- (taux_efforts$CA21$owner_30 - taux_efforts$CA16$owner_30) / taux_efforts$CA16$owner_30
taux_efforts$variation$tenant_30 <- (taux_efforts$CA21$tenant_30 - taux_efforts$CA16$tenant_30) / taux_efforts$CA16$tenant_30
taux_efforts$variation$total_30 <- (taux_efforts$CA21$total_30 - taux_efforts$CA16$total_30) / taux_efforts$CA16$total_30

taux_efforts$CA16$year <- "2016"
taux_efforts$CA21$year <- "2021"
taux_efforts$variation$year <- "Changement 2016-2021"

taux_efforts <- Reduce(rbind, taux_efforts[c("CA21")])

names(taux_efforts) <- c(" ", "Ménages propriétaires", "Ménages locataires", "Tous les ménages", "Année")
taux_efforts <- pivot_wider(taux_efforts, names_from = Année, 
                            values_from = c(`Ménages propriétaires`, `Ménages locataires`, 
                                            `Tous les ménages`))
taux_efforts <- taux_efforts[c(1,4,3,2)]

taux_efforts_table <- 
gt(taux_efforts) |> 
  # Appliquer une mise en couleur sur les colonnes médianes
  data_color(
    columns = 2:ncol(taux_efforts),
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = NULL
    ), 
  ) |> 
  fmt(columns = 2:ncol(taux_efforts), fns = convert_pct) |> 
  # tab_spanner(
  #   label = "2021",
  #   columns = c(`Ménages propriétaires_2021`, `Ménages locataires_2021`, 
  #                  `Tous les ménages_2021`)
  # ) |> 
  # tab_spanner(
  #   label = "Changement 2016-2021",
  #   columns = c(`Ménages propriétaires_Changement 2016-2021`, 
  #               `Ménages locataires_Changement 2016-2021`, 
  #                  `Tous les ménages_Changement 2016-2021`)
  # ) |> 
  cols_label(
    `Ménages propriétaires_2021` = "Ménages propriétaires",
    `Ménages locataires_2021` = "Ménages locataires",
    `Tous les ménages_2021` = "Tous les ménages"#,
    # `Ménages propriétaires_Changement 2016-2021` = "Ménages propriétaires",
    # `Ménages locataires_Changement 2016-2021` = "Ménages locataires",
    # `Tous les ménages_Changement 2016-2021` = "Tous les ménages"
  ) |>

  # Adding the row group for "Secteur"
  tab_row_group(
    label = "Secteur",
    rows = 3:nrow(taux_efforts)
  ) |>
  # Adding the row group for "Région"
  tab_row_group(
    label = "Région",
    rows = 1:2
  ) |>
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = 2
    )
  ) |>
  tab_style(
    style = cell_borders(
      sides = c("top"),
      color = "white",
      weight = px(10)
    ),
    locations = cells_row_groups()
  ) |>
  # Apply font style to the whole table
  tab_style(
    style = cell_text(
      font = "KMR Apparat Regular"
    ),
    locations = cells_body()
  ) |>
  tab_style(
    style = cell_text(
      font = "KMR Apparat Regular"
    ),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_text(
      font = "KMR Apparat Regular"
    ),
    locations = cells_row_groups()
  ) |>
  tab_style(
    style = cell_fill(color = "#F0F0F0"),
    locations = cells_row_groups()
  ) |> 
  tab_options(
    table.font.size = 12,
    row_group.font.size = 12,
    table.width = px(6 * 96)
  )

gtsave(taux_efforts_table, "output/axe1/housing/taux_efforts_table.png", zoom = 3)


rent_2021 <- avg_rent_annual$Value[avg_rent_annual$Geography == "Laval" & avg_rent_annual$Year == 2021]
rent_2022 <- avg_rent_annual$Value[avg_rent_annual$Geography == "Laval" & avg_rent_annual$Year == 2022]
rent_2023 <- avg_rent_annual$Value[avg_rent_annual$Geography == "Laval" & avg_rent_annual$Year == 2023]

rent_inc_2021_2023 <- convert_pct((rent_2023 - rent_2021) / rent_2021)
rent_inc_2021_2022 <- convert_pct((rent_2022 - rent_2021) / rent_2021)
rent_inc_2022_2023 <- convert_pct((rent_2023 - rent_2022) / rent_2022)


# Logement de taille inadéquat --------------------------------------------

acceptable_housing <- mapply(\(location, file) {
  acceptable_housing <- read.csv(file, sep = ";") |> 
    transmute(acceptability = Logement.acceptable..9., 
              tenure_status = Mode.d.occupation..4.,
              value = VALEUR) |> 
    tibble::as_tibble()
  acceptable_housing <- 
    acceptable_housing[!grepl("Logement fourni par", acceptable_housing$tenure_status), ]

  # Filter and sum the percentages per tenure_status for abordabilité
  abordabilite <- acceptable_housing %>% 
    filter(acceptability %in% c("Inférieur au seuil d'abordabilité seulement", 
                                "Inférieur aux seuils d'abordabilité et de taille convenable seulement",
                                "Inférieur aux seuils d'abordabilité et de qualité convenable seulement",
                                "Inférieur aux seuils d'abordabilité, de taille et de qualité convenable")) %>%
    group_by(tenure_status) %>%
    summarise(acceptability = "Inférieur au seuil d'abordabilité",
              value = sum(value))
  
  # Filter and sum the percentages per tenure_status for taille
  taille <- acceptable_housing %>% 
    filter(acceptability %in% c("Inférieur au seuil de taille convenable seulement", 
                                "Inférieur aux seuils d'abordabilité et de taille convenable seulement",
                                "Inférieur aux seuils de taille et qualité convenable", 
                                "Inférieur aux seuils d'abordabilité, de taille et de qualité convenable")) %>%
    group_by(tenure_status) %>%
    summarise(acceptability = "Inférieur au seuil de taille convenable",
              value = sum(value))
  
  # Filter and sum the percentages per tenure_status for qualité
  qualite <- acceptable_housing %>% 
    filter(acceptability %in% c("Inférieur au seuil de qualité convenable seulement", 
                                "Inférieur aux seuils d'abordabilité et de qualité convenable seulement", 
                                "Inférieur aux seuils de taille et qualité convenable", 
                                "Inférieur aux seuils d'abordabilité, de taille et de qualité convenable")) %>%
    group_by(tenure_status) %>%
    summarise(acceptability = "Inférieur au seuil de qualité convenable",
              value = sum(value))
  
  acceptable_housing_all <- rbind(abordabilite, taille, qualite)[c(2,1,3)]
  acceptable_housing_all <- rbind(acceptable_housing_all, acceptable_housing[acceptable_housing$acceptability == "Acceptable", ])
  
  # Create a lookup table for the total values by tenure status
  totals <- acceptable_housing$value[1:3]
  names(totals) <- acceptable_housing$tenure_status[1:3]
  
  # Calculate percentage based on the corresponding total value
  acceptable_housing_all$percentage <- mapply(function(value, tenure_status) {
    total_value <- totals[tenure_status]
    (value / total_value)
  }, acceptable_housing_all$value, acceptable_housing_all$tenure_status)
  
  # Exclude the first three rows for the display purpose
  acceptable_housing_all <- acceptable_housing_all[names(acceptable_housing_all) != "value"]
  
  acceptable_housing_all <- 
    pivot_wider(acceptable_housing_all, names_from = tenure_status, values_from = percentage)
  names(acceptable_housing_all) <- c(c(" ", "Ménages locataires", "Ménages propriétaires", "Tous les ménages", "location"))
  acceptable_housing_all$location <- location
  acceptable_housing_all
}, c("Laval", "Ensemble du Québec"), 
c("data/axe1/9810024601_donneesselectionnees.csv", 
  "data/axe1/9810024601_donneesselectionnees_QC.csv"), SIMPLIFY = FALSE)

acceptable_housing <- Reduce(rbind, acceptable_housing)
acceptable_housing <- pivot_wider(acceptable_housing, names_from = location, 
                                  values_from = c(`Ménages propriétaires`, `Ménages locataires`, 
                                                  `Tous les ménages`))
acceptable_housing_table <-
gt(acceptable_housing) |> 
  fmt(columns = 2:ncol(acceptable_housing), fns = convert_pct) |> 
  tab_spanner(
    label = "Laval",
    columns = c(`Ménages propriétaires_Laval`, `Ménages locataires_Laval`,
                `Tous les ménages_Laval`)
  ) |>
  tab_spanner(
    label = "Ensemble du Québec",
    columns = c(`Ménages propriétaires_Ensemble du Québec`,
                `Ménages locataires_Ensemble du Québec`,
                `Tous les ménages_Ensemble du Québec`)
  ) |>
  cols_label(
    `Ménages propriétaires_Laval` = "Ménages propriétaires",
    `Ménages locataires_Laval` = "Ménages locataires",
    `Tous les ménages_Laval` = "Tous les ménages",
    `Ménages propriétaires_Ensemble du Québec` = "Ménages propriétaires",
    `Ménages locataires_Ensemble du Québec` = "Ménages locataires",
    `Tous les ménages_Ensemble du Québec` = "Tous les ménages"
  ) |>
  # Add padding to the fourth row to move it further down
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "transparent",
      weight = px(10)  # Adjust this value to move the row further down
    ),
    locations = cells_body(
      rows = 4
    )
  ) |> 
  # Apply font style to the whole table
  tab_style(
    style = cell_text(
      font = "KMR Apparat Regular"
    ),
    locations = cells_body()
  ) |>
  tab_style(
    style = cell_text(
      font = "KMR Apparat Regular"
    ),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_text(
      font = "KMR Apparat Regular"
    ),
    locations = cells_row_groups()
  ) |>
  tab_style(
    style = cell_fill(color = "#F0F0F0"),
    locations = cells_row_groups()
  ) |> 
  tab_options(
    table.font.size = 12,
    row_group.font.size = 12,
    table.width = px(6 * 96)
  )

gtsave(acceptable_housing_table, "output/axe1/housing/acceptable_housing_table.png", zoom = 3)

taille <- "Inférieur au seuil de taille convenable"
taille_all <- convert_pct(acceptable_housing$`Tous les ménages_Laval`[acceptable_housing$` ` == taille])
taille_owner <- convert_pct(acceptable_housing$`Ménages propriétaires_Laval`[acceptable_housing$` ` == taille])
taille_tenant <- convert_pct(acceptable_housing$`Ménages locataires_Laval`[acceptable_housing$` ` == taille])

acceptable <- "Acceptable"
acceptable_all <- convert_pct(acceptable_housing$`Tous les ménages_Laval`[acceptable_housing$` ` == acceptable])
acceptable_owner <- convert_pct(acceptable_housing$`Ménages propriétaires_Laval`[acceptable_housing$` ` == acceptable])
acceptable_tenant <- convert_pct(acceptable_housing$`Ménages locataires_Laval`[acceptable_housing$` ` == acceptable])


# Core Housing Need --------------------------------------------

core_need <- read.csv("data/axe1/9810024601_donneesselectionnees_core.csv", sep = ";")[1:3,] |> 
  transmute(tenure_status = Mode.d.occupation..4.,
            value = VALEUR) |> 
  tibble::as_tibble()

core_need$percentage <- core_need$value / {
  read.csv("data/axe1/9810024601_donneesselectionnees.csv", sep = ";")[1:3,] |> 
    transmute(tenure_status = Mode.d.occupation..4.,
              value = VALEUR) |> 
    tibble::as_tibble() |> 
    pull(value)
}

core_need_QC <- read.csv("data/axe1/9810024601_donneesselectionnees_core_QC.csv", sep = ";")[1:3,] |> 
  transmute(tenure_status = Mode.d.occupation..4.,
            value = VALEUR) |> 
  tibble::as_tibble()

core_need_QC$percentage <- core_need_QC$value / {
  read.csv("data/axe1/9810024601_donneesselectionnees_QC.csv", sep = ";")[1:3,] |> 
    transmute(tenure_status = Mode.d.occupation..4.,
              value = VALEUR) |> 
    tibble::as_tibble() |> 
    pull(value)
}

core_need_tenant <- convert_pct(core_need$percentage[core_need$tenure_status == "Locataire"])
core_need_tenant_QC <- convert_pct(core_need_QC$percentage[core_need_QC$tenure_status == "Locataire"])


core_need_time <- get_cmhc(survey = "Core Housing Need", 
                           series = "Housing Standards",
                           dimension = "Households in Core Housing Need", 
                           breakdown = "Historical Time Periods",
                           geo_uid = "2465005") |> 
  transmute(region = "Ville de Laval", year = DateString, type = `Households in Core Housing Need`,
            value = Value)
core_need_time_QC <- get_cmhc(survey = "Core Housing Need", 
                              series = "Housing Standards",
                              dimension = "Households in Core Housing Need", 
                              breakdown = "Historical Time Periods",
                              geo_uid = "24") |> 
  transmute(region = "Ensemble du Québec", year = DateString, type = `Households in Core Housing Need`,
            value = Value)

core_need_time_total <- get_cmhc(survey = "Core Housing Need", 
                           series = "Housing Standards",
                           dimension = "Households Tested For Core Housing Need", 
                           breakdown = "Historical Time Periods",
                           geo_uid = "2465005") |> 
  transmute(region = "Ville de Laval", year = DateString, type = `Households Tested For Core Housing Need`,
            value = Value)
core_need_time_total_QC <- get_cmhc(survey = "Core Housing Need", 
                              series = "Housing Standards",
                              dimension = "Households Tested For Core Housing Need", 
                              breakdown = "Historical Time Periods",
                              geo_uid = "24") |> 
  transmute(region = "Ensemble du Québec", year = DateString, type = `Households Tested For Core Housing Need`,
            value = Value)

core_need_time <- core_need_time[core_need_time$type == "Total", ]
core_need_time_QC <- core_need_time_QC[core_need_time_QC$type == "Total", ]

core_need_time_total <- core_need_time_total[core_need_time_total$type == "Total", ]
core_need_time_total_QC <- core_need_time_total_QC[core_need_time_total_QC$type == "Total", ]

core_need_time$percentage <- core_need_time$value / core_need_time_total$value
core_need_time_total_QC$percentage <- core_need_time_QC$value / core_need_time_total_QC$value

core_need_time <- rbind(core_need_time, core_need_time_total_QC)
core_need_time <- core_need_time[names(core_need_time) != "type"]






names(core_need_time) <- c("Région", "Année", "Besoins impérieux (n)", "Besoins impérieux (%)")
core_need_wide <- core_need_time %>%
  pivot_wider(names_from = Région, 
              values_from = c(`Besoins impérieux (n)`, `Besoins impérieux (%)`))
core_need_wide <- core_need_wide[rev(seq_along(core_need_wide$Année)), ]

core_need_table <-
  gt(core_need_wide) |> 
  cols_label(
    Année = "Année",
    `Besoins impérieux (n)_Ville de Laval` = "Besoins impérieux (n)",
    `Besoins impérieux (%)_Ville de Laval` = "Besoins impérieux (%)",
    `Besoins impérieux (n)_Ensemble du Québec` = "Besoins impérieux (n)",
    `Besoins impérieux (%)_Ensemble du Québec` = "Besoins impérieux (%)"
  ) %>%
  tab_spanner(
    label = "Ville de Laval",
    columns = c(`Besoins impérieux (n)_Ville de Laval`, `Besoins impérieux (%)_Ville de Laval`)
  ) %>%
  tab_spanner(
    label = "Ensemble du Québec",
    columns = c(`Besoins impérieux (n)_Ensemble du Québec`, `Besoins impérieux (%)_Ensemble du Québec`)
  ) |> 
  fmt(columns = 4:5, fns = convert_pct) |> 
  fmt(columns = 2:3, fns = convert_number) |> 
  data_color(
    columns = 4:5,
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = NULL
    ), 
  ) |> 
  tab_style(
    style = cell_borders(
      sides = c("top"),
      color = "white",
      weight = px(10)
    ),
    locations = cells_row_groups()
  ) |>
  # Apply font style to the whole table
  tab_style(
    style = cell_text(
      font = "KMR Apparat Regular"
    ),
    locations = cells_body()
  ) |>
  tab_style(
    style = cell_text(
      font = "KMR Apparat Regular"
    ),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_text(
      font = "KMR Apparat Regular"
    ),
    locations = cells_row_groups()
  ) |>
  tab_style(
    style = cell_fill(color = "#F0F0F0"),
    locations = cells_row_groups()
  ) |> 
  tab_options(
      table.font.size = 12,
      row_group.font.size = 12,
      table.width = px(6 * 96)
  )

core_need_2006 <- core_need_time$`Besoins impérieux (%)`[core_need_time$Région == "Ville de Laval" &
                                                           core_need_time$Année == 2006] |> 
  convert_pct()
core_need_2021 <- core_need_time$`Besoins impérieux (%)`[core_need_time$Région == "Ville de Laval" &
                                                           core_need_time$Année == 2021] |> 
  convert_pct()



gtsave(core_need_table, "output/axe1/housing/core_need_table.png", zoom = 3)

get_census("CA21", regions = list(CSD = 2465005),
           level = "CSD",
           vectors = c(tenant = "v_CA21_4316", owner = "v_CA21_4308")
) |> View()



















qs::qsavem(housing_owner_2016, housing_owner_2021,
           logement_statutoccupation, housing_owner_2016, housing_owner_2021, housing_evol_owner_1621,
           housing_tenant_2016, housing_tenant_2021, housing_evol_tenant_1621,
           housing_prop_tenant_2021_CA, housing_prop_tenant_2016_CA, housing_prop_tenant_2001_CA,
           housing_prop_tenant_2021_QC, housing_prop_tenant_2016_QC, housing_prop_tenant_2001_QC,
           housing_loyermed_plot, housing_loyer_2023_QC, housing_loyer_var_QC, housing_loyer_2010_noinf,
           housing_loyer_2010, housing_loyer_var_noinf, housing_loyer_2023,
           housing_loyer_var, housing_loyer_2010_QC, lavalouest_loyer,
           chomedey_loyer, mobility_status_ldr, mobility_status_chomedey,
           mobility_status_CSD, housing_median_rent_plot, housing_loyer_med_table,
           housing_rent_losd_2016, housing_rent_losd, housing_rent_var_losd,
           housing_rent_var_srfb, housing_rent_srfb_2016, housing_rent_srfb,
           housing_rent_var_svpsf, housing_rent_svpsf_2016, housing_rent_svpsf,
           loyer_med_var_table, housing_owner_CSD_cost, housing_owner_CSD_cost_var,
           housing_owner_CSD_cost_2016, housing_owner_PR_cost_var, housing_owner_PR_cost_2016,
           housing_owner_PR_cost, housing_cost_losd, housing_cost_srfb,
           housing_median_rent_plot, housing_loyer_med_table, housing_cost_var_lrpv,
           housing_cost_lrpv_2016, housing_cost_lrpv, housing_cost_med_table,
           housing_tenant_CSD_rent_var, housing_tenant_CSD_rent, housing_cost_med_table_var,
           housing_starts, housing_completions, housing_starts_completions_ratio,
           housing_te, housing_te_tenant, housing_te_owner, housing_te_2016,
           housing_te_CMA, taux_efforts_table, rent_inc_2021_2023,
           rent_inc_2021_2022, rent_inc_2022_2023, taille_all, taille_owner,
           taille_tenant, acceptable_all, acceptable_owner, acceptable_tenant,
           core_need_table, core_need_2006, core_need_tenant,
           core_need_tenant_QC, core_need_2021, housing_owner_2016_pct,
           housing_owner_2021_pct, housing_owner_2001_pct,
           acceptable_housing_table, file = "data/axe1/housing.qsm")




