### Housing Updated ###################################################################
source("R/01_startup.R")
library(patchwork)
library(scales)
library(cmhc)

# Functions ---------------------------------------------------------------
#Laval census grabber - Occupation Status
pto_census <- function(datayear, pto_year, cyear){
  get_census(dataset = datayear,
             regions = list(CSD = 2465005),
             level = "CSD",
             vectors = pto_year) |> 
    mutate(Year = cyear) |> 
    select(-GeoUID, -Type, -`Region Name`, -`Area (sq km)`, -Population,
           -Dwellings, -Households, -CD_UID, -PR_UID, -CMA_UID)
  }

#Quebec census grabber - Occupation Status
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

#Grabbing values from the graph
housing_statut_fun <- function(occ, year) {
  x <- pto_graph$Proportion[pto_graph$Year == year & pto_graph$Type == occ]
  convert_pct(x)
}

#Housing Status Evolution
housing_statut_evol_fun <- function(year, type) {
  pto_graph$Households[pto_graph$Type == type & pto_graph$Year == year]
}

#Quebec evolution
housing_statut_prop_fun_QC <- function(year, type) {
  pto_graph_QC <- bind_rows(pto_21_QC, pto_16_QC, pto_11_QC, pto_06_QC, pto_01_QC) |> 
    pivot_longer(cols = -Year, names_to = "Type", values_to = "Households")
  out <- pto_graph_QC$Households[pto_graph_QC$Type == type & pto_graph_QC$Year == year] / 
    pto_graph_QC$Households[pto_graph_QC$Type == "total" & pto_graph_QC$Year == year]
  convert_pct(out)
}

#Affordability
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

# Consumer Price Index ----------------------------------------------------
years <- 2010:2023

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

# Occupation Status -------------------------------------------------------
#Vectors for the housing status
pto_21v <- c("total" = "v_CA21_4288", "owner" = "v_CA21_4305", "tenant" = "v_CA21_4313")
pto_16v <- c("total" = "v_CA16_4886", "owner" = "v_CA16_4890", "tenant" = "v_CA16_4897")
pto_11v <- c("total" = "v_CA11N_2277", "owner" = "v_CA11N_2281", "tenant" = "v_CA11N_2288")
pto_06v <- c("total" = "v_CA06_2048", "owner" = "v_CA06_2053", "tenant" = "v_CA06_2049")
pto_01v <- c("owner" = "v_CA01_1670", "tenant" = "v_CA01_1666")

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

#Making the data usable to graph
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
  labs(y = "Nombre de ménages (n)",
       x = "Année") +
  scale_fill_manual(values = c("owner" = color_theme("redhousing"), "tenant" = color_theme("greenecology")),
                    labels = c("Propriétaire", "Locataire")) +
  scale_y_continuous(labels = convert_number) +
  gg_cc_theme_no_sf +
  theme(legend.title = element_blank())

# Graphique pour la proportion des ménages
proportion_logement_statutoccupation <- ggplot(pto_graph, aes(x = Year, y = Proportion, fill = Type)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = ProportionLabel, group = Type),
            vjust = 2, color = "black", size = 2.75) +
  labs(y = "Proportion des ménages (%)",
       x = "Année") +
  scale_fill_manual(values = c("owner" = color_theme("redhousing"), "tenant" = color_theme("greenecology")),
                    labels = c("Propriétaire", "Locataire")) +
  scale_y_continuous(labels = convert_pct) +
  gg_cc_theme_no_sf +
  theme(legend.title = element_blank())

logement_statutoccupation <- (g1 + proportion_logement_statutoccupation) + plot_layout(guides = 'collect') &
  theme(legend.position = "bottom")

ggsave(filename = here::here("output/axe1/housing/proportion_logement_statutoccupation.pdf"),
       plot = proportion_logement_statutoccupation, width = 6.5, height = 3.5)

ggsave(filename = here::here("output/axe1/housing/logement_statutoccupation.pdf"),
       plot = logement_statutoccupation, width = 6.5, height = 3.5)

# Graph Values ------------------------------------------------------------
housing_owner_2016_pct <- housing_statut_fun("owner", 2016)
housing_owner_2001_pct <- housing_statut_fun("owner", 2001)
housing_owner_2021_pct <- housing_statut_fun("owner", 2021)

owner_2016_2021 <- pto_graph |> 
  filter(Type == "owner") |> 
  filter(Year %in% c(2016, 2021)) |> 
  select(Proportion) |> 
  summarise(Difference = (Proportion[1] - Proportion[2]) * 100) |> 
  pull() |> 
  convert_number()

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

housing_prop_tenant_2021_QC <- housing_statut_prop_fun_QC(2021, "tenant")
housing_prop_tenant_2016_QC <- housing_statut_prop_fun_QC(2016, "tenant")
housing_prop_tenant_2001_QC <- housing_statut_prop_fun_QC(2001, "tenant")

# Median Monthly Costs ---------------------------------------------------------
#Median Monthly Costs Vectors
medcost_21v <- c("owner" = "v_CA21_4309", "tenant" = "v_CA21_4317")
medcost_16v <- c("owner" = "v_CA16_4893", "tenant" = "v_CA16_4900")
medcost_11v <- c("owner" = "v_CA11N_2284", "tenant" = "v_CA11N_2291")

#Grabbing median cost data for Laval
medcost_21 <- pto_census("CA21", medcost_21v, "2021")
medcost_16 <- pto_census("CA16", medcost_16v, "2016")
medcost_11 <- pto_census("CA11", medcost_11v, "2011") |> 
  select(-`NHS Non Return Rate`)

#Grabbing median cost data for Quebec
medcost_21_QC <- pto_census_QC("CA21", medcost_21v, "2021")
medcost_16_QC <- pto_census_QC("CA16", medcost_16v, "2016")
medcost_11_QC <- pto_census_QC("CA11", medcost_11v, "2011") |> 
  select(-`NHS Non Return Rate`)
medcost_graph_QC <- bind_rows(medcost_21_QC, medcost_16_QC, medcost_11_QC) |> 
  mutate(Region = "Quebec")

#Creating the median monthly costs graph
medcost_graph <- bind_rows(medcost_21, medcost_16, medcost_11) |> 
  mutate(Region = "Laval") |> 
  bind_rows(medcost_graph_QC) |> 
  pivot_longer(
    cols = c(owner, tenant),
    names_to = "Type",
    values_to = "Cost"
  )

#Bar Graph Chart Data
medcost_bar <- medcost_graph |>
  pivot_wider(names_from = Type, values_from = Cost) |>
  mutate(owner = owner - tenant) |>
  pivot_longer(cols = c(tenant, owner), names_to = "Type", values_to = "Cost") |>
  mutate(Region_Type = paste0(Region, "_", Type))

#Table Data
medrent_table_data <- medcost_graph |> 
  filter(Type == "tenant") |>
  select(-Type) |> 
  pivot_wider(names_from = Year,
              values_from = Cost) |> 
  rename("Région" = Region) |> 
  mutate("Loyer médian mensuel (2011)" = `2011`,
         "Loyer médian mensuel (2016)" = `2016`,
         "Loyer médian mensuel (2021)" = `2021`,
         "Évolution du loyer mensuel médian (2016 - 2021)" = (`2021`-`2016`)/`2016`) |> 
  select("Région","Loyer médian mensuel (2011)", "Loyer médian mensuel (2016)", 
         "Loyer médian mensuel (2021)",
         "Évolution du loyer mensuel médian (2016 - 2021)") |> 
  mutate(Région = ifelse(Région == "Quebec", "Ensemble du Québec", Région)) |> 
  mutate(Région = factor(Région, levels = c("Ensemble du Québec", setdiff(Région, "Ensemble du Québec")))) |> 
  arrange(Région)

medown_table_data <- medcost_graph |> 
  filter(Type == "owner") |>
  select(-Type) |> 
  pivot_wider(names_from = Year,
              values_from = Cost) |> 
  rename("Région" = Region) |> 
  mutate("Frais de logement mensuels médian (2011)" = `2011`,
         "Frais de logement mensuels médian (2016)" = `2016`,
         "Frais de logement mensuels médian (2021)" = `2021`,
         "Évolution des frais de logement mensuels médian (2016 - 2021)" = (`2021`-`2016`)/`2016`) |> 
  select("Région","Frais de logement mensuels médian (2011)", "Frais de logement mensuels médian (2016)", 
         "Frais de logement mensuels médian (2021)",
         "Évolution des frais de logement mensuels médian (2016 - 2021)") |> 
  mutate(Région = ifelse(Région == "Quebec", "Ensemble du Québec", Région)) |> 
  mutate(Région = factor(Région, levels = c("Ensemble du Québec", setdiff(Région, "Ensemble du Québec")))) |> 
  arrange(Région)
  

#Graphing median monthly costs
medcost_owner_graph <- ggplot(data = medcost_graph |> filter(Type == "owner"),
  aes(x = Year, y = Cost, color = interaction(Region, Type), group = interaction(Region, Type))) +
  geom_line(size = 1.5) +
  scale_color_manual(
    name = "Région et Type",
    values = c(
      "Laval.owner" = color_theme("greenecology"),
      "Quebec.owner" = color_theme("blueexplorer")
    ),
    labels = c(
      "Laval.owner" = "Laval",
      "Quebec.owner" = "Québec"
    )
  ) +
  scale_y_continuous(labels = convert_number) +
  labs(
    x = "",
    y = "Coût médian du logement pour\nles propriétaires ($)",
    color = NULL
  ) +
  gg_cc_theme_no_sf +
  theme(legend.title = element_blank(),
        text = element_text(size = 24),
        legend.text = element_text(size = 22),
        axis.title.y = element_text(lineheight = 0.4))

medcost_tenant_graph <- ggplot(data = medcost_graph |> filter(Type == "tenant"),
                               aes(x = Year, y = Cost, color = interaction(Region, Type), group = interaction(Region, Type))) +
  geom_line(size = 1.5) +
  scale_color_manual(
    name = "Région et Type",
    values = c(
      "Laval.tenant" = color_theme("greenecology"),
      "Quebec.tenant" = color_theme("blueexplorer")
    ),
    labels = c(
      "Laval.tenant" = "Laval",
      "Quebec.tenant" = "Québec"
    )
  ) +
  scale_y_continuous(labels = convert_number) +
  labs(
    x = "",
    y = "Loyer mensuels médians ($)",
    color = NULL
  ) +
  gg_cc_theme_no_sf +
  theme(legend.title = element_blank(),
        text = element_text(size = 24),
        legend.text = element_text(size = 22),
        axis.title.y = element_text(lineheight = 0.4))
  

# SBar Graph comparining the two
medcost_bar_graph <- ggplot(medcost_bar, aes(x = Region, y = Cost, fill = Region_Type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Year) +
  scale_fill_manual(
    values = c(
      "Laval_tenant" = color_theme("greenecology"),
      "Laval_owner" = "#B8D6BE",
      "Quebec_tenant" = "#6C83B5",
      "Quebec_owner" = color_theme("blueexplorer")
    ),
    labels = c(
      "Laval_tenant" = "Locataires (Laval)",
      "Laval_owner" = "Propriétaires (Laval)",
      "Quebec_tenant" = "Locataires (Québec)",
      "Quebec_owner" = "Propriétaires (Québec)"
    )
  ) +
  scale_y_continuous(labels = convert_number) +
  labs(
    x = NULL,
    y = "Coût mensuel médian du logement ($)",
    fill = "Type de ménage et région",
    title = NULL
  ) +
  gg_cc_theme_no_sf +
  theme(legend.title = element_blank(),
        text = element_text(size = 24),
        legend.text = element_text(size = 22),
        axis.title.y = element_text(lineheight = 0.4))

#Creating the Table
medrent_table <- gt(medrent_table_data) |>
  data_color(columns = 2:5, colors = col_numeric(palette = c("white",color_theme("purpletransport")), domain = NULL)) |> 
  fmt(columns = 2:4, fns = \(x) paste(convert_number_tens(x), "$")) |> 
  fmt(columns = 5, fns = convert_pct) |>
  tab_style(style = cell_borders(sides = c("top"),
                                 color = "white",
                                 weight = px(10)),
            locations = cells_row_groups()) |>
  tab_style(style = cell_text(font = "KMR Apparat"),
            locations = cells_body()) |>
  tab_style(style = cell_text(font = "KMR Apparat"),
            locations = cells_column_labels()) |>
  tab_style(style = cell_text(font = "KMR Apparat"),
            locations = cells_row_groups()) |>
  tab_style(style = cell_fill(color = "#F0F0F0"),
            locations = cells_row_groups()) |> 
  tab_options(table.font.size = 12,
              row_group.font.size = 12,
              table.width = px(6 * 96))

medown_table <- gt(medown_table_data) |>
  data_color(columns = 2:5, colors = col_numeric(palette = c("white",color_theme("purpletransport")), domain = NULL)) |> 
  fmt(columns = 2:4, fns = \(x) paste(convert_number_tens(x), "$")) |> 
  fmt(columns = 5, fns = convert_pct) |>
  tab_style(style = cell_borders(sides = c("top"),
                                 color = "white",
                                 weight = px(10)),
            locations = cells_row_groups()) |>
  tab_style(style = cell_text(font = "KMR Apparat"),
            locations = cells_body()) |>
  tab_style(style = cell_text(font = "KMR Apparat"),
            locations = cells_column_labels()) |>
  tab_style(style = cell_text(font = "KMR Apparat"),
            locations = cells_row_groups()) |>
  tab_style(style = cell_fill(color = "#F0F0F0"),
            locations = cells_row_groups()) |> 
  tab_options(table.font.size = 12,
              row_group.font.size = 12,
              table.width = px(6 * 96))

#Saving the files
ggsave(filename = here::here("output/axe1/housing/medcost_owner_graph.png"),
       plot = medcost_owner_graph, width = 6.5, height = 3.5, bg = "white")

ggsave(filename = here::here("output/axe1/housing/medcost_tenant_graph.png"),
       plot = medcost_tenant_graph, width = 6.5, height = 3.5, bg = "white")

ggsave(filename = here::here("output/axe1/housing/medcost_bar_graph.png"),
       plot = medcost_bar_graph, width = 6.5, height = 3.5, bg = "white")

gtsave(medrent_table, "output/axe1/housing/medrent_table.png", zoom = 3)

gtsave(medown_table, "output/axe1/housing/medown_table.png", zoom = 3)

# Average Monthly Costs ---------------------------------------------------------
#Average Monthly Costs Vectors
avgcost_21v <- c("owner" = "v_CA21_4310", "tenant" = "v_CA21_4318")
avgcost_16v <- c("owner" = "v_CA16_4894", "tenant" = "v_CA16_4901")
avgcost_11v <- c("owner" = "v_CA11N_2285", "tenant" = "v_CA11N_2292")
avgcost_06v <- c("owner" = "v_CA06_2055", "tenant" = "v_CA06_2050")
avgcost_01v <- c("owner" = "v_CA01_1671", "tenant" = "v_CA01_1667")

#Grabbing median cost data for Laval
avgcost_21 <- pto_census("CA21", avgcost_21v, "2021")
avgcost_16 <- pto_census("CA16", avgcost_16v, "2016")
avgcost_11 <- pto_census("CA11", avgcost_11v, "2011") |> 
  select(-`NHS Non Return Rate`)
avgcost_06 <- pto_census("CA06", avgcost_06v, "2006")
avgcost_01 <- pto_census("CA01", avgcost_01v, "2001")

#Grabbing median cost data for Quebec
avgcost_21_QC <- pto_census_QC("CA21", avgcost_21v, "2021")
avgcost_16_QC <- pto_census_QC("CA16", avgcost_16v, "2016")
avgcost_11_QC <- pto_census_QC("CA11", avgcost_11v, "2011") |> 
  select(-`NHS Non Return Rate`)
avgcost_06_QC <- pto_census_QC("CA06", avgcost_06v, "2006")
avgcost_01_QC <- pto_census_QC("CA01", avgcost_01v, "2001")
avgcost_graph_QC <- bind_rows(avgcost_21_QC, avgcost_16_QC, avgcost_11_QC,
                              avgcost_06_QC, avgcost_01_QC) |> 
  mutate(Region = "Quebec")

#Creating the median monthly costs graph
avgcost_graph <- bind_rows(avgcost_21, avgcost_16, avgcost_11,
                           avgcost_06, avgcost_01) |> 
  mutate(Region = "Laval") |> 
  bind_rows(avgcost_graph_QC) |> 
  pivot_longer(
    cols = c(owner, tenant),
    names_to = "Type",
    values_to = "Cost"
  )

#Bar Graph Chart Data
avgcost_bar <- avgcost_graph |>
  pivot_wider(names_from = Type, values_from = Cost) |>
  mutate(owner = owner - tenant) |>
  pivot_longer(cols = c(tenant, owner), names_to = "Type", values_to = "Cost") |>
  mutate(Region_Type = paste0(Region, "_", Type))

#Graphing median monthly costs
avgcost_owner_graph <- ggplot(data = avgcost_graph |> filter(Type == "owner"),
                              aes(x = Year, y = Cost, color = interaction(Region, Type), group = interaction(Region, Type))) +
  geom_line(size = 1.5) +
  scale_color_manual(
    name = "Région et Type",
    values = c(
      "Laval.owner" = color_theme("greenecology"),
      "Quebec.owner" = color_theme("blueexplorer")
    ),
    labels = c(
      "Laval.owner" = "Laval",
      "Quebec.owner" = "Québec"
    )
  ) +
  scale_y_continuous(labels = convert_number) +
  labs(
    x = "",
    y = "Coût moyen du logement pour\nles propriétaires ($)",
    color = NULL
  ) +
  gg_cc_theme_no_sf +
  theme(legend.title = element_blank(),
        text = element_text(size = 24),
        legend.text = element_text(size = 22),
        axis.title.y = element_text(lineheight = 0.4))

avgcost_tenant_graph <- ggplot(data = avgcost_graph |> filter(Type == "tenant"),
                               aes(x = Year, y = Cost, color = interaction(Region, Type), group = interaction(Region, Type))) +
  geom_line(size = 1.5) +
  scale_color_manual(
    name = "Région et Type",
    values = c(
      "Laval.tenant" = color_theme("greenecology"),
      "Quebec.tenant" = color_theme("blueexplorer")
    ),
    labels = c(
      "Laval.tenant" = "Laval",
      "Quebec.tenant" = "Québec"
    )
  ) +
  scale_y_continuous(labels = convert_number) +
  labs(
    x = "",
    y = "Loyer moyen médians ($)",
    color = NULL
  ) +
  gg_cc_theme_no_sf +
  theme(legend.title = element_blank(),
        text = element_text(size = 24),
        legend.text = element_text(size = 22))


# Bar Graph comparining the two
avgcost_bar_graph <- ggplot(avgcost_bar, aes(x = Region, y = Cost, fill = Region_Type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Year, nrow = 1) +
  scale_fill_manual(
    values = c(
      "Laval_tenant" = color_theme("greenecology"),
      "Laval_owner" = "#B8D6BE",
      "Quebec_tenant" = "#6C83B5",
      "Quebec_owner" = color_theme("blueexplorer")
    ),
    labels = c(
      "Laval_tenant" = "Locataires (Laval)",
      "Laval_owner" = "Propriétaires (Laval)",
      "Quebec_tenant" = "Locataires (Québec)",
      "Quebec_owner" = "Propriétaires (Québec)"
    )
  ) +
  scale_y_continuous(labels = convert_number) +
  labs(
    x = NULL,
    y = "Coût mensuel moyen du logement ($)",
    fill = "Type de ménage et région",
    title = NULL
  ) +
  gg_cc_theme_no_sf +
  theme(legend.title = element_blank(),
        text = element_text(size = 24),
        legend.text = element_text(size = 22),
        axis.title.y = element_text(lineheight = 0.4))

#Saving the files
ggsave(filename = here::here("output/axe1/housing/avgcost_owner_graph.png"),
       plot = avgcost_owner_graph, width = 6.5, height = 3.5, bg = "white")

ggsave(filename = here::here("output/axe1/housing/avgcost_tenant_graph.png"),
       plot = avgcost_tenant_graph, width = 6.5, height = 3.5, bg = "white")

ggsave(filename = here::here("output/axe1/housing/avgcost_bar_graph.png"),
       plot = avgcost_bar_graph, width = 6.5, height = 3.5, bg = "white")

# Median Costs Map ---------------------------------------------------------
#Grabbing census info
osc_21v <- c("med_owner" = "v_CA21_4309", 
             "med_tenant" = "v_CA21_4317",
             "avg_owner" = "v_CA21_4310")

owner_tenant_sf <- get_census(dataset = "CA21",
                              regions = list(CSD = 2465005),
                              level = "CT",
                              vectors = c(osc_21v, owner_hou = "v_CA21_4305",
                                          tenant_hou = "v_CA21_4313"),
                              geo_format = "sf")

# Mapping median costs per DA
medrent_bins <- add_bins(df = owner_tenant_sf,
              variable = "med_tenant",
              breaks = c(-Inf, 600, 900, 1200, Inf),
              labels = c("300 $ - 600 $", "600 $ - 900 $", "900 $ - 1 200 $", "+ 1 200 $"))

medown_bins <- add_bins(df = owner_tenant_sf,
              variable = "med_owner",
              breaks = c(-Inf, 1200, 1500, 1800, Inf),
              labels = c("900 $ - 1 200 $", "1 200 $ - 1 500 $", "1 500 $ - 1 800 $", "+ 1 800 $")
)

#Mapping the map
housing_median_rent_plot <-
  ggplot(data = medrent_bins) +
  gg_cc_tiles +
  geom_sf(aes(fill = binned_variable), color = "transparent", lwd = 0) +
  scale_fill_manual(
    values = curbcut_colors$left_5$fill[2:5],  # Include NA color first
    # na.value = curbcut_colors$left_5$fill[1],
    name = element_blank(),
    breaks = levels(medrent_bins$binned_variable),
    labels = levels(medrent_bins$binned_variable),
    guide = guide_legend(
      title.position = "top",
      label.position = "bottom",
      keywidth = 4,
      keyheight = 0.5
    )
  ) +
  gg_cc_theme

housing_median_own_plot <-
  ggplot(data = medown_bins) +
  gg_cc_tiles +
  geom_sf(aes(fill = binned_variable), color = "transparent", lwd = 0) +
  scale_fill_manual(
    values = curbcut_colors$left_5$fill[2:5],
    name = element_blank(),
    breaks = levels(medown_bins$binned_variable),
    labels = levels(medown_bins$binned_variable),
    guide = guide_legend(
      title.position = "top",
      label.position = "bottom",
      keywidth = 4,
      keyheight = 0.5
    )
  ) +
  gg_cc_theme

#Saving the map
ggplot2::ggsave(filename = here::here("output/axe1/housing/housing_median_rent_plot.pdf"),
                plot = housing_median_rent_plot, width = 6.5, height = 4)

ggplot2::ggsave(filename = here::here("output/axe1/housing/housing_median_own_plot.pdf"),
                plot = housing_median_own_plot, width = 6.5, height = 4)

# Housing Starts and Completions ------------------------------------------

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
housing_te_QC <- housing_te_CMA <- get_census(dataset = "CA21",
                                              regions = list(PR = 24),
                                              level = "PR",
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

#Creating a new data frame as reference doesn't have a csv version
#https://www.lavalensante.com/fileadmin/internet/cisss_laval/Documentation/Sante_publique/Profils_et_portraits/Portraits/Donnees_par_secteur_d_amenagement_2021_VF.pdf
taux_efforts_new <- data.frame(
  Name = c("Ensemble du Québec", "Laval", "Secteur 1 : Duvernay, Saint-François et Saint-Vincent-de-Paul", 
           "Secteur 2 : Pont-Viau, Renaud-Coursol et Laval-des-Rapides", "Secteur 3 : Chomedey",
           "Secteur 4 : Sainte-Dorothée, Laval-Ouest, Les Îles-Laval, Fabreville-Ouest et Laval-sur-le-Lac",
           "Secteur 5 : Fabreville-Est et Sainte-Rose", "Secteur 6 : Vimont et Auteuil"),
  "Ménages locataires" = c(0.252, 0.282, 0.251, 0.254, 0.317, 0.341, 0.297, 0.225),
  "Ménages propriétaires" = c(0.100, 0.119, 0.101, 0.141, 0.173, 0.120, 0.087, 0.093),
  "Tous les ménages" = c(0.161, 0.173,
                         (0.251*4965+0.101*17550)/(4965+17550),
                         (0.254*19055+0.141*15600)/(19055+15600),
                         (0.317*17645+0.173*20460)/(17645+20460),
                         (0.341*4615+0.120*19935)/(4615+19935),
                         (0.297*5745+0.087*21800)/(21800+5745),
                         (0.225*4760+0.093*17660)/(4760+17660)),
  check.names = FALSE)

new_taux_efforts_table <- 
  gt(taux_efforts_new) |> 
  data_color(columns = 2:ncol(taux_efforts_new),
             colors = scales::col_numeric(palette = c("white",
                                                      color_theme("purpletransport")),
                                          domain = NULL)) |> 
  fmt(columns = 2:ncol(taux_efforts_new), fns = convert_pct) |> 
  tab_row_group(label = "Secteur",
                rows = 3:nrow(taux_efforts)) |>
  tab_row_group(label = "Région", rows = 1:2) |>
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
      font = "KMR Apparat"
    ),
    locations = cells_body()
  ) |>
  tab_style(
    style = cell_text(
      font = "KMR Apparat"
    ),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_text(
      font = "KMR Apparat"
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
      font = "KMR Apparat"
    ),
    locations = cells_body()
  ) |>
  tab_style(
    style = cell_text(
      font = "KMR Apparat"
    ),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_text(
      font = "KMR Apparat"
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

core_laval <- read.csv("data/axe1/9810024601_donneesselectionnees.csv", sep = ";") |> 
  select(8, 9, 16) |> 
  mutate(across(2, ~ if_else(str_detect(.x, "Total"), "Total", .x))) |> 
  rename(Need = 1, Type = 2, Value = 3) |> 
  pivot_wider(names_from = Need, values_from = Value) |> 
  mutate("Inférieur au seuil d'abordabilité (n)" = `Inférieur au seuil d'abordabilité seulement` + `Inférieur aux seuils d'abordabilité et de taille convenable seulement` +
           `Inférieur aux seuils d'abordabilité et de qualité convenable seulement` + `Inférieur aux seuils d'abordabilité, de taille et de qualité convenable`,
         "Inférieur au seuil de taille convenable (n)" = `Inférieur au seuil de taille convenable seulement` + `Inférieur aux seuils d'abordabilité et de taille convenable seulement` +
           `Inférieur aux seuils de taille et qualité convenable` + `Inférieur aux seuils d'abordabilité, de taille et de qualité convenable`,
         "Inférieur au seuil de qualité convenable (n)" = `Inférieur au seuil de qualité convenable seulement` + `Inférieur aux seuils de taille et qualité convenable` +
           `Inférieur aux seuils d'abordabilité et de qualité convenable seulement` + `Inférieur aux seuils d'abordabilité, de taille et de qualité convenable`) |> 
  select(Type, `Total - Logement acceptable`, "Inférieur au seuil d'abordabilité (n)", "Inférieur au seuil de taille convenable (n)",
         "Inférieur au seuil de qualité convenable (n)", Acceptable) |> 
  pivot_longer(cols = -Type, values_to = "Value") |> 
  pivot_wider(names_from = Type, values_from = Value) |> 
  rename("Tous les ménages (n)" = Total,
         "Ménages locataires (n)" = Locataire,
         "Ménages propriétaires (n)" = Propriétaire) |> 
  mutate(across(ends_with("(n)"), as.numeric)) |> 
  mutate(
    Total = `Tous les ménages (n)`[1],
    Owner = `Ménages propriétaires (n)`[1],
    Renter = `Ménages locataires (n)`[1]) |> 
  mutate(`Tous les ménages (%)` = `Tous les ménages (n)` / Total,
         `Ménages propriétaires (%)` = `Ménages propriétaires (n)` / Owner,
         `Ménages locataires (%)` = `Ménages locataires (n)` / Renter) |> 
  select(name, `Ménages propriétaires (n)`, `Ménages propriétaires (%)`, 
         `Ménages locataires (n)`, `Ménages locataires (%)`, 
         `Tous les ménages (n)`, `Tous les ménages (%)`) |> 
  slice(-1)

core_qc <- read.csv("data/axe1/9810024601_donneesselectionnees_QC.csv", sep = ";") |> 
  select(8, 9, 16) |> 
  mutate(across(2, ~ if_else(str_detect(.x, "Total"), "Total", .x))) |> 
  slice(-4, -11, -15, -25, -32) |> 
  rename(Need = 1, Type = 2, Value = 3) |> 
  pivot_wider(names_from = Need, values_from = Value) |> 
  mutate("Inférieur au seuil d'abordabilité (n)" = `Inférieur au seuil d'abordabilité seulement` + `Inférieur aux seuils d'abordabilité et de taille convenable seulement` +
           `Inférieur aux seuils d'abordabilité et de qualité convenable seulement` + `Inférieur aux seuils d'abordabilité, de taille et de qualité convenable`,
         "Inférieur au seuil de taille convenable (n)" = `Inférieur au seuil de taille convenable seulement` + `Inférieur aux seuils d'abordabilité et de taille convenable seulement` +
           `Inférieur aux seuils de taille et qualité convenable` + `Inférieur aux seuils d'abordabilité, de taille et de qualité convenable`,
         "Inférieur au seuil de qualité convenable (n)" = `Inférieur au seuil de qualité convenable seulement` + `Inférieur aux seuils de taille et qualité convenable` +
           `Inférieur aux seuils d'abordabilité et de qualité convenable seulement` + `Inférieur aux seuils d'abordabilité, de taille et de qualité convenable`) |> 
  select(Type, `Total - Logement acceptable`, "Inférieur au seuil d'abordabilité (n)", "Inférieur au seuil de taille convenable (n)",
         "Inférieur au seuil de qualité convenable (n)", Acceptable) |> 
  pivot_longer(cols = -Type, values_to = "Value") |> 
  pivot_wider(names_from = Type, values_from = Value) |> 
  rename("Tous les ménages (n)" = Total,
         "Ménages locataires (n)" = Locataire,
         "Ménages propriétaires (n)" = Propriétaire) |> 
  mutate(across(ends_with("(n)"), as.numeric)) |> 
  mutate(
    Total = `Tous les ménages (n)`[1],
    Owner = `Ménages propriétaires (n)`[1],
    Renter = `Ménages locataires (n)`[1]) |> 
  mutate(`Tous les ménages (%)` = `Tous les ménages (n)` / Total,
         `Ménages propriétaires (%)` = `Ménages propriétaires (n)` / Owner,
         `Ménages locataires (%)` = `Ménages locataires (n)` / Renter) |> 
  select(name, `Ménages propriétaires (n)`, `Ménages propriétaires (%)`, 
         `Ménages locataires (n)`, `Ménages locataires (%)`, 
         `Tous les ménages (n)`, `Tous les ménages (%)`) |> 
  slice(-1)

acceptable_housing_table <-
  gt(acceptable_housing) |> 
  data_color(
    columns = 2:ncol(acceptable_housing),
    fn = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = NULL
    )
  ) |> 
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
      font = "KMR-Apparat-Regular"
    ),
    locations = cells_body()
  ) |>
  tab_style(
    style = cell_text(
      font = "KMR-Apparat-Regular"
    ),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_text(
      font = "KMR-Apparat-Regular"
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

acceptable_laval_table <- gt(core_laval) |> 
  cols_label(name = "") |> 
  data_color(columns = 2:ncol(core_laval),
             fn = scales::col_numeric(palette = c("white", color_theme("purpletransport")),
                                      domain = NULL)) |> 
  fmt(columns = c(3, 5, 7), fns = convert_pct) |> 
  fmt(columns = c(2, 4, 6), fns = convert_number) |> 
  tab_style(style = cell_borders(sides = "top",
                                 color = "transparent",
                                 weight = px(10)),
            locations = cells_body(rows = 4)) |> 
  tab_style(style = cell_text(font = "KMR-Apparat-Regular"),
            locations = cells_body()) |>
  tab_style(style = cell_text(font = "KMR-Apparat-Regular"),
            locations = cells_column_labels()) |>
  tab_style(style = cell_text(font = "KMR-Apparat-Regular"),
            locations = cells_row_groups()) |>
  tab_style(style = cell_fill(color = "#F0F0F0"),
            locations = cells_row_groups()) |> 
  tab_options(table.font.size = 12,
              row_group.font.size = 12,
              table.width = px(6 * 104))

acceptable_qc_table <- gt(core_qc) |> 
  cols_label(name = "") |> 
  data_color(columns = 2:ncol(core_qc),
             fn = scales::col_numeric(palette = c("white", color_theme("purpletransport")),
                                      domain = NULL)) |> 
  fmt(columns = c(3, 5, 7), fns = convert_pct) |> 
  fmt(columns = c(2, 4, 6), fns = convert_number) |> 
  tab_style(style = cell_borders(sides = "top",
                                 color = "transparent",
                                 weight = px(10)),
            locations = cells_body(rows = 4)) |> 
  tab_style(style = cell_text(font = "KMR-Apparat-Regular"),
            locations = cells_body()) |>
  tab_style(style = cell_text(font = "KMR-Apparat-Regular"),
            locations = cells_column_labels()) |>
  tab_style(style = cell_text(font = "KMR-Apparat-Regular"),
            locations = cells_row_groups()) |>
  tab_style(style = cell_fill(color = "#F0F0F0"),
            locations = cells_row_groups()) |> 
  tab_options(table.font.size = 12,
              row_group.font.size = 12,
              table.width = px(6 * 104))

gtsave(acceptable_housing_table, "output/axe1/housing/acceptable_housing_table.png", zoom = 3)

taille <- "Inférieur au seuil de taille convenable"
taille_all <- convert_pct(acceptable_housing$`Tous les ménages_Laval`[acceptable_housing$` ` == taille])
taille_owner <- convert_pct(acceptable_housing$`Ménages propriétaires_Laval`[acceptable_housing$` ` == taille])
taille_tenant <- convert_pct(acceptable_housing$`Ménages locataires_Laval`[acceptable_housing$` ` == taille])

acceptable <- "Acceptable"
acceptable_all <- convert_pct(acceptable_housing$`Tous les ménages_Laval`[acceptable_housing$` ` == acceptable])
acceptable_owner <- convert_pct(acceptable_housing$`Ménages propriétaires_Laval`[acceptable_housing$` ` == acceptable])
acceptable_tenant <- convert_pct(acceptable_housing$`Ménages locataires_Laval`[acceptable_housing$` ` == acceptable])

taille_lvl_number <- convert_number(core_laval$`Tous les ménages (n)`[core_laval$name == "Inférieur au seuil de taille convenable (n)"])
taille_lvl_tenant <- convert_number(core_laval$`Ménages locataires (n)`[core_laval$name == "Inférieur au seuil de taille convenable (n)"])
taille_lvl_owner <- convert_number(core_laval$`Ménages propriétaires (n)`[core_laval$name == "Inférieur au seuil de taille convenable (n)"])


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

# core_need_tenant <- convert_pct(core_need$percentage[core_need$tenure_status == "Locataire"])
# core_need_tenant_QC <- convert_pct(core_need_QC$percentage[core_need_QC$tenure_status == "Locataire"])


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


core_need_tenant <- get_census("CA21", regions = list(CSD = 2465005),
                               level = "CSD",
                               vectors = c(need = "v_CA21_4316")) |> 
  mutate(percent = convert_pct(need / 100)) |> 
  pull()

core_need_tenant_QC <- get_census("CA21", regions = list(PR = 24),
                                  level = "PR",
                                  vectors = c(need = "v_CA21_4316")) |> 
  mutate(percent = convert_pct(need / 100)) |> 
  pull()

core_need_owner <- get_census("CA21", regions = list(CSD = 2465005),
                              level = "CSD",
                              vectors = c(need = "v_CA21_4308")) |> 
  mutate(percent = convert_pct(need / 100)) |> 
  pull()

core_need_owner_QC <- get_census("CA21", regions = list(PR = 24),
                                 level = "PR",
                                 vectors = c(need = "v_CA21_4308")) |> 
  mutate(percent = convert_pct(need / 100)) |> 
  pull()

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
    label = "Laval",
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
      font = "KMR-Apparat-Regular"
    ),
    locations = cells_body()
  ) |>
  tab_style(
    style = cell_text(
      font = "KMR-Apparat-Regular"
    ),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_text(
      font = "KMR-Apparat-Regular"
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