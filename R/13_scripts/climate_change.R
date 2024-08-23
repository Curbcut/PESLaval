### Climate change #############################################################

source("R/01_startup.R")


#Reading the Montreal CMA shapefile
mtlcma_sf <- cancensus::get_census(dataset = "CA21",
                                   regions = list(CMA = 24462),
                                   level = "CMA",
                                   geo_format = "sf")

#Grabbing Laval's shapefile by census subdivision
laval_csd <- cancensus::get_census(dataset = "CA21", 
                                  regions = list(CSD = 2465005), 
                                  level = "CSD", 
                                  geo_format = "sf")

#Grabbing Laval's shapefile by census tract
laval_ct <- cancensus::get_census(dataset = "CA21", 
                                  regions = list(CSD = 2465005), 
                                  level = "CT", 
                                  geo_format = "sf")

#Grabbing Laval shapefile by dissemination area
laval_da <- cancensus::get_census(dataset = "CA21", 
                                  regions = list(CSD = 2465005), 
                                  level = "DA", 
                                  geo_format = "sf")

#Grabbing Laval shapefile by dissemination block
laval_db <- cancensus::get_census(dataset = "CA21", 
                                  regions = list(CSD = 2465005), 
                                  level = "DB", 
                                  geo_format = "sf")

#Loading the colors for the curbcut and heat scale for ouranos_sf
curbcut_scale <- c("#C4CDE1", "#98A8CB", "#6C83B5", "#4C5C7F", "#2B3448")
heat_scale <- c("1" = "#2CA25F", "2" = "#98D6C7", "3" = "#E5F5F9", "4" = "#FCDFD2", "5" = "#FA9073")
curbcut_na <- "#B3B3BB"

# Heat --------------------------------------------------------------------
#Shapefile for heat index in Laval
#src = https://www.donneesquebec.ca/recherche/dataset/ilots-de-chaleur-fraicheur-urbains-et-ecarts-de-temperature-relatifs-2020-2022
#Edited using QGIS to be Laval only
heat_sf <- read_sf(dsn = "D://McGill/can_cache/heat", layer = "heat") |> 
  st_transform(4326)

if (is.na(st_crs(heat_sf))) {
  heat_sf <- st_set_crs(heat_sf, 4326)
}

#Mapping out heat_sf
ggplot() +
  geom_sf(data = heat_sf, aes(fill = `_label`), color = NA) +
  labs(title = "Indice d’intensité d’îlots de chaleur urbains de Laval 2020-2022 ",
       fill = "Indice d'Intensité de Chaleur") +
  scale_fill_gradientn(colors = curbcut_scale, na.value = curbcut_na,
                       labels = label_comma(big.mark = ".", decimal.mark = ",")) +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.justification = "center") +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
                               barwidth = 10, barheight = 1))

#Shapefile for heat vulnerability and edited using QGIS for Laval
#src = https://atlas-vulnerabilite.ulaval.ca/vague-de-chaleur/
ouranos_sf <- read_sf("D://McGill/can_cache/heat/ouranosheat.shp") |> 
  select(-URL_12) |> 
  mutate(Index = case_when(
    N_Vulnre_2 == 'Donnée manquante' ~ NA,
    N_Vulnre_2 == 'Très faible vulnérabilité' ~ "1",
    N_Vulnre_2 == 'Faible vulnérabilité' ~ "2",
    N_Vulnre_2 == 'Vulnérabilité modérée' ~ "3",
    N_Vulnre_2 == 'Vulnérabilité moyenne' ~ "4",
    N_Vulnre_2 == 'Forte vulnérabilité' ~ "5",
    TRUE ~ "0"  # Default value for all other cases
  ))

#calculating number of people who are highly vulnerable to heat waves
vuln_pop <- ouranos_sf |> 
  filter(N_Vulnre_2 %in% c("Forte vulnérabilité", "Vulnérabilité moyenne")) |> 
  group_by(N_Vulnre_2) |> 
  summarise(sum_Pop = sum(Pop16_12, na.rm = TRUE))

#Mapping out heat vulnerability
ggplot(data = ouranos_sf) +
  gg_cc_tiles +
  geom_sf(aes(fill = `Index`), color = "#454545", size = 0.2) +
  labs(fill = "Niveau de vulnérabilité") +
  scale_fill_manual(values = heat_scale) +
  theme_void() +
  theme(axis.text = element_blank(), axis.title = element_blank(),
        panel.grid = element_blank(), plot.title = element_blank(),
        legend.position = "bottom", legend.justification = "center") +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, 
                             label.position = "bottom", keywidth = 3, keyheight = 0.5)) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

# Biodiversity ------------------------------------------------------------
# #Reading greenspace shapefile and canopy .tif
# greenspace_shp <- st_read("/Users/justin/Documents/R/curbcut/parc_espace_vert_20240607_PG.shp")
# canopy_tif <- raster("D://Mcgill/can_cache/treecanopy/treecanopy.tif")
# 
# #Adding artificial margins, breaks, and associated colors
# par(mar = c(0, 0, 0, 0))
# breaks <- c(0, 1, 2, 3, 4, 5)
# color_breaks <- c("#feb24c", "#fff7bc", "#e5f5f9", "#99d8c9", "#a6bddb")
# 
# #Plotting the .tif file
# plot(canopy_tif, main = "", breaks = breaks, col = color_breaks,
#     box = FALSE, axes = FALSE, legend.shrink = 0.3)
# 
# #Adding title to the .tif map
# title(main = "Couverture du Couvert Végétal à Laval 2021", adj = 0.9, line = -2, cex.main = 1.2)
# 
# #Mapping out greenspaces (unused)
# ggplot() +
#   geom_sf(data = laval_csd, color = "black", fill = "#F8F8F8") +
#   geom_sf(data = greenspace_shp, color = "darkgreen", fill = "darkgreen") +
#   theme_minimal() +
#   theme(axis.line = element_blank(), axis.text = element_blank(),
#         axis.title = element_blank(), axis.ticks = element_blank(),
#         panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
#         legend.position = "bottom", legend.justification = "center")

## USING CURBCUT DATA
ndvi <- terra::rast("data/axe1/climaterisk/grd30.tif")
ndvi <- ndvi[["ndvi_2023"]]

# Specify the file name and dimensions for the output image
png(filename = "output/axe1/climaterisk/ndvi.png", width = 1000, height = 800)

# Adding artificial margins, breaks, and associated colors
par(mar = c(0, 0, 0, 0))
breaks <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
color_breaks <- c("#feb24c", "#fff7bc", "#e5f5f9", "#99d8c9", "#a6bddb")

# Plotting the .tif file
plot(ndvi, main = "", breaks = breaks, col = color_breaks, 
     box = FALSE, axes = FALSE, legend = FALSE)

# Adding the legend
legend("bottomright", legend = c("Très bas", "Bas", "Moyen", "Haut", "Très haut"),
       fill = color_breaks, cex = 1.75, bty = "n", xpd = TRUE)

# Close the device to save the plot to the file
dev.off()


# Biodiversity (Curbcut) --------------------------------------------------
biod_sf <- raster("D://Mcgill/can_cache/ndvi.tif")

# Flooding ----------------------------------------------------------------
#Vectors for the flooding
flooding <- st_read("D://Mcgill/can_cache/flood.gpkg") |> 
  st_transform(crs = 4326)
curbcut_flooding <- st_read("D://Mcgill/can_cache/enviro/zone_inondable_RCI_CDU_20240607_PG.shp") |> 
  st_transform(crs = 4326)
flooding0_2 <- curbcut_flooding |> 
  filter(LIMITE == "Zone inondable 0-2 ans")
flooding2_20 <- curbcut_flooding |> 
  filter(LIMITE == "Zone inondable 2-20 ans")
flooding20_100 <- curbcut_flooding |> 
  filter(LIMITE == "Zone inondable 20-100 ans")

#Grabbing the shapefile for Laval DB so number of areas affected by flooding can be calculated
laval_db <- cancensus::get_census(dataset = "CA21", 
                                  regions = list(CSD = 2465005), 
                                  level = "DB", 
                                  geo_format = "sf") |> 
  st_transform(4326)

#Bounding box of Laval
laval_bbox <- st_bbox(laval_csd)

#Mapping the Quebec given data
ggplot() +
  geom_sf(data = mtlcma_sf, fill = "grey", color = "black") +
  geom_sf(data = laval_csd, fill = "#FAF9F6", color = NA) +
  geom_sf(data = flooding, fill = "#74ccf4", color = NA) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  labs(title = "Zones inondables de Laval 2023",
       fill = "Level of Vulnerability") +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.justification = "center") +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, 
                             label.position = "bottom", keywidth = 3, keyheight = 0.5)) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

#Flood map using the more granular data, not included in the first draft version 1
ggplot() +
  geom_sf(data = mtlcma_sf, fill = "grey", color = "black") +
  geom_sf(data = laval_csd, fill = "#F8F8F8", color = "black") +
  geom_sf(data = flooding20_100, fill = "#bdc9e1",color = NA) +
  geom_sf(data = flooding2_20, fill = "#74a9cf",color = NA) +
  geom_sf(data = flooding0_2, fill = "#045a8d",color = NA) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  labs(title = "Zones inondables de Laval 2019",
       fill = "Level of Vulnerability") +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.text = element_blank(),
        panel.background = element_rect(fill = "#cedbdc"),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.justification = "center") +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

#Flood map using more granular data, version 2 without making separate files
ggplot(data = curbcut_flooding) +
  gg_cc_tiles +
  geom_sf(data = laval_csd, fill = "#B3B3BB", color = NA, alpha = 0.5) +
  geom_sf(aes(fill = LIMITE), color = NA) +
  scale_fill_manual(values = c(
    "Zone inondable 0-2 ans" = "#045a8d",
    "Zone inondable 2-20 ans" = "#74a9cf",
    "Zone inondable 20-100 ans" = "#bdc9e1"),
    labels = c(
      "Zone inondable 0-2 ans" = "Zone inondable 0-2 ans",
      "Zone inondable 2-20 ans" = "2-20 ans",
      "Zone inondable 20-100 ans" = "20-100 ans")) +
  theme_void() +
  theme(axis.text = element_blank(), axis.title = element_blank(),
        panel.grid = element_blank(), legend.title = element_blank(),
        legend.position = "bottom", legend.justification = "center") +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

#Validating shapefiles to ensure the analysis works
flooding <- st_make_valid(flooding)
laval_db <- st_make_valid(laval_db)
#Determining which DBs intersect with the flood zones, and then giving them a value of 1 in the flood column
intersects <- st_intersects(laval_db, flooding, sparse = FALSE)
laval_db$flooded <- apply(intersects, 1, function(x) as.integer(any(x)))
#Summing up the population of DBs that intersect with the flood zone and calculating the proportion of the population
flood_lvl <- laval_db |> 
  summarise(
    total_pop = sum(Population, na.rm = TRUE),
    flood_pop = sum(Population[flooded == 1], na.rm = TRUE)
  ) |> 
  mutate(flood_prop = flood_pop / total_pop)

#Analysis for granular data, first validating shapefiles
curbcut_flooding <- st_make_valid(curbcut_flooding)
laval_db <- st_make_valid(laval_db)

#Assigning flood values based on the LIMITE column
assign_flooding_value <- function(LIMITE) {
  case_when(
    LIMITE == "Zone inondable 0-2 ans" ~ 1,
    LIMITE == "Zone inondable 2-20 ans" ~ 2,
    LIMITE == "Zone inondable 20-100 ans" ~ 3,
    TRUE ~ NA_real_
  )
}

#Creating a new vector that combines the DBs of Laval and the flood data
laval_joined <- st_join(laval_db, curbcut_flooding) |> 
  mutate(Flooding = assign_flooding_value(LIMITE)) |> 
  group_by(GeoUID) |> 
  summarise(Flooding = if_else(any(!is.na(Flooding)), min(Flooding, na.rm = TRUE), 0)) |> 
  st_drop_geometry()

#Assign flood values to DBs and then calculates proportion of population that
#are at risk of flooding by year
laval_flooded <- laval_db |> 
  left_join(laval_joined, by = "GeoUID") |> 
  summarise(
    total_population = sum(Population, na.rm = TRUE),
    flooding_0_2 = sum(ifelse(Flooding == 1, Population, 0), na.rm = TRUE),
    flooding_2_20 = sum(ifelse(Flooding %in% c(1, 2), Population, 0), na.rm = TRUE),
    flooding_20_100 = sum(ifelse(Flooding %in% c(1, 2, 3), Population, 0), na.rm = TRUE)
  ) |> 
  mutate(proportion_0_2 = flooding_0_2 * 100 / total_population,
         proportion_2_20 = flooding_2_20 * 100 / total_population,
         proportion_20_100 = flooding_20_100 * 100 / total_population)

# Flooded Building Analysis -----------------------------------------------------
zoning <- st_read("D://McGill/can_cache/cdu_type_milieux.shp") |> 
  st_transform(crs = 4326) |> 
  select(TYPE_MILIE) |> 
  filter(!TYPE_MILIE %in% c("T1.1", "CI.1", "CI.2", "CI.3", "CE", "ZC",
                            "ZH", "ZI.1", "ZI.2", "ZI.3", "ZE", "ZP", "SZD.1",
                            "SZD.2", "SZD.3", "SZD.4", "SZD.5")) |> 
  st_make_valid()

buildings <- qread("D://McGill/can_cache/buildingss.qs") |> 
  rename("GeoUID" = "DB_ID") |> 
  st_join(zoning) |> 
  filter(!is.na(TYPE_MILIE)) |> 
  distinct(ID, .keep_all = TRUE) |> 
  select(GeoUID) |> 
  mutate(building = 1)

flooded_buildings <- buildings |>
  mutate(flood0_2 = 0,
         flood2_20 = 0,
         flood20_100 = 0)

intersections0_2 <- st_intersects(flooded_buildings, flooding0_2, sparse = FALSE)
intersections2_20 <- st_intersects(flooded_buildings, flooding2_20, sparse = FALSE)
intersections20_100 <- st_intersects(flooded_buildings, flooding20_100, sparse = FALSE)

flooded_buildings$flood0_2[apply(intersections0_2, 1, any)] <- 1
flooded_buildings$flood2_20[apply(intersections0_2, 1, any)] <- 1
flooded_buildings$flood20_100[apply(intersections0_2, 1, any)] <- 1

flooded_buildings$flood2_20[apply(intersections2_20, 1, any)] <- 1
flooded_buildings$flood20_100[apply(intersections2_20, 1, any)] <- 1

flooded_buildings$flood20_100[apply(intersections20_100, 1, any)] <- 1

flooded_buildings <- flooded_buildings |>
  st_drop_geometry() |> 
  group_by(GeoUID) |> 
  summarize(
    building = sum(building, na.rm = TRUE),
    flood0_2 = sum(flood0_2, na.rm = TRUE),
    flood2_20 = sum(flood2_20, na.rm = TRUE),
    flood20_100 = sum(flood20_100, na.rm = TRUE)
  )

flooded_db <- laval_db |> 
  st_drop_geometry() |> 
  select(GeoUID, Population) |> 
  left_join(flooded_buildings, by = "GeoUID") |> 
  mutate(pop0_2 = round(Population * (flood0_2 / building), 0),
         pop2_20 = round(Population * (flood2_20 / building), 0),
         pop20_100 = round(Population * (flood20_100 / building), 0)) |> 
  summarize(Population = sum(Population, na.rm = TRUE),
            pop0_2 = sum(pop0_2, na.rm = TRUE),
            pop2_20 = sum(pop2_20, na.rm = TRUE),
            pop20_100 = sum(pop20_100, na.rm = TRUE))

# Non-Residential Flooding ------------------------------------------------
non_res_zoning <- st_read("D://McGill/can_cache/cdu_type_milieux.shp") |> 
  st_transform(crs = 4326) |> 
  select(TYPE_MILIE) |> 
  filter(TYPE_MILIE %in% c("T1.1", "CI.1", "CI.2", "CI.3", "CE", "ZC",
                           "ZH", "ZI.1", "ZI.2", "ZI.3", "ZE", "ZP", "SZD.1",
                           "SZD.2", "SZD.3", "SZD.4", "SZD.5")) |> 
  st_make_valid()

non_res <- qread("D://McGill/can_cache/buildingss.qs") |> 
  rename("GeoUID" = "DB_ID") |> 
  st_join(non_res_zoning) |> 
  filter(!is.na(TYPE_MILIE)) |> 
  mutate(flood = 0)

flood_non_res <- st_intersects(non_res, flooding0_2, sparse = FALSE)
non_res$flood[apply(flood_non_res, 1, any)] <- 1
