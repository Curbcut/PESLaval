### Climate change #############################################################

source("R/01_startup.R")


# # Heat --------------------------------------------------------------------
# #Shapefile for heat index in Laval
# #src = https://www.donneesquebec.ca/recherche/dataset/ilots-de-chaleur-fraicheur-urbains-et-ecarts-de-temperature-relatifs-2020-2022
# #Edited using QGIS to be Laval only
# heat_sf <- read_sf(dsn = "D://McGill/can_cache/heat", layer = "heat") |> 
#   st_transform(4326)
# 
# if (is.na(st_crs(heat_sf))) {
#   heat_sf <- st_set_crs(heat_sf, 4326)
# }
# 
# #Mapping out heat_sf
# ggplot() +
#   geom_sf(data = heat_sf, aes(fill = `_label`), color = NA) +
#   labs(title = "Indice d’intensité d’îlots de chaleur urbains de Laval 2020-2022 ",
#        fill = "Indice d'Intensité de Chaleur") +
#   scale_fill_gradientn(colors = curbcut_scale, na.value = curbcut_na,
#                        labels = label_comma(big.mark = ".", decimal.mark = ",")) +
#   theme_minimal() +
#   theme(axis.line = element_blank(), axis.text = element_blank(),
#         axis.title = element_blank(), axis.ticks = element_blank(),
#         panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
#         legend.position = "bottom", legend.justification = "center") +
#   guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
#                                barwidth = 10, barheight = 1))
# 
# #Shapefile for heat vulnerability and edited using QGIS for Laval
# #src = https://atlas-vulnerabilite.ulaval.ca/vague-de-chaleur/
# ouranos_sf <- read_sf("D://McGill/can_cache/heat/ouranosheat.shp") |> 
#   select(-URL_12) |> 
#   mutate(Index = case_when(
#     N_Vulnre_2 == 'Donnée manquante' ~ NA,
#     N_Vulnre_2 == 'Très faible vulnérabilité' ~ "1",
#     N_Vulnre_2 == 'Faible vulnérabilité' ~ "2",
#     N_Vulnre_2 == 'Vulnérabilité modérée' ~ "3",
#     N_Vulnre_2 == 'Vulnérabilité moyenne' ~ "4",
#     N_Vulnre_2 == 'Forte vulnérabilité' ~ "5",
#     TRUE ~ "0"  # Default value for all other cases
#   ))
# 
# #calculating number of people who are highly vulnerable to heat waves
# vuln_pop <- ouranos_sf |> 
#   filter(N_Vulnre_2 %in% c("Forte vulnérabilité", "Vulnérabilité moyenne")) |> 
#   group_by(N_Vulnre_2) |> 
#   summarise(sum_Pop = sum(Pop16_12, na.rm = TRUE))
# 
# #Mapping out heat vulnerability
# ggplot(data = ouranos_sf) +
#   gg_cc_tiles +
#   geom_sf(aes(fill = `Index`), color = "#454545", size = 0.2) +
#   labs(fill = "Niveau de vulnérabilité") +
#   scale_fill_manual(values = heat_scale) +
#   theme_void() +
#   theme(axis.text = element_blank(), axis.title = element_blank(),
#         panel.grid = element_blank(), plot.title = element_blank(),
#         legend.position = "bottom", legend.justification = "center") +
#   guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, 
#                              label.position = "bottom", keywidth = 3, keyheight = 0.5)) +
#   coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
#            ylim = c(laval_bbox$ymin, laval_bbox$ymax))


library(httr)
library(sf)

fetch_all_pages <- function(base_url) {
  # Function to fetch data with pagination and error handling
  # Set initial query parameters
  query_params <- list(
    where = "1=1", # to get all the data; no filter
    outFields = "*",
    outSR = "4326", # output spatial reference; EPSG:4326 is WGS84 lat/long
    f = "geojson", # output format
    returnGeometry = "true" # to ensure geometry is included
  )
  
  all_data <- list()
  offset <- 0
  keep_fetching <- TRUE
  
  while (keep_fetching) {
    # Update the query parameters with the current offset and limit
    query_params$resultOffset <- offset
    query_params$resultRecordCount <- 1000
    
    # Make the GET request
    response <- httr::GET(url = base_url, query = query_params)
    
    # Check if the request was successful
    if (httr::status_code(response) != 200) {
      stop("Failed to retrieve data: ", httr::status_code(response))
    }
    
    # Inspect the content type to ensure it's GeoJSON
    content_type <- httr::headers(response)$`content-type`
    # if (!grepl("application/geo", content_type, ignore.case = TRUE)) {
    #   break
    # }
    
    # Read the content as geojson and convert to an sf object
    page_data <- tryCatch(
      sf::st_read(httr::content(response, "text"), quiet = TRUE),
      error = function(e) {
        message("Error reading data: ", e$message)
        return(NULL)
      }
    )
    
    # Check if data was successfully read
    if (is.null(page_data) || nrow(page_data) == 0) {
      keep_fetching <- FALSE
    } else {
      # Store the fetched data
      all_data[[length(all_data) + 1]] <- page_data
      # Increment the offset for the next page
      offset <- offset + 1000
      print(length(all_data))
    }
  }
  
  all_data <- Reduce(rbind, all_data)
  all_data <- sf::st_make_valid(all_data)
  
  return(all_data)
}

heat <- fetch_all_pages(
  "https://services2.arcgis.com/iMo05rGDH0l2KYh8/ArcGIS/rest/services/Chaleur_Vulnérabilité/FeatureServer/1/query"
)

heat <- heat[c("ADIDU", "VulnTot")]
heat <- heat[heat$VulnTot != "#NULL!", ]

heat$VulnTot <- as.factor(heat$VulnTot)  # Convert the vulnerability levels to factors.

heat_plot <- 
ggplot(heat) +
  gg_cc_tiles +
  geom_sf(aes(fill = VulnTot), color = "transparent", lwd = 0) +
  scale_fill_manual(values = curbcut_colors$left_5$fill[3:6],
                    name = "Vulnérabilité à la chaleur (2023, 2024)",
                    labels = c("Vulnérabilité 1",
                               "Vulnérabilité 2a",
                               "Vulnérabilité 2b",
                               "Vulnérabilité 3"),
                    guide = guide_legend(title.position = "top", label.position = "bottom", nrow = 1)) +
  gg_cc_theme

ggplot2::ggsave(filename = here::here("output/axe1/climaterisk/heat_plot.pdf"),
                plot = heat_plot, width = 7.5, height = 6)

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

library(terra)
## USING CURBCUT DATA
ndvi <- terra::rast("data/axe1/climaterisk/grd30.tif")
ndvi <- ndvi[["ndvi_2023"]]


# Convert the raster to a dataframe
ndvi_df <- as.data.frame(ndvi, xy = TRUE, na.rm = TRUE)

lvlsec_utm <- sf::st_transform(laval_sectors, crs = sf::st_crs(ndvi))
these_tiles <- raster::projectRaster(tiles, crs = crs(ndvi))

ndvi_plot <- 
  ggplot() +
  ggspatial::layer_spatial(these_tiles, alpha = 0.7) +
  geom_tile(data = ndvi_df, aes(x = x, y = y, fill = cut(ndvi_2023, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)))) +  
  scale_fill_manual(values = c("#feb24c", "#fff7bc", "#e5f5f9", "#99d8c9", "#a6bddb"),
                    labels = c("Très bas", "Bas", "Moyen", "Haut", "Très haut"),
                    na.value = curbcut_colors$left_5$fill[1]) +
  labs(title = NULL) +
  geom_sf(data = lvlsec_utm, fill = "transparent", color = "black", size = 0.5) +
  c(list(
    coord_sf(xlim = c(sf::st_bbox(lvlsec_utm)["xmin"], sf::st_bbox(lvlsec_utm)["xmax"]), 
             ylim = c(sf::st_bbox(lvlsec_utm)["ymin"], sf::st_bbox(lvlsec_utm)["ymax"]))),
    gg_cc_theme_no_sf,
    list(theme_void()),
    list(default_theme),
    list(theme(legend.box.margin = margin(t = 0)))
  ) +
  theme(legend.title = element_blank())

ggplot2::ggsave(filename = here::here("output/axe1/climaterisk/ndvi_plot.pdf"),
                plot = ndvi_plot, width = 12, height = 6)


ndvi_plot_infographics <- 
  ggplot() +
  # ggspatial::layer_spatial(these_tiles, alpha = 0.7) +
  geom_tile(data = ndvi_df, aes(x = x, y = y, fill = cut(ndvi_2023, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)))) +  
  scale_fill_manual(values = c("#feb24c", "#fff7bc", "#e5f5f9", "#99d8c9", "#a6bddb"),
                    labels = c("Très bas", "Bas", "Moyen", "Haut", "Très haut"),
                    na.value = curbcut_colors$left_5$fill[1]) +
  labs(title = NULL) +
  geom_sf(data = lvlsec_utm, fill = "transparent", color = "black", size = 0.5) +
  c(list(
    coord_sf(xlim = c(sf::st_bbox(lvlsec_utm)["xmin"], sf::st_bbox(lvlsec_utm)["xmax"]), 
             ylim = c(sf::st_bbox(lvlsec_utm)["ymin"], sf::st_bbox(lvlsec_utm)["ymax"]))),
    gg_cc_theme_no_sf,
    list(theme_void()),
    list(default_theme),
    list(theme(legend.box.margin = margin(t = 0)))
  ) +
  theme(legend.title = element_blank())

ggplot2::ggsave(filename = here::here("output/infographic/ndvi_plot.pdf"),
                plot = ndvi_plot_infographics, width = 4.5, height = 4)


# Indice Canopée ----------------------------------------------------------
indice_can <- rast("data/axe1/650_IndiceCanopee_2023.tif")
indice_can_smaller <- terra::aggregate(indice_can, fact = 10, fun = "modal")

indice_can_df <- as.data.frame(indice_can_smaller, xy = TRUE, na.rm = TRUE)

lvlsec_utm <- sf::st_transform(laval_sectors, crs = terra::crs(indice_can_smaller))
indice_tiles <- raster::projectRaster(tiles, crs = raster::crs(indice_can_smaller))

indice_can_map <-
  ggplot() +
  ggspatial::layer_spatial(indice_tiles, alpha = 0.7) +
  geom_tile(data = indice_can_df, aes(x = x, y = y, fill = factor(`650_IndiceCanopee_2023`))) +  
  scale_fill_manual(
    values = c("1" = "#fff7bc", "2" = "#feb24c", "3" = "#e5f5f9", "4" = "#99d8c9", "5" = "#575757"),
    labels = c("1" = "Minéral bas", "2" = "Minéral haut", "3" = "Végétal bas", "4" = "Végétal haut", "5" = "Aquatique"),
    na.value = curbcut_colors$left_5$fill[1]
  ) +
  labs(title = NULL) +
  geom_sf(data = lvlsec_utm, fill = "transparent", color = "black", size = 0.5) +
  c(list(
    coord_sf(xlim = c(sf::st_bbox(lvlsec_utm)["xmin"], sf::st_bbox(lvlsec_utm)["xmax"]), 
             ylim = c(sf::st_bbox(lvlsec_utm)["ymin"], sf::st_bbox(lvlsec_utm)["ymax"]))),
    gg_cc_theme_no_sf,
    list(theme_void()),
    list(default_theme),
    list(theme(legend.box.margin = margin(t = 0)))
  ) +
  theme(legend.title = element_blank())

indice_can_png <-
  ggplot() +
  ggspatial::layer_spatial(indice_tiles, alpha = 0.7) +
  geom_tile(data = indice_can_df, aes(x = x, y = y, fill = factor(`650_IndiceCanopee_2023`))) +  
  scale_fill_manual(
    values = c("1" = "#fff7bc", "2" = "#feb24c", "3" = "#e5f5f9", "4" = "#99d8c9", "5" = "#575757"),
    labels = c("1" = "Minéral bas", "2" = "Minéral haut", "3" = "Végétal bas", "4" = "Végétal haut", "5" = "Aquatique"),
    na.value = curbcut_colors$left_5$fill[1]
  ) +
  labs(title = NULL) +
  geom_sf(data = lvlsec_utm, fill = "transparent", color = "black", size = 0.5) +
  c(list(
    coord_sf(xlim = c(sf::st_bbox(lvlsec_utm)["xmin"], sf::st_bbox(lvlsec_utm)["xmax"]), 
             ylim = c(sf::st_bbox(lvlsec_utm)["ymin"], sf::st_bbox(lvlsec_utm)["ymax"]))),
    gg_cc_theme_no_sf,
    list(theme_void()),
    list(default_theme),
    list(theme(legend.box.margin = margin(t = 0)))
  ) +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 70),  # increase legend text size
    legend.key.size = unit(1.4, "cm")       # decrease icon size
  )


#PDF and PNG saves
ggsave(filename = here::here("output/axe1/climate/indice.pdf"),
       plot = indice_can_map, width = 5.5, height = 4)

ggsave(filename = here::here("output/axe1/climate/indice_can_map.png"),
       plot = indice_can_png, width = 19.25, height = 15, bg = "white")

# Flooding ----------------------------------------------------------------
#Vectors for the flooding
curbcut_flooding <- sf::st_read("data/axe1/climaterisk/zone_inondable_RCI_CDU_20240607_PG.shp") |> 
  sf::st_transform(crs = 4326) |> 
  mutate(LIMITE = gsub("Zone inondable ", "", LIMITE))
curbcut_flooding$LIMITE <- factor(curbcut_flooding$LIMITE, 
                                  levels = c("0-2 ans", "2-20 ans", "20-100 ans"))

#Flood map using the more granular data, not included in the first draft version 1
flood_plot <- 
ggplot(curbcut_flooding) +
  gg_cc_tiles +
  geom_sf(aes(fill = LIMITE), color = "transparent") +
  scale_fill_manual(values = curbcut_colors$left_5$fill[c(2,4,6)],
                    labels = c("0-2 ans", "2-20 ans", "20-100 ans")) +
  gg_cc_theme +
  theme(legend.title = element_blank())

ggplot2::ggsave(filename = here::here("output/axe1/climaterisk/flood_plot.pdf"),
                plot = flood_plot, width = 12, height = 6)


# Residential buildings
buildings <- qs::qread("data/buildings.qs")

zoning <- st_read("data/axe1/climaterisk/cdu_type_milieux.shp") |> 
  st_transform(crs = 4326) |> 
  select(TYPE_MILIE) |> 
  filter(TYPE_MILIE %in% c(unlist(sapply(paste0("T", 3:6, "."), paste0, 1:6, simplify = FALSE, USE.NAMES = FALSE)), 
                           "ZM", paste0("SEU.", 1:5))) |> 
  st_make_valid()

res_buildings <- buildings[buildings$ID %in% sf::st_filter(sf::st_centroid(buildings), zoning)$ID, ]

res_buildings_20_100 <- lengths(sf::st_intersects(
  res_buildings, 
  sf::st_union(curbcut_flooding))) |> sum()
res_buildings_2_20 <- lengths(sf::st_intersects(
  res_buildings, 
  sf::st_union(curbcut_flooding[curbcut_flooding$LIMITE %in% c("0-2 ans", "2-20 ans"), ]))) |> sum()
res_buildings_0_2 <- lengths(sf::st_intersects(
  res_buildings, 
  sf::st_union(curbcut_flooding[curbcut_flooding$LIMITE %in% c("0-2 ans"), ]))) |> sum()

res_buildings_20_100 <- convert_number(res_buildings_20_100)
res_buildings_2_20 <- convert_number(res_buildings_2_20)
res_buildings_0_2 <- convert_number(res_buildings_0_2)


# Save --------------------------------------------------------------------

qs::qsavem(ndvi_plot, flood_plot, res_buildings_20_100, res_buildings_2_20,
           res_buildings_0_2, file = "data/axe1/climate.qsm")
