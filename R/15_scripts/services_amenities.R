#Loading up libraries
source("R/01_startup.R")
library(sf)
library(patchwork)
library(cowplot)
library(biscale)

#Setting up the cancensus api key
set_cancensus_api_key("CensusMapper_4308d496f011429cf814385050f083dc", install = TRUE)

set_cancensus_cache_path("D:/McGill/can_cache", install = TRUE, overwrite = TRUE)

#Laval shapefile for DAs
laval_csd <- cancensus::get_census(dataset = "CA21", 
                                  regions = list(CSD = 2465005), 
                                  level = "CSD", 
                                  geo_format = "sf")

laval_bbox <- st_bbox(laval_csd)
#Montreal CMA shapefile for context
mtlcma_sf <- cancensus::get_census(dataset = "CA21", 
                                   regions = list(CSD = 24462), 
                                   level = "CMA", 
                                   geo_format = "sf")

curbcut_scale <- c("#FFFFFF","#C4CDE1", "#98A8CB", "#6C83B5",
                   "#4C5C7F", "#3d4a66", "#252c3d", "#0c0f14")
curbcut_na <- "#B3B3BB"

# Sociodemographics to compare --------------------------------------------
#Grabbing all the census variables needed for comparison in this section
lvl_sociodem <- get_census(dataset = "CA21", 
                           regions = list(CSD = 2465005), 
                           vectors = c("low_income" = "v_CA21_1040", "under_15" = "v_CA21_11",
                                       "15_64" = "v_CA21_68", "65_older" = "v_CA21_251",
                                       "tot_pop_min" = "v_CA21_4872", "tot_min" = "v_CA21_4875",
                                       "tot_pop_imm" = "v_CA21_4404", "tot_imm" = "v_CA21_4410",
                                       "avg_rent" = "v_CA21_4318", "tot_pop_aff" = "v_CA21_4288",
                                       "tot_aff" = "v_CA21_4290"),
                           level = "DA") |> 
  mutate("vis_min" = `tot_min` * 100 / `tot_pop_min`,
         "immigrant" = `tot_imm` * 100 / `tot_pop_imm`,
         "unafford_house" = `tot_aff` * 100 / `tot_pop_aff`) |> 
  select(GeoUID, low_income, under_15, `15_64`, `65_older`,
         vis_min, immigrant, avg_rent, unafford_house)

# Day Care ----------------------------------------------------------------


# Schools -----------------------------------------------------------------
#Importing accessible educational institutions within 20 minutes for Laval by
#bike, car, and walking, and binding together. Taken from the curbcut website
school_bike <- read_csv("D://Mcgill/can_cache/schools/bike.csv") |> 
  mutate(bike = as.double(access_bicycle_educational_total_20_2023),
         ID = as.character(ID)) |> 
  select(ID, bike)
school_car <- read_csv("D://Mcgill/can_cache/schools/car.csv") |> 
  mutate(car = as.double(access_car_educational_total_20_2023),
         ID = as.character(ID)) |> 
  select(ID, car)
school <- st_read("D://Mcgill/can_cache/schools/walk/access_data.shp") |> 
  rename("walk" = a____20) |> 
  left_join(school_bike, join_by(ID)) |> 
  left_join(school_car, join_by(ID)) |> 
  left_join(lvl_sociodem, join_by(ID == GeoUID))

#Creating the map for walk accessibility
walkschool <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = school, aes(fill = walk), color = NA, show.legend = FALSE) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 15, 30, 45, 60, 75, 90, 105, 120), 
                       limits = c(0, 120),
                       values = scales::rescale(c(0, 15, 30, 45, 60, 75, 90, 105, 120)),
                       oob = scales::squish) +
  theme_minimal() +
  labs(title = "À pied") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        legend.position = "bottom",
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.background = element_rect(fill = "lightblue")) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))
  
#Creating the map for bike accessibility
bikeschool <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = school, aes(fill = bike), color = NA) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 15, 30, 45, 60, 75, 90, 105, 120), 
                       limits = c(0, 120),
                       values = scales::rescale(c(0, 15, 30, 45, 60, 75, 90, 105, 120)),
                       oob = scales::squish,
                       name = "Nombre d'établissements d'enseignement accessibles en 20 minutes") +
  theme_minimal() +
  labs(title = "À vélo") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        legend.position = "bottom", legend.justification = "center",
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.background = element_rect(fill = "lightblue")) +
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = 0.5, barwidth = 20, barheight = 0.5)) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

#Creating the map for car accessibility
driveschool <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = school, aes(fill = car), color = NA, show.legend = FALSE) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 15, 30, 45, 60, 75, 90, 105, 120), 
                       limits = c(0, 120),
                       values = scales::rescale(c(0, 15, 30, 45, 60, 75, 90, 105, 120)),
                       oob = scales::squish) +
    theme_minimal() +
    labs(title = "En voiture") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        legend.position = "bottom",
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.background = element_rect(fill = "lightblue")) +
    coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
             ylim = c(laval_bbox$ymin, laval_bbox$ymax))

#Combining the three plots together into one
school_map <- walkschool + bikeschool + driveschool +
  plot_annotation(title = "Nombre d'établissements d'enseignement accessibles en 20 minutes") &
  theme(plot.title = element_text(hjust = 0.5))

#Creating another column with bivariate legend coordinates for each combination
#of socio-demopgrahic vector and bike accessibility for the bivariate map and legend
school_bi <- bi_class(school, x = low_income, y = bike, style = "fisher", dim = 3) |> 
  mutate(school_low_income = bi_class) |> 
  select(-bi_class, -low_income)
school_bi <- bi_class(school_bi, x = under_15, y = bike, style = "fisher", dim = 3) |> 
  mutate(school_under_15 = bi_class) |> 
  select(-bi_class, -under_15)
school_bi <- bi_class(school_bi, x = vis_min, y = bike, style = "fisher", dim = 3) |> 
  mutate(school_vis_min = bi_class) |> 
  select(-bi_class, -vis_min)

#Grabbing legend breaks for school and low income
school_income_breaks <- bi_class_breaks(school, x = low_income, y = bike,
                                    style = "fisher", dim = 3, split = TRUE)

#Creating legend for school and low income
school_income_legend <- bi_legend(pal = "DkCyan2",
                                  dim = 3,
                                  xlab = "Faible revenu",
                                  ylab = "Éducation",
                                  flip_axes = TRUE,
                                  size = 10,
                                  breaks = school_bi_breaks)

#Mapping out the bivariate map for school and income
school_income_map <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "#9a9da1", color = "black") +
  geom_sf(data = school_bi, mapping = aes(fill = school_low_income), color = NA,
          size = 0.1, show.legend = FALSE) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  bi_scale_fill(pal = "DkCyan2", dim = 3, flip_axes = TRUE) +
  labs(title = "Accès à l’éducation et faibles revenus") +
  bi_theme() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "lightblue", color = NA)) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

#Combining the legend and map (600x500) for school and income
ggdraw() +
  draw_plot(school_income_map, 0, 0, 1, 1) +
  draw_plot(school_income_legend, 0.67, 0.0375, 0.32, 0.32)

#Grabbing legend breaks for school and ages 15 and under
school_under15_breaks <- bi_class_breaks(school, x = under_15, y = bike,
                                    style = "fisher", dim = 3, split = TRUE)

#Creating the legend for school and ages 15 and under
school_under15_legend <- bi_legend(pal = "DkCyan2",
                                  dim = 3,
                                  xlab = "Moins de 15 ans",
                                  ylab = "Éducation",
                                  flip_axes = TRUE,
                                  size = 10,
                                  breaks = school_under15_breaks)

#Mapping out the bivariate map for schools and 15 and under
school_under15_map <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "#9a9da1", color = "black") +
  geom_sf(data = school_bi, mapping = aes(fill = school_under_15), color = NA,
          size = 0.1, show.legend = FALSE) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  bi_scale_fill(pal = "DkCyan2", dim = 3, flip_axes = TRUE) +
  labs(title = "Accès à l'éducation et personnes de moins de 15 ans") +
  bi_theme() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "lightblue", color = NA)) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

#Combining the legend and map (600x500) for school and 15 and under
ggdraw() +
  draw_plot(school_under15_map, 0, 0, 1, 1) +
  draw_plot(school_under15_legend, 0.67, 0.0375, 0.32, 0.32)

#Grabbing legend breaks for school and visible minority
school_vismin_breaks <- bi_class_breaks(school, x = vis_min, y = bike,
                                        style = "fisher", dim = 3, split = TRUE)

#Creating legend for school and visible minorities
school_vismin_legend <- bi_legend(pal = "DkCyan2",
                                  dim = 3,
                                  xlab = "Minorité visible",
                                  ylab = "Éducation",
                                  flip_axes = TRUE,
                                  size = 10,
                                  breaks = school_vismin_breaks)

#Mapping out the bivariate map for school and visible minorities
school_vismin_map <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "#9a9da1", color = "black") +
  geom_sf(data = school_bi, mapping = aes(fill = school_vis_min), color = NA,
          size = 0.1, show.legend = FALSE) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  bi_scale_fill(pal = "DkCyan2", dim = 3, flip_axes = TRUE) +
  labs(title = "Accès à l’éducation et proportion de minorités visibles") +
  bi_theme() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "lightblue", color = NA)) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

#Combining the legend and map (600x500) for school and visible minorities
ggdraw() +
  draw_plot(school_vismin_map, 0, 0, 1, 1) +
  draw_plot(school_vismin_legend, 0.67, 0.0375, 0.32, 0.32)
