#Loading up libraries
source("R/01_startup.R")
library(sf)
library(patchwork)
library(cowplot)
library(biscale)
library(rlang)

#Setting up the cancensus api key
set_cancensus_api_key("CensusMapper_4308d496f011429cf814385050f083dc", install = TRUE)
set_cancensus_cache_path("D:/McGill/can_cache", install = TRUE, overwrite = TRUE)

#Laval shapefile for DAs
laval_csd <- cancensus::get_census(dataset = "CA21", 
                                  regions = list(CSD = 2465005), 
                                  level = "CSD", 
                                  geo_format = "sf")

#Bounds box for Laval for later use in maps
laval_bbox <- st_bbox(laval_csd)

#Montreal CMA shapefile for context
mtlcma_sf <- cancensus::get_census(dataset = "CA21", 
                                   regions = list(CSD = 24462), 
                                   level = "CMA", 
                                   geo_format = "sf")

#Custom scales for maps
curbcut_scale <- c("#FFFFFF","#C4CDE1", "#98A8CB", "#6C83B5",
                   "#4C5C7F", "#3d4a66", "#252c3d", "#0c0f14")
curbcut_na <- "#B3B3BB"

# Sociodemographics to compare --------------------------------------------
#Grabbing all the census variables needed for comparison in this section
lvl_sociodem <- get_census(dataset = "CA21", 
                           regions = list(CSD = 2465005), 
                           vectors = c("low_income" = "v_CA21_1040", "under_15_pop" = "v_CA21_11",
                                       "15_64_pop" = "v_CA21_68", "65_older_pop" = "v_CA21_251",
                                       "tot_age_pop" = "v_CA21_8", "tot_pop_min" = "v_CA21_4872",
                                       "tot_min" = "v_CA21_4875", "tot_pop_imm" = "v_CA21_4404",
                                       "tot_imm" = "v_CA21_4410", "avg_rent" = "v_CA21_4318",
                                       "tot_pop_aff" = "v_CA21_4288", "tot_aff" = "v_CA21_4290"),
                           level = "DA") |> 
  mutate("vis_min" = `tot_min` * 100 / `tot_pop_min`,
         "immigrant" = `tot_imm` * 100 / `tot_pop_imm`,
         "unafford_house" = `tot_aff` * 100 / `tot_pop_aff`,
         "under_15" = `under_15_pop` * 100 / `tot_age_pop`,
         "15_64" = `15_64_pop` * 100 / `tot_age_pop`,
         "65_older" = `65_older_pop` * 100 / `tot_age_pop`) |> 
  select(GeoUID, low_income, under_15, `15_64`, `65_older`,
         vis_min, immigrant, avg_rent, unafford_house)

# Day Care ----------------------------------------------------------------
dc_transit <- read_csv("D://Mcgill/can_cache/daycare/transit.csv") |> 
  mutate(transit = as.double(access_transit_pwd_daycarespots_15_2024),
         ID = as.character(ID)) |> 
  select(ID, transit)
dc_car <- read_csv("D://Mcgill/can_cache/daycare/car.csv") |> 
  mutate(car = as.double(access_car_daycarespots_15_2024),
         ID = as.character(ID)) |> 
  select(ID, car)
daycare <- st_read("D://Mcgill/can_cache/daycare/access_data.shp") |> 
  rename("walk" = a___15_) |> 
  left_join(dc_transit, join_by(ID)) |> 
  left_join(dc_car, join_by(ID)) |> 
  left_join(lvl_sociodem, join_by(ID == GeoUID))

walkdaycare <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = daycare, aes(fill = walk), color = NA, show.legend = FALSE) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 1200, 2400, 3600, 4800, 6000, 7200, 8400, 9600), 
                       limits = c(0, 9600),
                       values = scales::rescale(c(0, 1200, 2400, 3600, 4800, 6000, 7200, 8400, 9600)),
                       oob = scales::squish) +
  theme_minimal() +
  labs(title = "À pied") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        legend.position = "bottom",
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.background = element_rect(fill = "lightblue")) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

transitdaycare <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = daycare, aes(fill = transit), color = NA) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 1200, 2400, 3600, 4800, 6000, 7200, 8400, 9600), 
                       limits = c(0, 9600),
                       values = scales::rescale(c(0, 1200, 2400, 3600, 4800, 6000, 7200, 8400, 9600)),
                       oob = scales::squish,
                       name = "Nombre de places de garderie accessibles en 15 minutes") +
  theme_minimal() +
  labs(title = "Transport en commun") +
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = 0.5, barwidth = 20, barheight = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        legend.position = "bottom",
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.background = element_rect(fill = "lightblue")) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

cardaycare <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = daycare, aes(fill = car), color = NA, show.legend = FALSE) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 1200, 2400, 3600, 4800, 6000, 7200, 8400, 9600), 
                       limits = c(0, 9600),
                       values = scales::rescale(c(0, 1200, 2400, 3600, 4800, 6000, 7200, 8400, 9600)),
                       oob = scales::squish) +
  theme_minimal() +
  labs(title = "En voiture") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        legend.position = "bottom",
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.background = element_rect(fill = "lightblue")) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

daycare_map <- walkdaycare + transitdaycare + cardaycare +
  plot_annotation(title = "Nombre de places de garderie accessibles en 15 minutes") &
  theme(plot.title = element_text(hjust = 0.5))
print(daycare_map)
# Schools -----------------------------------------------------------------
#Importing accessible educational institutions within 20 minutes for Laval by
#bike, car, and walking, and binding together. Taken from the curbcut website
school_transit <- read_csv("D://Mcgill/can_cache/schools/transit.csv") |> 
  mutate(transit = as.double(access_transit_pwd_educational_total_15_2023),
         ID = as.character(ID)) |> 
  select(ID, transit)
school_car <- read_csv("D://Mcgill/can_cache/schools/car.csv") |> 
  mutate(car = as.double(access_car_educational_total_15_2023),
         ID = as.character(ID)) |> 
  select(ID, car)
school <- st_read("D://Mcgill/can_cache/schools/access_data.shp") |> 
  rename("walk" = a____15) |> 
  left_join(school_transit, join_by(ID)) |> 
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
  
#Creating the map for transit accessibility
transitschool <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = school, aes(fill = transit), color = NA) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 15, 30, 45, 60, 75, 90, 105, 120), 
                       limits = c(0, 120),
                       values = scales::rescale(c(0, 15, 30, 45, 60, 75, 90, 105, 120)),
                       oob = scales::squish,
                       name = "Nombre d'établissements d'enseignement accessibles en 15 minutes") +
  theme_minimal() +
  labs(title = "Transport en commun") +
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
school_map <- walkschool + transitschool + driveschool +
  plot_annotation(title = "Nombre d'établissements d'enseignement accessibles en 15 minutes") &
  theme(plot.title = element_text(hjust = 0.5))
print(school_map)

# Health ------------------------------------------------------------------
health_transit <- read_csv("D://Mcgill/can_cache/health/transit.csv") |> 
  mutate(transit = as.double(access_transit_pwd_healthcare_total_15_2023),
         ID = as.character(ID)) |> 
  select(ID, transit)
health_car <- read_csv("D://Mcgill/can_cache/health/car.csv") |> 
  mutate(car = as.double(access_car_healthcare_total_15_2023),
         ID = as.character(ID)) |> 
  select(ID, car)
health <- st_read("D://Mcgill/can_cache/health/access_data.shp") |> 
  rename("walk" = a____15) |> 
  left_join(health_transit, join_by(ID)) |> 
  left_join(health_car, join_by(ID)) |> 
  left_join(lvl_sociodem, join_by(ID == GeoUID))

#Creating the map for walk accessibility
walkhealth <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = health, aes(fill = walk), color = NA, show.legend = FALSE) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40), 
                       limits = c(0, 40),
                       values = scales::rescale(c(0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40)),
                       oob = scales::squish) +
  theme_minimal() +
  labs(title = "À pied") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        legend.position = "bottom",
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.background = element_rect(fill = "lightblue")) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

#Creating the map for transit accessibility
transithealth <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = health, aes(fill = transit), color = NA) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40), 
                       limits = c(0, 40),
                       values = scales::rescale(c(0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40)),
                       oob = scales::squish,
                       name = "Nombre d'établissements de santé et de services sociaux en 15 minutes") +
  theme_minimal() +
  labs(title = "Transport en commun") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        legend.position = "bottom", legend.justification = "center",
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.background = element_rect(fill = "lightblue")) +
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = 0.5, barwidth = 20, barheight = 0.5)) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

#Creating the map for car accessibility
drivehealth <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = health, aes(fill = car), color = NA, show.legend = FALSE) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40), 
                       limits = c(0, 40),
                       values = scales::rescale(c(0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40)),
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
health_map <- walkhealth + transithealth + drivehealth +
  plot_annotation(title = "Nombre d'établissements de santé et de services sociaux en 15 minutes") &
  theme(plot.title = element_text(hjust = 0.5))
print(health_map)


# Arenas ---------------------------------------------------------------
arena_transit <- read_csv("D://Mcgill/can_cache/arena/transit.csv") |> 
  mutate(transit = as.double(access_transit_pwd_arena_15_2024),
         ID = as.character(ID)) |> 
  select(ID, transit)
arena_car <- read_csv("D://Mcgill/can_cache/arena/car.csv") |> 
  mutate(car = as.double(access_car_arena_15_2024),
         ID = as.character(ID)) |> 
  select(ID, car)
arena <- st_read("D://Mcgill/can_cache/arena/access_data.shp") |> 
  rename("walk" = a___15_) |> 
  left_join(arena_transit, join_by(ID)) |> 
  left_join(arena_car, join_by(ID)) |> 
  left_join(lvl_sociodem, join_by(ID == GeoUID))

#Creating the map for walk accessibility
walkarena <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = arena, aes(fill = walk), color = NA, show.legend = FALSE) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 
                       limits = c(0, 9),
                       values = scales::rescale(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)),
                       oob = scales::squish) +
  theme_minimal() +
  labs(title = "À pied") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        legend.position = "bottom",
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.background = element_rect(fill = "lightblue")) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

#Creating the map for transit accessibility
transitarena <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = arena, aes(fill = transit), color = NA) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 
                       limits = c(0, 9),
                       values = scales::rescale(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)),
                       oob = scales::squish,
                       name = "Nombre d'arènes en 15 minutes") +
  theme_minimal() +
  labs(title = "Transport en commun") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        legend.position = "bottom", legend.justification = "center",
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.background = element_rect(fill = "lightblue")) +
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = 0.5, barwidth = 20, barheight = 0.5)) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

#Creating the map for car accessibility
drivearena <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = arena, aes(fill = car), color = NA, show.legend = FALSE) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 
                       limits = c(0, 9),
                       values = scales::rescale(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)),
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
arena_map <- walkarena + transitarena + drivearena +
  plot_annotation(title = "Nombre d'arènes en 15 minutes") &
  theme(plot.title = element_text(hjust = 0.5))
print(arena_map)

# Libraries ------------------------------------------------------------------
library_transit <- read_csv("D://Mcgill/can_cache/library/transit.csv") |> 
  mutate(transit = as.double(access_transit_pwd_bibliotheque_15_2024),
         ID = as.character(ID)) |> 
  select(ID, transit)
library_car <- read_csv("D://Mcgill/can_cache/library/car.csv") |> 
  mutate(car = as.double(access_car_bibliotheque_15_2024),
         ID = as.character(ID)) |> 
  select(ID, car)
library <- st_read("D://Mcgill/can_cache/library/access_data.shp") |> 
  rename("walk" = a___15_) |> 
  left_join(library_transit, join_by(ID)) |> 
  left_join(library_car, join_by(ID)) |> 
  left_join(lvl_sociodem, join_by(ID == GeoUID))

#Creating the map for walk accessibility
walklibrary <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = library, aes(fill = walk), color = NA, show.legend = FALSE) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 
                       limits = c(0, 9),
                       values = scales::rescale(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)),
                       oob = scales::squish) +
  theme_minimal() +
  labs(title = "À pied") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        legend.position = "bottom",
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.background = element_rect(fill = "lightblue")) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

#Creating the map for transit accessibility
transitlibrary <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = library, aes(fill = transit), color = NA) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 
                       limits = c(0, 9),
                       values = scales::rescale(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)),
                       oob = scales::squish,
                       name = "Nombre de bibliothèques en 15 minutes") +
  theme_minimal() +
  labs(title = "Transport en commun") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        legend.position = "bottom", legend.justification = "center",
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.background = element_rect(fill = "lightblue")) +
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = 0.5, barwidth = 20, barheight = 0.5)) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

#Creating the map for car accessibility
drivelibrary <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = library, aes(fill = car), color = NA, show.legend = FALSE) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 
                       limits = c(0, 9),
                       values = scales::rescale(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)),
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
library_map <- walklibrary + transitlibrary + drivelibrary +
  plot_annotation(title = "Nombre de bibliothèques en 15 minutes") &
  theme(plot.title = element_text(hjust = 0.5))
print(library_map)

# Community Centres ------------------------------------------------------------------
community_transit <- read_csv("D://Mcgill/can_cache/community/transit.csv") |> 
  mutate(transit = as.double(access_transit_pwd_centre_communautaire_15_2024),
         ID = as.character(ID)) |> 
  select(ID, transit)
community_car <- read_csv("D://Mcgill/can_cache/community/car.csv") |> 
  mutate(car = as.double(access_car_centre_communautaire_15_2024),
         ID = as.character(ID)) |> 
  select(ID, car)
community <- st_read("D://Mcgill/can_cache/community/access_data.shp") |> 
  rename("walk" = a____15) |> 
  left_join(community_transit, join_by(ID)) |> 
  left_join(community_car, join_by(ID)) |> 
  left_join(lvl_sociodem, join_by(ID == GeoUID))

#Creating the map for walk accessibility
walkcommunity <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = community, aes(fill = walk), color = NA, show.legend = FALSE) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40), 
                       limits = c(0, 40),
                       values = scales::rescale(c(0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40)),
                       oob = scales::squish) +
  theme_minimal() +
  labs(title = "À pied") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        legend.position = "bottom",
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.background = element_rect(fill = "lightblue")) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

#Creating the map for transit accessibility
transitcommunity <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = community, aes(fill = transit), color = NA) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40), 
                       limits = c(0, 40),
                       values = scales::rescale(c(0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40)),
                       oob = scales::squish,
                       name = "Nombre de centres communautaires en 15 minutes") +
  theme_minimal() +
  labs(title = "Transport en commun") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        legend.position = "bottom", legend.justification = "center",
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.background = element_rect(fill = "lightblue")) +
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = 0.5, barwidth = 20, barheight = 0.5)) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

#Creating the map for car accessibility
drivecommunity <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = community, aes(fill = car), color = NA, show.legend = FALSE) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40), 
                       limits = c(0, 40),
                       values = scales::rescale(c(0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40)),
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
community_map <- walkcommunity + transitcommunity + drivecommunity +
  plot_annotation(title = "Nombre de centres communautaires en 15 minutes") &
  theme(plot.title = element_text(hjust = 0.5))
print(community_map)


# Parks -------------------------------------------------------------------
park_transit <- read_csv("D://Mcgill/can_cache/park/transit.csv") |> 
  mutate(transit = as.double(access_transit_pwd_parc_15_2024),
         ID = as.character(ID)) |> 
  select(ID, transit)
park_car <- read_csv("D://Mcgill/can_cache/park/car.csv") |> 
  mutate(car = as.double(access_car_parc_15_2024),
         ID = as.character(ID)) |> 
  select(ID, car)
park <- st_read("D://Mcgill/can_cache/park/access_data.shp") |> 
  rename("walk" = a___15_) |> 
  left_join(park_transit, join_by(ID)) |> 
  left_join(park_car, join_by(ID)) |> 
  left_join(lvl_sociodem, join_by(ID == GeoUID))

#Creating the map for walk accessibility
walkpark <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = park, aes(fill = walk), color = NA, show.legend = FALSE) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 250), 
                       limits = c(0, 250),
                       values = scales::rescale(c(0, 20, 40, 60, 80, 100, 120, 140, 250)),
                       oob = scales::squish) +
  theme_minimal() +
  labs(title = "À pied") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        legend.position = "bottom",
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.background = element_rect(fill = "lightblue")) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

#Creating the map for transit accessibility
transitpark <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = park, aes(fill = transit), color = NA) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 250), 
                       limits = c(0, 250),
                       values = scales::rescale(c(0, 20, 40, 60, 80, 100, 120, 140, 250)),
                       oob = scales::squish,
                       name = "Nombre de parcs en 15 minutes") +
  theme_minimal() +
  labs(title = "Transport en commun") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        legend.position = "bottom", legend.justification = "center",
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.background = element_rect(fill = "lightblue")) +
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = 0.5, barwidth = 20, barheight = 0.5)) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

#Creating the map for car accessibility
drivepark <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = park, aes(fill = car), color = NA, show.legend = FALSE) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 250), 
                       limits = c(0, 250),
                       values = scales::rescale(c(0, 20, 40, 60, 80, 100, 120, 140, 250)),
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
park_map <- walkpark + transitpark + drivepark +
  plot_annotation(title = "Nombre de parcs en 15 minutes") &
  theme(plot.title = element_text(hjust = 0.5))
print(park_map)

# Public Places -----------------------------------------------------------
carart <- read_csv("D://McGill/can_cache/public/carart.csv") |> 
  select(-name, -name_2, -population, -households)
carpool <- read_csv("D://McGill/can_cache/public/carpool.csv") |> 
  select(-name, -name_2, -population, -households)
carrink <- read_csv("D://McGill/can_cache/public/carrink.csv") |> 
  select(-name, -name_2, -population, -households)
carsport <- read_csv("D://McGill/can_cache/public/carsport.csv") |> 
  select(-name, -name_2, -population, -households)
carwater <- read_csv("D://McGill/can_cache/public/carwater.csv") |> 
  select(-name, -name_2, -population, -households)
car_public <- carwater |> 
  left_join(carart, by = "ID") |> 
  left_join(carpool, by = "ID") |> 
  left_join(carrink, by = "ID") |> 
  left_join(carsport, by = "ID") |> 
  mutate(car = rowSums(across(-ID))) |> 
  select(ID, car)
transitart <- read_csv("D://McGill/can_cache/public/transitart.csv") |> 
  select(-name, -name_2, -population, -households)
transitpool <- read_csv("D://McGill/can_cache/public/transitpool.csv") |> 
  select(-name, -name_2, -population, -households)
transitrink <- read_csv("D://McGill/can_cache/public/transitrink.csv") |> 
  select(-name, -name_2, -population, -households)
transitsport <- read_csv("D://McGill/can_cache/public/transitsport.csv") |> 
  select(-name, -name_2, -population, -households)
transitwater <- read_csv("D://McGill/can_cache/public/transitwater.csv") |> 
  select(-name, -name_2, -population, -households)
transit_public <- transitwater |> 
  left_join(transitart, by = "ID") |> 
  left_join(transitpool, by = "ID") |> 
  left_join(transitrink, by = "ID") |> 
  left_join(transitsport, by = "ID") |> 
  mutate(transit = rowSums(across(-ID))) |> 
  select(ID, transit)
walkart <- read_csv("D://McGill/can_cache/public/walkart.csv") |> 
  select(-name, -name_2, -population, -households)
walkpool <- read_csv("D://McGill/can_cache/public/walkpool.csv") |> 
  select(-name, -name_2, -population, -households)
walkrink <- read_csv("D://McGill/can_cache/public/walkrink.csv") |> 
  select(-name, -name_2, -population, -households)
walkwater <- read_csv("D://McGill/can_cache/public/walkwater.csv") |> 
  select(-name, -name_2, -population, -households)
walksport <- st_read("D://McGill/can_cache/public/access_data.shp") |> 
  select(-name, -name_2) |> 
  mutate(ID = as.double(ID))
public <- walksport |> 
  left_join(walkart, by = "ID") |> 
  left_join(walkpool, by = "ID") |> 
  left_join(walkrink, by = "ID") |> 
  left_join(walkwater, by = "ID") |> 
  mutate(walk = a____15 + access_foot_centres_et_galeries_dart_15_2024 +
         access_foot_piscine_exterieure_15_2024 + access_foot_patinoire_exterieure_15_2024 +
         access_foot_jeux_deau_15_2024) |> 
  select(ID, popultn, walk) |> 
  left_join(transit_public, by = "ID") |> 
  left_join(car_public, by = "ID") |> 
  mutate(ID = as.character(ID)) |> 
  left_join(lvl_sociodem, join_by(ID == GeoUID))

walkpublic <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = public, aes(fill = walk), color = NA, show.legend = FALSE) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 15, 30, 45, 60, 75, 90, 105, 200), 
                       limits = c(0, 105),
                       values = scales::rescale(c(0, 15, 30, 45, 60, 75, 90, 105, 200)),
                       oob = scales::squish) +
  theme_minimal() +
  labs(title = "À pied") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        legend.position = "bottom",
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.background = element_rect(fill = "lightblue")) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

#Creating the map for transit accessibility
transitpublic <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = public, aes(fill = transit), color = NA) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 15, 30, 45, 60, 75, 90, 105, 200), 
                       limits = c(0, 105),
                       values = scales::rescale(c(0, 15, 30, 45, 60, 75, 90, 105, 200)),
                       oob = scales::squish,
                       name = "Nombre de lieux publics en 15 minutes") +
  theme_minimal() +
  labs(title = "Transport en commun") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        legend.position = "bottom", legend.justification = "center",
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.background = element_rect(fill = "lightblue")) +
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = 0.5, barwidth = 20, barheight = 0.5)) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

#Creating the map for car accessibility
drivepublic <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = public, aes(fill = car), color = NA, show.legend = FALSE) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 15, 30, 45, 60, 75, 90, 105, 200), 
                       limits = c(0, 105),
                       values = scales::rescale(c(0, 15, 30, 45, 60, 75, 90, 105, 200)),
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
public_map <- walkpublic + transitpublic + drivepublic +
  plot_annotation(title = "Nombre de lieux publics en 15 minutes") &
  theme(plot.title = element_text(hjust = 0.5))
print(public_map)

# Calculations ------------------------------------------------------------
#Importing walk and transit data for arenas
arena_walk <- read_csv("/Users/justin/Documents/R/curbcut/arenas/walk.csv")
arena_transit <- read_csv("/Users/justin/Documents/R/curbcut/arenas/transit.csv")

#Calculating the proportion of the population that had access to at least 1 arena
arena_walk_0 <- arena_walk |> 
  summarize(total_population = sum(population, na.rm = TRUE),
            access_0 = sum(population[access_foot_arena_15_2024 != 0], na.rm = TRUE)) |> 
  mutate(proportion = access_0 / total_population)
arena_transit_0 <- arena_transit |> 
  summarize(total_population = sum(population, na.rm = TRUE),
            access_0 = sum(population[access_transit_pwd_arena_15_2024 != 0], na.rm = TRUE)) |> 
  mutate(proportion = access_0 / total_population)

#Importing walk and transit data for community centres
community_walk <- read_csv("/Users/justin/Documents/R/curbcut/community/walk.csv")
community_transit <- read_csv("/Users/justin/Documents/R/curbcut/community/transit.csv")

#Calculating the proportion of the population that does not have access to at least 1 community centre
community_walk_0 <- community_walk |> 
  summarize(total_population = sum(population, na.rm = TRUE),
            access_0 = sum(population[access_foot_centre_communautaire_15_2024 == 0], na.rm = TRUE)) |> 
  mutate(proportion = access_0 / total_population)
community_transit_0 <- community_transit |> 
  summarize(total_population = sum(population, na.rm = TRUE),
            access_0 = sum(population[access_transit_pwd_centre_communautaire_15_2024 == 0], na.rm = TRUE)) |> 
  mutate(proportion = access_0 / total_population)

walk_park <- read_csv("D://Mcgill/can_cache/park/walk.csv")
transit_park <- read_csv("D://Mcgill/can_cache/park/transit.csv")

walk_park_0 <- walk_park |> 
  summarize(total_population = sum(population, na.rm = TRUE),
            access_0 = sum(population[access_foot_parc_15_2024 != 0], na.rm = TRUE)) |> 
  mutate(proportion = access_0 / total_population)

transit_park_0 <- transit_park |> 
  summarize(total_population = sum(population, na.rm = TRUE),
            access_0 = sum(population[access_transit_pwd_parc_15_2024 != 0], na.rm = TRUE)) |> 
  mutate(proportion = access_0 / total_population)

# Bivariate Map Functions -------------------------------------------------
legend_breaks <- function(basedata, xvariable){
  basedata <- basedata %>%
    mutate(x = !!sym(xvariable), y = !!sym("car"))
  bi_class_breaks(basedata, x = "x", y = "y", style = "fisher", dim = 3, split = TRUE)
}

create_legend <- function(xlabel, ylabel, breakvariable){
  bi_legend(pal = "DkCyan2", dim = 3, xlab = xlabel, ylab = ylabel,
            flip_axes = TRUE, size = 10, breaks = breakvariable)
}

#Function to create the map portion of the bivariate plot
bi_map_create <- function(basedata, bi_variable, titletext){
  ggplot() +
    geom_sf(data = mtlcma_sf, fill = "#9a9da1", color = "black") +
    geom_sf(data = basedata, mapping = aes_string(fill = bi_variable), color = NA,
            size = 0.1, show.legend = FALSE) +
    geom_sf(data = laval_csd, fill = NA, color = "black") +
    bi_scale_fill(pal = "DkCyan2", dim = 3, flip_axes = TRUE) +
    labs(title = titletext) +
    bi_theme() +
    theme(plot.title = element_text(hjust = 0.5, size = 14),
          axis.text = element_blank(), axis.title = element_blank(),
          axis.ticks = element_blank(), panel.grid = element_blank(),
          panel.background = element_rect(fill = "lightblue", color = NA)) +
    coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
             ylim = c(laval_bbox$ymin, laval_bbox$ymax))
}

#Function to combine map and legend. Map MUST BE 600x500
bi_finish <- function(map_name, legend_name){
  ggdraw() +
    draw_plot(map_name, 0, 0, 1, 1) +
    draw_plot(legend_name, 0.67, 0.025, 0.32, 0.32)
}
# Daycare Bivariate Maps ---------------------------------------------------
#Creating another column with bivariate legend coordinates for each combination
#of socio-demopgrahic vector and bike accessibility for the bivariate map and legend
daycare_bi <- bi_class(daycare, x = low_income, y = transit, style = "fisher", dim = 3) |> 
  mutate(daycare_low_income_transit = bi_class) |> 
  select(-bi_class, -low_income)
daycare_bi <- bi_class(daycare, x = avg_rent, y = car, style = "fisher", dim = 3) |> 
  mutate(daycare_rent_car = bi_class) |> 
  select(-bi_class)

#Bivariate map for daycare and low income by transit
daycare_income_breaks <- legend_breaks(daycare, "low_income")
daycare_income_legend <- create_legend("Faible revenu", "Garderie", daycare_income_breaks)
daycare_income_map <- bi_map_create(daycare_bi, "daycare_low_income_transit",
                                   "Accès aux garderies via les transports\n en commun et faible revenu")
daycare_income_plot <- bi_finish(daycare_income_map, daycare_income_legend)
print(daycare_income_plot)

daycare_rent_breaks <- legend_breaks(daycare, "avg_rent")
daycare_rent_legend <- create_legend("Loyer moyen", "Garderie", daycare_rent_breaks)
daycare_rent_map <- bi_map_create(daycare_bi, "daycare_rent_car",
                                 "Accès aux garderies via en voiture\n et loyer moyen")
daycare_rent_plot <- bi_finish(daycare_rent_map, daycare_rent_legend)
print(daycare_rent_plot)


# School Bivariate Maps ---------------------------------------------------
#Creating another column with bivariate legend coordinates for each combination
#of socio-demopgrahic vector and bike accessibility for the bivariate map and legend
school_bi <- bi_class(school, x = under_15, y = car, style = "fisher", dim = 3) |> 
  mutate(school_under_15_car = bi_class) |> 
  select(-bi_class, -low_income)
school_bi <- bi_class(school, x = `15_64`, y = transit, style = "fisher", dim = 3) |> 
  mutate(school_15_64_transit = bi_class) |> 
  select(-bi_class)

#Bivariate map for school and low income by transit
school_under_15_breaks <- legend_breaks(school, "under_15")
school_under_15_legend <- create_legend("15 ans et moins", "Éducation", school_under_15_breaks)
school_under_15_map <- bi_map_create(school_bi, "school_under_15_car",
                                   "Accès à l’éducation via en voiture\n et 15 ans et moins")
school_under_15_plot <- bi_finish(school_under_15_map, school_under_15_legend)
print(school_under_15_plot)

school_15_64_breaks <- legend_breaks(school, "15_64")
school_15_64_legend <- create_legend("15 à 64 ans", "Éducation", school_15_64_breaks)
school_15_64_map <- bi_map_create(school_bi, "school_15_64_transit",
                                     "Accès à l’éducation via en transport\n en commune et 15 à 64 ans")
school_15_64_plot <- bi_finish(school_15_64_map, school_15_64_legend)
print(school_15_64_plot)

# Healthcare Bivariate Maps -----------------------------------------------
health_bi <- bi_class(health, x = `65_older`, y = car, style = "fisher", dim = 3) |> 
  mutate(health_65_older_car = bi_class) |> 
  select(-bi_class)
health_bi <- bi_class(health, x = immigrant, y = transit, style = "fisher", dim = 3) |> 
  mutate(health_immigrant_transit = bi_class) |> 
  select(-bi_class)

health_65_older_breaks <- legend_breaks(health, "65_older")
health_65_older_legend <- create_legend("65 ans et plus", "Soins de santé", health_65_older_breaks)
health_65_older_map <- bi_map_create(health_bi, "health_65_older_car",
                                     "Accès aux soins en voiture\n et les 65 ans et plus")
health_65_older_plot <- bi_finish(health_65_older_map, health_65_older_legend)
print(health_65_older_plot)

health_immigrant_breaks <- legend_breaks(health, "immigrant")
health_immigrant_legend <- create_legend("Immigrant", "Soins de santé", health_immigrant_breaks)
health_immigrant_map <- bi_map_create(health_bi, "health_immigrant_transit",
                                     "Accès aux soins via en transport\n en commune et population immigrée")
health_immigrant_plot <- bi_finish(health_immigrant_map, health_immigrant_legend)
print(health_immigrant_plot)

# Library Bivariate -------------------------------------------------------
library_bi <- bi_class(library, x = `under_15`, y = walk, style = "fisher", dim = 3) |> 
  mutate(library_under_15_walk = bi_class) |> 
  select(-bi_class)
library_bi <- bi_class(library, x = `immigrant`, y = transit, style = "fisher", dim = 3) |> 
  mutate(library_low_transit = bi_class) |> 
  select(-bi_class)

library_under_15_breaks <- legend_breaks(library, "under_15")
library_under_15_legend <- create_legend("15 ans et moins", "Bibliothèques", library_under_15_breaks)
library_under_15_map <- bi_map_create(library_bi, "library_under_15_walk",
                                     "Accès aux bibliothèques à pied\n et 15 ans et moins")
library_under_15_plot <- bi_finish(library_under_15_map, library_under_15_legend)
print(library_under_15_plot)

library_low_transit_breaks <- legend_breaks(library, "low_income")
library_low_transit_legend <- create_legend("Immigrant", "Bibliothèques", library_low_transit_breaks)
library_low_transit_map <- bi_map_create(library_bi, "library_low_transit",
                                      "Accès aux bibliothèques via en transport\n en commune et population immigrée")
library_low_transit_plot <- bi_finish(library_low_transit_map, library_low_transit_legend)
print(library_low_transit_plot)

# Arena Bivariate ---------------------------------------------------------
arena_bi <- bi_class(arena, x = `under_15`, y = walk, style = "fisher", dim = 3) |> 
  mutate(arena_under_15_walk = bi_class) |> 
  select(-bi_class)
arena_bi <- bi_class(arena, x = `15_64`, y = car, style = "fisher", dim = 3) |> 
  mutate(arena_15_64_car = bi_class) |> 
  select(-bi_class)

arena_under_15_breaks <- legend_breaks(arena, "under_15")
arena_under_15_legend <- create_legend("15 ans et moins", "Arènes", arena_under_15_breaks)
arena_under_15_map <- bi_map_create(arena_bi, "arena_under_15_walk",
                                      "Accès aux arènes à pied\n et 15 ans et moins")
arena_under_15_plot <- bi_finish(arena_under_15_map, arena_under_15_legend)
print(arena_under_15_plot)

arena_15_64_breaks <- legend_breaks(arena, "15_64")
arena_15_64_legend <- create_legend("15 à 64 ans", "Arènes", arena_15_64_breaks)
arena_15_64_map <- bi_map_create(arena_bi, "arena_15_64_car",
                                    "Accès aux arènes à pied\n et 15 à 64 ans")
arena_15_64_plot <- bi_finish(arena_15_64_map, arena_15_64_legend)
print(arena_15_64_plot)

# Community Bivariate -----------------------------------------------------
community_bi <- bi_class(community, x = `65_older`, y = car, style = "fisher", dim = 3) |> 
  mutate(community_65_older_car = bi_class) |> 
  select(-bi_class)
community_bi <- bi_class(community, x = `avg_rent`, y = transit, style = "fisher", dim = 3) |> 
  mutate(community_rent_transit = bi_class) |> 
  select(-bi_class)

community_65_older_breaks <- legend_breaks(community, "65_older")
community_65_older_legend <- create_legend("65 ans et plus", "Centres communautaires", community_65_older_breaks)
community_65_older_map <- bi_map_create(community_bi, "community_65_older_car",
                                    "Accès aux centres communautaires en voiture\n et 65 ans et plus")
community_65_older_plot <- bi_finish(community_65_older_map, community_65_older_legend)
print(community_65_older_plot)

community_rent_breaks <- legend_breaks(community, "avg_rent")
community_rent_legend <- create_legend("Loyer moyen", "Centres communautaires", community_rent_breaks)
community_rent_map <- bi_map_create(community_bi, "community_rent_transit",
                                        "Accès aux centres communautaires en transport\n en commun et loyer moyen")
community_rent_plot <- bi_finish(community_rent_map, community_rent_legend)
print(community_rent_plot)

# Public Bivariate --------------------------------------------------------
public_bi <- bi_class(public, x = `15_64`, y = transit, style = "fisher", dim = 3) |> 
  mutate(public_15_64_transit = bi_class) |> 
  select(-bi_class)
public_bi <- bi_class(public, x = `low_income`, y = walk, style = "fisher", dim = 3) |> 
  mutate(public_low_walk = bi_class) |> 
  select(-bi_class)


public_15_64_breaks <- legend_breaks(public, "15_64")
public_15_64_legend <- create_legend("15 à 64 ans", "Les lieux publics", public_15_64_breaks)
public_15_64_map <- bi_map_create(public_bi, "public_15_64_transit",
                                        "Accès aux lieux publics en transport\n en commun et 15 à 65 ans")
public_15_64_plot <- bi_finish(public_15_64_map, public_15_64_legend)
print(public_15_64_plot)

public_low_breaks <- legend_breaks(public, "low_income")
public_low_legend <- create_legend("Faible revenu", "Les lieux publics", public_low_breaks)
public_low_map <- bi_map_create(public_bi, "public_low_walk",
                                  "Accès aux lieux publics à pied\n et faible revenu")
public_low_plot <- bi_finish(public_low_map, public_low_legend)
print(public_low_plot)

# Parks Bivariate ---------------------------------------------------------
park_bi <- bi_class(park, x = `under_15`, y = walk, style = "fisher", dim = 3) |> 
  mutate(park_under_15_walk = bi_class) |> 
  select(-bi_class)
park_bi <- bi_class(park, x = `low_income`, y = transit, style = "fisher", dim = 3) |> 
  mutate(park_low_transit = bi_class) |> 
  select(-bi_class)

park_under_15_breaks <- legend_breaks(park, "under_15")
park_under_15_legend <- create_legend("15 ans et moins", "Parcs", park_under_15_breaks)
park_under_15_map <- bi_map_create(park_bi, "park_under_15_walk",
                                    "Accès aux parcs à pied\n et 15 ans et moins")
park_under_15_plot <- bi_finish(park_under_15_map, park_under_15_legend)
print(park_under_15_plot)

park_low_breaks <- legend_breaks(park, "low_income")
park_low_legend <- create_legend("Faible revenu", "Parcs", park_low_breaks)
park_low_map <- bi_map_create(park_bi, "park_low_transit",
                                   "Accès aux parcs en transport\n en commun et 15 ans et Faible revenu")
park_low_plot <- bi_finish(park_low_map, park_low_legend)
print(park_low_plot)
