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
dc_bike <- read_csv("D://Mcgill/can_cache/daycares/bike.csv") |> 
  mutate(bike = as.double(access_bicycle_daycarespots_20_2024),
         ID = as.character(ID)) |> 
  select(ID, bike)
dc_car <- read_csv("D://Mcgill/can_cache/daycares/car.csv") |> 
  mutate(car = as.double(access_car_daycarespots_20_2024),
         ID = as.character(ID)) |> 
  select(ID, car)
daycare <- st_read("D://Mcgill/can_cache/daycares/walk/access_data.shp") |> 
  rename("walk" = a___20_) |> 
  left_join(dc_bike, join_by(ID)) |> 
  left_join(dc_car, join_by(ID)) |> 
  left_join(lvl_sociodem, join_by(ID == GeoUID))

walkdaycare <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = daycare, aes(fill = walk), color = NA, show.legend = FALSE) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 1250, 2500, 3750, 5000, 6250, 7500, 8750, 10000), 
                       limits = c(0, 10000),
                       values = scales::rescale(c(0, 1250, 2500, 3750, 5000, 6250, 7500, 8750, 10000)),
                       oob = scales::squish) +
  theme_minimal() +
  labs(title = "À pied") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        legend.position = "bottom",
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.background = element_rect(fill = "lightblue")) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

bikedaycare <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = daycare, aes(fill = bike), color = NA) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 1250, 2500, 3750, 5000, 6250, 7500, 8750, 10000), 
                       limits = c(0, 10000),
                       values = scales::rescale(c(0, 1250, 2500, 3750, 5000, 6250, 7500, 8750, 10000)),
                       oob = scales::squish,
                       name = "Nombre de places de garderie accessibles en 20 minutes") +
  theme_minimal() +
  labs(title = "À vélo") +
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
                       breaks = c(0, 1250, 2500, 3750, 5000, 6250, 7500, 8750, 10000), 
                       limits = c(0, 10000),
                       values = scales::rescale(c(0, 1250, 2500, 3750, 5000, 6250, 7500, 8750, 10000)),
                       oob = scales::squish) +
  theme_minimal() +
  labs(title = "En voiture") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_blank(),
        legend.position = "bottom",
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.background = element_rect(fill = "lightblue")) +
  coord_sf(xlim = c(laval_bbox$xmin, laval_bbox$xmax),
           ylim = c(laval_bbox$ymin, laval_bbox$ymax))

daycare_map <- walkdaycare + bikedaycare + cardaycare +
  plot_annotation(title = "Nombre de places de garderie accessibles en 20 minutes") &
  theme(plot.title = element_text(hjust = 0.5))
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


# Health ------------------------------------------------------------------
health_bike <- read_csv("D://Mcgill/can_cache/health/bike.csv") |> 
  mutate(bike = as.double(access_bicycle_healthcare_total_20_2023),
         ID = as.character(ID)) |> 
  select(ID, bike)
health_car <- read_csv("D://Mcgill/can_cache/health/car.csv") |> 
  mutate(car = as.double(access_car_healthcare_total_20_2023),
         ID = as.character(ID)) |> 
  select(ID, car)
health <- st_read("D://Mcgill/can_cache/health/access_data.shp") |> 
  rename("walk" = a____20) |> 
  left_join(health_bike, join_by(ID)) |> 
  left_join(health_car, join_by(ID)) |> 
  left_join(lvl_sociodem, join_by(ID == GeoUID))

#Creating the map for walk accessibility
walkhealth <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = health, aes(fill = walk), color = NA, show.legend = FALSE) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50), 
                       limits = c(0, 50),
                       values = scales::rescale(c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)),
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
bikehealth <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = health, aes(fill = bike), color = NA) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50), 
                       limits = c(0, 50),
                       values = scales::rescale(c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)),
                       oob = scales::squish,
                       name = "Nombre d'établissements de santé et de services sociaux en 20 minutes") +
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
drivehealth <- ggplot() +
  geom_sf(data = mtlcma_sf, fill = "lightgrey") +
  geom_sf(data = health, aes(fill = car), color = NA, show.legend = FALSE) +
  geom_sf(data = laval_csd, fill = NA, color = "black") +
  scale_fill_gradientn(colors = curbcut_scale,
                       breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50), 
                       limits = c(0, 50),
                       values = scales::rescale(c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)),
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
health_map <- walkhealth + bikehealth + drivehealth +
  plot_annotation(title = "Nombre d'établissements de santé et de services sociaux en 20 minutes") &
  theme(plot.title = element_text(hjust = 0.5))
# Bivariate Map Functions -------------------------------------------------
legend_breaks <- function(basedata, xvariable){
  basedata <- basedata %>%
    mutate(x = !!sym(xvariable), y = !!sym("bike"))
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
    draw_plot(legend_name, 0.67, 0.0365, 0.32, 0.32)
}

# School Bivariate Maps ---------------------------------------------------
#Creating another column with bivariate legend coordinates for each combination
#of socio-demopgrahic vector and bike accessibility for the bivariate map and legend
school_bi <- bi_class(school, x = low_income, y = bike, style = "fisher", dim = 3) |> 
  mutate(school_low_income = bi_class) |> 
  select(-bi_class, -low_income)
school_bi <- bi_class(school_bi, x = under_15, y = bike, style = "fisher", dim = 3) |> 
  mutate(school_under_15 = bi_class) |> 
  select(-bi_class, -under_15)
school_bi <- bi_class(school_bi, x = `15_64`, y = bike, style = "fisher", dim = 3) |> 
  mutate(school_15_65 = bi_class) |> 
  select(-bi_class, -`15_64`)
school_bi <- bi_class(school_bi, x = `65_older`, y = bike, style = "fisher", dim = 3) |> 
  mutate(school_65_older = bi_class) |> 
  select(-bi_class, -`65_older`)
school_bi <- bi_class(school_bi, x = vis_min, y = bike, style = "fisher", dim = 3) |> 
  mutate(school_vis_min = bi_class) |> 
  select(-bi_class, -vis_min)
school_bi <- bi_class(school_bi, x = immigrant, y = bike, style = "fisher", dim = 3) |> 
  mutate(school_immigrant = bi_class) |> 
  select(-bi_class, -immigrant)
school_bi <- bi_class(school_bi, x = avg_rent, y = bike, style = "fisher", dim = 3) |> 
  mutate(school_rent = bi_class) |> 
  select(-bi_class, -avg_rent)
school_bi <- bi_class(school_bi, x = unafford_house, y = bike, style = "fisher", dim = 3) |> 
  mutate(school_house = bi_class) |> 
  select(-bi_class, -unafford_house)

#Bivariate map for school and low income
school_income_breaks <- legend_breaks(school, "low_income")
school_income_legend <- create_legend("Faible revenu", "Éducation", school_income_breaks)
school_income_map <- bi_map_create(school_bi, "school_low_income", "Accès à l’éducation et faibles revenus")
school_income_plot <- bi_finish(school_income_map, school_income_legend)

#Bivariate map for school and ages 15 and under
school_under15_breaks <- legend_breaks(school, "under_15")
school_under15_legend <- create_legend("Moins de 15 ans", "Éducation", school_under15_breaks)
school_under15_map <- bi_map_create(school_bi, "school_under_15", "Accès à l'éducation et personnes de moins de 15 ans")
school_under15_plot <- bi_finish(school_under15_map, school_under15_legend)

#Bivariate map for school and ages 15-65
school_1564_breaks <- legend_breaks(school, "15_64")
school_1564_legend <- create_legend("15-65 ans", "Éducation", school_1564_breaks)
school_1564_map <- bi_map_create(school_bi, "school_15_65", "Accès à l'éducation et personnes de 15 à 65 ans")
school_1564_plot <- bi_finish(school_1564_map, school_1564_legend)

#Bivariate map for school and ages 65 and older
school_older65_breaks <- legend_breaks(school, "65_older")
school_older65_legend <- create_legend("65 ans et plus", "Éducation", school_older65_breaks)
school_older65_map <- bi_map_create(school_bi, "school_65_older", "Accès à l’éducation et personnes de 65 ans et plus")
school_older65_plot <- bi_finish(school_older65_map, school_older65_legend)

#Bivariate map for school and visible minorities
school_vismin_breaks <- legend_breaks(school, "vis_min")
school_vismin_legend <- create_legend("Minorité visible", "Éducation", school_vismin_breaks)
school_vismin_map <- bi_map_create(school_bi, "school_vis_min", "Accès à l’éducation et proportion de minorités visibles")
school_vismin_plot <- bi_finish(school_vismin_map, school_vismin_legend)

#Bivariate map for school and immigrants
school_immigrant_breaks <- legend_breaks(school, "immigrant")
school_immigrant_legend <- create_legend("Immigrant", "Éducation", school_immigrant_breaks)
school_immigrant_map <- bi_map_create(school_bi, "school_immigrant", "Accès à l’éducation et population immigrée")
school_immigrant_plot <- bi_finish(school_immigrant_map, school_immigrant_legend)

#Bivariate map for school and rent
school_rent_breaks <- legend_breaks(school, "avg_rent")
school_rent_legend <- create_legend("Loyer moyen", "Éducation", school_rent_breaks)
school_rent_map <- bi_map_create(school_bi, "school_rent", "Accès à l’éducation et loyer moyen")
school_rent_plot <- bi_finish(school_rent_map, school_rent_legend)


# Daycare Bivariate Maps --------------------------------------------------
daycare_bi <- bi_class(daycare, x = low_income, y = bike, style = "fisher", dim = 3) |> 
  mutate(daycare_low_income = bi_class) |> 
  select(-bi_class, -low_income)
daycare_bi <- bi_class(daycare_bi, x = under_15, y = bike, style = "fisher", dim = 3) |> 
  mutate(daycare_under_15 = bi_class) |> 
  select(-bi_class, -under_15)
daycare_bi <- bi_class(daycare_bi, x = `15_64`, y = bike, style = "fisher", dim = 3) |> 
  mutate(daycare_15_65 = bi_class) |> 
  select(-bi_class, -`15_64`)
daycare_bi <- bi_class(daycare_bi, x = `65_older`, y = bike, style = "fisher", dim = 3) |> 
  mutate(daycare_65_older = bi_class) |> 
  select(-bi_class, -`65_older`)
daycare_bi <- bi_class(daycare_bi, x = vis_min, y = bike, style = "fisher", dim = 3) |> 
  mutate(daycare_vis_min = bi_class) |> 
  select(-bi_class, -vis_min)
daycare_bi <- bi_class(daycare_bi, x = immigrant, y = bike, style = "fisher", dim = 3) |> 
  mutate(daycare_immigrant = bi_class) |> 
  select(-bi_class, -immigrant)
daycare_bi <- bi_class(daycare_bi, x = avg_rent, y = bike, style = "fisher", dim = 3) |> 
  mutate(daycare_rent = bi_class) |> 
  select(-bi_class, -avg_rent)
daycare_bi <- bi_class(daycare_bi, x = unafford_house, y = bike, style = "fisher", dim = 3) |> 
  mutate(daycare_house = bi_class) |> 
  select(-bi_class, -unafford_house)

#Bivariate map for daycare and low income
daycare_income_breaks <- legend_breaks(daycare, "low_income")
daycare_income_legend <- create_legend("Faible revenu", "Garderies", daycare_income_breaks)
daycare_income_map <- bi_map_create(daycare_bi, "daycare_low_income", "Accès aux garderies et faibles revenus")
daycare_income_plot <- bi_finish(daycare_income_map, daycare_income_legend)

#Bivariate map for daycare and ages 15 and under
daycare_under15_breaks <- legend_breaks(daycare, "under_15")
daycare_under15_legend <- create_legend("Moins de 15 ans", "Garderies", daycare_under15_breaks)
daycare_under15_map <- bi_map_create(daycare_bi, "daycare_under_15", "Accès aux garderies et personnes de moins de 15 ans")
daycare_under15_plot <- bi_finish(daycare_under15_map, daycare_under15_legend)

#Bivariate map for daycare and ages 15-65
daycare_1564_breaks <- legend_breaks(daycare, "15_64")
daycare_1564_legend <- create_legend("15-65 ans", "Garderies", daycare_1564_breaks)
daycare_1564_map <- bi_map_create(daycare_bi, "daycare_15_65", "Accès aux garderies et personnes de 15 à 65 ans")
daycare_1564_plot <- bi_finish(daycare_1564_map, daycare_1564_legend)

#Bivariate map for daycare and ages 65 and older
daycare_older65_breaks <- legend_breaks(daycare, "65_older")
daycare_older65_legend <- create_legend("65 ans et plus", "Garderies", daycare_older65_breaks)
daycare_older65_map <- bi_map_create(daycare_bi, "daycare_65_older", "Accès aux garderies et personnes de 65 ans et plus")
daycare_older65_plot <- bi_finish(daycare_older65_map, daycare_older65_legend)

#Bivariate map for daycare and visible minorities
daycare_vismin_breaks <- legend_breaks(daycare, "vis_min")
daycare_vismin_legend <- create_legend("Minorité visible", "Garderies", daycare_vismin_breaks)
daycare_vismin_map <- bi_map_create(daycare_bi, "daycare_vis_min", "Accès aux garderies et proportion de minorités visibles")
daycare_vismin_plot <- bi_finish(daycare_vismin_map, daycare_vismin_legend)

#Bivariate map for daycare and immigrants
daycare_immigrant_breaks <- legend_breaks(daycare, "immigrant")
daycare_immigrant_legend <- create_legend("Immigrant", "Garderies", daycare_immigrant_breaks)
daycare_immigrant_map <- bi_map_create(daycare_bi, "daycare_immigrant", "Accès aux garderies et population immigrée")
daycare_immigrant_plot <- bi_finish(daycare_immigrant_map, daycare_immigrant_legend)

#Bivariate map for daycare and rent
daycare_rent_breaks <- legend_breaks(daycare, "avg_rent")
daycare_rent_legend <- create_legend("Loyer moyen", "Garderies", daycare_rent_breaks)
daycare_rent_map <- bi_map_create(daycare_bi, "daycare_rent", "Accès aux garderies et loyer moyen")
daycare_rent_plot <- bi_finish(daycare_rent_map, daycare_rent_legend)

# Health Bivariate Maps --------------------------------------------------
health_bi <- bi_class(health, x = low_income, y = bike, style = "fisher", dim = 3) |> 
  mutate(health_low_income = bi_class) |> 
  select(-bi_class, -low_income)
health_bi <- bi_class(health_bi, x = under_15, y = bike, style = "fisher", dim = 3) |> 
  mutate(health_under_15 = bi_class) |> 
  select(-bi_class, -under_15)
health_bi <- bi_class(health_bi, x = `15_64`, y = bike, style = "fisher", dim = 3) |> 
  mutate(health_15_65 = bi_class) |> 
  select(-bi_class, -`15_64`)
health_bi <- bi_class(health_bi, x = `65_older`, y = bike, style = "fisher", dim = 3) |> 
  mutate(health_65_older = bi_class) |> 
  select(-bi_class, -`65_older`)
health_bi <- bi_class(health_bi, x = vis_min, y = bike, style = "fisher", dim = 3) |> 
  mutate(health_vis_min = bi_class) |> 
  select(-bi_class, -vis_min)
health_bi <- bi_class(health_bi, x = immigrant, y = bike, style = "fisher", dim = 3) |> 
  mutate(health_immigrant = bi_class) |> 
  select(-bi_class, -immigrant)
health_bi <- bi_class(health_bi, x = avg_rent, y = bike, style = "fisher", dim = 3) |> 
  mutate(health_rent = bi_class) |> 
  select(-bi_class, -avg_rent)
health_bi <- bi_class(health_bi, x = unafford_house, y = bike, style = "fisher", dim = 3) |> 
  mutate(health_house = bi_class) |> 
  select(-bi_class, -unafford_house)

#Bivariate map for health and low income
health_income_breaks <- legend_breaks(health, "low_income")
health_income_legend <- create_legend("Faible revenu", "Établissements de santé", health_income_breaks)
health_income_map <- bi_map_create(health_bi, "health_low_income", "Accès aux établissements de santé et faibles revenus")
health_income_plot <- bi_finish(health_income_map, health_income_legend)
print(health_income_plot)

#Bivariate map for health and ages 15 and under
health_under15_breaks <- legend_breaks(health, "under_15")
health_under15_legend <- create_legend("Moins de 15 ans", "Établissements de santé", health_under15_breaks)
health_under15_map <- bi_map_create(health_bi, "health_under_15", "Accès aux établissements de santé et personnes de moins de 15 ans")
health_under15_plot <- bi_finish(health_under15_map, health_under15_legend)
print(health_under15_plot)

#Bivariate map for health and ages 15-65
health_1564_breaks <- legend_breaks(health, "15_64")
health_1564_legend <- create_legend("15-65 ans", "Établissements de santé", health_1564_breaks)
health_1564_map <- bi_map_create(health_bi, "health_15_65", "Accès aux établissements de santé et personnes de 15 à 65 ans")
health_1564_plot <- bi_finish(health_1564_map, health_1564_legend)
print(health_1564_plot)

#Bivariate map for health and ages 65 and older
health_older65_breaks <- legend_breaks(health, "65_older")
health_older65_legend <- create_legend("65 ans et plus", "Établissements de santé", health_older65_breaks)
health_older65_map <- bi_map_create(health_bi, "health_65_older", "Accès aux établissements de santé et personnes de 65 ans et plus")
health_older65_plot <- bi_finish(health_older65_map, health_older65_legend)
print(health_older65_plot)

#Bivariate map for health and visible minorities
health_vismin_breaks <- legend_breaks(health, "vis_min")
health_vismin_legend <- create_legend("Minorité visible", "Établissements de santé", health_vismin_breaks)
health_vismin_map <- bi_map_create(health_bi, "health_vis_min", "Accès aux établissements de santé et proportion de minorités visibles")
health_vismin_plot <- bi_finish(health_vismin_map, health_vismin_legend)
print(health_vismin_plot)

#Bivariate map for health and immigrants
health_immigrant_breaks <- legend_breaks(health, "immigrant")
health_immigrant_legend <- create_legend("Immigrant", "Établissements de santé", health_immigrant_breaks)
health_immigrant_map <- bi_map_create(health_bi, "health_immigrant", "Accès aux établissements de santé et population immigrée")
health_immigrant_plot <- bi_finish(health_immigrant_map, health_immigrant_legend)
print(health_immigrant_plot)

#Bivariate map for health and rent
health_rent_breaks <- legend_breaks(health, "avg_rent")
health_rent_legend <- create_legend("Loyer moyen", "Établissements de santé", health_rent_breaks)
health_rent_map <- bi_map_create(health_bi, "health_rent", "Accès aux établissements de santé et loyer moyen")
health_rent_plot <- bi_finish(health_rent_map, health_rent_legend)
print(health_rent_plot)