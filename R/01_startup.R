#### 01 Startup ################################################################

library(tidyverse)
library(cancensus)
library(curbcut)

source("R/utils/tt_fun.R")

# Addition of colors ------------------------------------------------------

curbcut_colors <- cc.buildr::build_colours()

# Colors from brandbook
curbcut_colors$brandbook <- 
  tibble::tibble(theme = c("greydata", "blueexplorer", "greenecology", "redhousing",
                           "pinkhealth", "purpletransport", "yellowclimate",
                           "greenurbanlife", "browndemographics"),
                 color = c("#F0F0F0", "#A3B0D1", "#73AD80", "#E08565", "#CD718C", 
                           "#C9C3FA", "#F5D574", "#ADB033", "#9E9090"))


# Plot texture ------------------------------------------------------------

lvl <- cancensus::get_census("CA21", regions = list(CSD = 2465005), 
                                  level = "CSD",
                                  geo_format = "sf")
lvlbbox <- sf::st_bbox(lvl)
laval_sectors <- qs::qread("data/geom_context/secteur.qs")

tiles <- mapboxapi::get_static_tiles(location = lvlbbox,
                                     username = "curbcut",
                                     zoom = 11,
                                     style_id = "cljkciic3002h01qveq5z1wrp",
                                     access_token = "pk.eyJ1IjoiY3VyYmN1dCIsImEiOiJjbGprYnVwOTQwaDAzM2xwaWdjbTB6bzdlIn0.Ks1cOI6v2i8jiIjk38s_kg",
                                     scaling_factor = "2x")

gg_cc_tiles <- list(ggspatial::layer_spatial(tiles, alpha = 0.7))
gg_cc_theme_no_sf <- list(
  theme_minimal(),
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.title.align = 0.5,
        legend.text.align = 0.5,
        text=element_text(family="KMR Apparat Regular"))
)
gg_cc_theme <- c(list(
  geom_sf(data = laval_sectors, fill = "transparent", color = "black"),
  coord_sf(xlim = c(lvlbbox["xmin"], lvlbbox["xmax"]), 
           ylim = c(lvlbbox["ymin"], lvlbbox["ymax"]))),
  gg_cc_theme_no_sf,
  list(theme_void())
)

# Helper function to make binned variables
add_bins <- function(df, variable, breaks, labels) {
  
  # Use cut to create the binned variable
  df$binned_variable <- cut(df[[variable]], 
                            breaks = breaks, 
                            include.lowest = TRUE, 
                            right = FALSE)
  
  # Assign the labels to the binned variable
  df$binned_variable <- factor(df$binned_variable, 
                               labels = labels)
  
  return(df)
}

color_theme <- function(theme) {
  curbcut_colors$brandbook$color[curbcut_colors$brandbook$theme == theme]
}

convert_hundreds <- function(x) {
  curbcut:::round_big_marks(
    x = x,
    min_dig = 5,
    scale_fun = scales::comma
  )
}

convert_number <- function(x) {
  out <- curbcut::convert_unit(x = x)
  gsub(",", " ", out)
}

convert_pct <- function(x) {
  out <- curbcut:::convert_unit.pct(x = x, decimal = 1)
  gsub("\\%", " %", out)
}
