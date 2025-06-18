#### 01 Startup ################################################################

library(tidyverse)
library(cancensus)
library(curbcut)
library(extrafont)
library(showtext)
library(sf)
library(gt)

# font_import()
# loadfonts(device = "win", quiet = TRUE)
# windowsFonts(`KMR-Apparat-Regular`=windowsFont("KMR-Apparat-Regular"))
# "KMR-Apparat-Regular" %in% names(windowsFonts())
font_add(family = "KMR-Apparat-Regular", regular = "data/fonts/KMR-Apparat-Regular.ttf")
showtext_auto()

source(here::here("R", "utils", "tt_fun.R"))

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
laval_sectors <- qs::qread(here::here("data", "geom_context", "secteur.qs"))
laval_sectors$old_name <- laval_sectors$name
laval_sectors$name <- c("Secteur 1 : Duvernay, Saint-François et Saint-Vincent-de-Paul",
                        "Secteur 6 : Vimont et Auteuil",
                        "Secteur 4 : Sainte-Dorothée, Laval-Ouest, Les Îles-Laval, Fabreville-Ouest et Laval-sur-le-Lac",
                        "Secteur 3 : Chomedey",
                        "Secteur 5 : Fabreville-Est et Sainte-Rose",
                        "Secteur 2 : Pont-Viau, Renaud-Coursol et Laval-des-Rapides")

tiles <- mapboxapi::get_static_tiles(
  location = lvlbbox, 
  username = "curbcut",
  zoom = 11,##
  style_id = "clz1cxvvi021f01nxef327y5p",#"cljkciic3002h01qveq5z1wrp",
  access_token = "pk.eyJ1IjoiY3VyYmN1dCIsImEiOiJjbGprYnVwOTQwaDAzM2xwaWdjbTB6bzdlIn0.Ks1cOI6v2i8jiIjk38s_kg",
  scaling_factor = "2x")

# Indesign fontsize (points) to ggplot sizes (mm)
indesign_fontsize <- 9
ggplot_fontsize <- 0.35278 * indesign_fontsize
indesign_title_fontsize <- 9
ggplot_title_fontsize <- 0.35278 * indesign_title_fontsize

gg_cc_tiles <- list(ggspatial::layer_spatial(tiles, alpha = 0.7))
default_theme <- theme(legend.position = "bottom",
                       legend.box = "horizontal",
                       legend.title = element_text(size = indesign_title_fontsize, 
                                                   family="KMR-Apparat-Regular"),
                       legend.text = element_text(size = indesign_fontsize, 
                                                  family="KMR-Apparat-Regular"),
                       legend.title.align = 0.5,
                       legend.text.align = 0.5,
                       text=element_text(size = indesign_fontsize, family="KMR-Apparat-Regular"), 
                       legend.box.margin = margin(t = -10))
gg_cc_theme_no_sf <- list(
  theme_minimal(),
  default_theme
)
gg_cc_theme <- c(list(
  geom_sf(data = laval_sectors, fill = "transparent", color = "black"),
  coord_sf(xlim = c(lvlbbox["xmin"], lvlbbox["xmax"]), 
           ylim = c(lvlbbox["ymin"], lvlbbox["ymax"]))),
  gg_cc_theme_no_sf,
  list(theme_void()),
  list(default_theme),
  list(theme(legend.box.margin = margin(t = 0)))
)
gg_cc_theme_nosecteurs <- c(list(
  # geom_sf(data = laval_sectors, fill = "transparent", color = "black"),
  coord_sf(xlim = c(lvlbbox["xmin"], lvlbbox["xmax"]), 
           ylim = c(lvlbbox["ymin"], lvlbbox["ymax"]))),
  gg_cc_theme_no_sf,
  list(theme_void()),
  list(default_theme),
  list(theme(legend.box.margin = margin(t = 0)))
)

# Helper function to make binned variables
add_bins <- function(df, variable, breaks, labels) {
  
  # Use cut to create the binned variable
  df$binned_variable <- cut(df[[variable]], 
                            breaks = breaks, 
                            include.lowest = TRUE, 
                            right = FALSE, drop = FALSE)
  
  # Assign the labels to the binned variable
  df$binned_variable <- factor(df$binned_variable, 
                               labels = labels)
  
  if (sum(is.na(df$binned_variable)) > 0) {
    df$binned_variable <- addNA(df$binned_variable)
  }
  
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
  x[!is.na(x)] <- curbcut::convert_unit(x = x[!is.na(x)])
  gsub(",", " ", x)
}

convert_number_tens <- \(x) convert_number(round(x / 10) * 10)

convert_number_noround <- function(x) {
  scales::comma(x, 1, accuracy = 1, big.mark = " ")
}

convert_pct <- function(x) {
  out <- curbcut:::convert_unit.pct(x = x, decimal = 1)
  out <- gsub("\\.", ",", out)
  out <- gsub("\\%", " %", out)
  for (i in seq_along(out)) {
    if (!grepl("\\,", out[[i]])) out[[i]] <- gsub(" %", ",0 %", out[[i]])
  }
  out
}


# Secteurs ----------------------------------------------------------------

library(ggplot2)
library(ggrepel)
library(sf)

# Use st_point_on_surface for label placement inside the geometry
laval_sectors_points <- st_point_on_surface(laval_sectors)

# Extract coordinates for centroids
laval_sectors_points_coords <- st_coordinates(laval_sectors_points)

# Convert to data frame and ensure unique column names
laval_sectors_points_df <- data.frame(
  name = laval_sectors$name,  # Keep only the necessary attributes
  X = laval_sectors_points_coords[, 1],
  Y = laval_sectors_points_coords[, 2]
)

# Create the ggplot
district_names <-
  ggplot(data = laval_sectors) +
  gg_cc_tiles +
  geom_sf(aes(fill = name), color = "white") +  # District polygons
  geom_label_repel(
    data = laval_sectors_points_df,  # Use clean data frame
    aes(x = X, y = Y, label = name),
    size = ggplot_fontsize,
    box.padding = 0.8,
    min.segment.length = 1,
    family = "KMR-Apparat-Regular"
  ) +
  scale_fill_manual(values = c(
    "Secteur 1 : Duvernay, Saint-François et Saint-Vincent-de-Paul" = color_theme("pinkhealth"),
    "Secteur 6 : Vimont et Auteuil" = color_theme("blueexplorer"),
    "Secteur 4 : Sainte-Dorothée, Laval-Ouest, Les Îles-Laval, Fabreville-Ouest et Laval-sur-le-Lac" = color_theme("greenecology"),
    "Secteur 3 : Chomedey" = color_theme("redhousing"),
    "Secteur 5 : Fabreville-Est et Sainte-Rose" = color_theme("yellowclimate"),
    "Secteur 2 : Pont-Viau, Renaud-Coursol et Laval-des-Rapides" = color_theme("purpletransport")
  )) +
  gg_cc_theme_nosecteurs +
  guides(fill = "none")

# Save the plot
#ggsave(plot = district_names, filename = "output/secteurs.pdf", width = 8, height = 5.5)



