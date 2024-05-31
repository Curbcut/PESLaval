### DEMOGRAPHY - POPULATION AND POPULATION DENSITY #############################

library(ggplot2)
#geom_context <- qs::qread("data/geom_context/geom_context.qs")

# Population count --------------------------------------------------------

# To get raw census data, we use the cancensus package. At this link, there is 
# documentation on how to get and save an API key: https://mountainmath.github.io/cancensus/

# We grab census information from Laval. This small call lets us retrieve 
# population counts for the whole city.
laval_census <- cancensus::get_census(dataset = "CA21", 
                                      regions = list(CSD = 2465005), 
                                      level = "CSD")

# curbcut::convert_unit lets us convert any type and number to 'Pretty' number. 
# To use curbcut functions, install the package through `devtools::install_github("Curbcut/curbcut")`
laval_population <- laval_census$Population
laval_population_pretty <- curbcut::convert_unit(x = laval_population)

# Another example is to use its method pct, or dollar
curbcut:::convert_unit.pct(x = 0.4555, decimal = 1)
curbcut:::convert_unit.dollar(x = 102512)


# Population density graph ------------------------------------------------

CT <- cancensus::get_census(dataset = "CA21", 
                            regions = list(CSD = 2465005), 
                            level = "CT", 
                            geo_format = "sf")

# Prepare a function that calculates population density. We will use it many
# times in this chapter.
pop_density_fun <- function(x, col_name = "pop_density") {
  # Calculate the area, in meters, of every feature
  x$area <- cc.buildr::get_area(x) / 1000000 # Converting square meters to square kilometers
  
  # Get population density
  x[[col_name]] <- x$Population / x$area
  
  # Return the df
  x
}

# Get population density
CT <- pop_density_fun(CT)


# Make a plot of density
pop_density_plot <- 
  ggplot(data = CT) +
  geom_sf(aes(fill = pop_density), color = "transparent") +
  scale_fill_viridis_c(
    option = "viridis", 
    direction = -1, 
    name = "Population density\n(per km²)",
    labels = \(x) curbcut::convert_unit(x = x),
    guide = guide_colourbar(barwidth = 15),
    limits = c(1000, 6000),
    oob = scales::oob_squish
  ) +
  theme(legend.position = "bottom")

  #geom_context(sf_focus = CT) +
  theme(legend.position = "bottom")

# Save the plot so it's in a good quality
ggplot2::ggsave(filename = here::here("output/0_demography/CT_pop_density.png"), 
                plot = pop_density_plot, width = 6, height = 4)


# Population density change -----------------------------------------------

# Get 2016 and 2021 population density in dissemination blocks
CT <- cancensus::get_census(dataset = "CA21", 
                            regions = list(CSD = 2465005), 
                            level = "CT", 
                            geo_format = "sf")
CT_2016 <- cancensus::get_census(dataset = "CA16", 
                                 regions = list(CSD = 2465005), 
                                 level = "CT", 
                                 geo_format = "sf")

# Interpolate 2016 population to 2021 spatial features
names(CT)[names(CT) == "GeoUID"] <- "ID"
names(CT_2016)[names(CT_2016) == "GeoUID"] <- "ID"
CT_2016 <- cc.buildr::interpolate_from_area(to = CT, 
                                            from = CT_2016, 
                                            additive = "Population", 
                                            crs = 32618)

# Calculate population density on the two years
CT <- pop_density_fun(CT)
CT_2016 <- pop_density_fun(CT_2016, col_name = "pop_density_2016")

# Merge 2016 population density with the 2021
CT <- merge(CT, sf::st_drop_geometry(CT_2016)[c("ID", "pop_density_2016")], by = "ID")

# Get change in population density
CT$pop_density_change <- CT$pop_density - CT$pop_density_2016

# Prepare the plot
# Define the new breaks with symmetry and without empty ranges
breaks <- c(-Inf, -300, -100, -50, -25, 25, 50, 100, 300, Inf)

# Define the new labels reflecting the breaks without empty categories
labels <- c("Perte de plus de 300", 
            "Perte de 100 à 300", 
            "Perte de 50 à 100",
            "Perte de 25 à 50", 
            "Environ identique",
            "Gain de 25 à 50", 
            "Gain de 50 à 100",
            "Gain de 100 à 300", 
            "Gain de plus de 300")

# Define the colors, making sure there is a color for each category
# Assuming viridis gives a range of colors from which we can take a subset
colors <- rev(viridis::viridis(length(labels)))
colors[which(labels == "Environ identique")] <- "white" # Set 'About the same' to white


# Create a factor variable based on the defined breaks
CT$pop_density_change_cat <- cut(CT$pop_density_change, breaks = breaks, 
                                 labels = labels, right = FALSE)
CT$pop_density_change_cat <- factor(CT$pop_density_change_cat, levels = labels)

# Switch to full geo (spanning under water!)
CT <- cc.data::census_switch_full_geo(CT, scale_name = "CT")
CT <- sf::st_transform(CT, crs = 32618)

# Generate the plot with manual color scaling
pop_density_change_plot <- 
  ggplot(data = CT) +
  geom_sf(aes(fill = pop_density_change_cat), linewidth=0.01, 
          color = "transparent") +
  scale_fill_manual(
    values = colors,
    name = "Population density\nchange (per km²)",
    labels = labels,
    limits = labels, 
    drop = FALSE,
    guide = guide_legend(title.position = "top", title.hjust = 0.5)
  ) +
  theme(legend.position = "right")
  #geom_context(sf_focus = CT) +
  theme(legend.position = "right")

# Save the plot so it's in a good quality
ggplot2::ggsave(filename = here::here("output/0_demography/CT_pop_density_change.png"), 
                plot = pop_density_change_plot, width = 6, height = 4)


# Save the objects that will be referred in the text ----------------------

qs::qsavem(laval_population_pretty, pop_density_plot, 
           pop_density_change_plot,
           file = "data/demography/pop_and_density.qsm")
