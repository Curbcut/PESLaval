source("R/01_startup.R")

# Data based on the image
years <- c(2019, 2020, 2021, 2022, 2023)
values <- c(14606, 14840, 16890, 17032, 18688)

# Create a data frame
df <- data.frame(years, values)

# Create the bar plot
crime_plot <- 
ggplot(df, aes(x = factor(years), y = values)) +
  geom_bar(stat = "identity", fill = color_theme("redhousing")) + 
  geom_text(aes(label = convert_number(values)), 
            vjust = 2, size = 3) +
  scale_y_continuous(labels = convert_number) +
  gg_cc_theme_no_sf +
  xlab("Année") +
  ylab("Nombre d'affaires criminelles")

ggplot2::ggsave(filename = here::here("output/axe2/crime_plot.pdf"), 
                plot = crime_plot, width = 6, height = 3, bg = "transparent")
