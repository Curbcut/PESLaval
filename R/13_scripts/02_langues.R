### LANGUES ####################################################################
source("R/01_startup.R")

# 1996 Census Data - Knowledge of official languages
LOffLang96 <- cancensus::get_census(
  dataset = "CA1996",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(
    "Total" = "v_CA1996_310",
    "English" = "v_CA1996_311",
    "French" = "v_CA1996_312",
    "Both" = "v_CA1996_313",
    "Neither" = "v_CA1996_314"))

# 2001 Census Data - Knowledge of official languages
LOffLang01 <- cancensus::get_census(
  dataset = "CA01",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(
    "Total" = "v_CA01_213",
    "English" = "v_CA01_214",
    "French" = "v_CA01_215",
    "Both" = "v_CA01_216",
    "Neither" = "v_CA01_217"))

# 2006 Census Data - Knowledge of Official languages
LOffLan06 <- cancensus::get_census(
  dataset = "CA06",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(
    "Total" = "v_CA06_243",
    "English" = "v_CA06_244",
    "French" = "v_CA06_245",
    "Both" = "v_CA06_246",
    "Neither" = "v_CA06_247"))

# 2011 Census Data - Knowledge of Official Languages in Laval
LOffLang11 <- cancensus::get_census(
  dataset = "CA11",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(
    "Total" = "v_CA11F_551",
    "English" = "v_CA11F_554",
    "French" = "v_CA11F_557",
    "Both" = "v_CA11F_560",
    "Neither" = "v_CA11F_563"))

# 2016 Census Data - Knowledge of Official Languages in Laval
LOffLang16 <- cancensus::get_census(
  dataset = "CA16",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(
    "Total" = "v_CA16_512",
    "English" = "v_CA16_515",
    "French" = "v_CA16_518",
    "Both" = "v_CA16_521",
    "Neither" = "v_CA16_524"))

# 2021 Census Data - Knowledge of Official Languages in Laval
LOffLang21 <- cancensus::get_census(
  dataset = "CA21",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(
    "Total" = "v_CA21_1144",
    "English" = "v_CA21_1147",
    "French" = "v_CA21_1150",
    "Both" = "v_CA21_1153",
    "Neither" = "v_CA21_1156"))

#census Data for QC 2021
QCoffLang21 <- cancensus::get_census(
  dataset = "CA21",
  regions = list(PR = 24),
  level = "PR",
  vectors = c(
    "Total" = "v_CA21_1144",
    "English" = "v_CA21_1147",
    "French" = "v_CA21_1150",
    "Both" = "v_CA21_1153",
    "Neither" = "v_CA21_1156"))

#Define function to calculate percentages
calculate_percentages <- function(data) {
  total_population <- data$Total
  data <- data %>%
    mutate(across(English:Neither, ~. / total_population*100))
  return(data)
}

#calculate percentages for each region and year
PerLoffLang96 <- calculate_percentages(LOffLang96)
PercLoffLang01 <- calculate_percentages(LOffLang01)
PercLoffLang06 <- calculate_percentages(LOffLan06)
PercLoffLang11 <- calculate_percentages(LOffLang11)
PerLoffLang16 <- calculate_percentages(LOffLang16)
PerLoffLang21 <-  calculate_percentages(LOffLang21)
PerQCoffLang21 <- calculate_percentages(QCoffLang21)

# tidy the dataframes of the percentages by doing pivot_longer 
LtidyOffLang96 <- pivot_longer(PerLoffLang96, cols = c(English:Neither), 
                               names_to = "Language", values_to = "Percentage")
LtidyOffLang01 <- pivot_longer(PercLoffLang01, cols = c(English:Neither),
                               names_to = "Language", values_to = "Percentage")
LtidyOffLang06 <- pivot_longer(PercLoffLang06, cols = c(English:Neither),
                               names_to = "Language", values_to = "Percentage")
LtidyOffLang11 <- pivot_longer(PercLoffLang11, cols = c(English:Neither),
                               names_to = "Language", values_to = "Percentage")
LtidyOffLang16 <-  pivot_longer(PerLoffLang16, cols = c(English:Neither), 
                                names_to = "Language", values_to = "Percentage")
LtidyOffLang21 <- pivot_longer(PerLoffLang21, cols = c(English:Neither),
                               names_to = "Language", values_to = "Percentage")
QCtidyOffLang21 <- pivot_longer(PerQCoffLang21, cols = c(English:Neither),
                                names_to = "Language", values_to = "Percentage")

# Mutate to add the years to each data frame by adding a column
tidyLOLang96 <- LtidyOffLang96 |> 
  mutate(Year = "1996")
tidyLOlang01 <- LtidyOffLang01 |> 
  mutate(Year = "2001")
tidyLOLang06 <- LtidyOffLang06 |> 
  mutate(Year= "2006")
tidyLOLang11 <- LtidyOffLang11 |> 
  mutate(Year = "2011")
tidyLOLang16 <- LtidyOffLang16 |> 
  mutate(Year = "2016")
tidyLOLang21 <- LtidyOffLang21 |> 
  mutate(Year = "2021")

# need to add laval and quebec as the region
tidyLOLang21 <- tidyLOLang21 |> 
  mutate(Region = "Laval")
tidyQCLang21 <- QCtidyOffLang21 |> 
  mutate(Region = "Quebec")

# Combine the pivot tables - use bind_rows function [comb2021offlang <- bind_rows(TidyPerLoffLang16, TidyPerLoffLang21)]
combinedKnowOffLang <- bind_rows(tidyLOLang96, tidyLOlang01, tidyLOLang06, tidyLOLang11, tidyLOLang16, tidyLOLang21)

# combine the pivot tables using bind rows
knowledgeLang21 <- bind_rows(tidyLOLang21, tidyQCLang21)

combinedKnowOffLang$Language <- factor(combinedKnowOffLang$Language,
                                       levels = c("Neither", "English", "French", "Both"),
                                       labels = c("Aucune", "Anglais", "Français", "Les deux"))

knowledge_official <- 
  ggplot(combinedKnowOffLang, aes(x = Year, y = Percentage, fill = Language)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = convert_pct) +
  geom_text(aes(label = convert_pct(Percentage/100)),
            position = position_fill(vjust = 0.5),
            size = 3,
            color = "black") +
  ylab(NULL) +
  xlab(NULL) +
  scale_fill_manual(values = c("Aucune" = color_theme("yellowclimate"), 
                               "Anglais" = color_theme("pinkhealth"), 
                               "Français" = color_theme("blueexplorer"), 
                               "Les deux" = color_theme("greenecology"))) +
  guides(fill = guide_legend(reverse = TRUE)) +
  gg_cc_theme_no_sf +
  theme(legend.title = element_blank())

ggplot2::ggsave(filename = here::here("output/axe1/langues/knowledge_official.pdf"), 
                plot = knowledge_official, width = 6.5, height = 6.5)

# Values for the text
bilingual_1996 <- combinedKnowOffLang$Percentage[
  combinedKnowOffLang$Year == 1996 & combinedKnowOffLang$Language == "Les deux"
]
bilingual_1996 <- convert_pct(bilingual_1996 / 100)
bilingual_2021 <- combinedKnowOffLang$Percentage[
  combinedKnowOffLang$Year == 2021 & combinedKnowOffLang$Language == "Les deux"
]
bilingual_2021 <- convert_pct(bilingual_2021 / 100)
no_official_1996 <- combinedKnowOffLang$Percentage[
  combinedKnowOffLang$Year == 1996 & combinedKnowOffLang$Language == "Aucune"
]
no_official_1996 <- convert_pct(no_official_1996 / 100)
no_official_2021 <- combinedKnowOffLang$Percentage[
  combinedKnowOffLang$Year == 2021 & combinedKnowOffLang$Language == "Aucune"
]
no_official_2021 <- convert_pct(no_official_2021 / 100)

know_fr_laval <- knowledgeLang21$Percentage[
  knowledgeLang21$`Region Name` == "Laval (V)" & knowledgeLang21$Language %in% c("French", "Both")
]
know_fr_laval <- convert_pct(sum(know_fr_laval) / 100)

know_fr_qc <- knowledgeLang21$Percentage[
  knowledgeLang21$`Region Name` == "Quebec (Que.)" & knowledgeLang21$Language %in% c("French", "Both")
]
know_fr_qc <- convert_pct(sum(know_fr_qc) / 100)

know_bilingual_laval <- knowledgeLang21$Percentage[
  knowledgeLang21$`Region Name` == "Laval (V)" & knowledgeLang21$Language %in% "Both"
]
know_bilingual_laval <- convert_pct(sum(know_bilingual_laval) / 100)

know_bilingual_qc <- knowledgeLang21$Percentage[
  knowledgeLang21$`Region Name` == "Quebec (Que.)" & knowledgeLang21$Language %in% "Both"
]
know_bilingual_qc <- convert_pct(sum(know_bilingual_qc) / 100)

know_only_fr_laval <- knowledgeLang21$Percentage[
  knowledgeLang21$`Region Name` == "Laval (V)" & knowledgeLang21$Language %in% "French"
]
know_only_fr_laval <- convert_pct(sum(know_only_fr_laval) / 100)

know_only_fr_qc <- knowledgeLang21$Percentage[
  knowledgeLang21$`Region Name` == "Quebec (Que.)" & knowledgeLang21$Language %in% "French"
]
know_only_fr_qc <- convert_pct(sum(know_only_fr_qc) / 100)

know_only_en_laval <- knowledgeLang21$Percentage[
  knowledgeLang21$`Region Name` == "Laval (V)" & knowledgeLang21$Language %in% "English"
]
know_only_en_laval <- convert_pct(sum(know_only_en_laval) / 100)

# Laval vs Quebec
knowledgeLang21$Language <- factor(knowledgeLang21$Language,
                                       levels = c("Neither", "English", "French", "Both"),
                                       labels = c("Aucune", "Anglais", "Français", "Les deux"))

# now to plot and compare laval to quebec in 2021 
know_official_laval_qc_diff <- 
  ggplot(knowledgeLang21, aes(x=Region, y=Percentage, fill= Language)) +
  geom_bar(stat="identity", position = "fill") +
  scale_y_continuous(labels = convert_pct) +
  geom_text(aes(label = convert_pct(Percentage/100)),
            position = position_fill(vjust = 0.5),
            size = 3,
            color = "black") +
  ylab(NULL) +
  xlab(NULL) +
  scale_fill_manual(values = c("Aucune" = color_theme("yellowclimate"), 
                               "Anglais" = color_theme("pinkhealth"), 
                               "Français" = color_theme("blueexplorer"), 
                               "Les deux" = color_theme("greenecology"))) +
  guides(fill = guide_legend(reverse = TRUE)) +
  gg_cc_theme_no_sf +
  theme(legend.title = element_blank())

ggplot2::ggsave(filename = here::here("output/axe1/langues/know_official_laval_qc_diff.pdf"), 
                plot = know_official_laval_qc_diff, width = 4, height = 6.5)


# Languages spoken (most) at Home ------------------------------------------------

homelang96vector <- c(
  "Total" = "v_CA1996_323",
  "English" = "v_CA1996_324",
  "French" = "v_CA1996_325",
  "Non-official" = "v_CA1996_326")
homelang96_laval <- get_census(dataset = "CA1996",
                               regions = list(CSD = 2465005),
                               level = "CSD",
                               vectors = homelang96vector)

homelang01vector <- c(
  "Total" = "v_CA01_226",
  "English" = "v_CA01_227",
  "French" = "v_CA01_228",
  "Non-official" = "v_CA01_229")
homelang01_laval <- get_census(dataset = "CA01",
                               regions = list(CSD = 2465005),
                               level = "CSD",
                               vectors = homelang01vector)

homelang06vector <-  c(
  "Total" = "v_CA06_256",
  "English" = "v_CA06_257",
  "French" = "v_CA06_258",
  "Non-official" = "v_CA06_259")
homelang06_laval <- get_census(dataset = "CA06",
                               regions = list(CSD = 2465005),
                               level = "CSD",
                               vectors = homelang06vector)

homelang11vector <-  c(
  "Total" = "v_CA11F_590",
  "English" = "v_CA11F_593",
  "French" = "v_CA11F_596",
  "Non-official" = "v_CA11F_599") 
homelang11_laval <- get_census(dataset = "CA16",
                               regions = list(CSD = 2465005),
                               level = "CSD",
                               vectors = homelang11vector)

homelang16vector <-  c(
  "Total" = "v_CA16_1358",
  "English" = "v_CA16_1364",
  "French" = "v_CA16_1367",
  "Non-official" = "v_CA16_1370") 
homelang16_laval <- get_census(dataset = "CA16",
                               regions = list(CSD = 2465005),
                               level = "CSD",
                               vectors = homelang16vector)

homelang21vector <- c(
  "Total" = "v_CA21_2203",
  "English" = "v_CA21_2209",
  "French" = "v_CA21_2212",
  "Non-official" = "v_CA21_2215")  
homelang21_laval <- get_census(dataset = "CA21",
                               regions = list(CSD = 2465005),
                               level = "CSD",
                               vectors = homelang21vector)

# also show for the province
homelang21_qc <- cancensus::get_census(
  dataset = "CA21",
  regions = list(PR = 24),
  level = "PR",
  vectors = c(homelang21vector))

#merge the laval datasets 
lavalhomelang <- 
  bind_rows(homelang96_laval, 
            homelang01_laval, 
            homelang06_laval, 
            homelang11_laval,
            homelang16_laval, 
            homelang21_laval) |> 
  mutate(year = c("1996", "2001", "2006", "2011", "2016", "2021"))

#merge the 2021 datasetst to compare Laval and Quebec
homelang21 <- 
  bind_rows(
    homelang21_laval,
    homelang21_qc) |> 
  mutate(region = c("Laval", "Quebec"))

# make sure that all the values are numeric
lavalhomelang <- lavalhomelang %>%
  mutate(across(c(Total, English, French, "Non-official"), ~ as.numeric(gsub("[^0-9.]", "", .))))

#need to convert the totals to percentages

lavalhomelang_percent <- lavalhomelang %>%
  group_by(year) %>%
  mutate(across(c(English, French, 'Non-official'), ~./ Total*100))

homelang21percent <- homelang21 |> 
  group_by(region) |> 
  mutate(across(c(English, French, 'Non-official'), ~./ Total*100))

# tidy the dataframes of the percentages by doing pivot_longer 
tidyhomelang <- 
  pivot_longer(lavalhomelang_percent, cols = c(English:'Non-official'),
               names_to = "Language", values_to = "Percentage")

tidyhomelang21 <- 
  pivot_longer(homelang21percent, cols = c(English:'Non-official'),
               names_to = "Language", values_to = "Percentage")

tidyhomelang$Language <- factor(tidyhomelang$Language,
                                levels = c("Non-official", "English", "French"),
                                labels = c("Autres", "Anglais", "Français"))

#trying to add percentages
most_spoken_at_home <- 
  ggplot(tidyhomelang, aes(x=year, y=Percentage, fill= Language)) +
  geom_bar(stat="identity", position = "fill") +
  scale_y_continuous(labels = convert_pct) +
  geom_text(aes(label = convert_pct(Percentage/100)),
            position = position_fill(vjust = 0.5),
            size = 3,
            color = "white") +
  ylab(NULL) +
  xlab(NULL) +
  scale_fill_manual(values = c("Autres" = color_theme("yellowclimate"), 
                               "Anglais" = color_theme("pinkhealth"), 
                               "Français" = color_theme("blueexplorer"))) +
  guides(fill = guide_legend(reverse = TRUE)) +
  gg_cc_theme_no_sf +
  theme(legend.title = element_blank())

# Values for the text
maison_fun <- \(year, lang) {
  z <- tidyhomelang$Percentage[tidyhomelang$year == year & 
                                 tidyhomelang$Language == lang]
  convert_pct(z/100)
}
maison_nonoff_laval <- maison_fun(2021, "Autres")
maison_nonoff_qc <- convert_pct(homelang21_qc$`Non-official` / homelang21_qc$Total)
maison_nonoff_laval_1996 <- maison_fun(1996, "Autres")
maison_en_laval_2001 <- maison_fun(2001, "Anglais")
maison_en_laval <- maison_fun(2021, "Anglais")

# See language most often spoken at home
homelang21vector <- c(
  "Total" = "v_CA21_2203",
  "English" = "v_CA21_2209",
  "French" = "v_CA21_2212",
  "Non-official" = "v_CA21_2215")
homelang21_laval <- get_census(dataset = "CA21",
                               regions = list(CSD = 2465005),
                               level = "DA",
                               vectors = homelang21vector,
                               geo_format = "sf")
homelang21_laval <-
  homelang21_laval |>
  mutate(across(c(English, French, 'Non-official'), ~./ Total))
homelang21_laval$mostspoken <- "French"
homelang21_laval$mostspoken[homelang21_laval$English > homelang21_laval$French] <- "English"
homelang21_laval$mostspoken[homelang21_laval$`Non-official` > homelang21_laval$English &
                              homelang21_laval$`Non-official` > homelang21_laval$French] <- "Non-official"

# Add an alpha depending on HOW MUCH language is spokenn in %
homelang21_laval$tolerance <- sapply(seq_along(homelang21_laval$mostspoken), \(x) {
  homelang21_laval[x, ][[homelang21_laval$mostspoken[x]]]
})
homelang21_laval$mostspoken <- factor(homelang21_laval$mostspoken,
                                      levels = c("Non-official", "English", "French"),
                                      labels = c("Autres", "Anglais", "Français"))


# Define custom colors based on tolerance levels
color_mosthome <- function(language, tolerance) {
  if (is.na(tolerance)) return("#B3B3B3")
  if (language == "Autres") {
    if (tolerance < 0.5) return("Autres < 50 %")  # Light blue
    if (tolerance < 0.75) return("Autres > 50 %")  # Medium blue
    return("Autres > 75 %")  # Dark blue
  }
  if (language == "Anglais") {
    if (tolerance < 0.5) return("Anglais < 50 %")  # Light blue
    if (tolerance < 0.75) return("Anglais > 50 %")  # Medium blue
    return("Anglais > 75 %")  # Dark blue
  }
  if (language == "Français") {
    if (tolerance < 0.5) return("Français < 50 %")  # Light blue
    if (tolerance < 0.75) return("Français > 50 %")  # Medium blue
    return("Français > 75 %")  # Dark blue
  }
}

color_labels <- c(
  "Français < 50 %" = "#c5cde3",
  "Français > 75 %" = "#7084b8",
  "Français > 50 %" = "#a3b0da",

  "Anglais < 50 %" = "#E1AABA",
  "Anglais > 50 %" = "#cd718c",
  "Anglais > 75 %" = "#b33f61",

  "Autres < 50 %" = "#faebbb",
  "Autres > 50 %" = "#f5d574",
  "Autres > 75 %" = "#f0bf2d"
)


# Apply custom colors based on language and tolerance
homelang21_laval$fill_color <- mapply(color_mosthome, homelang21_laval$mostspoken, homelang21_laval$tolerance)

homelang21_laval$fill_color <- factor(homelang21_laval$fill_color,
                                      levels = names(color_labels))

# Ensure there's one variable for all
for (i in names(color_labels)) {
  r <- nrow(homelang21_laval) + 1
  homelang21_laval[r, ]$fill_color <- i
  homelang21_laval[r, ]$mostspoken <- gsub(" .*", "", i)
}

homelang21_laval <- homelang21_laval[!is.na(homelang21_laval$fill_color), ]
homelang21_laval$fill_color <- gsub("Français |Anglais |Autres ", "", homelang21_laval$fill_color)
homelang21_laval <- split(homelang21_laval, homelang21_laval$mostspoken)

names(color_labels) <- gsub("Français |Anglais |Autres ", "", names(color_labels))

homelang21_laval <- 
  lapply(homelang21_laval, \(z) {
    out <- split(z, z$fill_color) |>
      lapply(\(x) {
        out <- tibble::tibble(x$fill_color)
        out$geometry <- sf::st_union(x)
        sf::st_as_sf(out, crs = 4326)[1, ]
      })
    out <- Reduce(rbind, out)
    names(out)[1] <- "fill_color"
    out
  })

most_spoken_at_home_DAs <-
  ggplot(data = homelang21_laval$Français) +
  gg_cc_tiles +
  geom_sf(aes(fill = fill_color), color = "transparent", lwd = 0) +
  scale_fill_manual(values = color_labels[1:3],
                    name = "Français",
                    guide = guide_legend(title.position = "top",
                                         label.position = "bottom", nrow = 1,
                                         order = 1)) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = homelang21_laval$Anglais, aes(fill = fill_color), color = "transparent", lwd = 0) +
  scale_fill_manual(values = color_labels[4:6],
                    name = "Anglais",
                    guide = guide_legend(title.position = "top",
                                         label.position = "bottom", nrow = 1,
                                         order = 2)) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = homelang21_laval$`Autres`, aes(fill = fill_color), color = "transparent", lwd = 0) +
  scale_fill_manual(values = color_labels[7:9],
                    name = "Autres",
                    guide = guide_legend(title.position = "top", 
                                         label.position = "bottom", nrow = 1, 
                                         order = 3)) +
  gg_cc_theme +
  theme(legend.spacing.x = unit(0.5, 'cm'),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.key.width = unit(0.5, 'cm'))

# Get a map of point density for population speaking a language
# See language most often spoken at home
homelang21vector <- c(
  "Total" = "v_CA21_2203",
  "English" = "v_CA21_2209",
  "French" = "v_CA21_2212",
  "Non-official" = "v_CA21_2215")  
homelang21_laval <- get_census(dataset = "CA21",
                               regions = list(CSD = 2465005),
                               level = "DA",
                               vectors = homelang21vector,
                               geo_format = "sf")

hl <- list()
hl$en <- homelang21_laval[c("English")]
hl$fr <- homelang21_laval[c("French")]
hl$no <- homelang21_laval[c("Non-official")]

# Function to generate n random points within a polygon
generate_points <- function(polygon, n) {
  points <- sf::st_sample(polygon, size = n / 10, type = "random")
  polygon$geometry <- sf::st_union(points)
  return(polygon)
}

hl_random_points <- lapply(hl, \(h) {
  for (i in seq_along(h$geometry)) {
    if (is.na(h[i,][[1]])) next
    h[i,] <- generate_points(polygon = h[i, ], n = h[i,][[1]])
  }
  h
})

hh <- lapply(hl_random_points, \(x) {
  x$lang <- names(x)[1]
  x["lang"]
})
hh <- Reduce(rbind, hh)

st_un_multipoint = function(x) {
  g = sf::st_geometry(x)
  i = rep(seq_len(nrow(x)), sapply(g, nrow))
  x = x[i,]
  sf::st_geometry(x) = sf::st_sfc(do.call(c,
                                  lapply(g, function(geom) lapply(1:nrow(geom), function(i) sf::st_point(geom[i,])))))
  x$original_geom_id = i
  x
}
hh <- hh[!sf::st_is_empty(hh), ]
hh <- st_un_multipoint(sf::st_cast(hh, "MULTIPOINT"))
sf::st_crs(hh) <- 4326

hh <- hh[sample(nrow(hh)),]

hh$lang <- gsub("English", "Anglais", hh$lang)
hh$lang <- gsub("French", "Français", hh$lang)
hh$lang <- gsub("Non-official", "Autres", hh$lang)

hh$lang <- factor(hh$lang, levels = c("Français", "Anglais", "Autres"))

most_spoken_at_home_density <- 
  ggplot(data = hh) +
  gg_cc_tiles +
  geom_sf(aes(color = lang), size = 0.05) +
  scale_color_manual(values = c("Français" = color_theme("blueexplorer"),
                                "Anglais" = color_theme("pinkhealth"),
                                "Autres" = color_theme("yellowclimate")),
                     name = "1 point = 10 individus", 
                     guide = guide_legend(title.position = "top", label.position = "bottom", nrow = 1,
                                          override.aes = list(size = 3, stroke = 0.5))) +
  gg_cc_theme +
  theme(legend.spacing.x = unit(0.2, 'cm'),
        legend.spacing.y = unit(0.2, 'cm'),
        legend.key.width = unit(1, 'cm'))

theme(legend.title = element_text(size = 9))

library(patchwork)
most_spoken_at_home_maps <- plot_spacer() + most_spoken_at_home_DAs + plot_spacer() + 
  most_spoken_at_home_density + plot_spacer() + plot_layout(widths = c(0.5, 5, 0.2, 5, 0.5))

ggplot2::ggsave(filename = here::here("output/axe1/langues/most_spoken_at_home_maps.pdf"), 
                plot = most_spoken_at_home_maps, width = 10, height = 5.5)


# Language most spoken at work --------------------------------------------

worklangvector_21 <- c(
  "total" = "v_CA21_6705",
  "official" = "v_CA21_6708",
  "english" = "v_CA21_6711", 
  "french" = "v_CA21_6714",
  "non_off" = "v_CA21_6717")
worklanglav_21 <- 
  get_census(dataset = "CA21",
             regions = list(CSD = 2465005),
             level = "CSD",
             vectors = worklangvector_21)
worklangqc_21 <- 
  get_census(dataset = "CA21",
             regions = list(PR = 24),
             level = "PR",
             vectors = worklangvector_21)

language_work_fr_laval <- convert_pct(worklanglav_21$french / worklanglav_21$total)
language_work_fr_qc <- convert_pct(worklangqc_21$french / worklangqc_21$total)
language_work_en_laval <- convert_pct(worklanglav_21$english / worklanglav_21$total)
language_work_en_qc <- convert_pct(worklangqc_21$english / worklangqc_21$total)


qs::qsavem(knowledge_official, bilingual_1996, bilingual_2021,
           no_official_1996, no_official_2021, know_official_laval_qc_diff,
           know_fr_qc, know_fr_laval, know_bilingual_laval, know_bilingual_qc,
           know_only_fr_laval, know_only_fr_qc, know_only_en_laval,
           most_spoken_at_home, maison_nonoff_laval, maison_nonoff_qc,
           maison_nonoff_laval_1996, maison_en_laval_2001, maison_en_laval,
           most_spoken_at_home_maps, language_work_fr_laval, language_work_fr_qc,
           language_work_en_laval, language_work_en_qc,
           file = "data/axe1/language.qsm")
