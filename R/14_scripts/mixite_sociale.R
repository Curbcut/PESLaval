##### MIXITÉ SOCIALE ###########################################################

source("R/01_startup.R")

# Load census variables ---------------------------------------------------

# Socioprofessionnelles
noc_total <- c(#"total" = "v_CA21_6561", "na" = "v_CA21_6564", 
  "all" = "v_CA21_6567",
  "0" = "v_CA21_6570", "1" = "v_CA21_6573", "2" = "v_CA21_6576",
  "3" = "v_CA21_6579", "4" = "v_CA21_6582", "5" = "v_CA21_6585",
  "6" = "v_CA21_6588", "7" = "v_CA21_6591", "8" = "v_CA21_6594",
  "9" = "v_CA21_6597")
noc_total <- unname(noc_total)
sociopro <- get_census(dataset = "CA21", 
                       regions = list(CSD = 2465005), 
                       level = "CT",
                       vectors = noc_total,
                       geo_format = "sf")
sociopro <- 
  sociopro |> 
  mutate(across(`v_CA21_6570: 0 Legislative and senior management occupations`:`v_CA21_6597: 9 Occupations in manufacturing and utilities`, ~ ./ `v_CA21_6567: All occupations`))

# Cultures
c21 <- cancensus::list_census_vectors("CA21")
ethn_cult <- c21$vector[c21$parent_vector == "v_CA21_4917"]
ethn_cult <- ethn_cult[!is.na(ethn_cult)]
ethn_cult <- get_census(dataset = "CA21", 
                        regions = list(CSD = 2465005), 
                        level = "CT",
                        vectors = c("v_CA21_4917", ethn_cult),
                        geo_format = "sf")
ethn_cult <- 
  ethn_cult |> 
  mutate(across(`v_CA21_4920: Canadian`:`v_CA21_5667: Paraguayan`, ~ ./ `v_CA21_4917: Total - Ethnic or cultural origin for the population in private households`))

# Tranches d'âge
age <- c21$vector[c21$parent_vector %in% c("v_CA21_11", "v_CA21_68", "v_CA21_251")]
age <- age[!is.na(age)]

age <- get_census(dataset = "CA21", 
                  regions = list(CSD = 2465005), 
                  level = "CT",
                  vectors = c("v_CA21_8", age),
                  geo_format = "sf")
age <- 
  age |> 
  mutate(across(`v_CA21_14: 0 to 4 years`:`v_CA21_326: 85 years and over`, ~ ./ `v_CA21_8: Total - Age`))

# Income
income <- c21$vector[c21$parent_vector == "v_CA21_671"]
income <- income[!is.na(income)]
income <- get_census(dataset = "CA21", 
                     regions = list(CSD = 2465005), 
                     level = "CT",
                     vectors = c("v_CA21_671", income),
                     geo_format = "sf")
income <- 
  income |> 
  mutate(across(`v_CA21_674: Under $10,000 (including loss)`:`v_CA21_704: $100,000 and over`, ~ ./ `v_CA21_671: With total income`))

# Education
education <- c21$vector[c21$parent_vector == "v_CA21_5817"]
education <- education[!is.na(education)]
education <- get_census(dataset = "CA21", 
                        regions = list(CSD = 2465005), 
                        level = "CT",
                        vectors = c("v_CA21_5817", education),
                        geo_format = "sf")
education <- 
  education |> 
  mutate(across(`v_CA21_5820: No certificate, diploma or degree`:`v_CA21_5826: Postsecondary certificate, diploma or degree`, ~ ./ `v_CA21_5817: Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households`))


# Calculate HHI for every category ----------------------------------------

calculate_hhi <- function(percentages) {
  # Calculate HHI
  hhi <- sum(percentages^2, na.rm = TRUE)
  
  return(hhi)
}
sociopro$hhi <- apply(sf::st_drop_geometry(sociopro)[18:27], MARGIN = 1, 
                      FUN = calculate_hhi, simplify = TRUE)
ethn_cult$hhi <- apply(sf::st_drop_geometry(ethn_cult)[18:264], MARGIN = 1, 
                       FUN = calculate_hhi, simplify = TRUE)
age$hhi <- apply(sf::st_drop_geometry(age)[18:35], MARGIN = 1, 
                 FUN = calculate_hhi, simplify = TRUE)
income$hhi <- apply(sf::st_drop_geometry(income)[18:28], MARGIN = 1, 
                    FUN = calculate_hhi, simplify = TRUE)
education$hhi <- apply(sf::st_drop_geometry(education)[18:20], MARGIN = 1, 
                       FUN = calculate_hhi, simplify = TRUE)


# Sum every HHI per zone --------------------------------------------------

CTs <- get_census(dataset = "CA21", 
                  regions = list(CSD = 2465005), 
                  level = "CT",
                  geo_format = "sf")

CTs$hhi <- sociopro$hhi + ethn_cult$hhi + age$hhi + income$hhi + education$hhi

t <- CTs
t <- add_bins(df = t,
              variable = "hhi",
              breaks = c(-Inf, quantile(t$hhi, probs = c(.20, .40, .60, .80)), Inf),
              labels = c("Mixité haute", "a", "b", "c", "Mixité basse")
)
labels <- c("Mixité haute", "", "", "", "Mixité basse")

# Union the features so the polygons don't show their borders. Might revisit
# with the addition of streets!
t <- Reduce(rbind,
            split(t, t$binned_variable) |>
              lapply(\(x) {
                out <- tibble::tibble(x$binned_variable)
                out$geometry <- sf::st_union(x)
                sf::st_as_sf(out, crs = 4326)[1, ]
              })
) |> sf::st_as_sf()
names(t)[1] <- "binned_variable"

# Plotting the sf map with custom bins
ggplot(t) +
  # gg_cc_tiles +
  geom_sf(aes(fill = binned_variable), color = "transparent", lwd = 0) +
  scale_fill_manual(values = curbcut_colors$left_5$fill[6:2],
                    name = "Indice de mixité",
                    labels = labels,
                    guide = guide_legend(title.position = "top", label.position = "bottom", nrow = 1)) +
  gg_cc_theme +
  theme(legend.spacing.x = unit(2, 'cm'),
        legend.spacing.y = unit(1, 'cm'))



# Do entropy instead ------------------------------------------------------

calculate_entropy <- function(percentages) {
  # Remplacer les zéros par des valeurs très petites pour éviter les problèmes de log(0)
  percentages[percentages == 0] <- .Machine$double.eps
  # Calculer l'entropie de Shannon
  entropy <- -sum(percentages * log(percentages), na.rm = TRUE)
  return(entropy)
}

# Calcul de l'entropie pour chaque catégorie
sociopro$entropy <- apply(sf::st_drop_geometry(sociopro)[18:27], MARGIN = 1, 
                          FUN = calculate_entropy, simplify = TRUE)
ethn_cult$entropy <- apply(sf::st_drop_geometry(ethn_cult)[18:264], MARGIN = 1, 
                           FUN = calculate_entropy, simplify = TRUE)
age$entropy <- apply(sf::st_drop_geometry(age)[18:35], MARGIN = 1, 
                     FUN = calculate_entropy, simplify = TRUE)
income$entropy <- apply(sf::st_drop_geometry(income)[18:28], MARGIN = 1, 
                        FUN = calculate_entropy, simplify = TRUE)
education$entropy <- apply(sf::st_drop_geometry(education)[18:20], MARGIN = 1, 
                           FUN = calculate_entropy, simplify = TRUE)

# Normalisation des entropies
normalize_entropy <- function(entropy) {
  (entropy - min(entropy, na.rm = TRUE)) / (max(entropy, na.rm = TRUE) - min(entropy, na.rm = TRUE))
}

sociopro$entropy_norm <- normalize_entropy(sociopro$entropy)
ethn_cult$entropy_norm <- normalize_entropy(ethn_cult$entropy)
age$entropy_norm <- normalize_entropy(age$entropy)
income$entropy_norm <- normalize_entropy(income$entropy)
education$entropy_norm <- normalize_entropy(education$entropy)

# Somme de l'entropie par zone
CTs <- get_census(dataset = "CA21", 
                  regions = list(CSD = 2465005), 
                  level = "CT",
                  geo_format = "sf")

CTs$entropy <- sociopro$entropy_norm + ethn_cult$entropy_norm + age$entropy_norm + income$entropy_norm + education$entropy_norm

t <- CTs
t <- add_bins(df = t,
              variable = "entropy",
              breaks = c(-Inf, quantile(t$entropy, probs = c(.25, .50, .75, .95)), Inf),
              labels = c("Mixité basse", "a", "b", "c", "Mixité haute")
)
labels <- c("Mixité basse", "", "", "", "Mixité haute")

# Union the features so the polygons don't show their borders. Might revisit
# with the addition of streets!
t <- Reduce(rbind,
            split(t, t$binned_variable) |>
              lapply(\(x) {
                out <- tibble::tibble(x$binned_variable)
                out$geometry <- sf::st_union(x)
                sf::st_as_sf(out, crs = 4326)[1, ]
              })
) |> sf::st_as_sf()
names(t)[1] <- "binned_variable"

# Plotting the sf map with custom bins
mixite_sociale <- 
  ggplot(t) +
  # gg_cc_tiles +
  geom_sf(aes(fill = binned_variable), color = "transparent", lwd = 0) +
  scale_fill_manual(values = curbcut_colors$left_5$fill[2:6],
                    name = "Indice de mixité",
                    labels = labels,
                    guide = guide_legend(title.position = "top", label.position = "bottom", nrow = 1)) +
  gg_cc_theme +
  theme(legend.spacing.x = unit(2, 'cm'),
        legend.spacing.y = unit(1, 'cm'))

ggplot2::ggsave(filename = here::here("output/axe2/mixitecomposite.pdf"), 
                plot = mixite_sociale, width = 6.5, height = 6)


mixite_sociale_infographics <- 
  ggplot(t) +
  # gg_cc_tiles +
  geom_sf(aes(fill = binned_variable), color = "transparent", lwd = 0) +
  scale_fill_manual(values = curbcut_colors$left_5$fill[2:6],
                    name = "Indice de mixité",
                    labels = labels,
                    guide = guide_legend(title.position = "top", label.position = "bottom", nrow = 1)) +
  gg_cc_theme +
  theme(legend.spacing.x = unit(2, 'cm'),
        legend.spacing.y = unit(1, 'cm'))

ggplot2::ggsave(filename = here::here("output/infographic/mixitecomposite.pdf"), 
                plot = mixite_sociale_infographics, width = 4.5, height = 4)


# Facet wrap entropy ------------------------------------------------------

# Normalisation des entropies pour chaque catégorie
sociopro$dimension <- "Profession"
ethn_cult$dimension <- "Origine ethnique ou culturelle"
age$dimension <- "Âge"
income$dimension <- "Revenu"
education$dimension <- "Éducation"

# Rassembler toutes les données dans un seul dataframe
all_data <- bind_rows(
  select(sociopro, GeoUID, entropy_norm, dimension),
  select(ethn_cult, GeoUID, entropy_norm, dimension),
  select(age, GeoUID, entropy_norm, dimension),
  select(income, GeoUID, entropy_norm, dimension),
  select(education, GeoUID, entropy_norm, dimension)
)

# Renommer la colonne d'entropie normalisée pour une utilisation cohérente
names(all_data)[2] <- "entropy"


all_data <- add_bins(df = all_data,
                     variable = "entropy",
                     breaks = c(-Inf, quantile(all_data$entropy, probs = c(.25, .50, .75, .95)), Inf),
                     labels = c("Mixité basse", "a", "b", "c", "Mixité haute")
)
labels <- c("Mixité basse", "", "", "", "Mixité haute")

all_data <- Reduce(rbind,
                   split(all_data, list(all_data$dimension, all_data$binned_variable)) |>
                     lapply(\(x) {
                       out <- tibble::tibble(x$dimension, x$binned_variable)
                       out$geometry <- sf::st_union(x)
                       sf::st_as_sf(out, crs = 4326)[1, ]
                     })
) |> sf::st_as_sf()

names(all_data)[1:2] <- c("dimension", "binned_variable")

mixite_sociale_facet <- 
  ggplot(all_data) +
  gg_cc_tiles +
  geom_sf(aes(fill = binned_variable), color = "transparent", lwd = 0) +
  scale_fill_manual(values = curbcut_colors$left_5$fill[2:6],
                    name = "Indice de mixité",
                    labels = labels,
                    guide = guide_legend(title.position = "top", label.position = "bottom", nrow = 1)) +
  facet_wrap(~ dimension, nrow = 2) +
  gg_cc_theme +
  theme(legend.spacing.x = unit(2, 'cm'),
        legend.spacing.y = unit(1, 'cm'))

ggplot2::ggsave(filename = here::here("output/axe2/mixitefacet.pdf"), 
                plot = mixite_sociale_facet, width = 9, height = 7)


# Save --------------------------------------------------------------------

qs::qsavem(mixite_sociale, mixite_sociale_facet, file = "data/axe2/mixite.qsm")

