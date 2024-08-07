### FAMILLES ###################################################################
source("R/01_startup.R")

CT <- get_census(dataset = "CA21", 
                 regions = list(CSD = 2465005), 
                 level = "CT", 
                 geo_format = "sf")


laval_census <- cancensus::get_census(dataset = "CA21", 
                                      regions = list(CSD = 2465005), 
                                      level = "CSD")


# Caractéristiques des familles -------------------------------------------

# For Laval (CT)
family_size <-  get_census(
  dataset = "CA21",
  regions = list(CSD = 2465005),
  level = "CT",
  geo_format = "sf",
  vectors = c(
    family_size = "v_CA21_492",
    family_2 = "v_CA21_493",
    family_3 = "v_CA21_494",
    family_4 = "v_CA21_495",
    family_5 = "v_CA21_496"))
    
family_size <- 
  family_size |> 
  mutate(family_2_pct = family_2/family_size,
         family_3_pct = family_3/family_size,
         family_4_pct = family_4/family_size,
         family_5_pct = family_5/family_size) 

## For Laval (CSD)
laval_family_size21 <-  get_census(
  dataset = "CA21",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(
    family_size = "v_CA21_492",
    family_2 = "v_CA21_493",
    family_3 = "v_CA21_494",
    family_4 = "v_CA21_495",
    family_5 = "v_CA21_496"))

laval_family_size21 <- 
  laval_family_size21 |> 
  mutate(family_2_pct = family_2/family_size,
         family_3_pct = family_3/family_size,
         family_4_pct = family_4/family_size,
         family_5_pct = family_5/family_size)

familles_nombreuse_pct <- 
  laval_family_size21$family_4_pct + laval_family_size21$family_5_pct
familles_nombreuse_pct <- convert_pct(familles_nombreuse_pct)

familles_taillemoy <- get_census(
  dataset = "CA21",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = "v_CA21_497")
familles_taillemoy <- familles_taillemoy[[ncol(familles_taillemoy)]]

# Québec
qc_family_size <-  get_census(
  dataset = "CA21",
  regions = list(PR = 24),
  level = "PR",
  geo_format = "sf",
  vectors = c(
    family_size = "v_CA21_492",
    family_2 = "v_CA21_493",
    family_3 = "v_CA21_494",
    family_4 = "v_CA21_495",
    family_5 = "v_CA21_496"))

qc_family_size <- 
  qc_family_size |> 
  mutate(family_2_pct = family_2/family_size,
         family_3_pct = family_3/family_size,
         family_4_pct = family_4/family_size,
         family_5_pct = family_5/family_size)

familles_nombreuse_qc_pct <- 
  qc_family_size$family_4_pct + qc_family_size$family_5_pct
familles_nombreuse_qc_pct <- convert_pct(familles_nombreuse_qc_pct)

familles_taillemoy_qc <- get_census(
  dataset = "CA21",
  regions = list(PR = 24),
  level = "PR",
  vectors = "v_CA21_497")
familles_taillemoy_qc <- familles_taillemoy_qc[[ncol(familles_taillemoy_qc)]]


# Family size in laval in 2016
laval_family_size16 <-  get_census(
  dataset = "CA16",
  regions = list(CSD = 2465005),
  level = "CSD",
  geo_format = "sf",
  vectors = c(
    family_size = "v_CA16_478",
    family_2 = "v_CA16_479",
    family_3 = "v_CA16_480",
    family_4 = "v_CA16_481",
    family_5 = "v_CA16_482"))

laval_family_size16 <- 
  laval_family_size16 |> 
  mutate(family_2_pct = family_2/family_size,
         family_3_pct = family_3/family_size,
         family_4_pct = family_4/family_size,
         family_5_pct = family_5/family_size)

# Family size in laval in 2011
laval_family_size11 <-  get_census(
  dataset = "CA11",
  regions = list(CSD = 2465005),
  level = "CSD",
  geo_format = "sf",
  vectors = c(
    family_size = "v_CA11F_110",
    family_2 = "v_CA11F_111",
    family_3 = "v_CA11F_112",
    family_4 = "v_CA11F_113",
    family_5 = "v_CA11F_114"))

laval_family_size11 <- 
  laval_family_size11 |> 
  mutate(family_2_pct = family_2/family_size,
         family_3_pct = family_3/family_size,
         family_4_pct = family_4/family_size,
         family_5_pct = family_5/family_size)

#  Family size in Laval in 2006
laval_family_size06 <-  get_census(
  dataset = "CA06",
  regions = list(CSD = 2465005),
  level = "CSD",
  geo_format = "sf",
  vectors = c(
    family_size = "v_CA06_50",
    family_2 = "v_CA06_51",
    family_3 = "v_CA06_52",
    family_4 = "v_CA06_53",
    family_5 = "v_CA06_54"))

laval_family_size06 <- 
  laval_family_size06 |> 
  mutate(family_2_pct = family_2/family_size,
         family_3_pct = family_3/family_size,
         family_4_pct = family_4/family_size,
         family_5_pct = family_5/family_size)

# family size in laval in 1996
laval_family_size96 <-  get_census(
  dataset = "CA1996",
  regions = list(CSD = 2465005),
  level = "CSD",
  geo_format = "sf",
  vectors = c(
    family_size = "v_CA1996_60",
    family_2 = "v_CA1996_61",
    family_3 = "v_CA1996_62",
    family_4 = "v_CA1996_63",
    family_5 = "v_CA1996_64"))

laval_family_size96 <- 
  laval_family_size96 |> 
  mutate(family_2_pct = family_2/family_size,
         family_3_pct = family_3/family_size,
         family_4_pct = family_4/family_size,
         family_5_pct = family_5/family_size)


#create a graph to visualise the evolution over time
# Combine datasets
laval_family_size <- bind_rows(
  laval_family_size21 %>% mutate(year = 2021),
  laval_family_size16 %>% mutate(year = 2016),
  laval_family_size11 %>% mutate(year = 2011),
  laval_family_size06 %>% mutate(year = 2006),
  laval_family_size96 %>% mutate(year = 1996)
)

# Select and rename columns for clarity
laval_family_size <- laval_family_size %>%
  select(year, family_2_pct, family_3_pct, family_4_pct, family_5_pct) %>%
  rename(`2` = family_2_pct,
         `3` = family_3_pct,
         `4` = family_4_pct,
         `5+` = family_5_pct)

# Convert to long format for plotting
laval_family_size_long <- laval_family_size %>%
  sf::st_drop_geometry() |> 
  pivot_longer(cols = -year, names_to = "family_size", values_to = "percentage")

# Plot the data
family_size_graph <-
  ggplot(laval_family_size_long, aes(x = year, y = percentage, color = family_size)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(y = "Proportion de familles",) +
  scale_color_manual(values = c("2" = "#A3B0D1", 
                                "3" = "#73AD80", 
                                "4" = "#E08565",
                                "5+" = "#CD718C"),
                     name = "Membres") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(2001, unique(laval_family_size_long$year))) +
  gg_cc_theme_no_sf +
  theme(axis.title.x = element_blank())

ggplot2::ggsave(filename = here::here("output/axe1/family/family_size_graph.png"), 
                plot = family_size_graph, width = 4, height = 3, bg = "transparent")

# Couples avec enfants
famille_couple <- get_census(
  dataset = "CA21",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(couple = "v_CA21_545", with_children = "v_CA21_546"))
famille_couple_enfants <- famille_couple[[ncol(famille_couple)]]
famille_couple_enfants <- convert_number(famille_couple_enfants)

famille_couple_aumoins1 <- famille_couple$with_children / famille_couple$couple

famille_couple_2016 <- get_census(
  dataset = "CA16",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(couple = "v_CA16_491", with_children = "v_CA16_493"))

famille_couple_aumoins1_2016 <- famille_couple_2016$with_children / famille_couple_2016$couple


# Couples avec enfants (Québec)
famille_couple_qc <- get_census(
  dataset = "CA21",
  regions = list(PR = 24),
  level = "PR",
  vectors = c(couple = "v_CA21_545", with_children = "v_CA21_546"))
famille_couple_aumoins1_qc <- famille_couple_qc$with_children / famille_couple_qc$couple

famille_couple_qc_2016 <- get_census(
  dataset = "CA16",
  regions = list(PR = 24),
  level = "PR",
  vectors = c(couple = "v_CA16_491", with_children = "v_CA16_493"))

famille_couple_aumoins1_qc_2016 <- famille_couple_qc_2016$with_children / famille_couple_qc_2016$couple

# Nombre moyen d'enfants
famille_nb_enf_moyen <- get_census(
  dataset = "CA21",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(avg_child = "v_CA21_498"))
# get_census(
#   dataset = "CA21",
#   regions = list(PR = 24),
#   level = "PR",
#   vectors = c(avg_child = "v_CA21_498"))
famille_nb_enf_moyen <- convert_number(famille_nb_enf_moyen[[ncol(famille_nb_enf_moyen)]])



# Structure de la famille -------------------------------------------------

# Structure de famille à Laval
household_family_21 <-  get_census(
  dataset = "CA21",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(
    household_total = "v_CA21_499",
    couple_fam = "v_CA21_500",
    couple_fam_married = "v_CA21_501",
    couple_fam_married_child = "v_CA21_502",
    couple_fam_married_nochild = "v_CA21_503",
    couple_fam_cl = "v_CA21_504",
    couple_fam_cl_child = "v_CA21_505",
    couple_fam_cl_nochild = "v_CA21_506",
    one_parent = "v_CA21_507",
    one_parent_woman = "v_CA21_508",
    one_parent_man = "v_CA21_509"
  ))

# Percentage by parent variables
laval_household_family_relative <- 
  household_family_21 |> 
  mutate(couple_fam_pct = couple_fam/household_total,
         couple_fam_married_pct = couple_fam_married/couple_fam,
         couple_fam_married_child_pct = couple_fam_married_child/couple_fam_married,
         couple_fam_married_nochild_pct = couple_fam_married_nochild/couple_fam_married,
         couple_fam_cl_pct = couple_fam_cl/couple_fam,
         couple_fam_cl_child_pct = couple_fam_cl_child/couple_fam_cl,
         couple_fam_cl_nochild_pct = couple_fam_cl_nochild/couple_fam_cl,
         one_parent_pct = one_parent/household_total,
         one_parent_woman_pct = one_parent_woman / one_parent,
         one_parent_man_pct = one_parent_man / one_parent
  ) 

# Percentage by all households
household_family_21 <- 
  household_family_21 |> 
  mutate(couple_fam_pct = couple_fam/household_total,
         couple_fam_married_pct = couple_fam_married/household_total,
         couple_fam_married_child_pct = couple_fam_married_child/household_total,
         couple_fam_married_nochild_pct = couple_fam_married_nochild/household_total,
         couple_fam_cl_pct = couple_fam_cl/household_total,
         couple_fam_cl_child_pct = couple_fam_cl_child/household_total,
         couple_fam_cl_nochild_pct = couple_fam_cl_nochild/household_total,
         one_parent_pct = one_parent/household_total,
         one_parent_woman_pct = one_parent_woman / household_total,
         one_parent_man_pct = one_parent_man / household_total
  ) 

famille_marie <- convert_pct(household_family_21$couple_fam_married_pct)
famille_marie_avecenf <- convert_pct(laval_household_family_relative$couple_fam_married_child_pct)

# Structure de famille à Laval
household_family_21_qc <-  get_census(
  dataset = "CA21",
  regions = list(PR = 24),
  level = "PR",
  vectors = c(
    household_total = "v_CA21_499",
    couple_fam = "v_CA21_500",
    couple_fam_married = "v_CA21_501",
    couple_fam_married_child = "v_CA21_502",
    couple_fam_married_nochild = "v_CA21_503",
    couple_fam_cl = "v_CA21_504",
    couple_fam_cl_child = "v_CA21_505",
    couple_fam_cl_nochild = "v_CA21_506",
    one_parent = "v_CA21_507",
    one_parent_woman = "v_CA21_508",
    one_parent_man = "v_CA21_509"
  ))

# Percentage by parent variables
household_family_relative <- 
  household_family_21_qc |> 
  mutate(couple_fam_pct = couple_fam/household_total,
         couple_fam_married_pct = couple_fam_married/couple_fam,
         couple_fam_married_child_pct = couple_fam_married_child/couple_fam_married,
         couple_fam_married_nochild_pct = couple_fam_married_nochild/couple_fam_married,
         couple_fam_cl_pct = couple_fam_cl/couple_fam,
         couple_fam_cl_child_pct = couple_fam_cl_child/couple_fam_cl,
         couple_fam_cl_nochild_pct = couple_fam_cl_nochild/couple_fam_cl,
         one_parent_pct = one_parent/household_total,
         one_parent_woman_pct = one_parent_woman / one_parent,
         one_parent_man_pct = one_parent_man / one_parent
  ) 

# Percentage by all households
household_family_21_qc <- 
  household_family_21_qc |> 
  mutate(couple_fam_pct = couple_fam/household_total,
         couple_fam_married_pct = couple_fam_married/household_total,
         couple_fam_married_child_pct = couple_fam_married_child/household_total,
         couple_fam_married_nochild_pct = couple_fam_married_nochild/household_total,
         couple_fam_cl_pct = couple_fam_cl/household_total,
         couple_fam_cl_child_pct = couple_fam_cl_child/household_total,
         couple_fam_cl_nochild_pct = couple_fam_cl_nochild/household_total,
         one_parent_pct = one_parent/household_total,
         one_parent_woman_pct = one_parent_woman / household_total,
         one_parent_man_pct = one_parent_man / household_total
  ) 

famille_marie_qc <- convert_pct(household_family_21_qc$couple_fam_married_pct)


# i first set the categories I will be using
household_family_plot <- data.frame(
  category = c("Couple Family", "Married Couple with Children", "Married Couple without Children", 
               "Common-law Couple", "Common-law Couple with Children", "Common-law Couple without Children", 
               "One Parent", "One Parent (Woman)", "One Parent (Man)"),
  percentage = c(
    household_family_21$couple_fam_pct,
    household_family_21$couple_fam_married_child_pct,
    household_family_21$couple_fam_married_nochild_pct,
    household_family_21$couple_fam_cl_pct,
    household_family_21$couple_fam_cl_child_pct,
    household_family_21$couple_fam_cl_nochild_pct,
    household_family_21$one_parent_pct,
    household_family_21$one_parent_woman_pct,
    household_family_21$one_parent_man_pct
  )
)

#i get the totals of married couples by adding up with and without children
married <- household_family_21 %>%
  summarize(
    married_with_children = mean(couple_fam_married_child_pct),
    married_without_children = mean(couple_fam_married_nochild_pct)
  )

common_law <- household_family_21 %>%
  summarize(
    common_law_with_children = mean(couple_fam_cl_child_pct),
    common_law_without_children = mean(couple_fam_cl_nochild_pct)
  )

single_parent <- household_family_21 %>%
  summarize(
    single_parent_woman = mean(one_parent_woman_pct),
    single_parent_man = mean(one_parent_man_pct))

# Create a data frame for plotting
#this df creates the categories for the x-axis and groups for children status
plot_data <- data.frame(
  category = c("Couples mariés", "Union libre", "Monopar. dirigée par femme", "Monopar. dirigée par homme"),
  avec_enfants = c(married$married_with_children, 
                    common_law$common_law_with_children, 
                    single_parent$single_parent_woman, 
                    single_parent$single_parent_man),
  sans_enfants = c(married$married_without_children, 
                       common_law$common_law_without_children, 0, 0)) |> 
  mutate(category = factor(category, levels = c("Couples mariés", "Union libre", "Monopar. dirigée par femme", "Monopar. dirigée par homme")))

# Reshape the data for stacked bar plot
plot_data <- plot_data %>%
  pivot_longer(cols = c(avec_enfants, sans_enfants), 
               names_to = "child_status", 
               values_to = "percentage") |> 
  mutate(perc = convert_pct(percentage),
         perc = ifelse(percentage == 0, NA, perc))

# Prepare plot for Quebec
married_qc <- household_family_21_qc %>%
  summarize(
    married_with_children = mean(couple_fam_married_child_pct),
    married_without_children = mean(couple_fam_married_nochild_pct)
  )
common_law_qc <- household_family_21_qc %>%
  summarize(
    common_law_with_children = mean(couple_fam_cl_child_pct),
    common_law_without_children = mean(couple_fam_cl_nochild_pct)
  )
single_parent_qc <- household_family_21_qc %>%
  summarize(
    single_parent_woman = mean(one_parent_woman_pct),
    single_parent_man = mean(one_parent_man_pct))

# Create a data frame for plotting
plot_data_qc <- data.frame(
  category = c("Couples mariés", "Union libre", "Monopar. dirigée par femme", "Monopar. dirigée par homme"),
  avec_enfants = c(married_qc$married_with_children, 
                    common_law_qc$common_law_with_children, 
                    single_parent_qc$single_parent_woman, 
                    single_parent_qc$single_parent_man),
  sans_enfants = c(married_qc$married_without_children, 
                       common_law_qc$common_law_without_children, 0, 0)) |> 
  mutate(category = factor(category, levels = c("Couples mariés", "Union libre", "Monopar. dirigée par femme", "Monopar. dirigée par homme")))

# Reshape the data for stacked bar plot
plot_data_qc <- plot_data_qc %>%
  pivot_longer(cols = c(avec_enfants, sans_enfants), 
               names_to = "child_status", 
               values_to = "percentage") |> 
  mutate(perc = convert_pct(percentage),
         perc = ifelse(percentage == 0, NA, perc))

# Combine data for both Laval and Quebec
combined_data <- rbind(
  transform(plot_data, region = "Laval"),
  transform(plot_data_qc, region = "Québec")
)

# Create the combined plot with facet_wrap
famille_structure_plot <-
  ggplot(combined_data, aes(x = category, y = percentage, fill = child_status)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = perc), 
            position = position_stack(vjust = 0.5), 
            size = 3, 
            color = "white") +
  labs(y = "Proportion de familles",
       x = element_blank(),
       fill = element_blank()) +
  scale_fill_manual(labels = c("avec_enfants" = "Avec enfants", 
                               "sans_enfants" = "Sans enfants"),
                    values = c("avec_enfants" = "#E08565", 
                               "sans_enfants" = "#A3B0D1")) + # Adjust colors
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # Format y-axis labels as percentages
  gg_cc_theme_no_sf +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1),
  ) +
  facet_wrap(~ region, scales = "fixed")

ggplot2::ggsave(filename = here::here("output/axe1/family/famille_structure_plot.png"), 
                plot = famille_structure_plot, width = 6, height = 4, bg = "transparent")



# Familles monoparentales -------------------------------------------------

famille_monop_census <-  get_census(
  dataset = "CA21",
  regions = list(CSD = 2465005),
  level = "CSD",
  geo_format = "sf",
  vectors = c(
    total = "v_CA21_519",
    men = "v_CA21_520",
    women = "v_CA21_521",
    one_child = "v_CA21_546"))

famille_monop_census <- 
  famille_monop_census |> 
  mutate(women_pct = women/total,
         men_pct = men/total,
         monop_pct = total /one_child)

famille_monop <- convert_number(famille_monop_census$total)
famille_monop_f <- convert_pct(famille_monop_census$women_pct)
famille_monop_m <- convert_pct(famille_monop_census$men_pct)

# Tailles des ménages -----------------------------------------------------

# Laval
laval_household_size21_raw <-  get_census(
  dataset = "CA21",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(
    avg_size = "v_CA21_452",
    total = "v_CA21_443",
    one = "v_CA21_444",
    two = "v_CA21_445",
    three = "v_CA21_446",
    four = "v_CA21_447",
    five_more = "v_CA21_448"
  ))
laval_household_size21 <- 
  laval_household_size21_raw |> 
  mutate(une_personne = one/total,
         deux_personnes = two/total,
         trois_personnes = three/total,
         quatre_personnes_plus = (four + five_more)/total)

# Québec 2021
qc_household_size <-  get_census(
  dataset = "CA21",
  regions = list(PR = 24),
  level = "PR",
  vectors = c(
    avg_size = "v_CA21_452",
    total = "v_CA21_443",
    one = "v_CA21_444",
    two = "v_CA21_445",
    three = "v_CA21_446",
    four = "v_CA21_447",
    five_more = "v_CA21_448"
  ))
qc_household_size <- 
  qc_household_size |> 
  mutate(une_personne = one/total,
         deux_personnes = two/total,
         trois_personnes = three/total,
         quatre_personnes_plus = (four + five_more)/total)

# Select only numeric columns for pivoting
laval_household_size_numeric <- laval_household_size21 %>%
  select(GeoUID, une_personne, deux_personnes, trois_personnes, quatre_personnes_plus) |> 
  sf::st_drop_geometry()

qc_household_size_numeric <- qc_household_size %>%
  select(GeoUID, une_personne, deux_personnes, trois_personnes, quatre_personnes_plus) |> 
  sf::st_drop_geometry()


#pivot tables
laval_household_size_pivot <- 
  laval_household_size_numeric %>%
  pivot_longer(cols = -GeoUID, names_to = "household_size", values_to = "percentage") %>%
  mutate(household_size = factor(household_size, levels = c("une_personne", "deux_personnes", "trois_personnes", "quatre_personnes_plus"))) |> 
  mutate(perc = convert_pct(x = percentage),
         region = "Laval")

qc_household_size_pivot <- 
  qc_household_size_numeric %>%
  pivot_longer(cols = -GeoUID, names_to = "household_size", values_to = "percentage") %>%
  mutate(household_size = factor(household_size, levels = c("une_personne", "deux_personnes", "trois_personnes", "quatre_personnes_plus"))) |> 
  mutate(perc = convert_pct(x = percentage),
         region = "Québec")

household_size_combined <- 
  bind_rows(laval_household_size_pivot, qc_household_size_pivot) |> 
  select(-GeoUID)

# Combined plot
household_comp_graph <- 
  ggplot(data = household_size_combined, aes(x = household_size, y = percentage, fill = region)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = "Proportion de ménages") +
  geom_text(aes(label = perc), 
            position = position_dodge(width = 0.9),
            vjust = 2.5,
            size = 4, 
            color = "white") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c("une_personne" = "1 personne", "deux_personnes" = "2 personnes",
                              "trois_personnes" = "3 personnes", "quatre_personnes_plus" = "4+ personnes")) +
  scale_fill_manual(values = c("Laval" = "#A3B0D1", "Québec" = "#73AD80")) +
  gg_cc_theme_no_sf +
  theme(plot.title = element_blank(), 
        axis.title.x = element_blank(),
        legend.title = element_blank())

ggplot2::ggsave(filename = here::here("output/axe1/family/household_comp_graph.png"), 
                plot = household_comp_graph, width = 6, height = 4, bg = "transparent")

# Comparison over time
laval_household_size16_raw <-  get_census(
  dataset = "CA16",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(
    avg_size = "v_CA16_425",
    total = "v_CA16_418",
    one = "v_CA16_419",
    two = "v_CA16_420",
    three = "v_CA16_421",
    four = "v_CA16_422",
    five_more = "v_CA16_423"
  ))

laval_household_size16 <- 
  laval_household_size16_raw |> 
  mutate(`1` = one/total,
         `2` = two/total,
         `3` = three/total,
         `4+` = (four + five_more)/total)

laval_household_size11 <- get_census(
  dataset = "CA11",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(
    avg_size = "v_CA16_425",
    total = "v_CA11F_209",
    one = "v_CA11F_210",
    two = "v_CA11F_211",
    three = "v_CA11F_212",
    four = "v_CA11F_213",
    five = "v_CA11F_214",
    six_more = "v_CA11F_215"
  ))

laval_household_size11 <- 
  laval_household_size11 |> 
  mutate(`1` = one/total,
         `2` = two/total,
         `3` = three/total,
         `4+` = (four + five + six_more)/total)

laval_household_size06 <-  get_census(
  dataset = "CA06",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(
    avg_size = "v_CA06_135",
    total = "v_CA06_128",
    one = "v_CA06_129",
    two = "v_CA06_130",
    three = "v_CA06_131",
    four_five = "v_CA06_132",
    six_more = "v_CA06_133"
  ))

laval_household_size06 <- 
  laval_household_size06 |> 
  mutate(`1` = one/total,
         `2` = two/total,
         `3` = three/total,
         `4+` = (four_five + six_more) /total)


laval_household_size01 <-  get_census(
  dataset = "CA01",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(
    avg_size = "v_CA01_128",
    total = "v_CA01_121",
    one = "v_CA01_122",
    two = "v_CA01_123",
    three = "v_CA01_124",
    four_five = "v_CA01_125",
    six_more = "v_CA01_126"
  ))

laval_household_size01 <- 
  laval_household_size01 |> 
  mutate(`1` = one/total,
         `2` = two/total,
         `3` = three/total,
         `4+` = (four_five + six_more) /total)

laval_household_size96 <- get_census(
  dataset = "CA1996",
  regions = list(CSD = 2465005),
  level = "CSD",
  vectors = c(
    total = "v_CA1996_116",
    one = "v_CA1996_117",
    two = "v_CA1996_118",
    three = "v_CA1996_119",
    four_five = "v_CA1996_120",
    six_more = "v_CA1996_121"
  ))

laval_household_size96 <- 
  laval_household_size96 |> 
  mutate(`1` = one/total,
         `2` = two/total,
         `3` = three/total,
         `4+` = (four_five + six_more) /total)

# Add the year column to each data frame
laval_household_size21 <- laval_household_size21 %>% mutate(year = 2021)  |> 
  select(year, une_personne, deux_personnes, trois_personnes, quatre_personnes_plus)
names(laval_household_size21) <- c("year", "1", "2", "3", "4+")
laval_household_size16 <- laval_household_size16 %>% mutate(year = 2016)  |> 
  select(year, c(`1`, `2` , `3`, `4+`))
laval_household_size11 <- laval_household_size11 %>% mutate(year = 2011)  |> 
  select(year, c(`1`, `2` , `3`, `4+`))
laval_household_size06 <- laval_household_size06 %>% mutate(year = 2006)  |> 
  select(year, c(`1`, `2` , `3`, `4+`))
laval_household_size01 <- laval_household_size01 %>% mutate(year = 2001)  |> 
  select(year, c(`1`, `2` , `3`, `4+`))
laval_household_size96 <- laval_household_size96 %>% mutate(year = 1996)  |> 
  select(year, c(`1`, `2` , `3`, `4+`))

# Combine the data frames into one
combined_data <- bind_rows(laval_household_size21, laval_household_size16, laval_household_size11, 
                           laval_household_size06, laval_household_size01, laval_household_size96)
  

# Reshape the data to a long format
long_data <- combined_data %>%
  pivot_longer(cols = c("1", "2" , "3", "4+"),
               names_to = "household_size",
               values_to = "percentage") |> 
  mutate(household_size = factor(household_size, 
                            levels =   c("1", "2" , "3", "4+")))

# Create the line graph with specified colors (Markdown)
household_evol_graph <- 
  ggplot(long_data, aes(x = year, y = percentage, color = household_size, group = household_size)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2.75) +
  scale_color_manual(values = c("1" = "#A3B0D1", 
                                "2" = "#73AD80", 
                                "3" = "#E08565",
                                "4+" = "#CD718C"),
                     name = "Personnes") +
  labs(y = "Proportion de ménages") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = unique(long_data$year)) +
  gg_cc_theme_no_sf +
  theme(axis.title.x = element_blank())

ggplot2::ggsave(filename = here::here("output/axe1/family/household_evol_graph.png"), 
                plot = household_evol_graph, width = 4, height = 3, bg = "transparent")



# Values for the text
famille_nb_menages <- convert_number(laval_household_size21_raw$total)
famille_nb_menages_aug <- laval_household_size21_raw$total - laval_household_size16_raw$total
famille_nb_menages_aug_pct <- convert_pct(famille_nb_menages_aug / laval_household_size16_raw$total)
famille_nb_menages_aug <- convert_number(famille_nb_menages_aug)

famille_taille_menage <- gsub("\\.", ",", laval_household_size21_raw$avg_size)
famille_taille_menage_qc <- gsub("\\.", ",", qc_household_size$avg_size)



















#evolution of household persons over time

laval_household_persons96 <- 
  laval_household_size96 |> 
  mutate(une_personne = one,
         deux_personnes = two * 2,
         trois_personnes = three * 3,
         quatre_personnes_plus = four_five * 4.5 + six_more * 6) |> 
  mutate(total = sum(c(une_personne, deux_personnes, trois_personnes, quatre_personnes_plus))) |> 
  select(GeoUID, total, une_personne, deux_personnes, trois_personnes, quatre_personnes_plus)

sort_vect <- 
  c("une_personne","deux_personnes" ,"trois_personnes","quatre_personnes_plus")

laval_household_persons96_long <- 
  laval_household_persons96 |> 
  pivot_longer(cols = c(une_personne, deux_personnes, trois_personnes, quatre_personnes_plus),
               names_to = "household_persons",
               values_to = "count") |> 
  mutate(household_persons = factor(household_persons, levels = sort_vect)) 


laval_household_persons96_long |> 
  ggplot(aes(x = household_persons, y = count))+
  geom_bar(stat = "identity", fill = "skyblue")+
  ylim(0, max_y_persons)

laval_household_persons_evol <- bind_rows(
  laval_household_persons21 |>  mutate(year = 2021),
  laval_household_persons96 |>  mutate(year = 1996)) |> 
  select(GeoUID, year, everything())

laval_household_persons_evol_long <- 
  laval_household_persons_evol |> 
  pivot_longer(cols = c(une_personne, deux_personnes, trois_personnes, quatre_personnes_plus),
               names_to = "household_persons",
               values_to = "count") |> 
  mutate(household_persons = factor(household_persons, levels = sort_vect)) 

# Assuming laval_household_persons_evol_pct is correctly formatted
laval_household_persons_evol_pct <- 
  laval_household_persons21 |> 
  mutate(un_evol = ((laval_household_persons21$une_personne - 
                        laval_household_persons96$une_personne)/
                       laval_household_persons96$une_personne)*100,
         deux_evol = ((laval_household_persons21$deux_personnes - 
                        laval_household_persons96$deux_personnes)/
                       laval_household_persons96$deux_personnes)*100,
         trois_evol = ((laval_household_persons21$trois_personnes - 
                          laval_household_persons96$trois_personnes)/
                         laval_household_persons96$trois_personnes)*100,
         quatre_plus_evol = ((laval_household_persons21$quatre_personnes_plus - 
                              laval_household_persons96$quatre_personnes_plus)/
                             laval_household_persons96$quatre_personnes_plus)*100) |> 
  mutate(un_evol = ifelse(un_evol >= 0, 
                           paste0("+", round(un_evol, 1), "%"), 
                           paste0(round(un_evol, 1), "%")),
         deux_evol = ifelse(deux_evol >= 0, 
                           paste0("+", round(deux_evol, 1), "%"), 
                           paste0(round(deux_evol, 1), "%")),
         trois_evol = ifelse(trois_evol >= 0, 
                             paste0("+", round(trois_evol, 1), "%"), 
                             paste0(round(trois_evol, 1), "%")),
         quatre_plus_evol = ifelse(quatre_plus_evol >= 0, 
                                 paste0("+", round(quatre_plus_evol, 1), "%"), 
                                 paste0(round(quatre_plus_evol, 1), "%"))) |> 
  pivot_longer(cols = c(un_evol, deux_evol, trois_evol, quatre_plus_evol),
               names_to = "household_persons",
               values_to = "percentage") |> 
  mutate(household_persons = recode(household_persons, 
                                    un_evol = "une_personne",
                                    deux_evol = "deux_personnes",
                                    trois_evol = "trois_personnes",
                                    quatre_plus_evol = "quatre_personnes_plus"))

# Merge percentage change data with the long format data
laval_household_persons_evol_long <- 
  laval_household_persons_evol_long |> 
  left_join(laval_household_persons_evol_pct, by = "household_persons") |> 
  mutate(household_persons = factor(household_persons, levels = sort_vect))

# Create the bar chart with percentage change labels
ggplot(laval_household_persons_evol_long, aes(x = household_persons, y = count, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(data = subset(laval_household_persons_evol_long, year == 2021), 
            aes(label = percentage, y = count + 0.05 * max(count)), 
            position = position_dodge(width = 0.9), 
            vjust = 0.5, 
            hjust = 0.05,
            size = 4, 
            color = "orange") +
  scale_fill_manual(values = c("1996" = "skyblue", "2021" = "orange"), name = "Year") +
  labs(title = "Nombre de personnes dans des ménages par taille de leur ménage à Laval (1996 c. 2021)",
       x = "Taille du ménage de la personne",
       y = "Nombre de personnes") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text("Année"))

  
#VISUALISATION

library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: Process the 2021 data
laval_household_persons21 <- 
  laval_household_size21 |> 
mutate(une_personne = one,
       deux_personnes = two * 2,
       trois_personnes = three * 3,
       quatre_personnes_plus = four * 4 + five_more * 5) |> 
  mutate(total = sum(c(une_personne, deux_personnes, trois_personnes, quatre_personnes_plus))) |> 
  select(GeoUID, total, une_personne, deux_personnes, trois_personnes, quatre_personnes_plus)


# Step 2: Process the 1996 data
laval_household_persons96 <- 
  laval_household_size96 |> 
  mutate(one = one,
         two = two * 2,
         three = three * 3,
         four_more = four_five * 4.5 + six_more * 6) |> 
  mutate(total = sum(c(one, two, three, four_more))) |> 
  select(GeoUID, total, one, two, three, four_more)

# Step 3: Combine data for both years
laval_household_persons_evol <- bind_rows(
  laval_household_persons21 |> mutate(year = 2021),
  laval_household_persons96 |> mutate(year = 1996)
) |> 
  select(GeoUID, year, everything())

# Step 4: Pivot to long format
laval_household_persons_evol_long <- 
  laval_household_persons_evol |> 
  pivot_longer(cols = c(une_personne, deux_personnes, trois_personnes, quatre_personnes_plus),
               names_to = "household_persons",
               values_to = "count") 
 
laval_household_persons_evol_long |> 
   mutate(household_persons = factor(household_persons, 
                                     levels = sort_vect))
                             

# Step 5: Create the side-by-side bar chart
ggplot(laval_household_persons_evol_long, aes(x = household_persons, y = count, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("1996" = "skyblue", "2021" = "orange"), name = "Year") +
  labs(title = "Nombre de personnes dans des ménages par taille de leur ménage à Laval (1996 c. 2021)",
       x = "Taille du ménage",
       y = "Nombre de personnes") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# TK

# Number of people in households 1996v2001 (Justin) -----------------------
lvl_hh_size96 <- get_census(dataset = "CA1996",
                                     regions = list(CSD = 2465005),
                                     level = "CSD",
                            vectors = c(total = "v_CA1996_94",
                                        one = "v_CA1996_117",
                                        two = "v_CA1996_118",
                                        three = "v_CA1996_119")) |> 
  mutate(year = 1996,
         one_p = one,
         two_p = two * 2,
         three_p = three * 3,
         four_plus = total - one - (two * 2) - (three * 3)) |> 
  select(year, one_p, two_p, three_p, four_plus)

lvl_hh_size21 <- get_census(dataset = "CA1996",
                            regions = list(CSD = 2465005),
                            level = "CSD",
                            vectors = c(total = "v_CA21_449", 
                                        one = "v_CA21_444",
                                        two = "v_CA21_445",
                                        three = "v_CA21_446")) |> 
  mutate(year = 2021,
         one_p = one,
         two_p = two * 2,
         three_p = three * 3,
         four_plus = total - one - (two * 2) - (three * 3)) |> 
  select(year, one_p, two_p, three_p, four_plus)

na_rows <- data.frame(difference = rep(NA, 4))

lvl_hh_size_change <- bind_rows(lvl_hh_size96, lvl_hh_size21) |> 
  select(-year) |> 
  t() |> 
  as.data.frame() |> 
  set_names(c("1996", "2021")) |> 
  mutate(difference = (`2021` / `1996`) - 1) |> 
  mutate(difference = convert_pct(x = difference)) |> 
  select(difference) |> 
  mutate(difference = paste0("+", difference))

lvl_hh_size_diff <- bind_rows(na_rows, lvl_hh_size_change)
rownames(lvl_hh_size_diff) <- NULL

lvl_hh_size <- bind_rows(lvl_hh_size96, lvl_hh_size21) |> 
  pivot_longer(cols = -year, names_to = "size", values_to = "count") |> 
  bind_cols(lvl_hh_size_diff) |> 
  mutate(size = factor(size, levels = c("one_p", "two_p", "three_p", "four_plus")))

hh_sizecomp_graph <- ggplot(lvl_hh_size, aes(x = size, y = count, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = difference),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  scale_fill_manual(values = c("1996" = "#CD718C", "2021" = "#A3B0D1")) +
  scale_x_discrete(labels = c("one_p" = "1 personne", "two_p" = "2 personnes",
                              "three_p" = "3 personnes", "four_plus" = "4+ personnes")) +
  labs(x = "La taille du ménage",
       y = "Nombre de personnes",
       fill = "Year") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank(),
        text=element_text(family="KMR Apparat Regular"))
# Number of people in private households ----------------------------------
#- Nombre de personnes dans les ménages privés

# People living alone by gender and age -----------------------------------
#- Personnes vivant seules (en fonction de sexe et d'âge)
 
#this stat should be obtained through the CISSS but there is partial
#information in the census

laval_living_alone21 <-  get_census(
  dataset = "CA21",
  regions = list(CSD = 2465005),
  level = "CSD",
  geo_format = "sf",
  vectors = c(
    total = "v_CA21_534",
    men = "v_CA21_535",
    women = "v_CA21_536" ))

laval_living_alone21 <- 
  laval_living_alone21 |> 
  mutate(men_pct = men/total,
         women_pct = women/total)


# Québec

qc_living_alone <-  get_census(
  dataset = "CA21",
  regions = list(PR = 24),
  level = "PR",
  geo_format = "sf",
  vectors = c(
    total = "v_CA21_534",
    men = "v_CA21_535",
    women = "v_CA21_536" ))

qc_living_alone <- 
  qc_living_alone |> 
  mutate(men_pct = men/total,
         women_pct = women/total)



# Save --------------------------------------------------------------------

qs::qsavem(family_size_graph, familles_nombreuse_pct, familles_nombreuse_qc_pct, 
           familles_taillemoy, familles_taillemoy_qc, famille_couple_enfants,
           famille_couple_aumoins1, famille_couple_aumoins1_2016,
           famille_couple_aumoins1_qc, famille_couple_aumoins1_qc_2016,
           famille_nb_enf_moyen, famille_marie, famille_marie_qc, 
           famille_marie_avecenf, famille_structure_plot, famille_monop,
           household_comp_graph, household_evol_graph, famille_nb_menages, 
           famille_nb_menages_aug_pct, famille_nb_menages_aug,
           famille_monop_f, famille_monop_m, file = "data/axe1/family.qsm")
