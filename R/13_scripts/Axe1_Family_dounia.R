install.packages("bookdown")
no
install.packages("devtools")

library(cancensus)
library(tidyverse)
devtools::install_github("Curbcut/curbcut")
no

View(list_census_vectors("CA21"))
View(list_census_regions("CA21"))

regions <- list_census_regions("CA21")
variables21 <- list_census_vectors("CA21")
variables16 <- list_census_vectors("CA16")
variables06 <- list_census_vectors("CA06")
variables01 <- list_census_vectors("CA01")
variables96 <- list_census_vectors("CA1996")
list_census_datasets()

CT <- cancensus::get_census(dataset = "CA21", 
                            regions = list(CSD = 2465005), 
                            level = "CT", 
                            geo_format = "sf")

CT <- cancensus::get_census(dataset = "CA21", 
                            regions = list(CSD = 2465005), 
                            level = "CT", 
                            geo_format = "sf")


laval_census <- cancensus::get_census(dataset = "CA21", 
                                      regions = list(CSD = 2465005), 
                                      level = "CSD")



# Family characteristics --------------------------------------------------



# Family size  --------
# - Familles de recensement dans les ménages privés selon la taille de la famille


## For laval CTs
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



## for laval as a whole

laval_family_size <-  get_census(
  dataset = "CA21",
  regions = list(CSD = 2465005),
  level = "CSD",
  geo_format = "sf",
  vectors = c(
    family_size = "v_CA21_492",
    family_2 = "v_CA21_493",
    family_3 = "v_CA21_494",
    family_4 = "v_CA21_495",
    family_5 = "v_CA21_496"))

laval_family_size <- 
  laval_family_size |> 
  mutate(family_2_pct = family_2/family_size,
         family_3_pct = family_3/family_size,
         family_4_pct = family_4/family_size,
         family_5_pct = family_5/family_size)

# Create a dataframe for plotting
family_size_plot <- data.frame(
  family_size = c("2 people", "3 people", "4 people", "5 or more people"),
  percentage = c(laval_family_size$family_2_pct, laval_family_size$family_3_pct, laval_family_size$family_4_pct, laval_family_size$family_5_pct)
)



# Modify the ggplot code
ggplot(family_size_plot, aes(x = family_size, y = percentage)) +
  geom_bar(stat = "identity") +
  labs(x = "Family Size", y = "Percentage", title = "Percentage of families by family size in Laval") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")  # Center title and make it bold
  ) 




#québec

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


# Average number of children ----------------------------------------------

# - Nombre moyen d'enfants dans les familles de recensement avec enfants

#Laval CT
avg_children <-  get_census(
  dataset = "CA21",
  regions = list(CSD = 2465005),
  level = "CT",
  geo_format = "sf",
  vectors =
    c(avg_child = "v_CA21_498"))

ggplot(avg_children) +
  geom_sf(aes(fill = avg_child)) +
  scale_fill_viridis_c() +  
  labs(title = "Average Number of Children per Census Family in Laval", fill = "Average Number of Children") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


#Laval total 2021
laval_avg_children_21 <-  get_census(
  dataset = "CA21",
  regions = list(CSD = 2465005),
  level = "CSD",
  geo_format = "sf",
  vectors = c(
    avg_child = "v_CA21_498"))

### for 2016, no average number of children
    

#Laval 2006
laval_avg_children_06 <-  get_census(
  dataset = "CA06",
  regions = list(CSD = 2465005),
  level = "CSD",
  geo_format = "sf",
  vectors = c(
    avg_child = "v_CA06_84"))

# Laval 2001
laval_avg_children_01 <-  get_census(
  dataset = "CA01",
  regions = list(CSD = 2465005),
  level = "CSD",
  geo_format = "sf",
  vectors = c(
    avg_child = "v_CA01_82"))


#Laval 1996 has no average number of children

#avg children over time

# Assuming laval_avg_children_21, laval_avg_children_06, and laval_avg_children_01 are already obtained

# Extract and format the data for each year
laval_avg_children_21 <- laval_avg_children_21 %>%
  mutate(year = 2021, avg_child = avg_child)

laval_avg_children_06 <- laval_avg_children_06 %>%
  mutate(year = 2006, avg_child = avg_child)

laval_avg_children_01 <- laval_avg_children_01 %>%
  mutate(year = 2001, avg_child = avg_child)

# Combine data into a single data frame
laval_avg_children <- bind_rows(
  laval_avg_children_21 %>% select(year, avg_child),
  laval_avg_children_06 %>% select(year, avg_child),
  laval_avg_children_01 %>% select(year, avg_child)
)

# Plot the data using ggplot2
ggplot(data = laval_avg_children, aes(x = year, y = avg_child)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(
    title = "Average Number of Children Over Time in Laval",
    x = "Year",
    y = "Average Number of Children"
  ) +
  theme_minimal()

#Québec 2021
qc_avg_children <-  get_census(
  dataset = "CA21",
  regions = list(PR = 24),
  level = "PR",
  geo_format = "sf",
  vectors = c(
    avg_child = "v_CA21_498"))




# Families in private households ------------------------------------------
# - Familles de recensement dans les ménages privés

## For laval CTs
household_family <-  get_census(
  dataset = "CA21",
  regions = list(CSD = 2465005),
  level = "CT",
  geo_format = "sf",
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

household_family_relative <- 
  household_family |> 
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

#for laval as a whole
laval_household_family_21 <-  get_census(
  dataset = "CA21",
  regions = list(CSD = 2465005),
  level = "CSD",
  geo_format = "sf",
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

#### differentiated percentages###
laval_household_family_relative <- 
  laval_household_family |> 
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



### total percentages

laval_household_family_21 <- 
  laval_household_family_21 |> 
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

#laval 2016
###############check that variables are the same
laval_household_family_16 <-  get_census(
  dataset = "CA16",
  regions = list(CSD = 2465005),
  level = "CSD",
  geo_format = "sf",
  vectors = c(
    household_total = "v_CA16_484",
    couple_fam = "v_CA16_485",
    couple_fam_married = "v_CA16_486",
    couple_fam_cl = "v_CA16_487",
    one_parent = "v_CA16_488",
    one_parent_woman = "v_CA16_489",
    one_parent_man = "v_CA16_490"
  ))


laval_household_family_16 <- 
  laval_household_family_16 |> 
  mutate(couple_fam_pct = couple_fam/household_total,
         couple_fam_married_pct = couple_fam_married/household_total,
         couple_fam_cl_pct = couple_fam_cl/household_total,
         one_parent_pct = one_parent/household_total,
         one_parent_woman_pct = one_parent_woman / household_total,
         one_parent_man_pct = one_parent_man / household_total)


#household families 2006

laval_household_family_06 <-  get_census(
  dataset = "CA06",
  regions = list(CSD = 2465005),
  level = "CSD",
  geo_format = "sf",
  vectors = c(
    household_total = "v_CA06_55",
    couple_fam = "v_CA06_56",
    couple_fam_married = "v_CA06_57",
    couple_fam_married_child = "v_CA06_59",
    couple_fam_married_nochild = "v_CA06_58",
    couple_fam_cl = "v_CA06_63",
    couple_fam_cl_child = "v_CA06_65",
    couple_fam_cl_nochild = "v_CA06_64",
    one_parent = "v_CA06_69",
    one_parent_woman = "v_CA06_70",
    one_parent_man = "v_CA06_74"
      ))


laval_household_family_06 <- 
  laval_household_family_06 |> 
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


#laval 2001

laval_household_family_01 <-  get_census(
  dataset = "CA01",
  regions = list(CSD = 2465005),
  level = "CSD",
  geo_format = "sf",
  vectors = c(
    household_total = "v_CA01_53",
    couple_fam = "v_CA01_54",
    couple_fam_married = "v_CA01_55",
    couple_fam_married_child = "v_CA01_57",
    couple_fam_married_nochild = "v_CA01_56",
    couple_fam_cl = "v_CA01_61",
    couple_fam_cl_child = "v_CA01_63",
    couple_fam_cl_nochild = "v_CA01_62",
    one_parent = "v_CA01_67",
    one_parent_woman = "v_CA01_68",
    one_parent_man = "v_CA01_72"
  ))


laval_household_family_01 <- 
  laval_household_family_01 |> 
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


# 1996
laval_household_family_96 <-  get_census(
  dataset = "CA1996",
  regions = list(CSD = 2465005),
  level = "CSD",
  geo_format = "sf",
  vectors = c(
    household_total = "v_CA1996_65",
    couple_fam_married = "v_CA1996_66",
    couple_fam_married_child = "v_CA1996_68",
    couple_fam_married_nochild = "v_CA1996_67",
    couple_fam_cl = "v_CA1996_72",
    couple_fam_cl_child = "v_CA1996_74",
    couple_fam_cl_nochild = "v_CA1996_73",
    one_parent = "v_CA1996_78",
    one_parent_woman = "v_CA1996_83",
    one_parent_man = "v_CA1996_79"
  ))


laval_household_family_96 <- 
  laval_household_family_96 |> 
  mutate(couple_fam_married_pct = couple_fam_married/household_total,
         couple_fam_married_child_pct = couple_fam_married_child/household_total,
         couple_fam_married_nochild_pct = couple_fam_married_nochild/household_total,
         couple_fam_cl_pct = couple_fam_cl/household_total,
         couple_fam_cl_child_pct = couple_fam_cl_child/household_total,
         couple_fam_cl_nochild_pct = couple_fam_cl_nochild/household_total,
         one_parent_pct = one_parent/household_total,
         one_parent_woman_pct = one_parent_woman / household_total,
         one_parent_man_pct = one_parent_man / household_total
  ) 


#household family over time

# Load necessary libraries
library(cancensus)
library(tidyverse)
library(sf)
library(ggplot2)

# Fetch and process data for each year
years <- c(2021, 2016, 2006, 2001)

# Define a function to fetch and process data
fetch_and_process_data <- function(year) {
  if (year == 2021) {
    dataset <- "CA21"
    vectors <- c(
      household_total = "v_CA21_499",
      couple_fam = "v_CA21_500",
      couple_fam_married = "v_CA21_501",
      couple_fam_cl = "v_CA21_504",
      one_parent = "v_CA21_507"
    )
  } else if (year == 2016) {
    dataset <- "CA16"
    vectors <- c(
      household_total = "v_CA16_484",
      couple_fam = "v_CA16_485",
      couple_fam_married = "v_CA16_486",
      couple_fam_cl = "v_CA16_487",
      one_parent = "v_CA16_488"
    )
  } else if (year == 2006) {
    dataset <- "CA06"
    vectors <- c(
      household_total = "v_CA06_55",
      couple_fam = "v_CA06_56",
      couple_fam_married = "v_CA06_57",
      couple_fam_cl = "v_CA06_63",
      one_parent = "v_CA06_69"
    )
  } else if (year == 2001) {
    dataset <- "CA01"
    vectors <- c(
      household_total = "v_CA01_53",
      couple_fam = "v_CA01_54",
      couple_fam_married = "v_CA01_55",
      couple_fam_cl = "v_CA01_61",
      one_parent = "v_CA01_67"
    )
  }
  
  data <- get_census(
    dataset = dataset,
    regions = list(CSD = 2465005),
    level = "CSD",
    geo_format = "sf",
    vectors = vectors
  )
  
  data <- data %>%
    mutate(
      year = year,
      couple_fam_married_pct = couple_fam_married / household_total,
      couple_fam_cl_pct = couple_fam_cl / household_total,
      single_parent_pct = one_parent / household_total
    ) %>%
    select(year, couple_fam_married_pct, couple_fam_cl_pct, single_parent_pct) %>%
    st_drop_geometry()
  
  return(data)
}

# Fetch and process data for all years
data_list <- lapply(years, fetch_and_process_data)
combined_data <- bind_rows(data_list)

# Pivot data for ggplot
long_data <- combined_data %>%
  pivot_longer(cols = -year, names_to = "family_type", values_to = "percentage")

# Plot the data
ggplot(long_data, aes(x = as.factor(year), y = percentage, fill = family_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Family Structure in Laval Over Time",
    x = "Year",
    y = "Percentage"
  ) +
  scale_fill_manual(
    values = c("couple_fam_married_pct" = "skyblue", 
               "couple_fam_cl_pct" = "lightgreen", 
               "single_parent_pct" = "pink"),
    labels = c("Married Couples", "Common Law Couples", "Single Parents")
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

ggplot(long_data, aes(x = as.factor(year), y = percentage, color = family_type, group = family_type)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = scales::percent(percentage, accuracy = 0.1)), 
            vjust = -1, size = 3) +
  labs(
    title = "Family Structure in Laval Over Time",
    x = "Year",
    y = "Percentage"
  ) +
  scale_color_manual(
    values = c("couple_fam_married_pct" = "skyblue",
               "couple_fam_cl_pct" = "lightgreen",
               "single_parent_pct" = "pink"),
    labels = c("couple_fam_married_pct" = "Married Couples",
               "couple_fam_cl_pct" = "Common Law Couples",
               "single_parent_pct" = "Single Parents")
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())



#laval plot

household_family_plot <- data.frame(
  category = c("Couple Family", "Married Couple with Children", "Married Couple without Children", 
               "Common-law Couple", "Common-law Couple with Children", "Common-law Couple without Children", 
               "One Parent", "One Parent (Woman)", "One Parent (Man)"),
  percentage = c(
    mean(laval_household_family$couple_fam_pct),
    mean(laval_household_family$couple_fam_married_child_pct),
    mean(laval_household_family$couple_fam_married_nochild_pct),
    mean(laval_household_family$couple_fam_cl_pct),
    mean(laval_household_family$couple_fam_cl_child_pct),
    mean(laval_household_family$couple_fam_cl_nochild_pct),
    mean(laval_household_family$one_parent_pct),
    mean(laval_household_family$one_parent_woman_pct),
    mean(laval_household_family$one_parent_man_pct)
  )
)


######THIS IS EXPERIMENTAL

married <- laval_household_family %>%
  summarize(
    married_with_children = mean(couple_fam_married_child_pct),
    married_without_children = mean(couple_fam_married_nochild_pct)
  )

common_law <- laval_household_family %>%
  summarize(
    common_law_with_children = mean(couple_fam_cl_child_pct),
    common_law_without_children = mean(couple_fam_cl_nochild_pct)
  )

single_parent <- laval_household_family %>%
  summarize(
    single_parent_woman = mean(one_parent_woman_pct),
    single_parent_man = mean(one_parent_man_pct))

# Create a data frame for plotting
plot_data <- data.frame(
  category = c("Married", "Common Law", "Single Parent (Woman)", "Single Parent (Man)"),
  with_children = c(married$married_with_children, 
                    common_law$common_law_with_children, 
                    single_parent$single_parent_woman, 
                    single_parent$single_parent_man),
  without_children = c(married$married_without_children, 
                       common_law$common_law_without_children, 0, 0))

# Reshape the data for stacked bar plot
plot_data <- plot_data %>%
  pivot_longer(cols = c(with_children, without_children), 
               names_to = "child_status", 
               values_to = "percentage")


# Create stacked bar plot
laval_plot <- 
ggplot(plot_data, aes(x = category, y = percentage, fill = child_status)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Household Family Composition in Laval",
       y = "Percentage",
       x = "Family Type",
       fill = "Children Status") +
  scale_fill_manual(values = c("with_children" = "blue", "without_children" = "lightblue")) + # Adjust colors
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5))



#for québec

qc_household_family <-  get_census(
  dataset = "CA21",
  regions = list(PR = 24),
  level = "PR",
  geo_format = "sf",
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
### differenciated
qc_household_family_relative <- 
  qc_household_family |> 
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

### absolute
qc_household_family <- 
  qc_household_family |> 
  mutate(couple_fam_pct = couple_fam/household_total,
         couple_fam_married_pct = couple_fam_married/household_total,
         couple_fam_married_child_pct = couple_fam_married_child/household_total,
         couple_fam_married_nochild_pct = couple_fam_married_nochild/household_total,
         couple_fam_cl_pct = couple_fam_cl/household_total,
         couple_fam_cl_child_pct = couple_fam_cl_child/household_total,
         couple_fam_cl_nochild_pct = couple_fam_cl_nochild/household_total,
         one_parent_pct = one_parent/household_total,
         one_parent_woman_pct = one_parent_woman / household_total,
         one_parent_man_pct = one_parent_man / household_total,
  ) 

#PLOT for QC


married_qc <- qc_household_family %>%
  summarize(
    married_with_children = mean(couple_fam_married_child_pct),
    married_without_children = mean(couple_fam_married_nochild_pct)
  )

common_law_qc <- qc_household_family %>%
  summarize(
    common_law_with_children = mean(couple_fam_cl_child_pct),
    common_law_without_children = mean(couple_fam_cl_nochild_pct)
  )

single_parent_qc <- qc_household_family %>%
  summarize(
    single_parent_woman = mean(one_parent_woman_pct),
    single_parent_man = mean(one_parent_man_pct))

# Create a data frame for plotting
plot_data_qc <- data.frame(
  category = c("Married", "Common Law", "Single Parent (Woman)", "Single Parent (Man)"),
  with_children = c(married_qc$married_with_children, 
                    common_law_qc$common_law_with_children, 
                    single_parent_qc$single_parent_woman, 
                    single_parent_qc$single_parent_man),
  without_children = c(married_qc$married_without_children, 
                       common_law_qc$common_law_without_children, 0, 0))

# Reshape the data for stacked bar plot
plot_data_qc <- plot_data_qc %>%
  pivot_longer(cols = c(with_children, without_children), 
               names_to = "child_status", 
               values_to = "percentage")




#### plot of laval and québec's household composition


max_y <- max(max(laval_household_family$couple_fam_married_pct), max(qc_household_family$couple_fam_married_pct))

plot_laval <- ggplot(plot_data, aes(x = category, y = percentage, fill = child_status)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Household Family Composition in Laval",
       y = "Percentage",
       x = "Family Type",
       fill = "Children Status") +
  scale_fill_manual(values = c("with_children" = "blue", "without_children" = "lightblue")) + # Adjust colors
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_blank(),  # Remove y-axis label to avoid duplication
    axis.ticks.y = element_blank(),  # Remove y-axis ticks for aesthetic purposes
    plot.margin = margin(5.5, 5.5, 5.5, 5.5)  # Adjust plot margins for alignment
  ) +
  ylim(0, max_y)  # Set the y-axis limit

# Create stacked bar plot for Quebec
plot_qc <- ggplot(plot_data_qc, aes(x = category, y = percentage, fill = child_status)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Household Family Composition in Québec",
       y = NULL,  # We'll set y-axis label to NULL to avoid duplication
       x = "Family Type",
       fill = "Children Status") +
  scale_fill_manual(values = c("with_children" = "blue", "without_children" = "lightblue")) + # Adjust colors
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_blank(),  # Remove y-axis label to avoid duplication
    axis.ticks.y = element_blank(),  # Remove y-axis ticks for aesthetic purposes
    plot.margin = margin(5.5, 5.5, 5.5, 5.5)  # Adjust plot margins for alignment
  ) +
  ylim(0, max_y)  # Set the y-axis limit


# Combine both plots
combined_plots <- ggplot() +
  geom_blank() +
  theme_void() +
  facet_wrap(~ ., ncol = 2, scales = "free_y") +  # Ensure y-axes have the same scale
  theme(panel.spacing = unit(2, "lines")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(clip = "off") +  # To prevent clipping of geom_blank()
  annotation_custom(ggplotGrob(plot_laval), xmin=-Inf, xmax=0.5, ymin=-Inf, ymax=Inf) +
  annotation_custom(ggplotGrob(plot_qc), xmin=0.5, xmax=Inf, ymin=-Inf, ymax=Inf)

# Plot combined plots
print(combined_plots)








# Gender of people in families --------------------------------------------
# - Type de familles (parents de sexe féminin, masculin, etc)

laval_family_gender <-  get_census(
  dataset = "CA21",
  regions = list(CSD = 2465005),
  level = "CSD",
  geo_format = "sf",
  vectors = c(
    total = "v_CA21_513",
    women = "v_CA21_514",
    men = "v_CA21_515"))

laval_family_gender <- 
  laval_family_gender |> 
  mutate(women_pct = women/total,
         men_pct = men/total)


# - Nombre de familles
#used family size total numbers

# - Âge des mères à la naissance du premier enfant
# this is another dataset from statscan, I need to figure out how to retrieve it
#and which year to choose from


# Marital status ----------------------------------------------------------
# État matrimonial pour la population totale âgée de 15 ans et plus

laval_marital_status <-  get_census(
  dataset = "CA21",
  regions = list(CSD = 2465005),
  level = "CSD",
  geo_format = "sf",
  vectors = c(
    total = "v_CA21_453",
    married_cl = "v_CA21_456",
    married = "v_CA21_459",
    cl = "v_CA21_462",
    not_married_not_cl = "v_CA21_477"
    ))

laval_marital_status <- 
  laval_marital_status |> 
  mutate(married_pct = married/total,
         cl_pct = cl/total,
         not_married_not_cl_pct = not_married_not_cl/total)


#Québec
qc_marital_status <-  get_census(
  dataset = "CA21",
  regions = list(PR = 24),
  level = "PR",
  geo_format = "sf",
  vectors = c(
    total = "v_CA21_453",
    married_cl = "v_CA21_456",
    married = "v_CA21_459",
    cl = "v_CA21_462",
    not_married_not_cl = "v_CA21_477"
  ))

qc_marital_status <- 
  qc_marital_status |> 
  mutate(married_pct = married/total,
         cl_pct = cl/total,
         not_married_not_cl_pct = not_married_not_cl/total)


# Create stacked bar plot

library(ggplot2)
library(dplyr)
library(tidyr)

# Function to create bar plots
create_bar_plot <- function(data, location) {
  ggplot(data, aes(x = marital_status, y = percentage, fill = marital_status)) +
    geom_bar(stat = "identity") +
    labs(title = paste("Marital Status in", location),
         x = "Marital Status",
         y = "Percentage") +
    theme_minimal() +
    theme(legend.position = "none")  # Remove legend
}

# Reshape data for Laval
laval_plot_data <- laval_marital_status %>%
  select(married_pct, cl_pct, not_married_not_cl_pct) %>%
  pivot_longer(cols = c(married_pct, cl_pct, not_married_not_cl_pct), 
               names_to = "marital_status", values_to = "percentage")




# Reshape data for Quebec
qc_plot_data <- qc_marital_status %>%
  select(marital_status, percentage) |> 
  pivot_longer(cols = c(married_pct, cl_pct, not_married_not_cl_pct), 
               names_to = "marital_status", values_to = "percentage")

# Create bar plots for Laval and Quebec
plot_laval <- create_bar_plot(laval_plot_data, "Laval")
plot_qc <- create_bar_plot(qc_plot_data, "Québec")

# Plot bar plots separately
print(plot_laval)
print(plot_qc)


marital_plot_wrap <- 
  ggplot() +
  geom_blank() +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_blank(),  # Remove y-axis label to avoid duplication
    axis.ticks.y = element_blank(),  # Remove y-axis ticks for aesthetic purposes
    plot.margin = margin(5.5, 5.5, 5.5, 5.5)) + # Adjust plot margins for alignment
  facet_wrap(~ ., ncol = 2, scales = "free_y") +  # Ensure y-axes have the same scale
  theme(panel.spacing = unit(2, "lines")) +
  coord_cartesian(clip = "off") +  # To prevent clipping of geom_blank()
  annotation_custom(ggplotGrob(plot_laval), xmin=-Inf, xmax=0.5, ymin=-Inf, ymax=Inf) +
  annotation_custom(ggplotGrob(plot_qc), xmin=0.5, xmax=Inf, ymin=-Inf, ymax=Inf)
 


#time comparison
laval_marital_status16 <-  get_census(
  dataset = "CA16",
  regions = list(CSD = 2465005),
  level = "CSD",
  geo_format = "sf",
  vectors = c(
    total = "v_CA16_451",
    married_cl = "v_CA16_454",
    married = "v_CA16_457",
    cl = "v_CA16_460",
    not_married_not_cl = "v_CA16_463"
  ))

laval_marital_status16 <- 
  laval_marital_status16 |> 
  mutate(married_pct = married/total,
         cl_pct = cl/total,
         not_married_not_cl_pct = not_married_not_cl/total)





# Household size -----------------------------------------------------------
#- Ménages privés selon la taille du ménage

#per laval CT
household_size <-  get_census(
  dataset = "CA21",
  regions = list(CSD = 2465005),
  level = "CT",
  geo_format = "sf",
  vectors = c(
    avg_size = "v_CA21_452",
    total = "v_CA21_443",
    one = "v_CA21_444",
    two = "v_CA21_445",
    three = "v_CA21_446",
    four = "v_CA21_447",
    five_more = "v_CA21_448"
  ))

household_size <- 
  household_size |> 
  mutate(one_pct = one/total,
         two_pct = two/total,
         three_pct = three/total,
         four_pct = four/total,
         five_more_pct = five_more/total)


#laval

laval_household_size21 <-  get_census(
  dataset = "CA21",
  regions = list(CSD = 2465005),
  level = "CSD",
  geo_format = "sf",
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
  laval_household_size21 |> 
  mutate(one_pct = one/total,
         two_pct = two/total,
         three_pct = three/total,
         four_more_pct = (four + five_more)/total)

#québec
qc_household_size <-  get_census(
  dataset = "CA21",
  regions = list(PR = 24),
  level = "PR",
  geo_format = "sf",
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
  mutate(one_pct = one/total,
         two_pct = two/total,
         three_pct = three/total,
         four_pct = four/total,
         five_more_pct = five_more/total)


### wrapped plot
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(sf)

# Select only numeric columns for pivoting
laval_household_size_numeric <- laval_household_size %>%
  select(GeoUID, one_pct, two_pct, three_pct, four_pct, five_more_pct) |> 
  st_drop_geometry()

qc_household_size_numeric <- qc_household_size %>%
  select(GeoUID, one_pct, two_pct, three_pct, four_pct, five_more_pct) |> 
  st_drop_geometry()


#pivot tables
laval_household_size_pivot <- laval_household_size_numeric %>%
  pivot_longer(cols = -GeoUID, names_to = "household_size", values_to = "percentage") %>%
  mutate(household_size = factor(household_size, levels = c("one_pct", "two_pct", "three_pct", "four_pct", "five_more_pct")))

qc_household_size_pivot <- qc_household_size_numeric %>%
  pivot_longer(cols = -GeoUID, names_to = "household_size", values_to = "percentage") %>%
  mutate(household_size = factor(household_size, levels = c("one_pct", "two_pct", "three_pct", "four_pct", "five_more_pct")))

# Find the maximum count to set the y-axis limit
max_count <- max(max(laval_household_size_pivot$percentage), max(qc_household_size_pivot$percentage))

# Plot for Laval household size
plot_laval <- ggplot(data = laval_household_size_pivot, aes(x = household_size, y = percentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  ylim(0, max_count) +
  labs(
    title = "Household Size Distribution in Laval",
    x = "Number of people in Household",
    y = "Percentage"
  ) +
  theme_minimal() +
theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")  # Center title and make it bold
  ) 

# Plot for Quebec household size
plot_qc <- ggplot(data = qc_household_size_pivot, aes(x = household_size, y = percentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  ylim(0, max_count) +
  labs(
    title = "Household Size Distribution in Quebec",
    x = "Number of People in Household",
    y = "Percentage"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")  # Center title and make it bold
  ) 


# Combine plots
grid.arrange(plot_laval, plot_qc, ncol = 2, top = "Household Size Distribution in Laval and Québec")

#comparison over time



laval_household_size16 <-  get_census(
  dataset = "CA16",
  regions = list(CSD = 2465005),
  level = "CSD",
  geo_format = "sf",
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
  laval_household_size16 |> 
  mutate(one_pct = one/total,
         two_pct = two/total,
         three_pct = three/total,
         four_more_pct = (four+five_more)/total)


laval_household_size06 <-  get_census(
  dataset = "CA06",
  regions = list(CSD = 2465005),
  level = "CSD",
  geo_format = "sf",
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
  mutate(one_pct = one/total,
         two_pct = two/total,
         three_pct = three/total,
         four_more_pct = (four_five + six_more) /total)


laval_household_size01 <-  get_census(
  dataset = "CA01",
  regions = list(CSD = 2465005),
  level = "CSD",
  geo_format = "sf",
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
  mutate(one_pct = one/total,
         two_pct = two/total,
         three_pct = three/total,
         four_more_pct = (four_five + six_more) /total)

laval_household_size96 <-  get_census(
  dataset = "CA1996",
  regions = list(CSD = 2465005),
  level = "CSD",
  geo_format = "sf",
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
  mutate(one_pct = one/total,
         two_pct = two/total,
         three_pct = three/total,
         four_more_pct = (four_five + six_more) /total)

# graph

# Add the year column to each data frame
laval_household_size21 <- laval_household_size21 %>% mutate(year = 2021)
laval_household_size16 <- laval_household_size16 %>% mutate(year = 2016)
laval_household_size06 <- laval_household_size06 %>% mutate(year = 2006)
laval_household_size01 <- laval_household_size01 %>% mutate(year = 2001)
laval_household_size96 <- laval_household_size96 %>% mutate(year = 1996)

# Select and rename columns to be consistent across data frames
laval_household_size21 <- laval_household_size21 %>%
  select(year, one_pct, two_pct, three_pct, four_more_pct)

laval_household_size16 <- laval_household_size16 %>%
  select(year, one_pct, two_pct, three_pct, four_more_pct)

laval_household_size06 <- laval_household_size06 %>%
  select(year, one_pct, two_pct, three_pct, four_more_pct)

laval_household_size01 <- laval_household_size01 %>%
  select(year, one_pct, two_pct, three_pct, four_more_pct)

laval_household_size96 <- laval_household_size96 %>%
  select(year, one_pct, two_pct, three_pct, four_more_pct)

# Combine the data frames into one
combined_data <- bind_rows(laval_household_size21, laval_household_size16, laval_household_size06,
                           laval_household_size01, laval_household_size96)

# Reshape the data to a long format
long_data <- combined_data %>%
  pivot_longer(cols = c(one_pct, two_pct, three_pct, four_more_pct),
               names_to = "household_size",
               values_to = "percentage")

# Define the colors for each household size category
household_colors <- c("one_pct" = "#CFFDBC",  # Pale green
                      "two_pct" = "#90EE90",  # Light green
                      "three_pct" = "#32CD32",  # Normal green
                      "four_more_pct" = "#006400")  # Dark green

# Create the line graph with specified colors
ggplot(long_data, aes(x = year, y = percentage, color = household_size, group = household_size)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = household_colors) +  # Apply the custom colors
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Household Size Percentages in Laval Over Time",
       x = "Year",
       y = "Percentage of Households",
       color = "Household Size") +
  theme_minimal()



# Number of people in private households ----------------------------------


#- Nombre de personnes dans les ménages privés


# People living alone by gender and age -----------------------------------
#- Personnes vivant seules (en fonction de sexe et d'âge)
    
laval_living_alone <-  get_census(
  dataset = "CA21",
  regions = list(CSD = 2465005),
  level = "CSD",
  geo_format = "sf",
  vectors = c(
    total = "v_CA21_534",
    men = "v_CA21_535",
    women = "v_CA21_536" ))

laval_living_alone <- 
  laval_living_alone |> 
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
  laval_living_alone |> 
  mutate(men_pct = men/total,
         women_pct = women/total)
