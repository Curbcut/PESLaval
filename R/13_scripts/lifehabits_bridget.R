### Axe 1 -- Habitudes de Vie ###
# note: most data is being pulled from written reports

# Smoking Habits ------------------------------------------------------------

# for laval
smoking_laval <- data.frame(`Region` = c("Laval"),
                            `Women2015` = c(15.6),
                            `Women2021`= c(12.0),
                            `Men2015` = c(20.5),
                            `Men2021` = c(18.2),
                            `Total2015` = c(18.0),
                            `Total2021` = c(15.1))

# Pivot 
smoking_laval_long <- smoking_laval |> 
  pivot_longer(cols = -Region, names_to = "Category", values_to = "Value")

# Separate the 'Category' column into 'Gender' and 'Year'
smoking_laval_long <- smoking_laval_long |> 
  mutate(Year = sub(".*([0-9]{4})$", "\\1", Category),
         Gender = sub("([A-Za-z]+).*", "\\1", Category))

# keep columns in correct order
smoking_laval_long$Gender <-
  factor(smoking_laval_long$Gender, levels = unique(smoking_laval_long$Gender))



ggplot(data = smoking_laval_long, aes(x = Year, y = Value, fill = Gender))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Women" = "pink", "Men" = "blue", "Total" = "grey")) +
  geom_text(aes(label = paste0(Value, "%")), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Smoking Population in Laval", x = "Year", y = "Percent") +
  facet_wrap(~ Gender)


  

#Consumption Habits - Alcohol -------------------------------------------------

#make df of alcohol consumption from report in order to make visualisations

alcohol_consumption <- data.frame(`Region` = c("Laval"),
                                   `None` = c(27),
                                  `Less_than_three_times_per_month` = c(36.2),
                                   `One_to_six_times_per_week` = c(31.5),
                                   `Everyday` = c(5.3))

# pivot longer
alcohol_consumption <- alcohol_consumption |> 
  pivot_longer(-c(Region))

# keep columns in correct order
alcohol_consumption$name <-
  factor(alcohol_consumption$name, levels = unique(alcohol_consumption$name))

# mutate to differentiate between consumption
alcohol_consumption_mutated <- alcohol_consumption |> 
  mutate("Consumed Alcohol" = ifelse(row_number() > n() - 3, "Yes", "No"))


plotlaval <- 
  ggplot(data = alcohol_consumption_mutated, aes(x = `Consumed Alcohol`, y = value, fill = name)) +
  geom_bar(stat = "identity") +
  labs(title = "Alcohol Consumption Laval", x = "Consumed Alcohol over 12-month Period", y = "Percentage") +
  geom_text(aes(label = paste0(value, "%")), position = position_stack(vjust = 0.5), color = "white")

# compared to quebec
alcohol_consumption_qc <- data.frame(`Region` = c("Quebec"),
                                  `None` = c(20.7),
                                  `Less_than_three_times_per_month` = c(32.1),
                                  `One_to_six_times_per_week` = c(41.5),
                                  `Everyday` = c(5.6))

# pivot longer
alcohol_consumption_qc <- alcohol_consumption_qc |> 
  pivot_longer(-c(Region))

# keep columns in correct order
alcohol_consumption_qc$name <-
  factor(alcohol_consumption_qc$name, levels = unique(alcohol_consumption_qc$name))

# mutate to differentiate between consumption
alcohol_consumption_qc_mutated <- alcohol_consumption_qc |> 
  mutate("Consumed Alcohol" = ifelse(row_number() > n() - 3, "Yes", "No"))



plotqc <- 
  ggplot(data = alcohol_consumption_qc_mutated, aes(x = `Consumed Alcohol`, y = value, fill = name)) +
  geom_bar(stat = "identity") +
  labs(title = "Alcohol Consumption Quebec", x = "Consumed Alcohol over 12-month Period", y = "Percentage", fill = "Consumption Habits") +
  geom_text(aes(label = paste0(value, "%")), position = position_stack(vjust = 0.5), color = "white")

#arrange laval and qc plots
library(gridExtra)

combined_alcohol_plot <- grid.arrange(plotlaval, plotqc, ncol = 2)



#Consumption Habits - Excessive Alcohol----------------------------------------

excess_alcohol <- data.frame(`Region` = c("Laval"),
                             `Women` = c(14.4),
                             `Men` = c(17.1),
                             `Total` = c(15.8))
# pivot longer
excess_alcohol <- excess_alcohol |> 
  pivot_longer(-c(Region))

# keep columns in correct order
excess_alcohol$name <-
  factor(excess_alcohol$name, levels = unique(excess_alcohol$name))

# plot
excess_alcohol_plot <-
  ggplot(data = excess_alcohol, aes(x = name, y = value, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Women" = "pink", "Men" = "blue", "Total" = "grey")) +
  geom_text(aes(label = paste0(value, "%")), position = position_dodge(width = 0.9), vjust = -0.5) +
  ylim(0, 27) +
  labs(title = "Laval", y = "Percent", x = "Population")


# make data frame for qc

excess_alcohol_qc <- data.frame(`Region` = c("Quebec"),
                                `Women` = c(20.5),
                                `Men` = c(26.7),
                                `Total` = c(23.6))

#pivot
excess_alcohol_qc <- excess_alcohol_qc |> 
  pivot_longer(-c(Region))

#keep columns in order
excess_alcohol_qc$name <-
  factor(excess_alcohol_qc$name, levels = unique(excess_alcohol_qc$name))

excess_alcohol_qc_plot <-
  ggplot(data = excess_alcohol_qc, aes(x = name, y = value, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Women" = "pink", "Men" = "blue", "Total" = "grey")) +
  geom_text(aes(label = paste0(value, "%")), position = position_dodge(width = 0.9), vjust = -0.5) +
  ylim(0,27) +
  labs(title = "Quebec", y = "Percent", x = "Population")


# visualise through grid arrange 
grid.arrange(excess_alcohol_plot, excess_alcohol_qc_plot, ncol = 2)


# or could visualise by combining data 
#combine data
combined_excess_alcohol <- bind_rows(excess_alcohol,excess_alcohol_qc)

ggplot(data = combined_excess_alcohol, aes(x = name, y = value, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~ Region)+
  labs(title = "Excessive Alcohol Consumption", x = "Population", y = "Percent")






# Consumption Habits - Drug Use -----------------------------------------------

drug_use_Laval <- data.frame(`Region` = c("Laval"),
                             `Women2015` = c(9.9),
                             `Women2021`= c(13.0),
                             `Men2015` = c(17.3),
                             `Men2021` = c(16.1),
                             `Total2015` = c(13.5),
                             `Total2021` = c(14.5))

# Pivot 
drug_use_Laval <- drug_use_Laval|> 
  pivot_longer(cols = -Region, names_to = "Category", values_to = "Value")

# Separate the 'Category' column into 'Gender' and 'Year'
drug_use_Laval <- drug_use_Laval|> 
  mutate(Year = sub(".*([0-9]{4})$", "\\1", Category),
         Gender = sub("([A-Za-z]+).*", "\\1", Category))

# keep columns in correct order
drug_use_Laval$Gender <-
  factor(drug_use_Laval$Gender, levels = unique(drug_use_Laval$Gender))


drug_use_Laval_plot <- 
  ggplot(data = drug_use_Laval, aes(x = Year, y = Value, fill = Gender))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Women" = "pink", "Men" = "blue", "Total" = "grey")) +
  geom_text(aes(label = paste0(Value, "%")), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Laval", x = "Year", y = "Percent") +
  facet_wrap(~ Gender) +
  ylim(0, 23)

# for quebec

drug_use_qc <- data.frame(`Region` = c("Quebec"),
                             `Women2015` = c(12.8),
                             `Women2021`= c(15.8),
                             `Men2015` = c(20.6),
                             `Men2021` = c(22.6),
                             `Total2015` = c(16.7),
                             `Total2021` = c(19.2))

# Pivot 
drug_use_qc <- drug_use_qc|> 
  pivot_longer(cols = -Region, names_to = "Category", values_to = "Value")

# Separate the 'Category' column into 'Gender' and 'Year'
drug_use_qc <- drug_use_qc|> 
  mutate(Year = sub(".*([0-9]{4})$", "\\1", Category),
         Gender = sub("([A-Za-z]+).*", "\\1", Category))

# keep columns in correct order
drug_use_qc$Gender <-
  factor(drug_use_qc$Gender, levels = unique(drug_use_qc$Gender))


drug_use_qc_plot <- 
  ggplot(data = drug_use_qc, aes(x = Year, y = Value, fill = Gender))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Women" = "pink", "Men" = "blue", "Total" = "grey")) +
  geom_text(aes(label = paste0(Value, "%")), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Quebec", x = "Year", y = "Percent") +
  facet_wrap(~ Gender) +
  ylim(0, 23)

grid.arrange(drug_use_Laval_plot, drug_use_qc_plot, ncol = 2)           
