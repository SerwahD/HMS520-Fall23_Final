library("readr")
library("dplyr")
library("data.table")
library("tidyr")
library(ggplot2)
library(viridis)
library(reshape2)

# Prepare data
csv_loader <- fread
crime_data <- fread("Crime_Data_from_2020_to_Present.csv")

# Remove columns not needed
columns_to_keep <- c("date_reported", "area_name", "reporting_district", "crime_code", "crime_description", "victim_age", "victim_sex")
crime_data <- crime_data %>%
  select(all_of(columns_to_keep))

# Sort by date reported so that dataset is in chronological order
crime_data <- crime_data %>%
  arrange(date_reported)

#Total crime per area name
crime_data <- crime_data %>%
  group_by(area_name) %>%
  mutate(crime_count = n()) %>%
  ungroup()

# Show relationship between crime description, victim age and sex
plot<- ggplot(crime_data, aes(x = victim_age, y = crime_code, fill = victim_sex)) +
  geom_point(position = position_jitter(h = 0.2, w = 0.1), alpha = 0.5) +
  scale_fill_viridis(discrete = TRUE) +  # Using the viridis color palette
  theme_minimal() +
  labs(title = "Relationship between Crime Victim Age and Sex",
       x = "Victim Age",
       y = "Crime Code",
       fill = "Victim Sex")

ggsave("CrimeCode_age_sex.png", plot, width = 10, height = 20)

# Most common crime by area 
# Find the most common crime descriptions by area
most_crime_by_area <- crime_data %>%
  group_by(area_name, crime_description) %>%
  summarise(crime_count = n()) %>%
  top_n(1, crime_count) %>%
  arrange(desc(crime_count))

#graph
plot<- ggplot(most_crime_by_area, aes(x = reorder(area_name, crime_count), y = crime_count, fill = crime_description)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Most Common Crime by Area",
       x = "Area Name",
       y = "Crime Count",
       fill = "Crime Description") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("MostCrime_areax.png", plot, width = 10, height = 20)

#Areas with the least crimes 
least_crime_areas <- crime_data %>%
  group_by(area_name) %>%
  summarise(total_crime = n()) %>%
  top_n(10, total_crime) %>%  # Choose the top 10 areas with the least crime
  arrange(total_crime)

# Create a bar plot for the areas with the least crime
plot_least_crime <- ggplot(least_crime_areas, aes(x = reorder(area_name, total_crime), y = total_crime)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Areas with the Least Crime",
       x = "Area Name",
       y = "Total Crime Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("LeastCrime_areas.png", plot_least_crime, width = 10, height = 6)

# List areas with most crime reported
# Calculate total crime counts by area
total_crime_by_area <- crime_data %>%
  group_by(area_name) %>%
  summarise(total_crime = n(), .groups = "drop") %>%
  arrange(desc(total_crime))

# Display the top areas with the most crime
top_areas_most_crime <- total_crime_by_area %>%
  top_n(10, total_crime)  # Adjust the number as needed

# Print the top areas with the most crime
print(top_areas_most_crime)

##Male and Female affected by crime by area
heatmap_data <- crime_data %>%
  group_by(area_name, crime_code, crime_description, victim_age, victim_sex) %>%
  summarise(total_victims = n()) %>%
  ungroup()

# Create a heatmap
heatmap_plot <- ggplot(heatmap_data, aes(x = crime_code, y = victim_age, fill = total_victims)) +
  geom_tile() +
  scale_fill_viridis() +
  facet_grid(rows = vars(area_name), cols = vars(victim_sex), scales = "free_y") +
  labs(title = "Heatmap of Victims by Crime, Age, Sex, and Area",
       x = "Crime Code",
       y = "Victim Age",
       fill = "Total Victims") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Saving the heatmap plot
ggsave("Victims_Heatmap.png", heatmap_plot, width = 12, height = 8)

#Victims by sex 
total_victims_by_sex <- crime_data %>%
  group_by(victim_sex) %>%
  summarise(total_victims = n(), .groups = "drop")

# Create a bar plot
gender_distribution_plot <- ggplot(total_victims_by_sex, aes(x = victim_sex, y = total_victims, fill = factor(victim_sex))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("F" = "pink", "M" = "blue", "X" = "gray", "H" = "green")) +  # Adjust colors as needed
  labs(title = "Distribution of Crime Victims by Gender",
       x = "Gender",
       y = "Total Victims",
       fill = "Gender") +
  theme_minimal()

# Saving the plot
ggsave("CrimeVictims_Distribution_by_Gender.png", gender_distribution_plot, width = 6, height = 6)

#Victims by age
crime_by_age_plot <- ggplot(crime_data, aes(x = as.numeric(victim_age))) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Crime by Age",
       x = "Victim Age",
       y = "Crime Count") +
  theme_minimal()

# Saving the plot
ggsave("CrimeByAge_Distribution.png", crime_by_age_plot, width = 8, height = 6)