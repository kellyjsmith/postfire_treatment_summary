library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(knitr)
library(viridis)


# Load the data
veg_severity_eco <- readRDS("veg_severity_eco_summary.RDS")

# 1. Calculate conifer:shrub ratio
veg_severity_eco_ratio <- veg_severity_eco %>%
  group_by(fire_id, US_L3NAME, Ig_Year) %>%
  summarize(
    Conifer_Acres = sum(Acres[Veg_Type == "Conifer"]),
    Shrub_Acres = sum(Acres[Veg_Type == "Shrubland"]),
    Conifer_Shrub_Ratio = Conifer_Acres / (Shrub_Acres + 0.001),  # Adding 0.001 to avoid division by zero
    Total_Acres = sum(Acres),
    .groups = "drop"
  ) %>%
  mutate(Conifer_Percent = Conifer_Acres / Total_Acres * 100)

# 2a. Summarize percentage of planted areas with >50% conifer cover by severity and ecoregion
conifer_summary_severity_eco <- veg_severity_eco %>%
  group_by(fire_id, US_L3NAME, Severity_Class) %>%
  summarize(
    Total_Acres = sum(Acres),
    Conifer_Acres = sum(Acres[Veg_Type == "Conifer"]),
    .groups = "drop"
  ) %>%
  mutate(Conifer_Percent = Conifer_Acres / Total_Acres * 100) %>%
  group_by(US_L3NAME, Severity_Class) %>%
  summarize(
    Total_Planted_Acres = sum(Total_Acres),
    Acres_Over_50_Conifer = sum(Total_Acres[Conifer_Percent > 50]),
    Percent_Over_50_Conifer = Acres_Over_50_Conifer / Total_Planted_Acres * 100,
    .groups = "drop"
  )

# 2b. Total (all severity classes) for each ecoregion
conifer_summary_eco <- conifer_summary_severity_eco %>%
  group_by(US_L3NAME) %>%
  summarize(
    Total_Planted_Acres = sum(Total_Planted_Acres),
    Acres_Over_50_Conifer = sum(Acres_Over_50_Conifer),
    Percent_Over_50_Conifer = Acres_Over_50_Conifer / Total_Planted_Acres * 100,
    .groups = "drop"
  )

# 2c. By ignition year
conifer_summary_year <- veg_severity_eco %>%
  group_by(fire_id, Ig_Year) %>%
  summarize(
    Total_Acres = sum(Acres),
    Conifer_Acres = sum(Acres[Veg_Type == "Conifer"]),
    .groups = "drop"
  ) %>%
  mutate(Conifer_Percent = Conifer_Acres / Total_Acres * 100) %>%
  group_by(Ig_Year) %>%
  summarize(
    Total_Planted_Acres = sum(Total_Acres),
    Acres_Over_50_Conifer = sum(Total_Acres[Conifer_Percent > 50]),
    Percent_Over_50_Conifer = Acres_Over_50_Conifer / Total_Planted_Acres * 100,
    .groups = "drop"
  )

# Save results
write.csv(conifer_summary_severity_eco, "conifer_summary_severity_eco.csv", row.names = FALSE)
write.csv(conifer_summary_eco, "conifer_summary_eco.csv", row.names = FALSE)
write.csv(conifer_summary_year, "conifer_summary_year.csv", row.names = FALSE)

# Visualize results
ggplot(conifer_summary_eco, aes(x = reorder(US_L3NAME, -Percent_Over_50_Conifer), y = Percent_Over_50_Conifer)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  labs(title = "Percentage of Planted Areas with >50% Conifer Cover by Ecoregion",
       x = "Ecoregion", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  coord_flip()

ggsave("conifer_cover_by_ecoregion.png", width = 10, height = 6)

ggplot(conifer_summary_year, aes(x = Ig_Year, y = Percent_Over_50_Conifer)) +
  geom_line() +
  geom_point() +
  labs(title = "Percentage of Planted Areas with >50% Conifer Cover by Ignition Year",
       x = "Ignition Year", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

ggsave("conifer_cover_by_year.png", width = 8, height = 6)






# Load the data if not already in environment
veg_severity_eco <- readRDS("veg_severity_eco_summary.RDS")

# 1. Conifer establishment across burn severity classes

# Calculate average conifer percentage by severity class
conifer_by_severity <- veg_severity_eco %>%
  group_by(fire_id, Severity_Class) %>%
  summarize(
    Total_Acres = sum(Acres),
    Conifer_Acres = sum(Acres[Veg_Type == "Conifer"]),
    Conifer_Percent = Conifer_Acres / Total_Acres * 100,
    .groups = "drop"
  )

# Visualize conifer percentage distribution by severity class
ggplot(conifer_by_severity, aes(x = Severity_Class, y = Conifer_Percent)) +
  geom_boxplot(fill = "forestgreen", alpha = 0.7) +
  labs(title = "Distribution of Conifer Percentage by Burn Severity Class",
       x = "Burn Severity Class",
       y = "Conifer Percentage") +
  theme_minimal()

ggsave("conifer_percent_by_severity.png", width = 10, height = 6)


# Conifer:shrub by severity and eco
shrubifer_by_severity_eco <- veg_severity_eco %>%
  group_by(fire_id, US_L3NAME, Severity_Class) %>%
  summarize(
    Total_Acres = sum(Acres),
    Conifer_Acres = sum(Acres[Veg_Type == "Conifer"]),
    Conifer_Percent = Conifer_Acres / Total_Acres * 100,
    Shrub_Acres = sum(Acres[Veg_Type == "Shrubland"]),
    Shrub_Percent = Shrub_Acres / Total_Acres * 100,
    Ratio = Conifer_Acres / (Shrub_Acres + 0.1),
    .groups = "drop"
  )

# Conifer:shrub ratios

# Calculate conifer:shrub ratio for each fire
conifer_shrub_ratio <- veg_severity_eco %>%
  group_by(fire_id, US_L3NAME, Severity_Class) %>%
  summarize(
    Conifer_Acres = sum(Acres[Veg_Type == "Conifer"]),
    Shrub_Acres = sum(Acres[Veg_Type == "Shrubland"]),
    Ratio = Conifer_Acres / (Shrub_Acres + 0.1),  # Adding 0.1 to avoid division by zero
    .groups = "drop"
  )

# Visualize conifer:shrub ratio distribution by ecoregion
ggplot(conifer_shrub_ratio, aes(x = US_L3NAME, y = Ratio)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7) +
  coord_flip() +
  labs(title = "Distribution of Conifer:Shrub Ratio by Ecoregion",
       x = "Ecoregion",
       y = "Conifer:Shrub Ratio (log scale)") +
  scale_y_log10() +
  theme_minimal()

ggsave("conifer_shrub_ratio_by_ecoregion.png", width = 12, height = 8)

# 3. Ecoregional variations

# Calculate average conifer percentage by ecoregion and severity class
eco_severity_conifer <- veg_severity_eco %>%
  group_by(US_L3NAME, Severity_Class) %>%
  summarize(
    Total_Acres = sum(Acres),
    Conifer_Acres = sum(Acres[Veg_Type == "Conifer"]),
    Conifer_Percent = Conifer_Acres / Total_Acres * 100,
    .groups = "drop"
  )

# Create a heatmap of conifer percentage by ecoregion and severity class
ggplot(eco_severity_conifer, aes(x = Severity_Class, y = US_L3NAME, fill = Conifer_Percent)) +
  geom_tile() +
  scale_fill_viridis(option = "C", name = "Conifer %") +
  labs(title = "Conifer Percentage by Ecoregion and Burn Severity Class",
       x = "Burn Severity Class",
       y = "Ecoregion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

ggsave("conifer_percent_heatmap.png", width = 12, height = 8)

# 4. Temporal trends

# Calculate average conifer percentage by ignition year and severity class
temporal_conifer <- veg_severity_eco %>%
  group_by(Ig_Year, Severity_Class) %>%
  summarize(
    Total_Acres = sum(Acres),
    Conifer_Acres = sum(Acres[Veg_Type == "Conifer"]),
    Conifer_Percent = Conifer_Acres / Total_Acres * 100,
    .groups = "drop"
  )

# Visualize temporal trends in conifer percentage by severity class
ggplot(temporal_conifer, aes(x = Ig_Year, y = Conifer_Percent, color = Severity_Class)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Severity_Class, scales = "free_y", ncol = 2) +
  labs(title = "Temporal Trends in Conifer Percentage by Burn Severity Class",
       x = "Ignition Year",
       y = "Conifer Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

ggsave("temporal_conifer_trends.png", width = 12, height = 8)

# 5. Reforestation success metric

# Recalculate reforestation success score
reforestation_success <- veg_severity_eco %>%
  group_by(fire_id, US_L3NAME, Ig_Year) %>%
  summarize(
    Total_Acres = sum(Acres),
    Conifer_Acres = sum(Acres[Veg_Type == "Conifer"]),
    Shrub_Acres = sum(Acres[Veg_Type == "Shrubland"]),
    Conifer_Percent = Conifer_Acres / Total_Acres * 100,
    Conifer_Shrub_Ratio = Conifer_Acres / (Shrub_Acres + 0.1),
    Success_Score = (Conifer_Percent + (Conifer_Shrub_Ratio / (1 + Conifer_Shrub_Ratio)) * 100) / 2,
    .groups = "drop"
  )

# Visualize the new reforestation success score
ggplot(reforestation_success, aes(x = reorder(US_L3NAME, -Success_Score), y = Success_Score)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.7) +
  coord_flip() +
  labs(title = "Revised Reforestation Success by Ecoregion",
       x = "Ecoregion",
       y = "Reforestation Success Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

ggsave("revised_reforestation_success_by_ecoregion.png", width = 12, height = 8)



# Calculate reforestation success score by fire, severity class, and ecoregion
reforestation_success_eco <- veg_severity_eco %>%
  group_by(fire_id, US_L3NAME, Ig_Year, Severity_Class) %>%
  summarize(
    Total_Acres = sum(Acres),
    Conifer_Acres = sum(Acres[Veg_Type == "Conifer"]),
    Shrub_Acres = sum(Acres[Veg_Type == "Shrubland"]),
    Conifer_Percent = Conifer_Acres / Total_Acres * 100,
    Conifer_Shrub_Ratio = Conifer_Acres / (Shrub_Acres + 0.1),
    Success_Score = (Conifer_Percent + (Conifer_Shrub_Ratio / (1 + Conifer_Shrub_Ratio)) * 100) / 2,
    .groups = "drop"
  )

# Create summary table
reforestation_success_eco_summary <- reforestation_success_eco %>%
  group_by(US_L3NAME, Severity_Class) %>%
  summarize(
    Mean_Success_Score = mean(Success_Score, na.rm = TRUE),
    Median_Success_Score = median(Success_Score, na.rm = TRUE),
    SD_Success_Score = sd(Success_Score, na.rm = TRUE),
    Total_Acres = sum(Total_Acres),
    Number_of_Fires = n_distinct(fire_id),
    .groups = "drop"
  ) %>%
  arrange(US_L3NAME, Severity_Class)

# Print the table
print(kable(reforestation_success_eco_summary, digits = 2))

# Save the table as a CSV file
write.csv(reforestation_success_eco_summary, "reforestation_success_summary.csv", row.names = FALSE)

# Create a more detailed table with top 6 ecoregions by total acres
top_6_ecoregions <- reforestation_success_eco_summary %>%
  group_by(US_L3NAME) %>%
  summarize(Total_Acres = sum(Total_Acres)) %>%
  top_n(6, Total_Acres) %>%
  pull(US_L3NAME)

reforestation_success_stats <- reforestation_success_eco %>%
  filter(US_L3NAME %in% top_6_ecoregions) %>%
  group_by(US_L3NAME, Severity_Class) %>%
  summarize(
    Mean_Success_Score = mean(Success_Score, na.rm = TRUE),
    Median_Success_Score = median(Success_Score, na.rm = TRUE),
    SD_Success_Score = sd(Success_Score, na.rm = TRUE),
    Total_Acres = sum(Total_Acres),
    Number_of_Fires = n_distinct(fire_id),
    Min_Success_Score = min(Success_Score, na.rm = TRUE),
    Max_Success_Score = max(Success_Score, na.rm = TRUE),
    Q1_Success_Score = quantile(Success_Score, 0.25, na.rm = TRUE),
    Q3_Success_Score = quantile(Success_Score, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(US_L3NAME, Severity_Class)

# Print the detailed table
print(kable(reforestation_success_stats, digits = 2))

# Save the detailed table as a CSV file
write.csv(reforestation_success_stats, "reforestation_success_detailed_summary.csv", row.names = FALSE)