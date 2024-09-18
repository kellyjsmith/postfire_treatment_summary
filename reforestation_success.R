library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(knitr)
library(viridis)
library(forcats)
library(scales)

setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output")

# Reforestation success metric

planted_veg_severity_eco <- readRDS("planted_veg_severity_eco_summary_with_reburns.RDS")
planted_veg_severity_eco_SUID <- readRDS("planted_veg_severity_eco_SUID.RDS")



# Calculate reforestation success score by fire, severity class, and ecoregion
reforestation_success_eco <- planted_veg_severity_eco %>%
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


reforestation_success_SUID <- planted_veg_severity_eco_SUID %>%
  group_by(US_L3NAME, Ig_Year, fire_id, SUID) %>%
  summarize(
    Total_Acres = sum(Acres),
    Conifer_Acres = sum(Acres[Veg_Type == "Conifer"]),
    Shrub_Acres = sum(Acres[Veg_Type == "Shrubland"]),
    Conifer_Percent = Conifer_Acres / Total_Acres * 100,
    Conifer_Shrub_Ratio = Conifer_Acres / (Shrub_Acres + 0.1),
    Success_Score = (Conifer_Percent + (Conifer_Shrub_Ratio / (1 + Conifer_Shrub_Ratio)) * 100) / 2,
    .groups = "drop"
  )

refor_success_year_summary <- reforestation_success_eco %>% 
  group_by(US_L3NAME, Ig_Year) %>% 
  summarize(
    Mean_Success_Score = mean(Success_Score, na.rm = TRUE),
    Median_Success_Score = median(Success_Score, na.rm = TRUE),
    SD_Success_Score = sd(Success_Score, na.rm = TRUE),
    Total_Acres = sum(Total_Acres),
    Number_of_Fires = n_distinct(fire_id),
    .groups = "drop"
  ) %>%
  arrange(US_L3NAME)

library(stringr)

# Define ecoregion name replacements
ecoregion_names <- c(
  "Klamath Mountains/California High North Coast Range" = "Klamath Mtns & North Coast",
  "Eastern Cascades Slopes and Foothills" = "East Cascades Slopes",
  "Central California Foothills and Coastal Mountains" = "Central Foothills & Coastal Mountains",
  "Southern California Mountains" = "Southern California Mtns"
)

ecoregion_totals <- refor_success_year_summary %>%
  mutate(US_L3NAME = str_replace_all(US_L3NAME, ecoregion_names)) %>%
  group_by(US_L3NAME) %>%
  summarize(Total_Planted_Acres = sum(Total_Acres))

# Get top 5 ecoregions
top_5_ecoregions <- ecoregion_totals %>%
  top_n(5, Total_Planted_Acres) %>%
  pull(US_L3NAME)

# Prepare the data for plotting
success_year_data <- refor_success_year_summary %>%
  mutate(US_L3NAME = str_replace_all(US_L3NAME, ecoregion_names)) %>%
  filter(US_L3NAME %in% top_5_ecoregions) %>%
  group_by(US_L3NAME, Ig_Year) %>%
  summarize(
    Mean_Success_Score = mean(Mean_Success_Score, na.rm = TRUE),
    SD_Success_Score = mean(SD_Success_Score, na.rm = TRUE),
    .groups = "drop"
  )

# Create the plot
ggplot(success_year_data, aes(x = Ig_Year, y = Mean_Success_Score, fill = US_L3NAME)) +
  geom_bar(stat = "identity", position = "dodge", fill = "lightblue", size = 0.1) +
  geom_errorbar(aes(ymin = Mean_Success_Score - SD_Success_Score, 
                    ymax = Mean_Success_Score + SD_Success_Score),
                position = position_dodge(width = 0.9), width = 0.25) +
  facet_wrap(~ US_L3NAME, scales = "free", ncol = 2) +
  # scale_fill_viridis_d(option = "plasma", begin = 0.2, end = 0.9) +
  scale_x_continuous(breaks = seq(2000, 2021, by = 4)) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Mean Reforestation Success Score by Ignition Year and Ecoregion",
       subtitle = "USFS Region 5 | Net Postfire Planted Acres 2001-2022",
       x = "Ignition Year",
       y = "Mean Reforestation Success Score",
       fill = "Ecoregion") +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 12),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 9)
  ) +
  geom_text(
    data = ecoregion_totals %>% filter(US_L3NAME %in% top_5_ecoregions),
    aes(x = Inf, y = Inf, label = paste0(comma(round(Total_Planted_Acres)), " acres")),
    hjust = 1.1, vjust = 1.5, size = 3, fontface = "bold"
  )

ggsave("reforestation_success_by_year_ecoregion.png", width = 7, height = 5, dpi = 300)



refor_success_prod = planted_veg_severity_eco %>%
  group_by(fire_id, US_L3NAME, mean_prod) %>% 
  summarize(
    Total_Acres = sum(Acres),
    Conifer_Acres = sum(Acres[Veg_Type == "Conifer"]),
    Shrub_Acres = sum(Acres[Veg_Type == "Shrubland"]),
    Conifer_Percent = Conifer_Acres / Total_Acres * 100,
    Conifer_Shrub_Ratio = Conifer_Acres / (Shrub_Acres + 0.1),
    Success_Score = (Conifer_Percent + (Conifer_Shrub_Ratio / (1 + Conifer_Shrub_Ratio)) * 100) / 2,
    .groups = "drop"
  )

refor_success_prod_summary <- refor_success_prod %>% 
  group_by(US_L3NAME, mean_prod) %>% 
  summarize(
    Mean_Success_Score = mean(Success_Score, na.rm = TRUE),
    Median_Success_Score = median(Success_Score, na.rm = TRUE),
    SD_Success_Score = sd(Success_Score, na.rm = TRUE),
    Total_Acres = sum(Total_Acres),
    Number_of_Fires = n_distinct(fire_id),
    .groups = "drop"
  ) %>%
  arrange(US_L3NAME, mean_prod)


library(dplyr)
library(ggplot2)
library(viridis)
library(stringr)



# Define ecoregion name replacements
ecoregion_names <- c(
  "Klamath Mountains/California High North Coast Range" = "Klamath Mtns & North Coast",
  "Eastern Cascades Slopes and Foothills" = "East Cascades Slopes",
  "Central California Foothills and Coastal Mountains" = "Central Foothills & Coastal Mountains",
  "Southern California Mountains" = "Southern California Mtns"
)

# Prepare the data
refor_success_prod_summary <- refor_success_prod %>% 
  mutate(
    US_L3NAME = str_replace_all(US_L3NAME, ecoregion_names),
    productivity_class = cut(mean_prod, 
                             breaks = seq(0, ceiling(max(mean_prod, na.rm = TRUE)), by = 1),
                             labels = seq(1, ceiling(max(mean_prod, na.rm = TRUE))),
                             include.lowest = TRUE)
  ) %>%
  group_by(US_L3NAME, productivity_class) %>% 
  summarize(
    Mean_Success_Score = mean(Success_Score, na.rm = TRUE),
    Net_Acres = sum(Total_Acres),
    Number_of_Fires = n_distinct(fire_id),
    .groups = "drop"
  ) %>%
  arrange(US_L3NAME, productivity_class)

# Calculate total acres and mean success score for each ecoregion
ecoregion_summary <- refor_success_prod_summary %>%
  group_by(US_L3NAME) %>%
  summarize(
    Total_Planted_Acres = sum(Net_Acres),
    Overall_Mean_Success = mean(Mean_Success_Score, na.rm = TRUE)
  ) %>%
  arrange(US_L3NAME)  # Sort by overall mean success score

# Get top 5 ecoregions
top_ecoregions <- ecoregion_summary %>%
  top_n(5, Total_Planted_Acres) %>%
  pull(US_L3NAME)

# Filter for top 5 ecoregions and set factor levels for ordering
plot_data <- refor_success_prod_summary %>%
  filter(US_L3NAME %in% top_ecoregions) %>%
  mutate(US_L3NAME = factor(US_L3NAME, levels = ecoregion_summary$US_L3NAME))

# Create the plot
success_prod_plot <- ggplot(plot_data, 
                            aes(x = productivity_class, y = Mean_Success_Score)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "darkblue", size = 0.1) +
  geom_text(aes(label = comma(round(Net_Acres))), 
            vjust = -0.5, size = 2.75) +
  facet_wrap(~ US_L3NAME, scales = "free", ncol = 3) +
  labs(title = "Mean Reforestation Success Score by Productivity Class and Ecoregion",
       subtitle = "USFS R5 | Net Postfire Planted Acres 2001-2022 | Existing Vegetation Type 2023\n
       Numbers above bars represent acres planted",
       x = "Productivity Class", 
       y = "Mean Reforestation Success Score") +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 12),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 9),
    strip.background = element_blank(),
    legend.position = "none"
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  coord_cartesian(clip = "off")  # Prevent clipping of labels

print(success_prod_plot)

ggsave("reforestation_success_by_productivity_ecoregion_ordered_labeled.png", 
       success_prod_plot, width = 7, height = 4, dpi = 300)


st_write(reforestation_success_eco, "r5_postfire_reforstation_success.shp")

# Create summary table
reforestation_success_eco_summary <- reforestation_success_eco %>%
  filter(Severity_Class != c("Non-Processing Area")) %>% 
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



reforestation_success_stats <- reforestation_success_eco %>%
  filter(US_L3NAME %in% top_ecoregions) %>%
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



bar_plot <- reforestation_success_eco_summary %>%
  filter(US_L3NAME %in% top_ecoregions) %>%
  ggplot(aes(x = Severity_Class, y = Mean_Success_Score, fill = Severity_Class)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ US_L3NAME, scales = "free_y") +
  scale_fill_viridis(discrete = TRUE, option = "D", direction = -1) +
  labs(title = "Mean Reforestation Success Score by Severity Class",
       subtitle = "Top 6 Ecoregions by Total Planted Acres",
       x = "Severity Class", y = "Mean Success Score") +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 12),
        legend.position = "none")
print(bar_plot)


library(ggplot2)
library(dplyr)
library(scales)
library(stringr)

# Define ecoregion name replacements
ecoregion_names <- c(
  "Klamath Mountains/California High North Coast Range" = "Klamath Mtns & North Coast",
  "Eastern Cascades Slopes and Foothills" = "East Cascades Slopes",
  "Central California Foothills and Coastal Mountains" = "Central Foothills & Coastal Mountains",
  "Southern California Mountains" = "Southern California Mtns"
)

# Calculate total planted acres for each ecoregion
ecoregion_totals <- reforestation_success_eco %>%
  mutate(US_L3NAME = str_replace_all(US_L3NAME, ecoregion_names)) %>% 
  group_by(US_L3NAME) %>%
  summarize(Total_Planted_Acres = sum(Total_Acres))

# Get top 5 ecoregions
top_ecoregions <- ecoregion_totals %>%
  top_n(5, Total_Planted_Acres) %>%
  pull(US_L3NAME)

# Create boxplot
success_boxplot <- reforestation_success_eco_summary %>%
  mutate(US_L3NAME = str_replace_all(US_L3NAME, ecoregion_names)) %>%
  filter(US_L3NAME %in% top_ecoregions) %>%
  ggplot(aes(x = reorder(US_L3NAME, Total_Acres), y = Success_Score)) +
  geom_boxplot(fill = "forestgreen", color = "black") +
  coord_flip() +
  labs(title = "Postfire Reforestation Success in Planted Areas by Ecoregion | R5 Fires 2000-2021",
       subtitle = "Success Score: 0-100 scale combining conifer percentage and conifer-to-shrub ratio",
       x = "Ecoregion",
       y = "Reforestation Success Score") +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 10),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 9)
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  scale_x_discrete(labels = function(x) paste0(x, "\n(", comma(
    ecoregion_totals$Total_Planted_Acres[match(x, ecoregion_totals$US_L3NAME)]), " planting acres)"))

print(success_boxplot)
ggsave("reforestation_success_by_ecoregion_boxplot.png", success_boxplot, width = 7, height = 3, dpi = 300)

# Define severity order
severity_levels <- c("High","Moderate","Low","Unburned to Low")

# Prepare data for bar plot
bar_plot_data <- reforestation_success_eco_summary %>%
  mutate(US_L3NAME = str_replace_all(US_L3NAME, ecoregion_names)) %>%
  filter(US_L3NAME %in% top_ecoregions,
         Severity_Class %in% severity_levels) %>%
  mutate(Severity_Class = factor(Severity_Class, levels = severity_levels))

# Bar plot
success_bar_plot <- ggplot(bar_plot_data, 
                           aes(x = Severity_Class, y = Mean_Success_Score, fill = Severity_Class)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ US_L3NAME, scales = "free_x", ncol = 3) +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE, end = 0.9, option = "D") +
  labs(title = "Mean Reforestation Success Score by Burn Severity and Ecoregion",
       subtitle = "Top 5 Ecoregions by Total Planted Acres | R5 Fires 2000-2021",
       x = "Burn Severity Class", 
       y = "Mean Reforestation Success Score") +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 12),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 10),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )

print(success_bar_plot)

ggsave("reforestation_success_by_severity_barplot.png", success_bar_plot, width = 7, height = 4, dpi = 300)



ggplot(refor_success_prod, aes(x = factor(mean_prod), y = Mean_Success_Score, fill = factor(US_L3NAME, levels = ecoregion_order))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = mean_success - se_success, ymax = mean_success + se_success), 
                position = position_dodge(width = 0.9), width = 0.25) +
  scale_fill_viridis_d(option = "plasma", name = "Ecoregion") +
  labs(title = "Mean Reforestation Success Score by Productivity Class and Ecoregion",
       subtitle = "USFS Region 5 | Fires 2000-2021 | Net Planted & Replanted Acres 2001-2022",
       x = "Timber Productivity Class",
       y = "Mean Reforestation Success Score") +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9)
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20))

ggsave("reforestation_success_by_productivity_and_ecoregion.png", width = 10, height = 6, dpi = 300)

# Print plots
print(boxplot)
print(bar_plot)

# 3. Scatter plot of success score vs. total acres
scatter_plot <- ggplot(reforestation_success_eco_summary, 
                       aes(x = Total_Acres, y = Mean_Success_Score, color = Severity_Class)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_x_log10(labels = comma_format()) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(title = "Reforestation Success Score vs. Total Acres",
       x = "Total Acres (log scale)", y = "Mean Success Score") +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 12))
print(scatter_plot)


# 4. Time series plot of success scores by ignition year
time_series_plot <- reforestation_success_eco %>%
  filter(US_L3NAME %in% top_ecoregions,
         Severity_Class != c("Non-Processing Area")) %>%
  ggplot(aes(x = Ig_Year, y = Success_Score, color = Severity_Class)) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ US_L3NAME, scales = "free_y") +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(title = "Reforestation Success Score Trends by Ignition Year",
       subtitle = "Postfire Planted Acres in R5 by Severity",
       x = "Ignition Year", y = "Success Score") +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 12))
print(time_series_plot)
