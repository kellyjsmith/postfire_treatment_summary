library(ggplot2)
library(dplyr)
library(broom)

setwd("E:/kelly/output/")

processed_activities <- readRDS("processed_activities.RDS")

# Prepare the data
plot_data <- processed_activities %>%
  filter(type_labels %in% c("Initial Planting", "Fill-in or Replant", "Release - Chemical", "Release - Non-Chemical", 
                            "TSI - Thin", "Harvest - Salvage", "Harvest - Non-Salvage", 
                            "Site Prep - Chemical", "Site Prep - Non-Chemical")) %>%
  filter(Ig_Year > 2005)

# Create the scatter plot with trend lines
trend_plot <- ggplot(plot_data, aes(x = Ig_Year, y = diff_years, color = type_labels)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ type_labels, scales = "free_y", ncol = 3) +
  labs(title = "Trend in Years Between Fire and Treatment Over Time",
       subtitle = "USFS R5 | Fires, 2000-2021 | Activities 2001-2022",
       x = "Year of Activity",
       y = "Years Since Fire") +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 9),
    axis.title = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 11),
    strip.text = element_text(size = 9, face = "bold")
  )

# Print and save the plot
print(trend_plot)
ggsave("Trend_Years_Since_Fire.png", trend_plot, width = 12, height = 8)

# Conduct linear regression for each treatment type
regression_results <- plot_data %>%
  group_by(type_labels) %>%
  do(tidy(lm(diff_years ~ Ig_Year, data = .))) %>%
  filter(term == "Ig_Year") %>%
  mutate(
    significant = ifelse(p.value < 0.05, "Yes", "No"),
    trend = ifelse(estimate > 0, "Increasing", "Decreasing")
  ) %>%
  select(type_labels, estimate, p.value, significant, trend) %>%
  arrange(p.value)

# Print the regression results
print(regression_results)







library(forcats)

treatment_types <- c("Initial Planting", "Fill-in or Replant", 
                     "Release - Chemical", "Release - Non-Chemical", "TSI - Thin",
                     "Harvest - Salvage", "Harvest - Non-Salvage", 
                     "Site Prep - Chemical", "Site Prep - Non-Chemical")


weighted_mean_diff_activity_year <- processed_activities %>%
  filter(type_labels %in% treatment_types) %>%
  mutate(
    fire_id = paste(Incid_Name, Event_ID, sep="_"),
    activity_area = as.numeric(st_area(geometry)) / 4046.86  # Convert to acres
  ) %>%
  group_by(type_labels, year) %>%
  summarize(
    weighted_mean_diff_years = weighted.mean(diff_years, w = activity_area, na.rm = TRUE),
    total_acres = sum(activity_area, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(type_labels = factor(type_labels, levels = treatment_types)) %>%
  st_drop_geometry()

weighted_mean_diff_ig_year <- processed_activities %>%
  st_collection_extract() %>%
  filter(type_labels %in% treatment_types) %>%
  mutate(
    fire_id = paste(Incid_Name, Event_ID, sep="_"),
    activity_area = as.numeric(st_area(geometry)) / 4046.86  # Convert to acres
  ) %>%
  group_by(type_labels, Ig_Year) %>%
  summarize(
    weighted_mean_diff_years = weighted.mean(diff_years, w = activity_area, na.rm = TRUE),
    total_acres = sum(activity_area, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(type_labels = factor(type_labels, levels = treatment_types))

# Create a faceted version grouped by Ig_Year
mean_diff_ig_year_plot <- ggplot(weighted_mean_diff_ig_year, 
                                 aes(x = Ig_Year, y = weighted_mean_diff_years, color = type_labels)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ type_labels, scales = "free", ncol = 3) +
  scale_color_viridis_d(option = "plasma", end = 0.9) +
  labs(title = "Average # of Years Between Fire and Reforestation Activity",
       subtitle = "USFS R5 Fires by Ignition Year, 2000-2021\n
       Means Weighted by Net Area of Activity for Each Fire",
       x = "Ignition Year",
       y = "Mean Years Since Fire") +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 11),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y = element_text(size = 9),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 9)
  ) +
  scale_x_continuous(breaks = seq(2000, 2021, by = 4))

# Print the faceted plot
print(mean_diff_ig_year_plot)

# Save the faceted plot
ggsave("mean_diff_ig_year_plot.png", mean_diff_ig_year_plot, width = 7, height = 5, dpi = 300)


# Create a faceted version for better readability
mean_diff_activity_year_plot <- ggplot(weighted_mean_diff_activity_year, aes(x = year, y = weighted_mean_diff_years, color = type_labels)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ type_labels, scales = "free", ncol = 3) +
  scale_color_viridis_d(option = "plasma", end = 0.9) +
  labs(title = "Mean Years Since Fire by Treatment Type and Year",
       subtitle = "USFS Region 5, 2000-2022",
       x = "Year of Activity",
       y = "Mean Years Since Fire") +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 11),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  ) +
  scale_x_continuous(breaks = seq(2000, 2022, by = 5))

# Print the faceted plot
print(mean_diff_activity_year_plot)

# Save the faceted plot
ggsave("mean_diff_activity_year_plot.png", mean_diff_activity_year_plot, width = 7, height = 6, dpi = 300)




min_ig_year <- min(processed_activities$Ig_Year, na.rm = TRUE)
max_ig_year <- max(processed_activities$Ig_Year, na.rm = TRUE)

# Calculate the weighted means and summary statistics
filtered_weighted_mean_diff <- processed_activities %>%
  filter(Ig_Year >= (min_ig_year + 5) & Ig_Year <= (max_ig_year - 5)) %>%
  group_by(type_labels, fire_id, year) %>%
  summarize(
    weighted_mean_diff = weighted.mean(diff_years, w = as.numeric(st_area(geometry)), na.rm = TRUE),
    n_observations = n(),
    total_acres = sum(as.numeric(st_area(geometry)) / 4046.86, na.rm = TRUE),
    min_diff_years = min(diff_years, na.rm = TRUE),
    max_diff_years = max(diff_years, na.rm = TRUE)
  ) %>%
  ungroup()

# Display the results
print(filtered_weighted_mean_diff)

# Calculate overall statistics for each treatment type
stats_filtered_mean_diff <- filtered_weighted_mean_diff %>%
  group_by(type_labels) %>%
  summarize(
    overall_weighted_mean = weighted.mean(weighted_mean_diff, w = total_acres, na.rm = TRUE),
    total_observations = sum(n_observations),
    total_acres_all_years = sum(total_acres),
    min_diff_years_overall = min(min_diff_years),
    max_diff_years_overall = max(max_diff_years)
  )

# Display the overall statistics
print(overall_stats)

