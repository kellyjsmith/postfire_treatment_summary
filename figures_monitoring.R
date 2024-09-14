library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(patchwork)

# Function to create monitoring acres plot by ignition year
create_monitoring_plot <- function(data, type, color_palette, levels) {
  data[[type]] <- factor(data[[type]], levels = levels)
  
  ggplot(data, aes(x = Ig_Year, y = Acres, fill = !!sym(type))) +
    geom_col(position = "stack", color = "black", size = 0.1) +
    facet_wrap(~ type_labels, scales = "free", ncol = 2) +
    scale_fill_manual(values = color_palette) +
    labs(title = paste("Monitoring Acres by", type, "and Ignition Year"),
         subtitle = "USFS Region 5, 2000-2021",
         x = "Ignition Year",
         y = "Acres",
         fill = type) +
    theme_bw(base_size = 10) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          strip.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          axis.text = element_text(size = 9),
          legend.title = element_text(face = "bold"),
          plot.title = element_text(face = "bold", size = 12),
          plot.subtitle = element_text(size = 10)) +
    scale_y_continuous(labels = scales::comma)
}

# Function to create overall summary plot
create_summary_plot <- function(data, type, color_palette, levels) {
  data[[type]] <- factor(data[[type]], levels = levels)
  
  ggplot(data, aes(x = fct_rev(type_labels), y = Percentage, fill = fct_rev(!!sym(type)))) +
    geom_col(position = "stack", color = "black", size = 0.1) +
    scale_fill_manual(values = color_palette) +
    labs(title = paste("Overall Distribution of", type, "in Monitoring Activities"),
         subtitle = "USFS Region 5, 2000-2021",
         x = "Monitoring Activity",
         y = "Percentage",
         fill = type) +
    theme_bw(base_size = 10) +
    theme(legend.position = "right",
          axis.text.y = element_text(hjust = 1),
          axis.title = element_text(face = "bold"),
          axis.text = element_text(size = 9),
          legend.title = element_text(face = "bold"),
          plot.title = element_text(face = "bold", size = 12),
          plot.subtitle = element_text(size = 10)) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format(scale = 1))
}

# Prepare data for severity classes
severity_data <- severity_summary %>%
  filter(type_labels %in% c("Survival Survey", "Stocking Survey", "Certification - Plant", "Reforest. Need - Fire")) %>%
  select(type_labels, Ig_Year, Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres) %>%
  pivot_longer(cols = c(Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres),
               names_to = "Severity",
               values_to = "Acres") %>%
  mutate(Severity = gsub("_acres", "", Severity),
         case_when(Severity == "Unburned_to_Low" ~ "Unburned to Low"))

# Prepare data for vegetation types
veg_data <- veg_summary %>%
  filter(type_labels %in% c("Survival Survey", "Stocking Survey", "Certification - Plant", "Reforest. Need - Fire")) %>%
  select(type_labels, Ig_Year, Conifer_acres, Shrubland_acres, Hardwood_acres, Other_acres) %>%
  pivot_longer(cols = c(Conifer_acres, Shrubland_acres, Hardwood_acres, Other_acres),
               names_to = "Veg_Type",
               values_to = "Acres") %>%
  mutate(Veg_Type = gsub("_acres", "", Veg_Type))

# Color palettes and levels
severity_colors <- c("High" = "red", "Moderate" = "yellow2", "Low" = "skyblue", "Unburned to Low" = "darkgreen")
veg_colors <- c("Conifer" = "springgreen4", "Shrubland" = "goldenrod", "Hardwood" = "purple2", "Other" = "gray70")
severity_levels <- c("High", "Moderate", "Low", "Unburned to Low")
veg_levels <- c("Conifer", "Shrubland", "Hardwood", "Other")

# Create plots by ignition year
severity_plot <- create_monitoring_plot(severity_data, "Severity", severity_colors, severity_levels)
veg_plot <- create_monitoring_plot(veg_data, "Veg_Type", veg_colors, veg_levels)


# Prepare summary data
severity_summary_data <- severity_data %>%
  group_by(type_labels, Severity) %>%
  summarize(Total_Acres = sum(Acres, na.rm = TRUE)) %>%
  group_by(type_labels) %>%
  mutate(Percentage = Total_Acres / sum(Total_Acres))

veg_summary_data <- veg_data %>%
  group_by(type_labels, Veg_Type) %>%
  summarize(Total_Acres = sum(Acres, na.rm = TRUE)) %>%
  group_by(type_labels) %>%
  mutate(Percentage = Total_Acres / sum(Total_Acres))

# Create summary plots
severity_summary_plot <- create_summary_plot(severity_summary_data, "Severity", severity_colors, severity_levels)
veg_summary_plot <- create_summary_plot(veg_summary_data, "Veg_Type", veg_colors, veg_levels)

# Combine plots into lists
severity_plots <- list(main_plot = severity_plot, summary_plot = severity_summary_plot)
veg_plots <- list(main_plot = veg_plot, summary_plot = veg_summary_plot)

print(severity_plots)
print(veg_plots)
# Save plots
ggsave("Monitoring_Acres_by_Severity.png", severity_plots$main_plot, width = 12, height = 8)
ggsave("Monitoring_Acres_by_Veg_Type.png", veg_plots$main_plot, width = 12, height = 8)
ggsave("Monitoring_Summary_by_Severity.png", severity_plots$summary_plot, width = 8, height = 6)
ggsave("Monitoring_Summary_by_Veg_Type.png", veg_plots$summary_plot, width = 8, height = 6)






# Load the required data
net_activities_eco <- readRDS("net_activities_eco.RDS")

library(dplyr)
library(sf)
library(tidyr)
library(purrr)
library(ggplot2)
library(viridis)

activities_in_plantations_eco <- function(net_activities_eco) {
  plant <- net_activities_eco %>%
    filter(type_labels == "Initial Planting")
  
  monitoring <- net_activities_eco %>%
    filter(type_labels %in% c("Stocking Survey"))
  
  planted_by_fire <- plant %>%
    rename(net_total = net_acres) %>%
    ungroup()
  
  monitoring_by_fire <- monitoring %>%
    group_by(fire_id, US_L3NAME) %>%
    summarize(
      monitoring_geom = st_union(geometry),
      net_total = sum(net_acres),
      n_monitoring = n(),
      weighted_mean_diff = weighted.mean(mean_diff, w = net_acres),
      weighted_mean_prod = weighted.mean(mean_prod, w = net_acres),
      max_reburns = max(max_reburns),
      Ig_Year = first(Ig_Year)
    ) %>%
    ungroup()
  
  calculate_intersection <- function(monitoring_group, planted_by_fire) {
    fire <- monitoring_group$fire_id
    eco <- monitoring_group$US_L3NAME
    
    tryCatch({
      planted_area <- planted_by_fire %>% 
        filter(fire_id == fire, US_L3NAME == eco)
      
      if (nrow(planted_area) == 0) {
        return(tibble(
          fire_id = fire,
          US_L3NAME = eco,
          Ig_Year = monitoring_group$Ig_Year,
          net_total_monitoring = monitoring_group$net_total,
          n_monitoring = monitoring_group$n_monitoring,
          intersecting_acres = 0,
          percent_total_planted = 0,
          percent_total_activity = 0,
          mean_diff = monitoring_group$weighted_mean_diff,
          mean_productivity = monitoring_group$weighted_mean_prod,
          reburns = monitoring_group$max_reburns
        ))
      }
      
      intersection <- st_intersection(monitoring_group$monitoring_geom, planted_area$geometry)
      intersecting_acres <- sum(as.numeric(st_area(intersection)) / 4046.86)
      
      planted_total_area <- sum(planted_area$net_total)
      
      percent_total_planted <- (intersecting_acres / planted_total_area) * 100
      percent_total_activity <- (intersecting_acres / monitoring_group$net_total) * 100
      
      tibble(
        fire_id = fire,
        US_L3NAME = eco,
        Ig_Year = monitoring_group$Ig_Year,
        net_total_monitoring = monitoring_group$net_total,
        n_monitoring = monitoring_group$n_monitoring,
        intersecting_acres = intersecting_acres,
        percent_total_planted = percent_total_planted,
        percent_total_activity = percent_total_activity,
        mean_diff = monitoring_group$weighted_mean_diff,
        mean_productivity = monitoring_group$weighted_mean_prod,
        reburns = monitoring_group$max_reburns
      )
    }, error = function(e) {
      warning(paste("Error processing fire_id:", fire, "- Error:", e$message))
      return(NULL)
    })
  }
  
  monitoring_in_plant <- map_dfr(1:nrow(monitoring_by_fire), 
                                 ~calculate_intersection(monitoring_by_fire[.x,], planted_by_fire), 
                                 .progress = TRUE)
  
  combined_monitoring_in_plant <- monitoring_in_plant %>%
    left_join(planted_by_fire %>% 
                group_by(fire_id, US_L3NAME) %>% 
                summarize(net_total_planted = sum(net_total)), 
              by = c("fire_id", "US_L3NAME"))
  
  return(combined_monitoring_in_plant)
}

monitoring_in_plantations_eco <- activities_in_plantations_eco(net_activities_eco)

# Create a summary table
monitoring_summary_by_eco <- monitoring_in_plantations_eco %>%
  group_by(US_L3NAME) %>%
  summarize(
    total_fires = n(),
    total_planted_acres = sum(net_total_planted, na.rm = TRUE),
    total_monitored_acres = sum(intersecting_acres, na.rm = TRUE),
    total_percent_monitored = (total_monitored_acres / total_planted_acres) * 100,
    median_percent_monitored = median(percent_total_planted, na.rm = TRUE),
    mean_percent_monitored = mean(percent_total_planted, na.rm = TRUE),
    min_percent_monitored = min(percent_total_planted, na.rm = TRUE),
    max_percent_monitored = max(percent_total_planted, na.rm = TRUE),
    avg_mean_diff = mean(mean_diff, na.rm = TRUE),
    avg_mean_productivity = mean(mean_productivity, na.rm = TRUE),
    max_reburns = max(reburns, na.rm = TRUE),
    avg_n_monitoring = mean(n_monitoring, na.rm = TRUE)
  ) %>%
  arrange(desc(total_percent_monitored))

# Display the summary table
print(monitoring_summary_by_eco)

# Save the summary table
write.csv(monitoring_summary_by_eco, "monitoring_summary_by_ecoregion.csv", row.names = FALSE)

# Create figures

# 1. Percentage of planted acres monitored by ecoregion

ggplot(monitoring_summary_by_eco, aes(x = reorder(US_L3NAME, total_percent_monitored), y = total_percent_monitored)) +
  geom_bar(stat = "identity", fill = "royalblue") +
  geom_text(aes(label = comma(round(total_monitored_acres))), 
            hjust = 2, 
            color = "black", 
            fontface = "bold", 
            size = 3.5) +
  coord_flip() +
  labs(title = "Percentage of Postfire Plantations with Survival Surveys by Ecoregion",
       subtitle = "USFS R5 | Fires 2001-2021 | Net Planting and Survey 2001-2022
       \nAcres of Survival Survey shown inside bars",
       x = "Ecoregion",
       y = "Percent Monitored") +
  theme_bw(base_size = 10) +
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 12, face = "bold"),
        axis.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 10),
        axis.text.y = element_text(size = 9))

ggsave("percent_monitored_by_ecoregion_with_acres.png", width = 7, height = 3)

# 2. Planted acres with monitoring by ignition year
monitoring_by_year <- monitoring_in_plantations_eco %>%
  group_by(Ig_Year) %>%
  summarize(
    total_planted = sum(net_total_planted, na.rm = TRUE),
    total_monitored = sum(intersecting_acres, na.rm = TRUE)
  )

ggplot(monitoring_by_year, aes(x = Ig_Year)) +
  geom_col(aes(y = total_planted, fill = "Planted")) +
  geom_col(aes(y = total_monitored, fill = "Monitored")) +
  scale_fill_manual(values = c("Planted" = "forestgreen", "Monitored" = "seagreen1")) +
  labs(title = "Planted and Monitored Acres by Ignition Year",
       x = "Ignition Year",
       y = "Acres",
       fill = "Type") +
  theme_minimal()

ggsave("planted_monitored_acres_by_year.png", width = 10, height = 6)


library(ggplot2)
library(dplyr)
library(scales)

# Modify the data preparation to include reburns
monitoring_by_year_reburns <- monitoring_in_plantations_eco %>%
  group_by(Ig_Year, reburns) %>%
  summarize(
    total_planted = sum(net_total_planted, na.rm = TRUE),
    total_monitored = sum(intersecting_acres, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Convert reburns to a factor for better labeling
  mutate(reburn_factor = factor(reburns, levels = 0:max(reburns), 
                                labels = c("No Reburn", paste(1:max(reburns), "Reburn(s)"))))

# Create the faceted plot
ggplot(monitoring_by_year_reburns, aes(x = Ig_Year)) +
  geom_col(aes(y = total_planted, fill = "Planted"), alpha = 0.7) +
  geom_col(aes(y = total_monitored, fill = "Monitored")) +
  facet_wrap(~ reburn_factor, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c("Planted" = "forestgreen", "Monitored" = "seagreen1")) +
  scale_x_continuous(breaks = seq(2000, 2021, by = 4)) +
  scale_y_continuous(labels = comma_format()) +
  labs(title = "Planted and Monitored Acres by Ignition Year and Reburns",
       subtitle = "USFS Region 5, 2000-2021",
       x = "Ignition Year",
       y = "Acres",
       fill = "Type") +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 10),
    panel.spacing = unit(1, "lines")
  )

ggsave("planted_monitored_acres_by_year_and_reburns.png", width = 12, height = 8)



library(ggplot2)
library(dplyr)
library(scales)

# Prepare the data
monitoring_by_productivity <- monitoring_in_plantations_eco %>%
  mutate(productivity_group = cut(mean_productivity, 
                                  breaks = seq(0, ceiling(max(mean_productivity)), by = 1),
                                  labels = seq(1, ceiling(max(mean_productivity))),
                                  include.lowest = TRUE)) %>%
  group_by(productivity_group) %>%
  summarize(
    total_planted = sum(net_total_planted, na.rm = TRUE),
    total_monitored = sum(intersecting_acres, na.rm = TRUE),
    percent_monitored = (total_monitored / total_planted) * 100,
    .groups = "drop"
  ) %>%
  filter(!is.na(productivity_group))  # Remove any NA groups

# Create the plot
ggplot(monitoring_by_productivity, aes(x = productivity_group)) +
  geom_col(aes(y = total_planted, fill = "Initial Planting"), 
           color = "black", size = 0.1, alpha = 0.7) +
  geom_col(aes(y = total_monitored, fill = "Survival Survey"), 
           color = "black", size = 0.1) + 
  scale_fill_manual(values = c("Initial Planting" = "forestgreen", "Survival Survey" = "seagreen1")) +
  scale_y_continuous(labels = comma_format()) +
  labs(title = "Postfire Planting & Survival Survey Area by Productivity Class",
       subtitle = "USFS Region 5 | Fires 2000-2021 | Activities 2001-2022",
       x = "Productivity Class",
       y = "Net Acres",
       fill = "Activity Type") +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold", size = 9),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  # guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip()

ggsave("planted_monitored_acres_by_productivity.png", width = 7, height = 3.5)



# 3. Relationship between monitoring percentage and mean years since fire
ggplot(monitoring_summary_by_eco, aes(x = avg_mean_diff, y = total_monitored_acres, size = total_fires)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(3, 10)) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Monitoring Acres vs. Mean Years Since Fire",
       x = "Mean Years Between Fire and Monitoring",
       y = "Total Planted Acres with Stocking Survey",
       size = "Total Fires",
       color = "Ecoregion") +
  theme_minimal()

ggsave("monitoring_vs_years_since_fire.png", width = 10, height = 8)

# 4. Average number of monitoring activities by ecoregion
ggplot(monitoring_summary_by_eco, aes(x = reorder(US_L3NAME, avg_n_monitoring), y = avg_n_monitoring)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Average Number of Monitoring Activities by Ecoregion",
       x = "Ecoregion",
       y = "Average Number of Monitoring Activities") +
  theme_minimal()

ggsave("avg_monitoring_activities_by_ecoregion.png", width = 10, height = 8)

library(dplyr)
library(ggplot2)
library(viridis)

# Assuming monitoring_in_plantations_eco is already loaded

# 1. Percentage of planted acres monitored over time by ecoregion
monitoring_by_year_eco <- monitoring_in_plantations_eco %>%
  group_by(Ig_Year, US_L3NAME) %>%
  summarize(
    total_planted = sum(net_total_planted, na.rm = TRUE),
    total_monitored = sum(intersecting_acres, na.rm = TRUE),
    percent_monitored = (total_monitored / total_planted) * 100
  ) %>%
  ungroup()

ggplot(monitoring_by_year_eco, aes(x = Ig_Year, y = percent_monitored, color = US_L3NAME)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_viridis(discrete = TRUE, option = "plasma") +
  labs(title = "Percentage of Planted Acres Monitored Over Time by Ecoregion",
       subtitle = "USFS Region 5, 2000-2021",
       x = "Ignition Year",
       y = "Percentage Monitored",
       color = "Ecoregion") +
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
  scale_x_continuous(breaks = seq(2000, 2021, by = 4)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20))

ggsave("percent_monitored_over_time_by_ecoregion.png", width = 10, height = 6)

# 2. Boxplot of percentage monitored by ecoregion
ggplot(monitoring_in_plantations_eco, aes(x = reorder(US_L3NAME, percent_total_planted, FUN = median), y = percent_total_planted)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  labs(title = "Distribution of Percentage Monitored by Ecoregion",
       subtitle = "USFS Region 5, 2000-2021",
       x = "Ecoregion",
       y = "Percentage Monitored") +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10)
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20))

ggsave("percent_monitored_distribution_by_ecoregion.png", width = 10, height = 6)

# 3. Heatmap of percentage monitored by ecoregion and time
ggplot(monitoring_by_year_eco, aes(x = Ig_Year, y = US_L3NAME, fill = percent_monitored)) +
  geom_tile() +
  scale_fill_viridis(option = "plasma", name = "Percentage\nMonitored") +
  labs(title = "Percentage of Planted Acres Monitored by Ecoregion and Year",
       subtitle = "USFS Region 5, 2000-2021",
       x = "Ignition Year",
       y = "Ecoregion") +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9)
  ) +
  scale_x_continuous(breaks = seq(2000, 2021, by = 4))

ggsave("percent_monitored_heatmap_by_ecoregion_and_year.png", width = 12, height = 8)

# 4. Scatter plot of percentage monitored vs. total planted acres by ecoregion
ggplot(monitoring_in_plantations_eco, aes(x = net_total_planted, y = percent_total_planted, color = US_L3NAME)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_viridis(discrete = TRUE, option = "plasma") +
  scale_x_log10(labels = scales::comma_format(big.mark = ",")) +
  labs(title = "Percentage Monitored vs. Total Planted Acres by Ecoregion",
       subtitle = "USFS Region 5, 2000-2021",
       x = "Total Planted Acres (log scale)",
       y = "Percentage Monitored",
       color = "Ecoregion") +
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
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20))

ggsave("percent_monitored_vs_planted_acres_by_ecoregion.png", width = 10, height = 6)