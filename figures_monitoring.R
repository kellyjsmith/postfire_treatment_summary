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
  mutate(Severity = gsub("_acres", "", Severity))

# Prepare data for vegetation types
veg_data <- veg_summary %>%
  filter(type_labels %in% c("Survival Survey", "Stocking Survey", "Certification - Plant", "Reforest. Need - Fire")) %>%
  select(type_labels, Ig_Year, Conifer_acres, Shrubland_acres, Hardwood_acres, Other_acres) %>%
  pivot_longer(cols = c(Conifer_acres, Shrubland_acres, Hardwood_acres, Other_acres),
               names_to = "Veg_Type",
               values_to = "Acres") %>%
  mutate(Veg_Type = gsub("_acres", "", Veg_Type),
         case_when(Severity == "Unburned_to_Low" ~ "Unburned to Low"))

# Color palettes and levels
severity_colors <- c("High" = "red", "Moderate" = "yellow2", "Low" = "skyblue", "Unburned_to_Low" = "darkgreen")
veg_colors <- c("Conifer" = "springgreen4", "Shrubland" = "goldenrod", "Hardwood" = "purple2", "Other" = "gray70")
severity_levels <- c("High", "Moderate", "Low", "Unburned_to_Low")
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

# Save plots
ggsave("Monitoring_Acres_by_Severity.png", severity_plots$main_plot, width = 12, height = 8)
ggsave("Monitoring_Acres_by_Veg_Type.png", veg_plots$main_plot, width = 12, height = 8)
ggsave("Monitoring_Summary_by_Severity.png", severity_plots$summary_plot, width = 8, height = 6)
ggsave("Monitoring_Summary_by_Veg_Type.png", veg_plots$summary_plot, width = 8, height = 6)