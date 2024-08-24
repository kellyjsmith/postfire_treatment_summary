
library(tidyverse)
library(sf)
library(gridExtra)
library(viridis)
library(scales)
library(ggnewscale)
library(colorspace)
library(grid)
library(ggpubr)


# planted_acres_severity_burned_dual <- function(activity_by_severity, severity_by_year, max_plant_axis = 32000, max_burned_axis = 2.5e6) {
#   # Prepare planted data
#   planted_data <- activity_by_severity %>%
#     st_drop_geometry() %>%
#     filter(type_labels == "Initial Planting") %>%
#     select(Ig_Year, Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres) %>%
#     pivot_longer(cols = -Ig_Year,
#                  names_to = "Severity",
#                  values_to = "Net_Acres") %>%
#     mutate(Severity = case_when(
#       Severity == "Unburned_to_Low_acres" ~ "Unburned to Low",
#       Severity == "Low_acres" ~ "Low",
#       Severity == "Moderate_acres" ~ "Moderate",
#       Severity == "High_acres" ~ "High"
#     )) %>%
#     group_by(Ig_Year, Severity) %>%
#     summarise(Net_Acres = sum(Net_Acres, na.rm = TRUE), .groups = "drop")
#   
#   # Prepare total burned acres data
#   high_severity_data <- severity_by_year %>%
#     select(Ig_Year, High_acres)
#   
#   # Calculate the scaling factor based on the desired maximum for the secondary axis
#   scale_factor <- max_plant_axis / max_burned_axis
#   
#   planted_data$Severity <- factor(
#     planted_data$Severity,
#     levels = c("Unburned to Low", "Low", "Moderate", "High")
#   )
#   
#   ggplot() +
#     geom_col(data = planted_data, 
#              aes(x = Ig_Year, y = Net_Acres, fill = Severity), 
#              position = "stack",
#              color = "black",  # Add black outline
#              size = 0.2) +     # Adjust the thickness of the outline
#     geom_line(data = high_severity_data, 
#               aes(x = Ig_Year, y = High_acres * scale_factor, color = "High Severity Acres Burned"), 
#               alpha = 0.6, size = 1) +
#     geom_point(data = high_severity_data, 
#                aes(x = Ig_Year, y = High_acres * scale_factor, color = "High Severity Acres Burned"), 
#                alpha = 0.9, shape = 24, size = 4, fill = "gray90") +
#     scale_fill_manual(values = c("Unburned to Low" = "darkgreen", 
#                                  "Low" = "skyblue", 
#                                  "Moderate" = "yellow2", 
#                                  "High" = "red"),
#                       name = "Planted Acres by Severity") +
#     scale_color_manual(values = c("High Severity Acres Burned" = "black"),
#                        name = NULL) +
#     scale_y_continuous(
#       name = "Net Planted Acres",
#       labels = function(x) format(x, big.mark = ",", scientific = FALSE),
#       limits = c(0, max_plant_axis),
#       sec.axis = sec_axis(~./scale_factor, 
#                           name = "High Severity Acres Burned",
#                           labels = function(x) format(x, big.mark = ",", scientific = FALSE))
#     ) +
#     labs(title = "Net Planted Acres by Severity Class and High Severity Acres Burned, R5 2000 - 2022",
#          x = "Ignition Year") +
#     theme_bw(base_size = 10) +
#     theme(
#       legend.position = "bottom",
#       legend.box = "vertical",
#       legend.margin = margin(),
#       legend.title = element_text(face = "bold", size = 10),
#       legend.text = element_text(size = 10),
#       axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
#       axis.text.y = element_text(size = 10),
#       axis.title = element_text(size = 11),
#       plot.title = element_text(size = 11)
#     ) +
#     scale_x_continuous(breaks = seq(min(planted_data$Ig_Year), max(planted_data$Ig_Year), by = 2)) +
#     guides(fill = guide_legend(order = 1),
#            color = guide_legend(order = 2))
# }
# 
# # Generate planted acres plot
# Planted_Burned_dualaxis <- planted_acres_severity_burned_dual(
#   activities_by_severity, 
#   severity_by_year,
#   max_plant_axis = 32000,
#   max_burned_axis = 7e5
# )
# 
# # Save planted acres plot
# ggsave("Planted_Burned_dualaxis.png", Planted_Burned_dualaxis, width = 7, height = 5)


activities_by_severity = activities_by_severity %>%
  filter(Ig_Year < 2022)

planted_acres_severity_burned_facets <- function(activity_by_severity, severity_by_year, max_plant_axis = 32000, max_burned_axis = 2.5e6) {
  # Prepare planted data
  planted_data <- activity_by_severity %>%
    st_drop_geometry() %>%
    filter(type_labels == "Initial Planting") %>%
    select(Ig_Year, Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres) %>%
    pivot_longer(cols = -Ig_Year,
                 names_to = "Severity",
                 values_to = "Net_Acres") %>%
    mutate(Severity = case_when(
      Severity == "Unburned_to_Low_acres" ~ "Unburned to Low",
      Severity == "Low_acres" ~ "Low",
      Severity == "Moderate_acres" ~ "Moderate",
      Severity == "High_acres" ~ "High"
    )) %>%
    group_by(Ig_Year, Severity) %>%
    summarise(Net_Acres = sum(Net_Acres, na.rm = TRUE), .groups = "drop") %>%
    mutate(Type = "Postfire Planted Acres")
  
  # Prepare burned area data
  burned_data <- severity_by_year %>%
    st_drop_geometry() %>%
    select(Ig_Year, Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres) %>%
    pivot_longer(cols = -Ig_Year,
                 names_to = "Severity",
                 values_to = "Acres") %>%
    mutate(Severity = case_when(
      Severity == "Unburned_to_Low_acres" ~ "Unburned to Low",
      Severity == "Low_acres" ~ "Low",
      Severity == "Moderate_acres" ~ "Moderate",
      Severity == "High_acres" ~ "High"
    )) %>%
    group_by(Ig_Year, Severity) %>%
    summarise(Acres = sum(Acres, na.rm = TRUE), .groups = "drop") %>%
    mutate(Type = "Burned Acres",
           Acres = Acres)  # Negate acres for mirroring effect
  
  # Combine datasets
  combined_data <- bind_rows(
    planted_data %>% rename(Acres = Net_Acres),
    burned_data
  )
  
  # Set factor levels for Severity
  severity_levels <- c("Unburned to Low", "Low", "Moderate", "High")
  combined_data$Severity <- factor(combined_data$Severity, levels = severity_levels)
  combined_data$Type <- factor(combined_data$Type, levels = c("Postfire Planted Acres", "Burned Acres"))
  
  # Create the plot
  ggplot(combined_data, aes(x = Ig_Year, y = Acres, fill = Severity)) +
    geom_col(data = subset(combined_data, Type == "Burned Acres"),
              position = "stack", color = "black", size = 0.2) +
    geom_col(data = subset(combined_data, Type == "Postfire Planted Acres"), 
             position = "stack", color = "black", size = 0.2) +
    scale_fill_manual(values = c("Unburned to Low" = "darkgreen", 
                                 "Low" = "skyblue", 
                                 "Moderate" = "yellow2", 
                                 "High" = "red"),
                      name = "Severity Class") +
    facet_wrap(~ Type, nrow = 2, scales = "free") +
    labs(title = "Net Planted and Burned Area by Severity Class",
         subtitle = "USFS Region 5, 2000 - 2021",
         x = "Ignition Year", y = "Acres") +
    theme_bw(base_size = 10) +
    theme(
      strip.text = element_text(face = "bold", size = 11),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.margin = margin(),
      legend.title = element_text(face = "bold", size = 11),
      legend.text = element_text(size = 10),
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(face = "bold", size = 11),
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 12),
      panel.spacing = unit(1, "lines")
    ) +
    scale_x_continuous(breaks = seq(2000, 2021, by = 2)) +
    scale_y_continuous(labels = function(x) scales::comma(abs(x))) +
    guides(fill = guide_legend(nrow = 1))
}

# Generate plot
planted_burned_faceted <- planted_acres_severity_burned_facets(
  activities_by_severity, 
  severity_by_year,
  max_plant_axis = 32000,
  max_burned_axis = 2.5e6
)

# Save plot
ggsave("planted_burned_faceted.png", planted_burned_faceted, width = 7, height = 6)


planted_acres_severity_burned_facets <- function(activity_by_severity, severity_by_year, max_plant_axis = 32000) {
  # Prepare planted data (unchanged)
  planted_data <- activity_by_severity %>%
    st_drop_geometry() %>%
    filter(type_labels == "Initial Planting") %>%
    select(Ig_Year, Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres) %>%
    pivot_longer(cols = -Ig_Year,
                 names_to = "Severity",
                 values_to = "Net_Acres") %>%
    mutate(Severity = case_when(
      Severity == "Unburned_to_Low_acres" ~ "Unburned to Low",
      Severity == "Low_acres" ~ "Low",
      Severity == "Moderate_acres" ~ "Moderate",
      Severity == "High_acres" ~ "High"
    )) %>%
    group_by(Ig_Year, Severity) %>%
    summarise(Net_Acres = sum(Net_Acres, na.rm = TRUE), .groups = "drop") %>%
    mutate(Type = "Net Planted Acres by Severity")
  
  # Prepare burned area data as percentages
  burned_data <- severity_by_year %>%
    st_drop_geometry() %>%
    select(Ig_Year, Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres) %>%
    pivot_longer(cols = -Ig_Year,
                 names_to = "Severity",
                 values_to = "Acres") %>%
    mutate(Severity = case_when(
      Severity == "Unburned_to_Low_acres" ~ "Unburned to Low",
      Severity == "Low_acres" ~ "Low",
      Severity == "Moderate_acres" ~ "Moderate",
      Severity == "High_acres" ~ "High"
    )) %>%
    group_by(Ig_Year) %>%
    mutate(Total_Acres = sum(Acres, na.rm = TRUE),
           Percentage = Acres / Total_Acres * 100) %>%
    ungroup() %>%
    mutate(Type = "Burned Area Percentage by Severity")
  
  # Combine datasets
  combined_data <- bind_rows(
    planted_data %>% rename(Value = Net_Acres) %>% mutate(Percentage = NA),
    burned_data %>% rename(Value = Acres) %>% select(-Total_Acres)
  )
  
  # Set factor levels for Severity
  severity_levels <- c("Unburned to Low", "Low", "Moderate", "High")
  combined_data$Severity <- factor(combined_data$Severity, levels = severity_levels)
  combined_data$Type <- factor(combined_data$Type, levels = c("Net Planted Acres by Severity", "Burned Area Percentage by Severity"))
  
  # Create the plot
  ggplot(combined_data, aes(x = Ig_Year, y = ifelse(Type == "Net Planted Acres by Severity", Value, Percentage), fill = Severity)) +
    geom_col(position = "stack", color = "black", size = 0.2) +
    scale_fill_manual(values = c("Unburned to Low" = "darkgreen", 
                                 "Low" = "skyblue", 
                                 "Moderate" = "yellow2", 
                                 "High" = "red"),
                      name = "Severity Class") +
    facet_wrap(~ Type, nrow = 2, scales = "free_y") +
    labs(title = "Net Planted Acres and Burned Area Percentage by Severity Class",
         subtitle = "USFS Region 5, 2000 - 2021",
         x = "Ignition Year", 
         y = "Acres / Percentage") +
    theme_bw(base_size = 10) +
    theme(
      strip.text = element_text(face = "bold", size = 11),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.margin = margin(),
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 10),
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(face = "bold", size = 11),
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 12),
      panel.spacing = unit(1, "lines")
    ) +
    scale_x_continuous(breaks = seq(2000, 2022, by = 2)) +
    scale_y_continuous(labels = function(x) ifelse(x > 100, scales::comma(x), paste0(round(x, 1), "%"))) +
    guides(fill = guide_legend(nrow = 1))
}

# Generate plot
planted_burned_faceted <- planted_acres_severity_burned_facets(
  activities_by_severity, 
  severity_by_year,
  max_plant_axis = 32000
)

# Save plot
ggsave("planted_burned_faceted_percentage.png", planted_burned_faceted, width = 7, height = 6)



library(patchwork)

planted_acres_severity_burned_facets <- function(activity_by_severity, severity_by_year, max_plant_axis = 32000, max_burned_axis = 2.5e6) {
  # Prepare planted data
  planted_data <- activity_by_severity %>%
    st_drop_geometry() %>%
    filter(type_labels == "Initial Planting") %>%
    select(Ig_Year, Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres) %>%
    pivot_longer(cols = -Ig_Year,
                 names_to = "Severity",
                 values_to = "Net_Acres") %>%
    mutate(Severity = case_when(
      Severity == "Unburned_to_Low_acres" ~ "Unburned to Low",
      Severity == "Low_acres" ~ "Low",
      Severity == "Moderate_acres" ~ "Moderate",
      Severity == "High_acres" ~ "High"
    )) %>%
    group_by(Ig_Year, Severity) %>%
    summarise(Net_Acres = sum(Net_Acres, na.rm = TRUE), .groups = "drop") %>%
    mutate(Type = "Postfire Planted Acres")
  
  # Prepare burned area data
  burned_data <- severity_by_year %>%
    st_drop_geometry() %>%
    select(Ig_Year, Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres) %>%
    pivot_longer(cols = -Ig_Year,
                 names_to = "Severity",
                 values_to = "Acres") %>%
    mutate(Severity = case_when(
      Severity == "Unburned_to_Low_acres" ~ "Unburned to Low",
      Severity == "Low_acres" ~ "Low",
      Severity == "Moderate_acres" ~ "Moderate",
      Severity == "High_acres" ~ "High"
    )) %>%
    group_by(Ig_Year, Severity) %>%
    summarise(Acres = sum(Acres, na.rm = TRUE), .groups = "drop") %>%
    mutate(Type = "Burned Acres")
  
  # Combine datasets
  combined_data <- bind_rows(
    planted_data %>% rename(Acres = Net_Acres),
    burned_data
  )
  
  # Set factor levels for Severity
  severity_levels <- c("Unburned to Low", "Low", "Moderate", "High")
  combined_data$Severity <- factor(combined_data$Severity, levels = severity_levels)
  combined_data$Type <- factor(combined_data$Type, levels = c("Postfire Planted Acres", "Burned Acres"))
  
  # Create the acres plot
  acres_plot <- ggplot(combined_data, aes(x = Ig_Year, y = Acres, fill = Severity)) +
    geom_col(position = "stack", color = "black", size = 0.1) +
    scale_fill_manual(values = c("Unburned to Low" = "darkgreen", 
                                 "Low" = "skyblue", 
                                 "Moderate" = "yellow2", 
                                 "High" = "red"),
                      name = "Severity Class") +
    facet_wrap(~ Type, nrow = 2, scales = "free") +
    labs(title = "Area by Severity Class",
         x = "Ignition Year", y = "Acres") +
    theme_bw(base_size = 10) +
    theme(
      strip.text = element_text(face = "bold", size = 10),
      legend.position = "none",
      legend.box = "vertical",
      legend.margin = margin(),
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 9),
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
      axis.text.y = element_text(size = 8),
      # axis.title = element_text(face = "bold", size = 11),
      axis.title = element_blank(),
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      plot.subtitle = element_text(size = 11),
      panel.spacing = unit(1, "lines")
    ) +
    scale_x_continuous(breaks = seq(2000, 2021, by = 4)) +
    scale_y_continuous(labels = function(x) scales::comma(abs(x))) +
    guides(fill = guide_legend(nrow = 1))
  
  # Calculate percentages
  combined_data_pct <- combined_data %>%
    group_by(Type, Ig_Year) %>%
    mutate(Percentage = Acres / sum(Acres) * 100) %>%
    ungroup()
  
  # Create the percentages plot
  percentages_plot <- ggplot(combined_data_pct, aes(x = Ig_Year, y = Percentage, fill = Severity)) +
    geom_col(position = "stack", width = 1, color = "black", size = 0.1) +
    scale_fill_manual(values = c("Unburned to Low" = "darkgreen", 
                                 "Low" = "skyblue", 
                                 "Moderate" = "yellow2", 
                                 "High" = "red"),
                      name = "Severity Class") +
    facet_wrap(~ Type, nrow = 2, scales = "free") +
    labs(title = "% by Severity Class",
         x = "Ignition Year", y = "Percentage") +
    theme_bw(base_size = 10) +
    theme(
      strip.text = element_text(face = "bold", size = 10),
      legend.position = "none",
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
      axis.text.y = element_text(size = 9),
      # axis.title = element_text(face = "bold", size = 11),
      axis.title = element_blank(),
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      panel.spacing = unit(0.5, "lines")
    ) +
    scale_x_continuous(breaks = seq(2000, 2021, by = 4)) +
    scale_y_continuous(labels = function(x) paste0(round(x), "%"))
  
  legend <- get_legend(
    acres_plot + 
      theme(legend.position = "bottom",
            legend.box = "vertical",
            legend.margin = margin(),
            legend.title = element_text(face = "bold", size = 10),
            legend.text = element_text(size = 9)) +
      guides(fill = guide_legend(nrow = 1))
  )
  
  # Combine plots using patchwork
  combined_plot <- (acres_plot + percentages_plot +
    plot_layout(widths = c(1.5, 1))) /
    legend +
    plot_layout(heights = c(20,1)) +
    plot_annotation(
      title = "Distribution of Planted and Total Burned Acres by Severity Class",
      subtitle = "USFS Region 5 Fires - 2000-2021, Net Acres by Ignition Year",
      theme = theme(
        plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(size = 11),
      )
    )
  
  return(combined_plot)
}

# Generate plot
planted_burned_combined <- planted_acres_severity_burned_facets(
  activities_by_severity, 
  severity_by_year,
  max_plant_axis = 32000,
  max_burned_axis = 2.5e6
)

print(planted_burned_combined)

# Save plot
ggsave("planted_burned_combined.png", planted_burned_combined, width = 7, height = 5)



library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

select_treatments_by_severity <- function(data, max_treat_axis = 10000) {
  # Prepare treated data
  treated_data <- data %>%
    st_drop_geometry() %>%
    filter(type_labels %in% c("Fill-in or Replant", "Harvest - Salvage", 
                              "Site Prep - Mechanical", "TSI - Release")) %>%
    select(Ig_Year, type_labels, Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres) %>%
    pivot_longer(cols = c(Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres),
                 names_to = "Severity",
                 values_to = "Net_Acres") %>%
    mutate(Severity = case_when(
      Severity == "Unburned_to_Low_acres" ~ "Unburned to Low",
      Severity == "Low_acres" ~ "Low",
      Severity == "Moderate_acres" ~ "Moderate",
      Severity == "High_acres" ~ "High"
    )) %>%
    group_by(Ig_Year, type_labels, Severity) %>%
    summarise(Net_Acres = sum(Net_Acres, na.rm = TRUE), .groups = "drop")
  
  treated_data$Severity <- factor(treated_data$Severity,
                                  levels = c("Unburned to Low", "Low", "Moderate", "High"))
  
  treated_data$type_labels <- factor(treated_data$type_labels,
                                     levels = c("Harvest - Salvage", "Site Prep - Mechanical",
                                                "Fill-in or Replant", "TSI - Release"))
  
  # Split data into pre-planting and post-planting
  pre_planting <- treated_data %>% 
    filter(type_labels %in% c("Harvest - Salvage", "Site Prep - Mechanical"))
  post_planting <- treated_data %>% 
    filter(type_labels %in% c("Fill-in or Replant", "TSI - Release"))
  
  # Create plot function
  create_pre_plot <- function(data, title) {
    ggplot(data, aes(x = Ig_Year, y = Net_Acres, fill = Severity)) +
      geom_col(position = "stack", color = "black", size = 0.1) +
      facet_wrap(~ type_labels, nrow = 1, scales = "free") +
      scale_fill_manual(values = c("Unburned to Low" = "darkgreen", 
                                   "Low" = "skyblue", 
                                   "Moderate" = "yellow2", 
                                   "High" = "red"),
                        name = "Severity Class") +
      scale_y_continuous(
        name = "Net Acres",
        labels = function(x) format(x, big.mark = ",", scientific = FALSE)
      ) +
      labs(title = title,
           x = "Ignition Year") +
      theme_bw(base_size = 11) +
      theme(
        legend.position = "none",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 9),
        axis.text = element_text(hjust = 0.5, size = 9),
        axis.title.y = element_text(face = "bold", size = 11),
        axis.title.x = element_blank(),
        plot.title = element_text(face = "bold", size = 11),
        strip.text = element_text(size = 10, face = "bold")
      ) +
      scale_x_continuous(breaks = seq(min(data$Ig_Year), max(data$Ig_Year), by = 4))
  }
  
  # Create plot function
  create_post_plot <- function(data, title) {
    ggplot(data, aes(x = Ig_Year, y = Net_Acres, fill = Severity)) +
      geom_col(position = "stack", color = "black", size = 0.2) +
      facet_wrap(~ type_labels, nrow = 1, scales = "free") +
      scale_fill_manual(values = c("Unburned to Low" = "darkgreen", 
                                   "Low" = "skyblue", 
                                   "Moderate" = "yellow2", 
                                   "High" = "red"),
                        name = "Severity Class") +
      scale_y_continuous(
        name = "Net Acres",
        labels = function(x) format(x, big.mark = ",", scientific = FALSE)
      ) +
      labs(title = title,
           x = "Ignition Year") +
      theme_bw(base_size = 11) +
      theme(
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 9),
        axis.text = element_text(hjust = 0.5, size = 9),
        axis.title = element_text(face = "bold", size = 11),
        plot.title = element_text(face = "bold", size = 11),
        strip.text = element_text(size = 10, face = "bold")
      ) +
      scale_x_continuous(breaks = seq(min(data$Ig_Year), max(data$Ig_Year), by = 4))
  }
  # Create individual plots
  pre_plot <- create_pre_plot(pre_planting, "Pre-planting Treatments")
  post_plot <- create_post_plot(post_planting, "Post-planting Treatments")
  
  # Combine plots using patchwork
  combined_plot <- pre_plot + post_plot +
    plot_layout(ncol = 1) +
    plot_annotation(
      title = "Distribution Postfire Treatment Area by Severity Class",
      subtitle = "USFS Region 5 Fires - 2000-2021, Net Acres by Ignition Year",
      theme = theme(plot.title = element_text(face = "bold", size = 11),
                    plot.subtitle = element_text(size = 11))
    )
  
  return(combined_plot)
}

# Generate combined plot
Combined_Treatments_Severity <- select_treatments_by_severity(severity_summary, max_treat_axis = 10000)

print(Combined_Treatments_Severity)

# Save combined plot
ggsave("Combined_Treatments_Severity.png", Combined_Treatments_Severity, width = 7, height = 6)



treatment_acres_by_severity_horizontal <- function(activities_by_severity) {
  # Prepare data
  treatment_data <- activities_by_severity %>%
    st_drop_geometry() %>%
    filter(type_labels %in% c("Initial Planting", "Fill-in or Replant", "TSI - Release", 
                              "Harvest - Salvage", "Site Prep - Mechanical", "Site Prep - Other", 
                              "Site Prep - Chemical", "TSI - Thin")) %>%
    select(type_labels, Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres) %>%
    pivot_longer(cols = c(Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres),
                 names_to = "Severity",
                 values_to = "Acres") %>%
    mutate(Severity = case_when(
      Severity == "Unburned_to_Low_acres" ~ "Unburned to Low",
      Severity == "Low_acres" ~ "Low",
      Severity == "Moderate_acres" ~ "Moderate",
      Severity == "High_acres" ~ "High"
    )) %>%
    group_by(type_labels, Severity) %>%
    summarise(Acres = sum(Acres, na.rm = TRUE), .groups = "drop")
  
  # Set factor levels for proper ordering
  treatment_data$type_labels <- factor(treatment_data$type_labels,
                                       levels = rev(c("Harvest - Salvage", "Site Prep - Mechanical", 
                                                  "Site Prep - Chemical", "Site Prep - Other",
                                                  "Initial Planting", "Fill-in or Replant", "TSI - Release", "TSI - Thin")))
  treatment_data$Severity <- factor(treatment_data$Severity,
                                    levels = c("High", "Moderate", "Low", "Unburned to Low"))
  
  # Create plot
  ggplot(treatment_data, aes(x = Acres, y = type_labels, fill = Severity)) +
    geom_bar(stat = "identity", position = "stack", color = "black", size = 0.1) +
    scale_fill_manual(values = c("High" = "red", 
                                 "Moderate" = "yellow2", 
                                 "Low" = "skyblue", 
                                 "Unburned to Low" = "darkgreen"),
                      name = "Burn Severity Class") +
    scale_x_continuous(labels = scales::comma) +
    labs(title = "Postfire Treatments by Severity Class, R5 2000 - 2021",
         x = "Acres",
         y = "Treatment Type") +
    theme_bw(base_size = 10) +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      legend.title = element_text(face = "bold"),
      axis.text = element_text(size = 10),
      axis.title = element_text(face = "bold", size = 11),
      plot.title = element_text(size = 12, face = "bold")
    )
}

# Generate horizontal bar chart of extended treatment acres by severity
Treatment_Acres_By_Severity <- treatment_acres_by_severity_horizontal(activities_by_severity)

# Save the plot
ggsave("Treatment_Acres_By_Severity.png", Treatment_Acres_By_Severity, width = 7, height = 5)





# pre_planting_treatments_by_severity <- function(activity_by_severity, max_treat_axis = 10000) {
#   # Prepare treated data
#   treated_data <- activity_by_severity %>%
#     st_drop_geometry() %>%
#     filter(type_labels %in% c("Harvest - Salvage", "Site Prep - Mechanical")) %>%
#     select(Ig_Year, type_labels, Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres) %>%
#     pivot_longer(cols = c(Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres),
#                  names_to = "Severity",
#                  values_to = "Net_Acres") %>%
#     mutate(Severity = case_when(
#       Severity == "Unburned_to_Low_acres" ~ "Unburned to Low",
#       Severity == "Low_acres" ~ "Low",
#       Severity == "Moderate_acres" ~ "Moderate",
#       Severity == "High_acres" ~ "High"
#     )) %>%
#     group_by(Ig_Year, type_labels, Severity) %>%
#     summarise(Net_Acres = sum(Net_Acres, na.rm = TRUE), .groups = "drop")
#   
#   treated_data$Severity <- factor(treated_data$Severity,
#                                   levels = c("Unburned to Low", "Low", "Moderate", "High"))
#   
#   treated_data$type_labels <- factor(treated_data$type_labels,
#                                      levels = c("Harvest - Salvage", "Site Prep - Mechanical"))
#   
#   ggplot(treated_data, aes(x = Ig_Year, y = Net_Acres, fill = Severity)) +
#     geom_col(position = "stack", color = "black", size = 0.1) +
#     facet_wrap(~ type_labels, nrow = 2, ncol = 1, scales = "free_y") +
#     scale_fill_manual(values = c("Unburned to Low" = "darkgreen", 
#                                  "Low" = "skyblue", 
#                                  "Moderate" = "yellow2", 
#                                  "High" = "red"),
#                       name = "Severity Class") +
#     scale_y_continuous(
#       name = "Net Treated Acres",
#       labels = function(x) format(x, big.mark = ",", scientific = FALSE)
#     ) +
#     labs(title = "Pre-Planting Treatments by Severity Class, R5 2000 - 2022",
#          x = "Ignition Year") +
#     theme_bw(base_size = 10) +
#     theme(
#       legend.position = "bottom",
#       legend.title = element_text(face = "bold", size = 10),
#       legend.text = element_text(size = 10),
#       axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
#       axis.text.y = element_text(size = 8),
#       axis.title = element_text(size = 11),
#       plot.title = element_text(size = 12),
#       strip.text = element_text(size = 10, face = "bold")
#     ) +
#     scale_x_continuous(breaks = seq(min(treated_data$Ig_Year), max(treated_data$Ig_Year), by = 2))
# }
# 
# post_planting_treatments_by_severity <- function(activity_by_severity, max_treat_axis = 10000) {
#   # Prepare treated data
#   treated_data <- activity_by_severity %>%
#     st_drop_geometry() %>%
#     filter(type_labels %in% c("Fill-in or Replant", "TSI - Release")) %>%
#     select(Ig_Year, type_labels, Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres) %>%
#     pivot_longer(cols = c(Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres),
#                  names_to = "Severity",
#                  values_to = "Net_Acres") %>%
#     mutate(Severity = case_when(
#       Severity == "Unburned_to_Low_acres" ~ "Unburned to Low",
#       Severity == "Low_acres" ~ "Low",
#       Severity == "Moderate_acres" ~ "Moderate",
#       Severity == "High_acres" ~ "High"
#     )) %>%
#     group_by(Ig_Year, type_labels, Severity) %>%
#     summarise(Net_Acres = sum(Net_Acres, na.rm = TRUE), .groups = "drop")
#   
#   treated_data$Severity <- factor(treated_data$Severity,
#                                   levels = c("Unburned to Low", "Low", "Moderate", "High"))
#   
#   treated_data$type_labels <- factor(treated_data$type_labels,
#                                      levels = c("Fill-in or Replant", "TSI - Release"))
#   
#   ggplot(treated_data, aes(x = Ig_Year, y = Net_Acres, fill = Severity)) +
#     geom_col(position = "stack", color = "black", size = 0.2) +
#     facet_wrap(~ type_labels, nrow = 2, ncol = 1, scales = "free_y") +
#     scale_fill_manual(values = c("Unburned to Low" = "darkgreen", 
#                                  "Low" = "skyblue", 
#                                  "Moderate" = "yellow2", 
#                                  "High" = "red"),
#                       name = "Severity Class") +
#     scale_y_continuous(
#       name = "Net Treated Acres",
#       labels = function(x) format(x, big.mark = ",", scientific = FALSE)
#     ) +
#     labs(title = "Post-Planting Treatments by Severity Class, R5 2000 - 2022",
#          x = "Ignition Year") +
#     theme_bw(base_size = 10) +
#     theme(
#       legend.position = "bottom",
#       legend.title = element_text(face = "bold", size = 10),
#       legend.text = element_text(size = 10),
#       axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
#       axis.text.y = element_text(size = 8),
#       axis.title = element_text(size = 11),
#       plot.title = element_text(size = 12),
#       strip.text = element_text(size = 10, face = "bold")
#     ) +
#     scale_x_continuous(breaks = seq(min(treated_data$Ig_Year), max(treated_data$Ig_Year), by = 2))
# }
# 
# # Generate pre-planting treatments plot
# Pre_Planting_Treatments <- pre_planting_treatments_by_severity(
#   activities_by_severity,
#   max_treat_axis = 10000
# )
# 
# # Save pre-planting treatments plot
# ggsave("Pre_Planting_Treatments.png", Pre_Planting_Treatments, width = 7, height = 5)
# 
# # Generate post-planting treatments plot
# Post_Planting_Treatments <- post_planting_treatments_by_severity(
#   activities_by_severity,
#   max_treat_axis = 10000
# )
# 
# # Save post-planting treatments plot
# ggsave("Post_Planting_Treatments.png", Post_Planting_Treatments, width = 7, height = 5)
# 
# 
# 
# library(tidyverse)
# library(scales)
# 
# index_burned_planted <- function(activities_by_severity, fires_by_severity) {
#   # Process planted area data
#   planted_data <- activities_by_severity %>%
#     st_drop_geometry() %>%
#     filter(type_labels == "Initial Planting") %>%
#     rename(year = Ig_Year,
#            high_planted_acres = High_acres) %>%
#     group_by(year) %>%
#     summarize(high_planted_acres = sum(high_planted_acres)) %>%
#     select(year, high_planted_acres)
#   
#   # Process burned area data
#   burned_data <- fires_by_severity %>%
#     st_drop_geometry() %>%
#     rename(year = Ig_Year) %>%
#     group_by(year) %>%
#     summarize(high_burned_acres = sum(High_acres))
#   
#   # Combine the data
#   combined_data <- full_join(planted_data, burned_data, by = "year") %>%
#     arrange(year) %>%
#     filter(!is.na(high_planted_acres) & !is.na(high_burned_acres))
#   
#   # Calculate indexed values (set maximum year to 100)
#   max_planted <- max(combined_data$high_planted_acres, na.rm = TRUE)
#   max_burned <- max(combined_data$high_burned_acres, na.rm = TRUE)
#   
#   combined_data <- combined_data %>%
#     mutate(
#       indexed_planted = high_planted_acres / max_planted * 100,
#       indexed_burned = high_burned_acres / max_burned * 100
#     )
#   
#   # Reshape data from wide to long format
#   data_long <- combined_data %>%
#     select(year, indexed_planted, indexed_burned) %>%
#     pivot_longer(cols = c(indexed_planted, indexed_burned), 
#                  names_to = "category", 
#                  values_to = "value")
#   
#   # Create the plot
#   ggplot(data_long, aes(x = year, y = value, color = category)) +
#     geom_line(size = 1.2) +
#     geom_point(size = 3) +
#     scale_y_continuous(
#       labels = scales::percent_format(scale = 1),
#       name = "Indexed Value (% of Maximum)",
#       expand = expansion(mult = c(0.05, 0.1))
#     ) +
#     scale_x_continuous(breaks = seq(min(combined_data$year), max(combined_data$year), by = 2)) +
#     scale_color_manual(values = c("indexed_planted" = "#82ca9d", "indexed_burned" = "#8884d8"),
#                        labels = c("High Severity Planted Area", "High Severity Burned Area")) +
#     labs(
#       title = "Indexed High Severity Planted vs Burned Areas by Ignition Year",
#       subtitle = paste(min(combined_data$year), "-", max(combined_data$year)),
#       x = "Ignition Year",
#       color = "Category"
#     ) +
#     theme_bw() +
#     theme(
#       plot.title = element_text(hjust = 0.5, face = "bold"),
#       plot.subtitle = element_text(hjust = 0.5),
#       legend.position = "bottom",
#       panel.grid.minor = element_blank()
#     )
# }
# 
# # Usage
# high_planted_burned_indexed <- index_burned_planted(activities_by_severity, fires_by_severity)
# ggsave("high_planted_burned_indexed.png", plot = high_planted_burned_indexed, width = 10, height = 6, dpi = 300)
# 

library(tidyverse)
library(scales)

ratio_burned_planted <- function(activities_by_severity, fires_by_severity) {
  # Process planted area data
  planted_data <- activities_by_severity %>%
    st_drop_geometry() %>%
    filter(type_labels == "Initial Planting") %>%
    rename(year = Ig_Year,
           high_planted_acres = High_acres) %>%
    group_by(year) %>%
    summarize(high_planted_acres = sum(high_planted_acres)) %>%
    select(year, high_planted_acres)
  
  # Process burned area data
  burned_data <- fires_by_severity %>%
    st_drop_geometry() %>%
    rename(year = Ig_Year) %>%
    group_by(year) %>%
    summarize(high_burned_acres = sum(High_acres))
  
  # Combine the data and calculate ratio
  combined_data <- full_join(planted_data, burned_data, by = "year") %>%
    arrange(year) %>%
    filter(!is.na(high_planted_acres) & !is.na(high_burned_acres)) %>%
    mutate(planted_to_burned_ratio = high_planted_acres / high_burned_acres)
  
  # Create the plot
  ggplot(combined_data, aes(x = year, y = planted_to_burned_ratio)) +
    geom_line(size = 1.2, color = "#4CAF50") +
    geom_point(size = 3, color = "#4CAF50") +
    scale_y_continuous(
      labels = scales::percent_format(scale = 100),
      name = "Ratio of Planted to Burned Area",
      expand = expansion(mult = c(0.05, 0.1))
    ) +
    scale_x_continuous(breaks = seq(min(combined_data$year), max(combined_data$year), by = 2)) +
    labs(
      title = "Ratio of High Severity Planted to Burned Areas by Ignition Year",
      subtitle = paste(min(combined_data$year), "-", max(combined_data$year)),
      x = "Ignition Year"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "none",
      panel.grid.minor = element_blank()
    )
}

# Usage
high_planted_burned_ratio <- ratio_burned_planted(activities_by_severity, fires_by_severity)
ggsave("high_planted_burned_ratio.png", plot = high_planted_burned_ratio, width = 10, height = 6, dpi = 300)


ratio_high_burned_planted <- function(activities_by_severity, fires_by_severity) {
  # Process planted area data
  planted_data <- activities_by_severity %>%
    st_drop_geometry() %>%
    filter(type_labels == "Initial Planting") %>%
    rename(year = Ig_Year,
           high_planted_acres = High_acres) %>%
    group_by(year) %>%
    summarize(high_planted_acres = sum(high_planted_acres)) %>%
    select(year, high_planted_acres)
  
  # Process burned area data
  burned_data <- fires_by_severity %>%
    st_drop_geometry() %>%
    rename(year = Ig_Year) %>%
    group_by(year) %>%
    summarize(high_burned_acres = sum(High_acres))
  
  # Combine the data and calculate ratio
  combined_data <- full_join(planted_data, burned_data, by = "year") %>%
    arrange(year) %>%
    filter(!is.na(high_planted_acres) & !is.na(high_burned_acres)) %>%
    mutate(planted_to_burned_ratio = high_planted_acres / high_burned_acres)
  
  # Calculate the scaling factor for the secondary axis
  scale_factor <- max(combined_data$planted_to_burned_ratio) / max(combined_data$high_planted_acres)
  
  # Create the plot
  ggplot(combined_data, aes(x = year)) +
    geom_col(aes(y = high_planted_acres * scale_factor, fill = "Net Planted Acres"), 
             color = "black", linewidth = 0.2, alpha = 1, width = 0.9) +
    geom_line(aes(y = planted_to_burned_ratio, color = "% of High Severity Planted"), 
              size = 1.75, alpha = 0.9) +
    scale_y_continuous(
      name = "Proportion of Total High Severity Acres Planted",
      labels = scales::percent_format(scale = 100),
      sec.axis = sec_axis(~ . / scale_factor, 
                          name = "Postfire Acres Planted in High Severity",
                          labels = scales::comma_format())
    ) +
    scale_x_continuous(breaks = seq(min(combined_data$year), max(combined_data$year), by = 2)) +
    labs(
      title = "Ratio of High Severity Planted to Burned Areas\nand Planted Area by Ignition Year",
      subtitle = paste(min(combined_data$year), "-", max(combined_data$year)),
      x = "Ignition Year"
    ) +
    scale_fill_manual(values = c("Net Planted Acres" = "forestgreen"), name = NULL) +
    scale_color_manual(values = c("% of High Severity Planted" = "red2"), name = NULL) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      axis.title = element_text(face="bold", size = 11),
      axis.text = element_text(size = 10),
      axis.title.y.left = element_text(face = "bold"),
      axis.title.y.right = element_text(face = "bold"),
      legend.position = "bottom",
      legend.box = "horizontal"
    ) +
    guides(fill = guide_legend(order = 2),
           color = guide_legend(order = 1))
}

# Usage
high_planted_burned_ratio_and_area <- ratio_high_burned_planted(activities_by_severity, fires_by_severity)
ggsave("high_planted_burned_ratio_and_area.png", plot = high_planted_burned_ratio_and_area, width = 7, height = 5, dpi = 300)
