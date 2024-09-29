
library(tidyverse)
library(sf)
library(gridExtra)
library(viridis)
library(scales)
library(ggnewscale)
library(colorspace)
library(grid)
library(ggpubr)
library(patchwork)

setwd("../Output")

severity_summary = readRDS("severity_summary.RDS")



planted_acres_severity_burned_facets <- function(
    severity_summary, max_plant_axis = 32000, max_burned_axis = 2.5e6) {
  # Prepare planted data
  planted_data <- severity_summary %>%
    st_drop_geometry() %>%
    filter(type_labels == "Initial Planting") %>%
    select(Ig_Year, Increased_Greenness_acres, Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres) %>%
    pivot_longer(cols = c(Increased_Greenness_acres, Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres),
                 names_to = "Severity",
                 values_to = "Net_Acres") %>%
    mutate(Severity = case_when(
      Severity == "Increased_Greenness_acres" ~ "Increased Greenness",
      Severity == "Unburned_to_Low_acres" ~ "Unburned to Low",
      Severity == "Low_acres" ~ "Low",
      Severity == "Moderate_acres" ~ "Moderate",
      Severity == "High_acres" ~ "High"
    )) %>%
    group_by(Ig_Year, Severity) %>%
    summarise(Net_Acres = sum(Net_Acres, na.rm = TRUE), .groups = "drop") %>%
    mutate(Type = "Net Initial Planting Acres")
  
  # Prepare burned area data
  burned_data <- severity_summary %>%
    # select(Ig_Year, Severity) %>%
    # st_drop_geometry() %>%
    # filter(Severity != "Non-Processing Area") %>%
    filter(Category == "Total Burned") %>%
    select(Ig_Year, Increased_Greenness_acres, Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres) %>%
    pivot_longer(cols = c(Increased_Greenness_acres, Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres),
                 names_to = "Severity",
                 values_to = "Acres") %>%
    mutate(Severity = case_when(
      Severity == "Increased_Greenness_acres" ~ "Increased Greenness",
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
  severity_levels <- c("Increased Greenness", "Unburned to Low", "Low", "Moderate", "High")
  combined_data$Severity <- factor(combined_data$Severity, levels = severity_levels)
  combined_data$Type <- factor(combined_data$Type, levels = c("Net Initial Planting Acres", "Burned Acres"))
  
  # Create the acres plot
  acres_plot <- ggplot(combined_data, aes(x = Ig_Year, y = Acres, fill = Severity)) +
    geom_col(position = "stack", color = "black", size = 0.1) +
    scale_fill_manual(values = c("Increased Greenness" = "green",
                                 "Unburned to Low" = "darkgreen", 
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
      axis.text.y = element_text(size = 9),
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
    scale_fill_manual(values = c("Increased Greenness" = "green",
                                 "Unburned to Low" = "darkgreen", 
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
      guides(fill = guide_legend(nrow = 1, reverse = TRUE))
  )
  
  # Combine plots using patchwork
  combined_plot <- (acres_plot + percentages_plot +
                      plot_layout(widths = c(1.5, 1))) /
    legend +
    plot_layout(heights = c(20,1)) +
    plot_annotation(
      title = "Postfire Planting and Total Burned Acres by Severity Class",
      subtitle = "USFS Region 5 | Fires - 2000-2021 | Planting by Fire Year 2001-2022",
      theme = theme(
        plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 11),
      )
    )
  
  return(combined_plot)
}

# Generate plot
planted_burned_combined <- planted_acres_severity_burned_facets(
  severity_summary,
  max_plant_axis = 32000,
  max_burned_axis = 2.5e6
)

print(planted_burned_combined)

# Save plot
ggsave("planted_burned_combined.png", planted_burned_combined, width = 7, height = 5)



select_treatments_by_severity <- function(data, max_treat_axis = 10000) {
  # Prepare treated data
  treated_data <- data %>%
    st_drop_geometry() %>%
    filter(Total_acres > 0.3) %>%
    filter(type_labels %in% c("Fill-in or Replant", "TSI - Thin",
                              "Release - Chemical", "Release - Non-Chemical",
                              "Harvest - Salvage", "Harvest - Non-Salvage", 
                              "Site Prep - Chemical", "Site Prep - Non-Chemical")) %>%
    select(Ig_Year, type_labels, Increased_Greenness_acres, Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres) %>%
    pivot_longer(cols = c(Increased_Greenness_acres, Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres),
                 names_to = "Severity",
                 values_to = "Net_Acres") %>%
    mutate(Severity = case_when(
      Severity == "Increased_Greenness_acres_percent" ~ "Increased Greenness",
      Severity == "Unburned_to_Low_acres" ~ "Unburned to Low",
      Severity == "Low_acres" ~ "Low",
      Severity == "Moderate_acres" ~ "Moderate",
      Severity == "High_acres" ~ "High"
    )) %>%
    group_by(Ig_Year, type_labels, Severity) %>%
    summarise(Net_Acres = sum(Net_Acres, na.rm = TRUE), .groups = "drop")
  
  # Define severity levels and treatment types
  severity_levels <- c("Increased Greenness","Unburned to Low", "Low", "Moderate", "High")
  type_labels_order <- c("Fill-in or Replant", 
                         "Release - Chemical", "Release - Non-Chemical", "TSI - Thin",
                         "Harvest - Salvage", "Harvest - Non-Salvage", 
                         "Site Prep - Chemical", "Site Prep - Non-Chemical")
  
  # Create a complete grid of all combinations
  all_combinations <- expand.grid(
    Ig_Year = min(treated_data$Ig_Year):max(treated_data$Ig_Year),
    type_labels = type_labels_order,
    Severity = severity_levels
  )
  
  # Join the complete grid with the existing data
  complete_data <- all_combinations %>%
    left_join(treated_data, by = c("Ig_Year", "type_labels", "Severity")) %>%
    mutate(Net_Acres = coalesce(Net_Acres, 0))
  
  # Set factor levels
  complete_data$Severity <- factor(complete_data$Severity, levels = severity_levels)
  complete_data$type_labels <- factor(complete_data$type_labels, levels = type_labels_order)
  
  # Split data into two groups
  reforest_release <- complete_data %>%
    filter(type_labels %in% c("Fill-in or Replant", "TSI - Thin",
                              "Release - Chemical", "Release - Non-Chemical"))
  harvest_prep <- complete_data %>% 
    filter(type_labels %in% c("Harvest - Salvage", "Harvest - Non-Salvage", 
                              "Site Prep - Chemical", "Site Prep - Non-Chemical"))
  
  # Create plot function
  create_plot <- function(data, title) {
    ggplot(data, aes(x = Ig_Year, y = Net_Acres, fill = Severity)) +
      geom_col(position = "stack", color = "black", size = 0.1) +
      facet_wrap(~ type_labels, ncol = 2, scales = "free") +
      scale_fill_manual(values = c("Increased Greenness" = "green",
                                   "Unburned to Low" = "darkgreen", 
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
        axis.text.x = element_text(hjust = 0.5, size = 9),
        axis.text.y = element_text(size = 9),
        axis.title = element_text(face = "bold", size = 11),
        plot.title = element_text(face = "bold", size = 12),
        strip.text = element_text(size = 10, face = "bold")
      ) +
      scale_x_continuous(breaks = seq(min(data$Ig_Year), max(data$Ig_Year), by = 4)) +
      guides(fill = guide_legend(reverse = TRUE))
  }
  
  # Create individual plots
  reforest_release_plot <- create_plot(reforest_release, "Postfire Reforestation & Release Activities by Severity Class")
  harvest_prep_plot <- create_plot(harvest_prep, "Postfire Harvest & Site Prep Activities by Severity Class")
  
  # Add subtitles to both plots
  reforest_release_plot <- reforest_release_plot +
    labs(subtitle = "USFS Region 5 Fires - 2000-2021, Net Acres by Ignition Year through 2022")
  
  harvest_prep_plot <- harvest_prep_plot +
    labs(subtitle = "USFS Region 5 Fires - 2000-2021, Net Acres by Ignition Year through 2022")
  
  # Return both plots in a list
  return(list(reforest_release = reforest_release_plot,
              harvest_prep = harvest_prep_plot))
}

# Generate plots
Treatment_Severity_Plots <- select_treatments_by_severity(severity_summary, max_treat_axis = 10000)

# Print plots
print(Treatment_Severity_Plots$reforest_release)
print(Treatment_Severity_Plots$harvest_prep)

# Save plots
ggsave("Reforestation_Release_Treatments_Severity.png", Treatment_Severity_Plots$reforest_release, width = 7, height = 4)
ggsave("Harvest_SitePrep_Treatments_Severity.png", Treatment_Severity_Plots$harvest_prep, width = 7, height = 4)






treatment_percentages_by_severity <- function(severity_summary) {
  # Prepare data
  treatment_data <- severity_summary %>%
    filter(Total_acres > 0.3) %>%
    filter(Category %in% c("All Activities", "Total Burned"),
           type_labels %in% c("Initial Planting", "Fill-in or Replant", "Release - Chemical", "Release - Non-Chemical", 
                              "Harvest - Salvage", "Harvest - Non-Salvage", "Site Prep - Non-Chemical", 
                              "Site Prep - Chemical", "TSI - Thin") | Category == "Total Burned") %>%
    select(Category, type_labels, Ig_Year, Increased_Greenness_acres_percent, Unburned_to_Low_acres_percent, Low_acres_percent, 
           Moderate_acres_percent, High_acres_percent) %>%
    pivot_longer(cols = c(Increased_Greenness_acres_percent, Unburned_to_Low_acres_percent, Low_acres_percent, 
                          Moderate_acres_percent, High_acres_percent),
                 names_to = "Severity",
                 values_to = "Percentage") %>%
    mutate(Severity = case_when(
      Severity == "Increased_Greenness_acres_percent" ~ "Increased Greenness",
      Severity == "Unburned_to_Low_acres_percent" ~ "Unburned to Low",
      Severity == "Low_acres_percent" ~ "Low",
      Severity == "Moderate_acres_percent" ~ "Moderate",
      Severity == "High_acres_percent" ~ "High"
    ))
  
  
  filtered_data <- severity_summary %>%
    filter(Category %in% c("All Activities"),
           type_labels %in% c("Initial Planting", "Fill-in or Replant", "Release - Chemical", "Release - Non-Chemical", 
                              "Harvest - Salvage", "Harvest - Non-Salvage", "Site Prep - Non-Chemical", 
                              "Site Prep - Chemical", "TSI - Thin")) %>%
    select(Category, type_labels, Ig_Year, Increased_Greenness_acres_percent, Unburned_to_Low_acres_percent, Low_acres_percent, 
           Moderate_acres_percent, High_acres_percent) %>%
    pivot_longer(cols = c(Increased_Greenness_acres_percent, Unburned_to_Low_acres_percent, Low_acres_percent, 
                          Moderate_acres_percent, High_acres_percent),
                 names_to = "Severity",
                 values_to = "Percentage") %>%
    mutate(Severity = case_when(
      Severity == "Increased_Greenness_acres_percent" ~ "Increased Greenness",
      Severity == "Unburned_to_Low_acres_percent" ~ "Unburned to Low",
      Severity == "Low_acres_percent" ~ "Low",
      Severity == "Moderate_acres_percent" ~ "Moderate",
      Severity == "High_acres_percent" ~ "High"
    ))
  
  
  # Set factor levels for proper ordering
  treatment_types <- c("Initial Planting", "Fill-in or Replant", "Release - Chemical", "Release - Non-Chemical",  
                       "TSI - Thin", "Harvest - Salvage", "Harvest - Non-Salvage", "Site Prep - Chemical", "Site Prep - Non-Chemical" 
                       )
  # filtered_types <- c("Initial Planting", "Fill-in or Replant", "TSI - Release",
  #                     "Site Prep - Mechanical", "Site Prep - Chemical", "Site Prep - Other",
  #                     "Harvest - Salvage", "Harvest - Non-Salvage", "TSI - Thin")
  severity_levels <- c("High", "Moderate", "Low", "Unburned to Low", "Increased Greenness")
  
  treatment_data$type_labels <- factor(treatment_data$type_labels, levels = treatment_types)
  treatment_data$Severity <- factor(treatment_data$Severity, levels = severity_levels)
  
  # filtered_data$type_labels <- factor(filtered_data$type_labels, levels = filtered_types)
  # filtered_data$Severity <- factor(filtered_data$Severity, levels = severity_levels)
  
  # Calculate overall percentages
  overall_percentages <- treatment_data %>%
    group_by(Category, type_labels, Severity) %>%
    summarize(Overall_Percentage = mean(Percentage, na.rm = TRUE), .groups = "drop")
  
  
  # Color palette
  severity_colors <- c("High" = "red", "Moderate" = "yellow2", "Low" = "skyblue", 
                       "Unburned to Low" = "darkgreen", "Increased Greenness" = "green")
  
  # Create main plot (excluding Total Burned)
  main_plot <- ggplot() +
    geom_col(data = treatment_data %>% filter(Category == "All Activities"), 
             aes(x = Ig_Year, y = Percentage, fill = Severity),
             position = "stack", width = 1, color = "black", size = 0.1) +
    facet_wrap(~ type_labels, scales = "free", ncol = 3) +
    scale_fill_manual(values = severity_colors, name = "Burn Severity Class") +
    scale_x_continuous(breaks = c(seq(min(filtered_data$Ig_Year), max(filtered_data$Ig_Year), by = 5), 
                                  max(treatment_data$Ig_Year) + 2),
                       labels = c(seq(min(filtered_data$Ig_Year), max(filtered_data$Ig_Year), by = 5), 
                                  "Overall")) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    labs(title = "Distribution of Postfire Activity Area by Burn Severity Class",
         subtitle = "Percentage of Total Acres by Ignition Year - USFS Region 5, 2000 - 2021",
         x = "Ignition Year",
         y = "Percentage of Total Acres") +
    theme_bw(base_size = 10) +
    theme(
      axis.text.x = element_text(hjust = 0.5, size = 9),
      strip.text = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      legend.title = element_text(face = "bold", size = 10), 
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 10),
      plot.margin = margin(10, 10, 10, 10),
      axis.title = element_text(face = "bold", size = 11),
      axis.text.y = element_text(size = 9),
      legend.position = "bottom",
      legend.justification = "center"
    ) +
    guides(fill = guide_legend(nrow = 1))
  
  # Create summary plot for treatment types
  summary_plot_treatments <- ggplot(overall_percentages %>% filter(Category == "All Activities"), 
                                    aes(x = fct_rev(type_labels), y = Overall_Percentage, fill = fct_rev(Severity))) +
    geom_col(position = "stack", color = "black", size = 0.1) +
    scale_fill_manual(values = severity_colors, name = "Burn Severity Class") +
    labs(title = NULL,
         subtitle = NULL,
         x = "Treatment Type",
         y = "Percentage of Total Net Acres") +
    theme_bw(base_size = 10) +
    theme(
      legend.text = element_text(size = 9),
      legend.title = element_text(face = "bold", size = 10), 
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 10),
      plot.margin = margin(10, 10, 10, 10),
      axis.title = element_text(face = "bold", size = 11),
      axis.title.y = element_blank(),
      axis.text = element_text(size = 10),
      legend.position = "none"
    ) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    coord_flip()
  
  # Create summary plot for total burned area
  summary_plot_total_burned <- ggplot(overall_percentages %>% filter(Category == "Total Burned"), 
                                      aes(x = Category, y = Overall_Percentage, fill = fct_rev(Severity))) +
    geom_col(position = "stack", color = "black", size = 0.1) +
    scale_fill_manual(values = severity_colors, name = "Burn Severity Class") +
    labs(title = NULL,
         x = NULL,
         y = "Percentage of Total Burned Acres") +
    theme_bw(base_size = 10) +
    theme(
      axis.text = element_text(size = 10),
      axis.title = element_text(face = "bold", size = 11),
      axis.title.y = element_text(margin = margin(r = 10)),
      legend.text = element_text(size = 9),
      legend.title = element_text(face = "bold", size = 10), 
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 10),
      plot.margin = margin(10, 10, 10, 10),
      legend.position = "bottom",
      legend.margin = margin(r = 80)
    ) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    coord_flip() +
    guides(fill = guide_legend(nrow = 1, reverse = TRUE))
  
  # Combine plots using patchwork
  combined_summary_plot <- summary_plot_treatments / summary_plot_total_burned +
    plot_layout(heights = c(7, 1)) +
    plot_annotation(
      title = "Distribution of Postfire Activity & Total Burned Area by Burn Severity",
      subtitle = "USFS R5 Fires - 2000-2021, % of Net Acres through 2022",
      theme = theme(plot.title = element_text(face = "bold", size = 12),
                    plot.subtitle = element_text(size = 11))
    )
  
  return(list(main_plot = main_plot, combined_summary_plot = combined_summary_plot))
}

# Generate plots
Treatment_Percentages_By_Severity <- treatment_percentages_by_severity(severity_summary)

print(Treatment_Percentages_By_Severity)

# Save the main plot
ggsave("Treatment_Percentages_By_Severity_Main.png", Treatment_Percentages_By_Severity$main_plot, width = 12, height = 8)

# Save the summary plot
ggsave("Treatment_Percentages_By_Severity_Summary.png", Treatment_Percentages_By_Severity$combined_summary_plot, width = 7, height = 4)


