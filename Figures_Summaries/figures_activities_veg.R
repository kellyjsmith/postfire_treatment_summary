setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output")


library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(patchwork)

veg_summary <- readRDS("veg_summary.RDS")
vegetation_activities = readRDS("vegetation_net_activities_summary.RDS")

planted_burned_veg_facets <- function(veg_data, max_plant_axis = 35000, max_burned_axis = 2.6e6) {
  # Prepare planted data
  planted_data <- veg_data %>%
    filter(Category == "All Activities", type_labels == "Initial Planting") %>%
    select(Ig_Year, Conifer_acres, Shrubland_acres, Hardwood_acres, Grassland_acres, Other_acres) %>%
    pivot_longer(cols = c(Conifer_acres, Shrubland_acres, Hardwood_acres, Grassland_acres, Other_acres),
                 names_to = "Veg_Type",
                 values_to = "Acres") %>%
    mutate(Veg_Type = gsub("_acres", "", Veg_Type)) %>%
    group_by(Ig_Year, Veg_Type) %>%
    summarise(Acres = sum(Acres, na.rm = TRUE), .groups = "drop") %>%
    mutate(Type = "Net Planted Acres")
  
  # Prepare burned area data
  burned_data <- veg_data %>%
    filter(Category == "Total Burned") %>%
    select(Ig_Year, Conifer_acres, Shrubland_acres, Hardwood_acres, Grassland_acres, Other_acres) %>%
    pivot_longer(cols = c(Conifer_acres, Shrubland_acres, Hardwood_acres, Grassland_acres, Other_acres),
                 names_to = "Veg_Type",
                 values_to = "Acres") %>%
    mutate(Veg_Type = gsub("_acres", "", Veg_Type)) %>%
    group_by(Ig_Year, Veg_Type) %>%
    summarise(Acres = sum(Acres, na.rm = TRUE), .groups = "drop") %>%
    mutate(Type = "Total Burned Acres in R5")
  
  # Combine datasets
  combined_data <- bind_rows(planted_data, burned_data)
  
  # Set factor levels for Veg_Type
  veg_levels <- c("Conifer", "Shrubland", "Grassland", "Hardwood", "Other")
  combined_data$Veg_Type <- factor(combined_data$Veg_Type, levels = veg_levels)
  combined_data$Type <- factor(combined_data$Type, levels = c("Net Planted Acres", "Total Burned Acres in R5"))
  
  # Color palette
  veg_colors <- c("Conifer" = "springgreen4", 
                  "Shrubland" = "goldenrod",
                  "Grassland" = "yellow",
                  "Hardwood" = "purple2",
                  "Other" = "gray70")
  
  # Create the acres plot
  acres_plot <- ggplot(combined_data, aes(x = Ig_Year, y = Acres, fill = Veg_Type)) +
    geom_col(position = "stack", color = "black", size = 0.2) +
    scale_fill_manual(values = veg_colors, name = "Vegetation Type") +
    facet_wrap(~ Type, nrow = 2, scales = "free") +
    labs(title = "Area by Vegetation Type",
         x = NULL, y = NULL) +
    theme_bw(base_size = 10) +
    theme(
      strip.text = element_text(face = "bold", size = 10),
      legend.position = "none",
      axis.text = element_text(size = 10),
      axis.title = element_blank(),
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      panel.spacing = unit(1, "lines")
    ) +
    scale_x_continuous(breaks = seq(2000, 2021, by = 4)) +
    scale_y_continuous(labels = function(x) scales::comma(abs(x)))
  
  # Calculate percentages
  combined_data_pct <- combined_data %>%
    group_by(Type, Ig_Year) %>%
    mutate(Percentage = Acres / sum(Acres) * 100) %>%
    ungroup()
  
  # Create the percentages plot
  percentages_plot <- ggplot(combined_data_pct, aes(x = Ig_Year, y = Percentage, fill = Veg_Type)) +
    geom_col(position = "stack", width = 1, color = "black", size = 0.1) +
    scale_fill_manual(values = veg_colors, name = "Vegetation Type") +
    facet_wrap(~ Type, nrow = 2, scales = "free") +
    labs(title = "% by Vegetation Type",
         x = NULL, y = NULL) +
    theme_bw(base_size = 10) +
    theme(
      strip.text = element_text(face = "bold", size = 10),
      legend.position = "none",
      axis.text = element_text(size = 10),
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
                      plot_layout(widths = c(1, 1))) /
    legend +
    plot_layout(heights = c(20,1)) +
    plot_annotation(
      title = "Existing Vegetation Type of Postfire Planted and Total Burned Acres",
      subtitle = "USFS Region 5 | Fires 2000-2021 | Planting 2001-2022 | EVT - 2023",
      theme = theme(
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 11),
      )
    )
  
  return(combined_plot)
}

# Generate plot
planted_veg_faceted <- planted_burned_veg_facets(
  veg_summary,
  max_plant_axis = 35000,
  max_burned_axis = 2.6e6
)

print(planted_veg_faceted)

# Save plot
ggsave("planted_veg_faceted.png", planted_veg_faceted, width = 7, height = 5)


# library(ggplot2)
# library(dplyr)
# library(tidyr)
# library(patchwork)
# 
# select_treatments_by_veg_type <- function(data, max_treat_axis = 10000) {
#   # Prepare treated data
#   treated_data <- data %>%
#     st_drop_geometry() %>%
#     mutate()
#     filter(type_labels %in% c("Fill-in or Replant", "Harvest - Salvage", 
#                               "Site Prep - Mechanical", "TSI - Release")) %>%
#     select(Ig_Year, Conifer_acres, Shrubland_acres, Hardwood_acres, Grassland_acres, Other_acres) %>%
#     pivot_longer(cols = c(Conifer_acres, Shrubland_acres, Hardwood_acres, Grassland_acres, Other_acres),
#                  names_to = "Veg_Type",
#                  values_to = "Net_Acres") %>%
#     mutate(Veg_Type = gsub("_acres", "", Veg_Type)) %>%
#     group_by(Ig_Year, type_labels, Veg_Type) %>%
#     summarise(Net_Acres = sum(Net_Acres, na.rm = TRUE), .groups = "drop")
#   
#   treated_data$Veg_Type <- factor(treated_data$Veg_Type,
#                                   levels = c("Conifer", "Shrubland", "Grassland","Hardwood", "Other"))
#   
#   treated_data$type_labels <- factor(treated_data$type_labels,
#                                      levels = c("Harvest - Salvage", "Site Prep - Mechanical",
#                                                 "Fill-in or Replant", "TSI - Release"))
#   
#   # Split data into pre-planting and post-planting
#   pre_planting <- treated_data %>% 
#     filter(type_labels %in% c("Harvest - Salvage", "Site Prep - Mechanical"))
#   post_planting <- treated_data %>% 
#     filter(type_labels %in% c("Fill-in or Replant", "TSI - Release"))
#   
#   # Color palette
#   veg_colors <- c("Conifer" = "springgreen4", 
#                   "Shrubland" = "goldenrod",
#                   "Grassland" = "yellow",
#                   "Hardwood" = "purple2",
#                   "Other" = "gray70")
#   
#   
#   # Create plot function
#   create_plot <- function(data, title, legend_position) {
#     ggplot(data, aes(x = Ig_Year, y = Net_Acres, fill = Veg_Type)) +
#       geom_col(position = "stack", color = "black", size = 0.1) +
#       facet_wrap(~ type_labels, nrow = 1, scales = "free") +
#       scale_fill_manual(values = veg_colors, name = "Vegetation Type") +
#       scale_y_continuous(
#         name = "Net Acres",
#         labels = function(x) format(x, big.mark = ",", scientific = FALSE)
#       ) +
#       labs(title = title,
#            x = "Ignition Year") +
#       theme_bw(base_size = 11) +
#       theme(
#         legend.position = legend_position,
#         legend.title = element_text(face = "bold", size = 10),
#         legend.text = element_text(size = 9),
#         axis.text = element_text(hjust = 0.5, size = 9),
#         axis.title.y = element_text(face = "bold", size = 11),
#         axis.title.x = element_blank(),
#         plot.title = element_text(face = "bold", size = 11),
#         strip.text = element_text(size = 10, face = "bold")
#       ) +
#       scale_x_continuous(breaks = seq(min(data$Ig_Year), max(data$Ig_Year), by = 4))
#   }
#   
#   # Create individual plots
#   pre_plot <- create_plot(pre_planting, "Pre-planting Treatments", "none")
#   post_plot <- create_plot(post_planting, "Post-planting Treatments", "bottom")
#   
#   # Combine plots using patchwork
#   combined_plot <- pre_plot / post_plot +
#     plot_layout(heights = c(1, 1.2)) +
#     plot_annotation(
#       title = "Distribution of Postfire Treatment Area by Vegetation Type",
#       subtitle = "USFS Region 5 Fires - 2000-2021, Net Acres by Ignition Year",
#       theme = theme(plot.title = element_text(face = "bold", size = 11),
#                     plot.subtitle = element_text(size = 11))
#     )
#   
#   return(combined_plot)
# }
# 
# # Generate combined plot
# Combined_Treatments_Veg_Type <- select_treatments_by_veg_type(veg_summary, max_treat_axis = 10000)
# 
# print(Combined_Treatments_Veg_Type)
# 
# # Save combined plot
# ggsave("Combined_Treatments_Veg_Type.png", Combined_Treatments_Veg_Type, width = 7, height = 6)


# library(patchwork)
# 
# treatment_percentages_by_veg_type <- function(veg_summary) {
#   # Prepare data
#   treatment_data <- veg_summary %>%
#     filter(Category %in% c("All Activities", "Total Burned"),
#            type_labels %in% c("Initial Planting", "Fill-in or Replant", "TSI - Release", 
#                               "Harvest - Salvage", "Site Prep - Mechanical", "Site Prep - Other", 
#                               "Site Prep - Chemical", "TSI - Thin") | Category == "Total Burned") %>%
#     select(Category, type_labels, Ig_Year, Conifer_acres_percent, Shrubland_acres_percent, 
#            Hardwood_acres_percent, Other_acres_percent) %>%
#     pivot_longer(cols = c(Conifer_acres_percent, Shrubland_acres_percent, 
#                           Hardwood_acres_percent, Other_acres_percent),
#                  names_to = "Veg_Type",
#                  values_to = "Percentage") %>%
#     mutate(Veg_Type = gsub("_acres_percent", "", Veg_Type))
#   
#   # Calculate overall percentages
#   overall_percentages <- treatment_data %>%
#     group_by(Category, type_labels, Veg_Type) %>%
#     summarize(Overall_Percentage = mean(Percentage, na.rm = TRUE), .groups = "drop")
#   
#   # Set factor levels for proper ordering
#   treatment_types <- c("Initial Planting", "Fill-in or Replant", "TSI - Release", 
#                        "Harvest - Salvage", "Site Prep - Mechanical", 
#                        "Site Prep - Chemical", "Site Prep - Other", "TSI - Thin")
#   veg_levels <- c("Conifer", "Shrubland", "Hardwood", "Other")
#   
#   overall_percentages$type_labels <- factor(overall_percentages$type_labels, levels = c(treatment_types, "Total Burned"))
#   overall_percentages$Veg_Type <- factor(overall_percentages$Veg_Type, levels = veg_levels)
#   
#   # Color palette
#   veg_colors <- c("Conifer" = "springgreen4", 
#                   "Shrubland" = "goldenrod", 
#                   "Hardwood" = "purple2",
#                   "Other" = "gray70")
#   
#   # Create summary plot for treatment types
#   summary_plot_treatments <- ggplot(overall_percentages %>% filter(Category == "All Activities"), 
#                                     aes(x = fct_rev(type_labels), y = Overall_Percentage, fill = fct_rev(Veg_Type))) +
#     geom_col(position = "stack", color = "black", size = 0.1) +
#     scale_fill_manual(values = veg_colors, name = "Vegetation Type") +
#     labs(title = NULL,
#          subtitle = NULL,
#          x = "Treatment Type",
#          y = "Percentage of Total Net Acres") +
#     theme_bw(base_size = 10) +
#     theme(
#       axis.text.x = element_text(hjust = 0.5, angle = 0),
#       axis.title.y = element_text(margin = margin(r = 10)),
#       legend.text = element_text(size = 9),
#       legend.title = element_text(face = "bold", size = 10), 
#       plot.title = element_text(face = "bold", size = 11),
#       plot.subtitle = element_text(size = 10),
#       plot.margin = margin(10, 10, 10, 10),
#       axis.title = element_text(face = "bold", size = 11),
#       axis.text.y = element_text(size = 9),
#       legend.position = "none"
#     ) +
#     scale_y_continuous(labels = scales::percent_format(scale = 1)) +
#     coord_flip()
#   
#   # Create summary plot for total burned area
#   summary_plot_total_burned <- ggplot(overall_percentages %>% filter(Category == "Total Burned"), 
#                                       aes(x = Category, y = Overall_Percentage, fill = fct_rev(Veg_Type))) +
#     geom_col(position = "stack", color = "black", size = 0.1) +
#     scale_fill_manual(values = veg_colors, name = "Vegetation Type") +
#     labs(title = NULL,
#          x = NULL,
#          y = "Percentage of Total Burned Acres") +
#     theme_bw(base_size = 10) +
#     theme(
#       axis.title.y = element_text(margin = margin(r = 10)),
#       legend.text = element_text(size = 9),
#       legend.title = element_text(face = "bold", size = 10), 
#       plot.title = element_text(face = "bold", size = 11),
#       plot.subtitle = element_text(size = 10),
#       plot.margin = margin(10, 10, 10, 10),
#       axis.title = element_text(face = "bold", size = 11),
#       axis.text.y = element_text(size = 9),
#       legend.position = "bottom",
#       legend.justification = "center"
#     ) +
#     scale_y_continuous(labels = scales::percent_format(scale = 1)) +
#     coord_flip() +
#     guides(fill = guide_legend(nrow = 1, reverse = TRUE))
#   
#   # Combine plots using patchwork
#   combined_summary_plot <- summary_plot_treatments / summary_plot_total_burned +
#     plot_layout(heights = c(7, 1)) +
#     plot_annotation(
#       title = "Overall Distribution of Vegetation Types",
#       subtitle = "By Treatment Type and Total Burned Area, USFS R5 Fires - 2000-2021",
#       theme = theme(plot.title = element_text(face = "bold", size = 12),
#                     plot.subtitle = element_text(size = 11))
#     )
#   
#   return(combined_summary_plot)
# }
# 
# # Generate plot
# Treatment_Percentages_By_Veg_Type <- treatment_percentages_by_veg_type(veg_SUID_summary)
# 
# # Save the summary plot
# ggsave("Treatment_Percentages_By_Veg_Type_Summary.png", Treatment_Percentages_By_Veg_Type, width = 7, height = 4)
# 
# 




treatment_percentages_by_vegetation <- function(veg_summary) {
  # Prepare data
  treatment_data <- veg_summary %>%
    st_drop_geometry() %>%
    filter(Total_acres > 0.3) %>%
    filter(Category %in% c("All Activities", "Total Burned"),
           type_labels %in% c("Initial Planting", "Fill-in or Replant", "Release - Chemical", "Release - Non-Chemical", 
                              "Harvest - Salvage", "Harvest - Non-Salvage", "Site Prep - Non-Chemical", 
                              "Site Prep - Chemical", "TSI - Thin") | Category == "Total Burned") %>%
    select(Category, type_labels, Ig_Year, Conifer_acres_percent, Shrubland_acres_percent, 
           Hardwood_acres_percent, Grassland_acres_percent, Other_acres_percent) %>%
    pivot_longer(cols = c(Conifer_acres_percent, Shrubland_acres_percent, 
                          Hardwood_acres_percent, Grassland_acres_percent, Other_acres_percent),
                 names_to = "Veg_Type",
                 values_to = "Percentage") %>%
    mutate(Veg_Type = gsub("_acres_percent", "", Veg_Type))
  
  # Set factor levels for proper ordering
  treatment_types <- c("Initial Planting", "Fill-in or Replant", "TSI - Thin", "Release - Non-Chemical", "Release - Chemical",  
                        "Harvest - Salvage", "Harvest - Non-Salvage", "Site Prep - Non-Chemical", 
                       "Site Prep - Chemical")
  veg_levels <- c("Conifer", "Shrubland", "Hardwood", "Grassland", "Other")
  
  # Create a complete grid of all combinations
  all_combinations <- expand.grid(
    Ig_Year = min(treatment_data$Ig_Year):max(treatment_data$Ig_Year),
    type_labels = treatment_types,
    Veg_Type = veg_levels,
    Category = "All Activities"
  )
  
  # Join the complete grid with the existing data
  complete_data <- all_combinations %>%
    left_join(treatment_data, by = c("Ig_Year", "type_labels", "Veg_Type", "Category")) %>%
    mutate(Percentage = coalesce(Percentage, 0))
  
  # Calculate overall percentages
  overall_percentages <- treatment_data %>%
    filter(Category == "All Activities") %>%
    group_by(Category, type_labels, Veg_Type) %>%
    summarize(Overall_Percentage = mean(Percentage, na.rm = TRUE), .groups = "drop")
  
  # Add total burned data to overall percentages
  total_burned <- treatment_data %>%
    filter(Category == "Total Burned") %>%
    group_by(Veg_Type) %>%
    summarize(Overall_Percentage = mean(Percentage, na.rm = TRUE), .groups = "drop") %>%
    mutate(Category = "Total Burned", type_labels = "Total Burned")
  
  overall_percentages <- bind_rows(overall_percentages, total_burned)
  
  # Set factor levels
  complete_data$type_labels <- factor(complete_data$type_labels, levels = treatment_types)
  complete_data$Veg_Type <- factor(complete_data$Veg_Type, levels = veg_levels)
  
  overall_percentages$type_labels <- factor(overall_percentages$type_labels, levels = c(treatment_types, "Total Burned"))
  overall_percentages$Veg_Type <- factor(overall_percentages$Veg_Type, levels = veg_levels)
  
  # Color palette
  veg_colors <- c("Conifer" = "springgreen4", 
                  "Shrubland" = "goldenrod", 
                  "Hardwood" = "purple2",
                  "Grassland" = "yellow",
                  "Other" = "gray70")
  
  # Create main plot (excluding Total Burned)
  main_plot <- ggplot() +
    geom_col(data = complete_data, 
             aes(x = Ig_Year, y = Percentage, fill = Veg_Type),
             position = "stack", width = 1, color = "black", size = 0.1) +
    facet_wrap(~ type_labels, scales = "free", ncol = 3) +
    scale_fill_manual(values = veg_colors, name = "Vegetation Type") +
    scale_x_continuous(breaks = c(seq(min(complete_data$Ig_Year), max(complete_data$Ig_Year), by = 5), 
                                  max(complete_data$Ig_Year) + 2),
                       labels = c(seq(min(complete_data$Ig_Year), max(complete_data$Ig_Year), by = 5), 
                                  "Overall")) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    labs(title = "Distribution of Existing Vegetation Type in Postfire Activity Areas",
         subtitle = "Percentage of Total Net Acres by Fire Year - USFS Region 5, 2000 - 2021",
         x = "Ignition Year",
         y = "Percentage of Net Acres") +
    theme_bw(base_size = 10) +
    theme(
      axis.text.x = element_text(hjust = 0.5, size = 9),
      strip.text = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 9),
      legend.title = element_text(face = "bold", size = 10), 
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 10),
      plot.margin = margin(10, 10, 10, 10),
      axis.title = element_text(face = "bold", size = 10),
      axis.text.y = element_text(size = 9),
      legend.position = "bottom",
      legend.justification = "center"
    ) +
    guides(fill = guide_legend(nrow = 1))
  
  # Create summary plot for treatment types
  summary_plot_treatments <- ggplot(overall_percentages %>% filter(Category == "All Activities"), 
                                    aes(x = fct_rev(type_labels), y = Overall_Percentage, fill = fct_rev(Veg_Type))) +
    geom_col(position = "stack", color = "black", size = 0.1) +
    scale_fill_manual(values = veg_colors, name = "Vegetation Type") +
    labs(title = NULL,
         subtitle = NULL,
         x = "Treatment Type",
         y = "Percentage of Total Net Acres") +
    theme_bw(base_size = 10) +
    theme(
      axis.text.x = element_text(hjust = 0.5),
      axis.title.y = element_text(margin = margin(r = 10)),
      legend.text = element_text(size = 9),
      legend.title = element_text(face = "bold", size = 10), 
      plot.title = element_text(face = "bold", size = 10),
      plot.subtitle = element_text(size = 10),
      plot.margin = margin(10, 10, 10, 10),
      axis.title = element_text(face = "bold", size = 10),
      axis.text.y = element_text(size = 9),
      legend.position = "none"
    ) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    coord_flip()
  
  # Create summary plot for total burned area
  summary_plot_total_burned <- ggplot(overall_percentages %>% filter(type_labels == "Total Burned"), 
                                      aes(x = type_labels, y = Overall_Percentage, fill = fct_rev(Veg_Type))) +
    geom_col(position = "stack", color = "black", size = 0.1) +
    scale_fill_manual(values = veg_colors, name = "Vegetation Type") +
    labs(title = NULL,
         x = NULL,
         y = "Percentage of Total Burned Acres") +
    theme_bw(base_size = 10) +
    theme(
      axis.title.y = element_text(margin = margin(r = 10)),
      legend.text = element_text(size = 9),
      legend.title = element_text(face = "bold", size = 10), 
      plot.title = element_text(face = "bold", size = 10),
      plot.subtitle = element_text(size = 10),
      plot.margin = margin(10, 10, 10, 10),
      axis.title = element_text(face = "bold", size = 10),
      axis.text.y = element_text(size = 9),
      legend.position = "bottom",
      legend.margin = margin(r = 40)
    ) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    coord_flip() +
    guides(fill = guide_legend(nrow = 1, reverse = TRUE),
           margin = margin(r=10))
  
  # Combine summary plots using patchwork
  combined_summary_plot <- summary_plot_treatments / summary_plot_total_burned +
    plot_layout(heights = c(7, 1)) +
    plot_annotation(
      title = "Overall Distribution of Vegetation Types in Postfire Reforestation Areas",
      subtitle = "By Treatment Type and Total Burned Area, USFS R5 Fires - 2000-2021",
      theme = theme(plot.title = element_text(face = "bold", size = 11),
                    plot.subtitle = element_text(size = 10))
    )
  
  return(list(main_plot = main_plot, combined_summary_plot = combined_summary_plot))
}

# Generate plots
Treatment_Percentages_By_Vegetation <- treatment_percentages_by_vegetation(veg_summary)

# Print plots
print(Treatment_Percentages_By_Vegetation$main_plot)
print(Treatment_Percentages_By_Vegetation$combined_summary_plot)

# Save plots
ggsave("veg_percent_by_year.png", Treatment_Percentages_By_Vegetation$main_plot, width = 7, height = 5)
ggsave("veg_percent_summary.png", Treatment_Percentages_By_Vegetation$combined_summary_plot, width = 7, height = 4)




library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

select_treatments_by_veg_type <- function(data, max_treat_axis = 10000) {
  # Prepare treated data
  treated_data <- data %>%
    filter(Total_acres > 0.3) %>%
    filter(type_labels %in% c("Initial Planting", "Fill-in or Replant", "Release - Chemical", "Release - Non-Chemical", 
                              "TSI - Thin", "Harvest - Salvage", "Harvest - Non-Salvage", 
                              "Site Prep - Chemical", "Site Prep - Non-Chemical")) %>%
    select(Ig_Year, type_labels, Conifer_acres, Shrubland_acres, Hardwood_acres, Grassland_acres, Other_acres) %>%
    pivot_longer(cols = c(Conifer_acres, Shrubland_acres, Hardwood_acres, Grassland_acres, Other_acres),
                 names_to = "Veg_Type",
                 values_to = "Net_Acres") %>%
    mutate(Veg_Type = gsub("_acres", "", Veg_Type)) %>%
    group_by(Ig_Year, type_labels, Veg_Type) %>%
    summarise(Net_Acres = sum(Net_Acres, na.rm = TRUE), .groups = "drop")
  
  # Set factor levels
  treated_data$Veg_Type <- factor(treated_data$Veg_Type,
                                  levels = c("Conifer", "Shrubland", "Hardwood", "Grassland", "Other"))
  
  treated_data$type_labels <- factor(treated_data$type_labels,
                                     levels = c("Initial Planting", "Fill-in or Replant", "Release - Chemical", "Release - Non-Chemical", 
                                                "TSI - Thin", "Harvest - Salvage", "Harvest - Non-Salvage", 
                                                "Site Prep - Chemical", "Site Prep - Non-Chemical"))
  
  # Split data into reforestation/release and harvest/prep
  reforest_release <- treated_data %>% 
    filter(type_labels %in% c("Initial Planting", "Fill-in or Replant", "Release - Chemical", "Release - Non-Chemical", "TSI - Thin"))
  harvest_prep <- treated_data %>% 
    filter(type_labels %in% c("Harvest - Salvage", "Harvest - Non-Salvage", "Site Prep - Chemical", "Site Prep - Non-Chemical"))
  
  # Color palette
  veg_colors <- c("Conifer" = "springgreen4", 
                  "Shrubland" = "goldenrod",
                  "Hardwood" = "purple2",
                  "Grassland" = "yellow",
                  "Other" = "gray70")
  
  # Create plot function
  create_plot <- function(data, title) {
    ggplot(data, aes(x = Ig_Year, y = Net_Acres, fill = Veg_Type)) +
      geom_col(position = "stack", color = "black", size = 0.1) +
      facet_wrap(~ type_labels, ncol = 2, scales = "free_y") +
      scale_fill_manual(values = veg_colors, name = "Vegetation Type") +
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
  reforest_release_plot <- create_plot(reforest_release, "Postfire Reforestation & Release Activities by Vegetation Type")
  harvest_prep_plot <- create_plot(harvest_prep, "Postfire Harvest & Site Prep Activities by Vegetation Type")
  
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
Treatment_Vegetation_Plots <- select_treatments_by_veg_type(veg_summary, max_treat_axis = 10000)

# Print plots
print(Treatment_Vegetation_Plots$reforest_release)
print(Treatment_Vegetation_Plots$harvest_prep)

# Save plots
ggsave("Reforestation_Release_Treatments_Vegetation.png", Treatment_Vegetation_Plots$reforest_release, width = 10, height = 8)
ggsave("Harvest_SitePrep_Treatments_Vegetation.png", Treatment_Vegetation_Plots$harvest_prep, width = 10, height = 6)



library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Prepare the data
veg_by_productivity <- veg_severity_eco %>%
  filter(type_labels == "Initial Planting") %>%
  mutate(productivity_group = cut(weighted_mean_prod, 
                                  breaks = seq(0, ceiling(max(weighted_mean_prod)), by = 1),
                                  labels = seq(1, ceiling(max(weighted_mean_prod))),
                                  include.lowest = TRUE)) %>%
  group_by(productivity_group, Veg_Type) %>%
  summarize(Acres = sum(Acres, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(productivity_group))

# Calculate total acres for the secondary axis
total_by_productivity <- veg_by_productivity %>%
  group_by(productivity_group) %>%
  summarize(Total = sum(Acres),
            Percent_of_Total = Total / sum(Total) * 100)

# Color scheme from figures_vegetation
veg_colors <- c("Conifer" = "springgreen4", 
                "Shrubland" = "goldenrod", 
                "Hardwood" = "purple2",
                "Grassland" = "yellow",
                "Other" = "gray70")

# Create the plot
ggplot(veg_by_productivity, aes(x = productivity_group, y = Acres)) +
  geom_col(aes(fill = Veg_Type), position = "stack") +
  scale_fill_manual(values = veg_colors) +
  scale_y_continuous(labels = comma_format(), 
                     sec.axis = sec_axis(~./1000, name = "Percent of Total Planted Acres", labels = function(x) paste0(x, "%"))) +
  labs(title = "Planted Acres by Vegetation Type and Productivity Class",
       subtitle = "USFS Region 5, 2000-2021 | ",
       x = "Productivity Class",
       y = "Acres",
       fill = "Vegetation Type") +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank(),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9)
  )

ggsave("planted_acres_by_veg_type_and_productivity.png", width = 12, height = 7)