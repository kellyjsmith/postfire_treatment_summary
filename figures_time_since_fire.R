

## Tables & Figures for summarizing postfire activities
library(dplyr)
library(ggplot2)
library(sf)
library(colorspace)
library(viridis)
library(forcats)

setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output")

assigned_activities = readRDS("assigned_activities_new.RDS")

veg_summary <- readRDS("veg_summary.RDS")
vegetation_activities = readRDS("vegetation_net_activities_summary.RDS")


#### Acres by Ig Year and Years Since Fire ####

# Prepare the data
treatments_diff <- assigned_activities %>%
  mutate(years_since_fire = diff_years) %>%
  mutate(year_group = case_when(
    years_since_fire <= 1 ~ "0-1",
    years_since_fire <= 2 ~ "1-2",
    years_since_fire <= 3 ~ "2-3",
    years_since_fire <= 4 ~ "3-4",
    years_since_fire <= 5 ~ "4-5",
    TRUE ~ ">5"
  )) %>%
  filter(!is.na(Ig_Year),
         type_labels %in% c("Initial Planting", "Fill-in or Replant", "Release - Chemical", "Release - Non-Chemical", 
                            "Harvest - Salvage", "Harvest - Non-Salvage", "Site Prep - Chemical", "Site Prep - Non-Chemical",
                            "TSI - Thin"))

# Summarize data
summarized_data <- treatments_diff %>%
  group_by(Ig_Year, type_labels, year_group) %>%
  summarize(Acres = sum(as.numeric(st_area(geometry))/4046.86)) %>%
  filter(Acres > 0.3) %>%
  group_by(Ig_Year, type_labels) %>%
  mutate(percentage = Acres / sum(Acres) * 100) %>%
  ungroup() %>%
  st_drop_geometry()



create_treatments_plot <- function(summarized_data) {
  # Create a complete grid of all combinations
  all_combinations <- expand.grid(
    type_labels = unique(summarized_data$type_labels),
    year_group = c("0-1", "1-2", "2-3", "3-4", "4-5", ">5"),
    Ig_Year = unique(summarized_data$Ig_Year)
  )
  
  # Join the complete grid with the existing data
  treatments_complete <- all_combinations %>%
    left_join(summarized_data, by = c("type_labels", "year_group", "Ig_Year")) %>%
    mutate(Acres = coalesce(Acres, 0))  # Replace NA with 0
  
  # Set factor levels
  treatments_complete$type_labels <- factor(treatments_complete$type_labels, 
                                            levels = c("Initial Planting", "Fill-in or Replant", 
                                                       "Release - Chemical", "Release - Non-Chemical", 
                                                       "Harvest - Salvage", "Harvest - Non-Salvage", 
                                                       "Site Prep - Chemical", "Site Prep - Non-Chemical",
                                                       "TSI - Thin"))
  treatments_complete$year_group <- factor(treatments_complete$year_group, 
                                           levels = c("0-1", "1-2", "2-3", "3-4", "4-5", ">5"))
  
  # # Create a custom color palette
  # custom_palette <- diverging_hcl(6, h = c(10, 240), c = 100, l = c(50, 90), power = 0.7)
  # 
  # Create plot
  treatments_plot <- ggplot(treatments_complete, aes(x = Ig_Year, y = Acres, fill = fct_rev(year_group))) +
    geom_col(position = "stack", width = 0.9) +
    facet_wrap(~ type_labels, scales = "free", ncol = 3) +
    scale_fill_viridis_d(option = "plasma", end = 0.9, direction = -1, name = "Years Since Fire") +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = seq(min(treatments_complete$Ig_Year), 
                                    max(treatments_complete$Ig_Year), by = 5)) +
    labs(title = "Distribution of Postfire Activities by Treatment Type and Years Since Fire",
         subtitle = "Net Postfire Activity Acres - USFS Region 5, 2000 - 2021",
         x = "Ignition Year",
         y = "Acres") +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(hjust = 0.5, size = 9),
      axis.text.y = element_text(size = 9),
      axis.title = element_text(size = 10, face = "bold"),
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 10),
      strip.text = element_text(face = "bold", size = 9),
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 9),
    ) +
    guides(fill = guide_legend(nrow = 1, reverse = TRUE))
  
  return(treatments_plot)
}

# Usage
treatments_diffyears <- create_treatments_plot(summarized_data)

# Display the plot
print(treatments_diffyears)

# Save the plot
ggsave("treatments_diffyears.png", treatments_diffyears, width = 7, height = 5)


library(dplyr)
library(ggplot2)
library(sf)
library(forcats)

treatment_types <- c("Initial Planting", "Fill-in or Replant", 
                     "Release - Chemical", "Release - Non-Chemical", "TSI - Thin",
                     "Harvest - Salvage", "Harvest - Non-Salvage", 
                     "Site Prep - Chemical", "Site Prep - Non-Chemical")


weighted_mean_diff_activity_year <- assigned_activities %>%
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

weighted_mean_diff_ig_year <- assigned_activities %>%
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
  labs(title = "Weighted Mean Years Between Fire and Treatment",
       subtitle = "By Ignition Year, USFS Region 5, 2000-2021",
       x = "Ignition Year",
       y = "Weighted Mean Years Since Fire") +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 11),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
    axis.text.y = element_text(size = 8),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 9)
  ) +
  scale_x_continuous(breaks = seq(2000, 2021, by = 4))

# Print the faceted plot
print(mean_diff_ig_year_plot)

# Save the faceted plot
ggsave("mean_diff_ig_year_plot.png", mean_diff_ig_year_plot, width = 10, height = 8, dpi = 300)


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




create_time_since_fire_plots <- function(summarized_data){
    
  # Calculate overall percentages for each type_label and year_group
  overall_percentages <- summarized_data %>%
    group_by(type_labels, year_group) %>%
    summarize(total_gross_acres = sum(Acres)) %>%
    group_by(type_labels) %>%
    mutate(total_percentage = total_gross_acres / sum(total_gross_acres) * 100)
  
  # Order the year groups
  year_group_order <- c("0-1", "1-2", "2-3", "3-4", "4-5", ">5")
  summarized_data$year_group <- factor(summarized_data$year_group, levels = year_group_order)
  overall_percentages$year_group <- factor(overall_percentages$year_group, levels = year_group_order)
  
  # Factor type_labels
  type_labels_order <- c("Initial Planting", "Fill-in or Replant", "Release - Chemical", "Release - Non-Chemical", 
                         "Harvest - Salvage", "Harvest - Non-Salvage", "Site Prep - Chemical", "Site Prep - Non-Chemical",
                         "TSI - Thin")
  summarized_data$type_labels <- factor(summarized_data$type_labels, levels = type_labels_order)
  overall_percentages$type_labels <- factor(overall_percentages$type_labels, levels = type_labels_order)
  
  # Create a complete grid of all combinations
  all_combinations <- expand.grid(
    Ig_Year = min(summarized_data$Ig_Year):max(summarized_data$Ig_Year),
    type_labels = type_labels_order,
    year_group = year_group_order
  )
  
  # Join the complete grid with the existing data
  complete_data <- all_combinations %>%
    left_join(summarized_data, by = c("Ig_Year", "type_labels", "year_group")) %>%
    mutate(Acres = coalesce(Acres, 0),
           percentage = coalesce(percentage, 0))
  
  # Set factor levels
  complete_data$year_group <- factor(complete_data$year_group, levels = year_group_order)
  complete_data$type_labels <- factor(complete_data$type_labels, levels = type_labels_order)
  overall_percentages$year_group <- factor(overall_percentages$year_group, levels = year_group_order)
  overall_percentages$type_labels <- factor(overall_percentages$type_labels, levels = type_labels_order)
  
  # Create main plot
  main_plot <- ggplot() +
    geom_col(data = complete_data, 
             aes(x = Ig_Year, y = percentage, fill = fct_rev(year_group)),
             position = "stack", width = 1) +
    facet_wrap(~ type_labels, scales = "free", ncol = 3) +
    scale_fill_viridis_d(option = "plasma", end = 0.9, direction = -1, name = "Years Since Fire") +
    scale_x_continuous(breaks = c(seq(min(complete_data$Ig_Year), max(complete_data$Ig_Year), by = 5), 
                                  max(complete_data$Ig_Year) + 2),
                       labels = c(seq(min(complete_data$Ig_Year), max(complete_data$Ig_Year), by = 5), 
                                  "Total")) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    labs(title = "Distribution of Postfire Activity Area by Years Since Fire",
         subtitle = "Percentage of Gross Acres by Ignition Year - USFS Region 5, 2000 - 2021",
         x = "Ignition Year",
         y = "Percentage of Gross Acres") +
    theme_bw(base_size = 10) +
    theme(
      axis.text.x = element_text(hjust = 0.5, size = 9),
      strip.text = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 9),
      legend.title = element_text(face = "bold", size = 9), 
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 9),
      plot.margin = margin(10, 10, 10, 10),
      axis.title = element_text(face = "bold", size = 10),
      axis.text.y = element_text(size = 8),
      legend.position = "bottom",
      legend.justification = "center"
    ) +
    guides(fill = guide_legend(nrow = 1, reverse = TRUE))
  
  # Create summary plot
  summary_plot <- ggplot(overall_percentages, aes(x = fct_rev(type_labels), y = total_percentage, fill = fct_rev(year_group))) +
    geom_col(position = "stack") +
    scale_fill_viridis_d(option = "plasma", end = 0.9, direction = -1, name = "Years Since Fire") +
    labs(title = "Overall Distribution of Years Between Fire & Activity",
         subtitle = "Percentage of Total Postfire Acres, USFS R5 Fires - 2000-2021",
         x = "Activity Type",
         y = "Percentage of Gross Acres") +
    theme_bw(base_size = 10) +
    theme(
      axis.text.x = element_text(hjust = 1, angle = 45),
      axis.title.y = element_text(margin = margin(r = 10)),
      legend.text = element_text(size = 8),
      legend.title = element_text(face = "bold", size = 9), 
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 9),
      plot.margin = margin(10, 10, 10, 10),
      axis.title = element_text(face = "bold", size = 10),
      axis.text.y = element_text(size = 8),
      legend.position = "bottom",
      legend.justification = "center"
    ) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    coord_flip() +
    guides(fill = guide_legend(nrow = 1, reverse = TRUE))
  
  # Return both plots
  return(list(main_plot = main_plot, summary_plot = summary_plot))
}

# Call the function and capture the returned plots
diff_plots <- create_time_since_fire_plots(summarized_data)

# Print and save the plots
print(diff_plots$main_plot)
print(diff_plots$summary_plot)

ggsave("time_since_fire_distribution_main.png", diff_plots$main_plot, width = 15, height = 10)
ggsave("time_since_fire_distribution_summary.png", diff_plots$summary_plot, width = 10, height = 6)

#### Boxplots ####

# Prepare the data
boxplot_data <- vegetation_activities %>%
  mutate(type_labels = case_when(
    # type_labels == "TSI - Release" & METHOD == "Chemical" ~ "TSI - Release (Chemical)",
    # type_labels == "TSI - Release" & METHOD != "Chemical" ~ "TSI - Release (Manual/Mechanical)",
    type_labels %in% c("Site Prep - Chemical","Site Prep - Non-Chemical") ~ "Site Prep - Any",
    TRUE ~ type_labels)) %>%
  mutate(EVT_PHYS = case_when(
    EVT_PHYS == "Conifer" ~ "Conifer",
    EVT_PHYS == "Shrubland" ~ "Shrubland",
    EVT_PHYS %in% c("Hardwood", "Riparian", "Conifer-Hardwood", "Exotic Tree-Shrub") ~ "Hardwood",
    TRUE ~ "Other"
  )) %>%
  filter(type_labels %in% c("Initial Planting", "Fill-in or Replant", "Site Prep - Any", "TSI - Release")) %>%
  mutate(type_labels = factor(type_labels, levels = c("Initial Planting", "Fill-in or Replant", "Site Prep - Any", "TSI - Release")))

# Create the box plot function
create_boxplot <- function(data) {
  ggplot(data, aes(x = factor(EVT_PHYS), y = min_diff, fill = type_labels)) +
    geom_boxplot(outlier.size = 0.5) +
    facet_wrap(~ type_labels, scales = "free", ncol = 2) +
    # scale_fill_discrete_sequential(palette = "BluGrn", rev = FALSE) +
    # scale_x_discrete(breaks = seq(min(data$Ig_Year), max(data$Ig_Year), by = 4)) +
    # scale_y_continuous(labels = scales::comma) +
    labs(title = "Distribution of Years Between Fire and Treatment",
         subtitle = "USFS Region 5 Fires, 2000 - 2021",
         x = "Ignition Year",
         y = "Years Since Fire",
         fill = "Treatment Type") +
    theme_bw(base_size = 10) +
    theme(
      axis.text.x = element_text(hjust = 0.5, lineheight = 0.8, size = 9),
      strip.text = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 10),
      legend.title = element_text(face = "bold", size = 10),
      plot.title = element_text(face = "bold", size = 12),
      plot.margin = margin(10,10,20,10),
      axis.title = element_text(face = "bold", size = 11),
      axis.text.y = element_text(size = 10),
      legend.position = "none"
    )
}

# Create and save plot
boxplot_veg <- create_boxplot(boxplot_data)
print(boxplot_veg)
ggsave("boxplot_veg_diffyears.png", boxplot_veg, width = 7, height = 5)



boxplot_years_since_fire <- function(data) {
  # Prepare the data
  plot_data <- data %>%
    filter(type_labels %in% c("Initial Planting", "Fill-in or Replant", "Release - Chemical", "Release - Non-Chemical", 
                              "TSI - Thin", "Harvest - Salvage", "Harvest - Non-Salvage", 
                              "Site Prep - Chemical", "Site Prep - Non-Chemical"))
  
  # Set factor levels for proper ordering
  plot_data$type_labels <- factor(plot_data$type_labels, 
                                  levels = c("Initial Planting", "Fill-in or Replant", "Release - Chemical", "Release - Non-Chemical", 
                                             "TSI - Thin", "Harvest - Salvage", "Harvest - Non-Salvage", 
                                             "Site Prep - Chemical", "Site Prep - Non-Chemical"))
  
  # Create the box plot
  ggplot(plot_data, aes(x = fct_rev(type_labels), y = mean_diff, fill = type_labels)) +
    geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.5) +
    coord_flip() +
    scale_fill_brewer(palette = "Set1") +
    labs(title = "Distribution of Years Between Fire and Treatment",
         subtitle = "USFS R5 | Fires, 2000-2021 | Activities 2001-2022",
         x = "Treatment Type",
         y = "Years Since Fire") +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "none",
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(size = 9),
      axis.title = element_text(face = "bold", size = 10),
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 11),
      panel.grid.major.y = element_blank()
    )
}

# Generate plot
Years_Since_Fire_Boxplot <- boxplot_years_since_fire(vegetation_activities)

# Print plot
print(Years_Since_Fire_Boxplot)

# Save plot
ggsave("Years_Since_Fire_Boxplot.png", Years_Since_Fire_Boxplot, width = 7, height = 3)





# Data preparation
veg_by_diff <- function(vegetation_activities){
    
  veg_treatments_diff <- vegetation_activities %>%
    filter(Category == "All Activities",
           type_labels %in% c("Initial Planting", "Fill-in or Replant", "Release - Chemical", "Release - Non-Chemical", 
                              "Harvest - Salvage", "Harvest - Non-Salvage", "Site Prep - Chemical", "Site Prep - Non-Chemical",
                              "TSI - Thin")) %>%
    mutate(year_group = case_when(
      mean_diff <= 1 ~ "0-1",
      mean_diff <= 2 ~ "1-2",
      mean_diff <= 3 ~ "2-3",
      mean_diff <= 4 ~ "3-4",
      mean_diff <= 5 ~ "4-5",
      TRUE ~ ">5"
    )) %>%
    mutate(Veg_Type = case_when(
      EVT_PHYS == "Conifer" ~ "Conifer",
      EVT_PHYS == "Shrubland" ~ "Shrubland",
      EVT_PHYS %in% c("Hardwood", "Riparian", "Conifer-Hardwood", "Exotic Tree-Shrub") ~ "Hardwood",
      EVT_PHYS == "Grassland" ~ "Grassland",
      TRUE ~ "Other"
    ))
  
  # Create a complete grid of all combinations
  all_combinations <- expand.grid(
    type_labels = unique(veg_treatments_diff$type_labels),
    year_group = c("0-1", "1-2", "2-3", "3-4", "4-5", ">5"),
    Veg_Type = c("Conifer", "Shrubland", "Hardwood", "Grassland", "Other")
  )
  
  # Join the complete grid with the existing data
  veg_treatments_complete <- all_combinations %>%
    left_join(veg_treatments_diff, by = c("type_labels", "year_group", "Veg_Type")) %>%
    mutate(Acres = coalesce(Area_acres, 0))  # Replace NA with 0
  
  # Set factor levels for type_labels and Veg_Type
  veg_treatments_complete$type_labels <- factor(veg_treatments_complete$type_labels, 
                                                levels = c("Initial Planting", "Fill-in or Replant", 
                                                           "Release - Chemical", "Release - Non-Chemical", "TSI - Thin",
                                                           "Harvest - Salvage", "Harvest - Non-Salvage", 
                                                           "Site Prep - Chemical", "Site Prep - Non-Chemical"
                                                           ))
  veg_treatments_complete$Veg_Type <- factor(veg_treatments_complete$Veg_Type, 
                                             levels = c("Conifer", "Shrubland", "Hardwood", "Grassland", "Other"))
  veg_treatments_complete$year_group <- factor(veg_treatments_complete$year_group, 
                                               levels = c("0-1", "1-2", "2-3", "3-4", "4-5", ">5"))
  
  # Create a plot comparing vegetation cover by time since fire for each treatment type
  veg_treatments_diff_plot <- ggplot(veg_treatments_complete, aes(x = year_group, y = Acres, fill = Veg_Type)) +
    geom_col(position = "stack", width = 0.9) +
    facet_wrap(~ type_labels, scales = "free", ncol = 3) +
    scale_fill_manual(values = c("Conifer" = "springgreen4", 
                                 "Shrubland" = "goldenrod", 
                                 "Hardwood" = "purple2",
                                 "Grassland" = "yellow",
                                 "Other" = "gray70")) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Distribution of Current Vegetation Type by Years Since Fire",
         subtitle = "Net Postfire Activity Acres - USFS Region 5, 2000 - 2021",
         x = "Years Since Fire",
         y = "Acres",
         fill = "Vegetation Type") +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
      axis.text.y = element_text(size = 9),
      axis.title = element_text(size = 10, face = "bold"),
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 10),
      strip.text = element_text(face = "bold", size = 10),
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 9),
    )
}

veg_treatments_diffyears <- veg_by_diff(vegetation_activities)

# Display the plot
print(veg_treatments_diffyears)

# Save the plot
ggsave("veg_treatments_diffyears.png", veg_treatments_diffyears, width = 7, height = 6)






