# Part 1: Zonal Analysis

# Load required libraries
library(terra)
library(sf)
library(tidyverse)

# Load data
assigned_activities <- readRDS("../../Output/assigned_activities_new.RDS")
fire_events <- st_read("../Data/Severity/R5_fires_00_22.shp")
evt_raster_file <- "../../Data/landfire/LC23_EVT_240.tif"
evt_csv <- read.csv("../../Data/landfire/LF23_EVT_240.csv")

# Prepare all activities data
prepare_all_activities <- function(activities) {
  activities %>%
    st_collection_extract("POLYGON") %>%
    mutate(fire_id = paste(Incid_Name, Ig_Year, sep="_")) %>%
    group_by(type_labels, Ig_Year, fire_id) %>%
    summarize(geometry = st_union(geometry),
              median_diff = median(diff_years, na.rm = TRUE)) %>%
    ungroup() %>%
    st_cast("MULTIPOLYGON") %>%
    mutate(net_acres = as.numeric(st_area(.))/4046.86)
}

all_activities <- prepare_all_activities(assigned_activities)

# Prepare total burned areas data (unchanged)
prepare_burned_areas <- function(fire_events) {
  fire_events %>%
    mutate(fire_id = paste(Incid_Name, Ig_Year, sep="_")) %>%
    group_by(Ig_Year, fire_id) %>%
    summarize(geometry = st_union(geometry)) %>%
    ungroup() %>%
    st_cast("MULTIPOLYGON") %>%
    mutate(net_acres = as.numeric(st_area(.))/4046.86)
}

burned_areas <- prepare_burned_areas(fire_events)

# Split data
all_activities_list <- group_split(all_activities, type_labels, Ig_Year, fire_id)
burned_areas_list <- group_split(burned_areas, Ig_Year, fire_id)

# Function to process activities raster area (modified to include type_labels)
process_activities_raster_area <- function(x, evt_raster_file, evt_csv, mtbs_folder="../Data/Severity") {
  # Load rasters
  severity_file <- paste0(mtbs_folder, "/mtbs_CA_", x$Ig_Year, ".tif")
  severity_raster <- rast(severity_file)
  evt_raster <- rast(evt_raster_file)
  
  # Prepare and crop data
  activity_vect <- vect(x)
  activity_vect <- project(activity_vect, crs(evt_raster))
  evt_mask <- mask(crop(evt_raster, ext(activity_vect)), activity_vect)
  severity_mask <- mask(crop(severity_raster, ext(activity_vect)), activity_vect)
  
  # Calculate cell size in acres
  cell_size_acres <- prod(res(evt_raster)) / 4046.86
  
  # Summarize vegetation and severity
  veg_summary <- as.data.frame(freq(evt_mask)) %>%
    set_names(c("Layer", "EVT_VALUE", "Count")) %>%
    mutate(Area_acres = Count * cell_size_acres)
  
  severity_summary <- as.data.frame(freq(severity_mask)) %>%
    set_names(c("Layer", "Severity", "Count")) %>%
    mutate(Area_acres = Count * cell_size_acres,
           Severity_Class = c("1" = "Unburned to Low", "2" = "Low", "3" = "Moderate",
                              "4" = "High", "5" = "Increased Greenness", 
                              "6" = "Non-Processing Area")[as.character(Severity)])
  
  # Merge EVT data and add metadata
  veg_summary <- merge(veg_summary, 
                       evt_csv[, c("VALUE", "EVT_NAME", "EVT_PHYS")], 
                       by.x = "EVT_VALUE", by.y = "VALUE",
                       all.x = TRUE) %>%
    mutate(fire_id = x$fire_id,
           Ig_Year = x$Ig_Year,
           type_labels = x$type_labels,
           median_diff = x$median_diff)
  
  severity_summary <- severity_summary %>%
    mutate(fire_id = x$fire_id,
           Ig_Year = x$Ig_Year,
           type_labels = x$type_labels,
           median_diff = x$median_diff)
  
  list(vegetation = veg_summary, severity = severity_summary)
}

# Process all activities and total burned areas
process_areas <- function(areas_list, evt_raster_file, evt_csv) {
  map(areas_list,
      ~process_activities_raster_area(., evt_raster_file, evt_csv),
      .progress = TRUE)
}

vegetation_severity_all <- process_areas(all_activities_list, evt_raster_file, evt_csv)
vegetation_severity_burned <- process_areas(burned_areas_list, evt_raster_file, evt_csv)

# Separate vegetation and severity summaries
extract_summaries <- function(data, type, category) {
  map_dfr(data, ~.x[[type]]) %>%
    mutate(Category = category)
}

# Usage of the corrected function
vegetation_activities <- extract_summaries(vegetation_severity_all, "vegetation", "All Activities")
severity_activities <- extract_summaries(vegetation_severity_all, "severity", "All Activities")
vegetation_burned <- extract_summaries(vegetation_severity_burned, "vegetation", "Total Burned")
severity_burned <- extract_summaries(vegetation_severity_burned, "severity", "Total Burned")

# Save intermediate outputs
saveRDS(vegetation_activities, "vegetation_all_activities_summary.RDS")
saveRDS(severity_activities, "severity_all_activities_summary.RDS")
saveRDS(vegetation_burned, "vegetation_burned_summary.RDS")
saveRDS(severity_burned, "severity_burned_summary.RDS")

# Summarize vegetation data (modified to handle both activity and burned area data)
summarize_vegetation <- function(data) {
  # Check if type_labels column exists
  has_type_labels <- "type_labels" %in% names(data)
  
  # Define grouping variables based on presence of type_labels
  group_vars <- if (has_type_labels) c("Category", "Ig_Year", "type_labels") else c("Category", "Ig_Year")
  
  data %>%
    mutate(Veg_Group = case_when(
      EVT_PHYS == "Conifer" ~ "Conifer",
      EVT_PHYS == "Shrubland" ~ "Shrubland",
      EVT_PHYS %in% c("Hardwood", "Riparian", "Conifer-Hardwood", "Exotic Tree-Shrub") ~ "Hardwood",
      TRUE ~ "Other"
    )) %>%
    group_by(across(all_of(group_vars))) %>%
    summarize(
      Conifer_acres = sum(Area_acres[Veg_Group == "Conifer"], na.rm = TRUE),
      Shrubland_acres = sum(Area_acres[Veg_Group == "Shrubland"], na.rm = TRUE),
      Hardwood_acres = sum(Area_acres[Veg_Group == "Hardwood"], na.rm = TRUE),
      Other_acres = sum(Area_acres[Veg_Group == "Other"], na.rm = TRUE),
      Total_acres = sum(Area_acres, na.rm = TRUE),
      median_diff = if (has_type_labels) first(median_diff) else NA
    ) %>%
    mutate(across(ends_with("_acres"), ~. / Total_acres * 100, .names = "{.col}_percent"))
}

# Summarize severity data (modified to include type_labels and median_diff)
summarize_severity <- function(data) {
  # Check if type_labels column exists
  has_type_labels <- "type_labels" %in% names(data)
  
  # Define grouping variables based on presence of type_labels
  group_vars <- if (has_type_labels) c("Category", "Ig_Year", "type_labels") else c("Category", "Ig_Year")
  
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarize(
      Unburned_to_Low_acres = sum(Area_acres[Severity_Class == "Unburned to Low"], na.rm = TRUE),
      Low_acres = sum(Area_acres[Severity_Class == "Low"], na.rm = TRUE),
      Moderate_acres = sum(Area_acres[Severity_Class == "Moderate"], na.rm = TRUE),
      High_acres = sum(Area_acres[Severity_Class == "High"], na.rm = TRUE),
      Total_acres = sum(Area_acres, na.rm = TRUE),
      median_diff = if (has_type_labels) first(median_diff) else NA
    ) %>%
    mutate(across(ends_with("_acres"), ~. / Total_acres * 100, .names = "{.col}_percent"))
}

veg_summary <- bind_rows(
  summarize_vegetation(vegetation_activities),
  summarize_vegetation(vegetation_burned)
)

severity_summary <- bind_rows(
  summarize_severity(severity_activities),
  summarize_severity(severity_burned)
)

# Save results
saveRDS(veg_summary, "veg_summary.RDS")
saveRDS(severity_summary, "severity_summary.RDS")


library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(patchwork)

planted_burned_veg_facets <- function(veg_summary, max_plant_axis = 35000, max_burned_axis = 2.6e6) {
  # Prepare planted data
  planted_data <- veg_summary %>%
    filter(type_labels == "Initial Planting") %>%
    select(Ig_Year, Conifer_acres, Shrubland_acres, Hardwood_acres, Other_acres) %>%
    pivot_longer(cols = -c(Ig_Year, type_labels),
                 names_to = "Veg_Type",
                 values_to = "Acres") %>%
    mutate(Veg_Type = gsub("_acres", "", Veg_Type)) %>%
    group_by(Ig_Year, Veg_Type) %>%
    summarise(Acres = sum(Acres, na.rm = TRUE), .groups = "drop")
  
  # Prepare burned area data
  burned_data <- veg_summary %>%
    filter(Category == "Total Burned") %>%
    select(Ig_Year, Conifer_acres, Shrubland_acres, Hardwood_acres, Other_acres) %>%
    pivot_longer(cols = -c(Ig_Year, Category),
                 names_to = "Veg_Type",
                 values_to = "Acres") %>%
    mutate(Veg_Type = gsub("_acres", "", Veg_Type)) %>%
    group_by(Ig_Year, Veg_Type) %>%
    summarise(Acres = sum(Acres, na.rm = TRUE), .groups = "drop")
  
  # Set factor levels for Veg_Type
  veg_levels <- c("Conifer", "Shrubland", "Hardwood", "Other")
  planted_data$Veg_Type <- factor(planted_data$Veg_Type, levels = veg_levels)
  burned_data$Veg_Type <- factor(burned_data$Veg_Type, levels = veg_levels)
  
  # Color palette
  veg_colors <- c("Conifer" = "springgreen4", 
                  "Shrubland" = "goldenrod", 
                  "Hardwood" = "purple2",
                  "Other" = "gray70")
  
  # Create the planted acres plot
  p1 <- ggplot(planted_data, aes(x = Ig_Year, y = Acres, fill = Veg_Type)) +
    geom_col(position = "stack", color = "black", size = 0.2) +
    scale_fill_manual(values = veg_colors, name = "Vegetation Type") +
    labs(title = "Net Postfire Planted Acres",
         x = NULL,
         y = "Acres") +
    theme_bw(base_size = 10) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
      axis.text.y = element_text(size = 9),
      axis.title = element_text(face = "bold", size = 10),
      plot.title = element_text(face = "bold", size = 10, hjust = 0.5)
    ) +
    scale_x_continuous(breaks = seq(2000, 2023, by = 2)) +
    scale_y_continuous(
      labels = comma,
      limits = c(0, max_plant_axis),
      expand = c(0, 0)
    )
  
  # Create the burned acres plot
  p2 <- ggplot(burned_data, aes(x = Ig_Year, y = Acres, fill = Veg_Type)) +
    geom_col(position = "stack", color = "black", size = 0.2) +
    scale_fill_manual(values = veg_colors, name = "Vegetation Type") +
    labs(title = "Burned Acres",
         x = "Ignition Year",
         y = "Acres") +
    theme_bw(base_size = 10) +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      legend.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
      axis.text.y = element_text(size = 9),
      axis.title = element_text(face = "bold", size = 10),
      plot.title = element_text(face = "bold", size = 10, hjust = 0.5)
    ) +
    scale_x_continuous(breaks = seq(2000, 2021, by = 2)) +
    scale_y_continuous(
      labels = function(x) comma(abs(x)),
      # limits = c(-max_burned_axis, 0),
      limits = c(0, max_burned_axis),
      expand = c(0, 0)
    )
  
  # Combine the plots
  combined_plot <- p1 / p2 +
    plot_layout(heights = c(1, 1)) +
    plot_annotation(
      # title = "Existing Vegetation Typ of Planted Acres and Burned Area by Ignition Year,"
      title = "Existing Vegetation Type of Planted Acres and Burned Area by Ignition Year",
      subtitle = "R5 2000 - 2022, Vegetation as of 2023",
      theme = theme(plot.title = element_text(face = "bold", size = 11),
                    plot.subtitle = element_text(size = 11, vjust = 0.5))
    )
  
  return(combined_plot)
}

# Generate plot
planted_burned_faceted <- planted_burned_veg_facets(
  veg_summary,
  max_plant_axis = 35000,
  max_burned_axis = 2.6e6
)

# Save plot
ggsave("planted_burned_veg_faceted.png", planted_burned_faceted, width = 6, height = 5)
