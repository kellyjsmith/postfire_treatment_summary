# Part 1: Zonal Analysis

# Load required libraries
library(terra)
library(sf)
library(tidyverse)

# Load data
assigned_activities <- readRDS("../Output/assigned_activities_new.RDS")
processed_activities <- readRDS("processed_activities.RDS")
fire_events <- st_read("../Output/R5_fires_00_21.shp")
evt_raster_file <- "../Data/landfire/LC23_EVT_240.tif"
evt_csv <- read.csv("../Data/landfire/LF23_EVT_240.csv")

# Prepare net activities data
prepare_net_activities <- function(activities) {
  activities %>%
    st_collection_extract() %>%
    st_cast("MULTIPOLYGON") %>%
    mutate(fire_id = paste(Incid_Name, Event_ID, sep="_")) %>%
    group_by(type_labels, Ig_Year, fire_id) %>%
    summarize(geometry = st_union(geometry),
              min_diff = min(diff_years, na.rm = TRUE),
              mean_diff = mean(diff_years, na.rm = TRUE),
              max_diff = max(diff_years, na.rm = TRUE),
              min_prod = min(as.numeric(PRODUCTIVI), na.rm = TRUE),
              mean_prod = mean(as.numeric(PRODUCTIVI), na.rm = TRUE),
              max_prod = max(as.numeric(PRODUCTIVI), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(net_acres = as.numeric(st_area(geometry))/4046.86)
}

net_activities <- prepare_net_activities(processed_activities)

saveRDS(net_activities, "net_activities.RDS")



# write.csv(net_activities %>% st_drop_geometry(), "net_activities.csv")


# Prepare all activities data 
# (these are not currently used in the veg/severity analyses)
# prepare_all_activities <- function(activities) {
#   activities %>%
#     st_collection_extract("POLYGON") %>%
#     mutate(fire_id = paste(Incid_Name, Ig_Year, sep="_")) %>%
#     group_by(type_labels, Ig_Year, fire_id) %>%
#     summarize(geometry = sum(st_area(geometry)),
#               min_diff = min(diff_years, na.rm = TRUE),
#               mean_diff = median(diff_years, na.rm = TRUE),
#               max_diff = max(diff_years, na.rm = TRUE)) %>%
#     ungroup() %>%
#     mutate(gross_acres = as.numeric(st_area(geometry))/4046.86)
# }
# 
# all_activities <- prepare_all_activities(assigned_activities)



# Prepare total burned areas data
prepare_burned_areas <- function(fire_events) {
  fire_events %>%
    mutate(fire_id = paste(Incid_Name, Event_ID, sep="_")) %>%
    group_by(Ig_Year, fire_id) %>%
    summarize(geometry = st_union(geometry)) %>%
    ungroup() %>%
    st_cast("MULTIPOLYGON") %>%
    mutate(net_acres = as.numeric(st_area(.))/4046.86)
}

burned_areas <- prepare_burned_areas(fire_events)

saveRDS(burned_areas, "burned_areas.RDS")
burned_areas <- readRDS("burned_areas.RDS")

# Split data
net_activities_list <- group_split(net_activities, type_labels, Ig_Year, fire_id)
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
           min_diff = x$min_diff,
           mean_diff = x$mean_diff,
           max_diff = x$max_diff,
           min_prod = x$min_prod,
           mean_prod = x$mean_prod,
           max_prod = x$max_prod)
  
  severity_summary <- severity_summary %>%
    mutate(fire_id = x$fire_id,
           Ig_Year = x$Ig_Year,
           type_labels = x$type_labels,
           min_diff = x$min_diff,
           mean_diff = x$mean_diff,
           max_diff = x$max_diff,
           min_prod = x$min_prod,
           mean_prod = x$mean_prod,
           max_prod = x$max_prod)
  
  list(vegetation = veg_summary, severity = severity_summary)
}

# Process all activities and total burned areas
process_areas <- function(areas_list, evt_raster_file, evt_csv) {
  map(areas_list,
      ~process_activities_raster_area(., evt_raster_file, evt_csv),
      .progress = TRUE)
}

vegetation_severity_activities <- process_areas(net_activities_list, evt_raster_file, evt_csv)
vegetation_severity_burned <- process_areas(burned_areas_list, evt_raster_file, evt_csv)

# Separate vegetation and severity summaries
extract_summaries <- function(data, type, category) {
  map_dfr(data, ~.x[[type]]) %>%
    mutate(Category = category)
}

# Usage of the corrected function
vegetation_activities <- extract_summaries(vegetation_severity_activities, "vegetation", "All Activities")
severity_activities <- extract_summaries(vegetation_severity_activities, "severity", "All Activities")
vegetation_burned <- extract_summaries(vegetation_severity_burned, "vegetation", "Total Burned")
severity_burned <- extract_summaries(vegetation_severity_burned, "severity", "Total Burned")

# Save intermediate outputs
saveRDS(vegetation_activities, "vegetation_net_activities_summary.RDS")
saveRDS(severity_activities, "severity_net_activities_summary.RDS")
saveRDS(vegetation_burned, "vegetation_burned_summary.RDS")
saveRDS(severity_burned, "severity_burned_summary.RDS")

severity_burned = readRDS("severity_burned_summary.RDS")
vegetation_burned = readRDS("vegetation_burned_summary.RDS")
severity_activities <- readRDS("severity_net_activities_summary.RDS")

write.csv(severity_activities, "severity_activities.csv")

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
      EVT_PHYS == "Grassland" ~ "Grassland",
      EVT_PHYS %in% c("Hardwood", "Riparian", "Conifer-Hardwood", "Exotic Tree-Shrub") ~ "Hardwood",
      TRUE ~ "Other"
    )) %>%
    group_by(across(all_of(group_vars))) %>%
    summarize(
      Conifer_acres = sum(Area_acres[Veg_Group == "Conifer"], na.rm = TRUE),
      Shrubland_acres = sum(Area_acres[Veg_Group == "Shrubland"], na.rm = TRUE),
      Grassland_acres = sum(Area_acres[Veg_Group == "Grassland"], na.rm = TRUE),
      Hardwood_acres = sum(Area_acres[Veg_Group == "Hardwood"], na.rm = TRUE),
      Other_acres = sum(Area_acres[Veg_Group == "Other"], na.rm = TRUE),
      Total_acres = sum(Area_acres, na.rm = TRUE),
      min_diff = if (has_type_labels) first(min_diff) else NA,
      mean_diff = if (has_type_labels) first(mean_diff) else NA,
      max_diff = if (has_type_labels) first(max_diff) else NA,
      min_prod = if (has_type_labels) first(min_prod) else NA,
      mean_prod = if (has_type_labels) first(mean_prod) else NA,
      max_prod = if (has_type_labels) first(max_prod) else NA
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
      Increased_Greenness_acres = sum(Area_acres[Severity_Class == "Increased Greenness"], na.rm = TRUE),
      Unburned_to_Low_acres = sum(Area_acres[Severity_Class == "Unburned to Low"], na.rm = TRUE),
      Low_acres = sum(Area_acres[Severity_Class == "Low"], na.rm = TRUE),
      Moderate_acres = sum(Area_acres[Severity_Class == "Moderate"], na.rm = TRUE),
      High_acres = sum(Area_acres[Severity_Class == "High"], na.rm = TRUE),
      Total_acres = sum(Area_acres, na.rm = TRUE),
      min_diff = if (has_type_labels) first(min_diff) else NA,
      mean_diff = if (has_type_labels) first(mean_diff) else NA,
      max_diff = if (has_type_labels) first(max_diff) else NA,
      min_prod = if (has_type_labels) first(min_prod) else NA,
      mean_prod = if (has_type_labels) first(mean_prod) else NA,
      max_prod = if (has_type_labels) first(max_prod) else NA
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




library(terra)
library(sf)
library(dplyr)
library(purrr)

# Modified process_activities_raster_area function
process_activities_raster_area <- function(x, evt_raster_file, evt_csv, mtbs_folder="../Data/Severity") {
  # Load rasters
  severity_file <- paste0(mtbs_folder, "/mtbs_CA_", x$Ig_Year[1], ".tif")
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
    mutate(fire_id = x$fire_id[1],
           Ig_Year = x$Ig_Year[1],
           type_labels = x$type_labels[1],
           min_diff = x$min_diff[1],
           mean_diff = x$mean_diff[1],
           max_diff = x$max_diff[1],
           min_prod = x$min_prod[1],
           mean_prod = x$mean_prod[1],
           max_prod = x$max_prod[1])
  
  severity_summary <- severity_summary %>%
    mutate(fire_id = x$fire_id[1],
           Ig_Year = x$Ig_Year[1],
           type_labels = x$type_labels[1],
           min_diff = x$min_diff[1],
           mean_diff = x$mean_diff[1],
           max_diff = x$max_diff[1],
           min_prod = x$min_prod[1],
           mean_prod = x$mean_prod[1],
           max_prod = x$max_prod[1])
  
  # Combine summaries into a list
  result <- list(vegetation = veg_summary, severity = severity_summary)
  
  # Add geometry to both summaries
  result$vegetation$geometry <- st_geometry(x)
  result$severity$geometry <- st_geometry(x)
  
  # Convert both to sf objects
  result$vegetation <- st_as_sf(result$vegetation)
  result$severity <- st_as_sf(result$severity)
  
  return(result)
}

# Modified process_areas function
process_areas <- function(areas_list, evt_raster_file, evt_csv) {
  map(areas_list,
      ~process_activities_raster_area(., evt_raster_file, evt_csv),
      .progress = TRUE)
}

# Load necessary data
processed_activities <- readRDS("processed_activities.RDS")
evt_raster_file <- "../Data/landfire/LC23_EVT_240.tif"
evt_csv <- read.csv("../Data/landfire/LF23_EVT_240.csv")

# Prepare net_activities
net_activities <- prepare_net_activities(processed_activities)

# Split data
net_activities_list <- group_split(net_activities, type_labels, Ig_Year, fire_id)

# Process activities
vegetation_severity_activities <- process_areas(net_activities_list, evt_raster_file, evt_csv)

# Separate vegetation and severity summaries
extract_summaries <- function(data, type) {
  map_dfr(data, ~.x[[type]])
}

vegetation_activities <- extract_summaries(vegetation_severity_activities, "vegetation")
severity_activities <- extract_summaries(vegetation_severity_activities, "severity")

# Save outputs
saveRDS(vegetation_activities, "vegetation_net_activities_summary.RDS")
saveRDS(severity_activities, "severity_net_activities_summary.RDS")

# Write shapefiles
st_write(vegetation_activities, "vegetation_net_activities_summary.shp", append=FALSE)
st_write(severity_activities, "severity_net_activities_summary.shp", append=FALSE)