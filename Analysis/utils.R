# Common libraries
library(dplyr)
library(sf)
library(terra)
library(ggplot2)
library(tidyr)
library(forcats)
library(stringr)

# Common functions
load_and_prepare_data <- function() {
  processed_activities <- readRDS("processed_activities_final.RDS")
  cal_eco3 <- st_read("../Data/ca_eco_l3.shp") %>%
    st_transform(3310)
  return(list(processed_activities = processed_activities, cal_eco3 = cal_eco3))
}

prepare_net_activities <- function(activities) {
  activities %>%
    st_collection_extract("POLYGON") %>%
    group_by(type_labels, Ig_Year, fire_id) %>%
    summarize(geometry = st_union(geometry),
              min_diff = min(diff_years, na.rm = TRUE),
              mean_diff = mean(diff_years, na.rm = TRUE),
              max_diff = max(diff_years, na.rm = TRUE),
              min_prod = min(as.numeric(PRODUCTIVI), na.rm = TRUE),
              mean_prod = mean(as.numeric(PRODUCTIVI), na.rm = TRUE),
              max_prod = max(as.numeric(PRODUCTIVI), na.rm = TRUE),
              .groups = "drop") %>%
    mutate(net_acres = as.numeric(st_area(geometry))/4046.86)
}

summarize_veg_severity <- function(areas, evt_raster_file, evt_csv_file, severity_folder) {
  evt_raster <- rast(evt_raster_file)
  evt_metadata <- read.csv(evt_csv_file)
  
  cell_size_acres <- (res(evt_raster)[1] * res(evt_raster)[2]) / 4046.86
  
  results <- areas %>%
    group_by(fire_id, US_L3NAME, Ig_Year) %>%
    group_map(~ {
      fire_data <- .x
      
      ig_year <- unique(fire_data$Ig_Year)
      current_fire_id <- unique(fire_data$fire_id)
      current_eco <- unique(fire_data$US_L3NAME)
      
      severity_file <- file.path(severity_folder, paste0("mtbs_CA_", ig_year, ".tif"))
      severity_raster <- rast(severity_file)
      
      fire_data_vect <- vect(fire_data)
      fire_data_vect <- project(fire_data_vect, crs(evt_raster))
      
      evt_crop <- crop(evt_raster, fire_data_vect)
      evt_mask <- mask(evt_crop, fire_data_vect)
      
      severity_crop <- crop(severity_raster, fire_data_vect)
      severity_mask <- mask(severity_crop, fire_data_vect)
      
      evt_values <- as.data.frame(evt_mask, xy = TRUE)
      severity_values <- as.data.frame(severity_mask, xy = TRUE)
      
      values <- left_join(evt_values, severity_values, by = c("x", "y"))
      names(values) <- c("x", "y", "evt", "severity")
      values <- na.omit(values)
      
      values <- left_join(values, evt_metadata[, c("VALUE", "EVT_PHYS")], by = c("evt" = "VALUE"))
      
      values <- values %>%
        mutate(Veg_Type = case_when(
          EVT_PHYS == "Conifer" ~ "Conifer",
          EVT_PHYS == "Shrubland" ~ "Shrubland",
          EVT_PHYS %in% c("Hardwood", "Riparian", "Conifer-Hardwood") ~ "Hardwood",
          EVT_PHYS == "Grassland" ~ "Grassland",
          TRUE ~ "Other"
        ))
      
      values$Severity_Class <- factor(values$severity,
                                      levels = 1:6,
                                      labels = c("Unburned to Low", "Low", "Moderate", "High", 
                                                 "Increased Greenness", "Non-Processing Area"))
      
      summary <- values %>%
        group_by(Severity_Class, Veg_Type) %>%
        summarise(Acres = n() * cell_size_acres, .groups = "drop") %>%
        mutate(Proportion = Acres / sum(Acres))
      
      summary$fire_id <- current_fire_id
      summary$Ig_Year <- ig_year
      summary$US_L3NAME <- current_eco
      
      summary <- summary %>%
        mutate(
          n_plantings = sum(fire_data$n_plantings),
          mean_diff = mean(fire_data$mean_diff),
          min_diff = min(fire_data$min_diff),
          max_diff = max(fire_data$max_diff),
          mean_prod = mean(fire_data$mean_prod),
          min_prod = min(fire_data$min_prod),
          max_prod = max(fire_data$max_prod),
          mean_reburns = mean(fire_data$mean_reburns),
          min_reburns = min(fire_data$min_reburns),
          max_reburns = max(fire_data$max_reburns),
          gross_acres = sum(fire_data$gross_acres),
          mean_unit_size = mean(fire_data$mean_unit_size),
          geometry = st_union(st_geometry(fire_data))
        )
      
      return(summary)
    }, .keep = TRUE) %>%
    bind_rows()
  
  results_sf <- st_as_sf(results)
  
  return(results_sf)
}