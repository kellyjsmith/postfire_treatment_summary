# Zonal Analysis Functions
library(dplyr)
library(sf)
library(terra)
library(tidyr)
library(stringr)

source("Analysis/process_assigned_activities.R")

# Function for SUID groupings
summarize_veg_severity_eco_suid <- function(areas, evt_raster_file, evt_csv_file, severity_folder) {
  
  evt_raster <- rast(evt_raster_file)
  evt_metadata <- read.csv(evt_csv_file)
  
  cell_size_acres <- (res(evt_raster)[1] * res(evt_raster)[2]) / 4046.86
  
  results <- areas %>%
    group_by(fire_id, US_L3NAME, SUID) %>%
    group_map(~ {
      fire_data <- .x
      
      ig_year <- unique(fire_data$Ig_Year)
      current_fire_id <- unique(fire_data$fire_id)
      current_eco <- unique(fire_data$US_L3NAME)
      current_suid <- unique(fire_data$SUID)
      
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
      
      summary <- fire_data %>%
        st_drop_geometry() %>%
        select(year, diff_years, prod, reburns, gross_acres, n_plantings) %>%
        mutate(total_pixels = nrow(values)) %>%
        left_join(
          values %>%
            group_by(Severity_Class, Veg_Type) %>%
            summarise(Pixels = n(), .groups = "drop") %>%
            mutate(Proportion = Pixels / sum(Pixels)),
          by = character()
        ) %>%
        mutate(
          Acres = Pixels * cell_size_acres,
          US_L3NAME = current_eco,
          Ig_Year = ig_year,
          fire_id = current_fire_id,
          SUID = current_suid
        )
      
      summary$geometry <- st_union(st_geometry(fire_data))
      
      return(summary)
    }, .keep = TRUE) %>%
    bind_rows()
  
  # Convert the results to an sf object
  results_sf <- st_as_sf(results)
  
  return(results_sf)
}

# Function for fire and ignition year groupings
summarize_planted_veg_severity_eco <- function(areas, evt_raster_file, evt_csv_file, severity_folder) {
  
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
  
  # Convert the results to an sf object
  results_sf <- st_as_sf(results)
  
  return(results_sf)
}

# Helper function to summarize with success score
summarize_with_success_score <- function(data) {
  data %>%
    group_by(US_L3NAME, fire_id, year, SUID, Ig_Year) %>%
    summarize(
      Tot_Ac = sum(Acres),
      Con_Ac = sum(Acres[Veg_Type == "Conifer"]),
      Shb_Ac = sum(Acres[Veg_Type == "Shrubland"]),
      HW_Ac = sum(Acres[Veg_Type == "Hardwood"]),
      Grs_Ac = sum(Acres[Veg_Type == "Grassland"]),
      Oth_Ac = sum(Acres[Veg_Type == "Other"]),
      Con_Pct = Con_Ac / Tot_Ac * 100,
      Shb_Pct = Shb_Ac / Tot_Ac * 100,
      HW_Pct = HW_Ac / Tot_Ac * 100,
      Grs_Pct = Grs_Ac / Tot_Ac * 100,
      Oth_Pct = Oth_Ac / Tot_Ac * 100,
      Prod = first(prod),
      Mean_Diff = mean(diff_years, na.rm = TRUE),
      Reburns = max(reburns, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Con_Shb_Ra = Con_Ac / (Shb_Ac + 0.1),
      Refor_Score = (Con_Pct + (Con_Shb_Ra / (1 + Con_Shb_Ra)) * 100) / 2,
      map_labels = sprintf("SUID: %s\nNet Acres: %.1f\nReforestation Score: %.1f\nPct Shrub: %.1f\nPlant Year: %d\nReburns: %d", 
                           SUID, Tot_Ac, Refor_Score, Shb_Pct, year, Reburns)
    ) %>%
    ungroup()
}