library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(purrr)

summarize_veg_severity_wide <- function(areas, evt_raster_file, evt_csv_file, severity_folder, is_planted = TRUE) {
  
  evt_raster <- rast(evt_raster_file)
  evt_metadata <- read.csv(evt_csv_file)
  
  cell_size_acres <- (res(evt_raster)[1] * res(evt_raster)[2]) / 4046.86
  
  results <- areas %>%
    group_by(fire_id) %>%
    group_map(~ {
      fire_data <- .x
      
      if (!"Ig_Year" %in% names(fire_data) || length(unique(fire_data$Ig_Year)) != 1) {
        warning(paste("Skipping fire_id:", unique(fire_data$fire_id), "- Invalid or missing Ig_Year"))
        return(NULL)
      }
      
      ig_year <- unique(fire_data$Ig_Year)
      current_fire_id <- unique(fire_data$fire_id)
      
      severity_file <- file.path(severity_folder, paste0("mtbs_CA_", ig_year, ".tif"))
      if (!file.exists(severity_file)) {
        warning(paste("Severity file not found for Ig_Year:", ig_year, "- Skipping this fire"))
        return(NULL)
      }
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
        mutate(Proportion = Acres / sum(Acres)) %>%
        pivot_wider(names_from = Veg_Type, values_from = c(Acres, Proportion), values_fill = 0)
      
      summary <- summary %>% mutate(across(everything(), ~replace_na(., 0)))
      
      summary$fire_id <- current_fire_id
      summary$Ig_Year <- ig_year
      
      if (is_planted) {
        summary$weighted_mean_diff <- weighted.mean(fire_data$mean_diff, w = fire_data$net_acres)
        summary$min_diff <- min(fire_data$min_diff)
        summary$max_diff <- max(fire_data$max_diff)
        summary$weighted_mean_prod <- weighted.mean(fire_data$mean_prod, w = fire_data$net_acres)
        summary$min_prod <- min(fire_data$min_prod)
        summary$max_prod <- max(fire_data$max_prod)
      }
      
      return(summary)
    }, .keep = TRUE) %>%
    bind_rows()
  
  return(results)
}



# Usage
planted_areas <- readRDS("net_activities.RDS") %>%
  filter(type_labels == "Initial Planting")

burned_areas <- readRDS("burned_areas.RDS")

evt_raster_file <- "../Data/landfire/LC23_EVT_240.tif"
evt_csv_file <- "../Data/landfire/LF23_EVT_240.csv"
severity_folder <- "../Data/Severity"

veg_severity_planted_wide <- summarize_veg_severity_wide(planted_areas, evt_raster_file, evt_csv_file, severity_folder, is_planted = TRUE)
veg_severity_burned_wide <- summarize_veg_severity_wide(burned_areas, evt_raster_file, evt_csv_file, severity_folder, is_planted = FALSE)

# Save results
saveRDS(veg_severity_planted_wide, "veg_severity_planted_summary_wide.RDS")
saveRDS(veg_severity_burned_wide, "veg_severity_burned_summary_wide.RDS")
write.csv(veg_severity_planted_wide, "veg_severity_planted_summary_wide.csv", row.names = FALSE)
write.csv(veg_severity_burned_wide, "veg_severity_burned_summary_wide.csv", row.names = FALSE)




library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(purrr)

summarize_veg_severity <- function(areas, evt_raster_file, evt_csv_file, severity_folder, is_planted = TRUE) {
  
  evt_raster <- rast(evt_raster_file)
  evt_metadata <- read.csv(evt_csv_file)
  
  cell_size_acres <- (res(evt_raster)[1] * res(evt_raster)[2]) / 4046.86
  
  results <- areas %>%
    group_by(fire_id) %>%
    group_map(~ {
      fire_data <- .x
      
      if (!"Ig_Year" %in% names(fire_data) || length(unique(fire_data$Ig_Year)) != 1) {
        warning(paste("Skipping fire_id:", unique(fire_data$fire_id), "- Invalid or missing Ig_Year"))
        return(NULL)
      }
      
      ig_year <- unique(fire_data$Ig_Year)
      current_fire_id <- unique(fire_data$fire_id)
      
      severity_file <- file.path(severity_folder, paste0("mtbs_CA_", ig_year, ".tif"))
      if (!file.exists(severity_file)) {
        warning(paste("Severity file not found for Ig_Year:", ig_year, "- Skipping this fire"))
        return(NULL)
      }
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
      
      if (is_planted) {
        # Add existing statistics from fire_data without recalculation
        summary <- summary %>%
          mutate(
            mean_diff = mean(fire_data$mean_diff),
            min_diff = min(fire_data$min_diff),
            max_diff = max(fire_data$max_diff),
            mean_prod = mean(fire_data$mean_prod),
            min_prod = min(fire_data$min_prod),
            max_prod = max(fire_data$max_prod)
          )
      }
      
      return(summary)
    }, .keep = TRUE) %>%
    bind_rows()
  
  return(results)
}

# Usage
planted_areas <- readRDS("net_activities.RDS") %>%
  filter(type_labels == "Initial Planting")

burned_areas <- readRDS("burned_areas.RDS")

evt_raster_file <- "../Data/landfire/LC23_EVT_240.tif"
evt_csv_file <- "../Data/landfire/LF23_EVT_240.csv"
severity_folder <- "../Data/Severity"

veg_severity_planted <- summarize_veg_severity(planted_areas, evt_raster_file, evt_csv_file, severity_folder, is_planted = TRUE)
veg_severity_burned <- summarize_veg_severity(burned_areas, evt_raster_file, evt_csv_file, severity_folder, is_planted = FALSE)

# Save results
saveRDS(veg_severity_planted, "veg_severity_planted_summary.RDS")
saveRDS(veg_severity_burned, "veg_severity_burned_summary.RDS")
write.csv(veg_severity_planted, "veg_severity_planted_summary.csv", row.names = FALSE)
write.csv(veg_severity_burned, "veg_severity_burned_summary.csv", row.names = FALSE)




library(sf)
library(dplyr)
library(terra)
library(tidyr)
library(sf)

summarize_veg_severity_eco <- function(areas, evt_raster_file, evt_csv_file, severity_folder) {
  
  evt_raster <- rast(evt_raster_file)
  evt_metadata <- read.csv(evt_csv_file)
  
  cell_size_acres <- (res(evt_raster)[1] * res(evt_raster)[2]) / 4046.86
  
  results <- areas %>%
    group_by(fire_id, US_L3NAME) %>%
    group_map(~ {
      fire_data <- .x
      
      if (!"Ig_Year" %in% names(fire_data) || length(unique(fire_data$Ig_Year)) != 1) {
        warning(paste("Skipping fire_id:", unique(fire_data$fire_id), "- Invalid or missing Ig_Year"))
        return(NULL)
      }
      
      ig_year <- unique(fire_data$Ig_Year)
      current_fire_id <- unique(fire_data$fire_id)
      current_eco <- unique(fire_data$US_L3NAME)
      
      severity_file <- file.path(severity_folder, paste0("mtbs_CA_", ig_year, ".tif"))
      if (!file.exists(severity_file)) {
        warning(paste("Severity file not found for Ig_Year:", ig_year, "- Skipping this fire"))
        return(NULL)
      }
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
          weighted_mean_diff = weighted.mean(fire_data$mean_diff, w = fire_data$net_acres),
          min_diff = min(fire_data$min_diff),
          max_diff = max(fire_data$max_diff),
          weighted_mean_prod = weighted.mean(fire_data$mean_prod, w = fire_data$net_acres),
          min_prod = min(fire_data$min_prod),
          max_prod = max(fire_data$max_prod),
          geometry = st_union(st_geometry(fire_data))
        )
      
      return(summary)
    }, .keep = TRUE) %>%
    bind_rows()
  
  # Convert the results to an sf object
  results_sf <- st_as_sf(results)
  
  return(results_sf)
}

# Usage
evt_raster_file <- "../Data/landfire/LC23_EVT_240.tif"
evt_csv_file <- "../Data/landfire/LF23_EVT_240.csv"
severity_folder <- "../Data/Severity"

veg_severity_eco <- summarize_veg_severity_eco(net_activities_eco, evt_raster_file, evt_csv_file, severity_folder)

# Save results
saveRDS(veg_severity_eco, "veg_severity_eco_summary.RDS")
st_write(veg_severity_eco, "veg_severity_eco_summary.shp", append = FALSE)
write.csv(veg_severity_eco %>% st_drop_geometry(), "veg_severity_eco_summary.csv", row.names = FALSE)


# Calculate areas and percentages by vegetation type, severity class, and ecoregion
veg_severity_eco_summary <- veg_severity_eco %>%
  group_by(US_L3NAME, Severity_Class, Veg_Type) %>%
  summarize(
    Area = sum(Acres),
    .groups = "drop"
  ) %>%
  group_by(US_L3NAME, Severity_Class) %>%
  mutate(
    Total_Area = sum(Area),
    Percentage = Area / Total_Area * 100
  ) %>%
  ungroup()

# Create a table of areas by severity class for each ecoregion
severity_eco_table <- veg_severity_eco_summary %>%
  st_drop_geometry() %>%
  group_by(US_L3NAME, Severity_Class) %>%
  summarize(
    Total_Planted_Acres = sum(Area),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Severity_Class,
    values_from = Total_Planted_Acres,
    values_fill = 0
  ) %>%
  mutate(Total = rowSums(across(where(is.numeric)))) %>%
  arrange(desc(Total))

# Identify top 5 ecoregions with the most planting areas
top_5_ecoregions <- severity_eco_table %>%
  top_n(5, Total) %>%
  pull(US_L3NAME)

# Create figures for top 5 ecoregions
plot_veg_severity_distribution <- function(eco_name) {
  veg_severity_eco_summary %>%
    filter(US_L3NAME == eco_name) %>%
    ggplot(aes(x = Severity_Class, y = Percentage, fill = Veg_Type)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_viridis_d() +
    labs(
      title = paste("Vegetation Type Distribution by Severity Class in", eco_name),
      x = "Severity Class",
      y = "Percentage",
      fill = "Vegetation Type"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.position = "right"
    )
}

# Generate plots for top 5 ecoregions
eco_plots <- lapply(top_5_ecoregions, plot_veg_severity_distribution)

# Display the first plot as an example
print(eco_plots[[1]])

# Save the table
write.csv(severity_eco_table, "severity_eco_table.csv", row.names = FALSE)

# Save the plots
for (i in 1:length(eco_plots)) {
  ggsave(paste0("veg_severity_distribution_", gsub(" ", "_", top_5_ecoregions[i]), ".png"), 
         eco_plots[[i]], width = 10, height = 6)
}





