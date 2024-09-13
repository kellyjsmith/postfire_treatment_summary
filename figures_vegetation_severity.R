library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)

# Load data if not already in environment
severity_activities <- readRDS("severity_net_activities_summary.RDS")
vegetation_activities <- readRDS("vegetation_net_activities_summary.RDS")

# 1. Extract planting types
planting_severity <- severity_activities %>%
  filter(type_labels %in% c("Initial Planting", "Fill-in or Replant"))

planting_vegetation <- vegetation_activities %>%
  filter(type_labels %in% c("Initial Planting", "Fill-in or Replant"))


# 2. Intersect datasets
planting_intersect <- planting_severity %>%
  st_join(planting_vegetation) %>%
  select(fire_id = fire_id.x, Ig_Year = Ig_Year.x, type_labels = type_labels.x,
         Severity_Class, EVT_PHYS, EVT_NAME,
         severity_acres = Area_acres.x, veg_acres = Area_acres.y) %>%
  mutate(intersecting_acres = st_area(geometry)/4046.86)


# 3. Calculate percentages
planting_summary <- planting_intersect %>%
  group_by(fire_id, Ig_Year, type_labels, Severity_Class, EVT_PHYS) %>%
  summarize(
    Conifer_Acres = sum(Area_acres.x * (EVT_PHYS == "Conifer"), na.rm = TRUE),
    Shrub_Acres = sum(Area_acres.x * (EVT_PHYS == "Shrubland"), na.rm = TRUE),
    Total_Acres = sum(Area_acres.x, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Conifer_Percent = Conifer_Acres / Total_Acres * 100,
    Shrub_Percent = Shrub_Acres / Total_Acres * 100
  )

# 4. Visualizations

# Stacked bar plot of vegetation types by severity class
ggplot(planting_summary, aes(x = Severity_Class, y = Total_Acres, fill = type_labels)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ Ig_Year, scales = "free_y") +
  labs(title = "Distribution of Planted Area by Severity Class",
       subtitle = "USFS Region 5, 2000-2021",
       x = "Severity Class", y = "Acres", fill = "Planting Type") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("planted_area_by_severity.png", width = 12, height = 8)

# Line plot of conifer vs shrub percentages over time
conifer_shrub_summary <- planting_summary %>%
  group_by(Ig_Year, Severity_Class) %>%
  summarize(
    Conifer_Percent = weighted.mean(Conifer_Percent, w = Total_Acres, na.rm = TRUE),
    Shrub_Percent = weighted.mean(Shrub_Percent, w = Total_Acres, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(conifer_shrub_summary, aes(x = Ig_Year)) +
  geom_line(aes(y = Conifer_Percent, color = "Conifer"), size = 1) +
  geom_line(aes(y = Shrub_Percent, color = "Shrub"), size = 1) +
  facet_wrap(~ Severity_Class, scales = "free_y") +
  labs(title = "Percentage of Conifer vs Shrub in Planted Areas by Severity Class",
       subtitle = "USFS Region 5, 2000-2021",
       x = "Ignition Year", y = "Percentage", color = "Vegetation Type") +
  theme_bw() +
  scale_color_manual(values = c("Conifer" = "darkgreen", "Shrub" = "orange"))

ggsave("conifer_shrub_percent_by_severity.png", width = 12, height = 8)

# Heatmap of conifer percentage by severity class and ignition year
ggplot(conifer_shrub_summary, aes(x = Ig_Year, y = Severity_Class, fill = Conifer_Percent)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Conifer %", option = "C") +
  labs(title = "Conifer Percentage in Planted Areas by Severity Class and Ignition Year",
       subtitle = "USFS Region 5, 2000-2021",
       x = "Ignition Year", y = "Severity Class") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("conifer_percent_heatmap.png", width = 10, height = 6)



library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(purrr)

summarize_veg_in_planted_areas <- function(planted_areas, evt_raster_file, evt_csv_file, severity_folder) {
  
  # Read EVT raster and metadata once
  evt_raster <- rast(evt_raster_file)
  evt_metadata <- read.csv(evt_csv_file)
  
  # Calculate cell size in acres
  cell_size_acres <- (res(evt_raster)[1] * res(evt_raster)[2]) / 4046.86  # Convert m^2 to acres
  
  results <- planted_areas %>%
    group_by(fire_id) %>%
    group_map(~ {
      fire_data <- .x
      
      # Ensure Ig_Year is available and is a single value
      if (!"Ig_Year" %in% names(fire_data) || length(unique(fire_data$Ig_Year)) != 1) {
        warning(paste("Skipping fire_id:", unique(fire_data$fire_id), "- Invalid or missing Ig_Year"))
        return(NULL)
      }
      
      ig_year <- unique(fire_data$Ig_Year)
      current_fire_id <- unique(fire_data$fire_id)
      
      # Read severity raster for this fire
      severity_file <- file.path(severity_folder, paste0("mtbs_CA_", ig_year, ".tif"))
      if (!file.exists(severity_file)) {
        warning(paste("Severity file not found for Ig_Year:", ig_year, "- Skipping this fire"))
        return(NULL)
      }
      severity_raster <- rast(severity_file)
      
      # Ensure CRS match
      fire_data_vect <- vect(fire_data)
      fire_data_vect <- project(fire_data_vect, crs(evt_raster))
      
      # Crop and mask EVT and severity rasters
      evt_crop <- crop(evt_raster, fire_data_vect)
      evt_mask <- mask(evt_crop, fire_data_vect)
      
      severity_crop <- crop(severity_raster, fire_data_vect)
      severity_mask <- mask(severity_crop, fire_data_vect)
      
      # Extract values
      evt_values <- as.data.frame(evt_mask, xy = TRUE)
      severity_values <- as.data.frame(severity_mask, xy = TRUE)
      
      # Combine EVT and severity values
      values <- left_join(evt_values, severity_values, by = c("x", "y"))
      names(values) <- c("x", "y", "evt", "severity")
      values <- na.omit(values)
      
      # Join with EVT metadata
      values <- left_join(values, evt_metadata[, c("VALUE", "EVT_PHYS")], by = c("evt" = "VALUE"))
      
      # Aggregate EVT_PHYS into broader categories
      values <- values %>%
        mutate(Veg_Type = case_when(
          EVT_PHYS == "Conifer" ~ "Conifer",
          EVT_PHYS == "Shrubland" ~ "Shrubland",
          EVT_PHYS %in% c("Hardwood", "Riparian", "Conifer-Hardwood") ~ "Hardwood",
          EVT_PHYS == "Grassland" ~ "Grassland",
          TRUE ~ "Other"
        ))
      
      # Categorize severity
      values$Severity_Class <- factor(values$severity,
                                      levels = 1:6,
                                      labels = c("Unburned to Low", "Low", "Moderate", "High", 
                                                 "Increased Greenness", "Non-Processing Area"))
      
      # Summarize
      summary <- values %>%
        group_by(Severity_Class, Veg_Type) %>%
        summarise(Acres = n() * cell_size_acres, .groups = "drop") %>%
        mutate(Proportion = Acres / sum(Acres)) %>%
        pivot_wider(names_from = Veg_Type, values_from = c(Acres, Proportion), values_fill = 0)
      
      # Replace NAs with 0
      summary <- summary %>% mutate(across(everything(), ~replace_na(., 0)))
      
      summary$fire_id <- current_fire_id
      summary$Ig_Year <- ig_year
      
      return(summary)
    }, .keep = TRUE) %>%  # Add .keep = TRUE to retain grouping variable
    bind_rows()
  
  return(results)
}

# Usage
planted_areas <- readRDS("net_activities.RDS") %>%
  filter(type_labels %in% c("Initial Planting", "Fill-in or Replant"))

evt_raster_file <- "../Data/landfire/LC23_EVT_240.tif"
evt_csv_file <- "../Data/landfire/LF23_EVT_240.csv"
severity_folder <- "../Data/Severity"

veg_severity_summary <- summarize_veg_in_planted_areas(planted_areas, evt_raster_file, evt_csv_file, severity_folder)

veg_severity_summary <- veg_severity_summary %>%
  select(Ig_Year, fire_id, Severity_Class, everything())

# Check for any missing fire_ids
if (any(is.na(veg_severity_summary$fire_id))) {
  warning("Some fire_ids are missing in the final output. Please check the input data.")
}

# Print summary of processed fires
cat("Total fires processed:", length(unique(veg_severity_summary$fire_id)), "\n")
cat("Years range:", min(veg_severity_summary$Ig_Year), "to", max(veg_severity_summary$Ig_Year), "\n")

# Save results
saveRDS(veg_severity_summary, "veg_severity_summary_in_planted_areas.RDS")
write.csv(veg_severity_summary, "veg_severity_summary_in_planted_areas.csv", row.names = FALSE)





# Analysis and Visualization
library(ggplot2)

# Overall distribution of vegetation types by severity class
overall_distribution <- veg_severity_planted_wide %>%
  group_by(Ig_Year, Severity_Class) %>%
  mutate(across(everything(), ~replace_na(., 0))) %>%
  summarise(across(starts_with("Proportion_"), ~ mean(.x, na.rm = TRUE)),
            across(starts_with("Acres_"), ~ sum(.x, na.rm = TRUE)))

ggplot(overall_distribution %>% pivot_longer(starts_with("Acres")), 
       aes(x = Severity_Class, y = value, fill = name)) +
  geom_bar(stat = "identity") +
  labs(title = "Overall Vegetation Distribution by Severity Class in Planted Areas",
       x = "Severity Class", y = "Acres", fill = "Vegetation Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("overall_veg_distribution_by_severity.png", width = 10, height = 6)

# Trend of tree proportion in high severity areas over time
conifer_high_severity <- veg_severity_summary %>%
  filter(Severity_Class == "High") %>%
  group_by(Ig_Year) %>%
  summarise(Conifer_Proportion = mean(Proportion_Conifer, na.rm = TRUE))

ggplot(conifer_high_severity, aes(x = Ig_Year, y = Conifer_Proportion)) +
  geom_line() +
  geom_point() +
  labs(title = "Proportion of Conifer in High Severity Planted Areas Over Time",
       x = "Ignition Year", y = "Conifer Proportion") +
  theme_minimal()

ggsave("tree_high_severity_trend.png", width = 10, height = 6)







library(sf)
library(dplyr)
library(tidyr)

# Load data
veg_severity_planted <- readRDS("veg_severity_planted_summary.RDS")
cal_eco3 <- st_read("../Data/ca_eco_l3.shp")

# Ensure CRS match
st_crs(veg_severity_planted) <- st_crs(cal_eco3)

# Join veg_severity_planted with ecoregion information
veg_severity_eco <- st_join(veg_severity_planted, cal_eco3)

# Calculate conifer acres and total acres by fire and ecoregion
conifer_summary <- veg_severity_eco %>%
  group_by(fire_id, Ig_Year, US_L3NAME) %>%
  summarize(
    Conifer_Acres = sum(Acres[Veg_Type == "Conifer"], na.rm = TRUE),
    Total_Acres = sum(Acres, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Proportion_Conifer = Conifer_Acres / Total_Acres)

# Summarize by ecoregion
ecoregion_summary <- conifer_summary %>%
  group_by(US_L3NAME) %>%
  summarize(
    Total_Fires = n(),
    Total_Planted_Acres = sum(Total_Acres, na.rm = TRUE),
    Total_Conifer_Acres = sum(Conifer_Acres, na.rm = TRUE),
    Mean_Proportion_Conifer = mean(Proportion_Conifer, na.rm = TRUE),
    Median_Proportion_Conifer = median(Proportion_Conifer, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Overall_Proportion_Conifer = Total_Conifer_Acres / Total_Planted_Acres) %>%
  arrange(desc(Total_Planted_Acres))

# Print the summary
print(ecoregion_summary)

# Save the results
write.csv(ecoregion_summary, "conifer_proportion_by_ecoregion.csv", row.names = FALSE)

# If you want to join this summary back to the cal_eco3 shapefile for mapping
cal_eco3_with_conifer <- cal_eco3 %>%
  left_join(ecoregion_summary, by = "US_L3NAME")

# Save the shapefile
st_write(cal_eco3_with_conifer, "cal_eco3_with_conifer_summary.shp", append = FALSE)