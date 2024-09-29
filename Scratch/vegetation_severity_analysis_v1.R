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

# Prepare planted areas data
prepare_planted_areas <- function(activities) {
  activities %>%
    filter(type_labels == "Initial Planting") %>%
    st_collection_extract() %>%
    mutate(fire_id = paste(Incid_Name, Ig_Year, sep="_")) %>%
    group_by(type_labels, Ig_Year, fire_id) %>%
    summarize(geometry = st_union(geometry)) %>%
    ungroup() %>%
    st_cast("MULTIPOLYGON") %>%
    mutate(net_acres = as.numeric(st_area(.))/4046.86)
}

planted_areas <- prepare_planted_areas(assigned_activities)

# Prepare total burned areas data
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
planted_areas_list <- group_split(planted_areas, Ig_Year, fire_id)
burned_areas_list <- group_split(burned_areas, Ig_Year, fire_id)

# Function to process activities raster area
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
           Ig_Year = x$Ig_Year)
  
  severity_summary <- severity_summary %>%
    mutate(fire_id = x$fire_id,
           Ig_Year = x$Ig_Year)
  
  list(vegetation = veg_summary, severity = severity_summary)
}

# Process planted and total burned areas
process_areas <- function(areas_list, evt_raster_file, evt_csv) {
  map(areas_list,
      ~process_activities_raster_area(., evt_raster_file, evt_csv),
      .progress = TRUE) %>%
    map_dfr(~bind_rows(.x$vegetation, .x$severity))
}

vegetation_severity_planted <- process_areas(planted_areas_list, evt_raster_file, evt_csv)
vegetation_severity_burned <- process_areas(burned_areas_list, evt_raster_file, evt_csv)

# Combine planted and total burned data
all_veg_severity <- bind_rows(
  mutate(vegetation_severity_planted, Category = "Planted"),
  mutate(vegetation_severity_burned, Category = "Total Burned")
)

# Summarize data
summarize_veg_severity <- function(data) {
  data %>%
    mutate(Veg_Group = case_when(
      EVT_PHYS == "Conifer" ~ "Conifer",
      EVT_PHYS == "Shrubland" ~ "Shrubland",
      EVT_PHYS %in% c("Hardwood", "Riparian", "Conifer-Hardwood", "Exotic Tree-Shrub") ~ "Hardwood",
      TRUE ~ "Other"
    )) %>%
    group_by(Category, Ig_Year, fire_id) %>%
    summarize(
      Conifer_acres = sum(Area_acres[Veg_Group == "Conifer"], na.rm = TRUE),
      Shrubland_acres = sum(Area_acres[Veg_Group == "Shrubland"], na.rm = TRUE),
      Hardwood_acres = sum(Area_acres[Veg_Group == "Hardwood"], na.rm = TRUE),
      Other_acres = sum(Area_acres[Veg_Group == "Other"], na.rm = TRUE),
      Unburned_to_Low_acres = sum(Area_acres[Severity_Class == "Unburned to Low"], na.rm = TRUE),
      Low_acres = sum(Area_acres[Severity_Class == "Low"], na.rm = TRUE),
      Moderate_acres = sum(Area_acres[Severity_Class == "Moderate"], na.rm = TRUE),
      High_acres = sum(Area_acres[Severity_Class == "High"], na.rm = TRUE),
      Total_acres = sum(Area_acres, na.rm = TRUE)
    ) %>%
    mutate(across(ends_with("_acres"), ~. / Total_acres * 100, .names = "{.col}_percent"))
}

veg_severity_summary <- summarize_veg_severity(all_veg_severity)

# Save results
write.csv(veg_severity_summary, "veg_severity_summary.csv", row.names = FALSE)
saveRDS(veg_severity_summary, "veg_severity_summary.RDS")



library(ggplot2)
library(scales)
library(dplyr)

# Prepare data for visualization
viz_data <- veg_severity_summary %>%
  filter(Category == "Planted") %>%
  group_by(Ig_Year) %>%
  summarize(
    High_Severity_Percent = mean(High_acres_percent, na.rm = TRUE),
    Conifer_Percent = sum(Conifer_acres, na.rm = TRUE) / sum(Total_acres, na.rm = TRUE) * 100,
    Shrubland_Percent = sum(Shrubland_acres, na.rm = TRUE) / sum(Total_acres, na.rm = TRUE) * 100,
    Hardwood_Percent = sum(Hardwood_acres, na.rm = TRUE) / sum(Total_acres, na.rm = TRUE) * 100,
    Other_Percent = sum(Other_acres, na.rm = TRUE) / sum(Total_acres, na.rm = TRUE) * 100,
    Total_Acres = sum(Total_acres, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(Conifer_Percent, Shrubland_Percent, Hardwood_Percent, Other_Percent),
               names_to = "Veg_Group", values_to = "Percent")

# Create the plot
ggplot(viz_data, aes(x = Ig_Year)) +
  geom_col(aes(y = Percent, fill = Veg_Group), position = "stack") +
  geom_line(aes(y = High_Severity_Percent, color = "High Severity %"), size = 1) +
  geom_point(aes(y = High_Severity_Percent, color = "High Severity %"), size = 3) +
  scale_y_continuous(name = "Vegetation Composition (%)", 
                     labels = function(x) paste0(x, "%"),
                     sec.axis = sec_axis(~., name = "High Severity %", labels = function(x) paste0(x, "%"))) +
  scale_x_continuous(breaks = seq(2000, 2022, by = 2)) +
  scale_color_manual(values = c("High Severity %" = "red")) +
  scale_fill_manual(values = c("Conifer_Percent" = "darkgreen", 
                               "Shrubland_Percent" = "tan", 
                               "Hardwood_Percent" = "purple",
                               "Other_Percent" = "grey")) +
  labs(title = "High Severity Burn % and Vegetation Composition by Year (Planted Areas)",
       subtitle = paste("Total Planted Acres:", format(sum(viz_data$Total_Acres), big.mark = ",")),
       x = "Ignition Year",
       color = "",
       fill = "Vegetation Group") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave("severity_vegetation_composition_planted.png", width = 12, height = 8, dpi = 300)

# Print summary statistics
print(summary(viz_data))
print(viz_data %>% group_by(Ig_Year) %>% summarize(total_percent = sum(Percent)))

# Print summary statistics to check for any anomalies
print(summary(viz_data))
print(viz_data %>% group_by(Category) %>% summarize(max_severity = max(High_Severity_Percent)))