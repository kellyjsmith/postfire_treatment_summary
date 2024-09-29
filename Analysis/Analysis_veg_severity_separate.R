library(terra)
library(sf)
library(dplyr)
library(purrr)

process_activities_raster_area <- function(x, evt_raster_file, evt_csv, mtbs_folder="../Data/Severity") {
  severity_file <- paste0(mtbs_folder, "/mtbs_CA_", x$Ig_Year[1], ".tif")
  severity_raster <- rast(severity_file)
  evt_raster <- rast(evt_raster_file)
  
  activity_vect <- vect(x)
  activity_vect <- project(activity_vect, crs(evt_raster))
  evt_mask <- mask(crop(evt_raster, ext(activity_vect)), activity_vect)
  severity_mask <- mask(crop(severity_raster, ext(activity_vect)), activity_vect)
  
  cell_size_acres <- prod(res(evt_raster)) / 4046.86
  
  veg_summary <- as.data.frame(freq(evt_mask)) %>%
    set_names(c("Layer", "EVT_VALUE", "Count")) %>%
    mutate(Area_acres = Count * cell_size_acres)
  
  severity_summary <- as.data.frame(freq(severity_mask)) %>%
    set_names(c("Layer", "Severity", "Count")) %>%
    mutate(Area_acres = Count * cell_size_acres,
           Severity_Class = c("1" = "Unburned to Low", "2" = "Low", "3" = "Moderate",
                              "4" = "High", "5" = "Increased Greenness", 
                              "6" = "Non-Processing Area")[as.character(Severity)])
  
  veg_summary <- merge(veg_summary, 
                       evt_csv[, c("VALUE", "EVT_NAME", "EVT_PHYS")], 
                       by.x = "EVT_VALUE", by.y = "VALUE",
                       all.x = TRUE) %>%
    mutate(fire_id = x$fire_id[1],
           Ig_Year = x$Ig_Year[1],
           type_labels = x$type_labels[1],
           US_L3NAME = x$US_L3NAME[1],
           min_diff = x$min_diff[1],
           mean_diff = x$mean_diff[1],
           max_diff = x$max_diff[1],
           min_prod = x$min_prod[1],
           mean_prod = x$mean_prod[1],
           max_prod = x$max_prod[1],
           mean_reburns = x$mean_reburns[1],
           min_reburns = x$min_reburns[1],
           max_reburns = x$max_reburns[1])
  
  severity_summary <- severity_summary %>%
    mutate(fire_id = x$fire_id[1],
           Ig_Year = x$Ig_Year[1],
           type_labels = x$type_labels[1],
           US_L3NAME = x$US_L3NAME[1],
           min_diff = x$min_diff[1],
           mean_diff = x$mean_diff[1],
           max_diff = x$max_diff[1],
           min_prod = x$min_prod[1],
           mean_prod = x$mean_prod[1],
           max_prod = x$max_prod[1],
           mean_reburns = x$mean_reburns[1],
           min_reburns = x$min_reburns[1],
           max_reburns = x$max_reburns[1])
  
  list(vegetation = veg_summary, severity = severity_summary)
}

process_all_areas <- function(areas_list, evt_raster_file, evt_csv) {
  map(areas_list,
      ~process_activities_raster_area(., evt_raster_file, evt_csv),
      .progress = TRUE)
}

# Load necessary data
processed_activities <- readRDS("processed_activities_final.RDS")
cal_eco3 <- st_read("../Data/ca_eco_l3.shp") %>%
  st_transform(3310)

evt_raster_file <- "../Data/landfire/LC23_EVT_240.tif"
evt_csv <- read.csv("../Data/landfire/LF23_EVT_240.csv")

# Update net_activities with reburn information and intersect with ecoregions
all_net_activities_eco <- processed_activities %>%
  st_collection_extract("POLYGON") %>%
  st_transform(3310) %>%
  mutate(activity_area = st_area(.)) %>%
  group_by(fire_id, type_labels) %>%
  mutate(
    gross_acres = sum(as.numeric(activity_area)) / 4046.86,
    mean_unit_size = mean(as.numeric(activity_area)) / 4046.86
  ) %>%
  ungroup() %>%
  st_intersection(cal_eco3 %>% select(US_L3NAME)) %>%
  group_by(US_L3NAME, Ig_Year, fire_id, type_labels) %>%
  summarize(
    geometry = st_union(geometry),
    n_plantings = n(),
    net_acres = sum(as.numeric(st_area(geometry))) / 4046.86,
    gross_acres = first(gross_acres),
    mean_unit_size = first(mean_unit_size),
    mean_reburns = mean(reburns, na.rm = TRUE),
    min_reburns = min(reburns, na.rm = TRUE),
    max_reburns = max(reburns, na.rm = TRUE),
    mean_diff = mean(diff_years, na.rm = TRUE),
    min_diff = min(diff_years, na.rm = TRUE),
    max_diff = max(diff_years, na.rm = TRUE),
    mean_prod = mean(as.numeric(PRODUCTIVI), na.rm = TRUE),
    min_prod = min(as.numeric(PRODUCTIVI), na.rm = TRUE),
    max_prod = max(as.numeric(PRODUCTIVI), na.rm = TRUE),
    .groups = "drop"
  )

# Split data
all_net_activities_eco_list <- group_split(all_net_activities_eco, type_labels, Ig_Year, fire_id, US_L3NAME)

# Process activities
vegetation_severity_activities <- process_all_areas(all_net_activities_eco_list, evt_raster_file, evt_csv)

# Separate vegetation and severity summaries
extract_summaries <- function(data, type) {
  map_dfr(data, ~.x[[type]])
}

vegetation_activities <- extract_summaries(vegetation_severity_activities, "vegetation")
severity_activities <- extract_summaries(vegetation_severity_activities, "severity")

# Summarize vegetation data
summarize_vegetation <- function(data) {
  data %>%
    mutate(Veg_Group = case_when(
      EVT_PHYS == "Conifer" ~ "Conifer",
      EVT_PHYS == "Shrubland" ~ "Shrubland",
      EVT_PHYS == "Grassland" ~ "Grassland",
      EVT_PHYS %in% c("Hardwood", "Riparian", "Conifer-Hardwood", "Exotic Tree-Shrub") ~ "Hardwood",
      TRUE ~ "Other"
    )) %>%
    group_by(US_L3NAME, Ig_Year, fire_id, type_labels) %>%
    summarize(
      Conifer_acres = sum(Area_acres[Veg_Group == "Conifer"], na.rm = TRUE),
      Shrubland_acres = sum(Area_acres[Veg_Group == "Shrubland"], na.rm = TRUE),
      Grassland_acres = sum(Area_acres[Veg_Group == "Grassland"], na.rm = TRUE),
      Hardwood_acres = sum(Area_acres[Veg_Group == "Hardwood"], na.rm = TRUE),
      Other_acres = sum(Area_acres[Veg_Group == "Other"], na.rm = TRUE),
      Total_acres = sum(Area_acres, na.rm = TRUE),
      min_diff = first(min_diff),
      mean_diff = first(mean_diff),
      max_diff = first(max_diff),
      min_prod = first(min_prod),
      mean_prod = first(mean_prod),
      max_prod = first(max_prod),
      mean_reburns = first(mean_reburns),
      min_reburns = first(min_reburns),
      max_reburns = first(max_reburns)
    ) %>%
    mutate(across(ends_with("_acres"), ~. / Total_acres * 100, .names = "{.col}_percent"))
}

# Summarize severity data
summarize_severity <- function(data) {
  data %>%
    group_by(US_L3NAME, Ig_Year, fire_id, type_labels) %>%
    summarize(
      Increased_Greenness_acres = sum(Area_acres[Severity_Class == "Increased Greenness"], na.rm = TRUE),
      Unburned_to_Low_acres = sum(Area_acres[Severity_Class == "Unburned to Low"], na.rm = TRUE),
      Low_acres = sum(Area_acres[Severity_Class == "Low"], na.rm = TRUE),
      Moderate_acres = sum(Area_acres[Severity_Class == "Moderate"], na.rm = TRUE),
      High_acres = sum(Area_acres[Severity_Class == "High"], na.rm = TRUE),
      Total_acres = sum(Area_acres, na.rm = TRUE),
      min_diff = first(min_diff),
      mean_diff = first(mean_diff),
      max_diff = first(max_diff),
      min_prod = first(min_prod),
      mean_prod = first(mean_prod),
      max_prod = first(max_prod),
      mean_reburns = first(mean_reburns),
      min_reburns = first(min_reburns),
      max_reburns = first(max_reburns)
    ) %>%
    mutate(across(ends_with("_acres"), ~. / Total_acres * 100, .names = "{.col}_percent"))
}

veg_summary <- summarize_vegetation(vegetation_activities)
severity_summary <- summarize_severity(severity_activities)

# Save results
saveRDS(veg_summary, "veg_summary_eco.RDS")
saveRDS(severity_summary, "severity_summary_eco.RDS")

# Write shapefiles
st_write(veg_summary, "veg_summary_eco.shp", append=FALSE)
st_write(severity_summary, "severity_summary_eco.shp", append=FALSE)
