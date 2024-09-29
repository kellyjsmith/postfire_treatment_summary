#### Zonal Analysis for all SUID ####


# Prepare all activities data
prepare_all_SUID <- function(activities) {
  activities %>%
    st_collection_extract() %>%
    st_cast("MULTIPOLYGON") %>%
    mutate(fire_id = paste(Incid_Name, Event_ID, sep="_")) %>%
    group_by(type_labels, Ig_Year, fire_id, year, SUID) %>%
    summarize(geometry = st_union(geometry)) %>%
    ungroup() %>%
    mutate(all_acres = as.numeric(st_area(geometry))/4046.86)
}

all_SUID <- prepare_all_SUID(assigned_activities)

saveRDS(all_activities, "all_activities.RDS")

# Prepare total burned areas data
prepare_burned_areas <- function(fire_events) {
  fire_events %>%
    mutate(fire_id = paste(Incid_Name, Event_ID, sep="_")) %>%
    group_by(Ig_Year, fire_id) %>%
    summarize(geometry = st_union(geometry)) %>%
    ungroup() %>%
    st_cast("MULTIPOLYGON") %>%
    mutate(all_acres = as.numeric(st_area(.))/4046.86)
}

burned_areas <- prepare_burned_areas(fire_events)

saveRDS(burned_areas, "burned_areas.RDS")

# Function to process activities raster area
process_SUID_raster_area <- function(x, evt_raster_file, evt_csv, mtbs_folder="../Data/Severity") {
  # Load rasters
  evt_raster <- rast(evt_raster_file)
  
  # Prepare and crop data
  activity_vect <- vect(x)
  activity_vect <- project(activity_vect, crs(evt_raster))
  evt_mask <- mask(crop(evt_raster, ext(activity_vect)), activity_vect)
  
  # Summarize vegetation
  veg_summary <- as.data.frame(freq(evt_mask)) %>%
    set_names(c("Layer", "EVT_VALUE", "Count"))
  
  # Merge EVT data and add metadata
  veg_summary <- merge(veg_summary, 
                       evt_csv[, c("VALUE", "EVT_NAME", "EVT_PHYS")], 
                       by.x = "EVT_VALUE", by.y = "VALUE",
                       all.x = TRUE) %>%
    mutate(fire_id = x$fire_id[1],
           Ig_Year = x$Ig_Year[1])
  
  # Add activity-specific fields if they exist
  if ("type_labels" %in% names(x)) veg_summary$type_labels <- x$type_labels[1]
  if ("year" %in% names(x)) veg_summary$year <- x$year[1]
  if ("SUID" %in% names(x)) veg_summary$SUID <- x$SUID[1]
  
  # Calculate proportions by vegetation group
  veg_summary <- veg_summary %>%
    mutate(Veg_Group = case_when(
      EVT_PHYS == "Conifer" ~ "Conifer",
      EVT_PHYS == "Shrubland" ~ "Shrubland",
      EVT_PHYS %in% c("Hardwood", "Riparian", "Conifer-Hardwood", "Exotic Tree-Shrub") ~ "Hardwood",
      TRUE ~ "Other"
    )) %>%
    group_by(across(c("fire_id", "Ig_Year", intersect(c("type_labels", "year", "SUID"), names(.)))), Veg_Group) %>%
    summarise(Group_Count = sum(Count), .groups = "drop") %>%
    mutate(Group_Proportion = Group_Count / sum(Group_Count)) %>%
    select(-Group_Count) %>%
    complete(Veg_Group = c("Conifer", "Shrubland", "Hardwood", "Other"), 
             fill = list(Group_Proportion = 0)) %>%
    pivot_wider(names_from = Veg_Group, values_from = Group_Proportion, values_fill = 0) %>%
    rename_with(~paste0(., "_proportion"), c("Conifer", "Shrubland", "Hardwood", "Other"))
  
  # Add all_acres and calculate actual acres for each vegetation type
  veg_summary$all_acres <- x$all_acres[1]
  veg_summary <- veg_summary %>%
    mutate(
      Conifer_acres = Conifer_proportion * all_acres,
      Shrubland_acres = Shrubland_proportion * all_acres,
      Hardwood_acres = Hardwood_proportion * all_acres,
      Other_acres = Other_proportion * all_acres,
      Conifer_percent = Conifer_proportion * 100,
      Shrubland_percent = Shrubland_proportion * 100,
      Hardwood_percent = Hardwood_proportion * 100,
      Other_percent = Other_proportion * 100
    ) %>%
    select(-ends_with("_proportion"))
  
  veg_summary$geometry <- st_geometry(x)
  st_geometry(veg_summary) <- "geometry"
  
  return(veg_summary)
}

# Process all activities and total burned areas
process_SUID <- function(areas_list, evt_raster_file, evt_csv) {
  map_dfr(areas_list,
          ~process_SUID_raster_area(., evt_raster_file, evt_csv),
          .progress = TRUE) %>%
    st_as_sf()
}

# Split data
all_SUID_list <- group_split(all_SUID, type_labels, Ig_Year, fire_id, year, SUID)
burned_areas_list <- group_split(burned_areas, Ig_Year, fire_id)

# Process activities
vegetation_activities_SUID <- process_SUID(all_activities_list, evt_raster_file, evt_csv)

# Process burned areas
vegetation_burned_SUID <- process_SUID(burned_areas_list, evt_raster_file, evt_csv)

# Remove NAs
vegetation_activities_SUID <- vegetation_activities_SUID %>% drop_na()
vegetation_burned_SUID <- vegetation_burned_SUID %>% drop_na()

# Save outputs
st_write(vegetation_activities_SUID, "vegetation_activities_SUID.shp", append = FALSE)
st_write(vegetation_burned_SUID, "vegetation_burned_SUID.shp", append = FALSE)

vegetation_planted_SUID <- vegetation_activities_SUID %>%
  filter(type_labels == "Initial Planting")
st_write(vegetation_planted_SUID, "vegetation_planted_SUID.shp", append = FALSE)


# Calculate summary statistics
veg_SUID_summary <- vegetation_activities_SUID %>%
  group_by(type_labels, Ig_Year) %>%
  summarize(
    Total_All_Acres = sum(all_acres),
    Conifer_Acres = sum(Conifer_acres),
    Shrubland_Acres = sum(Shrubland_acres),
    Hardwood_Acres = sum(Hardwood_acres),
    Other_Acres = sum(Other_acres),
    .groups = "drop"
  ) %>%
  mutate(
    Conifer_Percent = Conifer_Acres / Total_All_Acres * 100,
    Shrubland_Percent = Shrubland_Acres / Total_All_Acres * 100,
    Hardwood_Percent = Hardwood_Acres / Total_All_Acres * 100,
    Other_Percent = Other_Acres / Total_All_Acres * 100
  )

# Add a check to ensure the percentages sum to 100 (or very close to it)
veg_SUID_summary <- veg_SUID_summary %>%
  mutate(Total_Percent = Conifer_Percent + Shrubland_Percent + Hardwood_Percent + Other_Percent,
         Percent_Check = abs(Total_Percent - 100) < 0.01)

# Save summary statistics
write.csv(veg_SUID_summary, "veg_summary_by_type_year_and_SUID.csv", row.names = FALSE)





#### Sierra Nevada Bioregion Analysis ####

sierra_bound <- st_read("../Data/Sierra_Nevada_Conservancy_Boundary.shp")


# Prepare net activities data
prepare_net_activities_sierra <- function(activities) {
  activities %>%
    st_collection_extract("POLYGON") %>%
    st_intersection(sierra_bound) %>%
    mutate(fire_id = paste(Incid_Name, Ig_Year, sep="_")) %>%
    group_by(type_labels, Ig_Year, fire_id) %>%
    summarize(geometry = st_union(geometry),
              min_diff = min(diff_years, na.rm = TRUE),
              mean_diff = median(diff_years, na.rm = TRUE),
              max_diff = max(diff_years, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(net_acres = as.numeric(st_area(geometry))/4046.86)
}

net_activities_sierra <- prepare_net_activities_sierra(assigned_activities)
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
prepare_burned_areas_sierra <- function(fire_events) {
  fire_events %>%
    st_intersection(sierra_bound) %>%
    mutate(fire_id = paste(Incid_Name, Ig_Year, sep="_")) %>%
    group_by(Ig_Year, fire_id) %>%
    summarize(geometry = st_union(geometry)) %>%
    ungroup() %>%
    st_cast("MULTIPOLYGON") %>%
    mutate(net_acres = as.numeric(st_area(.))/4046.86)
}

burned_areas_sierra <- prepare_burned_areas_sierra(fire_events)

# Split data
net_activities_list_sierra <- group_split(net_activities_sierra, type_labels, Ig_Year, fire_id)
burned_areas_list_sierra <- group_split(burned_areas_sierra, Ig_Year, fire_id)

# Function to process activities raster area (modified to include type_labels)
process_activities_raster_area_sierra <- function(x, evt_raster_file, evt_csv, mtbs_folder="../Data/Severity") {
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
           max_diff = x$max_diff)
  
  severity_summary <- severity_summary %>%
    mutate(fire_id = x$fire_id,
           Ig_Year = x$Ig_Year,
           type_labels = x$type_labels,
           min_diff = x$min_diff,
           mean_diff = x$mean_diff,
           max_diff = x$max_diff)
  
  list(vegetation = veg_summary, severity = severity_summary)
}

# Process all activities and total burned areas
process_areas_sierra <- function(areas_list, evt_raster_file, evt_csv) {
  map(areas_list,
      ~process_activities_raster_area(., evt_raster_file, evt_csv),
      .progress = TRUE)
}

vegetation_severity_activities_sierra <- process_areas_sierra(net_activities_list_sierra, evt_raster_file, evt_csv)
vegetation_severity_burned_sierra <- process_areas_sierra(burned_areas_list_sierra, evt_raster_file, evt_csv)

# Separate vegetation and severity summaries
extract_summaries_sierra <- function(data, type, category) {
  map_dfr(data, ~.x[[type]]) %>%
    mutate(Category = category)
}

# Usage of the corrected function
vegetation_activities_sierra <- extract_summaries(vegetation_severity_activities_sierra, "vegetation", "All Activities")
severity_activities_sierra <- extract_summaries(vegetation_severity_activities_sierra, "severity", "All Activities")
vegetation_burned_sierra <- extract_summaries(vegetation_severity_burned_sierra, "vegetation", "Total Burned")
severity_burned_sierra <- extract_summaries(vegetation_severity_burned_sierra, "severity", "Total Burned")


summarize_severity_sierra <- function(data) {
  
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
      max_diff = if (has_type_labels) first(max_diff) else NA
    ) %>%
    mutate(across(ends_with("_acres"), ~. / Total_acres * 100, .names = "{.col}_percent"))
}

veg_summary_sierra <- bind_rows(
  summarize_vegetation_sierra(vegetation_activities_sierra),
  summarize_vegetation_sierra(vegetation_burned_sierra)
)


severity_summary_sierra <- bind_rows(
  summarize_severity_sierra(severity_activities_sierra),
  summarize_severity_sierra(severity_burned_sierra)
)
