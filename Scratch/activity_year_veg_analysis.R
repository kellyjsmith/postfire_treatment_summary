library(purrr)
library(dplyr)
library(terra)
library(tidyr)
library(sf)


# Load data
assigned_activities <- readRDS("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output/assigned_activities_new.RDS")
fire_events <- st_read("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output/R5_fires_00_21.shp")
evt_raster_file <- "C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Data/landfire/LC23_EVT_240.tif"
evt_csv <- read.csv("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Data/landfire/LF23_EVT_240.csv")

setwd("E:/kelly/output")

# Modify prepare_net_activities function
prepare_net_activity_year <- function(activities) {
  activities %>%
    filter(type_labels == "Initial Planting") %>%
    st_collection_extract("POLYGON") %>%
    mutate(fire_id = paste(Incid_Name, Event_ID, sep="_")) %>%
    group_by(type_labels, year) %>%  # Group by activity type and year
    summarize(geometry = st_union(geometry)) %>%
    ungroup() %>%
    st_cast("MULTIPOLYGON") %>%
    mutate(net_acres = as.numeric(st_area(geometry))/4046.86)
}

net_activity_year <- prepare_net_activity_year(assigned_activities)



# Modify the process_activities_raster_area function with improved error handling
process_activity_year_raster_area <- function(x, evt_raster_file, evt_csv) {
  tryCatch({
    evt_raster <- rast(evt_raster_file)
    
    # Convert sf object to SpatVector
    activity_vect <- vect(x)
    
    # Print diagnostic information
    cat("Processing year:", x$year[1], "\n")
    cat("Extent of activity vector:", paste(as.vector(ext(activity_vect)), collapse=", "), "\n")
    cat("Extent of EVT raster:", paste(as.vector(ext(evt_raster)), collapse=", "), "\n")
    
    # Project activity vector to match EVT raster CRS
    activity_vect <- project(activity_vect, crs(evt_raster))
    
    evt_crop <- crop(evt_raster, ext(activity_vect))
    if (is.null(evt_crop)) {
      cat("Cropped EVT raster is NULL for year", x$year[1], "\n")
      return(NULL)
    }
    
    evt_mask <- mask(evt_crop, activity_vect)
    if (is.null(evt_mask)) {
      cat("Masked EVT raster is NULL for year", x$year[1], "\n")
      return(NULL)
    }
    
    veg_summary <- as.data.frame(freq(evt_mask)) %>%
      set_names(c("Layer", "EVT_VALUE", "Count"))
    
    veg_summary <- merge(veg_summary, 
                         evt_csv[, c("VALUE", "EVT_NAME", "EVT_PHYS")], 
                         by.x = "EVT_VALUE", by.y = "VALUE",
                         all.x = TRUE) %>%
      mutate(year = x$year[1])
    
    veg_summary <- veg_summary %>%
      mutate(Veg_Group = case_when(
        EVT_PHYS == "Conifer" ~ "Conifer",
        EVT_PHYS == "Shrubland" ~ "Shrubland",
        EVT_PHYS == "Grassland" ~ "Grassland",
        EVT_PHYS %in% c("Hardwood", "Riparian", "Conifer-Hardwood", "Exotic Tree-Shrub") ~ "Hardwood",
        TRUE ~ "Other"
      )) %>%
      group_by(year, Veg_Group) %>%
      summarise(Group_Count = sum(Count), .groups = "drop") %>%
      mutate(Group_Proportion = Group_Count / sum(Group_Count)) %>%
      select(-Group_Count) %>%
      complete(Veg_Group = c("Conifer", "Shrubland", "Grassland", "Hardwood", "Other"), 
               fill = list(Group_Proportion = 0)) %>%
      pivot_wider(names_from = Veg_Group, values_from = Group_Proportion, values_fill = 0) %>%
      rename_with(~paste0(., "_proportion"), c("Conifer", "Shrubland", "Grassland", "Hardwood", "Other"))
    
    veg_summary$net_acres <- sum(x$net_acres)
    veg_summary <- veg_summary %>%
      mutate(
        Conifer_acres = Conifer_proportion * net_acres,
        Shrubland_acres = Shrubland_proportion * net_acres,
        Grassland_acres = Grassland_proportion * net_acres,
        Hardwood_acres = Hardwood_proportion * net_acres,
        Other_acres = Other_proportion * net_acres,
        Conifer_percent = Conifer_proportion * 100,
        Shrubland_percent = Shrubland_proportion * 100,
        Grassland_percent = Grassland_proportion * 100,
        Hardwood_percent = Hardwood_proportion * 100,
        Other_percent = Other_proportion * 100
      ) %>%
      select(-ends_with("_proportion"))
    
    return(veg_summary)
  }, error = function(e) {
    cat("Error processing year", x$year[1], ":", conditionMessage(e), "\n")
    cat("Class of x:", class(x), "\n")
    cat("Columns of x:", paste(names(x), collapse=", "), "\n")
    return(NULL)
  })
}

# Process net activities one by one
vegetation_activity_year <- list()
for (i in seq_along(net_activity_year_list)) {
  result <- process_activity_year_raster_area(net_activity_year_list[[i]], evt_raster_file, evt_csv)
  if (!is.null(result)) {
    vegetation_activity_year[[i]] <- result
  }
}

# Combine results
vegetation_activity_year <- bind_rows(vegetation_activity_year)

# If there are still issues, you can identify which years were processed successfully
successful_years <- unique(vegetation_activity_year$year)
cat("Successfully processed years:", paste(successful_years, collapse = ", "), "\n")

# Identify years that failed
all_years <- unique(unlist(lapply(net_activity_year_list, function(x) x$year[1])))
failed_years <- setdiff(all_years, successful_years)
cat("Failed to process years:", paste(failed_years, collapse = ", "), "\n")


# Summarize vegetation data
summarize_vegetation_activity_year <- function(data) {
  data %>%
    group_by(year, type_labels) %>%
    summarize(
      Conifer_acres = sum(Conifer_acres, na.rm = TRUE),
      Shrubland_acres = sum(Shrubland_acres, na.rm = TRUE),
      Grassland_acres = sum(Grassland_acres, na.rm = TRUE),
      Hardwood_acres = sum(Hardwood_acres, na.rm = TRUE),
      Other_acres = sum(Other_acres, na.rm = TRUE),
      Total_acres = sum(net_acres, na.rm = TRUE)
    ) %>%
    mutate(across(ends_with("_acres"), ~. / Total_acres * 100, .names = "{.col}_percent"))
}

veg_summary_by_activity_year <- summarize_vegetation_activity_year(vegetation_activity_year)

# Save results
write.csv(veg_summary_by_activity_year, "veg_summary_by_activity_year.csv", row.names = FALSE)



library(dplyr)
library(tidyr)

# Filter for planted areas and calculate percentages
planted_veg_summary <- veg_summary_by_activity_year %>%
  filter(type_labels == "Initial Planting") %>%
  mutate(
    Conifer_percent = Conifer_acres / Total_acres * 100,
    Shrub_Hardwood_percent = (Shrubland_acres + Hardwood_acres) / Total_acres * 100,
    Other_percent = (Grassland_acres + Other_acres) / Total_acres * 100
  ) %>%
  select(year, Conifer_percent, Shrub_Hardwood_percent, Other_percent, Total_acres)

# Round percentages to two decimal places
planted_veg_summary <- planted_veg_summary %>%
  mutate(across(ends_with("percent"), ~round(., 2)))

# Table 1: Percentages for each year
yearly_summary <- planted_veg_summary %>%
  arrange(year)

# Save Table 1
write.csv(yearly_summary, "planted_veg_summary_yearly.csv", row.names = FALSE)

# Table 2: Percentages for 5-year groups
five_year_summary <- planted_veg_summary %>%
  mutate(year_group = cut(year, 
                          breaks = seq(2000, 2025, by = 5),
                          labels = paste(seq(2000, 2020, by = 5), 
                                         seq(2004, 2024, by = 5), 
                                         sep = "-"),
                          include.lowest = TRUE)) %>%
  group_by(year_group) %>%
  summarize(
    Conifer_percent = weighted.mean(Conifer_percent, Total_acres),
    Shrub_Hardwood_percent = weighted.mean(Shrub_Hardwood_percent, Total_acres),
    Other_percent = weighted.mean(Other_percent, Total_acres),
    Total_acres = sum(Total_acres)
  ) %>%
  mutate(across(ends_with("percent"), ~round(., 2)))

# Save Table 2
write.csv(five_year_summary, "planted_veg_summary_5year.csv", row.names = FALSE)

# Print the tables
print("Yearly Summary:")
print(yearly_summary)

print("\n5-Year Summary:")
print(five_year_summary)