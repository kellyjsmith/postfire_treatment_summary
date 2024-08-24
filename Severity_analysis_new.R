library("terra")
library("sf")
library("tidyverse")
library("mapview")
library("purrr")
library("doParallel")


# Summarize severity by activity ####
# Load assigned activities
assigned_activities <- readRDS("../../Output/assigned_activities_new.RDS")
facts_fires <- readRDS("../Output/facts_fires_new.RDS")
# Load the fire events shapefile
fire_events <- st_read("../Data/Severity/R5_fires_00_22.shp")
fire_events$unique_id <- paste(fire_events$Incid_Name, fire_events$Ig_Year, sep="_")
assigned_activities2 <- st_collection_extract(assigned_activities,"POLYGON")
sum(st_area(assigned_activities))-sum(st_area(assigned_activities2))

# Create fire_id in assigned_activities2
assigned_activities2$fire_id <- paste(assigned_activities2$Incid_Name, assigned_activities2$Ig_Year, sep="_")

# Calculate median diff_years for each grouping
median_diff_years <- assigned_activities2 %>%
  group_by(type_labels, Ig_Year, fire_id) %>%
  summarize(median_diff_years = median(diff_years, na.rm = TRUE)) %>%
  ungroup() %>% st_drop_geometry()

area_by_type_year_fire <-  assigned_activities2 %>% 
  group_by(type_labels, Ig_Year, fire_id) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  ungroup()

# Join median_diff_years to area_by_type_year_fire
area_by_type_year_fire <- left_join(area_by_type_year_fire, median_diff_years, 
                                    by = c("type_labels", "Ig_Year", "fire_id"))

area_by_type_year = area_by_type_year_fire %>%
  group_by(type_labels, Ig_Year) %>%
  summarize(geometry = sum(st_area(geometry)),
            median_diff_years = median(median_diff_years, na.rm = TRUE)) %>% 
  ungroup()

area_by_type_year <- st_cast(area_by_type_year,"MULTIPOLYGON")
area_by_type_year$area <- as.numeric(st_area(area_by_type_year))/4046.85
area_by_type_year_list <- group_split(area_by_type_year,Ig_Year,type_labels)

# Function to process each year
process_year_activities <- function(x, mtbs_folder="../Data/Severity") {
  severity_file <- paste0(mtbs_folder, "/mtbs_CA_", x$Ig_Year, ".tif")
  print(x)
  # Load the severity raster for the processing year
  severity_raster <- rast(severity_file)
  
  # Vectorize the sf object for the processing year/type
  activity_vect <- vect(x)
  
  # Ensure CRS match
  activity_vect <- project(activity_vect, crs(severity_raster))
  
  # Perform zonal calculations
  activity_severity <- crop(severity_raster, ext(activity_vect))
  activity_severity <- mask(activity_severity, activity_vect, touches = FALSE)
  
  cell_size_acres <- 30 * 30 / 4046.86
  
  severity_summary <- as.data.frame(freq(activity_severity))
  names(severity_summary) <- c("Layer", "Severity", "Count")
  severity_summary$Area_acres <- severity_summary$Count * cell_size_acres
  
  all_severities <- data.frame(Severity = 0:6)
  severity_summary <- merge(all_severities, severity_summary, by = "Severity", all.x = TRUE)
  severity_summary$Count[is.na(severity_summary$Count)] <- 0
  severity_summary$Area_acres[is.na(severity_summary$Area_acres)] <- 0
  severity_summary$Layer <- 1
  
  severity_summary2<-pivot_wider(severity_summary,
                                 id_cols = "Layer",
                                 names_from = Severity,
                                 values_from = Area_acres,
                                 names_prefix = "class_"
  )
  
  # Define new class names
  class_names <- c(
    "0" = "Background",
    "1" = "Unburned_to_Low",
    "2" = "Low",
    "3" = "Moderate",
    "4" = "High",
    "5" = "Increased_Greenness",
    "6" = "Non_Mapping_Area"
  )
  
  severity_summary2 = severity_summary2 %>%
    rename_with(~ paste0(class_names[sub("class_", "", .)], "_acres"), starts_with("class_"))
  
  severity_summary2$total_area <- sum(severity_summary2[1,-1])
  res <- cbind(x,severity_summary2)
  return(res)
}

print("Starting intersection")
print(Sys.time())
loaded  <-  .packages()
cl <- makeCluster(10)
registerDoParallel(cl)
activities_by_severity  <-  foreach(
  x  =  area_by_type_year_list,
  .packages  =  loaded,
  .combine = rbind
)  %dopar%  {
  process_year_activities(x,mtbs_folder="../Data/Severity")
}
stopCluster(cl)


write.csv(st_drop_geometry(activities_by_severity),"Activities_severity.csv",row.names=FALSE)

saveRDS(activities_by_severity, "activities_by_severity.RDS")

activities_by_severity = readRDS("activities_by_severity.RDS")

area_by_type_year_df<-merge(class_names,area_by_type_year_df)

# Combine all summaries
activity_summaries_combined <- do.call(rbind, activity_summaries)

# Reshape the data and rename classes
final_summaries <- activity_summaries_combined %>%
  group_by(type_labels, Ig_Year, Severity) %>%
  summarize(Count = sum(Count), Area_acres = sum(Area_acres)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c(type_labels, Ig_Year),
    names_from = Severity,
    values_from = Area_acres,
    names_prefix = "class_"
  ) %>%
  rename_with(~ paste0(class_names[sub("class_", "", .)], "_acres"), starts_with("class_"))

# Write results
write.csv(final_summaries, "activity_severity_summaries.csv", row.names = FALSE)




# Summarize severity by fire ####


# Find all severity files in the directory
fire_area_by_type_year<-  fire_events%>% 
  group_by(unique_id,Ig_Year) %>% 
  dplyr::summarize(geometry=st_union(geometry)) %>% ungroup()
fire_area_by_type_year <- st_cast(fire_area_by_type_year,"MULTIPOLYGON")

fire_area_by_type_year$area <- as.numeric(st_area(fire_area_by_type_year))/4046.85

mtbs_folder<-"../Data/Severity"
severity_file <- paste0(mtbs_folder,"/mtbs_CA_2007.tif")
fire_area_by_type_year<-st_transform(fire_area_by_type_year,crs(rast(severity_file)))

fire_area_by_type_year_list <- group_split(fire_area_by_type_year,Ig_Year,unique_id)

print("Starting intersection")
print(Sys.time())
loaded  <-  .packages()
cl <- makeCluster(10)
registerDoParallel(cl)
fires_by_severity  <-  foreach(
  x  =  fire_area_by_type_year_list,
  .packages  =  loaded,
  .combine = rbind
)  %dopar%  {
  process_year_activities(x,mtbs_folder="../Data/Severity")
}
stopCluster(cl)

severity_by_year = fires_by_severity %>%
  group_by(Ig_Year) %>%
  summarize(
    Unburned_to_Low_acres = sum(Unburned_to_Low_acres),
    Low_acres = sum(Low_acres),
    Moderate_acres = sum(Moderate_acres),
    High_acres = sum(High_acres),
    Increased_Greenness_acres = sum(Increased_Greenness_acres),
    Non_Mapping_Area_acres = sum(Non_Mapping_Area_acres),
    Total_severity_acres = sum(total_area),
    Percent_high = High_acres / Total_severity_acres)

write.csv(st_drop_geometry(fires_by_severity),"Fires_severity.csv",row.names=FALSE)

saveRDS(severity_by_year, "severity_by_year.RDS")

severity_by_year = readRDS("severity_by_year.RDS")
