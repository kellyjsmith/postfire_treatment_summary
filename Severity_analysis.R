library("terra")
library("sf")
library("tidyverse")
library("mapview")


# Summarize severity by activity ####

# Load assigned activities
assigned_activities <- readRDS("../Output/assigned_activities_new.RDS")
fact_fires <- readRDS("../Output/facts_fires_new.RDS")

# Subset activities to simplify severity summary
activity_types <- c("Initial Planting", "Fill in or Replant", "TSI - Release", 
                    "Survival Survey", "Stocking Survey", "Certification - Plant")

filtered_activities <- assigned_activities %>%
  # filter(type_labels %in% activity_types)
  filter(type_labels == "Initial Planting")

# Find all severity files in the directory
severity_files <- list.files("../Data/Severity", pattern = "mtbs_CA_\\d{4}\\.tif$", full.names = TRUE)

# Function to process each year
process_year_activities <- function(year) {
  print(paste("Processing year:", year))
  print(Sys.time())
  
  severity_file <- severity_files[grep(paste0("mtbs_CA_", year, "\\.tif$"), severity_files)]
  
  # Load the severity raster for the processing year
  severity_raster <- rast(severity_file)
  
  # Filter activities for the processing year
  year_activities <- filtered_activities %>%
    filter(Ig_Year == year) %>%
    group_by(type_labels) %>%
    summarize(geometry = st_union(geometry)) %>%
    ungroup()
  
  # Ensure CRS match
  year_activities <- st_transform(year_activities, crs(severity_raster))
  
  # Process each activity type
  result <- list()
  for (i in 1:nrow(year_activities)) {
    activity <- year_activities[i, ]
    activity_vect <- vect(activity)
    
    activity_severity <- crop(severity_raster, ext(activity_vect))
    activity_severity <- mask(activity_severity, activity_vect)
    
    # Calculate area in acres (30m resolution)
    cell_size_acres <- 30 * 30 / 4046.86
    
    severity_summary <- as.data.frame(freq(activity_severity))
    names(severity_summary) <- c("Layer", "Severity", "Count")
    severity_summary$Area_acres <- severity_summary$Count * cell_size_acres
    
    all_severities <- data.frame(Severity = 0:6)
    severity_summary <- merge(all_severities, severity_summary, by = "Severity", all.x = TRUE)
    severity_summary$Count[is.na(severity_summary$Count)] <- 0
    severity_summary$Area_acres[is.na(severity_summary$Area_acres)] <- 0
    
    severity_summary$type_labels <- activity$type_labels
    severity_summary$Ig_Year <- year
    
    result[[i]] <- severity_summary
  }
  
  do.call(rbind, result)
}

# Process all years
activity_summaries <- list()
for (year in 2007:2007) {
  tryCatch({
    year_summary <- process_year_activities(year)
    if (!is.null(year_summary)) {
      activity_summaries[[as.character(year)]] <- year_summary
    }
  }, error = function(e) {
    message(paste("Error processing year", year, ":", e$message))
  })
}


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

# Load the fire events shapefile
fire_events <- st_read("../Data/Severity/r5_fires_00_22.shp")

# Find all severity files in the directory
severity_files <- list.files("../Data/Severity", pattern = "mtbs_CA_\\d{4}\\.tif$", full.names = TRUE)

# Function to process each year
process_year <- function(year) {
  print(paste("Processing year:", year))
  
  severity_file <- severity_files[grep(paste0("mtbs_CA_", year, "\\.tif$"), severity_files)]
  
  # Load the severity raster for the processing year
  severity_raster <- rast(severity_file)
  
  # Filter fire events for the processing year
  year_events <- fire_events %>%
    filter(Ig_Year == year) %>%
    group_by(Event_ID, Incid_Name) %>%
    summarise(total_fire_acres = BurnBndAc) %>%
    ungroup()
  
  # Ensure CRS match
  year_events <- st_transform(year_events, crs(severity_raster))
  
  # Process each event
  result <- list()
  for (i in 1:nrow(year_events)) {
    event <- year_events[i, ]
    event_vect <- vect(event)
    
    event_severity <- crop(severity_raster, ext(event_vect))
    event_severity <- mask(event_severity, event_vect)
    
    # Calculate area in acres (30m resolution)
    cell_size_acres <- 30 * 30 / 4046.86
    
    severity_summary <- as.data.frame(freq(event_severity))
    names(severity_summary) <- c("Layer", "Severity", "Count")
    severity_summary$Area_acres <- severity_summary$Count * cell_size_acres
    
    all_severities <- data.frame(Severity = 0:6)
    severity_summary <- merge(all_severities, severity_summary, by = "Severity", all.x = TRUE)
    severity_summary$Count[is.na(severity_summary$Count)] <- 0
    severity_summary$Area_acres[is.na(severity_summary$Area_acres)] <- 0
    
    severity_summary$Layer <- year
    severity_summary$Event_ID <- event$Event_ID
    severity_summary$Incid_Name <- event$Incid_Name
    severity_summary$Ig_Year <- year
    severity_summary$total_fire_acres <- event$total_fire_acres
    
    result[[i]] <- severity_summary
  }
  
  do.call(rbind, result)
}

# Process all years
fire_summaries <- list()
for (year in 2000:2022) {
  tryCatch({
    year_summary <- process_year(year)
    if (!is.null(year_summary)) {
      fire_summaries[[as.character(year)]] <- year_summary
    }
  }, error = function(e) {
    message(paste("Error processing year", year, ":", e$message))
  })
}

# Combine all summaries
fire_summaries_combined <- do.call(rbind, fire_summaries)

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

# Reshape the data and rename classes
final_fire_summaries <- fire_summaries_combined %>%
  na.omit() %>%
  pivot_wider(
    id_cols = c(Ig_Year, Event_ID, Incid_Name, total_fire_acres),
    names_from = Severity,
    values_from = Area_acres,
    # names_glue = "{class_names[Severity]}_Acres"
    names_prefix = "class_"
  ) %>%
  rename_with(~ paste0(class_names[sub("class_", "", .)], "_acres"), starts_with("class_")) %>%
  mutate(across(4:11, round, 2)) %>%
  group_by(Ig_Year, Event_ID, Incid_Name) %>%
  mutate(total_severity = sum(Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres, Increased_Greenness_acres)) %>%
  mutate(Percent_high = High_acres / total_severity) %>%
  ungroup()

severity_year_summaries = final_fire_summaries %>%
  group_by(Ig_Year) %>%
  summarize(
    Total_burned_acres = sum(total_fire_acres),
    Total_severity_acres = sum(total_severity),
    Severity_diff = Total_burned_acres - Total_severity_acres,
    Severity_percent = Total_severity_acres / Total_burned_acres,
    Unburned_to_Low_acres = sum(Unburned_to_Low_acres),
    Low_acres = sum(Low_acres),
    Moderate_acres = sum(Moderate_acres),
    High_acres = sum(High_acres),
    Increased_Greenness_acres = sum(Increased_Greenness_acres),
    Percent_high = High_acres / Total_severity_acres)


# Write results
write.csv(final_fire_summaries, "fire_event_severity_summaries.csv", row.names = FALSE)
write.csv(severity_year_summaries, "severity_year_summaries.csv", row.names = FALSE)


# Paco's code below

# severity_files <- list.files("../Data/Severity",".tif$",full.names=TRUE)
# severity <- vrt(severity_files,"../Data/Severity/severity.vrt",overwrite=TRUE)

# # fires california
# severity <- rast("../Data/Severity/severity.vrt")
# xr <- rast(severity_files[1])
# names(severity)<-names(xr)
# severity_fires <- vect("../Data/Severity/r5_fires_00_22.shp")
# severity_fires <- project(severity_fires,crs(severity))
# summaries <- map_dfr(2000:2022,function(year){
#   print(year)
#   severity_fires_year <- severity_fires[severity_fires$Ig_Year==year,]
#   severity_year <- severity[[paste0("mtbs_CA_",year,"_Severity")]]
#   map_dfr(1:nrow(severity_fires_year),function(x){
#     print(x)
#     fire_year_x <- severity_fires_year[x,]
#     fire_year_x_severity <- crop(severity_year,ext(fire_year_x))
#     fire_year_x_severity <- mask(fire_year_x_severity,fire_year_x)
#     levels(fire_year_x_severity)<- data.frame(id=c(0:7),severity=c(0:7))
#     res <- data.frame(table(fire_year_x_severity[]))
#     names(res)[1]<-"Severity"
#     res[,"Event_ID"]<-fire_year_x$Event_ID[1]
#     res
#   })
# })
# write.csv(summaries,"summaries_severity_fires_EE.csv",row.names=FALSE)
# 
# 
# assigned_activities <- readRDS("assigned_activities.RDS")
# assigned_activities$intersection_id
# # assigned_activities <-svc(assigned_activities)
# # assigned_activities <- st_as_sf(assigned_activities)
# summaries <- map_dfr(unique(assigned_activities$Ig_Year),function(year){
#   print(year)
#   activities_year <- assigned_activities[assigned_activities$Ig_Year==year,]
#   severity_year <- severity[[paste0("mtbs_CONUS_",year,"_Severity")]]
#   map_dfr(1:nrow(activities_year),function(x){
#     print(x)
#     activitiy_year_x <- activities_year[x,]
#     if(st_geometry(activitiy_year_x)=="GEOMETRYCOLLECTION"){
#       a<-st_cast(activitiy_year_x)
#       a <- a[st_dimension(a)==2,]
#       activitiy_year_x<- st_cast(a,"MULTIPOLYGON")
#     }
#     activitiy_year_x_severity <- crop(severity_year,ext(activitiy_year_x))
#     activitiy_year_x_severity <- mask(activitiy_year_x_severity,activitiy_year_x)
#     levels(activitiy_year_x_severity)<- data.frame(id=c(0:7),severity=c(0:7))
#     res <- data.frame(table(activitiy_year_x_severity[]))
#     names(res)[1]<-"Severity"
#     res[,"intersection_id"]<-activitiy_year_x$intersection_id[1]
#     res
#   })
# })
# write.csv(summaries,"summaries_severity_activities.csv",row.names=FALSE)
# 


