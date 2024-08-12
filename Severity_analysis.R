library("terra")
library("sf")
library("tidyverse")
library("mapview")
# 


# Load assigned activities
assigned_activities <- readRDS("../Output/assigned_activities_new.RDS")

# Subset activities to simplify severity summary
activity_types <- c("Initial Planting", "Fill in or Replant", "TSI - Release", 
                    "Survival Survey", "Stocking Survey", "Certification - Plant")
filtered_activities <- assigned_activities %>%
  filter(type_labels %in% activity_types)

# # Choose a specific year for testing
# test_year <- 2007

# Find the severity file for the test year
severity_files <- list.files("../Data/Severity", pattern = "mtbs_CA_\\d{4}\\.tif$", full.names = TRUE)


# Function to process each year
process_year <- function(year) {
  print(paste("Processing year:", year))
  
  severity_file <- severity_files[grep(paste0("mtbs_CA_", year, "\\.tif$"), severity_files)]
  
  # Load the severity raster for the test year
  severity_raster <- rast(severity_file)
  
  # Filter activities for the test year
  year_activities <- filtered_activities %>%
    filter(Ig_Year == year) %>%
    group_by(type_labels) %>%
    summarise(geometry = st_union(geometry)) %>%
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
    names(severity_summary) <- c("Layer","Severity", "Count")
    severity_summary$Area_acres <- severity_summary$Count * cell_size_acres
    
    all_severities <- data.frame(Severity = 0:6)
    severity_summary <- merge(all_severities, severity_summary, by = "Severity", all.x = TRUE)
    severity_summary$Count[is.na(severity_summary$Layer)] <- 1
    severity_summary$Count[is.na(severity_summary$Count)] <- 0
    severity_summary$Area_acres[is.na(severity_summary$Area_acres)] <- 0
    
    severity_summary$type_labels <- activity$type_labels
    severity_summary$Ig_Year <- year
    
    result[[i]] <- severity_summary
  }

  do.call(rbind, result)
  
}

# Process all years
all_summaries <- list()
for (year in 2000:2022) {
  tryCatch({
    year_summary <- process_year(year)
    if (!is.null(year_summary)) {
      all_summaries[[as.character(year)]] <- year_summary
    }
  }, error = function(e) {
    message(paste("Error processing year", year, ":", e$message))
  })
}

# Combine all summaries
all_summaries_combined <- do.call(rbind, all_summaries)

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
final_summaries <- all_summaries_combined %>%
  na.omit() %>%
  pivot_wider(
    id_cols = c(type_labels, Ig_Year),
    names_from = Severity,
    values_from = Area_acres,
    names_glue = "{class_names[Severity]}_Acres"
  ) %>%
  dplyr::select(type_labels, Ig_Year, ends_with("_Acres"))

# Write results
write.csv(final_summaries, "activity_severity_summaries_test.csv", row.names = FALSE)



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


