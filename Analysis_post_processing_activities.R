library(dplyr)
library(sf)

facts_fires <- readRDS("facts_fires_new.RDS")

post_process_processed_activities <- function(assigned_activities, fires) {
  
  # Filter out NAs
  assigned_activities <- filter(assigned_activities, !is.na(assigned_fire))
  
  # Find intersections between assigned_activities and fires
  intersections <- st_intersects(assigned_activities, fires)
  
  # Function to process each activity
  process_activity <- function(activity, intersecting_fires) {
    if (length(intersecting_fires) <= 1) {
      activity$reburns <- 0
      return(activity)
    }
    
    # Get the relevant fires
    relevant_fires <- fires[intersecting_fires, ]
    
    # Calculate time differences
    time_diffs <- activity$year - relevant_fires$Ig_Year
    
    # Check if the current assignment has a time difference > 11 years
    current_diff <- activity$year - activity$assigned_fire_year
    
    if (current_diff > 11) {
      # Find the most recent fire that's not after the activity
      valid_fires <- time_diffs >= 0
      if (any(valid_fires)) {
        most_recent_fire <- relevant_fires[valid_fires, ][which.max(relevant_fires$Ig_Year[valid_fires]), ]
        
        # Update fire attributes
        activity$assigned_fire <- most_recent_fire$Event_ID
        activity$assigned_fire_year <- most_recent_fire$Ig_Year
        activity$Incid_Name <- most_recent_fire$Incid_Name
        activity$Ig_Date <- most_recent_fire$Ig_Date
        activity$flag <- sum(valid_fires)  # Update flag to indicate number of valid intersections
      }
    }
    
    # Add reburns column
    activity$reburns <- length(intersecting_fires) - 1
    
    return(activity)
  }
  
  # Apply the processing function to each activity
  processed_activities <- assigned_activities %>%
    mutate(row_id = row_number()) %>%
    group_by(row_id) %>%
    group_modify(~process_activity(., intersections[[.y$row_id]])) %>%
    ungroup() %>%
    select(-row_id)
  
  # Ensure the output is an sf object
  processed_activities <- st_as_sf(processed_activities)
  
  return(processed_activities)
}



# Workflow after the function

# Run Post-processing
processed_activities <- post_process_processed_activities(
  facts_fires$assigned_activities, facts_fires$fires)

# Apply initial cleaning steps
processed_activities <- processed_activities %>%
  filter(as.numeric(st_area(geometry)) > 0,
         !is.na(st_area(geometry)),
         !st_geometry_type(.) == "POINT")

# Save the initially processed activities
saveRDS(processed_activities, "processed_activities_initial.RDS")

# Identify columns to keep from the original activities data
keep_columns <- c(keep, "Event_ID", "activity_area", "facts_polygon_id", "assigned_fire", "reburns")

# Subset columns from processed activities
processed_activities <- processed_activities[, keep_columns]

# Identify columns to merge from the fires data
fire_columns <- c("Event_ID", "Incid_Name", "Ig_Date", "Ig_Year", "fire_area")

# Prepare fires data with renamed columns to avoid conflicts
fires_data <- st_drop_geometry(facts_fires$fires)[, fire_columns] %>%
  rename(
    fire_Event_ID = Event_ID,
    fire_Incid_Name = Incid_Name,
    fire_Ig_Date = Ig_Date,
    fire_Ig_Year = Ig_Year
  )

# Merge with fires data, updating all relevant fire information
processed_activities <- processed_activities %>%
  left_join(fires_data, by = c("assigned_fire" = "fire_Event_ID"))

# Update the relevant columns with fire data
processed_activities <- processed_activities %>%
  mutate(
    Event_ID = assigned_fire,
    Incid_Name = fire_Incid_Name,
    Ig_Date = fire_Ig_Date,
    Ig_Year = fire_Ig_Year,
    fire_area = fire_area
  ) %>%
  select(-starts_with("fire_"), -assigned_fire)

# Now calculate diff_years and activity_fire_area using the updated fire information
processed_activities <- processed_activities %>%
  mutate(
    diff_years = year - Ig_Year,
    activity_fire_area = st_area(geometry)
  )

# Final cleaning steps (if needed)
processed_activities <- processed_activities %>%
  filter(
    !is.na(activity_fire_area),
    as.numeric(activity_fire_area) > 0
  )

# CREATE ACTIVITY TYPE
processed_activities$ACTIVITY_TYPE <- NA

types = c("Certified_Planted","Certified_TSI_Release","Harvest_NonSalv","Harvest_Salvage",
          "Need_by_Failure","Need_by_Fire","Need_by_Harvest","Plant","Replant",
          "SitePrep_Chem","SitePrep_NonChem","Stand_Exam", "Survey_Other",
          "Survey_Stocking","Survey_Survival","TSI_Release","TSI_Thin")

for (i in types) {
  categories <- eval(parse(text=i))
  
  processed_activities$ACTIVITY_TYPE <- ifelse(processed_activities$ACTIVITY %in% categories, i, processed_activities$ACTIVITY_TYPE)
}

# Separate Release types
processed_activities <- processed_activities %>%
  mutate(ACTIVITY_TYPE = case_when(
    ACTIVITY_TYPE == "TSI_Release" & METHOD == "Chemical" ~ "Release_Chem",
    ACTIVITY_TYPE == "TSI_Release" & METHOD != "Chemical" ~ "Release_NonChem",
    TRUE ~ ACTIVITY_TYPE
  ))


# Update with new labels
new_labels <- data.frame(
  ACTIVITY_TYPE = c("Certified_Planted", "Certified_TSI_Release","Harvest_NonSalv", "Harvest_Salvage", 
                    "Need_by_Failure", "Need_by_Fire", "Need_by_Harvest", "Plant", 
                    "Replant", "Stand_Exam", "Survey_Stocking", "Survey_Survival", 
                    "Survey_Other", "TSI_Thin", "Release_Chem", "Release_NonChem",
                    "SitePrep_Chem", "SitePrep_NonChem"),
  type_labels = c("Certification - Plant", "Certification - Release", "Harvest - Non-Salvage", "Harvest - Salvage", 
                  "Reforest. Need - Failure", "Reforest. Need - Fire", "Reforest. Need - Harvest", 
                  "Initial Planting", "Fill-in or Replant", "Stand Exam", "Stocking Survey", 
                  "Survival Survey", "Survey - Other","TSI - Thin", 
                  "Release - Chemical", "Release - Non-Chemical",
                  "Site Prep - Chemical", "Site Prep - Non-Chemical")
)

# Merge the new labels with processed_activities and intersected_activities
processed_activities <- merge(processed_activities, new_labels, by = "ACTIVITY_TYPE", all.x = TRUE)

# Add fire_id column
processed_activities <- processed_activities %>%
  mutate(fire_id = paste(Incid_Name, Event_ID, sep="_"))

# Save the final processed activities
saveRDS(processed_activities, "processed_activities_final.RDS")

# Verification steps
final_activities <- readRDS("processed_activities_final.RDS")




# Check for any unexpected NA values in key columns
summary(final_activities[, c("Event_ID", "Ig_Year", "year", "diff_years", "reburns")])

# Check the range of diff_years
range(final_activities$diff_years, na.rm = TRUE)

# Verify that all diff_years are non-negative
all(final_activities$diff_years >= 0, na.rm = TRUE)

# Check for any remaining duplicate column names
any(duplicated(names(final_activities)))

# Check the distribution of reburns
table(final_activities$reburns)
