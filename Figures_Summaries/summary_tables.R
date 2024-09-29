# SUMMARY TABLES BY CATEGORY ####

library(dplyr)
library(sf)
library(tidyverse)

setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output")

facts_activities = readRDS("facts_activities_new.RDS")
intersected_activities = readRDS("intersected_activities_new.RDS")
processed_activities = readRDS("processed_activities_final.RDS")

assigned_activities = readRDS("assigned_activities_new.RDS")
fire_events = st_read("R5_fires_00_21.shp")


# Calculate total and cumulative area burned
# Assuming fire_events is loaded

total_area_burned <- function(data) {
  result <- data %>%
    group_by(Ig_Year, Event_ID) %>%
    summarize(
      fire_acres = as.numeric(st_area(geometry)) / 4046.86
    ) %>%
    group_by(Ig_Year) %>%
    summarize(
      acres_burned = sum(fire_acres)
    ) %>%
    arrange(Ig_Year) %>%
    mutate(
      cumulative_acres_burned = cumsum(acres_burned)
    ) %>%
    st_drop_geometry()
  return(result)
}

# Calculate cumulative area burned
burned_area_by_year <- total_area_burned(fire_events)

write.csv(burned_area_by_year, "burned_area_by_year_R5.csv")

saveRDS(burned_area_by_year, "burned_area_by_year.RDS")



# Calculate weighted average % High for each fire_id and Ig_Year
weighted_high_severity <- severity_burned %>%
  group_by(fire_id, Ig_Year) %>%
  summarize(
    total_acres = sum(Area_acres),
    weighted_high_percent = sum(Area_acres[Severity_Class == "High"] / sum(Area_acres) * 100)
  ) %>%
  ungroup()

# Calculate weighted average % High for each Ig_Year
yearly_weighted_high_severity <- weighted_high_severity %>%
  group_by(Ig_Year) %>%
  summarize(
    weighted_avg_high_percent = sum(weighted_high_percent * total_acres) / sum(total_acres)
  ) %>%
  arrange(Ig_Year)


late_planting = processed_activities %>%
  filter(
         type_labels == "Initial Planting",
         diff_years > 12) %>%
  mutate(acres = as.numeric(st_area(geometry)) / 4046.86) %>%
  group_by(Ig_Year, fire_id, year) %>%
  summarize(planted_per_year = sum(acres),
            date = first(DATE_COMPL))





#### Gross and Net Postfire / Net within Planted for Table ####

net_postfire_acres <- processed_activities %>%
  st_collection_extract() %>%
  group_by(type_labels, fire_id) %>%
  summarize(geometry = st_union(geometry)) %>%
  mutate(net_acres = as.numeric(st_area(geometry))/4046.86)



# Calculate the net area within plantations for all non-plant activities

plant <- net_postfire_acres %>%
  filter(type_labels == "Initial Planting")
non_plant <- net_postfire_acres %>%
  filter(type_labels != "Initial Planting")
# 
# plant <- net_activities %>%
#   filter(type_labels == "Initial Planting")
# non_plant <- net_activities %>%
#   filter(type_labels != "Initial Planting")

planted_by_fire = plant %>%
  rename(net_total = net_acres) %>%
  ungroup()
non_plant_by_fire = non_plant %>%
  rename(net_total = net_acres) %>%
  ungroup()

# non_plant_by_year_list <- non_plant_by_year %>% group_split(Ig_Year, type_labels)
non_plant_by_fire_list <- non_plant_by_fire %>% group_split(fire_id, type_labels)

calculate_intersection <- function(non_plant_group, planted_by_fire) {
  fire <- unique(non_plant_group$fire_id)
  type <- unique(non_plant_group$type_labels)
  
  non_plant_area <- non_plant_group %>%
    summarize(net_acres = sum(net_total)) %>%
    pull(net_acres)
  
  planted_area <- planted_by_fire %>% 
    filter(fire_id == fire)
  
  if (nrow(planted_area) == 0) {
    return(tibble(
      fire_id = fire,
      type_labels = type,
      net_total = non_plant_area,
      intersecting_acres = 0,
      percent_total_planted = 0,
      percent_total_activity = 0
    ))
  }
  
  intersection <- st_intersection(non_plant_group$geometry, planted_area$geometry)
  intersecting_acres <- as.numeric(st_area(intersection)) / 4046.86  # Convert to acres
  
  planted_total_area <- planted_area %>% 
    pull(net_total)
  
  percent_total_planted <- (intersecting_acres / planted_total_area)
  percent_total_activity <- (intersecting_acres / non_plant_area)
  
  tibble(
    fire_id = fire,
    type_labels = type,
    net_total = non_plant_area,
    intersecting_acres = intersecting_acres,
    percent_total_planted = percent_total_planted,
    percent_total_activity = percent_total_activity
  )
}

intersecting_planted <- function(non_plant_by_fire_list, planted_by_fire) {
  map_dfr(non_plant_by_fire_list, ~calculate_intersection(.x, planted_by_fire), .progress = TRUE)
}

# Usage:
activities_in_plant <- intersecting_planted(non_plant_by_fire_list, planted_by_fire)

combined_activities_in_plant <- activities_in_plant %>%
  full_join(planted_by_fire, by = c("fire_id", "type_labels", "net_total")) %>%
  select(-geometry)

saveRDS(combined_activities_in_plant, "combined_activities_in_plant.RDS")
write.csv(activities_in_plantations, "activities_in_plantations.csv")

combined_activities_in_plant <- readRDS("combined_activities_in_plant.RDS")
# Create the total & gross tables then combine with net

facts_acres_by_fire <- intersected_activities %>%
  st_collection_extract() %>%
  mutate(fire_id = paste(Incid_Name, Event_ID, sep="_")) %>%
  # First, group by facts_polygon_id and summarize
  group_by(facts_polygon_id) %>%
  summarize(
    type_labels = first(type_labels),
    fire_id = first(fire_id),
    geometry = st_union(geometry),
    n_records = n()
  ) %>%
  # Then, group by type_labels and fire_id
  group_by(type_labels, fire_id) %>%
  summarize(
    geometry = st_union(geometry),
    n_records = sum(n_records)
  ) %>%
  mutate(facts_acres = as.numeric(st_area(geometry))/4046.86)

facts_acres <- facts_acres_by_fire %>%
  group_by(type_labels) %>%
  summarize(
    "Total FACTS Acres" = sum(facts_acres),
    n_records = sum(n_records)
  ) %>%
  ungroup() %>% 
  st_drop_geometry()


gross_postfire_acres_by_fire <- processed_activities %>%
  st_collection_extract() %>%
  group_by(type_labels, fire_id) %>%
  summarise(geometry = sum(st_area(geometry)),
            n_records = n()) %>%
  mutate(gross_acres = as.numeric(st_area(geometry))/4046.86)

gross_postfire_acres = gross_postfire_acres_by_fire %>%
  group_by(type_labels) %>%
  summarize("Gross Postfire Acres" = sum(gross_acres),
            n_records = sum(n_records)) %>%
  ungroup() %>% 
  st_drop_geometry()
  
  
net_postfire_acres_combined <- combined_activities_in_plant %>%
  group_by(type_labels) %>%
  summarize(
    "Net Postfire Acres" = sum(net_total),
    "Net Acres in Plantations" = sum(intersecting_acres)
  ) %>%
  ungroup() %>%
  mutate(
    "Percent Planted Acres" = case_when(
      type_labels != "Initial Planting" ~ 
        `Net Acres in Plantations` / (combined_activities_in_plant %>% 
                                        filter(type_labels == "Initial Planting") %>% 
                                        summarize(total_planted = sum(net_total)) %>% 
                                        pull(total_planted)),
      TRUE ~ NA_real_
    )
  )



# Combine the tables
combined_table <- facts_acres %>%
  rename(n_pre_assign = n_records) %>%
  full_join(gross_postfire_acres, by = "type_labels") %>%
  full_join(net_postfire_acres_combined, by = "type_labels") %>%
  replace_na(list(`Total FACTS Acres` = 0, 
                  n_pre_assign = 0,
                  `Gross Postfire Acres` = 0, 
                  n_records = 0,
                  `Net Postfire Acres` = 0, 
                  `Net Acres in Plantations` = 0, 
                  `Percent Planted Acres` = 0)) %>%
  select(type_labels, n_pre_assign, "Total FACTS Acres", n_records, everything())


treatment_types_report <- c(
  "Initial Planting",
  "Fill-in or Replant",
  "Release - Chemical",
  "Release - Non-Chemical",
  "TSI - Thin",
  "Harvest - Salvage",
  "Harvest - Non-Salvage",
  "Site Prep - Chemical",
  "Site Prep - Non-Chemical"
)

monitor_types_report <- c(
  "Certification - Plant",
  "Certification - Release",
  "Stocking Survey",
  "Survival Survey",
  "Stand Exam",
  "Survey - Other",
  "Reforest. Need - Failure",
  "Reforest. Need - Fire",
  "Reforest. Need - Harvest"
)

get_priority <- function(type) {
  if (is.na(type)) return(5)  # Assign lowest priority to NA values
  if (type == "Initial Planting") return(1)
  if (type %in% treatment_types_report) return(2)
  if (type %in% monitor_types_report) return(3)
  return(4)  # for any other types
}

# Add priority column and sort
final_table <- combined_table %>%
  mutate(
    priority = sapply(type_labels, get_priority),
    `Percent Planted Acres` = ifelse(is.na(`Percent Planted Acres`), 0, `Percent Planted Acres`)
  ) %>%
  arrange(priority, type_labels) %>%
  select(-priority) # Remove the priority column after sorting

# Verify the result
print(head(final_table))

# Round numeric columns to 2 decimal places
numeric_columns <- c("Total FACTS Acres", "Gross Postfire Acres", "Net Postfire Acres", 
                     "Net Acres in Plantations", "Percent Planted Acres")

final_table <- final_table %>%
  mutate(across(all_of(numeric_columns), ~round(., 3)))

# Display the result
view(final_table)

write.csv(final_table, "combined_postfire_activities.csv", row.names = FALSE)
saveRDS(final_table, "combined_postfire_activities.RDS")



# 
# 
# mean_acres_by_year = assigned_activities %>%
#   st_collection_extract() %>%
#   group_by(type_labels, Ig_Year) %>%
#   summarize(geometry = sum(st_area(geometry))) %>%
#   group_by(type_labels) %>%
#   summarize(mean_acres = mean(as.numeric(st_area(geometry))/4046.86)) %>%
#   st_drop_geometry()


# Create fire_id column
fires_fires <- facts_fires$fires_fires %>%
  mutate(fire_id = paste(Incid_Name, Event_ID, sep = "_"))

# Create fire_id column
fire_events <- facts_fires$fires %>%
  mutate(fire_id = paste(Incid_Name, Event_ID, sep = "_"))

# Summarize the data
burned_once <- fires_fires %>%
  group_by(Ig_Year, fire_id) %>%
  summarize(
    acres_burned_once = sum(ifelse(n.overlaps == 1, area, 0)) / 4046.86, # Convert m^2 to acres
    n_overlaps = sum(n.overlaps)
  )

total_fire = fire_events %>%
  # group_by(Ig_Year, fire_id) %>% 
  summarize(total_acres = sum(as.numeric(st_area(geometry)))/4046.86) %>%
  st_drop_geometry()


# Calculate overall statistics
fire_summary_reburns <- burned_once %>%
  left_join(total_fire_events) %>%
  mutate(reburned_acres = total_acres - acres_burned_once) %>%
  group_by(Ig_Year, fire_id) %>%
  summarize(total_acres = total_acres,
            acres_burned_once = acres_burned_once,
            reburned_acres = reburned_acres,
            n_overlaps = n_overlaps)
  # 
  # summarize(
  #   `N Fires` = n_distinct(fire_id),
  #   `Total Acres Burned` = sum(total_acres),
  #   `Acres Reburned` = sum(reburned_acres),
  #   `(%)` = (sum(reburned_acres) / sum(total_acres)) * 100,
  # ) %>%
  # arrange(desc(`Total Acres Burned`))

view(fire_summary_reburns)
write.csv(fire_summary_reburns, "fire_summary_reburns.csv")

# intersect with eco layer
cal_eco3 <- st_read("../Data/ca_eco_l3.shp")
cal_eco3 <- st_transform(cal_eco3, crs(fire_summary_reburns))

fire_summary_reburns_eco <- fire_summary_reburns %>%
  st_collection_extract() %>%
  st_cast("MULTIPOLYGON") %>%
  st_intersection(cal_eco3 %>% select(US_L3NAME))




severity_by_fire = severity_burned %>%
  group_by(Ig_Year, fire_id, Severity_Class) %>%
  pivot_wider(id_cols = c(Ig_Year, fire_id),
              names_from = Severity_Class,
              values_from = Area_acres) %>%
  mutate(total = sum(across(where(is.numeric)), na.rm = TRUE))
write.csv(severity_by_fire, "severity_by_fire.csv")



#### Total Fires Reburned ####

# Assuming assigned_activities is already loaded

# Step 1: Identify all initial planting activities before filtering
all_plantings <- facts_fires$process %>%
  filter(ACTIVITY == "Plant Trees")

# Step 2: Identify plantings that intersect multiple fires
plantings_multiple_fires <- all_plantings %>%
  group_by(facts_polygon_id) %>%
  filter(n() > 1) %>%
  ungroup()

# Step 3: Identify plantings that were kept after assignment
kept_plantings <- assigned_activities %>%
  filter(type_labels == "Initial Planting")

# Convert to non-spatial data frames for the anti_join
all_plantings_df <- all_plantings %>% st_drop_geometry()
kept_plantings_df <- kept_plantings %>% st_drop_geometry()

filtered_plantings_df <- anti_join(all_plantings_df, kept_plantings_df, by = "facts_polygon_id")

# Recreate spatial object for filtered plantings
filtered_plantings <- all_plantings %>%
  filter(facts_polygon_id %in% filtered_plantings_df$facts_polygon_id)

# Step 5: Calculate areas
calculate_area <- function(sf_object) {
  sum(st_area(sf_object)) / 4046.86  # Convert to acres
}

total_planted_acres <- calculate_area(all_plantings)
kept_planted_acres <- calculate_area(kept_plantings)
filtered_planted_acres <- calculate_area(filtered_plantings)
multiple_fire_acres <- calculate_area(plantings_multiple_fires)

# Step 6: Create summary
planting_summary <- data.frame(
  Total_Planted_Acres = total_planted_acres,
  Kept_Planted_Acres = kept_planted_acres,
  Filtered_Planted_Acres = filtered_planted_acres,
  Multiple_Fire_Acres = multiple_fire_acres,
  Percent_Kept = (kept_planted_acres / total_planted_acres) * 100,
  Percent_Filtered = (filtered_planted_acres / total_planted_acres) * 100,
  Percent_Multiple_Fires = (multiple_fire_acres / total_planted_acres) * 100
)



# Step 7: Analyze filtered plantings with reburn information
filtered_planting_analysis <- filtered_plantings %>%
  group_by(facts_polygon_id) %>%
  summarize(
    Num_Intersecting_Fires = n(),
    Earliest_Fire_Year = min(Ig_Year),
    Latest_Fire_Year = max(Ig_Year),
    Planting_Year = first(year),
    Area_Acres = calculate_area(geometry),
    Is_Reburn = Num_Intersecting_Fires > 1
  ) %>%
  ungroup()

# Calculate reburn statistics
reburn_stats <- filtered_planting_analysis %>%
  summarize(
    Total_Filtered_Acres = sum(Area_Acres),
    Reburn_Acres = sum(Area_Acres[Is_Reburn]),
    Percent_Reburn = (Reburn_Acres / Total_Filtered_Acres) * 100,
    Num_Reburn_Plantings = sum(Is_Reburn),
    Total_Plantings = n()
  )

# Add reburn information to the planting summary
planting_summary$Filtered_Reburn_Acres <- reburn_stats$Reburn_Acres
planting_summary$Percent_Filtered_Reburn <- (reburn_stats$Reburn_Acres / planting_summary$Filtered_Planted_Acres) * 100

# Print the updated summary
print(planting_summary)

# Print reburn statistics
print(reburn_stats)

# Optionally, save the updated summary to a CSV file
write.csv(planting_summary, "planting_fire_intersection_summary_with_reburns.csv", row.names = FALSE)

# Print the first few rows of the filtered planting analysis
print(head(filtered_planting_analysis))

# Optionally, save the filtered planting analysis to a CSV file
write.csv(st_drop_geometry(filtered_planting_analysis), "filtered_planting_analysis_with_reburns.csv", row.names = FALSE)

# Create a summary of reburns by number of intersecting fires
reburn_summary <- filtered_planting_analysis %>%
  group_by(Num_Intersecting_Fires) %>%
  summarize(
    Total_Acres = sum(Area_Acres),
    Num_Plantings = n()
  ) %>%
  mutate(
    Percent_of_Filtered_Acres = (Total_Acres / sum(Total_Acres)) * 100,
    Percent_of_Filtered_Plantings = (Num_Plantings / sum(Num_Plantings)) * 100
  )

# Print the reburn summary
print(reburn_summary)

# Optionally, save the reburn summary to a CSV file
write.csv(reburn_summary, "filtered_planting_reburn_summary.csv", row.names = FALSE)





# Update net_activities with reburn information and intersect with ecoregions
total_net_activities_summary <- processed_activities %>%
  st_collection_extract("POLYGON") %>%
  st_transform(3310) %>%
  mutate(activity_area = st_area(.)) %>%
  group_by(fire_id, ACTIVITY) %>%
  mutate(
    gross_acres = sum(as.numeric(activity_area)) / 4046.86,
  ) %>%
  ungroup() %>%
  st_intersection(cal_eco3 %>% select(US_L3NAME)) %>%
  group_by(US_L3NAME, type_labels, ACTIVITY) %>%
  summarize(
    geometry = st_union(geometry),
    n_records = n(),
    net_acres = sum(as.numeric(st_area(geometry))) / 4046.86,
    gross_acres = sum(gross_acres),
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
  ) %>% st_drop_geometry()


library(dplyr)

# Function to calculate high severity and total acres for an activity
calculate_acres <- function(data, activity_label) {
  data %>%
    filter(Category == "All Activities", 
           type_labels == activity_label) %>%
    group_by(Category, type_labels) %>% 
    summarize(
      moderate_severity_acres = sum(Moderate_acres, na.rm = TRUE),
      high_severity_acres = sum(High_acres, na.rm = TRUE),
      total_acres = sum(Total_acres, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Activity = activity_label)
}

# Calculate acres for each activity
initial_planting <- calculate_acres(severity_summary, "Initial Planting")
fill_in_replant <- calculate_acres(severity_summary, "Fill-in or Replant")
certification_plant <- calculate_acres(severity_summary, "Certification - Plant")
release_nonchemical <- calculate_acres(severity_summary, "Release - Non-Chemical")
release_chemical <- calculate_acres(severity_summary, "Release - Chemical")
siteprep_nonchemical <- calculate_acres(severity_summary, "Site Prep - Non-Chemical")
siteprep_chemical <- calculate_acres(severity_summary, "Site Prep - Chemical")
reforest_need_fire <- calculate_acres(severity_summary, "Reforest. Need - Fire")
harvest_salvage <- calculate_acres(severity_summary, "Harvest - Salvage")
harvest_nonsalvage <- calculate_acres(severity_summary, "Harvest - Non-Salvage")
thinning <- calculate_acres(severity_summary, "TSI - Thin")

# Calculate total burned acres
total_burned <- severity_summary %>%
  filter(Category == "Total Burned") %>%
  group_by(Category) %>% 
  summarize(
    moderate_severity_acres = sum(Moderate_acres, na.rm = TRUE),
    high_severity_acres = sum(High_acres, na.rm = TRUE),
    total_acres = sum(Total_acres, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Activity = "Total Burned")

# Combine all results
activity_summary <- bind_rows(
  initial_planting,
  fill_in_replant,
  certification_plant,
  release_nonchemical,
  release_chemical,
  siteprep_nonchemical,
  siteprep_chemical,
  reforest_need_fire,
  harvest_salvage,
  harvest_nonsalvage,
  thinning,
  total_burned
)

# Calculate total high severity burned acres
total_high_severity_burned <- total_burned$high_severity_acres
total_moderate_severity_burned <- total_burned$moderate_severity_acres

# Calculate percentages and round the numbers
activity_summary <- activity_summary %>%
  mutate(
    moderate_severity_acres = round(moderate_severity_acres),
    high_severity_acres = round(high_severity_acres, 2),
    total_acres = round(total_acres, 2),
    percent_high_severity = round(high_severity_acres / total_acres * 100, 2),
    percent_of_total_high_severity = round(high_severity_acres / total_high_severity_burned * 100, 2),
    percent_moderate_severity = round(moderate_severity_acres / total_acres * 100, 2),
    percent_of_total_moderate_severity = round(moderate_severity_acres / total_moderate_severity_burned * 100, 2)
  ) %>%
  select(Activity, total_acres, moderate_severity_acres, high_severity_acres, percent_moderate_severity, 
         percent_of_total_moderate_severity, percent_high_severity, percent_of_total_high_severity)

# Rename columns for clarity
colnames(activity_summary) <- c("Activity", "Total Acres", "Moderate Acres", "High Severity Acres", 
                                "% Moderate Severity", "% of Total Moderate Burned", 
                                "% High Severity", "% of Total High Burned")

# Print the summary
print(activity_summary)
