# SUMMARY TABLES BY CATEGORY ####

library(dplyr)
library(sf)
library(tidyverse)

setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output")
# setwd("~/Library/CloudStorage/Box-Box/Kelly_postfire_reforestation_project/Output")

facts_activities = readRDS("facts_activities_new.RDS")
intersected_activities = readRDS("intersected_activities_new.RDS")
assigned_activities = readRDS("assigned_activities_new.RDS")
fire_events = st_read("R5_fires_00_22.shp")


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

saveRDS(burned_area_by_year, "burned_area_by_year.RDS")



# # Summarize activity_area by ACTIVITY_TYPE for facts_df
# facts_acres <- intersected_activities %>%
#   st_collection_extract("POLYGON") %>%
#   # group_by(big_groups) %>%
#   group_by(type_labels) %>%
#   summarise(geometry = sum(st_area(geometry)),
#             # "Types" = n_distinct(type_labels),
#             "Activities" = n_distinct(ACTIVITY)) %>%
#   mutate("Total FACTS Acres" = as.numeric(st_area(geometry))/4046.86) %>%
#   st_drop_geometry()
# 
# gross_postfire_acres_by_fire <- assigned_activities %>%
#   st_collection_extract("POLYGON") %>%
#   mutate(fire_id = paste(Incid_Name, Ig_Year, sep="_")) %>%
#   group_by(type_labels, fire_id) %>%
#   summarise(geometry = sum(st_area(geometry))) %>%
#   mutate(gross_acres = as.numeric(st_area(geometry))/4046.86)
# 
# gross_postfire_acres = gross_postfire_acres_by_fire %>%
#   group_by(type_labels) %>%
#   summarize("Gross Postfire Acres" = sum(gross_acres)) %>%
#   ungroup() %>% st_drop_geometry()
# 


net_postfire_acres <- assigned_activities %>%
  filter(Ig_Year < 2022) %>%
  st_collection_extract("POLYGON") %>%
  mutate(fire_id = paste(Incid_Name, Ig_Year, sep="_")) %>%
  group_by(type_labels, fire_id) %>%
  summarize(geometry = st_union(geometry)) %>%
  mutate(net_acres = as.numeric(st_area(geometry))/4046.86)


# Calculate the net area within plantations for all non-plant activities
# Assuming all_activities is loaded

# Filter for the relevant activities
monitor_types <- c("Certification - Plant", "Certification - Release", "Silvicultural Prescription",
                         "Stand Exam", "Survival Survey", "Stocking Survey")

plant <- net_postfire_acres %>%
  filter(type_labels == "Initial Planting")
non_plant <- net_postfire_acres %>%
  filter(type_labels != "Initial Planting")

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

write.csv(activities_in_plantations, "activities_in_plantations.csv")

monitor_types_report = c("Certification - Plant", "Certification - Release", "Silvicultural Prescription",
                  "Stand Exam", "Survival Survey", "Stocking Survey", "Reforest. Need - Failure",
                  "Reforest. Need - Fire", "Reforest. Need - Harvest")
treatment_types_report = c("Fill-in or Replant", "Harvest - Salvage", "Harvest - Non-Salvage", "Site Prep - Chemical",
                    "Site Prep - Mechanical", "Site Prep - Other", "TSI - Release", "TSI - Thin")

# monitor_in_planted = activities_in_plantations %>%
#   filter(type_labels %in% monitor_types)
# treatment_in_planted = activities_in_plantations %>%
#   filter(type_labels %in% treatment_types)


facts_acres <- intersected_activities %>%
  filter(Ig_Year < 2022) %>%
  st_collection_extract("POLYGON") %>%
  group_by(type_labels) %>%
  summarize(geometry = sum(st_area(geometry)),
            "Activities" = n_distinct(ACTIVITY)) %>%
  mutate("Total FACTS Acres" = as.numeric(st_area(geometry))/4046.86) %>%
  st_drop_geometry()


gross_postfire_acres_by_fire <- assigned_activities %>%
  filter(Ig_Year < 2022) %>%
  st_collection_extract("POLYGON") %>%
  mutate(fire_id = paste(Incid_Name, Ig_Year, sep="_")) %>%
  group_by(type_labels, fire_id) %>%
  summarise(geometry = sum(st_area(geometry))) %>%
  mutate(gross_acres = as.numeric(st_area(geometry))/4046.86)

gross_postfire_acres = gross_postfire_acres_by_fire %>%
  group_by(type_labels) %>%
  summarize("Gross Postfire Acres" = sum(gross_acres)) %>%
  ungroup() %>% st_drop_geometry()

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
  full_join(gross_postfire_acres, by = "type_labels") %>%
  full_join(net_postfire_acres_combined, by = "type_labels") %>%
  replace_na(list(Activities = 0, 
                  `Total FACTS Acres` = 0, 
                  `Gross Postfire Acres` = 0, 
                  `Net Postfire Acres` = 0, 
                  `Net Acres in Plantations` = 0, 
                  `Percent Planted Acres` = 0))

# Create a function to assign priority to activity types
get_priority <- function(type) {
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

# Round numeric columns to 2 decimal places
numeric_columns <- c("Total FACTS Acres", "Gross Postfire Acres", "Net Postfire Acres", 
                     "Net Acres in Plantations", "Percent Planted Acres")

final_table <- final_table %>%
  mutate(across(all_of(numeric_columns), ~round(., 3)))

# Display the result
view(final_table)


write.csv(final_table, "combined_postfire_activities.csv", row.names = FALSE)

total_area_before <- sum(st_area(fire_events)) / 4046.86
total_area_after <- sum(st_area(fires_fires)) / 4046.86


# Create fire_id column
fires_fires <- fires_fires %>%
  mutate(fire_id = paste(Incid_Name, Ig_Year, sep = "_"))

# Create fire_id column
fire_events <- fire_events %>%
  mutate(fire_id = paste(Incid_Name, Ig_Year, sep = "_"))

# Summarize the data
burned_once <- fires_fires %>%
  filter(Ig_Year < 2022) %>%
  group_by(fire_id) %>%
  summarize(
    acres_burned_once = sum(ifelse(n.overlaps == 1, area, 0)) / 4046.86, # Convert m^2 to acres
    complete_reburn = sum(ifelse(acres_burned_once == 0, 1, 0)), 
    Max_Overlaps = max(n.overlaps)
  ) %>%
  st_drop_geometry()

total_fire_events = fire_events %>%
  filter(Ig_Year < 2022) %>%
  group_by(fire_id) %>% 
  summarize(total_acres = sum(as.numeric(st_area(geometry)))/4046.86) %>% 
  st_drop_geometry

# Calculate overall statistics
overall_total <- total_fire_events %>%
  left_join(burned_once) %>%
  mutate(reburned_acres = total_acres - acres_burned_once) %>%
  ungroup() %>%
  summarize(
    Total_Fires = n_distinct(fire_id),
    Total_Acres_Burned_Once = sum(acres_burned_once),
    Total_Acres_Reburned = sum(reburned_acres),
    Total_Acres = sum(total_acres),
    Overall_Percent_Reburned = (sum(reburned_acres) / sum(total_acres)) * 100,
    Complete_Reburn_n = sum(complete_reburn),
    Complete_Reburn_acres = sum(ifelse(acres_burned_once == 0, reburned_acres, 0))
  ) %>%
  arrange(desc(Total_Acres))

print(overall_total)
