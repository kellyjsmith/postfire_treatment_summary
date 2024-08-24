# SUMMARY TABLES BY CATEGORY ####

library(dplyr)
library(sf)
library(tidyverse)

setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output")
# setwd("~/Library/CloudStorage/Box-Box/Kelly_postfire_reforestation_project/Output")

intersected_activities = readRDS("intersected_activities_new.RDS")
assigned_activities = readRDS("assigned_activities_new.RDS")


# Summarize total and postfire activity acres by all activities and types
# total_facts =  assigned_df %>%
#   group_by(type_labels,ACTIVITY) %>%
#   summarize(total_facts_acres = sum(GIS_ACRES, na.rm = TRUE))
# total_postfire <- assigned_df %>%
#   group_by(type_labels,ACTIVITY) %>%
#   summarise(total_postfire_acres = sum(activity_fire_area/4046.86, na.rm = TRUE))
# 
# total_summary = full_join(total_facts,total_postfire,by="ACTIVITY") %>%
#   select(-type_labels.y)
# write.csv(total_summary,"acreage_summary_all_activities.csv")


# Summarize activity_area by ACTIVITY_TYPE for facts_df
facts_acres <- intersected_activities %>%
  group_by(big_groups) %>%
  summarise(group_geometry = sum(st_area(geometry)),
            "Types" = n_distinct(type_labels),
            "Activities" = n_distinct(ACTIVITY)) %>%
  mutate("Total FACTS Acres" = as.numeric(group_geometry)/4046.86) %>%
  select(-group_geometry) %>%
  st_drop_geometry()

gross_postfire_acres <- assigned_activities %>%
  group_by(Event_ID, big_groups) %>%
  summarise(event_gross_geometry = sum(st_area(geometry))) %>%
  mutate(event_gross_acres = as.numeric(st_area(event_gross_geometry))/4046.86) %>%
  group_by(big_groups) %>%
  summarise(
    "Gross Postfire Acres" = sum(event_gross_acres),
    gross_geometry = sum(event_gross_geometry)
  ) %>%
  select(-gross_geometry) %>%
  st_drop_geometry()

net_postfire_total <- assigned_activities %>%
  group_by(Event_ID, big_groups) %>%
  summarise(event_net_geometry = st_union(geometry)) %>%
  mutate(event_net_acres = as.numeric(st_area(event_net_geometry))/4046.86) %>%
  group_by(big_groups) %>%
  summarise(
    "Net Postfire Acres" = sum(event_net_acres),
    net_geometry = st_union(event_net_geometry)
  ) %>%
  select(-net_geometry) %>%
  st_drop_geometry()

net_postfire_5years <- assigned_activities %>%
  filter(diff_years <= 5) %>%
  group_by(Event_ID, big_groups) %>%
  summarise(event_net_geometry = st_union(geometry)) %>%
  mutate(event_net_acres = as.numeric(st_area(event_net_geometry))/4046.86) %>%
  group_by(big_groups) %>%
  summarise(
    "Net Acres <= 5 Years" = sum(event_net_acres),
    net_geometry = st_union(event_net_geometry)
  ) %>%
  select(-net_geometry) %>%
  st_drop_geometry()

net_postfire_acres = net_postfire_total %>%
  left_join()
  
# Combine the tables
combined_acreage_summary <- facts_acres %>%
  left_join(gross_postfire_acres, by = "big_groups") %>%
  left_join(net_postfire_acres, by = "big_groups") %>%
  left_join(net_postfire_5years, by = "big_groups") %>%
  mutate(
    `% Net <= 5 Years` = `Net Acres <= 5 Years` / `Net Postfire Acres`
  )
write.csv(combined_acreage_summary, "combined_acreage_summary_big.csv")


total_by_year = assigned_activities %>% 
  st_drop_geometry() %>%
  group_by(Ig_Year) %>% 
  summarize(total_acres = as.numeric(sum(activity_fire_area))/4046.86)


# library(parallel)
# library(foreach)
# library(doParallel)
# 
# # Function to process facts_acres in parallel
# parallel_facts_acres <- function(intersected_activities) {
#   groups <- group_split(intersected_activities, big_groups)
#   
#   cl <- makeCluster(7)
#   registerDoParallel(cl)
#   
#   results <- foreach(group = groups, .combine = rbind, .packages = c("dplyr", "sf")) %dopar% {
#     group %>%
#       summarise(n_types = n_distinct(type_labels),
#                 "Total FACTS Acres" = as.numeric(sum(st_area(geometry)))/4046.86) %>%
#       st_drop_geometry()
#   }
#   
#   stopCluster(cl)
#   
#   return(results)
# }
# 
# # Function to process gross_postfire_acres in parallel
# parallel_gross_postfire_acres <- function(assigned_activities) {
#   groups <- group_split(assigned_activities, big_groups)
#   
#   cl <- makeCluster(7)
#   registerDoParallel(cl)
#   
#   results <- foreach(group = groups, .combine = rbind, .packages = c("dplyr", "sf")) %dopar% {
#     group %>%
#       summarise("Total (Gross) Postfire Acres" = as.numeric(sum(st_area(geometry)))/4046.86) %>%
#       st_drop_geometry()
#   }
#   
#   stopCluster(cl)
#   
#   return(results)
# }
# 
# # Function to process net_postfire_acres in parallel
# parallel_net_postfire_acres <- function(assigned_activities) {
#   groups <- group_split(assigned_activities, big_groups)
#   
#   cl <- makeCluster(7)
#   registerDoParallel(cl)
#   
#   results <- foreach(group = groups, .combine = rbind, .packages = c("dplyr", "sf")) %dopar% {
#     group %>%
#       group_by(Event_ID, big_groups) %>%
#       summarize(net_geometry = st_union(geometry)) %>% 
#       group_by(big_groups) %>%
#       summarize("Net Postfire Acres" = st_union(net_geometry)/4046.86) %>%
#       st_drop_geometry()
#   }
#   
#   stopCluster(cl)
#   
#   return(results)
# }
# 
# # Usage example:
# facts_acres_result <- parallel_facts_acres(intersected_activities)
# gross_postfire_acres_result <- parallel_gross_postfire_acres(assigned_activities)
# net_postfire_acres_result <- parallel_net_postfire_acres(assigned_activities)


# fire_plant_summary = combined_ignition_year %>%
#   filter(type_labels == "Initial Planting") %>%
#   group_by(Ig_Year, fire_id) %>%
#   summarize(total_net = sum(net_acres))
# 
# 
# plant_year_summary = assigned_activities %>%
#   filter(type_labels == "Plant Trees") %>%
#   group_by(Ig_Year, Incid_Name, diff_years, type_labels) %>%
#   summarize(total_acres = as.numeric(sum(activity_fire_area) / 4046.86))


fire_events = st_read("R5_fires_00_22.shp")

# Create cumulative area burned function
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




# Read in budget and national burn data
usfs_budgets_burn = read.csv("../Data/usfs_budgets.csv")

analyze_proportions <- function(data) {
  
  # Ensure valid geometries
  data <- st_make_valid(data)
  
  # Simplify geometries
  data = st_simplify(data, dTolerance = 1)
  
  # Separate plant data
  plant <- data %>% filter(IS_Plant == TRUE)
  
  # List of activity types to join
  activity_types <- c("Certification - Plant", "Fill-in or Replant", "TSI - Release", "Stocking Survey", "TSI - Thin")
  
  # Initialize result dataframe
  result <- plant %>%
    group_by(Ig_Year, Incid_Name) %>%
    summarise(
      acres_burned_r5 = sum(as.numeric(fire_area)/4046.86),
      planted_acres_gross = sum(as.numeric(st_area(geometry))/4046.86, na.rm = TRUE),
      planted_acres_net = as.numeric(st_area(st_union(geometry)))/4046.86,
      prop_planted = planted_acres_net / first(BurnBndAc),
      .groups = "drop"
    )
  
  # Perform spatial join for each activity type
  for (activity in activity_types) {
    activity_data <- data %>% 
      filter(type_labels == activity) %>%
      mutate(geometry_activity = geometry)
    
    tryCatch({
      joined <- st_join(plant, activity_data, suffix = c("_plant","_activity")) %>%
        group_by(Ig_Year_plant, Incid_Name_plant) %>%
        summarise(
          # activity_acres_gross = sum(as.numeric(st_area(geometry_activity))/4046.86, na.rm = TRUE),
          activity_acres_net = as.numeric(st_area(st_union(geometry_activity)))/4046.86,
          .groups = "drop"
        )
      
      # Add to result
      col_name <- tolower(gsub(" - | ", "_", activity))
      # result[[paste0(col_name, "_acres_gross")]] <- joined$activity_acres_gross
      result[[paste0(col_name, "_acres_net")]] <- joined$activity_acres_net
      # result[[paste0("prop_", col_name)]] <- joined$activity_acres_net / result$planted_acres_net
    }, error = function(e) {
      warning(paste("Error processing", activity, ":", e$message))
      # Add columns with NA values if join fails
      col_name <- tolower(gsub(" - | ", "_", activity))
      # result[[paste0(col_name, "_acres_gross")]] <- NA
      result[[paste0(col_name, "_acres_net")]] <- NA
      # result[[paste0("prop_", col_name)]] <- NA
    })
  }
  
  return(result)
}

# Usage
activity_proportions <- analyze_proportions(assigned_activities)
write.csv(activity_proportions,"activity_proportions.csv")


analyze_proportions_by_productivity <- function(data) {
  
  # Ensure valid geometries
  data <- st_make_valid(data)
  
  # Simplify geometries
  data = st_simplify(data, dTolerance = 1)
  
  # Separate plant data
  plant <- data %>% filter(IS_Plant == TRUE)
  
  # List of activity types to join
  activity_types <- c("Certification - Plant", "Fill-in or Replant", "TSI - Release", "Stocking Survey", "TSI - Thin")
  
  # Initialize result dataframe
  result <- plant %>%
    group_by(Ig_Year, Incid_Name, PRODUCTIVI) %>%
    summarise(
      planted_acres_gross = sum(as.numeric(st_area(geometry))/4046.86, na.rm = TRUE),
      planted_acres_net = as.numeric(st_area(st_union(geometry)))/4046.86,
      .groups = "drop"
    )
  
  # Perform spatial join for each activity type
  for (activity in activity_types) {
    activity_data <- data %>% 
      filter(type_labels == activity) %>%
      mutate(geometry_activity = geometry)
    
    tryCatch({
      joined <- st_join(plant, activity_data, suffix = c("_plant","_activity")) %>%
        group_by(Ig_Year_plant, Incid_Name_plant, PRODUCTIVI_plant) %>%
        summarise(
          activity_acres_net = as.numeric(st_area(st_union(geometry_activity)))/4046.86,
          .groups = "drop"
        )
      
      # Add to result
      col_name <- tolower(gsub(" - | ", "_", activity))
      result[[paste0(col_name, "_acres_net")]] <- joined$activity_acres_net
    }, error = function(e) {
      warning(paste("Error processing", activity, ":", e$message))
      # Add columns with NA values if join fails
      col_name <- tolower(gsub(" - | ", "_", activity))
      result[[paste0(col_name, "_acres_net")]] <- NA
    })
  }
  
  result <- result %>% arrange(PRODUCTIVI, Ig_Year, Incid_Name)
  
  # Create secondary summary by productivity class
  summary_by_productivity <- result %>%
    group_by(PRODUCTIVI) %>%
    summarise(across(where(is.numeric), sum, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(PRODUCTIVI)
  
  # Return both the detailed result and the summary
  return(list(detailed_result = result, summary_by_productivity = summary_by_productivity))
}

# Usage
productivity_output <- analyze_proportions_by_productivity(assigned_activities)
productivity_by_fire <- productivity_output$detailed_result
summary_by_productivity <- productivity_output$summary_by_productivity
