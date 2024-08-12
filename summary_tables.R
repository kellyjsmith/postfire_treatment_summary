# SUMMARY TABLES BY CATEGORY ####

library(dplyr)
library(sf)
library(tidyverse)

setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output")
# setwd("~/Library/CloudStorage/Box-Box/Kelly_postfire_reforestation_project/Output")

facts_df = readRDS("intersected_activities_02_22_new.RDS") %>%
  st_drop_geometry()
assigned_activities = readRDS("assigned_activities_02_22.RDS")
assigned_df = readRDS("assigned_activities_02_22_new.RDS") %>%
  st_drop_geometry()
# combined_df = readRDS("combined_by_fire.RDS")
combined_ignition_df = readRDS("combined_ignition_year.RDS")
  st_drop_geometry()
combined_ignition_5yr_df = readRDS("combined_ignition_year_5yr.RDS") %>%
  st_drop_geometry()


# Summarize total and postfire activity acres by all activities and types
total_facts =  assigned_df %>%
  group_by(type_labels,ACTIVITY) %>%
  summarize(total_facts_acres = sum(GIS_ACRES, na.rm = TRUE))
total_postfire <- assigned_df %>%
  group_by(type_labels,ACTIVITY) %>%
  summarise(total_postfire_acres = sum(activity_fire_area/4046.86, na.rm = TRUE))
total_summary = full_join(total_facts,total_postfire,by="ACTIVITY") %>%
  select(-type_labels.y)
write.csv(total_summary,"acreage_summary_all_activities.csv")


# Summarize activity_area by ACTIVITY_TYPE for facts_df
facts_acres <- facts_df %>%
  group_by(big_groups) %>%
  summarise(n_types = n_distinct(type_labels),
            "Total FACTS Acres" = sum(GIS_ACRES, na.rm = TRUE))

gross_postfire_acres <- assigned_df %>%
  group_by(big_groups) %>%
  summarise("Total (Gross) Postfire Acres" = sum(as.numeric(activity_fire_area/4046.86, na.rm = TRUE)))

net_postfire_acres <- combined_df %>%
  group_by(big_groups) %>%
  summarise("Net Postfire Acres" = sum(net_acres, na.rm = TRUE))

# net_postfire_acres_5yr = combined_df %>%
#   group_by(big_groups) %>%
#   filter(diff_years <= 5) %>%
#   summarize("Net Acres <= 5 Yrs" = sum(net_acres, na.rm = TRUE))


# Combine the four tables
combined_acreage_summary <- full_join(facts_acres, gross_postfire_acres, by = "big_groups") %>%
  full_join(net_postfire_acres, by = "big_groups")
  # full_join(net_postfire_acres_5yr, by = "big_groups")
write.csv(combined_acreage_summary, "combined_acreage_summary_big.csv")

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



# Create cumulative area burned function
total_area_burned <- function(data) {
  result <- data %>%
    group_by(Ig_Year, Event_ID) %>%
    summarize(
      BurnBndAc = first(BurnBndAc)
    ) %>%
    group_by(Ig_Year) %>%
    summarize(
      acres_burned = sum(BurnBndAc)
    ) %>%
    arrange(Ig_Year) %>%
    mutate(
      cumulative_acres_burned = cumsum(acres_burned)
    ) %>%
    st_drop_geometry()
  return(result)
}

# Calculate cumulative area burned
burned_area_by_year <- total_area_burned(fires)



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
