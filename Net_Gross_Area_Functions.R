# Functions for summarizing net and gross activity area by different groupings

library("sf")
library("dplyr")
library("terra")
library("tidyverse")
library("units")
library("data.table")
library("future")
library("furrr")

setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output")

assigned_activities = readRDS("assigned_activities_2024.RDS")
facts_fires = readRDS("facts_fires_2024.RDS")

# Summarize cumulative net and gross area for each combo ####
# of fire and activity type for a given timespan
net_gross_cumulative <- function(start,end,assigned_activities){
  
  filtered <- assigned_activities |>
    filter(Ig_Year >= start & Ig_Year + diff_years <= end)
  
  if (dim(filtered)[1] == 0) {
    return(NULL)
  } else{
    filtered$gross_area <- st_area(filtered)
    net_gross <- filtered |> group_by(Event_ID, ACTIVITY_TYPE) |>
      summarize(
        geometry = st_union(geometry),
        gross_area = sum(gross_area)
      )
    net_gross$net_area <- st_area(net_gross)
    
    result <- net_gross |> group_by(ACTIVITY_TYPE) |>
      summarize(
        net_area = sum(net_area),
        gross_area = sum(gross_area)
      )
    result$start <- start
    result$end <- end
    
    return(result)
  }
}

# Map to function, cumulative area since 1994
gross_net_cumulative <- map2_dfr(rep(1994,30),c(1994:2023),
                           net_gross_cumulative,assigned_activities=assigned_activities)

gross_net_cumulative$net_area_ac <- as.numeric(gross_net_cumulative$net_area)/4046.86
gross_net_cumulative$gross_area_ac <- as.numeric(gross_net_cumulative$gross_area)/4046.86
saveRDS(gross_net_cumulative, "gross_net_cumulative.RDS")

# Cumulative area since 1994, only activities within 5 years of ignition
gross_net_cumulative_5years <- map2_dfr(rep(1994,25),c(1994:2018),
                                  net_gross_cumulative,
                                  assigned_activities=assigned_activities[assigned_activities$diff_years<=5,])

gross_net_cumulative_5years$net_area_ac <- as.numeric(gross_net_cumulative_5years$net_area)/4046.86
gross_net_cumulative_5years$gross_area_ac <- as.numeric(gross_net_cumulative_5years$gross_area)/4046.86
saveRDS(gross_net_cumulative_5years, "gross_net_cumulative_5years.RDS")


#### Summarize the net and gross area completed for each ignition year ####

# Function to calculate net and gross by ignition year and activity type
net_gross_ignition <- function(nyears,assigned_activities){
  
  filtered <- assigned_activities %>%
    filter(diff_years<=nyears)
  
  if (dim(filtered)[1] == 0) {
    return(NULL)
  } else{
    filtered$gross_area <- st_area(filtered)
    result <- filtered %>% group_by(Ig_Year, ACTIVITY_TYPE) %>%
      summarize(
        geometry = st_union(geometry),
        gross_area = sum(gross_area)
      )
    result$net_area <- st_area(result)
    result$nyears <- nyears
    return(result)
  }
}

# Map to function, summarize area completed within 15 years by ACTIVITY_TYPE
gross_net_ignition_15years <- map_dfr(rep(1:15),net_gross_ignition,assigned_activities=assigned_activities)
gross_net_ignition_15years$net_area_ac <- as.numeric(gross_net_ignition_15years$net_area)/4046.86
gross_net_ignition_15years$gross_area_ac <- as.numeric(gross_net_ignition_15years$gross_area)/4046.86
saveRDS(gross_net_ignition_15years, "gross_net_ignition_15years.RDS")

# Function to calculate net and gross by ignition year **for all activities**
net_gross_ignition_all <- function(nyears,assigned_activities){
  
  filtered <- assigned_activities %>%
    filter(diff_years<=nyears)
  
  if (dim(filtered)[1] == 0) {
    return(NULL)
  } else{
    filtered$gross_area <- st_area(filtered)
    result <- filtered %>% group_by(Ig_Year, ACTIVITY) %>%
      summarize(
        geometry = st_union(geometry),
        gross_area = sum(gross_area)
      )
    result$net_area <- st_area(result)
    result$nyears <- nyears
    return(result)
  }
}

# Map to function, summarize area completed within 15 years by ACTIVITY
gross_net_ignition_all_15years <- map_dfr(rep(1:15),net_gross_ignition_all,assigned_activities=assigned_activities)
gross_net_ignition_all_15years$net_area_ac <- as.numeric(gross_net_ignition_all_15years$net_area)/4046.86
gross_net_ignition_all_15years$gross_area_ac <- as.numeric(gross_net_ignition_all_15years$gross_area)/4046.86
saveRDS(gross_net_ignition_all_15years, "gross_net_ignition_all_15years.RDS")