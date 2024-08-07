# Functions for summarizing net and gross activity area by different groupings

library("sf")
library("dplyr")
library("terra")
library("tidyverse")
library("units")
library("data.table")
library("future")
library("furrr")
library("units")

setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output")

assigned_activities = readRDS("assigned_activities_2024.RDS")
facts_fires = readRDS("facts_fires_2024.RDS")

new_labels = data.frame(ACTIVITY_TYPE = c("Certified_Planted","Fuels","Plant","Stand_Exam",
                                          "Survey_Stocking","TSI","Harvest_NonSalv","Replant","Silv_Prescription","SitePrep_NonChem",
                                          "Thin","Harvest_Salvage","Need_by_Fire","Survey_Other","Survey_Pretreatment",
                                          "SitePrep_Chem","Survey_Survival","Prune","Need_by_Failure","Certified_Thin","Certified_TSI"),
                        type_labels = c("Certification - Plant Trees","Fuel Reduction","Plant Trees","Stand Exam",
                                        "Stocking Survey","TSI","Harvest - Non-Salvage","Replant/Fill-in","Silvicultural Prescription","Site Prep - Manual",
                                        "Thin","Harvest - Salvage","Reforestation Need (Fire)","Survey (Other)","Survey (Pretreatment)",
                                        "Site Prep - Chemical","Survival Survey","Prune","Reforestation Need (Failure)","Thinning Certification",
                                        "Certification - TSI"))
assigned_activities = merge(assigned_activities, new_labels, by = "ACTIVITY_TYPE", all.x = TRUE)


# Summarize cumulative net and gross area for each combo ####
# of fire and activity type for a given timespan
net_gross_cumulative <- function(start,end,assigned_activities){
  
  filtered <- assigned_activities |>
    filter(Ig_Year >= start & Ig_Year + diff_years <= end)
  
  if (dim(filtered)[1] == 0) {
    return(NULL)
  } else{
    filtered$gross_area <- st_area(filtered)
    net_gross <- filtered |> group_by(Event_ID, type_labels) |>
      summarize(
        geometry = st_union(geometry),
        gross_area = sum(gross_area)
      )
    net_gross$net_area <- st_area(net_gross)
    
    result <- net_gross |> group_by(type_labels) |>
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
gross_net_cumulative$diff_years = gross_net_cumulative$end - gross_net_cumulative$start

saveRDS(gross_net_cumulative, "gross_net_cumulative.RDS")

combined_cumulative = st_drop_geometry(gross_net_cumulative)

treat = c("Fuel Reduction", "Harvest - Non-Salvage", "Harvest - Salvage", "Plant Trees",
          "Prune", "Replant/Fill-in", "Site Prep - Chemical", "Site Prep - Manual", "Thin", "TSI")
monitor = c("Plant Certification", "Reforestation Need (Failure)", "Reforestation Need (Fire)",
            "Silvicultural Prescription", "Stand Exam", "Stocking Survey", "Survey (Other)",
            "Survey (Pretreatment)", "Survival Survey","Thin Certification", "TSI Certification")

combined_cumulative_treat = combined_cumulative %>%
  filter(type_labels %in% treat)
combined_cumulative_monitor = combined_cumulative %>%
  filter(type_labels %in% monitor)

# Cumulative area since 1994, only activities within 5 years of ignition
gross_net_cumulative_5years <- map2_dfr(rep(1994,25),c(1994:2018),
                                  net_gross_cumulative,
                                  assigned_activities=assigned_activities[assigned_activities$diff_years<=5,])

gross_net_cumulative_5years$net_area_ac <- as.numeric(gross_net_cumulative_5years$net_area)/4046.86
gross_net_cumulative_5years$gross_area_ac <- as.numeric(gross_net_cumulative_5years$gross_area)/4046.86

saveRDS(gross_net_cumulative_5years, "gross_net_cumulative_5years.RDS")

combined_cumulative_5years = st_drop_geometry(gross_net_cumulative_5years)

combined_cumulative_treat_5years = combined_cumulative_5years %>%
  filter(type_labels %in% treat)
combined_cumulative_monitor_5years = combined_cumulative_5years %>%
  filter(type_labels %in% monitor)

#### Summarize the net and gross area completed for each ignition year ####

# Function to calculate net and gross by ignition year and activity type
net_gross_ignition <- function(nyears,assigned_activities){
  
  filtered <- assigned_activities %>%
    filter(diff_years<=nyears)
  
  if (dim(filtered)[1] == 0) {
    return(NULL)
  } else{
    filtered$gross_area <- st_area(filtered)
    result <- filtered %>% group_by(Ig_Year, type_labels) %>%
      summarize(
        geometry = st_union(geometry),
        gross_area = sum(gross_area)
      )
    result$net_area <- st_area(result)
    result$nyears <- nyears
    return(result)
  }
}

# Map to function, summarize area completed within year for all 30 years by ACTIVITY_TYPE
gross_net_ignition_30years <- map_dfr(rep(1:30),net_gross_ignition,assigned_activities=assigned_activities)
gross_net_ignition_30years$net_area_ac <- as.numeric(gross_net_ignition_30years$net_area)/4046.86
gross_net_ignition_30years$gross_area_ac <- as.numeric(gross_net_ignition_30years$gross_area)/4046.86
new_labels = data.frame(ACTIVITY_TYPE = c("Certified_Planted","Fuels","Plant","Stand_Exam",
                                          "Survey_Stocking","TSI","Harvest_NonSalv","Replant","Silv_Prescription","SitePrep_NonChem",
                                          "Thin","Harvest_Salvage","Need_by_Fire","Survey_Other","Survey_Pretreatment",
                                          "SitePrep_Chem","Survey_Survival","Prune","Need_by_Failure","Certified_TSI"),
                        type_labels = c("Plant Certification","Fuel Reduction","Plant Trees","Stand Exam",
                                        "Stocking Survey","TSI","Harvest - Non-Salvage","Replant/Fill-in","Silvicultural Prescription","Site Prep (Manual)",
                                        "Thin","Harvest - Salvage","Reforestation Need (Fire)","Survey (Other)","Survey (Pretreatment)",
                                        "Site Prep - Chemical","Survival Survey","Prune","Reforestation Need (Failure)","TSI Certification"))
gross_net_ignition_30years = merge(gross_net_ignition_30years, new_labels, by = "ACTIVITY_TYPE", all.x = TRUE)
saveRDS(gross_net_ignition_30years, "gross_net_ignition_30years.RDS")

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

# Map to function, summarize area completed within year for all 30 years by ACTIVITY
gross_net_ignition_30years_all <- map_dfr(rep(1:30),net_gross_ignition,assigned_activities=assigned_activities)
gross_net_ignition_30years$net_area_ac <- as.numeric(gross_net_ignition_30years$net_area)/4046.86
gross_net_ignition_30years$gross_area_ac <- as.numeric(gross_net_ignition_30years$gross_area)/4046.86
new_labels = data.frame(ACTIVITY_TYPE = c("Certified_Planted","Fuels","Plant","Stand_Exam",
                                          "Survey_Stocking","TSI","Harvest_NonSalv","Replant","Silv_Prescription","SitePrep_NonChem",
                                          "Thin","Harvest_Salvage","Need_by_Fire","Survey_Other","Survey_Pretreatment",
                                          "SitePrep_Chem","Survey_Survival","Prune","Need_by_Failure","Certified_TSI"),
                        type_labels = c("Plant Certification","Fuel Reduction","Plant Trees","Stand Exam",
                                        "Stocking Survey","TSI","Harvest - Non-Salvage","Replant/Fill-in","Silvicultural Prescription","Site Prep (Manual)",
                                        "Thin","Harvest - Salvage","Reforestation Need (Fire)","Survey (Other)","Survey (Pretreatment)",
                                        "Site Prep - Chemical","Survival Survey","Prune","Reforestation Need (Failure)","TSI Certification"))
gross_net_ignition_30years = merge(gross_net_ignition_30years, new_labels, by = "ACTIVITY_TYPE", all.x = TRUE)
saveRDS(gross_net_ignition_30years, "gross_net_ignition_30years.RDS")




#### Summarize the net and gross area completed for each combination of ig_year and diff_year ####

## Original method

comb_fire_diff <- expand.grid(fire_year = unique(assigned_activities$Ig_Year),
                              diff_years =unique(assigned_activities$diff_years))

net_activities <-
  map2_dfr(comb_fire_diff$fire_year, comb_fire_diff$diff_years,
           function(x, y, assigned_activities) {
             filtered <- assigned_activities |>
               filter(Ig_Year == x & diff_years <= y)
             
             if (dim(filtered)[1] == 0) {
               return(NULL)
             } else{
               result <- filtered |> group_by(vb_id, type_labels) |>
                 summarize(
                   geometry = st_union(geometry),
                   n_dissolved = n(),
                   Ig_Year = x,
                   diff_years = y
                 )
               result$ref_year <-
                 result$Ig_Year + result$diff_years
               result$net_area <- st_area(result)
               return(result)
             }
           }, assigned_activities = assigned_activities)

gross_activities <-
  map2_dfr(comb_fire_diff$fire_year, comb_fire_diff$diff_years,
           function(x, y, assigned_activities) {
             filtered <- assigned_activities |>
               filter(Ig_Year == x & diff_years <= y)
             
             if (dim(filtered)[1] == 0) {
               return(NULL)
             } else{
               result <- filtered |> group_by(vb_id, type_labels) |>
                 summarize(
                   gross_area = sum(st_area(geometry)),
                   Ig_Year = x,
                   diff_years = y
                 )
               result$ref_year <-
                 result$Ig_Year + result$diff_years
               return(result)
             }
           }, assigned_activities = assigned_activities)


gross_activities$gross_acres = as.numeric(gross_activities$gross_area / 4046.86)
net_activities$net_acres = as.numeric(net_activities$net_area / 4046.86)

saveRDS(gross_activities, "gross_activities.RDS")
saveRDS(net_activities, "net_activities.RDS")

gross_activities = readRDS("gross_activities.RDS")
net_activities = readRDS("net_activities.RDS")

gross_activities_ref_year = gross_activities %>%
  st_drop_geometry() %>%
  group_by(vb_id, ref_year, type_labels) %>%
  summarize(total_gross_acres = sum(gross_acres, na.rm = TRUE))

net_activities_ref_year = st_drop_geometry(net_activities)

gross_net_activity_year = merge(gross_activities, net_activities, by = "vb_id")


## Another attempt

combined_net_gross_activities <- function(assigned_activities) {
  comb_fire_diff <- expand.grid(fire_year = unique(assigned_activities$Ig_Year),
                                diff_years = unique(assigned_activities$diff_years))
  
  map2_dfr(comb_fire_diff$fire_year, comb_fire_diff$diff_years,
           function(x, y, assigned_activities) {
             filtered <- assigned_activities |>
               filter(Ig_Year == x & diff_years <= y)
             
             if (dim(filtered)[1] == 0) {
               return(NULL)
             } else {
               # Calculate gross area
               gross_result <- filtered |> 
                 group_by(Event_ID, type_labels) |>
                 summarize(
                   gross_acres = sum(st_area(geometry)/4046.86),
                   Ig_Year = x,
                   diff_years = y,
                 )
               
               # Calculate net area
               net_result <- filtered |> 
                 group_by(Event_ID, type_labels) |>
                 summarize(
                   geometry = st_union(geometry),
                   n_dissolved = n(),
                   Ig_Year = x,
                   diff_years = y,
                 )
               net_result$net_acres <- st_area(net_result$geometry)/4046.86
               
               # Combine results
               combined_result <- left_join(gross_result, 
                                            net_result |> select(-geometry), 
                                            by = c("Event_ID", "type_labels", "Ig_Year", "diff_years"))
               
               combined_result$ref_year <- combined_result$Ig_Year + combined_result$diff_years
               
               return(combined_result)
             }
           }, assigned_activities = assigned_activities)
}

# Usage
result <- combined_net_gross_activities(assigned_activities)
combined_net_gross_activities$net_area_ac <- as.numeric(combined_net_gross_activities$net_area)/4046.86
combined_net_gross_activities$gross_area_ac <- as.numeric(combined_net_gross_activities$gross_area)/4046.86
saveRDS(result, "combined_net_gross_activities.RDS")



## Cumulative gross and net by Activity Year

# Function to calculate gross acres
calculate_gross_acres <- function(assigned_activities) {
  gross_activities1 <- assigned_activities %>%
    group_by(vb_id, type_labels, year) %>%
    summarize(
      gross_area = sum(as.numeric(st_area(geometry))),
      .groups = "drop"
    ) %>%
    mutate(gross_acres = gross_area / 4046.86)
  
  return(gross_activities1)
}

# Function to calculate net acres
calculate_net_acres <- function(assigned_activities) {
  net_activities1 <- assigned_activities %>%
    group_by(vb_id, type_labels, year) %>%
    summarize(
      geometry = st_union(geometry),
      n_dissolved = n(),
      .groups = "drop"
    ) %>%
    mutate(
      net_area = as.numeric(st_area(geometry)),
      net_acres = net_area / 4046.86
    )
  
  return(net_activities1)
}

# Function to combine gross and net acres
combine_gross_net_acres <- function(gross_activities1, net_activities1) {
  combined_activities <- gross_activities1 %>%
    select(vb_id, type_labels, year, gross_acres) %>%
    left_join(
      net_activities1 %>%
        st_drop_geometry() %>%
        select(vb_id, type_labels, year, net_acres, n_dissolved),
      by = c("vb_id", "type_labels", "year")
    )
  
  return(combined_activities)
}

# Main function to process the data
process_activities <- function(assigned_activities) {
  gross_activities1 <- calculate_gross_acres(assigned_activities)
  net_activities1 <- calculate_net_acres(assigned_activities)
  combined_activities <- combine_gross_net_acres(gross_activities1, net_activities1)
  
  return(list(
    gross_activities1 = gross_activities1,
    net_activities1 = net_activities1,
    combined_activities = combined_activities
  ))
}

# Usage
results <- process_activities(assigned_activities)
gross_net_activity_year = results$combined_activities %>%
  st_drop_geometry()

# Save results
saveRDS(results$gross_activities, "gross_activities1.RDS")
saveRDS(results$net_activities, "net_activities1.RDS")
saveRDS(results$combined_activities, "combined_activities.RDS")

# Print the first few rows of the combined results
print(head(results$combined_activities))



#### Simplified gross and net summary ####

gross_net_summarize_by_year <- function(assigned_activities) {
  assigned_activities %>%
    # Create unique fire yearname
    unite("vb_id", Ig_Year:Incid_Name, sep = "_", remove = FALSE) %>%
    # Group and summarize
    group_by(vb_id, Incid_Name, Ig_Year, type_labels, year) %>%
    summarize(
      gross_acres = sum(st_area(geometry)) / 4046.86,
      net_acres = st_area(st_union(geometry)) / 4046.86,
      n_dissolved = n(),
      diff_years = first(year) - first(Ig_Year),
      .groups = "drop"
    )
}


# Usage
gross_net_summary1 <- gross_net_summarize_by_year(assigned_activities)

