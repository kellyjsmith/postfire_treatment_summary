# SUMMARY TABLES BY CATEGORY ####

library(dplyr)
library(sf)
library(tidyverse)

setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output")
setwd("~/Library/CloudStorage/Box-Box/Kelly_postfire_reforestation_project/Output")

assigned_activities_df = readRDS("assigned_activities_2024.RDS") %>%
  st_drop_geometry()
combined_ignition_year = readRDS("combined_ignition_year.RDS")
  st_drop_geometry()
combined_5yr_df = readRDS("combined_ignition_year_5yr.RDS") %>%
  st_drop_geometry()

units(combined_df$gross_acres) <- NULL
units(combined_df$net_acres) <- NULL
units(combined_5yr_df$net_acres) <- NULL




# # Summarize total and postfire activity acres by all activities and types
# total_facts =  assigned_df %>%
#   group_by(type_labels,ACTIVITY) %>%
#   summarize(total_facts_acres = sum(GIS_ACRES, na.rm = TRUE))
# total_postfire <- assigned_df %>%
#   group_by(type_labels,ACTIVITY) %>%
#   summarise(total_postfire_acres = sum(activity_fire_area/4046.86, na.rm = TRUE))
# total_summary = full_join(total_facts,total_postfire,by="ACTIVITY") %>%
#   select(-type_labels.y)
# write.csv(total_summary,"acreage_summary_all_activities.csv")


# Summarize activity_area by ACTIVITY_TYPE for facts_df
facts_acres <- assigned_df %>%
  group_by(type_labels) %>%
  summarise("Total FACTS Acres" = sum(GIS_ACRES, na.rm = TRUE))

gross_postfire_acres <- combined_df %>%
  group_by(type_labels) %>%
  summarise("Gross Postfire Acres" = sum(gross_acres, na.rm = TRUE))

net_postfire_acres <- combined_df %>%
  group_by(type_labels) %>%
  summarise("Net Postfire Acres" = sum(net_acres, na.rm = TRUE))

net_postfire_acres_5yr = combined_5yr_df %>%
  group_by(type_labels) %>%
  summarize("Net Acres <= 5 Yrs" = sum(net_acres, na.rm = TRUE))


# Combine the four tables
combined_acreage_summary <- full_join(facts_acres, gross_postfire_acres, by = "type_labels") %>%
  full_join(net_postfire_acres, by = "type_labels") %>%
  full_join(net_postfire_acres_5yr, by = "type_labels")
write.csv(combined_acreage_summary, "combined_acreage_summary.csv")

fire_summary = combined_ignition_year %>%
  filter(type_labels == "Plant Trees") %>%
  group_by(Ig_Year, Incid_Name) %>%
  summarize(total_net = sum(net_acres))



# Certification summaries
plant_cert_prop = gross_net_time %>%
  filter(ACTIVITY_TYPE %in% c("Plant","Cert_Planted")) %>%
  pivot_wider(id_cols = c("start", "end"),
              names_from = "ACTIVITY_TYPE",
              values_from = "net_area")


plant_only = assigned_df %>%
  filter(ACTIVITY_TYPE == "Plant")
plant_cert_only = assigned_df %>%
  filter(ACTIVITY_TYPE == "Certified_Planted")
plant_cert_merge_allcert = merge(plant_only,plant_cert_only,
                             by = "SUID",
                             all.x = FALSE, all.y = TRUE)
plant_cert_merge_allplant = merge(plant_only,plant_cert_only,
                                 by = "SUID",
                                 all.x = TRUE, all.y = FALSE)
not_certified = plant_cert_merge_allplant %>%
  filter(ACTIVITY.y = NA)


analyze_certification <- function(data) {{
  
  #Split data frame
  plant = data %>% filter(type_labels == "Plant Trees")
  cert = data %>% filter(type_labels == "Certified - Plant")
  
  plantcert <- st_join(st_as_sf(plant), st_as_sf(cert), join = st_intersects, suffix = c("_plant", "_cert"))
  
  result <- plantcert %>%
    group_by(Ig_Year_plant,Event_ID_plant) %>%
        summarise(
          total_planted_acres = sum(activity_area_plant/4046.86),
          certified_acres = sum(!is.na(SUID_cert), activity_area_cert/4046.86),
          prop_certified = certified_acres / total_planted_acres,
        )
    }
  
  # Visualization
  plot <- ggplot(result, aes(x = plant_year, y = prop_certified, fill = "Certified")) +
    geom_bar(stat = "identity", width = 1) +
    geom_bar(aes(y = prop_not_certified, fill = "Not Certified"), stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    scale_fill_manual(values = c("Certified" = "green", "Not Certified" = "red")) +
    labs(title = "Proportion of Certified Plantings")
  
  list(result = result, plot = plot)
}

# Usage
results <- analyze_certification(assigned_activities)
print(results$result)
print(results$plot)

