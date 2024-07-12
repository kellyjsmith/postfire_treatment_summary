# SUMMARY TABLES BY CATEGORY ####

library(dplyr)
library(sf)
library(tidyverse)

setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output")

assigned_activities = readRDS("assigned_activities_2024.RDS")
facts_fires = readRDS("facts_fires_2024.RDS")
gross_net_nyears = readRDS("gross_net_nyears.RDS")


# Convert sf objects to dataframes
gross_net_df = st_drop_geometry(gross_net_nyears)
assigned_df = st_drop_geometry(assigned_activities)
facts_df = st_drop_geometry(facts_fires$fires_activities)


# CREATE IS_* fields for FACTS
fields <- c(types,"treat","manage","monitor")
for(i in fields){
  categories <- eval(parse(text=i))
  is_cat <- facts_df$ACTIVITY%in%categories
  facts_df[,paste0("IS_",i)]<-is_cat
} 

# CREATE ACTIVITY TYPE for FACTS
types <- types
facts_df$ACTIVITY_TYPE<-NA
for(i in types){
  print(i)
  categories <- eval(parse(text=i))
  is_cat <- facts_df$ACTIVITY%in%categories
  facts_df[,"ACTIVITY_TYPE"]<-ifelse(is_cat,i,facts_df$ACTIVITY_TYPE)
} 


# Remove embedded units from values
units(assigned_df$activity_fire_area) <- NULL
units(facts_df$activity_area) <- NULL

# Summarize total and postfire activity acres by all activities and types
total_facts =  facts_df %>%
  group_by(ACTIVITY_TYPE,ACTIVITY) %>%
  summarize(total_facts_acres = sum(activity_area/4046.86, na.rm = TRUE))
total_postfire <- assigned_df %>%
  group_by(ACTIVITY_TYPE,ACTIVITY) %>%
  summarise(total_postfire_acres = sum(activity_fire_area/4046.86, na.rm = TRUE))
total_summary = full_join(total_facts,total_postfire,by="ACTIVITY")
write.csv(total_summary,"acreage_summary_all_activities.csv")


# Summarize activity_area by ACTIVITY_TYPE for facts_df
facts_acreage <- facts_df %>%
  group_by(ACTIVITY_TYPE) %>%
  summarise(facts_activity_acres = sum(activity_area/4046.86, na.rm = TRUE))

# Summarize activity_area by ACTIVITY_TYPE for assigned_df
assigned_acreage <- assigned_df %>%
  group_by(ACTIVITY_TYPE) %>%
  summarise(assigned_activity_acres = sum(activity_fire_area/4046.86, na.rm = TRUE))

split_acreage_5years <- gross_net_df %>%
  filter(nyears==5) %>%
  group_by(ACTIVITY_TYPE) %>%
  summarise(gross_acres_5years = sum(gross_area_ac, na.rm = TRUE),
            net_acres_5years = sum(net_area_ac, na.rm = TRUE))

split_acreage_10years <- gross_net_df %>%
  filter(nyears==10) %>%
  group_by(ACTIVITY_TYPE) %>%
  summarise(gross_acres_10years = sum(gross_area_ac, na.rm = TRUE),
            net_acres_10years = sum(net_area_ac, na.rm = TRUE))

# Combine the four tables
combined_acreage_summary <- full_join(facts_acreage, assigned_acreage, by = "ACTIVITY_TYPE") %>%
  full_join(split_acreage_5years, by = "ACTIVITY_TYPE") %>%
  full_join(split_acreage_10years, by = "ACTIVITY_TYPE")
write.csv(combined_acreage_summary, "combined_acreage_summary.csv")





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
  plant = data %>% filter(ACTIVITY_TYPE == "Plant")
  certified = data %>% filter(ACTIVITY_TYPE == "Certified_Planted")
  
  joined <- st_join(plant, certified, join = st_intersects, suffix = c("_plant", "_cert"))
  
  result <- joined %>%
    group_by(Ig_Year_plant,Event_ID_plant) %>%
        summarise(
          total_planted_acres = sum(activity_area_plant/4046.86),
          certified_acres = sum(!is.na(SUID_cert), activity_area_cert/4046.86),
          prop_certified = certified_acres / total_planted_acres,
        )
    }
  
  # Visualization
  plot <- ggplot(result, aes(x = "", y = prop_certified, fill = "Certified")) +
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

