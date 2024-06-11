# SUMMARY TABLES BY CATEGORY ####


# Convert sf objects to dataframes
gross_net_df = st_drop_geometry(gross_net_nyears)
assigned_df = st_drop_geometry(assigned_activities)
facts_df = st_drop_geometry(facts_fires$fires_activities)



# CREATE IS_* fields for FACTS
fields <- c("planting","harvest_salvage","harvest","prep","release","thin","replant","survey",
            "prune","fuel","cert_planted","cert_tsi","review","need","manage.except.plant","manage")
for(i in fields){
  categories <- eval(parse(text=i))
  is_cat <- facts_df$ACTIVITY%in%categories
  facts_df[,paste0("IS_",i)]<-is_cat
} 

# CREATE ACTIVITY TYPE for FACTS
types <- c("planting","harvest_salvage","harvest","prep","release","thin","replant",
           "prune","fuel","cert_planted","cert_tsi","survey","review","need")
facts_df$ACTIVITY_TYPE<-NA
for(i in types){
  print(i)
  categories <- eval(parse(text=i))
  is_cat <- facts_df$ACTIVITY%in%categories
  facts_df[,"ACTIVITY_TYPE"]<-ifelse(is_cat,i,facts_df$ACTIVITY_TYPE)
} 



# Remove embedded units from values
units(net_activities_df$net_area) <- NULL
units(gross_activities_df$gross_area) <- NULL
units(assigned_df$activity_fire_area) <- NULL
units(facts_df$activity_area) <- NULL


# Summarize activity_area by ACTIVITY_TYPE for facts_df
facts_acreage <- facts_df %>%
  group_by(ACTIVITY_TYPE) %>%
  summarise(facts_activity_acres = sum(activity_area/4046.86, na.rm = TRUE))

# Summarize activity_area by ACTIVITY_TYPE for assigned_df
assigned_acreage <- assigned_df %>%
  group_by(ACTIVITY_TYPE) %>%
  summarise(assigned_activity_acres = sum(activity_fire_area/4046.86, na.rm = TRUE))

# # Summarize activity_area by ACTIVITY_TYPE for net_activities_df
# net_acreage <- net_activities_df %>%
#   group_by(ACTIVITY_TYPE) %>%
#   summarise(net_activity_acres = sum(net_area/4046.86, na.rm = TRUE))
# 
# # Summarize activity_area by ACTIVITY_TYPE for gross_activities_df
# gross_acreage <- gross_activities_df %>%
#   group_by(ACTIVITY_TYPE) %>%
#   summarise(gross_activity_acres = sum(gross_area/4046.86, na.rm = TRUE))

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
