## Tables & Figures for summarizing postfire activities

library("sf")
library("terra")
library("tidyverse")

assigned_activities = readRDS("assigned_activities.RDS")

assigned_activities = assigned_activities %>%
  mutate(activtiy_fire_acres = activity_fire_area/4046.86)

# a <- assigned_activities |>filter((diff_years<10) & (IS_planting==TRUE | IS_prep==TRUE |IS_release==TRUE))
a <- pivot_longer(assigned_activities,cols=starts_with("IS_"),names_to = "type",values_to = "IS_type")
a$type <- gsub("IS_","",a$type)
a <- a[a$IS_type,]
a <- group_by(a,diff_years,Ig_Year,type) |> summarize(activity_fire_area=sum(activity_fire_area))



# Filter relevant activities (e.g., planting, salvage, etc.)
plantings_10years <- assigned_activities %>%
  filter(diff_years < 10 & IS_planting)

plantings_summary = plantings_10years %>%
  group_by(Ig_Year)
  summarize(years_to_plant = min(diff_years, na.rm = TRUE))

# Summarize total treated area by year and activity type
summary_table_year <- filtered_activities %>%
  group_by(VB_ID, year, ACTIVITY) %>%
  summarize(total_area = sum(activity_fire_acres))

# Calculate average number of years between fire and first planting treatment
summary_table_fires <- filtered_activities %>%
  filter(IS_planting) %>%
  group_by(VB_ID, ACTIVITY) %>%
  mutate(first_plant = min(year)) %>%
  mutate(years_to_planting = first_plant - Ig_Year)
  # summarize(avg_years_to_planting = mean(years_to_planting))



ggplot(a)+ aes(x=diff_years,y=Ig_Year,fill=as.numeric(activity_fire_area)/4046.86) + 
  facet_wrap(~type,ncol=3) +
  geom_tile(stat = "identity" , height=1,width=1,color="grey") + scale_fill_gradient("acres") +
  scale_x_continuous(breaks=c(0:10)) + scale_y_continuous(breaks=seq(1993,2017,by=2)) + 
  xlim(c(0,15)) + ylim(c(1993,2017))

ggplot(a)+ aes(x=diff_years,y=Ig_Year,fill=as.numeric(activity_fire_area)/4046.86) + 
  facet_wrap(~type,ncol=3) +
  geom_tile(stat = "identity" , height=1,width=1,color="grey") + scale_fill_gradient("acres") +
  scale_x_continuous(breaks=c(0:10)) + scale_y_continuous(breaks=seq(1993,2017,by=2)) + 
  xlim(c(0,15)) + ylim(c(1993,2017))

ggplot(filter(assigned_activities, activity_year <= 2018 & activity_year >= 1998)) +
  aes(x = activity_year, y = as.numeric(activity_fire_area) / 4046.86) +
  facet_wrap(~type, ncol = 3) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous("Year of Activity", breaks = seq(1998, 2022, by = 4)) +
  scale_y_continuous("Acres")



a$activity_year <- a$Ig_Year +a$diff_years
b <- group_by(a,activity_year,type) |> summarize(activity_fire_area=sum(activity_fire_area))

ggplot(filter(b,activity_year<=2018 & activity_year>=1998))+ 
  aes(x=activity_year,y=as.numeric(activity_fire_area)/4046.86) + 
  facet_wrap(~type,ncol=3) + geom_point() + geom_smooth(method="lm") +
  scale_x_continuous("Year activity",breaks=seq(1998,2022,by=4)) +
  scale_y_continuous("Acres")


ggplot(filter(b,activity_year<=2022 & activity_year>=1993 & type=="prep"))+ 
  aes(x=activity_year,y=as.numeric(activity_fire_area)/4046.86) + 
  facet_wrap(~type,ncol=3) + geom_point() + geom_smooth(method="lm") +
  scale_x_continuous("Year activity",breaks=seq(1998,2022,by=4)) +
  scale_y_continuous("Acres")

