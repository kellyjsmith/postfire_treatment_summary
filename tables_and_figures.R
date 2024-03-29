## Tables & Figures for summarizing postfire activities

library("sf")
library("dplyr")
library("terra")
library("tidyverse")

assigned_activities = readRDS("assigned_activities.RDS")

assigned_activities = assigned_activities %>%
  mutate(activity_fire_acres = activity_fire_area/4046.86)

a <- assigned_activities |>filter((diff_years<10) & (IS_planting==TRUE | IS_prep==TRUE |IS_release==TRUE))
a <- pivot_longer(assigned_activities,cols=starts_with("IS_"),names_to = "type",values_to = "IS_type")
a$type <- gsub("IS_","",a$type)
a <- a[a$IS_type,]
a <- group_by(a,diff_years,Ig_Year,type) |> summarize(activity_fire_area=sum(activity_fire_area))

# Filter 5-year Activities
activities_summary_10years = assigned_activities %>%
  filter(diff_years < 10)


# Planting Summary & Trends
planting_10years <- assigned_activities %>%
  filter(diff_years < 10 & IS_planting)

planting_summary = planting_10years %>%
  group_by(Ig_Year, VB_ID) %>%
  summarize(years_to_first_plant = min(diff_years), avg_years_to_plant = mean(diff_years),
            mode(diff_years), net_acres_planted = sum(activity_fire_acres))

planting_trends = planting_summary %>%
  group_by(Ig_Year) %>%
  summarize(wtd_avg_years_to_plant = mean(avg_years_to_plant))

ggplot(planting_trends) +
  aes(x = Ig_Year, y = wtd_avg_years_to_plant) +
  geom_bar() +
  geom_smooth(method = "lm") +
  scale_x_binned("Year of Ignition", breaks = seq(1993, 2017, by = 3)) +
  scale_y_continuous("Average Number of Years Until Planting")

ggplot(planting_trends) +
  aes(Ig_Year) +
  geom_bar(weight = wtd_avg_years_to_plant) +
  geom_smooth(method = "lm") +
  scale_x_discrete("Year of Ignition") +
  scale_y_continuous("Average Number of Years Until Planting")

ggplot(a)+ aes(x=diff_years,y=Ig_Year,fill=as.numeric(activity_fire_area)/4046.86) + 
  facet_wrap(~type,ncol=3) +
  geom_tile(stat = "identity" , height=1,width=1,color="grey") + scale_fill_gradient("acres") +
  scale_x_continuous(breaks=c(0:10)) + scale_y_continuous(breaks=seq(1993,2017,by=2)) + 
  xlim(c(0,15)) + ylim(c(1993,2017))


# Release Summary & Trends
release_10years <- assigned_activities %>%
  filter(diff_years < 10 & IS_release)

release_summary = release_10years %>%
  group_by(Ig_Year, VB_ID) %>%
  summarize(years_to_first_plant = min(diff_years), avg_years_to_plant = mean(diff_years),
            mode(diff_years), net_acres_planted = sum(activity_fire_acres))

release_trends = release_summary %>%
  group_by(Ig_Year) %>%
  summarize(wtd_avg_years_to_plant = mean(avg_years_to_plant))

ggplot(release_trends) +
  aes(x = Ig_Year, y = wtd_avg_years_to_plant) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous("Year of Ignition", breaks = seq(1993, 2017, by = 3)) +
  scale_y_continuous("Average Number of Years Until Release")



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





ggplot(a)+ aes(x=diff_years,y=Ig_Year,fill=as.numeric(activity_fire_area)/4046.86) + 
  facet_wrap(~type,ncol=3) +
  geom_tile(stat = "identity" , height=1,width=1,color="grey") + scale_fill_gradient("acres") +
  scale_x_continuous(breaks=c(0:10)) + scale_y_continuous(breaks=seq(1993,2017,by=2)) + 
  xlim(c(0,15)) + ylim(c(1993,2017))


ggplot(filter(assigned_activities, year <= 2018 & year >= 1998)) +
  aes(x = year, y = as.numeric(activity_fire_area) / 4046.86) +
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

