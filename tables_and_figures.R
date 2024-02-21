## Tables & Figures for summarizing postfire activities

library("sf")
library("terra")
library("tidyverse")

assigned_activities = readRDS("assigned_activities.RDS")

# a <- assigned_activities |>filter((diff_years<10) & (IS_planting==TRUE | IS_prep==TRUE |IS_release==TRUE))
a <- pivot_longer(assigned_activities,cols=starts_with("IS_"),names_to = "type",values_to = "IS_type")
a$type <- gsub("IS_","",a$type)
a <- a[a$IS_type,]
a <- group_by(a,diff_years,Ig_Year,type) |> summarize(activity_fire_area=sum(activity_fire_area))

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

hist()

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
