

## Tables & Figures for summarizing postfire activities

library("sf")
library("dplyr")
library("terra")
library("tidyverse")
library("units")

setwd("C:/Users/smithke3/OneDrive - Oregon State University/Kelly/Output")

assigned_activities = readRDS("assigned_activities_2022.RDS")
facts_fires = readRDS("facts_fires_2022.RDS")

assigned_activities = assigned_activities %>%
  mutate(VB_ID = paste(Incid_Name, Ig_Year, sep = " - "))


comb_fire_diff <- expand.grid(fire_year = unique(assigned_activities$Ig_Year),
                              diff_years =unique(assigned_activities$diff_years))

net_activities_v2 <-
  map2_dfr(comb_fire_diff$fire_year, comb_fire_diff$diff_years,
           function(x, y, assigned_activities) {
             filtered <- assigned_activities |>
               filter(Ig_Year == x & diff_years == y)
             
             if (dim(filtered)[1] == 0) {
               return(NULL)
             } else{
               result <- filtered |> group_by(VB_ID, ACTIVITY_TYPE) |>
                 summarize(
                   geometry = st_union(geometry),
                   n_dissolved = n(),
                   Ig_Year = first(Ig_Year),
                   diff_years = first(diff_years)
                 )
               result$ref_year <-
                 result$Ig_Year + result$diff_years
               result$net_area <- st_area(result)
               return(result)
             }
           }, assigned_activities = assigned_activities)


gross_activities_v2 <-
  map2_dfr(comb_fire_diff$fire_year, comb_fire_diff$diff_years,
           function(x, y, assigned_activities) {
             filtered <- assigned_activities |>
               filter(Ig_Year == x &
                        diff_years == y)
             
             if (dim(filtered)[1] == 0) {
               return(NULL)
             } else{
               result <- filtered |> group_by(VB_ID, ACTIVITY_TYPE) |>
                 summarize(
                   gross_area = sum(st_area(geometry)),
                   Ig_Year = first(Ig_Year),
                   diff_years = first(diff_years)
                 )
               result$ref_year <-
                 result$Ig_Year + result$diff_years
               return(result)
             }
           }, assigned_activities = assigned_activities)



# net_over_gross <- merge()

# Convert sf object to data frame
gross_activities_df = st_drop_geometry(gross_activities)
net_activities_df = st_drop_geometry(net_activities)
assigned_df = st_drop_geometry(assigned_activities)
facts = st_drop_geometry(facts_fires$fires_activities)

incids = assigned_df %>%
  mutate(VB_ID = paste(Incid_Name, Ig_Year, sep = " - ")) %>%
  group_by(VB_ID) %>%
  summarize(
    n_events = n_distinct(Event_ID),
    events = list(unique(Event_ID))
  )

incids_unnested = unnest(incids, events)
net_activities_df = merge(net_activities_df, incids_unnested, by.x = "Event_ID", by.y = "events", all.x = TRUE)


# drop units
units(net_activities_df$net_area) <- NULL
units(gross_activities_df$gross_area) <- NULL
units(assigned_df$activity_fire_acres) <- NULL
