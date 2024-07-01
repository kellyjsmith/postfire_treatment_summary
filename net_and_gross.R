
# calculate gross vs. net areas by fire and activity


# read intersected and assigned activity polygons
# assigned_activities<-readRDS("assigned_activities_2022.RDS")
assigned_activities<-readRDS("assigned_activities.RDS")

# convert split area (activity_fire_area, m^2) to acres
assigned_activities = assigned_activities %>%
  mutate(activity_fire_acres = activity_fire_area/4046.86)

# create tibble representing all combinations of fire year and diff_year
comb_fire_diff <- expand.grid(fire_year = unique(assigned_activities$Ig_Year),
                              diff_years =unique(assigned_activities$diff_years))

# create list of treatment categories for assigning activity type
types <- c("planting","salvage","prep","release","thin","replant","prune","fuel","cert","review","need")

# Create activity type fields
assigned_activities$ACTIVITY_TYPE<-NA
facts$ACTIVITY_TYPE = NA

for(i in types){
  print(i)
  categories <- eval(parse(text=i))
  is_cat <- assigned_activities$ACTIVITY%in%categories
  assigned_activities[,"ACTIVITY_TYPE"]<-ifelse(is_cat,i,assigned_activities$ACTIVITY_TYPE)
}

for(i in types){
  print(i)
  categories <- eval(parse(text=i))
  is_cat <- facts$ACTIVITY%in%categories
  facts[,"ACTIVITY_TYPE"]<-ifelse(is_cat,i,facts$ACTIVITY_TYPE)
}

# loop through each pair of fire_year and diff_year combos and filter the assigned_activities
  # where ig_year = diff_year. For each combo, group by fire id and activity type,then
  # summarize within that grouping by unioning (net) or totaling (gross) the geometries
net_activities_v2 <- map2_dfr(comb_fire_diff$fire_year,comb_fire_diff$diff_years,
                           function(x,y,assigned_activities){
                             
                             filtered <- assigned_activities |> 
                               filter(Ig_Year ==x & diff_years==y)
                             
                             if(dim(filtered)[1]==0){
                               return(NULL)
                             }else{
                               result <- filtered |> group_by(assigned_fire,ACTIVITY_TYPE)|> 
                                 summarize(geometry=st_union(geometry),n_dissolved=n(),
                                           Ig_Year=first(Ig_Year),diff_years=first(diff_years))
                               result$ref_year <-result$Ig_Year+result$diff_years
                               result$net_area <- st_area(result)
                               return(result)
                             }
                           },assigned_activities=assigned_activities)


gross_activities_v2 <- map2_dfr(comb_fire_diff$fire_year,comb_fire_diff$diff_years,
                             function(x,y,assigned_activities){
                               
                               filtered <- assigned_activities |> 
                                 filter(Ig_Year ==x & diff_years==y)
                               
                               if(dim(filtered)[1]==0){
                                 return(NULL)
                               }else{
                                 result <-filtered |> group_by(Event_ID,ACTIVITY_TYPE)|> 
                                   summarize(gross_area=sum(st_area(geometry)),
                                             Ig_Year=first(Ig_Year),diff_years=first(diff_years))
                                 result$ref_year <-result$Ig_Year+result$diff_years
                                 return(result)
                               }
                             },assigned_activities=assigned_activities)