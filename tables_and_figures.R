## Tables & Figures for summarizing postfire activities

library("sf")
library("dplyr")
library("terra")
library("tidyverse")

assigned_activities = readRDS("assigned_activities.RDS")

a <- assigned_activities |>filter((diff_years<10) & (IS_planting==TRUE | IS_prep==TRUE |IS_release==TRUE))
a <- pivot_longer(assigned_activities,cols=starts_with("IS_"),names_to = "type",values_to = "IS_type")
a$type <- gsub("IS_","",a$type)
a <- a[a$IS_type,]
a <- group_by(a,diff_years,Ig_Year,type) |> summarize(activity_fire_area=sum(activity_fire_area))

assigned_df = as.data.frame(assigned_activities)
facts_df = as.data.frame(facts)

# gross vs net areas
assigned_activities<-readRDS("assigned_activities_2022.RDS")
assigned_activities = assigned_activities %>%
  mutate(activity_fire_acres = activity_fire_area/4046.86)
comb_fire_diff <- expand.grid(fire_year = unique(assigned_activities$Ig_Year),
                              diff_years =unique(assigned_activities$diff_years))
types <- c("planting","salvage","prep","release","thin","replant","prune","fuel")
assigned_activities$ACTIVITY_TYPE<-NA
facts$ACTIVITY_TYPE = NA

# Create activity type fields
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


# Summarize activity_area by ACTIVITY_TYPE for facts_df
summary_facts <- facts_df %>%
  group_by(ACTIVITY_TYPE) %>%
  summarise(facts_activity_acres = sum(activity_area/4046.86, na.rm = TRUE))

# Summarize activity_area by ACTIVITY_TYPE for assigned_df
summary_assigned <- assigned_df %>%
  group_by(ACTIVITY_TYPE) %>%
  summarise(assigned_activity_acres = sum(activity_area/4046.86, na.rm = TRUE))

# Summarize activity_area by ACTIVITY_TYPE for net_activities_df
summary_net <- net_activities_df %>%
  group_by(ACTIVITY_TYPE) %>%
  summarise(net_activity_acres = sum(net_area/4046.86, na.rm = TRUE))

# Summarize activity_area by ACTIVITY_TYPE for gross_activities_df
summary_gross <- gross_activities_df %>%
  group_by(ACTIVITY_TYPE) %>%
  summarise(gross_activity_acres = sum(gross_area/4046.86, na.rm = TRUE))

# Combine the four tables
combined_summary <- full_join(summary_facts, summary_assigned, by = "ACTIVITY_TYPE") %>%
  full_join(summary_net, by = "ACTIVITY_TYPE") %>%
  full_join(summary_gross, by = "ACTIVITY_TYPE")

# Plot acreage summary by category with facets
ggplot(combined_filter1, aes(x = Ig_Year)) +
  geom_line(aes(y = gross_acres, color = "Gross Acres")) +
  geom_line(aes(y = net_acres, color = "Net Acres")) +
  labs(x = "Ignition Year", y = "Value", color = "Type of Acres") +
  scale_x_continuous(breaks = seq(1993, 2017, 3)) +
  facet_wrap(~ ACTIVITY_TYPE) +
  theme_minimal()

check_overlaps = assigned_activities %>%
  mutate(activity_acres = activity_area/4046.86) %>%
  mutate(intersecting_acres = intersecting_area/4046.86) %>%
  group_by(facts_polygon_id) %>%
  add_count() %>%
  filter(n >= 2)

net_activities <- map2_dfr(comb_fire_diff$fire_year,comb_fire_diff$diff_years,
                           function(x,y,assigned_activities){
                             
                             filtered <- assigned_activities |> 
                               filter(Ig_Year ==x & diff_years==y)
                             
                             if(dim(filtered)[1]==0){
                               return(NULL)
                             }else{
                               result <-filtered |> group_by(VB_ID,ACTIVITY_TYPE)|> 
                                 summarize(geometry=st_union(geometry),n_dissolved=n(),
                                           Ig_Year=first(Ig_Year),diff_years=first(diff_years))
                               result$ref_year <-result$Ig_Year+result$diff_years
                               result$net_area <- st_area(result)
                               return(result)
                             }
                           },assigned_activities=assigned_activities)


gross_activities <- map2_dfr(comb_fire_diff$fire_year,comb_fire_diff$diff_years,
                             function(x,y,assigned_activities){
                               
                               filtered <- assigned_activities |> 
                                 filter(Ig_Year ==x & diff_years==y)
                               
                               if(dim(filtered)[1]==0){
                                 return(NULL)
                               }else{
                                 result <-filtered |> group_by(VB_ID,ACTIVITY_TYPE)|> 
                                   summarize(gross_area=sum(st_area(geometry)),
                                             Ig_Year=first(Ig_Year),diff_years=first(diff_years))
                                 result$ref_year <-result$Ig_Year+result$diff_years
                                 return(result)
                               }
                             },assigned_activities=assigned_activities)





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

  
gross_net_diff = setdiff(gross_activities$gross_area - net_activities$net_area)


# Convert sf object to data frame
gross_activities_10years_df = as.data.frame(gross_activities_10years)
gross_activities_df = as.data.frame(gross_activities)
net_activities_df = as.data.frame(net_activities)


# Group by ig_year and summarize acres
gross_by_ig_year = gross_activities_df %>%
  group_by(Ig_Year, ACTIVITY_TYPE) %>%
  summarize(gross_acres = as.numeric(sum(gross_area)/4046.86), .groups = 'drop') %>%
  spread(key = ACTIVITY_TYPE, value = gross_acres)
net_by_ig_year = net_activities_df %>%
  group_by(Ig_Year, ACTIVITY_TYPE) %>%
  summarize(net_acres = as.numeric(sum(net_area)/4046.86), .groups = 'drop') %>%
  spread(key = ACTIVITY_TYPE, value = net_acres)



# Filter the data frames for the specified columns
net_by_ig_year_filter1 = net_by_ig_year[, c("Ig_Year", "planting", "release", "replant", "thin")]
gross_by_ig_year_filter1 = gross_by_ig_year[, c("Ig_Year", "planting", "release", "replant", "thin")]

# Convert the data frames from wide to long format
net_by_ig_year_filter1_long = tidyr::gather(net_by_ig_year_filter1, ACTIVITY_TYPE, value, -Ig_Year)
gross_by_ig_year_filter1_long = tidyr::gather(gross_by_ig_year_filter1, ACTIVITY_TYPE, value, -Ig_Year)

# Add a new column to identify the type of acres
net_by_ig_year_filter1_long$type = "net_acres"
gross_by_ig_year_filter1_long$type = "gross_acres"

# Combine the data frames
combined_ig_year_filter1 = rbind(net_by_ig_year_filter1_long, gross_by_ig_year_filter1_long)
# Spread the data frame to have gross_acres and net_acres in separate columns
combined_ig_year_filter1 = spread(combined_ig_year_filter1, key = type, value = value)

# Add a new column for the difference between gross and net acres
combined_ig_year_filter1$difference = combined_filter1$gross_acres - combined_filter1$net_acres

# Plot difference
ggplot(combined_filter1, aes(x = Ig_Year)) +
  geom_line(aes(y = difference, color = ACTIVITY_TYPE)) +
  labs(x = "Ignition Year", y = "Difference (Gross Acres - Net Acres)", color = "Activity Type") +
  scale_x_continuous(breaks = seq(1993, 2017, 3)) +
  facet_wrap(~ ACTIVITY_TYPE) +
  theme_minimal()

# Plot gross and net with facets
ggplot(combined_filter1, aes(x = Ig_Year)) +
  geom_line(aes(y = gross_acres, color = "Gross Acres")) +
  geom_line(aes(y = net_acres, color = "Net Acres")) +
  labs(x = "Ignition Year", y = "Value", color = "Type of Acres") +
  scale_x_continuous(breaks = seq(1993, 2017, 3)) +
  facet_wrap(~ ACTIVITY_TYPE) +
  theme_minimal()


### Repeat with ref_year

# Group by ref_year and summarize acres
gross_by_ref_year = gross_activities_df %>%
  group_by(ref_year, ACTIVITY_TYPE) %>%
  summarize(gross_acres = as.numeric(sum(gross_area)/4046.86), .groups = 'drop') %>%
  spread(key = ACTIVITY_TYPE, value = gross_acres)
net_by_ref_year = net_activities_df %>%
  group_by(ref_year, ACTIVITY_TYPE) %>%
  summarize(net_acres = as.numeric(sum(net_area)/4046.86), .groups = 'drop') %>%
  spread(key = ACTIVITY_TYPE, value = net_acres)

# Filter the data frames for the specified columns
net_by_ref_year_filter1 = net_by_ref_year[, c("ref_year", "planting", "release", "replant", "thin")]
gross_by_ref_year_filter1 = gross_by_ref_year[, c("ref_year", "planting", "release", "replant", "thin")]

# Convert the data frames from wide to long format
net_by_ref_year_filter1_long = tidyr::gather(net_by_ref_year_filter1, ACTIVITY_TYPE, value, -ref_year)
gross_by_ref_year_filter1_long = tidyr::gather(gross_by_ref_year_filter1, ACTIVITY_TYPE, value, -ref_year)

# Add a new column to identify the type of acres
net_by_ref_year_filter1_long$type = "net_acres"
gross_by_ref_year_filter1_long$type = "gross_acres"

# Combine the data frames
combined_ref_year_filter1 = rbind(net_by_ref_year_filter1_long, gross_by_ref_year_filter1_long)
# Spread the data frame to have gross_acres and net_acres in separate columns
combined_ref_year_filter1 = spread(combined_ref_year_filter1, key = type, value = value)

# Add a new column for the difference between gross and net acres
combined_ref_year_filter1$difference = combined_ref_year_filter1$gross_acres - combined_filter1$net_acres

# Plot difference
ggplot(combined_ref_year_filter1, aes(x = ref_year)) +
  geom_line(aes(y = difference, color = ACTIVITY_TYPE)) +
  labs(x = "Ignition Year", y = "Difference (Gross Acres - Net Acres)", color = "Activity Type") +
  scale_x_continuous(breaks = seq(1994, 2022, 3)) +
  facet_wrap(~ ACTIVITY_TYPE) +
  theme_minimal()

# Plot gross and net with facets
ggplot(combined_ref_year_filter1, aes(x = ref_year)) +
  geom_line(aes(y = gross_acres, color = "Gross Acres")) +
  geom_line(aes(y = net_acres, color = "Net Acres")) +
  labs(x = "Activity Year", y = "Value", color = "Type of Acres") +
  scale_x_continuous(breaks = seq(1994, 2022, 3)) +
  facet_wrap(~ ACTIVITY_TYPE) +
  theme_minimal()






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

