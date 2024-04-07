## Tables & Figures for summarizing postfire activities

library("sf")
library("dplyr")
library("terra")
library("tidyverse")
library("flextable")
library("insight")
library("units")

assigned_activities = readRDS("assigned_activities.RDS")

a <- assigned_activities |>filter((diff_years<10) & (IS_planting==TRUE | IS_prep==TRUE |IS_release==TRUE))
a <- pivot_longer(assigned_activities,cols=starts_with("IS_"),names_to = "type",values_to = "IS_type")
a$type <- gsub("IS_","",a$type)
a <- a[a$IS_type,]
a <- group_by(a,diff_years,Ig_Year,type) |> summarize(activity_fire_area=sum(activity_fire_area))



# Convert sf objects to data frames
assigned_df = st_drop_geometry(assigned_activities)
facts_df = st_drop_geometry(facts)

# drop units
units(assigned_df$activity_fire_acres) <- NULL


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
combined_summary <- full_join(summary_assigned, summary_gross, by = "ACTIVITY_TYPE") %>%
  full_join(summary_net, by = "ACTIVITY_TYPE")

ft_combined = flextable(combined_summary)
ft_combined = theme_vanilla(ft_combined)
ft_combined

# Plot acreage summary by category with facets
ggplot(combined_ig_year_filter1, aes(x = Ig_Year)) +
  geom_line(aes(y = gross_acres, color = "Gross Acres")) +
  geom_line(aes(y = net_acres, color = "Net Acres")) +
  labs(x = "Ignition Year", y = "Value", color = "Type of Acres") +
  scale_x_continuous(breaks = seq(1993, 2017, 3)) +
  facet_wrap(~ ACTIVITY_TYPE) +
  theme_minimal()



# Planting Summary & Trends
planting_10years <- assigned_df %>%
  filter(diff_years < 10) %>%
  filter(ACTIVITY_TYPE == "planting")

planting_10years = planting_10years %>%
  group_by(Ig_Year, VB_ID) %>%
  summarize(years_to_first_plant = min(diff_years), avg_years_to_plant = mean(diff_years),
            mode(diff_years), net_acres_planted = sum(activity_fire_acres))

planting_10years = planting_10years %>%
  group_by(Ig_Year) %>%
  summarize(avg_years_to_first_plant = mean(years_to_first_plant), 
            wtd_avg_years_to_plant = mean(avg_years_to_plant),
            total_net_acres_planted = sum(net_acres_planted))

ggplot(planting_10years) +
  aes(x = Ig_Year, y = total_net_acres_planted) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous("Year of Ignition", breaks = seq(1992, 2018, by = 4)) +
  scale_y_continuous("Total Postfire Acres Planted") +
  theme_classic()

ggplot(planting_10years) +
  aes(x = Ig_Year, y = avg_years_to_first_plant) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous("Year of Ignition", breaks = seq(1992, 2018, by = 4)) +
  scale_y_continuous("Average Years Until First Plant") +
  theme_classic()

ggplot(planting_10years) +
  aes(x = Ig_Year, y = wtd_avg_years_to_plant) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous("Year of Ignition", breaks = seq(1992, 2018, by = 4)) +
  scale_y_continuous("Average Number of Years Until Planting") +
  theme_classic()


# Release Summary & Trends
release_10years <- assigned_df %>%
  filter(diff_years < 10) %>%
  filter(ACTIVITY_TYPE == "release")

release_10years = release_10years %>%
  group_by(Ig_Year, VB_ID) %>%
  summarize(years_to_first_release = min(diff_years), 
            avg_years_to_release = mean(diff_years),
            net_acres_released = sum(activity_fire_acres))

release_10years = release_10years %>%
  group_by(Ig_Year) %>%
  summarize(avg_years_to_first_release = mean(years_to_first_release), 
            wtd_avg_years_to_release = mean(avg_years_to_release),
            total_net_acres_released = sum(net_acres_released))

ggplot(release_10years) +
  aes(x = Ig_Year, y = total_net_acres_released) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous("Year of Ignition", breaks = seq(1992, 2018, by = 4)) +
  scale_y_continuous("Total Postfire Acres Released") +
  theme_classic()

ggplot(release_10years) +
  aes(x = Ig_Year, y = avg_years_to_first_release) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous("Year of Ignition", breaks = seq(1992, 2018, by = 4)) +
  scale_y_continuous("Average Years Until First Release") +
  theme_classic()

ggplot(release_10years) +
  aes(x = Ig_Year, y = wtd_avg_years_to_release) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous("Year of Ignition", breaks = seq(1992, 2018, by = 4)) +
  scale_y_continuous("Weighted Average Years Until Release") +
  theme_classic()



#### Summarize Treatment Area by Category ####

# Convert sf object to data frame
gross_activities_df = st_drop_geometry(gross_activities)
net_activities_df = st_drop_geometry(net_activities)


# Group by ig_year and summarize acres
gross_by_ig_year = gross_activities_df %>%
  group_by(Ig_Year, ACTIVITY_TYPE) %>%
  summarize(gross_acres = as.numeric(sum(gross_area)/4046.86), .groups = 'drop') %>%
  spread(key = ACTIVITY_TYPE, value = gross_acres)
gross_5yr_by_ig_year = gross_activities_df %>%
  filter(diff_years < 5) %>%
  group_by(Ig_Year, ACTIVITY_TYPE) %>%
  summarize(gross_acres = as.numeric(sum(gross_area)/4046.86), .groups = 'drop') %>%
  spread(key = ACTIVITY_TYPE, value = gross_acres)
net_by_ig_year = net_activities_df %>%
  group_by(Ig_Year, ACTIVITY_TYPE) %>%
  summarize(net_acres = as.numeric(sum(net_area)/4046.86), .groups = 'drop') %>%
  spread(key = ACTIVITY_TYPE, value = net_acres)
net_5yr_by_ig_year = net_activities_df %>%
  filter(diff_years < 5) %>%
  group_by(Ig_Year, ACTIVITY_TYPE) %>%
  summarize(net_acres = as.numeric(sum(net_area)/4046.86), .groups = 'drop') %>%
  spread(key = ACTIVITY_TYPE, value = net_acres)

# Filter the data frames for the specified columns
net_by_ig_year_filter1 = net_by_ig_year[, c("Ig_Year", "planting", "release", "replant", "thin")]
gross_by_ig_year_filter1 = gross_by_ig_year[, c("Ig_Year", "planting", "release", "replant", "thin")]
net_5yr_by_ig_year_filter1 = net_5yr_by_ig_year[, c("Ig_Year", "planting", "release", "replant", "thin")]
gross_5yr_by_ig_year_filter1 = gross_5yr_by_ig_year[, c("Ig_Year", "planting", "release", "replant", "thin")]

net_by_ig_year_filter1 = net_by_ig_year
gross_by_ig_year_filter1 = gross_by_ig_year
net_5yr_by_ig_year_filter1 = net_5yr_by_ig_year
gross_5yr_by_ig_year_filter1 = gross_5yr_by_ig_year

# Convert the data frames from wide to long format
net_by_ig_year_filter1_long = tidyr::gather(net_by_ig_year_filter1, ACTIVITY_TYPE, value, -Ig_Year)
gross_by_ig_year_filter1_long = tidyr::gather(gross_by_ig_year_filter1, ACTIVITY_TYPE, value, -Ig_Year)
net_5yr_by_ig_year_filter1_long = tidyr::gather(net_5yr_by_ig_year_filter1, ACTIVITY_TYPE, value, -Ig_Year)
gross_5yr_by_ig_year_filter1_long = tidyr::gather(gross_5yr_by_ig_year_filter1, ACTIVITY_TYPE, value, -Ig_Year)

# Add a new column to identify the type of acres
net_by_ig_year_filter1_long$type = "net_acres"
gross_by_ig_year_filter1_long$type = "gross_acres"
net_5yr_by_ig_year_filter1_long$type = "net_acres_5yr"
gross_5yr_by_ig_year_filter1_long$type = "gross_acres_5yr"

# Combine the data frames
combined_ig_year_filter1 = rbind(net_by_ig_year_filter1_long, gross_by_ig_year_filter1_long)
combined_5yr_ig_year_filter1 = rbind(net_5yr_by_ig_year_filter1_long, gross_5yr_by_ig_year_filter1_long)
# Spread the data frame to have gross_acres and net_acres in separate columns
combined_ig_year_filter1 = spread(combined_ig_year_filter1, key = type, value = value)
combined_5yr_ig_year_filter1 = spread(combined_5yr_ig_year_filter1, key = type, value = value)

# Add a new column for the difference between gross and net acres
combined_ig_year_filter1$difference = combined_ig_year_filter1$gross_acres - combined_ig_year_filter1$net_acres
combined_5yr_ig_year_filter1$difference = combined_5yr_ig_year_filter1$gross_acres_5yr - combined_5yr_ig_year_filter1$net_acres_5yr


# Plot difference
ggplot(combined_ig_year_filter1, aes(x = Ig_Year)) +
  geom_line(aes(y = difference, color = ACTIVITY_TYPE)) +
  labs(x = "Ignition Year", y = "Difference (Gross Acres - Net Acres)", color = "Activity Type") +
  scale_x_continuous(breaks = seq(1993, 2017, 3)) +
  facet_wrap(~ ACTIVITY_TYPE) +
  theme_classic()
ggplot(combined_5yr_ig_year_filter1, aes(x = Ig_Year)) +
  geom_line(aes(y = difference, color = ACTIVITY_TYPE)) +
  labs(x = "Ignition Year", y = "Difference (Gross Acres - Net Acres)", color = "Activity Type") +
  scale_x_continuous(breaks = seq(1993, 2017, 3)) +
  facet_wrap(~ ACTIVITY_TYPE) +
  theme_classic()


# Plot gross and net with facets
ggplot(combined_ig_year_filter1, aes(x = Ig_Year)) +
  geom_line(aes(y = gross_acres, color = "Gross Acres")) +
  geom_line(aes(y = net_acres, color = "Net Acres")) +
  labs(x = "Ignition Year", y = "Value", color = "Type of Acres") +
  scale_x_continuous(breaks = seq(1993, 2017, 3)) +
  facet_wrap(~ ACTIVITY_TYPE) +
  theme_classic()
ggplot(combined_5yr_ig_year_filter1, aes(x = Ig_Year)) +
  geom_line(aes(y = gross_acres_5yr, color = "Gross Acres")) +
  geom_line(aes(y = net_acres_5yr, color = "Net Acres")) +
  ggtitle("Gross and Net Acres Completed within 5 Years of Fire") +
  labs(x = "Ignition Year", y = "Treatment Acres", color = "Type of Acres") +
  scale_x_continuous(breaks = seq(1992, 2018, 6)) +
  facet_wrap(~ ACTIVITY_TYPE, ncol = 2) +
  theme_classic() +
  theme(legend.position="bottom", legend.box = "horizontal")




#### Examples ####

ggplot(a)+ aes(x=diff_years,y=Ig_Year,fill=as.numeric(activity_fire_area)/4046.86) + 
  facet_wrap(~type,ncol=3) +
  geom_tile(stat = "identity" , height=1,width=1,color="grey") + scale_fill_gradient("acres") +
  scale_x_continuous(breaks=c(0:10)) + scale_y_continuous(breaks=seq(1993,2017,by=2)) + 
  xlim(c(0,15)) + ylim(c(1993,2017))


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

