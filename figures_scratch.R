
#### Examples ####

 assigned_df %>%
a <- group_by(a,diff_years,Ig_Year,type) |> summarize(activity_fire_area=sum(activity_fire_area))


ggplot(net_5yr_by_ig_year_filter1_long) +
  ggtitle("R5 Net Acres Treated Postfire by Activity Type") +
  aes(x = Ig_Year, y=ACTIVITY_TYPE, fill=value) + 
  facet_wrap(~type) +
  geom_tile(stat = "identity", height=1,width=1,color="grey") + 
  scale_fill_binned("Net Acres")
  # scale_x_continuous(breaks=c(0:10)) + scale_y_continuous(breaks=seq(1993,2017,by=2)) +

ggplot(net_by_prod_ref_year_filter1) +
  ggtitle("R5 Net Acres Planted Postfire by Productivity Class") +
  aes(x = ref_year, y=PRODUCTIVI, fill=value) + 
  geom_tile(stat = "identity", height=1,width=1,color="grey") + 
  scale_fill_binned("Net Acres")
# scale_x_continuous(breaks=c(0:10)) + scale_y_continuous(breaks=seq(1993,2017,by=2))

ggplot(planting_net_10years) +
  ggtitle("R5 Net Acres Planted by Years After Fire") +
  aes(x = Ig_Year, y=years_to_first_plant, fill=net_acres_planted) + 
  geom_tile(stat = "identity", height=1,width=1,color="gray") + 
  scale_fill_binned("Net Acres") +
  scale_x_continuous(breaks=seq(1992,2018,by=4)) + scale_y_continuous(breaks=seq(0,10,by=2))


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


# Summarize activity_area by ACTIVITY_TYPE for facts_df
facts_acreage <- facts_df %>%
  group_by(ACTIVITY_TYPE) %>%
  summarise(facts_activity_acres = sum(activity_area/4046.86, na.rm = TRUE))

# Summarize activity_area by ACTIVITY_TYPE for assigned_df
assigned_acreage <- assigned_df %>%
  group_by(ACTIVITY_TYPE) %>%
  summarise(assigned_activity_acres = sum(activity_area/4046.86, na.rm = TRUE))

# Summarize activity_area by ACTIVITY_TYPE for net_activities_df
net_acreage <- net_activities %>%
  group_by(ACTIVITY_TYPE) %>%
  summarise(net_activity_acres = sum(net_area/4046.86, na.rm = TRUE))

# Summarize activity_area by ACTIVITY_TYPE for gross_activities_df
gross_acreage <- gross_activities %>%
  group_by(ACTIVITY_TYPE) %>%
  summarise(gross_activity_acres = sum(gross_area/4046.86, na.rm = TRUE))

# Combine the four tables
combined_acreage_summary <- full_join(facts_acreage, assigned_acreage, by = "ACTIVITY_TYPE") %>%
  full_join(net_acreage, by = "ACTIVITY_TYPE") %>%
  full_join(gross_acreage, by = "ACTIVITY_TYPE")


# Plot acreage summary by category with facets
ggplot(combined_ig_year_filter1, aes(x = Ig_Year)) +
  geom_line(aes(y = gross_acres, color = "Gross Acres")) +
  geom_line(aes(y = net_acres, color = "Net Acres")) +
  labs(x = "Ignition Year", y = "Value", color = "Type of Acres") +
  scale_x_continuous(breaks = seq(1993, 2017, 3)) +
  facet_wrap(~ ACTIVITY_TYPE) +
  theme_minimal()


# drop units
units(net_activities_df$net_area) <- NULL

# Planting Summary & Trends
planting_net_10years <- net_activities_df %>%
  filter(diff_years < 10) %>%
  filter(ACTIVITY_TYPE == "planting") %>%
  group_by(Ig_Year, Event_ID) %>%
  summarize(years_to_first_plant = min(diff_years), avg_years_to_plant = mean(diff_years),
            net_acres_planted = sum(net_area/4046.86))

planting_net_10years_summary = planting_net_10years %>%
  group_by(Ig_Year) %>%
  summarize(avg_years_to_first_plant = mean(years_to_first_plant), 
            wtd_avg_years_to_plant = mean(avg_years_to_plant),
            total_net_acres_planted = sum(net_acres_planted))

ggplot(planting_net_10years_summary) +
  aes(x = Ig_Year, y = total_net_acres_planted) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous("Year of Ignition", breaks = seq(1992, 2018, by = 4)) +
  scale_y_continuous("Total Postfire Acres Planted") +
  theme_bw()

ggplot(planting_net_10years_summary) +
  aes(x = Ig_Year, y = avg_years_to_first_plant) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous("Year of Ignition", breaks = seq(1992, 2018, by = 4)) +
  scale_y_continuous("Average Years Until First Plant") +
  theme_bw()

ggplot(planting_10years) +
  aes(x = Ig_Year, y = wtd_avg_years_to_plant) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous("Year of Ignition", breaks = seq(1992, 2018, by = 4)) +
  scale_y_continuous("Average Number of Years Until Planting") +
  theme_bw()


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
  theme_bw()

ggplot(release_10years) +
  aes(x = Ig_Year, y = wtd_avg_years_to_release) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous("Year of Ignition", breaks = seq(1992, 2018, by = 4)) +
  scale_y_continuous("Weighted Average Years Until Release") +
  theme_bw()




