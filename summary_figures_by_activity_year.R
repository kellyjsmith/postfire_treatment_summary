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
net_by_ref_year_filter1 = net_by_ref_year
gross_by_ref_year_filter1 = gross_by_ref_year

# net_by_ref_year_filter1 = net_by_ref_year[, c("ref_year", "planting", "release", "replant", "thin")]
# gross_by_ref_year_filter1 = gross_by_ref_year[, c("ref_year", "planting", "release", "replant", "thin")]

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
combined_ref_year_filter1$difference = combined_ref_year_filter1$gross_acres - combined_ref_year_filter1$net_acres


combined_ref_year_filter2 = combined_ref_year_filter1 %>%
  filter(ACTIVITY_TYPE %in% c("cert", "planting"))

# Plot difference
ggplot(combined_ref_year_filter1, aes(x = ref_year)) +
  geom_line(aes(y = difference, color = ACTIVITY_TYPE)) +
  labs(x = "Activity Year", y = "Difference (Gross Acres - Net Acres)", color = "Activity Type") +
  scale_x_continuous(breaks = seq(1992, 2022, 4)) +
  facet_wrap(~ ACTIVITY_TYPE, ncol = 2) +
  theme_classic() +
  theme(legend.position="bottom", legend.box = "horizontal")

# Plot gross and net with facets
ggplot(combined_ref_year_filter2, aes(x = ref_year)) +
  geom_line(aes(y = gross_acres, color = "Gross Acres")) +
  geom_line(aes(y = net_acres, color = "Net Acres")) +
  ggtitle("Gross and Net Acres Treated by Activity Year") +
  labs(x = "Activity Year", y = "Treatment Acres", color = "Type of Acres") +
  scale_x_continuous(breaks = seq(1992, 2024, 6)) +
  facet_wrap(~ ACTIVITY_TYPE, ncol = 1) +
  theme_bw()+
  theme(legend.position="bottom", legend.box = "horizontal")

