# Group by ref_year and summarize acres
gross_by_ref_year = gross_activities %>%
  st_drop_geometry() %>%
  group_by(ref_year, type_labels) %>%
  summarize(gross_acres = as.numeric(sum(gross_area)/4046.86), .groups = 'drop') %>%
  spread(key = type_labels, value = gross_acres)
net_by_ref_year = net_activities %>%
  st_drop_geometry() %>%
  group_by(ref_year, type_labels) %>%
  summarize(net_acres = as.numeric(sum(net_acres/4046.86), .groups = 'drop')) %>%
  spread(key = type_labels, value = net_acres)


# Convert the data frames from wide to long format
net_by_ref_year_long = tidyr::gather(net_by_ref_year, type_labels, value, -ref_year)
gross_by_ref_year_long = tidyr::gather(gross_by_ref_year, type_labels, value, -ref_year)

# Add a new column to identify the type of acres
net_by_ref_year_long$type = "net_acres"
gross_by_ref_year_long$type = "gross_acres"

# Combine the data frames
combined_ref_year = rbind(net_by_ref_year_long, gross_by_ref_year_long)

# Spread the data frame to have gross_acres and net_acres in separate columns
combined_ref_year = spread(combined_ref_year, key = type, value = value)

# Add a new column for the difference between gross and net acres
combined_ref_year$difference = combined_ref_year$gross_acres - combined_ref_year$net_acres


# Filter the data frames for the specified columns
filter1 = c("prep","planting","replant","release","prune")
filter2 = c("fuel", "harvest_salvage", "harvest","thin")
filter3 = c("cert_planted","cert_tsi","survey","review","need")

combined_ref_year_filter1 = combined_ref_year %>%
  filter(ACTIVITY_TYPE %in% filter1)
combined_ref_year_filter2 = combined_ref_year %>%
  filter(ACTIVITY_TYPE %in% filter2)
combined_ref_year_filter3 = combined_ref_year %>%
  filter(ACTIVITY_TYPE %in% filter3)


# Plot gross and net with facets
ggplot(combined_ref_year_filter1, aes(x = ref_year)) +
  geom_line(aes(y = gross_acres, color = "Gross Acres")) +
  geom_line(aes(y = net_acres, color = "Net Acres")) +
  ggtitle("Gross and Net Acres Treated by Activity Year") +
  labs(x = "Activity Year", y = "Treatment Acres", color = "Type of Acres") +
  scale_x_continuous(breaks = seq(1992, 2024, 6)) +
  facet_wrap(~ ACTIVITY_TYPE, ncol = 3) +
  theme_bw()+
  theme(legend.position="bottom", legend.box = "horizontal")
ggplot(combined_ref_year_filter2, aes(x = ref_year)) +
  geom_line(aes(y = gross_acres, color = "Gross Acres")) +
  geom_line(aes(y = net_acres, color = "Net Acres")) +
  ggtitle("Gross and Net Acres Treated by Activity Year") +
  labs(x = "Activity Year", y = "Treatment Acres", color = "Type of Acres") +
  scale_x_continuous(breaks = seq(1992, 2024, 6)) +
  facet_wrap(~ ACTIVITY_TYPE, ncol = 3) +
  theme_bw()+
  theme(legend.position="bottom", legend.box = "horizontal")
ggplot(combined_ref_year_filter3, aes(x = ref_year)) +
  geom_line(aes(y = gross_acres, color = "Gross Acres")) +
  geom_line(aes(y = net_acres, color = "Net Acres")) +
  ggtitle("Gross and Net Acres Treated by Activity Year") +
  labs(x = "Activity Year", y = "Treatment Acres", color = "Type of Acres") +
  scale_x_continuous(breaks = seq(1992, 2024, 6)) +
  facet_wrap(~ ACTIVITY_TYPE, ncol = 3) +
  theme_bw()+
  theme(legend.position="bottom", legend.box = "horizontal")


# Summarize net planting cert by ref_year
net_cert_by_refyear = net_activities %>%
  filter(ACTIVITY_TYPE == "cert_planted") %>%
  group_by(ref_year) %>%
  summarize(net_acres = as.numeric(sum(net_area)/4046.86), .groups = 'drop')

# Plot net planting cert by ref_year
ggplot(net_cert_by_refor_refyear, aes(x = ref_year)) +
  ggtitle("R5 Net Postfire Planting Acres Certified by Activity Year, 1994 - 2022") +
  geom_bar(aes(y = net_acres), stat = "identity") +
  labs(x = "Activity Year", y = "Net Acres") +
  scale_x_continuous(breaks = seq(1990, 2022, 4)) +
  theme_bw()+
  theme(legend.position="bottom", legend.box = "horizontal")

