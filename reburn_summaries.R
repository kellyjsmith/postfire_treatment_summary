library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)

cal_eco3 <- st_read("../Data/ca_eco_l3.shp") %>%
  st_transform(3310)

fires_fires <- facts_fires$fires_fires %>%
  mutate(fire_id = paste(Incid_Name, Event_ID, sep = "_")) %>%
  left_join(facts_fires$fires %>% select(Event_ID, Ig_Year), by = "Event_ID") %>%
  st_transform(3310)

fire_events <- facts_fires$fires %>%
  mutate(fire_id = paste(Incid_Name, Event_ID, sep = "_")) %>%
  st_transform(3310) %>%
  st_collection_extract() %>%
  st_cast("MULTIPOLYGON") %>%
  st_intersection(cal_eco3 %>% select(US_L3NAME))


burned_summary <- fires_fires %>%
  st_collection_extract() %>%
  st_cast("MULTIPOLYGON") %>%
  st_intersection(cal_eco3 %>% select(US_L3NAME)) %>%
  mutate(area = st_area(.)) %>%
  group_by(fire_id, Ig_Year, US_L3NAME) %>%
  summarize(
    acres_burned_once = sum(ifelse(n.overlaps == 1, as.numeric(area), 0)) / 4046.86,
    .groups = "drop"
  ) %>%
  st_drop_geometry()

total_fire_events <- fire_events %>%
  mutate(area = st_area(.)) %>%
  group_by(fire_id, Ig_Year, US_L3NAME) %>% 
  summarize(total_acres = sum(as.numeric(area)) / 4046.86,
            .groups = "drop") %>%
  st_drop_geometry()

fire_summary_reburns <- burned_summary %>%
  left_join(total_fire_events, by = c("fire_id", "Ig_Year", "US_L3NAME")) %>%
  mutate(
    total_acres = as.numeric(total_acres),
    acres_burned_once = as.numeric(acres_burned_once),
    reburned_acres = pmax(total_acres - acres_burned_once, 0)
  ) %>%
  group_by(US_L3NAME) %>%
  summarize(
    total_acres = sum(total_acres),
    acres_burned_once = sum(acres_burned_once),
    reburned_acres = sum(reburned_acres),
    .groups = "drop"
  ) %>%
  arrange(desc(total_acres))


reburn_plot <- ggplot(fire_summary_reburns, aes(x = reorder(US_L3NAME, total_acres))) +
  geom_col(aes(y = total_acres, fill = "Total Burned"), width = 0.7) +
  geom_col(aes(y = reburned_acres, fill = "Reburned"), width = 0.7) +
  scale_fill_manual(values = c("Total Burned" = "red", "Reburned" = "darkred"),
                    name = "Burn Category") +
  geom_text(
    data = fire_summary_reburns %>% filter(grepl("Basin and Range$", US_L3NAME)),
    aes(y = total_acres, label = scales::comma(round(total_acres))),
    hjust = -0.1,
    size = 3,
    color = "black"
  ) +
  labs(title = "Total Acres Burned and Reburned by Ecoregion",
       subtitle = "USFS Region 5, 2000-2021\nReburned acres represent areas where multiple fire polygons intersect",
       x = "Ecoregion",
       y = "Acres") +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(face = "bold", size = 10),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.title = element_text(face = "bold", size = 12),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 10),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    panel.grid.major.x = element_line(color = "gray80", size = 0.2),
    panel.grid.minor.x = element_line(color = "gray90", size = 0.2)
  ) +
  scale_y_continuous(
    labels = scales::comma_format(scale = 1e-6, suffix = "M"),
    minor_breaks = seq(0, max(fire_summary_reburns$total_acres), by = 500000)
  ) +
  coord_flip()

print(reburn_plot)

ggsave("reburn_acres_by_ecoregion.png", reburn_plot, width = 7, height = 4, dpi = 300)

summary_table <- fire_summary_reburns %>%
  mutate(Percent_Reburned = reburned_acres / total_acres * 100)

print(summary_table)

write.csv(summary_table, "reburn_summary_by_ecoregion.csv", row.names = FALSE)



total_burn_eco_plot <- ggplot(total_fire_events, aes(x = Ig_Year, fill = US_L3NAME)) +
  geom_col(aes(y = total_acres), position = "stack", width = 0.8) +
  # geom_col(aes(y = reburned_acres), position = "stack", width = 0.8, alpha = 0.5) +
  scale_fill_viridis_d(option = "plasma", name = "Ecoregion") +
  labs(title = "Total and Reburned Acres by Ignition Year and Ecoregion",
       subtitle = "USFS Region 5, 2000-2021",
       x = "Ignition Year",
       y = "Acres") +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9)
  ) +
  # guides(fill = guide_legend(nrow = 4)) +
  scale_x_continuous(breaks = seq(2000, 2021, by = 4)) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M"))

print(total_burn_eco_plot)

ggsave("reburn_acres_by_year_ecoregion.png", reburn_plot, width = 12, height = 8, dpi = 300)

eco_fire_summary_table <- fire_summary_reburns %>%
  group_by(US_L3NAME) %>%
  summarize(
    Total_Acres = sum(total_acres),
    Reburned_Acres = sum(reburned_acres),
    Percent_Reburned = Reburned_Acres / Total_Acres * 100
  ) %>%
  arrange(desc(Total_Acres))

print(eco_fire_summary_table)

write.csv(eco_fire_summary_table, "reburn_summary_by_ecoregion.csv", row.names = FALSE)


year_fire_summary_table <- fire_summary_reburns %>%
  group_by(Ig_Year) %>%
  summarize(
    Total_Acres = sum(total_acres),
    Reburned_Acres = sum(reburned_acres),
    Percent_Reburned = Reburned_Acres / Total_Acres * 100
  ) %>% arrange(Ig_Year)

print(year_fire_summary_table)

write.csv(year_fire_summary_table, "reburn_summary_by_ecoregion.csv", row.names = FALSE)



library(sf)
library(dplyr)

# Assuming cal_eco3 and eco_fire_summary_table are already loaded

# Join the summary table with the shapefile
cal_eco3_with_summary <- cal_eco3 %>%
  left_join(eco_fire_summary_table, by = "US_L3NAME")

# # Round Total_Acres to nearest thousand for display
# cal_eco3_with_summary <- cal_eco3_with_summary %>%
#   mutate(Total_Acres = round(Total_Acres / 1000) * 1000)

# Create a new field for QGIS legend
cal_eco3_with_summary <- cal_eco3_with_summary %>%
  mutate(Legend_Display = paste0(US_L3NAME, "\n", 
                                 format(Total_Acres, big.mark = ","), " acres"))

# Export the shapefile
st_write(cal_eco3_with_summary, "cal_eco3_with_fire_summary_final.shp", append = FALSE)

# Print the first few rows to verify
print(head(cal_eco3_with_summary))
