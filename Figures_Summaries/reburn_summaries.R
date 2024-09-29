library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)
library(scales)
library(viridis)

# Load and prepare data
fire_events <- st_read("r5_fires_00_21.shp") %>%
  mutate(fire_id = paste(Incid_Name, Event_ID, sep = "_"))

cal_eco3 <- st_read("../Data/ca_eco_l3.shp") %>%
  st_transform(3310)

facts_fires <- readRDS("facts_fires_new.RDS")

fires_with_activity <- unique(processed_activities$fire_id)

fires_fires <- facts_fires$fires_fires %>%
  mutate(fire_id = paste(Incid_Name, Event_ID, sep = "_")) %>%
  st_transform(3310)

fire_events_eco <- fire_events %>%
  st_transform(3310) %>%
  st_collection_extract() %>%
  st_cast("MULTIPOLYGON") %>%
  st_intersection(cal_eco3 %>% select(US_L3NAME))

# Calculate burned summary by ecoregion
burned_summary_eco <- fires_fires %>%
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

total_fire_eco <- fire_events_eco %>%
  mutate(area = st_area(.)) %>%
  group_by(fire_id, Ig_Year, US_L3NAME) %>% 
  summarize(total_acres = sum(as.numeric(area)) / 4046.86,
            .groups = "drop") %>%
  st_drop_geometry()

# Calculate reburn summary
fire_summary_reburns <- burned_summary_eco %>%
  left_join(total_fire_eco, by = c("fire_id", "Ig_Year", "US_L3NAME")) %>%
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

# Create reburn plot
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

# Create summary tables
summary_table <- fire_summary_reburns %>%
  mutate(Percent_Reburned = reburned_acres / total_acres * 100)

write.csv(summary_table, "reburn_summary_by_ecoregion.csv", row.names = FALSE)

# Create total burn by ecoregion plot
total_burn_eco_plot <- ggplot(total_fire_eco, aes(x = Ig_Year, fill = US_L3NAME)) +
  geom_col(aes(y = total_acres), position = "stack", width = 0.8) +
  scale_fill_viridis_d(option = "plasma", name = "Ecoregion") +
  labs(title = "Total Acres Burned by Ignition Year and Ecoregion",
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
  scale_x_continuous(breaks = seq(2000, 2021, by = 4)) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M"))

print(total_burn_eco_plot)
ggsave("burn_acres_by_year_ecoregion.png", total_burn_eco_plot, width = 12, height = 8, dpi = 300)

# Create summary tables
eco_fire_summary_table <- fire_summary_reburns %>%
  summarize(
    Total_Acres = sum(total_acres),
    Reburned_Acres = sum(reburned_acres),
    Percent_Reburned = Reburned_Acres / Total_Acres * 100
  ) %>%
  arrange(desc(Total_Acres))

write.csv(eco_fire_summary_table, "reburn_summary_by_ecoregion.csv", row.names = FALSE)

year_fire_summary_table <- total_fire_eco %>%
  group_by(Ig_Year) %>%
  summarize(
    Total_Acres = sum(total_acres),
    .groups = "drop"
  ) %>% 
  arrange(Ig_Year)

write.csv(year_fire_summary_table, "burn_summary_by_year.csv", row.names = FALSE)

# Create combined fire data plot
total_burned_by_year <- fire_events %>%
  group_by(Ig_Year) %>%
  summarize(
    Total_Acres = sum(as.numeric(st_area(geometry)) / 4046.86),
    Total_Count = n()
  ) %>%
  st_drop_geometry()

ggplot(total_burned_by_year, aes(x = Ig_Year)) +
  geom_col(aes(y = Total_Acres, fill = "Total Burned"), width = 0.8, 
           color = "black", size = 0.1) +
  geom_text(aes(y = Total_Acres, label = Total_Count), 
            position = position_stack(vjust = 0.9), color = "white", size = 3.25) +
  scale_fill_manual(values = c("Total Burned" = "firebrick"),
                    labels = c("Total Burned" = "All Fires")) +
  scale_x_continuous(breaks = seq(2000, 2021, by = 2)) +
  scale_y_continuous(labels = comma_format(scale = 1e-6, suffix = "M")) +
  labs(title = "Wildfires in USFS Region 5",
       subtitle = "MTBS Wildfires 2000-2021 | Numbers Represent Fire Count",
       x = "Ignition Year",
       y = "Acres Burned",
       fill = "Category") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 11),
    legend.title = element_blank(),
    legend.text = element_text(size = 9)
  )

ggsave("r5_total_burned.png", width = 7, height = 4, dpi = 300)