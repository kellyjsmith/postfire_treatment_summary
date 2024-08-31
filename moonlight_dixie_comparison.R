library(ggplot2)
library(sf)
library(dplyr)
library(ggrepel)
library(mapview)

# Filter for Moonlight and Dixie fires
selected_fires <- fires_by_severity %>%
  filter(unique_id %in% c("MOONLIGHT_2007", "DIXIE_2021")) %>%
  mutate(fire_label = unique_id)

# Calculate bounding box around Moonlight fire with some padding
moonlight_bbox <- st_bbox(selected_fires[selected_fires$unique_id == "MOONLIGHT_2007",])
expanded_bbox <- moonlight_bbox + c(-0.2, -0.2, 0.2, 0.2) * (moonlight_bbox[3] - moonlight_bbox[1])

# Create a polygon from the expanded bounding box
bbox_polygon <- st_as_sfc(st_bbox(expanded_bbox))

# Clip Dixie fire to the expanded bounding box
selected_fires <- selected_fires %>%
  st_intersection(bbox_polygon)

# Filter for activities intersecting Moonlight fire after Dixie fire
selected_activities <- assigned_activities %>%
  filter(Incid_Name == "MOONLIGHT" & year == 2022 & type_labels == "Initial Planting") %>%
  # mutate(activity_label = case_when(
  #   grepl("Site Prep", type_labels) ~ "Site Prep",
  #   grepl("TSI", type_labels) ~ "TSI",
  #   TRUE ~ type_labels)) %>%
  mutate(activity_label = type_labels) %>%
  mutate(activity_label = paste(year, activity_label))

# Calculate centroid for labeling
selected_fires_centroid <- st_centroid(selected_fires)
selected_activities_centroid <- st_centroid(selected_activities)

# Create the ggplot
ggplot() +
  geom_sf(data = selected_fires, aes(fill = fire_label), alpha = 0.5, size = 2) +
  geom_sf(data = selected_activities, aes(color = activity_label), size = 2) +
  geom_text_repel(data = selected_fires_centroid, aes(label = fire_label, geometry = geometry),
                  stat = "sf_coordinates", fontface = "bold", size = 6, force = 7, force_pull = 5) +
  geom_text_repel(data = selected_activities_centroid, aes(label = activity_label, geometry = geometry),
                  stat = "sf_coordinates", size = 4, force = 7, max.overlaps = 10) +
  scale_fill_manual(values = c("tomato", "dodgerblue"), name = "Fires") +
  scale_color_brewer(palette = 1, name = "Activities") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Activities Related to Dixie Fire (2021) Assigned to Moonlight Fire (2007)",
       subtitle = "Due to spatial intersection with Moonlight Fire perimeter") +
  coord_sf(xlim = expanded_bbox[c(1,3)], ylim = expanded_bbox[c(2,4)], datum = NA)

# Save the ggplot
ggsave("dixie_moonlight_activities_ggplot.png", width = 8, height = 6, dpi = 300)

# Create mapview version with adjusted colors
mapview_plot <- mapview(selected_fires, zcol = "fire_label", 
                        col.regions = c("orangered", "red4"),
                        alpha.regions = 0.7, lineweight = 5,
                        layer.name = "Fires") +
  mapview(selected_activities, zcol = "activity_label", 
          col.regions = mako(n = length(unique(selected_activities$activity_label)),
                             begin = 0.2, end = 0.8),
          alpha.regions = 1, lineweight = 2,
          layer.name = "Activities")

print(mapview_plot)

# Set the view to the expanded bounding box
mapview_plot@map <- mapview_plot@map %>%
  leaflet::fitBounds(lng1 = expanded_bbox["xmin"], lat1 = expanded_bbox["ymin"],
                     lng2 = expanded_bbox["xmax"], lat2 = expanded_bbox["ymax"])

# Save the mapview plot
mapview::mapshot(mapview_plot, file = "dixie_moonlight_activities_mapview.png")
