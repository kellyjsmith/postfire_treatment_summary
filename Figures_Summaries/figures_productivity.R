library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(viridis)
library(patchwork)

# Assuming vegetation_activities data is already loaded

# Prepare the data
activities_by_productivity <- vegetation_activities %>%
  filter(type_labels %in% c("Site Prep - Non-Chemical", "Release - Non-Chemical", "Survival Survey", "Stocking Survey")) %>%
  mutate(
    productivity_group = cut(mean_prod, 
                             breaks = seq(0, ceiling(max(mean_prod, na.rm = TRUE)), by = 1),
                             labels = seq(1, ceiling(max(mean_prod, na.rm = TRUE))),
                             include.lowest = TRUE)
  ) %>%
  group_by(type_labels, productivity_group, EVT_PHYS) %>%
  summarize(Acres = sum(Area_acres, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(productivity_group))

# Set the desired order for activities
activity_order <- c("Site Prep - Non-Chemical", "Release - Non-Chemical", "Survival Survey", "Stocking Survey")
activities_by_productivity$type_labels <- factor(activities_by_productivity$type_labels, levels = activity_order)

# Color scheme from figures_vegetation
veg_colors <- c("Conifer" = "springgreen4", 
                "Shrubland" = "goldenrod", 
                "Hardwood" = "purple2",
                "Grassland" = "yellow",
                "Other" = "gray70")

# Create the plot
productivity_plot <- ggplot(activities_by_productivity, aes(x = productivity_group, y = Acres, fill = EVT_PHYS)) +
  geom_col(position = "stack", color = "black", size = 0.1) +
  facet_wrap(~ type_labels, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = veg_colors, name = "Vegetation Type") +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "Distribution of Vegetation Types in Postfire Activities by Productivity Class",
       subtitle = "USFS Region 5 | Fires 2000-2021 | Activities 2001-2022",
       x = "Productivity Class",
       y = "Acres") +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 11),
    strip.text = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1))

# Display the plot
print(productivity_plot)

# Save the plot
ggsave("postfire_activities_by_productivity_and_vegetation.png", productivity_plot, width = 10, height = 8, dpi = 300)






library(dplyr)
library(sf)
library(tidyr)
library(purrr)
library(ggplot2)
library(viridis)

activities_in_plantations_eco <- function(all_net_activities_eco) {
  plant <- all_net_activities_eco %>%
    filter(type_labels == "Initial Planting")
  
  other_activities <- all_net_activities_eco %>%
    filter(type_labels %in% c("Site Prep - Non-Chemical", "Release - Non-Chemical", 
                              "Survival Survey", "Stocking Survey"))
  
  planted_by_fire <- plant %>%
    group_by(fire_id, US_L3NAME) %>%
    rename(net_total = net_acres) %>%
    ungroup()
  
  calculate_intersection <- function(activity_group, planted_by_fire) {
    fire <- activity_group$fire_id
    eco <- activity_group$US_L3NAME
    
    tryCatch({
      planted_area <- planted_by_fire %>% 
        filter(fire_id == fire, US_L3NAME == eco)
      
      if (nrow(planted_area) == 0) {
        return(tibble(
          fire_id = fire,
          US_L3NAME = eco,
          type_labels = activity_group$type_labels,
          Ig_Year = activity_group$Ig_Year,
          net_total_activity = activity_group$net_total,
          n_activities = activity_group$n_activities,
          intersecting_acres = 0,
          percent_total_planted = 0,
          percent_total_activity = 0,
          mean_diff = activity_group$weighted_mean_diff,
          mean_productivity = activity_group$weighted_mean_prod,
          reburns = activity_group$max_reburns
        ))
      }
      
      intersection <- st_intersection(activity_group$activity_geom, planted_area$geometry)
      intersecting_acres <- sum(as.numeric(st_area(intersection)) / 4046.86)
      
      planted_total_area <- sum(planted_area$net_total)
      
      percent_total_planted <- (intersecting_acres / planted_total_area) * 100
      percent_total_activity <- (intersecting_acres / activity_group$net_total) * 100
      
      tibble(
        fire_id = fire,
        US_L3NAME = eco,
        type_labels = activity_group$type_labels,
        Ig_Year = activity_group$Ig_Year,
        net_total_activity = activity_group$net_total,
        n_activities = activity_group$n_activities,
        intersecting_acres = intersecting_acres,
        percent_total_planted = percent_total_planted,
        percent_total_activity = percent_total_activity,
        mean_diff = activity_group$weighted_mean_diff,
        mean_productivity = activity_group$weighted_mean_prod,
        reburns = activity_group$max_reburns
      )
    }, error = function(e) {
      warning(paste("Error processing fire_id:", fire, "- Error:", e$message))
      return(NULL)
    })
  }
  
  activities_by_fire <- other_activities %>%
    group_by(fire_id, US_L3NAME, type_labels) %>%
    summarize(
      activity_geom = st_union(geometry),
      net_total = sum(net_acres),
      n_activities = n(),
      weighted_mean_diff = weighted.mean(mean_diff, w = net_acres),
      weighted_mean_prod = weighted.mean(mean_prod, w = net_acres),
      max_reburns = first(max_reburns),
      Ig_Year = first(Ig_Year)
    ) %>%
    ungroup()
  
  activities_in_plant <- map_dfr(1:nrow(activities_by_fire), 
                                 ~calculate_intersection(activities_by_fire[.x,], planted_by_fire), 
                                 .progress = TRUE)
  
  combined_activities_in_plant <- activities_in_plant %>%
    left_join(planted_by_fire %>% 
                group_by(fire_id, US_L3NAME) %>% 
                summarize(net_total_planted = sum(net_total)), 
              by = c("fire_id", "US_L3NAME"))
  
  return(combined_activities_in_plant)
}

# Assuming all_net_activities_eco is available
activities_in_plantations_eco <- activities_in_plantations_eco(all_net_activities_eco)

saveRDS(activities_in_plantations_eco, "activities_in_plantations_eco.RDS")

# Create a summary table
activities_summary_by_eco <- activities_in_plantations_eco %>%
  group_by(US_L3NAME, type_labels) %>%
  summarize(
    total_fires = n(),
    total_planted_acres = sum(net_total_planted, na.rm = TRUE),
    total_activity_acres = sum(net_total_activity, na.rm = TRUE),
    total_intersecting_acres = sum(intersecting_acres, na.rm = TRUE),
    total_percent_intersecting = (total_intersecting_acres / total_planted_acres) * 100,
    median_percent_intersecting = median(percent_total_planted, na.rm = TRUE),
    mean_percent_intersecting = mean(percent_total_planted, na.rm = TRUE),
    min_percent_intersecting = min(percent_total_planted, na.rm = TRUE),
    max_percent_intersecting = max(percent_total_planted, na.rm = TRUE),
    avg_mean_diff = mean(mean_diff, na.rm = TRUE),
    avg_mean_productivity = mean(mean_productivity, na.rm = TRUE),
    max_reburns = max(reburns, na.rm = TRUE),
    avg_n_activities = mean(n_activities, na.rm = TRUE)
  ) %>%
  arrange(type_labels, desc(total_percent_intersecting))

# Save the summary table
write.csv(activities_summary_by_eco, "activities_summary_by_ecoregion.csv", row.names = FALSE)

# Create the faceted figure
ggplot(activities_summary_by_eco, aes(x = reorder(US_L3NAME, total_planted_acres))) +
  geom_col(aes(y = total_planted_acres, fill = "Initial Planting"), stat = "identity") +
  geom_bar(aes(y = total_intersecting_acres, fill = "Activity in Planted Area"), stat = "identity") +
  geom_text(aes(y = total_planted_acres, 
                label = scales::comma(round(total_planted_acres))),
            hjust = -0.1, 
            size = 2.5,
            fontface = "bold") +
  coord_flip() +
  facet_wrap(~ factor(type_labels, levels = c("Site Prep - Non-Chemical", "Release - Non-Chemical", 
                                              "Survival Survey", "Stocking Survey")),
             scales = "free_x") +
  scale_fill_manual(values = c("Initial Planting" = "forestgreen", "Activity in Planted Area" = "skyblue"),
                    name = "Activity Type") +
  labs(title = "Postfire Planted Acres with Other Activities by Ecoregion",
       subtitle = "USFS R5 | Fires 2000-2021 | Net Planting and Activities 2001-2022",
       x = "Ecoregion",
       y = "Acres") +
  theme_bw(base_size = 11) +
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 12, face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(size = 8),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 9),
        strip.text = element_text(size = 9, face = "bold")) +
  scale_y_continuous(labels = scales::comma_format(),
                     expand = expansion(mult = c(0, 0.1)))

ggsave("activities_in_planted_acres_by_ecoregion.png", width = 10, height = 8)





library(dplyr)
library(ggplot2)
library(scales)
library(forcats)
library(stringr)

# Prepare the data
planted_by_productivity_eco <- planted_veg_severity_eco %>%
  st_drop_geometry() %>%
  mutate(productivity_class = cut(mean_prod, 
                                  breaks = seq(0, ceiling(max(mean_prod, na.rm = TRUE)), by = 1),
                                  labels = seq(1, ceiling(max(mean_prod, na.rm = TRUE))),
                                  include.lowest = TRUE)) %>%
  group_by(US_L3NAME, productivity_class) %>%
  summarize(net_planted_acres = sum(Acres, na.rm = TRUE),
            .groups = "drop") %>%
  filter(!is.na(productivity_class))

# Rename longer ecoregion names
ecoregion_names <- c(
  "Klamath Mountains/California High North Coast Range" = "Klamath Mountains & North Coast",
  "Eastern Cascades Slopes and Foothills" = "East Cascades Slopes",
  "Central California Foothills and Coastal Mountains" = "Central Foothills & Coastal Mountains"
)

planted_by_productivity_eco <- planted_by_productivity_eco %>%
  mutate(US_L3NAME = str_replace_all(US_L3NAME, ecoregion_names))

# Calculate total acres planted for each ecoregion
total_eco_acres <- planted_by_productivity_eco %>%
  group_by(US_L3NAME) %>%
  summarize(Total_Acres = sum(net_planted_acres, na.rm = TRUE)) %>%
  arrange(desc(Total_Acres))

# Identify top 5 ecoregions with the most planting areas
top_5_ecoregions <- total_eco_acres %>%
  top_n(5, Total_Acres) %>%
  pull(US_L3NAME)

# Filter data for top 5 ecoregions
planted_by_productivity_top5_eco <- planted_by_productivity_eco %>%
  filter(US_L3NAME %in% top_5_ecoregions)

# Create ordered factor for ecoregions
ecoregion_order <- total_eco_acres$US_L3NAME

# Add total acres to ecoregion names with proper comma formatting and order
planted_by_productivity_top5_eco <- planted_by_productivity_top5_eco %>%
  left_join(total_eco_acres, by = "US_L3NAME") %>%
  mutate(US_L3NAME = paste0(US_L3NAME, "\n(", scales::comma(round(Total_Acres)), " planting acres)"),
         US_L3NAME = factor(US_L3NAME, levels = paste0(ecoregion_order, "\n(", scales::comma(round(total_eco_acres$Total_Acres)), " planting acres)")))

# Define color palette
productivity_colors <- viridis::plasma(n = length(unique(planted_by_productivity_top5_eco$productivity_class)))

# Create faceted plot
planted_productivity_eco_plot <- ggplot(planted_by_productivity_top5_eco, 
                                        aes(x = productivity_class, y = net_planted_acres, fill = productivity_class)) +
  geom_col(position = "stack", color = "black", size = 0.1) +
  facet_wrap(~ US_L3NAME, scales = "free", ncol = 2) +
  scale_fill_manual(values = productivity_colors, name = "Productivity Class") +
  labs(title = "Net Planted Acres by Productivity Class and Ecoregion",
       subtitle = "USFS Region 5 | Fires 2000-2021 | Net Planted & Replanted Acres 2001-2022",
       x = "Productivity Class",
       y = "Net Planted Acres") +
  theme_bw(base_size = 10) +
  theme(
    legend.position = c("none"),
    legend.justification = c("right", "bottom"),
    legend.box.just = "right",
    legend.margin = margin(6, 0, 6, 6),
    legend.background = element_rect(fill = "white", color = NA),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(face = "bold", size = 12),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 11),
    strip.text = element_text(face = "bold", size = 9),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9)
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_y_continuous(labels = scales::comma_format())


print(planted_productivity_eco_plot)

# Save the plot
ggsave("net_planted_acres_by_productivity_and_ecoregion.png", planted_productivity_eco_plot, width = 7, height = 4, dpi = 300)



library(dplyr)
library(ggplot2)
library(scales)
library(forcats)
library(stringr)
library(viridis)

# Prepare the data
planted_by_productivity_eco <- planted_veg_severity_eco %>%
  st_drop_geometry() %>%
  mutate(productivity_class = cut(mean_prod, 
                                  breaks = seq(0, ceiling(max(mean_prod, na.rm = TRUE)), by = 1),
                                  labels = seq(1, ceiling(max(mean_prod, na.rm = TRUE))),
                                  include.lowest = TRUE)) %>%
  group_by(US_L3NAME, productivity_class) %>%
  summarize(net_planted_acres = sum(Acres, na.rm = TRUE),
            .groups = "drop") %>%
  filter(!is.na(productivity_class))

# Rename longer ecoregion names
ecoregion_names <- c(
  "Klamath Mountains/California High North Coast Range" = "Klamath Mountains & North Coast",
  "Eastern Cascades Slopes and Foothills" = "East Cascades Slopes",
  "Central California Foothills and Coastal Mountains" = "Central Foothills & Coastal Mountains"
)

planted_by_productivity_eco <- planted_by_productivity_eco %>%
  mutate(US_L3NAME = str_replace_all(US_L3NAME, ecoregion_names))

# Calculate total acres planted for each ecoregion
total_eco_acres <- planted_by_productivity_eco %>%
  group_by(US_L3NAME) %>%
  summarize(Total_Acres = sum(net_planted_acres, na.rm = TRUE)) %>%
  arrange(desc(Total_Acres))

# Identify top ecoregions (let's use top 8 for better visibility)
top_ecoregions <- total_eco_acres %>%
  top_n(5, Total_Acres) %>%
  pull(US_L3NAME)

# Filter data for top ecoregions
planted_by_productivity_top_eco <- planted_by_productivity_eco %>%
  filter(US_L3NAME %in% top_ecoregions)

# Create ordered factor for ecoregions
planted_by_productivity_top_eco$US_L3NAME <- factor(planted_by_productivity_top_eco$US_L3NAME, 
                                                    levels = top_ecoregions)

# Create stacked bar plot
planted_productivity_stacked_plot <- ggplot(planted_by_productivity_top_eco, 
                                            aes(x = productivity_class, y = net_planted_acres, fill = US_L3NAME)) +
  geom_col(position = "stack", color = "black", size = 0.1) +
  scale_fill_viridis_d(option = "plasma", name = "Ecoregion") +
  labs(title = "Net Postfire Planting by Timber Productivity Class and Ecoregion",
       subtitle = "USFS Region 5 | Fires 2000-2021 | Net Planted & Replanted Acres 2001-2022",
       x = "Productivity Class",
       y = "Net Planted Acres") +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 11, face = "bold"),
    plot.title = element_text(face = "bold", size = 12),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 11),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9)
  ) +
  scale_y_continuous(labels = scales::comma_format(), 
                     expand = expansion(mult = c(0, 0.05))) +
  scale_x_discrete(name = "Productivity Class")

print(planted_productivity_stacked_plot)

# Save the plot
ggsave("net_planted_acres_by_productivity_stacked_ecoregion.png", planted_productivity_stacked_plot, width = 7, height = 4, dpi = 300)
