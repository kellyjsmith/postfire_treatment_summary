library(dplyr)
library(sf)
library(terra)
library(ggplot2)
library(tidyr)
library(forcats)
library(stringr)

# Load necessary data
processed_activities <- readRDS("processed_activities_final.RDS")
cal_eco3 <- st_read("../Data/ca_eco_l3.shp") %>%
  st_transform(3310)

# Update net_activities with reburn information and intersect with ecoregions
planted_net_activities_eco <- processed_activities %>%
  filter(type_labels %in% c("Initial Planting", "Fill-in or Replant")) %>%
  st_collection_extract("POLYGON") %>%
  st_transform(3310) %>%
  mutate(activity_area = st_area(.)) %>%
  group_by(fire_id, Ig_Year) %>%
  mutate(
    gross_acres = sum(as.numeric(activity_area)) / 4046.86,
    mean_unit_size = mean(as.numeric(activity_area)) / 4046.86
  ) %>%
  ungroup() %>%
  st_intersection(cal_eco3 %>% select(US_L3NAME)) %>%
  group_by(US_L3NAME, Ig_Year, fire_id, type_labels) %>%
  summarize(
    geometry = st_union(geometry),
    n_plantings = n(),
    net_acres = sum(as.numeric(st_area(geometry))) / 4046.86,
    gross_acres = first(gross_acres),
    mean_unit_size = first(mean_unit_size),
    mean_reburns = mean(reburns, na.rm = TRUE),
    min_reburns = min(reburns, na.rm = TRUE),
    max_reburns = max(reburns, na.rm = TRUE),
    mean_diff = mean(diff_years, na.rm = TRUE),
    min_diff = min(diff_years, na.rm = TRUE),
    max_diff = max(diff_years, na.rm = TRUE),
    mean_prod = mean(as.numeric(PRODUCTIVI), na.rm = TRUE),
    min_prod = min(as.numeric(PRODUCTIVI), na.rm = TRUE),
    max_prod = max(as.numeric(PRODUCTIVI), na.rm = TRUE),
    .groups = "drop"
  )

saveRDS(planted_net_activities_eco, "planted_net_activities_eco_with_reburns.RDS")

planted_net_activities_eco <- readRDS("planted_net_activities_eco_with_reburns.RDS")


reburn_summary <- processed_activities %>%
  filter(type_labels == "Initial Planting") %>% 
  summarize(
    total_planted_acres = sum(activity_fire_area)/4046.86,
    reburned_acres = sum(activity_fire_area[reburns > 0]/4046.86),
    percent_reburned = (reburned_acres / total_planted_acres) * 100
  )

print(reburn_summary)


# Function to crop & mask the net postfire activities with severity and veg rasters
summarize_planted_veg_severity_eco <- function(areas, evt_raster_file, evt_csv_file, severity_folder) {
  
  evt_raster <- rast(evt_raster_file)
  evt_metadata <- read.csv(evt_csv_file)
  
  cell_size_acres <- (res(evt_raster)[1] * res(evt_raster)[2]) / 4046.86
  
  results <- areas %>%
    group_by(fire_id, US_L3NAME, Ig_Year) %>%
    group_map(~ {
      fire_data <- .x
      
      ig_year <- unique(fire_data$Ig_Year)
      current_fire_id <- unique(fire_data$fire_id)
      current_eco <- unique(fire_data$US_L3NAME)
      
      severity_file <- file.path(severity_folder, paste0("mtbs_CA_", ig_year, ".tif"))
      severity_raster <- rast(severity_file)
      
      fire_data_vect <- vect(fire_data)
      fire_data_vect <- project(fire_data_vect, crs(evt_raster))
      
      evt_crop <- crop(evt_raster, fire_data_vect)
      evt_mask <- mask(evt_crop, fire_data_vect)
      
      severity_crop <- crop(severity_raster, fire_data_vect)
      severity_mask <- mask(severity_crop, fire_data_vect)
      
      evt_values <- as.data.frame(evt_mask, xy = TRUE)
      severity_values <- as.data.frame(severity_mask, xy = TRUE)
      
      values <- left_join(evt_values, severity_values, by = c("x", "y"))
      names(values) <- c("x", "y", "evt", "severity")
      values <- na.omit(values)
      
      values <- left_join(values, evt_metadata[, c("VALUE", "EVT_PHYS")], by = c("evt" = "VALUE"))
      
      values <- values %>%
        mutate(Veg_Type = case_when(
          EVT_PHYS == "Conifer" ~ "Conifer",
          EVT_PHYS == "Shrubland" ~ "Shrubland",
          EVT_PHYS %in% c("Hardwood", "Riparian", "Conifer-Hardwood") ~ "Hardwood",
          EVT_PHYS == "Grassland" ~ "Grassland",
          TRUE ~ "Other"
        ))
      
      values$Severity_Class <- factor(values$severity,
                                      levels = 1:6,
                                      labels = c("Unburned to Low", "Low", "Moderate", "High", 
                                                 "Increased Greenness", "Non-Processing Area"))
      
      summary <- values %>%
        group_by(Severity_Class, Veg_Type) %>%
        summarise(Acres = n() * cell_size_acres, .groups = "drop") %>%
        mutate(Proportion = Acres / sum(Acres))
      
      summary$fire_id <- current_fire_id
      summary$Ig_Year <- ig_year
      summary$US_L3NAME <- current_eco
      
      summary <- summary %>%
        mutate(
          n_plantings = sum(fire_data$n_plantings),
          mean_diff = mean(fire_data$mean_diff),
          min_diff = min(fire_data$min_diff),
          max_diff = max(fire_data$max_diff),
          mean_prod = mean(fire_data$mean_prod),
          min_prod = min(fire_data$min_prod),
          max_prod = max(fire_data$max_prod),
          mean_reburns = mean(fire_data$mean_reburns),
          min_reburns = min(fire_data$min_reburns),
          max_reburns = max(fire_data$max_reburns),
          gross_acres = sum(fire_data$gross_acres),
          mean_unit_size = mean(fire_data$mean_unit_size),
          geometry = st_union(st_geometry(fire_data))
        )
      
      return(summary)
    }, .keep = TRUE) %>%
    bind_rows()
  
  # Convert the results to an sf object
  results_sf <- st_as_sf(results)
  
  return(results_sf)
}

# Load veg and severity locations
evt_raster_file <- "../Data/landfire/LC23_EVT_240.tif"
evt_csv_file <- "../Data/landfire/LF23_EVT_240.csv"
severity_folder <- "../Data/Severity"

# Run the function
planted_veg_severity_eco <- summarize_planted_veg_severity_eco(planted_net_activities_eco, evt_raster_file, evt_csv_file, severity_folder)

# Save results
saveRDS(planted_veg_severity_eco, "planted_veg_severity_eco_summary_with_reburns.RDS")
st_write(planted_veg_severity_eco, "planted_veg_severity_eco_summary_with_reburns.shp", append = FALSE)
write.csv(planted_veg_severity_eco %>% st_drop_geometry(), "planted_veg_severity_eco_summary_with_reburns.csv", row.names = FALSE)



# Calculate reforestation success score by fire, severity class, and ecoregion
reforestation_success_eco <- planted_veg_severity_eco %>%
  group_by(fire_id, US_L3NAME, Ig_Year, Severity_Class) %>%
  summarize(
    Total_Acres = sum(Acres),
    Conifer_Acres = sum(Acres[Veg_Type == "Conifer"]),
    Shrub_Acres = sum(Acres[Veg_Type == "Shrubland"]),
    Conifer_Percent = Conifer_Acres / Total_Acres * 100,
    Conifer_Shrub_Ratio = Conifer_Acres / (Shrub_Acres + 0.1),
    Success_Score = (Conifer_Percent + (Conifer_Shrub_Ratio / (1 + Conifer_Shrub_Ratio)) * 100) / 2,
    .groups = "drop"
  )




# Calculate areas and percentages by vegetation type, severity class, and ecoregion
planted_veg_severity_eco_summary <- planted_veg_severity_eco %>%
  st_drop_geometry() %>%
  group_by(US_L3NAME, Severity_Class, Veg_Type) %>%
  summarize(
    Area = sum(Acres),
    .groups = "drop"
  ) %>%
  group_by(US_L3NAME, Severity_Class) %>%
  mutate(
    Total_Area = sum(Area),
    Percentage = Area / Total_Area * 100
  ) %>%
  ungroup()






# Rename longer ecoregion names
ecoregion_names <- c(
  "Klamath Mountains/California High North Coast Range" = "Klamath Mountains & North Coast",
  "Eastern Cascades Slopes and Foothills" = "East Cascades Slopes",
  "Central California Foothills and Coastal Mountains" = "Central Foothills & Coastal Mountains"
)

planted_veg_severity_eco_summary <- planted_veg_severity_eco_summary %>%
  mutate(US_L3NAME = str_replace_all(US_L3NAME, ecoregion_names))

# Create a table of areas by severity class for each ecoregion
planted_severity_eco_table <- planted_veg_severity_eco_summary %>%
  group_by(US_L3NAME, Severity_Class) %>%
  summarize(
    Total_Planted_Acres = sum(Area),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Severity_Class,
    values_from = Total_Planted_Acres,
    values_fill = 0
  ) %>%
  mutate(Total = rowSums(across(where(is.numeric)))) %>%
  arrange(desc(Total))

# Identify top 6 ecoregions with the most planting areas
top_5_ecoregions <- planted_severity_eco_table %>%
  top_n(5, Total) %>%
  pull(US_L3NAME)

# Filter data for top 6 ecoregions and exclude non-processing area
planted_veg_severity_top5_eco <- planted_veg_severity_eco_summary %>%
  filter(US_L3NAME %in% top_5_ecoregions,
         Severity_Class != "Non-Processing Area")

# Calculate total acres planted for each ecoregion
total_eco_acres <- planted_veg_severity_eco_summary %>%
  group_by(US_L3NAME) %>%
  summarize(Total_Acres = sum(Area, na.rm = TRUE)) %>%
  arrange(desc(Total_Acres))

# Create ordered factor for ecoregions
ecoregion_order <- total_eco_acres$US_L3NAME

# Modify severity labels
severity_order <- c("Unb.-Low", "Low", "Moderate", "High")
planted_veg_severity_top5_eco <- planted_veg_severity_top5_eco %>%
  filter(!Severity_Class == "Increased Greenness") %>% 
  mutate(Severity_Class = case_when(
    Severity_Class == "Unburned to Low" ~ "Unb.-Low",
    TRUE ~ as.character(Severity_Class)
  )) %>%
  mutate(Severity_Class = factor(Severity_Class, levels = severity_order))

# Add total acres to ecoregion names with proper comma formatting and order
planted_veg_severity_top5_eco <- planted_veg_severity_top5_eco %>%
  left_join(total_eco_acres, by = "US_L3NAME") %>%
  mutate(US_L3NAME = paste0(US_L3NAME, "\n(", scales::comma(round(Total_Acres)), " planting acres)"),
         US_L3NAME = factor(US_L3NAME, levels = paste0(ecoregion_order, "\n(", scales::comma(round(total_eco_acres$Total_Acres)), " planting acres)")))

# Define color palette and order vegetation types
veg_colors <- c("Conifer" = "springgreen4", 
                "Shrubland" = "goldenrod", 
                "Hardwood" = "purple2",
                "Grassland" = "yellow",
                "Other" = "gray70")

veg_order <- c("Conifer", "Shrubland", "Hardwood", "Grassland", "Other")
planted_veg_severity_top5_eco$Veg_Type <- factor(planted_veg_severity_top5_eco$Veg_Type, levels = veg_order)

# Create faceted plot
planted_veg_severity_plot <- ggplot(planted_veg_severity_top5_eco, aes(x = fct_rev(Severity_Class), y = Percentage, fill = fct_rev(Veg_Type))) +
  geom_col(position = "stack", color = "black", size = 0.1) +
  facet_wrap(~ US_L3NAME, scales = "free_x", ncol = 2) +
  scale_fill_manual(values = veg_colors, name = "Vegetation Type") +
  labs(title = "Existing Vegetation Types in Postfire Plantations by Burn Severity & Ecoregion",
       subtitle = "USFS Region 5 | Fires 2000-2021 | Net Planted & Replanted Acres 2001-2022",
       x = "Severity Class",
       y = "% of Net Planted Area") +
  theme_bw(base_size = 10) +
  theme(
    legend.position = c(0.95, 0.05),  # This places the legend inside, bottom right
    legend.justification = c("right", "bottom"),  # This aligns the legend to the bottom right
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
  guides(fill = guide_legend(nrow = 2, rev = TRUE)) +
  scale_x_discrete(position = "bottom") +
  coord_flip()

print(planted_veg_severity_plot)

# Save the plot
ggsave("planted_veg_severity_distribution_ecoregions.png", planted_veg_severity_plot, width = 7, height = 5, dpi = 300)



library(ggplot2)
library(dplyr)
library(forcats)
library(scales)
library(stringr)

# Prepare the data
planting_summary_severity <- planted_veg_severity_eco %>%
  group_by(US_L3NAME, Ig_Year, Severity_Class) %>%
  summarize(Net_Planted_Acres = sum(Acres, na.rm = TRUE), .groups = "drop") %>%
  filter(Severity_Class != "Non-Processing Area")

# Set factor levels for severity
severity_order <- c("Increased Greenness", "Unburned to Low", "Low", "Moderate", "High")
planting_summary_severity$Severity_Class <- factor(planting_summary_severity$Severity_Class, levels = severity_order)

# Define severity colors
severity_colors <- c(
  "Unburned to Low" = "darkgreen",
  "Low" = "skyblue",
  "Moderate" = "yellow",
  "High" = "red"
)

# Rename longer ecoregion names
ecoregion_names <- c(
  "Klamath Mountains/California High North Coast Range" = "Klamath Mountains & North Coast",
  "Eastern Cascades Slopes and Foothills" = "East Cascades Slopes",
  "Central California Foothills and Coastal Mountains" = "Central Foothills & Coastal Mountains"
)

planting_summary_severity <- planting_summary_severity %>%
  mutate(US_L3NAME = str_replace_all(US_L3NAME, ecoregion_names))

# Calculate total acres planted for each ecoregion
total_eco_acres <- planting_summary_severity %>%
  st_drop_geometry() %>% 
  group_by(US_L3NAME) %>%
  summarize(Total_Acres = sum(Net_Planted_Acres, na.rm = TRUE)) %>%
  arrange(desc(Total_Acres))

# Select top 5 ecoregions
top_5_ecoregions <- total_eco_acres %>%
  top_n(5, Total_Acres) %>%
  pull(US_L3NAME)

# Filter data for top 5 ecoregions
planting_summary_severity <- planting_summary_severity %>%
  filter(US_L3NAME %in% top_5_ecoregions)

# Create ordered factor for ecoregions
ecoregion_order <- total_eco_acres$US_L3NAME[total_eco_acres$US_L3NAME %in% top_5_ecoregions]

# Add total acres to ecoregion names with proper comma formatting and order
planting_summary_severity <- planting_summary_severity %>%
  left_join(total_eco_acres, by = "US_L3NAME") %>%
  mutate(US_L3NAME = paste0(US_L3NAME, "\n(", scales::comma(round(Total_Acres)), " total acres)"),
         US_L3NAME = factor(US_L3NAME, levels = paste0(ecoregion_order, "\n(", scales::comma(round(total_eco_acres$Total_Acres[total_eco_acres$US_L3NAME %in% top_5_ecoregions])), " total acres)")))

# Create the plot
ggplot(planting_summary_severity, aes(x = fct_rev(US_L3NAME), y = Net_Planted_Acres, fill = Severity_Class)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = severity_colors, name = "Burn Severity") +
  scale_y_continuous(labels = scales::comma_format(), expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Net Planted Acres by Ecoregion and Burn Severity",
       subtitle = "USFS Region 5, 2000-2021",
       x = "Ecoregion",
       y = "Net Planted Acres") +
  theme_bw(base_size = 10) +
  theme(
    legend.position = c(0.8, 0.2),
    legend.justification = c("right", "bottom"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 11),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9)
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
  coord_flip()

ggsave("net_planting_by_severity_ecoregion_top5.png", width = 7, height = 5, dpi = 300)


# Save the table
write.csv(planted_severity_eco_table, "planted_severity_eco_table.csv", row.names = FALSE)



library(dplyr)

# Calculate total planted acres and reburned acres
reburn_summary <- planted_veg_severity_eco %>%
  group_by(fire_id) %>%
  summarize(
    total_planted_acres = sum(Acres),
    reburned_acres = sum(Acres[max_reburns > 0]),
    high_severity_reburned_acres = sum(Acres[max_reburns > 0 & Severity_Class == "High"])
  ) %>%
  summarize(
    total_planted_acres = sum(total_planted_acres),
    total_reburned_acres = sum(reburned_acres),
    total_high_severity_reburned_acres = sum(high_severity_reburned_acres),
    percent_reburned = (total_reburned_acres / total_planted_acres) * 100,
    percent_high_severity_reburned = (total_high_severity_reburned_acres / total_planted_acres) * 100
  )

print(reburn_summary)
