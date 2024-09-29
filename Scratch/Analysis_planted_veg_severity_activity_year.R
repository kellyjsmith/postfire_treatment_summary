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
net_planting_eco_year <- processed_activities %>%
  filter(type_labels %in% c("Initial Planting", "Fill-in or Replant")) %>%
  st_collection_extract("POLYGON") %>%
  st_transform(3310) %>%
  group_by(fire_id, year) %>%
  mutate(geometry = st_union(geometry)) %>% 
  ungroup() %>%
  st_intersection(cal_eco3 %>% select(US_L3NAME)) %>%
  group_by(fire_id, year) %>%
  mutate(area_fraction = st_area(geometry) / sum(st_area(geometry))) %>%
  ungroup() %>%
  group_by(US_L3NAME, Ig_Year, fire_id, year) %>%
  summarize(
    geometry = st_union(geometry),
    n_plantings = n(),
    net_acres = sum(as.numeric(st_area(geometry)) / 4046.86 * area_fraction),
    mean_reburns = weighted.mean(reburns, w = area_fraction, na.rm = TRUE),
    min_reburns = min(reburns, na.rm = TRUE),
    max_reburns = max(reburns, na.rm = TRUE),
    mean_diff = weighted.mean(diff_years, w = area_fraction, na.rm = TRUE),
    min_diff = min(diff_years, na.rm = TRUE),
    max_diff = max(diff_years, na.rm = TRUE),
    mean_prod = weighted.mean(as.numeric(PRODUCTIVI), w = area_fraction, na.rm = TRUE),
    min_prod = min(as.numeric(PRODUCTIVI), na.rm = TRUE),
    max_prod = max(as.numeric(PRODUCTIVI), na.rm = TRUE)
  ) %>%
  ungroup()

saveRDS(net_planting_eco_year, "net_planting_eco_year.RDS")

net_planting_eco_year <- readRDS("net_planting_eco_year.RDS")

# Function to crop & mask the net postfire activities with severity and veg rasters
summarize_veg_severity_eco_year <- function(areas, evt_raster_file, evt_csv_file, severity_folder) {
  
  evt_raster <- rast(evt_raster_file)
  evt_metadata <- read.csv(evt_csv_file)
  
  cell_size_acres <- (res(evt_raster)[1] * res(evt_raster)[2]) / 4046.86
  
  results <- areas %>%
    group_by(US_L3NAME, fire_id, year) %>%
    group_map(~ {
      fire_data <- .x
      
      ig_year <- unique(fire_data$Ig_Year)
      current_fire_id <- unique(fire_data$fire_id)
      current_eco <- unique(fire_data$US_L3NAME)
      current_year <- unique(fire_data$year)
      
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
      
      summary$US_L3NAME <- current_eco
      summary$Ig_Year <- ig_year
      summary$fire_id <- current_fire_id
      summary$year <- current_year
      
      summary <- summary %>%
        mutate(
          n_plantings = sum(fire_data$n_plantings),
          weighted_mean_diff = fire_data$mean_diff[1],
          min_diff = fire_data$min_diff[1],
          max_diff = fire_data$max_diff[1],
          weighted_mean_prod = fire_data$mean_prod[1],
          min_prod = fire_data$min_prod[1],
          max_prod = fire_data$max_prod[1],
          weighted_mean_reburns = fire_data$mean_reburns[1],
          min_reburns = fire_data$min_reburns[1],
          max_reburns = fire_data$max_reburns[1],
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
planted_veg_severity_eco_year <- summarize_veg_severity_eco_year(net_planting_eco_year, evt_raster_file, evt_csv_file, severity_folder)

# Save results
saveRDS(planted_veg_severity_eco_year, "planted_veg_severity_eco_year.RDS")
st_write(planted_veg_severity_eco_year, "planted_veg_severity_eco_year.shp", append = FALSE)
write.csv(planted_veg_severity_eco_year %>% st_drop_geometry(), "planted_veg_severity_eco_year.csv", row.names = FALSE)






# Calculate areas and percentages by vegetation type, severity class, and ecoregion
planted_veg_severity_eco_year_summary <- planted_veg_severity_eco_year %>%
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

# Create a table of areas by severity class for each ecoregion
veg_severity_year_table <- planted_veg_severity_eco_year_summary %>%
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

# Identify top 5 ecoregions with the most planting areas
top_6_ecoregions <- veg_severity_year_table %>%
  top_n(6, Total) %>%
  pull(US_L3NAME)


# Filter data for top 6 ecoregions and exclude non-processing area
year_veg_severity_top6_eco <- planted_veg_severity_eco_year_summary %>%
  filter(US_L3NAME %in% top_6_ecoregions,
         Severity_Class != "Non-Processing Area")

# Rename longer ecoregion names
ecoregion_names <- c(
  "Klamath Mountains/California High North Coast Range" = "Klamath Mountains & North Coast",
  "Eastern Cascades Slopes and Foothills" = "East Cascades Slopes",
  "Central California Foothills and Coastal Mountains" = "Central Foothills & Coastal Mountains"
)

year_veg_severity_top6_eco <- year_veg_severity_top6_eco %>%
  mutate(US_L3NAME = str_replace_all(US_L3NAME, ecoregion_names))

# Calculate total acres planted for each ecoregion
total_eco_acres <- planted_veg_severity_eco_year_summary %>%
  group_by(US_L3NAME) %>%
  summarize(Total_Acres = sum(Area, na.rm = TRUE)) %>%
  arrange(desc(Total_Acres))

# Create ordered factor for ecoregions
ecoregion_order <- total_eco_acres$US_L3NAME

# Add total acres to ecoregion names with proper comma formatting and order
year_veg_severity_top6_eco <- year_veg_severity_top6_eco %>%
  left_join(total_eco_acres, by = "US_L3NAME") %>%
  mutate(US_L3NAME = paste0(US_L3NAME, "\n(", comma(round(Total_Acres)), " planting acres)"),
         US_L3NAME = factor(US_L3NAME, levels = paste0(ecoregion_order, "\n(", comma(round(total_eco_acres$Total_Acres)), " planting acres)")))


# Reorder severity classes
severity_order <- c("Increased Greenness", "Unburned to Low", "Low", "Moderate", "High")
year_veg_severity_top6_eco$Severity_Class <- factor(year_veg_severity_top6_eco$Severity_Class, levels = severity_order)

# Define color palette and order vegetation types
veg_colors <- c("Conifer" = "springgreen4", 
                "Shrubland" = "goldenrod", 
                "Hardwood" = "purple2",
                "Grassland" = "yellow",
                "Other" = "gray70")

veg_order <- c("Conifer", "Shrubland", "Hardwood", "Grassland", "Other")
year_veg_severity_top6_eco$Veg_Type <- factor(year_veg_severity_top6_eco$Veg_Type, levels = veg_order)


# Create faceted plot
ggplot(year_veg_severity_top6_eco, aes(x = fct_rev(Severity_Class), y = Percentage, fill = fct_rev(Veg_Type))) +
  geom_col(position = "stack", color = "black", size = 0.1) +
  facet_wrap(~ US_L3NAME, scales = "free_x", ncol = 2) +
  scale_fill_manual(values = veg_colors, name = "Vegetation Type") +
  labs(title = "Existing Vegetation Types in Postfire Plantations by Burn Severity & Ecoregion",
       subtitle = "USFS Region 5 | Fires 2000-2021 | Net Planted & Replanted Acres 2001-2022",
       x = "Severity Class",
       y = "% of Net Planted Area") +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(face = "bold", size = 12),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 9),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9)
  ) +
  guides(fill = guide_legend(nrow = 1, rev = TRUE)) +
  scale_x_discrete(position = "bottom") +
  coord_flip()

# Save the plot
ggsave("veg_severity_distribution_ecoregions.png", width = 7, height = 5.5, dpi = 300)


