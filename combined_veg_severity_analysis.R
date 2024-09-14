library(dplyr)
library(sf)
library(terra)

# Load necessary data
processed_activities <- readRDS("processed_activities_final.RDS")
cal_eco3 <- st_read("../Data/ca_eco_l3.shp") %>%
  st_transform(3310)

# Update net_activities with reburn information and intersect with ecoregions
net_activities_eco <- processed_activities %>%
  filter(type_labels %in% c("Initial Planting", "Fill-in or Replant")) %>%
  st_collection_extract() %>%
  st_cast("MULTIPOLYGON") %>%
  st_transform(3310) %>%
  st_intersection(cal_eco3 %>% select(US_L3NAME)) %>%
  group_by(Ig_Year, fire_id, US_L3NAME) %>%
  summarize(
    geometry = st_union(geometry),
    n_plantings = n(),
    net_acres = as.numeric(st_area(geometry)) / 4046.86,
    mean_reburns = mean(reburns, na.rm = TRUE),
    min_reburns = min(reburns, na.rm = TRUE),
    max_reburns = max(reburns, na.rm = TRUE),
    mean_diff = mean(diff_years, na.rm = TRUE),
    min_diff = min(diff_years, na.rm = TRUE),
    max_diff = max(diff_years, na.rm = TRUE),
    mean_prod = mean(as.numeric(PRODUCTIVI), na.rm = TRUE),
    min_prod = min(as.numeric(PRODUCTIVI), na.rm = TRUE),
    max_prod = max(as.numeric(PRODUCTIVI), na.rm = TRUE)
  ) %>%
  ungroup()

saveRDS(net_activities_eco, "net_activities_eco_with_reburns.RDS")


# Function to crop & mask the net postfire activities with severity and veg rasters
summarize_veg_severity_eco <- function(areas, evt_raster_file, evt_csv_file, severity_folder) {
  
  evt_raster <- rast(evt_raster_file)
  evt_metadata <- read.csv(evt_csv_file)
  
  cell_size_acres <- (res(evt_raster)[1] * res(evt_raster)[2]) / 4046.86
  
  results <- areas %>%
    group_by(fire_id, US_L3NAME) %>%
    group_map(~ {
      fire_data <- .x
      
      if (!"Ig_Year" %in% names(fire_data) || length(unique(fire_data$Ig_Year)) != 1) {
        warning(paste("Skipping fire_id:", unique(fire_data$fire_id), "- Invalid or missing Ig_Year"))
        return(NULL)
      }
      
      ig_year <- unique(fire_data$Ig_Year)
      current_fire_id <- unique(fire_data$fire_id)
      current_eco <- unique(fire_data$US_L3NAME)
      
      severity_file <- file.path(severity_folder, paste0("mtbs_CA_", ig_year, ".tif"))
      if (!file.exists(severity_file)) {
        warning(paste("Severity file not found for Ig_Year:", ig_year, "- Skipping this fire"))
        return(NULL)
      }
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
          weighted_mean_diff = weighted.mean(fire_data$mean_diff, w = fire_data$net_acres),
          min_diff = min(fire_data$min_diff),
          max_diff = max(fire_data$max_diff),
          weighted_mean_prod = weighted.mean(fire_data$mean_prod, w = fire_data$net_acres),
          min_prod = min(fire_data$min_prod),
          max_prod = max(fire_data$max_prod),
          weighted_mean_reburns = weighted.mean(fire_data$mean_reburns, w = fire_data$net_acres),
          min_reburns = min(fire_data$min_reburns),
          max_reburns = max(fire_data$max_reburns),
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
veg_severity_eco <- summarize_veg_severity_eco(net_activities_eco, evt_raster_file, evt_csv_file, severity_folder)

# Save results
saveRDS(veg_severity_eco, "veg_severity_eco_summary_with_reburns.RDS")
st_write(veg_severity_eco, "veg_severity_eco_summary_with_reburns.shp", append = FALSE)
write.csv(veg_severity_eco %>% st_drop_geometry(), "veg_severity_eco_summary_with_reburns.csv", row.names = FALSE)





# Calculate areas and percentages by vegetation type, severity class, and ecoregion
veg_severity_eco_summary <- veg_severity_eco %>%
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
severity_eco_table <- veg_severity_eco_summary %>%
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
top_6_ecoregions <- severity_eco_table %>%
  top_n(6, Total) %>%
  pull(US_L3NAME)

# Create figures for top 5 ecoregions
plot_veg_severity_distribution <- function(eco_name) {
  veg_severity_eco_summary %>%
    filter(US_L3NAME == eco_name) %>%
    ggplot(aes(x = Severity_Class, y = Percentage, fill = Veg_Type)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_viridis_d() +
    labs(
      title = paste("Vegetation Type Distribution by Severity Class in", eco_name),
      x = "Severity Class",
      y = "Percentage",
      fill = "Vegetation Type"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.position = "right"
    )
}

# Generate plots for top 5 ecoregions
eco_plots <- lapply(top_6_ecoregions, plot_veg_severity_distribution)

# Display the first plot as an example
print(eco_plots)

# Save the table
write.csv(severity_eco_table, "severity_eco_table.csv", row.names = FALSE)

# Save the plots
for (i in 1:length(eco_plots)) {
  ggsave(paste0("veg_severity_distribution_", gsub(" ", "_", top_5_ecoregions[i]), ".png"), 
         eco_plots[[i]], width = 10, height = 6)
}



reburn_by_severity <- veg_severity_eco %>%
  st_drop_geometry() %>%
  group_by(Severity_Class) %>%
  summarize(
    mean_reburns = mean(weighted_mean_reburns, na.rm = TRUE),
    max_reburns = max(max_reburns, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_reburns))




library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(stringr)

# Filter data for top 6 ecoregions and exclude non-processing area
veg_severity_top_6 <- veg_severity_eco_summary %>%
  filter(US_L3NAME %in% top_6_ecoregions,
         Severity_Class != "Non-Processing Area")

# Rename longer ecoregion names
ecoregion_names <- c(
  "Klamath Mountains/California High North Coast Range" = "Klamath Mountains & North Coast",
  "Eastern Cascades Slopes and Foothills" = "East Cascades Slopes",
  "Central California Foothills and Coastal Mountains" = "Central Foothills & Coastal Mountains"
)

veg_severity_top_6 <- veg_severity_top_6 %>%
  mutate(US_L3NAME = str_replace_all(US_L3NAME, ecoregion_names))

# Calculate total acres planted for each ecoregion
total_acres <- veg_severity_top_6 %>%
  group_by(US_L3NAME) %>%
  summarize(Total_Acres = sum(Area, na.rm = TRUE)) %>%
  arrange(desc(Total_Acres))

# Create ordered factor for ecoregions
ecoregion_order <- total_acres$US_L3NAME

# Add total acres to ecoregion names with proper comma formatting and order
veg_severity_top_6 <- veg_severity_top_6 %>%
  left_join(total_acres, by = "US_L3NAME") %>%
  mutate(US_L3NAME = paste0(US_L3NAME, "\n(", comma(round(Total_Acres)), " planting acres)"),
         US_L3NAME = factor(US_L3NAME, levels = paste0(ecoregion_order, "\n(", comma(round(total_acres$Total_Acres)), " planting acres)")))


# Reorder severity classes
severity_order <- c("Increased Greenness", "Unburned to Low", "Low", "Moderate", "High")
veg_severity_top_6$Severity_Class <- factor(veg_severity_top_6$Severity_Class, levels = severity_order)

# Define color palette and order vegetation types
veg_colors <- c("Conifer" = "springgreen4", 
                "Shrubland" = "goldenrod", 
                "Hardwood" = "purple2",
                "Grassland" = "yellow",
                "Other" = "gray70")

veg_order <- c("Conifer", "Shrubland", "Hardwood", "Grassland", "Other")
veg_severity_top_6$Veg_Type <- factor(veg_severity_top_6$Veg_Type, levels = veg_order)


# Create faceted plot
ggplot(veg_severity_top_6, aes(x = fct_rev(Severity_Class), y = Percentage, fill = fct_rev(Veg_Type))) +
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








