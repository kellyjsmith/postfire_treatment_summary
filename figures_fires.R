library(dplyr)
library(sf)
library(terra)
library(ggplot2)
library(tidyr)
library(forcats)
library(stringr)

# Load necessary data
treated_fires <- readRDS("treated_fires.RDS")
st_write(treated_fires, "treated_fires_r5_00_21.shp")
cal_eco3 <- st_read("../Data/ca_eco_l3.shp") %>%
  st_transform(3310)

fire_events <- fire_events %>% mutate(fire_id = paste(Incid_Name, Event_ID, sep="_"))

# Intersect treated fires with ecoregions
treated_fires_eco <- fire_events %>%
  st_transform(3310) %>%
  st_intersection(cal_eco3 %>% select(US_L3NAME)) %>%
  group_by(US_L3NAME, Ig_Year, fire_id) %>%
  summarize(
    geometry = st_union(geometry),
    total_acres = sum(as.numeric(st_area(geometry))) / 4046.86,
    .groups = "drop"
  )

# Function to summarize vegetation and severity for treated fires
summarize_treated_fires_veg_severity_eco <- function(areas, evt_raster_file, evt_csv_file, severity_folder) {
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
          total_acres = sum(fire_data$total_acres),
          geometry = st_union(st_geometry(fire_data))
        )
      
      return(summary)
    }, .keep = TRUE) %>%
    bind_rows()
  
  results_sf <- st_as_sf(results)
  
  return(results_sf)
}

# Load veg and severity locations
evt_raster_file <- "../Data/landfire/LC23_EVT_240.tif"
evt_csv_file <- "../Data/landfire/LF23_EVT_240.csv"
severity_folder <- "../Data/Severity"

# Run the function
treated_fires_veg_severity_eco <- summarize_treated_fires_veg_severity_eco(treated_fires_eco, evt_raster_file, evt_csv_file, severity_folder)

# Save results
saveRDS(treated_fires_veg_severity_eco, "treated_fires_veg_severity_eco_summary.RDS")
st_write(treated_fires_veg_severity_eco, "treated_fires_veg_severity_eco_summary.shp", append = FALSE)
write.csv(treated_fires_veg_severity_eco %>% st_drop_geometry(), "treated_fires_veg_severity_eco_summary.csv", row.names = FALSE)

treated_fires_veg_severity_eco <- readRDS("treated_fires_veg_severity_eco.RDS")



# Calculate areas and percentages by vegetation type, severity class, and ecoregion
treated_fires_severity_year <- treated_fires_veg_severity_eco %>%
  st_drop_geometry() %>%
  group_by(Ig_Year, Severity_Class) %>%
  summarize(
    Area = sum(Acres),
    .groups = "drop"
  ) %>%
  ungroup()

# Calculate areas and percentages by vegetation type, severity class, and ecoregion
treated_fires_veg_severity_eco_summary <- treated_fires_veg_severity_eco %>%
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

treated_fires_veg_severity_eco_summary <- treated_fires_veg_severity_eco_summary %>%
  mutate(US_L3NAME = str_replace_all(US_L3NAME, ecoregion_names))

# Create a table of areas by severity class for each ecoregion
treated_fires_severity_eco_table <- treated_fires_veg_severity_eco_summary %>%
  group_by(US_L3NAME, Severity_Class) %>%
  summarize(
    Total_Burned_Acres = sum(Area),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Severity_Class,
    values_from = Total_Burned_Acres,
    values_fill = 0
  ) %>%
  mutate(Total = rowSums(across(where(is.numeric)))) %>%
  arrange(desc(Total))

# Identify top 5 ecoregions with the most burned areas
top_5_ecoregions <- treated_fires_severity_eco_table %>%
  top_n(5, Total) %>%
  pull(US_L3NAME)

# Filter data for top 5 ecoregions and exclude non-processing area
treated_fires_veg_severity_top5_eco <- treated_fires_veg_severity_eco_summary %>%
  filter(US_L3NAME %in% top_5_ecoregions,
         Severity_Class != "Non-Processing Area")

# Calculate total acres burned for each ecoregion
total_eco_burned_acres <- treated_fires_veg_severity_eco_summary %>%
  group_by(US_L3NAME) %>%
  summarize(Total_Acres = sum(Area, na.rm = TRUE)) %>%
  arrange(desc(Total_Acres))

# Create ordered factor for ecoregions
ecoregion_order <- total_eco_burned_acres$US_L3NAME

# Modify severity labels
severity_order <- c("I-G", "U-L", "Low", "Moderate", "High")
treated_fires_veg_severity_top5_eco <- treated_fires_veg_severity_top5_eco %>%
  mutate(Severity_Class = case_when(
    Severity_Class == "Increased Greenness" ~ "I-G",
    Severity_Class == "Unburned to Low" ~ "U-L",
    TRUE ~ as.character(Severity_Class)
  )) %>%
  mutate(Severity_Class = factor(Severity_Class, levels = severity_order))

# Add total acres to ecoregion names with proper comma formatting and order
treated_fires_veg_severity_top5_eco <- treated_fires_veg_severity_top5_eco %>%
  left_join(total_eco_burned_acres, by = "US_L3NAME") %>%
  mutate(US_L3NAME = paste0(US_L3NAME, "\n(", scales::comma(round(Total_Acres)), " burned acres)"),
         US_L3NAME = factor(US_L3NAME, levels = paste0(ecoregion_order, "\n(", scales::comma(round(total_eco_burned_acres$Total_Acres)), " burned acres)")))

# Define color palette and order vegetation types
veg_colors <- c("Conifer" = "springgreen4", 
                "Shrubland" = "goldenrod", 
                "Hardwood" = "purple2",
                "Grassland" = "yellow",
                "Other" = "gray70")

veg_order <- c("Conifer", "Shrubland", "Hardwood", "Grassland", "Other")
treated_fires_veg_severity_top5_eco$Veg_Type <- factor(treated_fires_veg_severity_top5_eco$Veg_Type, levels = veg_order)

# Create faceted plot
treated_fires_veg_severity_plot <- ggplot(treated_fires_veg_severity_top5_eco, aes(x = fct_rev(Severity_Class), y = Percentage, fill = fct_rev(Veg_Type))) +
  geom_col(position = "stack", color = "black", size = 0.1) +
  facet_wrap(~ US_L3NAME, scales = "free_x", ncol = 2) +
  scale_fill_manual(values = veg_colors, name = "Vegetation Type") +
  labs(title = "Existing Vegetation Types in Treated Fires by Burn Severity & Ecoregion",
       subtitle = "USFS Region 5 | Fires with Reforestation Activities, 2000-2021",
       x = "Severity Class",
       y = "% of Burned Area") +
  theme_bw(base_size = 10) +
  theme(
    legend.position = c(0.95, 0.05),
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
  guides(fill = guide_legend(nrow = 2, rev = TRUE)) +
  scale_x_discrete(position = "bottom") +
  coord_flip()

print(treated_fires_veg_severity_plot)

# Save the plot
ggsave("treated_fires_veg_severity_distribution_ecoregions.png", treated_fires_veg_severity_plot, width = 7, height = 5, dpi = 300)

# Save the table
write.csv(treated_fires_severity_eco_table, "treated_fires_severity_eco_table.csv", row.names = FALSE)



library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(scales)

# Assuming treated_fires_veg_severity_eco is already loaded

treated_fires_veg_severity_eco_data <- treated_fires_veg_severity_eco %>% 
  filter(Severity_Class != "Non-Processing Area",
         Severity_Class != "Increased Greenness")

# Prepare the data
burned_area_distribution <- treated_fires_veg_severity_eco_data %>%
  st_drop_geometry() %>%
  group_by(US_L3NAME, Ig_Year, Severity_Class) %>%
  summarize(Burned_Acres = sum(Acres, na.rm = TRUE), .groups = "drop") %>%
  mutate(US_L3NAME = str_replace_all(US_L3NAME, ecoregion_names))

# Calculate percentages
burned_area_distribution <- burned_area_distribution %>%
  group_by(US_L3NAME, Ig_Year) %>%
  mutate(Percentage = Burned_Acres / sum(Burned_Acres) * 100) %>%
  ungroup()

# Calculate total burned acres for each ecoregion
total_eco_burned <- burned_area_distribution %>%
  group_by(US_L3NAME) %>%
  summarize(Total_Burned = sum(Burned_Acres, na.rm = TRUE)) %>%
  arrange(desc(Total_Burned))

# Select top 5 ecoregions
top_5_ecoregions <- total_eco_burned$US_L3NAME[1:5]

# Filter for top 5 ecoregions and add total burned acres to names
burned_area_top5 <- burned_area_distribution %>%
  filter(US_L3NAME %in% top_5_ecoregions) %>%
  left_join(total_eco_burned, by = "US_L3NAME") %>%
  mutate(US_L3NAME = factor(paste0(US_L3NAME, "\n(", scales::comma(round(Total_Burned)), " total burned acres)"),
                            levels = paste0(top_5_ecoregions, "\n(", scales::comma(round(total_eco_burned$Total_Burned[1:5])), " total burned acres)")))

# Set up color palette for severity classes
severity_colors <- c(
  "Unburned to Low" = "darkgreen",
  "Low" = "skyblue",
  "Moderate" = "yellow2",
  "High" = "red"
)

# Create the stacked area plot
ggplot(burned_area_top5, aes(x = Ig_Year, y = Percentage, fill = Severity_Class)) +
  geom_area(position = "stack", color = "black", size = 0.1) +
  facet_wrap(~ US_L3NAME, scales = "free", ncol = 2) +
  scale_fill_manual(values = severity_colors, name = "Severity Class") +
  scale_x_continuous(breaks = seq(2000, 2020, by = 5), expand = c(0, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = c(0, 0),
                     breaks = seq(0, 100, by = 25)) +
  labs(title = "Distribution of Burned Area by Severity Class Over Time",
       subtitle = "USFS Region 5 | Fires 2000-2021",
       x = "Ignition Year",
       y = "Percentage of Burned Area") +
  theme_bw(base_size = 11) +
  theme(
    legend.position = c(0.92, 0.02),  # This places the legend inside, bottom right
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
  guides(fill = guide_legend(nrow = 2))

# Save the plot
ggsave("burned_area_percentage_distribution_by_severity_and_ecoregion.png", width = 7, height = 5, dpi = 300)
