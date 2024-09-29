library(dplyr)
library(sf)
library(terra)
library(ggplot2)
library(tidyr)
library(forcats)
library(stringr)

setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output")

# Load necessary data
processed_activities <- readRDS("processed_activities_final.RDS")
cal_eco3 <- st_read("../Data/ca_eco_l3.shp") %>%
  st_transform(3310)

net_planting_eco_suid <- processed_activities %>%
  filter(type_labels %in% c("Initial Planting", "Fill-in or Replant")) %>%
  st_collection_extract() %>%
  st_transform(3310) %>%
  mutate(activity_area = st_area(.)) %>%
  group_by(fire_id, year) %>%
  mutate(
    gross_acres = sum(as.numeric(activity_area)) / 4046.86,
    mean_unit_size = mean(as.numeric(activity_area)) / 4046.86
  ) %>%
  ungroup() %>%
  st_intersection(cal_eco3 %>% select(US_L3NAME)) %>%
  group_by(US_L3NAME, Ig_Year, fire_id, SUID, year) %>%
  summarize(
    geometry = st_union(geometry),
    n_plantings = n(),
    net_acres = sum(as.numeric(st_area(geometry))) / 4046.86,
    gross_acres = first(gross_acres),
    diff_years = first(diff_years),
    prod = first(PRODUCTIVI),
    reburns = first(reburns),
    .groups = "drop"
  )

saveRDS(net_planting_eco_suid, "net_planting_eco_suid.RDS")

net_planting_eco_suid <- readRDS("net_planting_eco_suid.RDS")


reburn_summary <- net_planting_eco_suid %>%
  summarize(
    total_planted_acres = sum(net_acres),
    reburned_acres = sum(net_acres[reburns > 0]),
    percent_reburned = (reburned_acres / total_planted_acres) * 100
  )

print(reburn_summary)



# Assuming net_planting_eco_suid is already loaded
net_planting_summary <- net_planting_eco_suid %>% 
  st_drop_geometry() %>% 
  group_by(US_L3NAME, fire_id) %>% 
  summarize(
    n_plantings = sum(n_plantings),
    total_acres = sum(net_acres),
    reburned_acres = sum(net_acres[max_reburns > 0]),
    n_reburned_suids = n_distinct(SUID[max_reburns > 0]),
    min_reburns = min(min_reburns),
    mean_reburns = mean(mean_reburns),
    max_reburns = max(max_reburns),
    mean_unit_acres = mean(mean_unit_size),
    mean_diff = mean(mean_diff)
  ) %>% ungroup() %>% 
  # group_by(US_L3NAME) %>% 
  summarize(
    n_plantings = sum(n_plantings),
    total_acres = sum(total_acres),
    reburned_acres = sum(reburned_acres),
    n_reburned_suids = sum(n_reburned_suids),
    percent_reburned = reburned_acres / total_acres * 100,
    min_reburns = min(min_reburns),
    mean_reburns = mean(mean_reburns),
    max_reburns = max(max_reburns),
    mean_unit_acres = mean(mean_unit_acres),
    mean_diff = mean(mean_diff)
  )

view(net_planting_summary)
            

summarize_veg_severity_eco_suid <- function(areas, evt_raster_file, evt_csv_file, severity_folder) {
  
  evt_raster <- rast(evt_raster_file)
  evt_metadata <- read.csv(evt_csv_file)
  
  cell_size_acres <- (res(evt_raster)[1] * res(evt_raster)[2]) / 4046.86
  
  results <- areas %>%
    group_by(fire_id, US_L3NAME, SUID) %>%
    group_map(~ {
      fire_data <- .x
      
      ig_year <- unique(fire_data$Ig_Year)
      current_fire_id <- unique(fire_data$fire_id)
      current_eco <- unique(fire_data$US_L3NAME)
      current_suid <- unique(fire_data$SUID)
      
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
      
      summary <- fire_data %>%
        st_drop_geometry() %>%
        select(year, diff_years, prod, reburns, gross_acres, n_plantings) %>%
        mutate(total_pixels = nrow(values)) %>%
        left_join(
          values %>%
            group_by(Severity_Class, Veg_Type) %>%
            summarise(Pixels = n(), .groups = "drop") %>%
            mutate(Proportion = Pixels / sum(Pixels)),
          by = character()
        ) %>%
        mutate(
          Acres = Pixels * cell_size_acres,
          US_L3NAME = current_eco,
          Ig_Year = ig_year,
          fire_id = current_fire_id,
          SUID = current_suid
        )
      
      summary$geometry <- st_union(st_geometry(fire_data))
      
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
planted_veg_severity_eco_suid <- summarize_veg_severity_eco_suid(net_planting_eco_suid, evt_raster_file, evt_csv_file, severity_folder)


# Save results
saveRDS(planted_veg_severity_eco_suid, "planted_veg_severity_eco_suid.RDS")
st_write(planted_veg_severity_eco_suid, "planted_veg_severity_eco_suid.shp", append = FALSE)
write.csv(planted_veg_severity_eco_suid %>% st_drop_geometry(), "planted_veg_severity_eco_suid.csv", row.names = FALSE)

planted_veg_severity_eco_suid <- readRDS("planted_veg_severity_eco_suid.RDS")

# Summary function with success score and map_labels
summarize_with_success_score <- function(data) {
  data %>%
    group_by(US_L3NAME, fire_id, year, SUID, Ig_Year) %>%  # Add Ig_Year to grouping
    summarize(
      Tot_Ac = sum(Acres),
      Con_Ac = sum(Acres[Veg_Type == "Conifer"]),
      Shb_Ac = sum(Acres[Veg_Type == "Shrubland"]),
      HW_Ac = sum(Acres[Veg_Type == "Hardwood"]),
      Grs_Ac = sum(Acres[Veg_Type == "Grassland"]),
      Oth_Ac = sum(Acres[Veg_Type == "Other"]),
      Con_Pct = Con_Ac / Tot_Ac * 100,
      Shb_Pct = Shb_Ac / Tot_Ac * 100,
      HW_Pct = HW_Ac / Tot_Ac * 100,
      Grs_Pct = Grs_Ac / Tot_Ac * 100,
      Oth_Pct = Oth_Ac / Tot_Ac * 100,
      Prod = first(prod),
      Mean_Diff = mean(diff_years, na.rm = TRUE),
      Reburns = max(reburns, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Con_Shb_Ra = Con_Ac / (Shb_Ac + 0.1),
      Refor_Score = (Con_Pct + (Con_Shb_Ra / (1 + Con_Shb_Ra)) * 100) / 2,
      map_labels = sprintf("SUID: %s\nNet Acres: %.1f\nReforestation Score: %.1f\nPct Shrub: %.1f\nPlant Year: %d\nReburns: %d", 
                           SUID, Tot_Ac, Refor_Score, Shb_Pct, year, Reburns)
    ) %>%
    ungroup()
}

planted_veg_summary <- summarize_with_success_score(planted_veg_severity_eco_suid)

# Round numeric columns to one decimal place
planted_veg_summary <- planted_veg_summary %>%
  mutate(across(where(is.numeric), ~round(., 1)))

saveRDS(planted_veg_summary, "planted_veg_summary.RDS")

# Display the first few rows of the summary
print(head(planted_veg_summary))

# Save the summary as a shapefile
st_write(planted_veg_summary, "postfire_plantation_success_labels_final.shp", append = FALSE)

# Optionally, save the summary to a CSV file
write.csv(planted_veg_summary, "planted_veg_summary_by_fire.csv", row.names = FALSE)


vegetation_activities_suid <- st_read("vegetation_activities_SUID.shp")



# Calculate areas and percentages by vegetation type, severity class, and ecoregion
planted_veg_severity_eco_suid_summary <- planted_veg_severity_eco_suid %>%
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





# Rename ecoregions
ecoregion_names <- c(
  "Klamath Mountains/California High North Coast Range" = "Klamath Mountains & North Coast",
  "Eastern Cascades Slopes and Foothills" = "East Cascades Slopes",
  "Central California Foothills and Coastal Mountains" = "Central Foothills & Coastal Mountains"
)

planted_veg_severity_eco_suid_summary <- planted_veg_severity_eco_suid_summary %>%
  mutate(US_L3NAME = str_replace_all(US_L3NAME, ecoregion_names))

# Create a table of areas by severity class for each ecoregion
veg_severity_suid_table <- planted_veg_severity_eco_suid_summary %>%
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
top_6_ecoregions <- veg_severity_suid_table %>%
  top_n(6, Total) %>%
  pull(US_L3NAME)

# Filter data for top 6 ecoregions and exclude non-processing area
suid_veg_severity_top6_eco <- planted_veg_severity_eco_suid_summary %>%
  filter(US_L3NAME %in% top_6_ecoregions,
         Severity_Class != "Non-Processing Area")

# Calculate total acres planted for each ecoregion
total_eco_acres <- planted_veg_severity_eco_suid_summary %>%
  group_by(US_L3NAME) %>%
  summarize(Total_Acres = sum(Area, na.rm = TRUE)) %>%
  arrange(desc(Total_Acres))

# Create ordered factor for ecoregions
ecoregion_order <- total_eco_acres$US_L3NAME

# Add total acres to ecoregion names with proper comma formatting and order
suid_veg_severity_top6_eco <- suid_veg_severity_top6_eco %>%
  left_join(total_eco_acres, by = "US_L3NAME") %>%
  mutate(US_L3NAME = paste0(US_L3NAME, "\n(", scales::comma(round(Total_Acres)), " planting acres)"),
         US_L3NAME = factor(US_L3NAME, levels = paste0(ecoregion_order, "\n(", scales::comma(round(total_eco_acres$Total_Acres)), " planting acres)")))

# Reorder severity classes
severity_order <- c("Increased Greenness", "Unburned to Low", "Low", "Moderate", "High")
suid_veg_severity_top6_eco$Severity_Class <- factor(suid_veg_severity_top6_eco$Severity_Class, levels = severity_order)

# Define color palette and order vegetation types
veg_colors <- c("Conifer" = "springgreen4", 
                "Shrubland" = "goldenrod", 
                "Hardwood" = "purple2",
                "Grassland" = "yellow",
                "Other" = "gray70")

veg_order <- c("Conifer", "Shrubland", "Hardwood", "Grassland", "Other")
suid_veg_severity_top6_eco$Veg_Type <- factor(suid_veg_severity_top6_eco$Veg_Type, levels = veg_order)

# Create faceted plot
planted_veg_severity_suid_plot <- ggplot(suid_veg_severity_top6_eco, aes(x = fct_rev(Severity_Class), y = Percentage, fill = fct_rev(Veg_Type))) +
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

print(planted_veg_severity_suid_plot)

# Save the plot
ggsave("planted_veg_severity_distribution_ecoregions.png", planted_veg_severity_plot, width = 7, height = 5.5, dpi = 300)

# Save the table
write.csv(veg_severity_suid_table, "planted_severity_eco_table.csv", row.names = FALSE)


# Calculate total planted acres and reburned acres
reburn_summary_severity <- planted_veg_severity_eco_suid %>%
  group_by(fire_id) %>%
  summarize(
    total_planted_acres = sum(Acres, na.rm = TRUE),
    reburned_acres = sum(Acres[reburns > 0]),
    high_severity_reburned_acres = sum(Acres[reburns > 0 & Severity_Class == "High"])
  ) %>%
  summarize(
    total_planted_acres = sum(total_planted_acres),
    total_reburned_acres = sum(reburned_acres),
    total_high_severity_reburned_acres = sum(high_severity_reburned_acres),
    percent_reburned = (total_reburned_acres / total_planted_acres) * 100,
    percent_high_severity_reburned = (total_high_severity_reburned_acres / total_planted_acres) * 100
  )

print(reburn_summary_severity)
