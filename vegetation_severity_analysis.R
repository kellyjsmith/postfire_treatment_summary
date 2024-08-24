# Load required libraries
library(terra)
library(sf)
library(tidyverse)
library(purrr)

# Load assigned activities and fire data
assigned_activities <- readRDS("../../Output/assigned_activities_new.RDS")
fire_events <- st_read("../Data/Severity/R5_fires_00_22.shp")

# Load EVT data
evt_raster_file <- "../../Data/landfire/LC23_EVT_240.tif"
evt_csv <- read.csv("../../Data/landfire/LF23_EVT_240.csv")

# Prepare activities data
planted_activities <- assigned_activities %>%
  filter(type_labels == "Initial Planting") %>%
  st_collection_extract()

planted_activities$fire_id <- paste(planted_activities$Incid_Name, planted_activities$Ig_Year, sep="_")

# Group and split data
planted_areas <- planted_activities %>%
  group_by(type_labels, Ig_Year, fire_id) %>%
  summarize(geometry = st_union(geometry)) %>%
  ungroup()

planted_areas = st_cast(planted_areas, "MULTIPOLYGON")
planted_areas$net_planted_acres = as.numeric(st_area(planted_areas))/4046.86

planted_areas_list = group_split(planted_areas, type_labels, Ig_Year, fire_id)

# Prepare activities data
fire_activities <- assigned_activities %>%
  st_collection_extract()

fire_activities$fire_id <- paste(fire_activities$Incid_Name, fire_activities$Ig_Year, sep="_")

# Group and split data
fire_areas <- fire_activities %>%
  group_by(type_labels, Ig_Year, fire_id) %>%
  summarize(geometry = st_union(geometry)) %>%
  ungroup()

fire_areas = st_cast(fire_areas, "MULTIPOLYGON")
fire_areas$net_activity_acres = as.numeric(st_area(fire_areas))/4046.86

fire_areas_list = group_split(fire_areas, type_labels, Ig_Year, fire_id)

process_activities_raster_area <- function(x, evt_raster_file, evt_csv, mtbs_folder="../Data/Severity") {
  # Load the severity raster for the processing year
  severity_file <- paste0(mtbs_folder, "/mtbs_CA_", x$Ig_Year, ".tif")
  severity_raster <- rast(severity_file)
  
  # Load EVT raster
  evt_raster <- rast(evt_raster_file)
  
  # Ensure CRS match
  activity_vect <- vect(x)
  activity_vect <- project(activity_vect, crs(evt_raster))
  
  # Crop and mask EVT and severity rasters
  evt_crop <- crop(evt_raster, ext(activity_vect))
  evt_mask <- mask(evt_crop, activity_vect)
  
  severity_crop <- crop(severity_raster, ext(activity_vect))
  severity_mask <- mask(severity_crop, activity_vect)
  
  # Calculate cell size in acres
  cell_size_acres <- prod(res(evt_raster)) / 4046.86
  
  # Summarize vegetation and severity
  veg_summary <- as.data.frame(freq(evt_mask))
  names(veg_summary) <- c("Layer", "EVT_VALUE", "Count")
  veg_summary$Area_acres <- veg_summary$Count * cell_size_acres
  
  severity_summary <- as.data.frame(freq(severity_mask))
  names(severity_summary) <- c("Layer", "Severity", "Count")
  severity_summary$Area_acres <- severity_summary$Count * cell_size_acres
  
  # Reclassify severity values
  severity_classes <- c(
    "1" = "Unburned to Low",
    "2" = "Low",
    "3" = "Moderate",
    "4" = "High",
    "5" = "Increased Greenness",
    "6" = "Non-Processing Area"
  )
  severity_summary$Severity_Class <- severity_classes[as.character(severity_summary$Severity)]
  
  # Merge EVT data
  veg_summary <- merge(veg_summary, 
                       evt_csv[, c("VALUE", "EVT_NAME", "EVT_PHYS")], 
                       by.x = "EVT_VALUE", by.y = "VALUE",
                       all.x = TRUE)
  
  # Add metadata
  veg_summary$fire_id <- x$fire_id
  veg_summary$Ig_Year <- x$Ig_Year
  veg_summary$type_labels <- x$type_labels
  
  severity_summary$fire_id <- x$fire_id
  severity_summary$Ig_Year <- x$Ig_Year
  severity_summary$type_labels <- x$type_labels
  
  # Return a list containing both summaries
  return(list(vegetation = veg_summary, severity = severity_summary))
}

# Process planted areas
print("Processing planted areas...")
vegetation_severity_planted <- map(
  planted_areas_list,
  ~process_activities_raster_area(., evt_raster_file, evt_csv),
  .progress = TRUE
)

# Separate vegetation and severity data
vegetation_planted <- map_dfr(vegetation_severity_planted, "vegetation")
severity_planted <- map_dfr(vegetation_severity_planted, "severity")


# Process total burned areas (assuming you've prepared fire_areas_list as before)
print("Processing total burned areas...")
vegetation_severity_total <- map(
  fire_areas_list,
  ~process_activities_raster_area(., evt_raster_file, evt_csv),
  .progress = TRUE
)

# Separate vegetation and severity data for total burned areas
vegetation_total <- map_dfr(vegetation_severity_total, "vegetation")
severity_total <- map_dfr(vegetation_severity_total, "severity")

# Combine planted and total burned data
all_vegetation <- bind_rows(
  mutate(vegetation_planted, Category = "Planted"),
  mutate(vegetation_total, Category = "Total Burned")
)

all_severity <- bind_rows(
  mutate(severity_planted, Category = "Planted"),
  mutate(severity_total, Category = "Total Burned")
)




# Summarize vegetation data
total_veg_summary <- all_vegetation %>%
  drop_na() %>%
  mutate(Veg_Group = case_when(
    EVT_PHYS == "Conifer" ~ "Conifer",
    EVT_PHYS == "Shrubland" ~ "Shrubland",
    EVT_PHYS %in% c("Hardwood", "Riparian", "Conifer-Hardwood","Exotic Tree-Shrub")  ~ "Hardwood",
    TRUE ~ "Other"
  )) %>%
  group_by(Category, Ig_Year, Veg_Group) %>%
  summarize(Total_acres = sum(Area_acres)) %>%
  mutate(Percentage = Total_acres / sum(Total_acres) * 100)

veg_summary$Veg_Group = factor(veg_summary$Veg_Group, 
                               levels = c("Conifer","Shrubland","Hardwood","Other"))

# Summarize severity data
severity_summary <- all_severity %>%
  group_by(Category, Ig_Year, Severity_Class) %>%
  summarize(Total_acres = sum(Area_acres)) %>%
  mutate(Percentage = Total_acres / sum(Total_acres) * 100)

# Visualize vegetation results
ggplot(veg_summary, aes(x = Ig_Year, y = Percentage, fill = Veg_Group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Conifer" = "darkgreen", 
                               "Shrubland" = "seagreen3", 
                               "Hardwood" = "purple", 
                               "Other" = "orangered2")) +
  facet_wrap(~ Category) +
  theme_minimal() +
  labs(title = "Vegetation Distribution by Ignition Year",
       x = "Ignition Year", y = "Percentage", fill = "Vegetation Group")



#### Planted area summaries ####

# Summarize vegetation data
planted_veg_summary <- vegetation_planted %>%
  mutate(Veg_Group = case_when(
    EVT_PHYS == "Conifer" ~ "Conifer",
    EVT_PHYS == "Shrubland" ~ "Shrubland",
    EVT_PHYS %in% c("Hardwood", "Riparian", "Conifer-Hardwood", "Exotic Tree-Shrub") ~ "Hardwood",
    TRUE ~ "Other"
  )) %>%
  group_by(Ig_Year, fire_id) %>%
  summarize(
    Conifer_acres = sum(Area_acres[Veg_Group == "Conifer"]),
    Shrubland_acres = sum(Area_acres[Veg_Group == "Shrubland"]),
    Hardwood_acres = sum(Area_acres[Veg_Group == "Hardwood"]),
    Other_acres = sum(Area_acres[Veg_Group == "Other"]),
    Total_acres = sum(Area_acres)
  ) %>%
  mutate(
    Conifer_percent = Conifer_acres / Total_acres * 100,
    Shrubland_percent = Shrubland_acres / Total_acres * 100,
    Hardwood_percent = Hardwood_acres / Total_acres * 100,
    Other_percent = Other_acres / Total_acres * 100
  )

# Summarize severity data
planted_severity_summary <- severity_planted %>%
  group_by(Ig_Year, fire_id) %>%
  summarize(
    Unburned_to_Low_acres = sum(Area_acres[Severity_Class == "Unburned to Low"]),
    Low_acres = sum(Area_acres[Severity_Class == "Low"]),
    Moderate_acres = sum(Area_acres[Severity_Class == "Moderate"]),
    High_acres = sum(Area_acres[Severity_Class == "High"]),
    Greener_acres = sum(Area_acres[Severity_Class == "Increased Greenness"]),
    Total_severity_acres = sum(Area_acres)
  ) %>%
  mutate(
    Unburned_to_Low_percent = Unburned_to_Low_acres / Total_severity_acres * 100,
    Low_percent = Low_acres / Total_severity_acres * 100,
    Moderate_percent = Moderate_acres / Total_severity_acres * 100,
    High_percent = High_acres / Total_severity_acres * 100,
    Greener_percent = Greener_acres / Total_severity_acres * 100
  )

# Join vegetation and severity summaries
veg_severity_summary <- left_join(planted_veg_summary, planted_severity_summary, by = c("Ig_Year", "fire_id"))


# Prepare data for boxplot
veg_severity_long <- veg_severity_summary %>%
  select(fire_id, Ig_Year, 
         Conifer_percent, Shrubland_percent, Hardwood_percent, Other_percent,
         Unburned_to_Low_percent, Low_percent, Moderate_percent, High_percent) %>%
  pivot_longer(
    cols = c(Conifer_percent, Shrubland_percent, Hardwood_percent, Other_percent),
    names_to = "Veg_Type",
    values_to = "Veg_Percent"
  ) %>%
  pivot_longer(
    cols = c(Unburned_to_Low_percent, Low_percent, Moderate_percent, High_percent, Greener_percent),
    names_to = "Severity_Class",
    values_to = "Severity_Percent"
  ) %>%
  mutate(
    Veg_Type = factor(Veg_Type, levels = c("Conifer_percent", "Shrubland_percent", "Hardwood_percent","Other_percent"),
                      labels = c("Conifer", "Shrubland")),
    Severity_Class = factor(Severity_Class, 
                            levels = c("Unburned_to_Low_percent", "Low_percent", "Moderate_percent", "High_percent"),
                            labels = c("Unburned to Low", "Low", "Moderate", "High"))
  )

# Create the boxplot
ggplot(veg_severity_long, aes(x = Severity_Class, y = Veg_Percent, fill = Veg_Type)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("Conifer" = "darkgreen", "Shrubland" = "seagreen3")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Conifer and Shrubland Cover by Severity Class",
       x = "Severity Class", y = "Vegetation Cover (%)", fill = "Vegetation Type")

# Save results
write.csv(veg_severity_summary, "planted_veg_severity_summary.csv", row.names = FALSE)
saveRDS(veg_severity_summary, "planted_veg_severity_summary.RDS")


library(tidyverse)
library(ggplot2)

# Assuming veg_severity_summary is already loaded

# Calculate conifer to shrub ratio and high severity percentage
veg_ratio_high_severity <- veg_severity_summary %>%
  mutate(
    Conifer_Shrub_Ratio = Conifer / (Shrubland + 0.1),  # Adding 0.1 to avoid division by zero
    High_Severity_Percent = High / Total_Acres * 100
  ) %>%
  group_by(Ig_Year) %>%
  summarize(
    Mean_Conifer_Shrub_Ratio = mean(Conifer_Shrub_Ratio, na.rm = TRUE),
    Mean_High_Severity_Percent = mean(High_Severity_Percent, na.rm = TRUE),
    Total_Planted_Acres = sum(Total_Acres)
  )

# Visualize the relationship
ggplot(veg_ratio_high_severity, aes(x = Ig_Year)) +
  geom_col(aes(y = Mean_High_Severity_Percent, fill = "High Severity %"), alpha = 0.7) +
  geom_line(aes(y = Mean_Conifer_Shrub_Ratio * 10, color = "Conifer:Shrub Ratio"), size = 1) +
  geom_point(aes(y = Mean_Conifer_Shrub_Ratio * 10, color = "Conifer:Shrub Ratio"), size = 3) +
  scale_y_continuous(
    name = "High Severity %",
    sec.axis = sec_axis(~./10, name = "Conifer:Shrub Ratio")
  ) +
  scale_fill_manual(values = c("High Severity %" = "red")) +
  scale_color_manual(values = c("Conifer:Shrub Ratio" = "darkgreen")) +
  labs(
    title = "High Severity Burn % and Conifer:Shrub Ratio by Ignition Year",
    x = "Ignition Year",
    fill = "",
    color = ""
  ) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "darkgreen"),
    axis.text.y.right = element_text(color = "darkgreen"),
    legend.position = "bottom"
  )

# Create a scatter plot
ggplot(veg_severity_analysis, aes(x = Mean_High_Severity_Percent, y = Mean_Conifer_Shrub_Ratio)) +
  geom_point(aes(size = Total_Planted_Acres), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_size_continuous(name = "Total Planted Acres", labels = scales::comma) +
  labs(
    title = "Relationship between High Severity Burn % and Conifer:Shrub Ratio",
    x = "Mean High Severity %",
    y = "Mean Conifer:Shrub Ratio"
  ) +
  theme_minimal()

# Print correlation coefficient
cor_coef <- cor(veg_severity_analysis$Mean_High_Severity_Percent, veg_severity_analysis$Mean_Conifer_Shrub_Ratio)
print(paste("Correlation coefficient:", round(cor_coef, 3)))

# Save the analysis results
write.csv(veg_severity_analysis, "veg_severity_analysis_by_year.csv", row.names = FALSE)