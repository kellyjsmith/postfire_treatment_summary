# Combined Figures
library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)
library(forcats)
library(stringr)
library(scales)

# Load necessary data and functions
source("Analysis/zonal_planted_combined_veg_sev.R")

# Load data (adjust paths as necessary)
planted_veg_severity_eco <- readRDS("Output/planted_veg_severity_eco_summary_with_reburns.RDS")
planted_veg_severity_eco_suid <- readRDS("Output/planted_veg_severity_eco_suid.RDS")

# Rename ecoregions
ecoregion_names <- c(
  "Klamath Mountains/California High North Coast Range" = "Klamath Mountains & North Coast",
  "Eastern Cascades Slopes and Foothills" = "East Cascades Slopes",
  "Central California Foothills and Coastal Mountains" = "Central Foothills & Coastal Mountains"
)

# Function to prepare data for plotting
prepare_plot_data <- function(data, top_n = 5) {
  data %>%
    mutate(US_L3NAME = str_replace_all(US_L3NAME, ecoregion_names)) %>%
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
    ungroup() %>%
    # Get top n ecoregions
    left_join(
      group_by(., US_L3NAME) %>%
        summarize(Total_Acres = sum(Area)) %>%
        top_n(top_n, Total_Acres),
      by = "US_L3NAME"
    ) %>%
    filter(!is.na(Total_Acres), Severity_Class != "Non-Processing Area") %>%
    mutate(
      US_L3NAME = paste0(US_L3NAME, "\n(", scales::comma(round(Total_Acres)), " planting acres)"),
      US_L3NAME = factor(US_L3NAME, levels = unique(US_L3NAME[order(Total_Acres, decreasing = TRUE)]))
    )
}

# Prepare data for both datasets
plot_data_eco <- prepare_plot_data(planted_veg_severity_eco)
plot_data_suid <- prepare_plot_data(planted_veg_severity_eco_suid)

# Define color palette and order vegetation types
veg_colors <- c("Conifer" = "springgreen4", 
                "Shrubland" = "goldenrod", 
                "Hardwood" = "purple2",
                "Grassland" = "yellow",
                "Other" = "gray70")

veg_order <- c("Conifer", "Shrubland", "Hardwood", "Grassland", "Other")

# Function to create plot
create_veg_severity_plot <- function(data, title) {
  ggplot(data, aes(x = fct_rev(Severity_Class), y = Percentage, fill = fct_rev(Veg_Type))) +
    geom_col(position = "stack", color = "black", size = 0.1) +
    facet_wrap(~ US_L3NAME, scales = "free_x", ncol = 2) +
    scale_fill_manual(values = veg_colors, name = "Vegetation Type") +
    labs(title = title,
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
}

# Create and save plots
plot_eco <- create_veg_severity_plot(plot_data_eco, "Existing Vegetation Types in Postfire Plantations by Burn Severity & Ecoregion (Fire/Ig Year)")
plot_suid <- create_veg_severity_plot(plot_data_suid, "Existing Vegetation Types in Postfire Plantations by Burn Severity & Ecoregion (SUID)")

ggsave("Figures/planted_veg_severity_distribution_ecoregions_fire_ig_year.png", plot_eco, width = 7, height = 5.5, dpi = 300)
ggsave("Figures/planted_veg_severity_distribution_ecoregions_suid.png", plot_suid, width = 7, height = 5.5, dpi = 300)

# Reburn summary function
calculate_reburn_summary <- function(data) {
  data %>%
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
}

# Calculate and print reburn summaries
reburn_summary_eco <- calculate_reburn_summary(planted_veg_severity_eco)
reburn_summary_suid <- calculate_reburn_summary(planted_veg_severity_eco_suid)

print("Reburn summary for Fire/Ig Year grouping:")
print(reburn_summary_eco)

print("Reburn summary for SUID grouping:")
print(reburn_summary_suid)

# Save reburn summaries
write.csv(reburn_summary_eco, "Output/reburn_summary_fire_ig_year.csv", row.names = FALSE)
write.csv(reburn_summary_suid, "Output/reburn_summary_suid.csv", row.names = FALSE)