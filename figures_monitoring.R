library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(patchwork)

# Function to create monitoring acres plot by ignition year
create_monitoring_plot <- function(data, type, color_palette, levels) {
  data[[type]] <- factor(data[[type]], levels = levels)
  
  ggplot(data, aes(x = Ig_Year, y = Acres, fill = !!sym(type))) +
    geom_col(position = "stack", color = "black", size = 0.1) +
    facet_wrap(~ type_labels, scales = "free", ncol = 2) +
    scale_fill_manual(values = color_palette) +
    labs(title = paste("Monitoring Acres by", type, "and Ignition Year"),
         subtitle = "USFS Region 5, 2000-2021",
         x = "Ignition Year",
         y = "Acres",
         fill = type) +
    theme_bw(base_size = 10) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          strip.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          axis.text = element_text(size = 9),
          legend.title = element_text(face = "bold"),
          plot.title = element_text(face = "bold", size = 12),
          plot.subtitle = element_text(size = 10)) +
    scale_y_continuous(labels = scales::comma)
}

# Function to create overall summary plot
create_summary_plot <- function(data, type, color_palette, levels) {
  data[[type]] <- factor(data[[type]], levels = levels)
  
  ggplot(data, aes(x = fct_rev(type_labels), y = Percentage, fill = fct_rev(!!sym(type)))) +
    geom_col(position = "stack", color = "black", size = 0.1) +
    scale_fill_manual(values = color_palette) +
    labs(title = paste("Overall Distribution of", type, "in Monitoring Activities"),
         subtitle = "USFS Region 5, 2000-2021",
         x = "Monitoring Activity",
         y = "Percentage",
         fill = type) +
    theme_bw(base_size = 10) +
    theme(legend.position = "right",
          axis.text.y = element_text(hjust = 1),
          axis.title = element_text(face = "bold"),
          axis.text = element_text(size = 9),
          legend.title = element_text(face = "bold"),
          plot.title = element_text(face = "bold", size = 12),
          plot.subtitle = element_text(size = 10)) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format(scale = 1))
}

# Prepare data for severity classes
severity_data <- severity_summary %>%
  filter(type_labels %in% c("Survival Survey", "Stocking Survey", "Certification - Plant", "Reforest. Need - Fire")) %>%
  select(type_labels, Ig_Year, Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres) %>%
  pivot_longer(cols = c(Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres),
               names_to = "Severity",
               values_to = "Acres") %>%
  mutate(Severity = gsub("_acres", "", Severity),
         case_when(Severity == "Unburned_to_Low" ~ "Unburned to Low"))

# Prepare data for vegetation types
veg_data <- veg_summary %>%
  filter(type_labels %in% c("Survival Survey", "Stocking Survey", "Certification - Plant", "Reforest. Need - Fire")) %>%
  select(type_labels, Ig_Year, Conifer_acres, Shrubland_acres, Hardwood_acres, Other_acres) %>%
  pivot_longer(cols = c(Conifer_acres, Shrubland_acres, Hardwood_acres, Other_acres),
               names_to = "Veg_Type",
               values_to = "Acres") %>%
  mutate(Veg_Type = gsub("_acres", "", Veg_Type))

# Color palettes and levels
severity_colors <- c("High" = "red", "Moderate" = "yellow2", "Low" = "skyblue", "Unburned to Low" = "darkgreen")
veg_colors <- c("Conifer" = "springgreen4", "Shrubland" = "goldenrod", "Hardwood" = "purple2", "Other" = "gray70")
severity_levels <- c("High", "Moderate", "Low", "Unburned to Low")
veg_levels <- c("Conifer", "Shrubland", "Hardwood", "Other")

# Create plots by ignition year
severity_plot <- create_monitoring_plot(severity_data, "Severity", severity_colors, severity_levels)
veg_plot <- create_monitoring_plot(veg_data, "Veg_Type", veg_colors, veg_levels)


# Prepare summary data
severity_summary_data <- severity_data %>%
  group_by(type_labels, Severity) %>%
  summarize(Total_Acres = sum(Acres, na.rm = TRUE)) %>%
  group_by(type_labels) %>%
  mutate(Percentage = Total_Acres / sum(Total_Acres))

veg_summary_data <- veg_data %>%
  group_by(type_labels, Veg_Type) %>%
  summarize(Total_Acres = sum(Acres, na.rm = TRUE)) %>%
  group_by(type_labels) %>%
  mutate(Percentage = Total_Acres / sum(Total_Acres))

# Create summary plots
severity_summary_plot <- create_summary_plot(severity_summary_data, "Severity", severity_colors, severity_levels)
veg_summary_plot <- create_summary_plot(veg_summary_data, "Veg_Type", veg_colors, veg_levels)

# Combine plots into lists
severity_plots <- list(main_plot = severity_plot, summary_plot = severity_summary_plot)
veg_plots <- list(main_plot = veg_plot, summary_plot = veg_summary_plot)

print(severity_plots)
print(veg_plots)
# Save plots
ggsave("Monitoring_Acres_by_Severity.png", severity_plots$main_plot, width = 12, height = 8)
ggsave("Monitoring_Acres_by_Veg_Type.png", veg_plots$main_plot, width = 12, height = 8)
ggsave("Monitoring_Summary_by_Severity.png", severity_plots$summary_plot, width = 8, height = 6)
ggsave("Monitoring_Summary_by_Veg_Type.png", veg_plots$summary_plot, width = 8, height = 6)






# Load the required data
net_activities_eco <- readRDS("net_activities_eco.RDS")

library(dplyr)
library(sf)
library(tidyr)
library(purrr)
library(ggplot2)
library(viridis)

activities_in_plantations_eco <- function(all_net_activities_eco) {
  plant <- all_net_activities_eco %>%
    filter(type_labels == "Initial Planting")
  
  monitoring <- all_net_activities_eco %>%
    filter(type_labels %in% c("Stocking Survey"))
  
  planted_by_fire <- plant %>%
    group_by(fire_id, US_L3NAME) %>%
    rename(net_total = net_acres) %>%
    ungroup()
  
  monitoring_by_fire <- monitoring %>%
    group_by(fire_id, US_L3NAME) %>%
    summarize(
      monitoring_geom = st_union(geometry),
      net_total = sum(net_acres),
      n_monitoring = n(),
      weighted_mean_diff = weighted.mean(mean_diff, w = net_acres),
      weighted_mean_prod = weighted.mean(mean_prod, w = net_acres),
      max_reburns = first(max_reburns),
      Ig_Year = first(Ig_Year)
    ) %>%
    ungroup()
  
  calculate_intersection <- function(monitoring_group, planted_by_fire) {
    fire <- monitoring_group$fire_id
    eco <- monitoring_group$US_L3NAME
    
    tryCatch({
      planted_area <- planted_by_fire %>% 
        filter(fire_id == fire, US_L3NAME == eco)
      
      if (nrow(planted_area) == 0) {
        return(tibble(
          fire_id = fire,
          US_L3NAME = eco,
          Ig_Year = monitoring_group$Ig_Year,
          net_total_monitoring = monitoring_group$net_total,
          n_monitoring = monitoring_group$n_monitoring,
          intersecting_acres = 0,
          percent_total_planted = 0,
          percent_total_activity = 0,
          mean_diff = monitoring_group$weighted_mean_diff,
          mean_productivity = monitoring_group$weighted_mean_prod,
          reburns = monitoring_group$max_reburns
        ))
      }
      
      intersection <- st_intersection(monitoring_group$monitoring_geom, planted_area$geometry)
      intersecting_acres <- sum(as.numeric(st_area(intersection)) / 4046.86)
      
      planted_total_area <- sum(planted_area$net_total)
      
      percent_total_planted <- (intersecting_acres / planted_total_area) * 100
      percent_total_activity <- (intersecting_acres / monitoring_group$net_total) * 100
      
      tibble(
        fire_id = fire,
        US_L3NAME = eco,
        Ig_Year = monitoring_group$Ig_Year,
        net_total_monitoring = monitoring_group$net_total,
        n_monitoring = monitoring_group$n_monitoring,
        intersecting_acres = intersecting_acres,
        percent_total_planted = percent_total_planted,
        percent_total_activity = percent_total_activity,
        mean_diff = monitoring_group$weighted_mean_diff,
        mean_productivity = monitoring_group$weighted_mean_prod,
        reburns = monitoring_group$max_reburns
      )
    }, error = function(e) {
      warning(paste("Error processing fire_id:", fire, "- Error:", e$message))
      return(NULL)
    })
  }
  
  monitoring_in_plant <- map_dfr(1:nrow(monitoring_by_fire), 
                                 ~calculate_intersection(monitoring_by_fire[.x,], planted_by_fire), 
                                 .progress = TRUE)
  
  combined_monitoring_in_plant <- monitoring_in_plant %>%
    left_join(planted_by_fire %>% 
                group_by(fire_id, US_L3NAME) %>% 
                summarize(net_total_planted = sum(net_total)), 
              by = c("fire_id", "US_L3NAME"))
  
  return(combined_monitoring_in_plant)
}

monitoring_in_plantations_eco <- activities_in_plantations_eco(all_net_activities_eco)

saveRDS(monitoring_in_plantations_eco, "monitoring_in_plantations_eco.RDS")


# Create a summary table
monitoring_summary_by_eco <- monitoring_in_plantations_eco %>%
  group_by(US_L3NAME) %>%
  summarize(
    total_fires = n(),
    total_planted_acres = sum(net_total_planted, na.rm = TRUE),
    total_monitored_acres = sum(intersecting_acres, na.rm = TRUE),
    total_percent_monitored = (total_monitored_acres / total_planted_acres) * 100,
    median_percent_monitored = median(percent_total_planted, na.rm = TRUE),
    mean_percent_monitored = mean(percent_total_planted, na.rm = TRUE),
    min_percent_monitored = min(percent_total_planted, na.rm = TRUE),
    max_percent_monitored = max(percent_total_planted, na.rm = TRUE),
    avg_mean_diff = mean(mean_diff, na.rm = TRUE),
    avg_mean_productivity = mean(mean_productivity, na.rm = TRUE),
    max_reburns = max(reburns, na.rm = TRUE),
    avg_n_monitoring = mean(n_monitoring, na.rm = TRUE)
  ) %>%
  arrange(desc(total_percent_monitored))

# Display the summary table
print(monitoring_summary_by_eco)

saveRDS(monitoring_summary_by_eco, "monitoring_summary_by_eco.RDS")

# Save the summary table
write.csv(monitoring_summary_by_eco, "monitoring_summary_by_ecoregion.csv", row.names = FALSE)

# Create figures

# 1. Percentage of planted acres monitored by ecoregion

ggplot(monitoring_summary_by_eco, aes(x = reorder(US_L3NAME, total_percent_monitored), y = total_percent_monitored)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  geom_text(aes(label = comma(round(total_monitored_acres))), 
            hjust = 0.5, 
            color = "black", 
            fontface = "bold", 
            size = 3.5) +
  coord_flip() +
  labs(title = "Percentage of Postfire Plantations with Stocking Surveys by Ecoregion",
       subtitle = "USFS R5 | Fires 2001-2021 | Net Planting and Stocking Survey 2001-2022
       \nAcres of Stocking Survey shown next to bars",
       x = "Ecoregion",
       y = "Percent with Stocking Surveys") +
  theme_bw(base_size = 10) +
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 12, face = "bold"),
        axis.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 10),
        axis.text.y = element_text(size = 9)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(0, 100))

ggsave("percent_stocked_by_ecoregion_with_acres.png", width = 7, height = 4)


# Rename longer ecoregion names
ecoregion_names <- c(
  "Klamath Mountains/California High North Coast Range" = "Klamath Mountains & North Coast",
  "Eastern Cascades Slopes and Foothills" = "East Cascades Slopes",
  "Central California Foothills and Coastal Mountains" = "Central Foothills & Coastal Mountains"
)

# Update the ecoregion names in the data
monitoring_summary_by_eco <- monitoring_summary_by_eco %>%
  mutate(US_L3NAME = str_replace_all(US_L3NAME, ecoregion_names))

# Identify the two ecoregions with the least monitored acres
least_monitored <- monitoring_summary_by_eco %>%
  arrange(total_monitored_acres) %>%
  slice_head(n = 2) %>%
  pull(US_L3NAME)

# Prepare the label data
label_data <- monitoring_summary_by_eco %>%
  filter(US_L3NAME %in% least_monitored) %>%
  mutate(label = sprintf("%s planted, %s surveyed", 
                         scales::comma(round(total_planted_acres)), 
                         scales::comma(round(total_monitored_acres))))

# Create the plot
ggplot(monitoring_summary_by_eco, aes(x = reorder(US_L3NAME, total_planted_acres))) +
  geom_col(aes(y = total_planted_acres, fill = "Initial Planting"), stat = "identity") +
  geom_bar(aes(y = total_monitored_acres, fill = "Planting + Survey"), stat = "identity") +
  geom_text(data = label_data, 
            aes(y = total_planted_acres, label = label),
            hjust = -0.1, 
            size = 3.25,
            fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("Initial Planting" = "forestgreen", "Planting + Survey" = "skyblue"),
                    name = "Activity Type") +
  labs(title = "Postfire Planted Acres with Stocking Survey by Ecoregion",
       subtitle = "USFS R5 | Fires 2000-2021 | Net Planting and Stocking Survey 2001-2022",
       x = "Ecoregion",
       y = "Acres") +
  theme_bw(base_size = 11) +
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 12, face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(size = 9),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 10)) +
  scale_y_continuous(labels = scales::comma_format(),
                     expand = expansion(mult = c(0, 0.1)))

ggsave("planted_and_surveyed_acres_by_ecoregion.png", width = 7, height = 3)




# Monitored in plantations by year and ecoregion

top5_monitored_list <- monitoring_in_plantations_eco %>%
  group_by(US_L3NAME) %>%
  summarize(total_stock_survey = sum(intersecting_acres, na.rm = TRUE)) %>%
  arrange(desc(total_stock_survey)) %>%
  slice_head(n = 5) %>%
  pull(US_L3NAME)

# Now, let's prepare the data for these top 6 ecoregions
top5_monitored <- monitoring_in_plantations_eco %>%
  filter(US_L3NAME %in% top5_monitored_list) %>%
  group_by(US_L3NAME, Ig_Year) %>%
  summarize(
    total_planted = sum(net_total_planted, na.rm = TRUE),
    total_stock_survey = sum(intersecting_acres, na.rm = TRUE),
    .groups = "drop"
  )

# Create the faceted plot
ggplot(top5_monitored, aes(x = Ig_Year)) +
  geom_col(aes(y = total_planted, fill = "Net Planted"), alpha = 0.7, color = "black", size = 0.1) +
  geom_col(aes(y = total_stock_survey, fill = "Planted + Surveyed"), alpha = 0.7, color = "black", size = 0.1) +
  scale_fill_manual(values = c("Net Planted" = "forestgreen", "Planted + Surveyed" = "seagreen1"), name = "Acres") +
  facet_wrap(~ US_L3NAME, scales = "free", ncol = 2) +
  labs(title = "Postfire Planted and Surveyed Acres by Ignition Year and Ecoregions",
       subtitle = "USFS R5 | Fires 2000-2021 | Net Planting and Stocking Survey 2001-2022",
       x = "Ignition Year",
       y = "Acres") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 9),
        plot.title = element_text(size = 12, face = "bold"),
        axis.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 10),
        strip.background = element_rect(fill = "lightgray"),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(size = 9, hjust = 1)) +
  scale_x_continuous(breaks = seq(2000, 2021, by = 5)) +
  scale_y_continuous(labels = scales::comma_format())

ggsave("top6_monitored_ecoregions_by_year.png", width = 12, height = 8)
ggsave("planted_stock_survey_acres_by_year.png", width = 7, height = 6)



ggplot(monitoring_summary_by_eco, aes(x = avg_mean_diff, y = total_percent_monitored)) +
  geom_point(aes(size = total_planted_acres), alpha = 0.7, color = "forestgreen") +
  geom_text(aes(label = US_L3NAME), check_overlap = TRUE, vjust = -1, size = 3) +
  geom_smooth(method = "lm", color = "blue", se = FALSE, linetype = "dashed") +
  labs(title = "Percentage of Plantations with Stocking Surveys vs. Average Time Since Planting",
       subtitle = "USFS R5 | Fires 2001-2021 | Net Planting and Stocking Survey 2001-2022",
       x = "Average Years Between Planting and Survey",
       y = "Percent of Planted Area Surveyed",
       size = "Total Planted Acres") +
  theme_bw(base_size = 10) +
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 12, face = "bold"),
        axis.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_text(face = "bold")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(0, 100)) +
  scale_size_continuous(labels = scales::comma_format()) +
  expand_limits(x = 0)

ggsave("percent_surveyed_vs_time_since_planting.png", width = 10, height = 7)




library(ggplot2)
library(dplyr)
library(scales)

# Prepare the data
monitoring_by_productivity <- monitoring_in_plantations_eco %>%
  mutate(productivity_group = cut(mean_productivity, 
                                  breaks = seq(0, ceiling(max(mean_productivity)), by = 1),
                                  labels = seq(1, ceiling(max(mean_productivity))),
                                  include.lowest = TRUE)) %>%
  group_by(productivity_group) %>%
  summarize(
    total_planted = sum(net_total_planted, na.rm = TRUE),
    total_monitored = sum(intersecting_acres, na.rm = TRUE),
    percent_monitored = (total_monitored / total_planted) * 100,
    .groups = "drop"
  ) %>%
  filter(!is.na(productivity_group))  # Remove any NA groups

# Create the plot
ggplot(monitoring_by_productivity, aes(x = productivity_group)) +
  geom_col(aes(y = total_planted, fill = "Initial Planting"), 
           color = "black", size = 0.1, alpha = 0.7) +
  geom_col(aes(y = total_monitored, fill = "Survival Survey"), 
           color = "black", size = 0.1) + 
  scale_fill_manual(values = c("Initial Planting" = "forestgreen", "Survival Survey" = "seagreen1")) +
  scale_y_continuous(labels = comma_format()) +
  labs(title = "Postfire Planting & Survival Survey Area by Productivity Class",
       subtitle = "USFS Region 5 | Fires 2000-2021 | Activities 2001-2022",
       x = "Productivity Class",
       y = "Net Acres",
       fill = "Activity Type") +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold", size = 9),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank()
  )
  # guides(fill = guide_legend(reverse = TRUE)) 

ggsave("planted_monitored_acres_by_productivity.png", width = 7, height = 3.5)



library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)

# Prepare the data
treatments_by_productivity <- activities_in_plantations_eco %>%
  mutate(productivity_group = cut(mean_productivity, 
                                  breaks = seq(0, ceiling(max(mean_productivity)), by = 1),
                                  labels = seq(1, ceiling(max(mean_productivity))),
                                  include.lowest = TRUE)) %>%
  filter(type_labels %in% c("Site Prep - Non-Chemical", "Release - Non-Chemical", 
                            "Survival Survey", "Stocking Survey")) %>%
  group_by(type_labels, productivity_group) %>%
  summarize(
    total_planted = sum(net_total_planted, na.rm = TRUE),
    total_treated = sum(intersecting_acres, na.rm = TRUE),
    percent_treated = (total_treated / total_planted) * 100,
    .groups = "drop"
  ) %>%
  filter(!is.na(productivity_group))  # Remove any NA groups

# Set the order of treatment types
treatment_order <- c("Site Prep - Non-Chemical", "Release - Non-Chemical", 
                     "Survival Survey", "Stocking Survey")
treatments_by_productivity$type_labels <- factor(treatments_by_productivity$type_labels, 
                                                 levels = treatment_order)

library(dplyr)
library(ggplot2)
library(scales)
library(viridis)

# Assuming treatments_by_productivity data is already prepared

# Create the plot
treatments_productivity_plot <- ggplot(treatments_by_productivity %>% filter(total_treated > 0), aes(x = productivity_group, fill = type_labels)) +
  geom_col(aes(y = total_treated), 
           color = "black", size = 0.1, position = "dodge") + 
  geom_text(aes(y = total_treated, 
                label = sprintf("%.1f%%", percent_treated),
                vjust = ifelse(total_treated < 2000, -0.5, 1.5)),
            position = position_dodge(width = 0.9),
            size = 3, color = "black", fontface = "bold") +
  scale_fill_viridis_d(option = "plasma", begin = 0.5, end = 0.9) +
  scale_y_continuous(labels = comma_format()) +
  facet_wrap(~ type_labels, scales = "free", ncol = 2) +
  labs(title = "Treatments & Monitoring in Postfire Plantations By Productivity Class",
       subtitle = "USFS Region 5 | Fires 2000-2021 | Activities 2001-2022\n
       % = proportion of net planted area overlapped by activity",
       x = "Productivity Class",
       y = "Net Acres",
       fill = "Treatment Type") +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "none",
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold", size = 9),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 10)
  )

print(treatments_productivity_plot)

# Save the plot
ggsave("planted_treated_acres_by_productivity_faceted.png", 
       treatments_productivity_plot, width = 7, height = 4)



# Create individual plots for each treatment type
individual_plots <- treatment_order %>%
  map(function(treatment) {
    data <- treatments_by_productivity %>% filter(type_labels == treatment)
    
    ggplot(data, aes(x = productivity_group)) +
      geom_col(aes(y = total_planted, fill = "Initial Planting"), 
               color = "black", size = 0.1, alpha = 0.7) +
      geom_col(aes(y = total_treated, fill = treatment), 
               color = "black", size = 0.1) + 
      scale_fill_manual(values = c("Initial Planting" = "forestgreen", 
                                   "Site Prep - Non-Chemical" = "goldenrod",
                                   "Release - Non-Chemical" = "steelblue",
                                   "Survival Survey" = "coral",
                                   "Stocking Survey" = "purple")) +
      scale_y_continuous(labels = comma_format()) +
      labs(title = paste("Postfire Planting &", treatment, "Area"),
           subtitle = "USFS Region 5 | Fires 2000-2021 | Activities 2001-2022",
           x = "Productivity Class",
           y = "Net Acres",
           fill = "Activity Type") +
      theme_bw(base_size = 11) +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 9),
        legend.title = element_text(face = "bold", size = 9),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
        axis.text.y = element_text(size = 9),
        axis.title = element_text(face = "bold", size = 10),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 10),
        panel.grid.minor = element_blank()
      )
  })

# Combine individual plots
combined_plot <- wrap_plots(individual_plots, ncol = 2)

# Save the combined plot
ggsave("planted_treated_acres_by_productivity_individual.png", combined_plot, width = 14, height = 12)

library(dplyr)
library(ggplot2)
library(viridis)


# 3. Heatmap of percentage monitored by ecoregion and time
ggplot(monitoring_by_year_eco, aes(x = Ig_Year, y = US_L3NAME, fill = percent_monitored)) +
  geom_tile() +
  scale_fill_viridis(option = "plasma", name = "Percentage\nMonitored") +
  labs(title = "Percentage of Planted Acres Monitored by Ecoregion and Year",
       subtitle = "USFS Region 5, 2000-2021",
       x = "Ignition Year",
       y = "Ecoregion") +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9)
  ) +
  scale_x_continuous(breaks = seq(2000, 2021, by = 4))

ggsave("percent_monitored_heatmap_by_ecoregion_and_year.png", width = 12, height = 8)




