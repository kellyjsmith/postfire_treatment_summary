

## Tables & Figures for summarizing postfire activities

library("sf")
library("dplyr")
library("terra")
library("tidyverse")
library("units")
library(colorspace)

setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output")

# # Create year breaks
# year_breaks <- seq(min(stacked_data$Ig_Year), max(stacked_data$Ig_Year), by = 3)
# # Create two-row labels
# year_labels <- paste(format(year_breaks[-length(year_breaks)]), 
#                      format(year_breaks[-1] - 1), 
#                      sep = " -\n")


assigned_activities = readRDS("assigned_activities_new.RDS")


# Prepare the data
stacked_data <- assigned_activities %>%
  mutate(years_since_fire = diff_years) %>%
  mutate(time_period = case_when(
    years_since_fire <= 1 ~ "0-1",
    years_since_fire <= 2 ~ "1-2",
    years_since_fire <= 3 ~ "2-3",
    years_since_fire <= 5 ~ "3-5",
    years_since_fire <= 8 ~ "5-8",
    TRUE ~ ">8"
  )) %>%
  mutate(type_labels = case_when(
    # type_labels == "TSI - Release" & METHOD == "Chemical" ~ "TSI - Release (Chemical)",
    # type_labels == "TSI - Release" & METHOD != "Chemical" ~ "TSI - Release (Manual/Mechanical)",
    type_labels %in% c("Site Prep - Chemical","Site Prep - Mechanical","Site Prep - Other") ~ "Site Prep - Any",
    TRUE ~ type_labels
  ))



# Summarize data and calculate percentages
summarized_data <- stacked_data %>%
  filter(!is.na(Ig_Year),
         type_labels %in% c("Initial Planting", "Fill-in or Replant", 
                            "Harvest - Salvage", "TSI - Release",
                            "Site Prep - Any", "TSI - Thin")) %>%
  group_by(Ig_Year, type_labels, time_period) %>%
  summarize(gross_acres = sum(as.numeric(st_area(geometry))/4046.86)) %>%
  ungroup() %>%
  st_drop_geometry() %>%
  group_by(Ig_Year, type_labels) %>%
  mutate(percentage = gross_acres / sum(gross_acres) * 100) %>%
  ungroup()

# Order the time periods
summarized_data$time_period <- factor(summarized_data$time_period, 
                                      levels = c("0-1", "1-2", "2-3", "3-5", "5-8", ">8"))

# Factor type_labels
summarized_data$type_labels <- factor(summarized_data$type_labels,
                                      levels = c("Initial Planting", "Fill-in or Replant",
                                                 "Harvest - Salvage", "Site Prep - Any", 
                                                 "TSI - Release", "TSI - Thin"))


#### Gross Acres by Year ####
combined_diffyears_plot <- function(summarized_data) {
  ggplot(summarized_data, aes(x = Ig_Year, y = gross_acres, fill = time_period)) +
    geom_bar(stat = "identity", position = "stack") +
    facet_wrap(~ type_labels, scales = "free", ncol = 3) +
    scale_fill_discrete_sequential(palette = "BluGrn", rev = FALSE) +
    scale_x_continuous(breaks = seq(min(summarized_data$Ig_Year), max(summarized_data$Ig_Year), by = 5)) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Gross Postfire Activities by Ignition Year and Years Since Fire",
         subtitle = "USFS Region 5 Fires, 2000 - 2021",
         x = "Ignition Year",
         y = "Gross Acres Completed",
         fill = "Years Since Fire") +
    theme_bw(base_size = 10) +
    theme(
      axis.text.x = element_text(hjust = 0.5, lineheight = 0.8, size = 9),
      # axis.title.x = element_text(margin = margin(10,0,0,0)),
      strip.text = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 10),
      legend.title = element_text(face = "bold", size = 10),
      plot.title = element_text(face = "bold", size = 12),
      plot.margin = margin(10,10,20,10),
      axis.title = element_text(face = "bold", size = 11),
      axis.text.y = element_text(size = 10),
      legend.position = "bottom"
    ) +
    guides(fill = guide_legend(nrow = 1))
}

# Create and save plot
combined_diffyears <- combined_diffyears_plot(summarized_data)
print(combined_diffyears)

ggsave("combined_time_since_fire.png", combined_diffyears, width = 7, height = 5)



#### Percentages ####


# Calculate overall percentages for each type_label and time_period
overall_percentages <- summarized_data %>%
  group_by(type_labels, time_period) %>%
  summarize(total_gross_acres = sum(gross_acres)) %>%
  group_by(type_labels) %>%
  mutate(total_percentage = total_gross_acres / sum(total_gross_acres) * 100)

# Order the time periods
summarized_data$time_period <- factor(summarized_data$time_period, 
                                      levels = c("0-1", "1-2", "2-3", "3-5", "5-8", ">8"))
overall_percentages$time_period <- factor(overall_percentages$time_period, 
                                          levels = c("0-1", "1-2", "2-3", "3-5", "5-8", ">8"))

# Factor type_labels
type_labels_order <- c("Initial Planting", "Fill-in or Replant",
                       "Harvest - Salvage", "Site Prep - Any", 
                       "TSI - Release", "TSI - Thin")
summarized_data$type_labels <- factor(summarized_data$type_labels, levels = type_labels_order)
overall_percentages$type_labels <- factor(overall_percentages$type_labels, levels = type_labels_order)

# Function to create main plot
combined_diffyears_percentage <- function(summarized_data, overall_percentages) {
  ggplot() +
    geom_col(data = summarized_data, 
             aes(x = Ig_Year, y = percentage, fill = time_period),
             position = "stack", width = 1) +
    geom_col(data = overall_percentages,
             aes(x = max(summarized_data$Ig_Year) + 2, y = total_percentage, fill = time_period),
             position = "stack", width = 2) +
    facet_wrap(~ type_labels, scales = "free", ncol = 2) +
    scale_fill_discrete_sequential(palette = "BluGrn", rev = FALSE) +
    scale_x_continuous(breaks = c(seq(min(summarized_data$Ig_Year), max(summarized_data$Ig_Year), by = 4), 
                                  max(summarized_data$Ig_Year) + 2),
                       labels = c(seq(min(summarized_data$Ig_Year), max(summarized_data$Ig_Year), by = 4), 
                                  "Total")) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    labs(title = "Distribution of Postfire Activity Area by Years Since Fire",
         subtitle = "Percentage of Gross Acres by Ignition Year - USFS Region 5, 2000 - 2021",
         x = "Ignition Year",
         y = "Percentage of Gross Acres",
         fill = "Years Since Fire") +
    theme_bw(base_size = 10) +
    theme(
      axis.text.x = element_text(hjust = 0.5, angle = 0, size = 9),
      strip.text = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      legend.title = element_text(face = "bold", size = 10), 
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 10),
      plot.margin = margin(10, 10, 10, 10),
      axis.title = element_text(face = "bold", size = 11),
      axis.text.y = element_text(size = 9),
      legend.position = "bottom",
      legend.justification = "center"
    ) +
    guides(fill = guide_legend(nrow = 1))
}

# Create summary plot for overall distribution
summary_plot <- ggplot(overall_percentages, aes(x = fct_rev(type_labels), y = total_percentage, fill = fct_rev(time_period))) +
  geom_col(position = "stack") +
  scale_fill_discrete_sequential(palette = "BluGrn", rev = TRUE) +
  labs(title = "Overall Distribution of Years Between Fire & Activity",
       subtitle = "Percentage of Total Postfire Acres, USFS R5 Fires - 2000-2021",
       x = "Activity Type",
       y = "Percentage of Gross Acres",
       fill = "Years Since Fire") +
  theme_bw(base_size = 10) +
  theme(
    axis.text.x = element_text(hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold", size = 10), 
    plot.title = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 10),
    plot.margin = margin(10, 10, 10, 10),
    axis.title = element_text(face = "bold", size = 11),
    axis.text.y = element_text(size = 9),
    legend.position = "bottom",
    legend.justification = "center"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  coord_flip() +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))

print(summary_plot)
ggsave("time_since_fire_distribution_summary.png", summary_plot, width = 7, height = 3)

# Create and save plots
main_plot <- combined_diffyears_percentage(summarized_data, overall_percentages)
print(main_plot)
ggsave("time_since_fire_distribution_main.png", main_plot, width = 12, height = 8)


# Create a table of overall percentages
overall_percentages_table <- overall_percentages %>%
  select(-total_gross_acres) %>%
  pivot_wider(names_from = time_period, values_from = total_percentage) %>%
  mutate(across(where(is.numeric), ~ sprintf("%.1f%%", .)))

print(overall_percentages_table)



#### Boxplots ####
library(ggplot2)
library(dplyr)
library(colorspace)

  
# Prepare the data
boxplot_data <- net_activities %>%
  mutate(type_labels = case_when(
    # type_labels == "TSI - Release" & METHOD == "Chemical" ~ "TSI - Release (Chemical)",
    # type_labels == "TSI - Release" & METHOD != "Chemical" ~ "TSI - Release (Manual/Mechanical)",
    type_labels %in% c("Site Prep - Chemical","Site Prep - Mechanical","Site Prep - Other") ~ "Site Prep - Any",
    TRUE ~ type_labels)) %>%
  filter(type_labels %in% c("Initial Planting", "Fill-in or Replant", "Site Prep - Any", "TSI - Release")) %>%
  mutate(type_labels = factor(type_labels, levels = c("Initial Planting", "Fill-in or Replant", "Site Prep - Any", "TSI - Release")))

# Create the box plot function
create_boxplot <- function(data) {
  ggplot(data, aes(x = factor(Ig_Year), y = diff_years, fill = type_labels)) +
    geom_boxplot(outlier.size = 0.5) +
    facet_wrap(~ type_labels, scales = "free_y", ncol = 2) +
    # scale_fill_discrete_sequential(palette = "BluGrn", rev = FALSE) +
    scale_x_discrete(breaks = seq(min(data$Ig_Year), max(data$Ig_Year), by = 5)) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Distribution of Years Between Fire and Treatment",
         subtitle = "USFS Region 5 Fires, 2000 - 2021",
         x = "Ignition Year",
         y = "Years Since Fire",
         fill = "Treatment Type") +
    theme_bw(base_size = 10) +
    theme(
      axis.text.x = element_text(hjust = 0.5, lineheight = 0.8, size = 9),
      strip.text = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 10),
      legend.title = element_text(face = "bold", size = 10),
      plot.title = element_text(face = "bold", size = 12),
      plot.margin = margin(10,10,20,10),
      axis.title = element_text(face = "bold", size = 11),
      axis.text.y = element_text(size = 10),
      legend.position = "none"
    )
}

# Create and save plot
boxplot_diffyears <- create_boxplot(boxplot_data)
print(boxplot_diffyears)
ggsave("boxplot_time_since_fire.png", boxplot_diffyears, width = 7, height = 5)



# Data preparation
veg_treatments_diff <- veg_summary %>%
  filter(Category == "All Activities",
         type_labels %in% c("Initial Planting", "Harvest - Salvage", "Fill-in or Replant", 
                            "Site Prep - Chemical", "Site Prep - Mechanical", "Site Prep - Other",
                            "TSI - Release", "TSI - Thin")) %>%
  mutate(year_group = case_when(
    median_diff <= 1 ~ "0-1",
    median_diff <= 2 ~ "1-2",
    median_diff <= 3 ~ "2-3",
    median_diff <= 5 ~ "3-5",
    median_diff <= 8 ~ "5-8",
    TRUE ~ ">8"
  )) %>%
  mutate(type_labels = case_when(
    type_labels %in% c("Site Prep - Chemical", "Site Prep - Mechanical", "Site Prep - Other") ~ "Site Prep - Any",
    TRUE ~ type_labels
  )) %>%
  select(type_labels, year_group, Conifer_acres, Shrubland_acres, Hardwood_acres, Other_acres, Total_acres) %>%
  pivot_longer(cols = c(Conifer_acres, Shrubland_acres, Hardwood_acres, Other_acres),
               names_to = "Veg_Type",
               values_to = "Acres") %>%
  mutate(Veg_Type = gsub("_acres", "", Veg_Type))

# Create a complete grid of all combinations
all_combinations <- expand.grid(
  type_labels = unique(veg_treatments_diff$type_labels),
  year_group = c("0-1", "1-2", "2-3", "3-5", "5-8", ">8"),
  Veg_Type = c("Conifer", "Shrubland", "Hardwood", "Other")
)

# Join the complete grid with the existing data
veg_treatments_complete <- all_combinations %>%
  left_join(veg_treatments_diff, by = c("type_labels", "year_group", "Veg_Type")) %>%
  mutate(Acres = coalesce(Acres, 0))  # Replace NA with 0

# Set factor levels for type_labels and Veg_Type
veg_treatments_complete$type_labels <- factor(veg_treatments_complete$type_labels, 
                                              levels = c("Harvest - Salvage", "Site Prep - Any", "Initial Planting", 
                                                         "Fill-in or Replant", "TSI - Release", "TSI - Thin"))
veg_treatments_complete$Veg_Type <- factor(veg_treatments_complete$Veg_Type, 
                                           levels = c("Conifer", "Shrubland", "Hardwood", "Other"))
veg_treatments_complete$year_group <- factor(veg_treatments_complete$year_group, 
                                             levels = c("0-1", "1-2", "2-3", "3-5", "5-8", ">8"))

# Create a plot comparing vegetation cover by time since fire for each treatment type
veg_treatments_diffyears <- ggplot(veg_treatments_complete, aes(x = year_group, y = Acres, fill = Veg_Type)) +
  geom_col(position = "stack", width = 0.9) +
  facet_wrap(~ type_labels, scales = "free", ncol = 3) +
  scale_fill_manual(values = c("Conifer" = "springgreen4", 
                               "Shrubland" = "goldenrod", 
                               "Hardwood" = "purple2",
                               "Other" = "gray70")) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Distribution of Current Vegetation Type by Years Since Fire",
       subtitle = "Net Postfire Activity Acres - USFS Region 5, 2000 - 2021",
       x = "Years Since Fire",
       y = "Acres",
       fill = "Vegetation Type") +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
  )

# Display the plot
print(veg_treatments_diffyears)

# Save the plot
ggsave("veg_treatments_diffyears.png", veg_treatments_diffyears, width = 7, height = 5)




# Create a plot showing the distribution of treated acres by time since fire for each treatment type
acres_distribution_plot <- ggplot(veg_treatments_diff, aes(x = year_group, y = Total_acres, fill = type_labels)) +
  geom_col(position = "dodge", width = 0.8, color = "black", size = 0.2) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Distribution of Treated Acres by Time Since Fire",
       x = "Time Since Fire",
       y = "Total Acres",
       fill = "Treatment Type") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )

# Combine plots
combined_plot <- veg_treatments_comparison / acres_distribution_plot +
  plot_layout(heights = c(2, 1)) +
  plot_annotation(
    title = "Vegetation Cover and Treatment Distribution by Time Since Fire",
    subtitle = "Comparing current vegetation cover based on the time between fire and treatment",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )
  )

# Save plot
ggsave("veg_cover_by_treatment_timing.png", combined_plot, width = 14, height = 16)



