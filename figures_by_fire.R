

## Tables & Figures for summarizing postfire activities

library("sf")
library("dplyr")
library("terra")
library("tidyverse")
library("units")

setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output")

assigned_activities = readRDS("assigned_activities_2024.RDS")
combined_by_fire = readRDS("combined_by_fire.RDS")

# Assuming you have a dataframe 'treatment_data' with columns:
# Incid_Name, Ig_Year, ref_year, net_acres

# Prepare the data
stacked_data <- combined_by_fire %>%
  mutate(years_since_fire = ref_year - Ig_Year) %>%
  mutate(time_period = case_when(
    years_since_fire <= 2 ~ "0-2 years",
    years_since_fire <= 5 ~ "2-5 years",
    years_since_fire <= 10 ~ "5-10 years",
    years_since_fire <= 15 ~ "10-15 years",
    TRUE ~ ">15 years"
  ))

# Create year breaks
year_breaks <- seq(min(stacked_data$Ig_Year), max(stacked_data$Ig_Year), by = 5)
# Create two-row labels
year_labels <- paste(year_breaks[-length(year_breaks)], year_breaks[-1] - 1, sep = "\n")


# Add Ig_Year_group based on the breaks
stacked_data <- stacked_data %>%
  mutate(Ig_Year_group = cut(Ig_Year, 
                             breaks = year_breaks, 
                             labels = year_labels, 
                             include.lowest = TRUE, 
                             right = FALSE))

# Summarize data
stacked_data <- stacked_data %>%
  group_by(Ig_Year_group, type_labels, time_period) %>%
  summarize(total_net_acres = sum(net_acres, na.rm = TRUE)) %>%
  ungroup()

# Order the time periods
stacked_data$time_period <- factor(stacked_data$time_period, 
                                   levels = c("0-2 years", "2-5 years", "5-10 years", "10-15 years", ">15 years"))

# Function to create plot
create_plot <- function(data, title) {
  ggplot(data, aes(x = Ig_Year_group, y = total_net_acres, fill = time_period)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_brewer(palette = "Set3") +
    theme_minimal() +
    labs(title = title,
         subtitle = "Distributed across Time Since Fire",
         x = "Ignition Year Range",
         y = "Net Treated Acres",
         fill = "Time Since Fire") +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
          legend.position = "bottom",
          axis.text.x.bottom = element_text(lineheight = 0.8))
}

# Create the plot for planting activities
planting_plot <- summarized_data %>%
  filter(type_labels == "Plant Trees") %>%
  create_plot("Net Planted Acres by Ignition Year Group")

# Create a faceted plot for multiple treatment types
faceted_plot <- summarized_data %>%
  filter(type_labels %in% c("Plant Trees", "TSI - Release", "Survival Survey", "Stocking Survey")) %>%
  create_plot("Net Treated Acres by Ignition Year Range and Treatment Type") +
  facet_wrap(~ type_labels, scales = "free_y", ncol = 2) +
  theme(strip.background = element_rect(fill = "lightgray"),
        strip.text = element_text(face = "bold"))
