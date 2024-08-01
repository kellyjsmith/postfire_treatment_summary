

## Tables & Figures for summarizing postfire activities

library("sf")
library("dplyr")
library("terra")
library("tidyverse")
library("units")

setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output")

assigned_activities = readRDS("assigned_activities_02_22.RDS")
combined_by_fire = readRDS("combined_by_fire.RDS")


# Assuming you have a dataframe 'treatment_data' with columns:
# Incid_Name, Ig_Year, ref_year, net_acres

# Prepare the data
stacked_data <- assigned_activities %>%
  mutate(years_since_fire = diff_years) %>%
  mutate(time_period = case_when(
    years_since_fire <= 1 ~ "0-1 years",
    years_since_fire <= 2 ~ "1-2 years",
    years_since_fire <= 5 ~ "2-5 years",
    years_since_fire <= 10 ~ "5-10 years",
    TRUE ~ ">10 years"
  ))

# Create year breaks
year_breaks <- seq(min(stacked_data$Ig_Year), max(stacked_data$Ig_Year), by = 3)
# Create two-row labels
year_labels <- paste(format(year_breaks[-length(year_breaks)]), 
                     format(year_breaks[-1] - 1), 
                     sep = " -\n")


# Add Ig_Year_group based on the breaks
stacked_data <- stacked_data %>%
  mutate(Ig_Year_group = cut(Ig_Year, 
                             breaks = year_breaks, 
                             labels = year_labels, 
                             include.lowest = TRUE, 
                             right = FALSE))

# Summarize data
summarized_data <- stacked_data %>%
  group_by(Ig_Year_group, type_labels, time_period) %>%
  summarize(total_acres = sum(as.numeric(activity_fire_area)/4046.86, na.rm = TRUE)) %>%
  ungroup()

# Order the time periods
summarized_data$time_period <- factor(summarized_data$time_period, 
                                   levels = c("0-1 years", "1-2 years", "2-5 years", "5-10 years", ">10 years"))

# Factor type_labels
summarized_data$type_labels = factor(summarized_data$type_labels,
                                     levels = c("Plant Trees", "Fill-in or Replant Trees", "TSI - Release", "Stocking Survey"))

# Function to create plot
create_plot <- function(data, title) {
  ggplot(data, aes(x = Ig_Year_group, y = total_acres, fill = as.factor(time_period))) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_brewer(palette = "PuBuGn", direction = -1) +
    theme_bw() +
    labs(title = title,
         x = "Ignition Year Range",
         y = "Total Activity Acres",
         fill = "Time Since Fire") +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
          legend.position = "bottom",
          axis.text.x.bottom = element_text(lineheight = 0.8),
          axis.title.x = element_text(margin = margin(10,0,0,0)))
}

# # Create the plot for planting activities
# planting_plot <- summarized_data %>%
#   filter(type_labels == "Plant Trees") %>%
#   create_plot("Net Planted Acres by Ignition Year Group")

# Create plot
fig = {
  jpeg(filename = "net_ignition_select_diffyears.jpg",
       width = 800, height = 600,
       quality = 100,
       bg = "white",
       symbolfamily="default")
  faceted_plot <- summarized_data %>%
    filter(!is.na(Ig_Year_group),type_labels %in% c("Plant Trees", "Fill-in or Replant Trees", "TSI - Release", "Stocking Survey")) %>%
    create_plot("Net Postfire Reforestation Acres Completed in R5 by Ignition Year Range") +
    facet_wrap(~ type_labels, scales = "free_y", ncol = 2) +
    scale_y_continuous(labels = scales::comma) +
    theme(strip.background = element_rect(fill = "lightgray"),
          strip.text = element_text(face = "bold", size=14),
          plot.background = element_rect(color = "#EEEEEE"),
          legend.text = element_text(size=13), 
          plot.title = element_text(size=18),
          plot.margin = margin(10,10,20,10),
          axis.title = element_text(face="bold",size=14), 
          axis.text = element_text(size=12))
  print(faceted_plot)
  dev.off()
}


