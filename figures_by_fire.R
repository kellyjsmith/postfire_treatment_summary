

## Tables & Figures for summarizing postfire activities

library("sf")
library("dplyr")
library("terra")
library("tidyverse")
library("units")

setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output")

# # Create year breaks
# year_breaks <- seq(min(stacked_data$Ig_Year), max(stacked_data$Ig_Year), by = 3)
# # Create two-row labels
# year_labels <- paste(format(year_breaks[-length(year_breaks)]), 
#                      format(year_breaks[-1] - 1), 
#                      sep = " -\n")


assigned_activities = readRDS("assigned_activities_02_22.RDS")

# Prepare the data
stacked_data <- assigned_activities %>%
  mutate(years_since_fire = diff_years) %>%
  mutate(time_period = case_when(
    years_since_fire <= 1 ~ "0-1 years",
    years_since_fire <= 2 ~ "1-2 years",
    years_since_fire <= 5 ~ "2-5 years",
    years_since_fire <= 10 ~ "5-10 years",
    TRUE ~ ">10 years"
  )) %>%
  mutate(type_labels = case_when(
    type_labels == "TSI - Release" & METHOD == "Chemical" ~ "TSI - Release (Chemical)",
    type_labels == "TSI - Release" & METHOD != "Chemical" ~ "TSI - Release (Manual/Mechanical)",
    TRUE ~ type_labels
  ))

# Add Ig_Year_group based on the breaks
stacked_data <- stacked_data %>%
  mutate(Ig_Year_group = cut(Ig_Year, 
                             breaks = year_breaks, 
                             labels = year_labels, 
                             include.lowest = TRUE, 
                             right = FALSE))

# Summarize data
summarized_data <- stacked_data %>%
  filter(!is.na(Ig_Year),
         type_labels %in% c("Initial Planting", "Fill-in or Replant", 
                            "TSI - Release (Chemical)", "TSI - Release (Manual/Mechanical)", 
                            "Stocking Survey", "Survival Survey")) %>%
  group_by(Ig_Year, type_labels, time_period) %>%
  summarize(total_acres = sum(as.numeric(activity_fire_area)/4046.86, na.rm = TRUE)) %>%
  ungroup()

# Order the time periods
summarized_data$time_period <- factor(summarized_data$time_period, 
                                      levels = c("0-1 years", "1-2 years", "2-5 years", "5-10 years", ">10 years"))

# Factor type_labels
summarized_data$type_labels = factor(summarized_data$type_labels,
                                     levels = c("Initial Planting", "Fill-in or Replant", 
                                                "TSI - Release (Chemical)", "TSI - Release (Manual/Mechanical)", 
                                                "Stocking Survey", "Survival Survey"))

# Function to create plot
create_plot <- function(data, title) {
  ggplot(data, aes(x = Ig_Year, y = total_acres, fill = as.factor(time_period))) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_discrete_sequential(palette = "BluGrn", rev = FALSE) +
    theme_bw() +
    labs(title = title,
         x = "Ignition Year",
         y = "Gross Activity Acres",
         fill = "Time Since Fire") +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
          axis.text.x.bottom = element_text(lineheight = 0.8),
          axis.title.x = element_text(margin = margin(10,0,0,0)),
          legend.position = "bottom", 
    )
}

# Create plots
fig = {
  jpeg(filename = "acres_diffyears_by_ignition.jpg",
       width = 700, height = 550,
       quality = 100,
       bg = "white",
       symbolfamily="default")
  
  # Treatments plot
  treatments_plot <- summarized_data %>%
    filter(type_labels %in% c("Initial Planting", "Fill-in or Replant", 
                              "TSI - Release (Chemical)", "TSI - Release (Manual/Mechanical)")) %>%
    create_plot("Reforestation and Release") +
    facet_wrap(~ type_labels, scales = c("free"), ncol = 2) +
    scale_x_continuous(breaks = seq(min(summarized_data$Ig_Year), max(summarized_data$Ig_Year), by = 3)) +
    scale_y_continuous(labels = scales::comma) +
    theme(strip.background = element_rect(fill = "gray80"),
          strip.text = element_text(size=13),
          legend.text = element_text(size=12), 
          plot.title = element_text(face = "bold", size=13),
          plot.margin = margin(10,10,20,10),
          axis.title.x = element_blank(),
          # axis.title.y = element_text(face="bold",size=13),
          axis.title.y = element_blank(),
          axis.text = element_text(size=12)) +
    guides(fill = "none")
  
  # Monitoring plot
  monitoring_plot <- summarized_data %>%
    filter(type_labels %in% c("Stocking Survey", "Survival Survey")) %>%
    create_plot("Monitoring") +
    facet_wrap(~ type_labels, scales = "free_y", ncol = 2) +
    scale_x_continuous(breaks = seq(min(summarized_data$Ig_Year), max(summarized_data$Ig_Year), by = 3)) +
    scale_y_continuous(labels = scales::comma) +
    theme(strip.background = element_rect(fill = "gray80"),
          strip.text = element_text(size=13),
          legend.text = element_text(size=12), 
          plot.title = element_text(face="bold", size=13),
          plot.margin = margin(0,10,0,10),
          axis.title.x = element_text(face="bold",size=14), 
          axis.title.y = element_blank(),
          axis.text = element_text(size=12))
  
  # Combine plots
  grid.arrange(treatments_plot, monitoring_plot, ncol = 1, heights = c(1.5, 1), 
               top = textGrob("Postfire Activity Acres by Ignition Year and Time Since Fire, R5 2000 - 2021", 
                              gp=gpar(fontsize=16)),
               left = textGrob("Gross Activity Acres", rot = 90,
                              gp=gpar(fontface="bold", fontsize=14)))
  dev.off()
}


