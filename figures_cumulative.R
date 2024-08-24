
library(tidyverse)
library(sf)
library(gridExtra)
library(viridis)
library(scales)
library(ggnewscale)
library(colorspace)
library(grid)

setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output")

combined_cumulative = readRDS("combined_cumulative.RDS")
severity_by_year = readRDS("severity_by_year.RDS")
burned_area_by_year = readRDS("burned_area_by_year.RDS")

combined_cumulative <- combined_cumulative %>%
  filter(end_year < 2022)
# severity_by_year<- severity_by_year %>%
#   filter(Ig_Year < 2022)
severity_summary<- severity_summary %>%
  filter(Ig_Year < 2022)
# burned_area_by_year<- burned_area_by_year %>%
#   filter(Ig_Year < 2022)


#### Cumulative Planting + Burn Severity ####

cumulative_planted_severity_plot <- function(combined_cumulative, severity_summary, max_plant_axis = 5e5, max_burned_axis = 1.5e7) {
  # Prepare cumulative burned acres data by severity
  cumulative_burned_severity <- severity_summary %>%
    st_drop_geometry() %>%
    filter(Category == "Total Burned") %>%
    select(Ig_Year, Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres) %>%
    pivot_longer(cols = -c(Ig_Year, Category),
                 names_to = "Severity",
                 values_to = "Acres") %>%
    mutate(Severity = case_when(
      Severity == "Unburned_to_Low_acres" ~ "Unburned to Low",
      Severity == "Low_acres" ~ "Low",
      Severity == "Moderate_acres" ~ "Moderate",
      Severity == "High_acres" ~ "High"
    )) %>%
    group_by(Ig_Year, Severity) %>%
    summarise(Acres = sum(Acres)) %>%
    ungroup() %>%
    group_by(Severity) %>%
    arrange(Ig_Year) %>%
    mutate(Cumulative_Acres = cumsum(Acres)) %>%
    ungroup() %>%
    mutate(Type = "Burned Acres by Severity")
  
  # Prepare cumulative planting data
  planting_data <- combined_cumulative %>% 
    filter(type_labels == "Initial Planting") %>%
    select(end_year, gross_acres, net_acres) %>%
    pivot_longer(cols = c(gross_acres, net_acres),
                 names_to = "Metric",
                 values_to = "Acres") %>%
    mutate(Metric = ifelse(Metric == "gross_acres", "Gross Acres to Date", "Net Acres to Date"),
           Type = "Postfire Planting Acres")
  
  # Combine datasets
  combined_data <- bind_rows(
    cumulative_burned_severity %>% rename(Year = Ig_Year),
    planting_data %>% rename(Year = end_year)
  )
  
  # Set factor levels for Severity and Metric
  severity_levels <- c("Unburned to Low", "Low", "Moderate", "High")
  combined_data$Severity <- factor(combined_data$Severity, levels = severity_levels)
  combined_data$Type <- factor(combined_data$Type, levels = c("Postfire Planting Acres", "Burned Acres by Severity"))

  
  min_year <- min(combined_data$Year)
  max_year <- max(combined_data$Year)
  
  # Create the plot
  ggplot() +
    # Lines for cumulative planting data
    geom_line(data = subset(combined_data, Type == "Postfire Planting Acres"), 
              aes(x = Year, y = Acres, color = Metric), 
              alpha = 1, size = 1.25) +
    # Stacked area for cumulative burned acres by severity
    geom_area(data = subset(combined_data, Type == "Burned Acres by Severity"), 
              aes(x = Year, y = Cumulative_Acres, fill = Severity), 
              alpha = 0.7) +
    # Color scales
    scale_fill_manual(values = c("Unburned to Low" = "darkgreen", 
                                 "Low" = "skyblue", 
                                 "Moderate" = "yellow2", 
                                 "High" = "red"),
                      name = "Severity Classes") +
    scale_color_manual(values = c("Gross Acres to Date" = "darkblue", 
                                  "Net Acres to Date" = "royalblue"),
                       name = "Planted Area") +
    # Facet the plots
    facet_wrap(~ Type, nrow = 2, scales = "free") +
    # Labels and theme
    labs(title = "Cumulative Area Burned by Severity & Planted Postfire",
         subtitle = "USFS Region 5, 2000 - 2021",
         x = "Year", y = "Acres") +
    theme_bw(base_size = 10) +
    theme(
      strip.text = element_text(face = "bold", size = 11),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.margin = margin(),
      legend.location = "plot",
      legend.title = element_text(face = "bold", size = 11),
      legend.text = element_text(size = 10),
      # axis.text.x.top = element_text(size = 10, hjust = 1),
      axis.text = element_text(size = 11),
      axis.title = element_text(face = "bold", size = 11),
      plot.title = element_text(face = "bold", size = 12),
      plot.title.position = "plot",
      plot.subtitle = element_text(size = 11),
      panel.spacing = unit(0.5, "lines")
    ) +
    scale_x_continuous(breaks = seq(min_year, max_year, by = 2),
                       limits = c(min_year, max_year)) +
    scale_y_continuous(labels = scales::comma_format()) +
    guides(fill = guide_legend(order = 2, nrow = 1),
           color = guide_legend(order = 1)) 
}

cumulative_planted_severity <- cumulative_planted_severity_plot(combined_cumulative, severity_summary)
ggsave("cumulative_planted_severity.png", cumulative_planted_severity, width = 7, height = 6)


#### Cumulative Treatments ####

cumulative_treatments_acres <- function(combined_cumulative) {
  ggplot() +
    theme_bw(base_size = 10) +
    geom_line(data = combined_cumulative %>% filter(type_labels %in% c(
      "Harvest - Salvage", "Site Prep - Mechanical", "Site Prep - Burn", "Fill-in or Replant", 
      "TSI - Release")), 
              aes(x = end_year, y = gross_acres, color = "Gross Acres to Date"), alpha = 0.9, size = 1.25) +
    geom_line(data = combined_cumulative %>% filter(type_labels %in% c("Harvest - Salvage", "Site Prep - Mechanical","Fill-in or Replant", "TSI - Release")), 
              aes(x = end_year, y = net_acres, color = "Net Acres to Date"), alpha = 0.9, size = 1.25) +
    # ggtitle("Cumulative Postfire Treatment Activity Area in R5, 2000 - 2022") +
    labs(title = "Cumulative Postfire Treatment Area by Activity Type",
         subtitle = "USFS Region 5, 2000 - 2021",
         x = "Year", y = "Completed Acres") +
    scale_x_continuous(breaks = seq(2000, 2022, 4)) +
    scale_y_continuous(labels = scales::comma) +
    facet_wrap(~ type_labels, ncol = 2, scales = "free") +
    scale_color_manual(values = c("Gross Acres to Date" = "darkblue", "Net Acres to Date" = "royalblue"),
                       name = "Treatment Area") +
    theme(legend.position="bottom", 
          legend.box = "vertical", 
          legend.text = element_text(size=10),
          legend.title = element_text(face = "bold", size=11),
          plot.title = element_text(face = "bold", size=12),
          plot.title.position = "plot",
          plot.margin = margin(t=10,r=20,b=10,l=10),
          axis.title = element_text(face="bold", size=11),
          axis.text = element_text(size=10),
          # strip.background = element_blank(),
          strip.text = element_text(face="bold", size=11)) +
    guides(fill=guide_legend(title=NULL, nrow=1, vjust = -1))
}


# Generate cumulative treatments acres plot
Cumulative_Treatments <- cumulative_treatments_acres(combined_cumulative)

# Save cumulative treatments acres plot
ggsave("Cumulative_Treatments.png", Cumulative_Treatments, width = 7, height = 5)

# 
# 
# index_burned_planted <- function(combined_cumulative, burned_area_by_year) {
#   # Process planted area data
#   planted_data <- combined_cumulative %>%
#     filter(type_labels == "Initial Planting") %>%
#     rename(year = end_year) %>%
#     select(year, net_acres) %>%
#     rename(planted_acres = net_acres)
#   
#   # Process burned area data
#   burned_data <- burned_area_by_year %>%
#     rename(year = Ig_Year) %>%
#     select(year, cumulative_acres_burned)
#   
#   # Combine the data
#   combined_data <- full_join(planted_data, burned_data, by = "year") %>%
#     arrange(year) %>%
#     filter(!is.na(planted_acres) & !is.na(cumulative_acres_burned))
#   
#   # Calculate indexed growth (set first year to 100)
#   first_year <- min(combined_data$year, na.rm = TRUE)
#   combined_data <- combined_data %>%
#     mutate(
#       indexed_planted = planted_acres / planted_acres[year == first_year],
#       indexed_burned = cumulative_acres_burned / cumulative_acres_burned[year == first_year]
#     )
#   
#   # Reshape data from wide to long format
#   data_long <- combined_data %>%
#     select(year, indexed_planted, indexed_burned) %>%
#     pivot_longer(cols = c(indexed_planted, indexed_burned), 
#                  names_to = "category", 
#                  values_to = "value")
#   
#   # Create the plot
#   ggplot(data_long, aes(x = year, y = value, color = category)) +
#     geom_line(size = 1.2) +
#     geom_point(size = 3) +
#     scale_y_continuous(
#       labels = scales::percent_format(scale = 1),
#       name = "Indexed Growth (%)",
#       expand = expansion(mult = c(0.05, 0.1))
#     ) +
#     scale_x_continuous(breaks = seq(min(combined_data$year), max(combined_data$year), by = 2)) +
#     scale_color_manual(values = c("indexed_planted" = "#82ca9d", "indexed_burned" = "#8884d8"),
#                        labels = c("Cumulative Planted Area", "Cumulative Burned Area")) +
#     labs(
#       title = "Indexed Growth: Cumulative Planted vs Burned Areas",
#       subtitle = paste(first_year, "-", max(combined_data$year)),
#       x = "Year",
#       color = "Category"
#     ) +
#     theme_bw() +
#     theme(
#       plot.title = element_text(hjust = 0.5, face = "bold"),
#       plot.subtitle = element_text(hjust = 0.5),
#       legend.position = "bottom",
#       panel.grid.minor = element_blank()
#     )
# }
# 
# # Usage
# cumulative_planted_burned_indexed <- index_burned_planted(combined_cumulative, burned_area_by_year)
# ggsave("indexed_growth_plot_cumulative_data.png", plot = cumulative_planted_burned_indexed, width = 10, height = 6, dpi = 300)
# 
# 
# 
# fig = {
#   jpeg(filename = "combined_cumulative_treat.jpg",
#        width = 700, height = 500,
#        quality = 100,
#        bg = "white",
#        symbolfamily="default")
#   p = ggplot() +
#     theme_bw() +
#     geom_area(data = combined_cumulative_treat, aes(x = end, y = gross_acres, fill = "Gross Acres to Date"), alpha = 0.9) +
#     geom_area(data = combined_cumulative_treat, aes(x = end, y = net_acres, fill = "Net Acres to Date"), alpha = 0.9) +
#     geom_line(data = combined_cumulative_5years_treat, 
#               aes(x = end, y = gross_acres, color = "Gross Acres <= 5 Years Postfire"), linewidth = 1.25) +
#     geom_line(data = combined_cumulative_5years_treat, 
#               aes(x = end, y = net_acres, color = "Net Acres <= 5 Years Postfire"), linewidth = 1.25) +
#     ggtitle("Cumulative Postfire Reforestation Activities in R5, 2002 - 2023 (Treatments)") +
#     labs(x = "Year", y = "Activity Acres") +
#     scale_x_continuous(breaks = seq(2000, 2024, 5)) +
#     scale_y_continuous(labels = scales::comma) +
#     facet_wrap(~ type_labels, ncol = 3, scales = "free_y") +
#     scale_fill_manual(values = c("Gross Acres to Date" = "gray50", "Net Acres to Date" = "gray80")) +
#     scale_color_manual(values = c("Gross Acres <= 5 Years Postfire" = "blue2", "Net Acres <= 5 Years Postfire" = "orangered2")) +
#     theme(legend.position="bottom", 
#           legend.box = "horizontal", 
#           legend.text = element_text(size=12),
#           plot.title = element_text(size=15), 
#           plot.margin = margin(t=10,r=20,b=10,l=10),
#           axis.title = element_text(face="bold", size=13),
#           axis.text.x = element_text(size=12),
#           axis.text.y = element_text(size=12),
#           strip.text = element_text(face="bold", size=13)) +
#     guides(fill=guide_legend(title=NULL, nrow=2), color=guide_legend(title=NULL, nrow=2))
#   print(p)
#   dev.off()
# }
# 
# 
# fig = {
#   jpeg(filename = "combined_cumulative_monitor.jpg",
#        width = 700, height = 500,
#        quality = 100, bg = "white")
#   p = ggplot() +
#     theme_bw() +
#     geom_area(data = combined_cumulative_monitor, aes(x = end, y = gross_acres, fill = "Gross Acres to Date"), alpha = 0.9) +
#     geom_area(data = combined_cumulative_monitor, aes(x = end, y = net_acres, fill = "Net Acres to Date"), alpha = 0.9) +
#     geom_line(data = combined_cumulative_5years_monitor, 
#               aes(x = end, y = gross_acres, color = "Gross Acres <= 5 Years Postfire"), linewidth = 1.25) +
#     geom_line(data = combined_cumulative_5years_monitor, 
#               aes(x = end, y = net_acres, color = "Net Acres <= 5 Years Postfire"), linewidth = 1.25) +
#     ggtitle("Cumulative Postfire Reforestation Activities in R5, 2002 - 2023 (Monitoring)") +
#     labs(x = "Year", y = "Activity Acres") +
#     scale_x_continuous(breaks = seq(2000, 2024, 5)) +
#     scale_y_continuous(labels = scales::comma) +
#     facet_wrap(~ type_labels, ncol = 3, scales = "free_y") +
#     scale_fill_manual(values = c("Gross Acres to Date" = "gray50", "Net Acres to Date" = "gray80")) +
#     scale_color_manual(values = c("Gross Acres <= 5 Years Postfire" = "blue2", "Net Acres <= 5 Years Postfire" = "orangered2")) +
#     theme(legend.position="bottom", 
#           legend.box = "horizontal", 
#           legend.text = element_text(size=12),
#           plot.title = element_text(size=15), 
#           plot.margin = margin(t=10,r=20,b=10,l=10),
#           axis.title = element_text(face="bold", size=13),
#           axis.text.x = element_text(size=12),
#           axis.text.y = element_text(size=12),
#           strip.text = element_text(face="bold", size=13)) +
#     guides(fill=guide_legend(title=NULL, nrow=2), color=guide_legend(title=NULL, nrow=2))
#   print(p)
#   dev.off()
# }
# 
# 
