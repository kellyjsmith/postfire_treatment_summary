
library(tidyverse)
library(sf)
library(gridExtra)
library(viridis)
library(scales)
library(ggnewscale)
library(colorspace)
library(grid)

setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output")

all_activities = readRDS("all_activities.RDS")
net_activities = readRDS("net_activities.RDS")


combined_cumulative = readRDS("combined_cumulative.RDS")
severity_by_year = readRDS("severity_by_year.RDS")
severity_summary = readRDS("severity_summary.RDS")
burned_area_by_year = readRDS("burned_area_by_year.RDS")


gross_by_fire_activity_year <- assigned_activities %>%
  st_collection_extract() %>%
  mutate(fire_id = paste(Incid_Name, Event_ID, sep="_")) %>%
  group_by(type_labels, year) %>%
  summarize(geometry = sum(st_area(geometry)), .groups = "drop") %>%
  mutate(gross_acres = as.numeric(st_area(geometry)) / 4046.86) %>%  # Convert m^2 to acres
  arrange(type_labels, year) %>%
  group_by(type_labels) %>%
  mutate(cumulative_gross_acres = cumsum(gross_acres)) %>%
  ungroup()


#### Cumulative Planting + Burn Severity ####

cumulative_planted_severity_plot <- function(cumulative_data, fire_data, max_plant_axis = 5e5, max_burned_axis = 1.5e7) {
  # Prepare cumulative burned acres data by severity
  cumulative_burned_severity <- fire_data %>%
    st_drop_geometry() %>%
    filter(Category == "Total Burned") %>%
    select(Ig_Year, Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres, Increased_Greenness_acres) %>%
    pivot_longer(cols = -c(Ig_Year, Category),
                 names_to = "Severity",
                 values_to = "Acres") %>%
    mutate(Severity = case_when(
      Severity == "Unburned_to_Low_acres" ~ "Unburned to Low",
      Severity == "Low_acres" ~ "Low",
      Severity == "Moderate_acres" ~ "Moderate",
      Severity == "High_acres" ~ "High",
      Severity == "Increased_Greenness_acres" ~ "Increased Greenness"
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
  planting_data <- cumulative_data %>% 
    filter(type_labels == "Initial Planting") %>%
    select(year, type_labels, cumulative_gross_acres) %>%
    rename(Acres = cumulative_gross_acres) %>%
    mutate(Type = "Postfire Planting Acres")
  
  # Combine datasets
  combined_data <- bind_rows(
    cumulative_burned_severity %>% rename(Year = Ig_Year),
    planting_data %>% rename(Year = year)
  )
  
  # Set factor levels for Severity
  severity_levels <- c("Increased Greenness", "Unburned to Low", "Low", "Moderate", "High")
  combined_data$Severity <- factor(combined_data$Severity, levels = severity_levels)
  combined_data$Type <- factor(combined_data$Type, levels = c("Postfire Planting Acres", "Burned Acres by Severity"))
  
  min_year <- min(combined_data$Year)
  max_year <- max(combined_data$Year)
  
  # Calculate max cumulative values
  max_planted_gross <- max(planting_data$Acres)
  
  max_burned_by_severity <- cumulative_burned_severity %>%
    filter(Ig_Year == max(Ig_Year)) %>%
    group_by(Severity) %>%
    summarize(max_burned = sum(Cumulative_Acres)) %>%
    mutate(label = paste0(Severity, ": ", scales::comma(max_burned)))
  
  total_burned <- sum(max_burned_by_severity$max_burned)
  
  # Create the plot
  ggplot() +
    # Line for cumulative planting data
    geom_line(data = subset(combined_data, Type == "Postfire Planting Acres"), 
              aes(x = Year, y = Acres), 
              color = "darkblue", alpha = 1, size = 1.25) +
    geom_point(data = subset(combined_data, Type == "Postfire Planting Acres"), 
              aes(x = Year, y = Acres), 
              fill = "royalblue", alpha = 1, size = 3, shape = 21) +
    # Stacked area for cumulative burned acres by severity
    geom_area(data = subset(combined_data, Type == "Burned Acres by Severity"), 
              aes(x = Year, y = Cumulative_Acres, fill = Severity), 
              alpha = 0.7) +
    # Color scales
    scale_fill_manual(values = c("Increased Greenness" = "limegreen",
                                 "Unburned to Low" = "darkgreen", 
                                 "Low" = "skyblue", 
                                 "Moderate" = "yellow2", 
                                 "High" = "red"),
                      name = "Severity Classes") +
    # Facet the plots
    facet_wrap(~ Type, nrow = 2, scales = "free_y") +
    # Labels and theme
    labs(title = "Cumulative Area Planted Postfire & Burned by Severity",
         subtitle = "USFS R5 | Fires, 2000 - 2021 | Planting, 2001 - 2022",
         x = "Year", y = "Acres") +
    theme_bw(base_size = 10) +
    theme(
      strip.text = element_text(face = "bold", size = 10),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.margin = margin(r = 50),
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 9),
      axis.text = element_text(size = 9),
      axis.title = element_text(face = "bold", size = 11),
      plot.title = element_text(face = "bold", size = 12),
      plot.title.position = "plot",
      plot.subtitle = element_text(size = 11),
      panel.spacing = unit(0.5, "lines")
    ) +
    scale_x_continuous(breaks = seq(min_year, max_year, by = 4),
                       limits = c(min_year, max_year)) +
    scale_y_continuous(labels = scales::comma_format()) +
    guides(fill = guide_legend(nrow = 1, reverse = TRUE))
}

cumulative_planted_severity <- cumulative_planted_severity_plot(gross_by_fire_activity_year, severity_summary)
print(cumulative_planted_severity)

ggsave("cumulative_planted_severity.png", cumulative_planted_severity, width = 7, height = 4)



#### Cumulative % Planted ####

# Calculate cumulative burned acres
cumulative_burned_total <- severity_summary %>%
  st_drop_geometry() %>%
  filter(Category == "Total Burned") %>%
  ungroup() %>%
  select(Ig_Year, Total_acres) %>%
  arrange(Ig_Year) %>%
  mutate(cumulative_burned_acres = cumsum(Total_acres))

# Calculate cumulative planted acres
planting_data <- combined_cumulative %>% 
  filter(type_labels == "Initial Planting") %>%
  select(end_year, net_acres)
  

# Calculate percentage of planted to burned
percentage_data <-  planting_data %>%
  left_join(cumulative_burned_total, by = c("end_year" = "Ig_Year")) %>%
  mutate(Percentage = (net_acres / cumulative_burned_acres) * 100)

# Create the percentage plot
percentage_plot <- ggplot(percentage_data, aes(x = end_year, y = Percentage)) +
  geom_line(color = "darkgreen", size = 1.25) +
  geom_point(color = "darkgreen", size = 3) +
  labs(title = "Cumulative Percentage of Burned Area Planted",
       subtitle = "USFS Region 5, 2000 - 2021",
       x = "Year", 
       y = "Percentage") +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 11),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(size = 11),
        panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = seq(min(percentage_data$end_year), 
                                  max(percentage_data$end_year), 
                                  by = 2)) +
  scale_y_continuous(limits = c(0, 2.2),
                     labels = scales::percent_format(scale = 1, accuracy = 0.1),
                     breaks = seq(0, 2.5, by = 0.5))

# Display the plot
print(percentage_plot)

# Save the plot
ggsave("cumulative_percentage_planted.png", percentage_plot, width = 7, height = 2, dpi = 300)


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



