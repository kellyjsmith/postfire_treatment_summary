library(ggplot2)
library(dplyr)
library(tidyr)




burned_area_severity <- function(data, severity_summary) {
  # Reshape data for stacked area plot
  data_long <- data %>%
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
    ))
  
  data_long$Severity <- factor(
    data_long$Severity,
    levels = c("Unburned to Low", "Low", "Moderate", "High"))
  
  # Prepare high severity percentage data
  high_severity_data <- severity_summary %>%
    filter(Category == "Total Burned") %>%
    select(Ig_Year, High_acres_percent)
  
  # Create the plot
  ggplot() +
    geom_area(data = data_long, aes(x = Ig_Year, y = Acres, fill = Severity), position = "stack") +
    geom_line(data = high_severity_data, aes(x = Ig_Year, y = High_acres_percent * 50000, color = "High Severity %"), 
              size = 1, linetype = "solid") +
    geom_point(data = high_severity_data, aes(x = Ig_Year, y = High_acres_percent * 50000, color = "High Severity %"),
               size = 2) +
    scale_fill_manual(values = c("Unburned to Low" = "darkgreen", 
                                 "Low" = "skyblue", 
                                 "Moderate" = "yellow2", 
                                 "High" = "red"),
                      name = "Burn Severity Acres") +
    scale_color_manual(values = c("High Severity %" = "darkred"),
                       name = NULL) +
    scale_y_continuous(
      name = "Acres Burned",
      labels = function(x) format(x, big.mark = ",", scientific = FALSE),
      limits = c(0, 2500000),
      minor_breaks = waiver(),
      sec.axis = sec_axis(~ . / 5000000, name = "High Severity %", 
                          labels = function(x) paste0(round(x * 100, 1), "%"))
    ) +
    scale_x_continuous(breaks = seq(min(data$Ig_Year), max(data$Ig_Year), by = 2),
                       minor_breaks = waiver()) +
    labs(title = "Total Acres Burned by Severity Class and High Severity Percentage, R5 2000 - 2022",
         x = "Ignition Year") +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10),
      legend.box = "horizontal",
      legend.box.background = element_rect(colour = "black"),
      legend.box.margin = margin(3, 3, 3, 3),
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 11),
      plot.title = element_text(size = 12),
      panel.grid.minor = element_line(color = "gray90", size = 0.2)
    ) +
    guides(
      fill = guide_legend(order = 1, title.position = "left"),
      color = guide_legend(order = 2, title.position = "right")
    )
}

# Generate plot
Burned_Area_Severity_Plot <- burned_area_severity(severity_summary, severity_summary)

print(Burned_Area_Severity_Plot)

# Save plot
ggsave("Burned_Area_Severity_Plot.png", Burned_Area_Severity_Plot, width = 8, height = 5)
