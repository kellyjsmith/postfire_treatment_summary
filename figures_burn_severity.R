library(ggplot2)
library(dplyr)
library(tidyr)

burned_area_severity <- function(data) {
  # Reshape data for stacked area plot
  data_long <- data %>%
    dplyr::select(Ig_Year, Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres) %>%
    pivot_longer(cols = -Ig_Year,
                 names_to = "Severity",
                 values_to = "Acres") %>%
    mutate(Severity = case_when(
      Severity == "Unburned_to_Low_acres" ~ "Unburned to Low",
      Severity == "Low_acres" ~ "Low",
      Severity == "Moderate_acres" ~ "Moderate",
      Severity == "High_acres" ~ "High",
    ))
  
  # Calculate total burned area
  data_total <- data %>%
    mutate(Total_Burned = Total_burned_acres)
  
  # # Calculate the scaling factor
  # scale_factor <- max(data_total$Total_Burned, na.rm = TRUE) / max(data_long$Acres, na.rm = TRUE)
  
  data_long$Severity = factor(
    data_long$Severity,
    levels = c("Unburned to Low", "Low", "Moderate", "High"))
  
  ggplot() +
    geom_area(data = data_long, aes(x = Ig_Year, y = Acres, fill = Severity), position = "stack") +
    geom_line(data = data_total, aes(x = Ig_Year, y = Total_Burned), 
              color = "red3", linetype = "dashed", size = 1.5) +
    scale_fill_manual(values = c("Unburned to Low" = "darkgreen", 
                                 "Low" = "skyblue", 
                                 "Moderate" = "yellow2", 
                                 "High" = "red"),
                      name = "Burn Severity") +
    scale_y_continuous(
      name = "Acres by Severity",
      labels = function(x) format(x, big.mark = ",", scientific = FALSE),
      # sec.axis = sec_axis(~.*scale_factor, name = "Total Burned Acres",
      #                     labels = function(x) format(x, big.mark = ",", scientific = FALSE))
    ) +
    labs(title = "Total Acres Burned and Acres by Severity Class, R5 2000 - 2022",
         x = "Ignition Year") +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_text(size = 11),
          legend.text = element_text(size = 10),
          axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 11),
          plot.title = element_text(size = 12)) +
    scale_x_continuous(breaks = seq(min(data$Ig_Year), max(data$Ig_Year), by = 2))
}

# Generate plot
Burned_Area_Severity_Plot <- burned_area_severity(severity_year_summaries)

# Save plot
ggsave("Burned_Area_Severity_Plot.png", Burned_Area_Severity_Plot, width = 8, height = 6)

