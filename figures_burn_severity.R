library(ggplot2)
library(dplyr)
library(tidyr)

burned_area_severity <- function(data, burned_area_by_year) {
  # Reshape data for stacked area plot
  data_long <- data %>%
    st_drop_geometry() %>%
    dplyr::select(Ig_Year, Unburned_to_Low_acres, Low_acres, Moderate_acres, High_acres, 
                  Increased_Greenness_acres, Non_Mapping_Area_acres) %>%
    pivot_longer(cols = -Ig_Year,
                 names_to = "Severity",
                 values_to = "Acres") %>%
    mutate(Severity = case_when(
      Severity == "Unburned_to_Low_acres" ~ "Unburned to Low",
      Severity == "Low_acres" ~ "Low",
      Severity == "Moderate_acres" ~ "Moderate",
      Severity == "High_acres" ~ "High",
      Severity == "Increased_Greenness_acres" ~ "Increased Greenness",
      Severity == "Non_Mapping_Area_acres" ~ "Non-Mapping Area"
    ))
  
  data_long$Severity = factor(
    data_long$Severity,
    levels = c("Unburned to Low", "Low", "Moderate", "High", "Increased Greenness", "Non-Mapping Area"))
  
  ggplot() +
    geom_area(data = data_long, aes(x = Ig_Year, y = Acres, fill = Severity), position = "stack") +
    geom_line(data = burned_area_by_year, aes(x = Ig_Year, y = acres_burned, color = "Total MTBS Acres"), 
              linetype = "dashed", size = 1.5) +
    scale_fill_manual(values = c("Unburned to Low" = "darkgreen", 
                                 "Low" = "skyblue", 
                                 "Moderate" = "yellow2", 
                                 "High" = "red",
                                 "Increased Greenness" = "lightgreen",
                                 "Non-Mapping Area" = "gray"),
                      name = "Burn Severity Acres") +
    scale_color_manual(values = c("Total MTBS Acres" = "red3"),
                       name = NULL) +
    scale_y_continuous(
      name = "Acres Burned",
      labels = function(x) format(x, big.mark = ",", scientific = FALSE),
      minor_breaks = waiver()) +
    scale_x_continuous(breaks = seq(min(data$Ig_Year), max(data$Ig_Year), by = 2),
                       minor_breaks = waiver()) +
    labs(title = "Total Acres Burned and Acres by Severity Class, R5 2000 - 2022",
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
Burned_Area_Severity_Plot <- burned_area_severity(severity_by_year, burned_area_by_year)

# Save plot
ggsave("Burned_Area_Severity_Plot.png", Burned_Area_Severity_Plot, width = 8, height = 6)


