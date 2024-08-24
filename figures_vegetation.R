setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output")


library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(patchwork)

veg_data = veg_summary %>%
  filter(Ig_Year < 2022)

planted_burned_veg_facets <- function(veg_data, max_plant_axis = 35000, max_burned_axis = 2.6e6) {
  # Prepare planted data
  planted_data <- veg_data %>%
    filter(Category == "All Activities", type_labels == "Initial Planting") %>%
    select(Ig_Year, Conifer_acres, Shrubland_acres, Hardwood_acres, Other_acres) %>%
    pivot_longer(cols = c(Conifer_acres, Shrubland_acres, Hardwood_acres, Other_acres),
                 names_to = "Veg_Type",
                 values_to = "Acres") %>%
    mutate(Veg_Type = gsub("_acres", "", Veg_Type)) %>%
    group_by(Ig_Year, Veg_Type) %>%
    summarise(Acres = sum(Acres, na.rm = TRUE), .groups = "drop") %>%
    mutate(Type = "Net Postfire Planted Acres")
  
  # Prepare burned area data
  burned_data <- veg_data %>%
    filter(Category == "Total Burned") %>%
    select(Ig_Year, Conifer_acres, Shrubland_acres, Hardwood_acres, Other_acres) %>%
    pivot_longer(cols = c(Conifer_acres, Shrubland_acres, Hardwood_acres, Other_acres),
                 names_to = "Veg_Type",
                 values_to = "Acres") %>%
    mutate(Veg_Type = gsub("_acres", "", Veg_Type)) %>%
    group_by(Ig_Year, Veg_Type) %>%
    summarise(Acres = sum(Acres, na.rm = TRUE), .groups = "drop") %>%
    mutate(Type = "Burned Acres")
  
  # Combine datasets
  combined_data <- bind_rows(planted_data, burned_data)
  
  # Set factor levels for Veg_Type
  veg_levels <- c("Conifer", "Shrubland", "Hardwood", "Other")
  combined_data$Veg_Type <- factor(combined_data$Veg_Type, levels = veg_levels)
  combined_data$Type <- factor(combined_data$Type, levels = c("Net Postfire Planted Acres", "Burned Acres"))
  
  # Color palette
  veg_colors <- c("Conifer" = "springgreen4", 
                  "Shrubland" = "goldenrod", 
                  "Hardwood" = "purple2",
                  "Other" = "gray70")
  
  # Create the acres plot
  acres_plot <- ggplot(combined_data, aes(x = Ig_Year, y = Acres, fill = Veg_Type)) +
    geom_col(position = "stack", color = "black", size = 0.2) +
    scale_fill_manual(values = veg_colors, name = "Vegetation Type") +
    facet_wrap(~ Type, nrow = 2, scales = "free") +
    labs(title = "Area by Vegetation Type",
         x = NULL, y = NULL) +
    theme_bw(base_size = 10) +
    theme(
      strip.text = element_text(face = "bold", size = 10),
      legend.position = "none",
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
      axis.text.y = element_text(size = 8),
      axis.title = element_blank(),
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      panel.spacing = unit(1, "lines")
    ) +
    scale_x_continuous(breaks = seq(2000, 2021, by = 4)) +
    scale_y_continuous(labels = function(x) scales::comma(abs(x)))
  
  # Calculate percentages
  combined_data_pct <- combined_data %>%
    group_by(Type, Ig_Year) %>%
    mutate(Percentage = Acres / sum(Acres) * 100) %>%
    ungroup()
  
  # Create the percentages plot
  percentages_plot <- ggplot(combined_data_pct, aes(x = Ig_Year, y = Percentage, fill = Veg_Type)) +
    geom_col(position = "stack", width = 1, color = "black", size = 0.1) +
    scale_fill_manual(values = veg_colors, name = "Vegetation Type") +
    facet_wrap(~ Type, nrow = 2, scales = "free") +
    labs(title = "% by Vegetation Type",
         x = NULL, y = NULL) +
    theme_bw(base_size = 10) +
    theme(
      strip.text = element_text(face = "bold", size = 10),
      legend.position = "none",
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
      axis.text.y = element_text(size = 9),
      axis.title = element_blank(),
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      panel.spacing = unit(0.5, "lines")
    ) +
    scale_x_continuous(breaks = seq(2000, 2021, by = 4)) +
    scale_y_continuous(labels = function(x) paste0(round(x), "%"))
  
  legend <- get_legend(
    acres_plot + 
      theme(legend.position = "bottom",
            legend.box = "vertical",
            legend.margin = margin(),
            legend.title = element_text(face = "bold", size = 10),
            legend.text = element_text(size = 9)) +
      guides(fill = guide_legend(nrow = 1))
  )
  
  # Combine plots using patchwork
  combined_plot <- (acres_plot + percentages_plot +
                      plot_layout(widths = c(1.5, 1))) /
    legend +
    plot_layout(heights = c(20,1)) +
    plot_annotation(
      title = "Vegetation Type of Net Planted and Total Burned Acres by Ignition Year",
      subtitle = "USFS Region 5 Fires - 2000-2021, Existing Vegetation Type - 2023",
      theme = theme(
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(size = 11),
      )
    )
  
  return(combined_plot)
}

# Generate plot
planted_veg_faceted <- planted_burned_veg_facets(
  veg_data,
  max_plant_axis = 35000,
  max_burned_axis = 2.6e6
)

print(planted_veg_faceted)

# Save plot
ggsave("planted_veg_faceted.png", planted_veg_faceted, width = 7, height = 5)


library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

select_treatments_by_veg_type <- function(data, max_treat_axis = 10000) {
  # Prepare treated data
  treated_data <- data %>%
    st_drop_geometry() %>%
    filter(type_labels %in% c("Fill-in or Replant", "Harvest - Salvage", 
                              "Site Prep - Mechanical", "TSI - Release")) %>%
    select(Ig_Year, type_labels, Conifer_acres, Shrubland_acres, Hardwood_acres, Other_acres) %>%
    pivot_longer(cols = c(Conifer_acres, Shrubland_acres, Hardwood_acres, Other_acres),
                 names_to = "Veg_Type",
                 values_to = "Net_Acres") %>%
    mutate(Veg_Type = gsub("_acres", "", Veg_Type)) %>%
    group_by(Ig_Year, type_labels, Veg_Type) %>%
    summarise(Net_Acres = sum(Net_Acres, na.rm = TRUE), .groups = "drop")
  
  treated_data$Veg_Type <- factor(treated_data$Veg_Type,
                                  levels = c("Conifer", "Shrubland", "Hardwood", "Other"))
  
  treated_data$type_labels <- factor(treated_data$type_labels,
                                     levels = c("Harvest - Salvage", "Site Prep - Mechanical",
                                                "Fill-in or Replant", "TSI - Release"))
  
  # Split data into pre-planting and post-planting
  pre_planting <- treated_data %>% 
    filter(type_labels %in% c("Harvest - Salvage", "Site Prep - Mechanical"))
  post_planting <- treated_data %>% 
    filter(type_labels %in% c("Fill-in or Replant", "TSI - Release"))
  
  # Color palette
  veg_colors <- c("Conifer" = "springgreen4", 
                  "Shrubland" = "goldenrod", 
                  "Hardwood" = "purple2",
                  "Other" = "gray70")
  
  # Create plot function
  create_plot <- function(data, title, legend_position) {
    ggplot(data, aes(x = Ig_Year, y = Net_Acres, fill = Veg_Type)) +
      geom_col(position = "stack", color = "black", size = 0.1) +
      facet_wrap(~ type_labels, nrow = 1, scales = "free") +
      scale_fill_manual(values = veg_colors, name = "Vegetation Type") +
      scale_y_continuous(
        name = "Net Acres",
        labels = function(x) format(x, big.mark = ",", scientific = FALSE)
      ) +
      labs(title = title,
           x = "Ignition Year") +
      theme_bw(base_size = 11) +
      theme(
        legend.position = legend_position,
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 9),
        axis.text = element_text(hjust = 0.5, size = 9),
        axis.title.y = element_text(face = "bold", size = 11),
        axis.title.x = element_blank(),
        plot.title = element_text(face = "bold", size = 11),
        strip.text = element_text(size = 10, face = "bold")
      ) +
      scale_x_continuous(breaks = seq(min(data$Ig_Year), max(data$Ig_Year), by = 4))
  }
  
  # Create individual plots
  pre_plot <- create_plot(pre_planting, "Pre-planting Treatments", "none")
  post_plot <- create_plot(post_planting, "Post-planting Treatments", "bottom")
  
  # Combine plots using patchwork
  combined_plot <- pre_plot / post_plot +
    plot_layout(heights = c(1, 1.2)) +
    plot_annotation(
      title = "Distribution of Postfire Treatment Area by Vegetation Type",
      subtitle = "USFS Region 5 Fires - 2000-2021, Net Acres by Ignition Year",
      theme = theme(plot.title = element_text(face = "bold", size = 11),
                    plot.subtitle = element_text(size = 11))
    )
  
  return(combined_plot)
}

# Generate combined plot
Combined_Treatments_Veg_Type <- select_treatments_by_veg_type(veg_data, max_treat_axis = 10000)

print(Combined_Treatments_Veg_Type)

# Save combined plot
ggsave("Combined_Treatments_Veg_Type.png", Combined_Treatments_Veg_Type, width = 7, height = 6)


