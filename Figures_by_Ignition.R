#

library(tidyverse)
library(sf)
library(gridExtra)
library(viridis)
library(scales)
library(ggnewscale)
library(colorspace)
library(grid)

setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output")

combined_ignition_year = readRDS("combined_ignition_year.RDS")
combined_ignition_year_treat = readRDS("combined_ignition_year_treat.RDS")
combined_ignition_year_monitor = readRDS("combined_ignition_year_monitor.RDS")
combined_ignition_select = combined_ignition_year %>%
  filter(type_labels %in% c("Initial Planting", "TSI - Release", "Survival Survey", "Stocking Survey"))


#### Treated + Burned by Ignition Year ####

# Assuming 'activity_proportions' is your data frame

activity_proportions_by_fire = activity_proportions %>% st_drop_geometry()
activity_proportions_by_fire = rename(activity_proportions_by_fire, replant_acres_net = `fill-in_or_replant_acres_net`)

# 
# # Bar chart for net acres planted, certified, replanted, and released
# bar_chart <- function(treat_data) {
#   data_long <- treat_data %>%
#     select(Ig_Year, planted_acres_net, 
#            replant_acres_net, tsi_release_acres_net) %>%
#     pivot_longer(cols = -Ig_Year, 
#                  names_to = "Activity", 
#                  values_to = "Net_Acres") %>%
#     mutate(Activity = case_when(
#       Activity == "planted_acres_net" ~ "Planted",
#       Activity == "replant_acres_net" ~ "Replanted",
#       Activity == "tsi_release_acres_net" ~ "Released"
#     )) %>%
#     group_by(Ig_Year, Activity) %>%
#     summarise(Net_Acres = sum(Net_Acres, na.rm = TRUE), .groups = "drop")
#   
#   ggplot(data_long, aes(x = Ig_Year, y = Net_Acres, fill = Activity)) +
#     geom_col(position = "dodge") +
#     scale_fill_brewer(palette = "Set2") +
#     labs(title = "Net Acres by Activity and Ignition Year",
#          x = "Ignition Year", 
#          y = "Net Acres") +
#     theme_bw(base_size = 14) +
#     theme(legend.position = "bottom",
#           legend.title = element_text(size = 14),
#           legend.text = element_text(size = 12),
#           axis.text.x = element_text(angle = 0, hjust = 0, size = 12),
#           axis.text.y = element_text(size = 12),
#           axis.title = element_text(size = 16),
#           plot.title = element_text(size = 16)) +
#     scale_x_continuous(breaks = seq(min(treat_data$Ig_Year), max(treat_data$Ig_Year), by = 2)) +
#     scale_y_continuous(labels = comma)
# }
# 
# # Line graph for acres burned
# line_graph_burned <- function(burn_data) {
#   data_summarized <- burn_data %>%
#     group_by(Ig_Year) %>%
#     summarise(acres_burned_r5 = sum(acres_burned, na.rm = TRUE))
#   
#   ggplot() +
#     geom_line(data = data_summarized, aes(x = Ig_Year, y = acres_burned_r5), color = "red3", size = 2) +
#     labs(title = "Acres Burned by Ignition Year",
#          x = "Ignition Year", 
#          y = "Acres Burned") +
#     theme_bw(base_size = 14) +
#     theme(legend.position = "none",
#           axis.text.x = element_text(angle = 0, hjust = 0, size = 12),
#           axis.text.y = element_text(size = 12),
#           axis.title = element_text(size = 16),
#           plot.title = element_text(size = 16)) +
#     scale_x_continuous(breaks = seq(min(data_summarized$Ig_Year), max(data_summarized$Ig_Year), by = 2)) +
#     labels = function(x) format(x, big.mark = ",", scientific = FALSE)
# }
# 
# # Heatmap for acres burned with more variable scale
# heatmap_burned_varied_scale <- function(burn_data) {
#   data_summarized <- burn_data %>%
#     group_by(Ig_Year) %>%
#     summarise(acres_burned_r5 = sum(acres_burned, na.rm = TRUE))
#   
#   # Calculate quartiles for custom breaks
#   quant <- quantile(data_summarized$acres_burned_r5, probs = seq(0, 1, 0.1), na.rm = TRUE)
#   
#   ggplot(data_summarized, aes(x = Ig_Year, y = 1, fill = acres_burned_r5)) +
#     geom_tile() +
#     scale_fill_gradient2(colours = heat_hcl(10),
#                          name = "Acres Burned", 
#                          labels = comma,
#                          breaks = quant,
#                          limits = c(min(data_summarized$acres_burned_r5), max(data_summarized$acres_burned_r5))) +
#     labs(title = "Acres Burned by Ignition Year",
#          x = "Ignition Year", 
#          y = NULL) +
#     theme_bw(base_size = 14) +
#     theme(axis.text.y = element_blank(),
#           axis.ticks.y = element_blank(),
#           legend.position = "bottom",
#           legend.key.width = unit(4, "cm"),
#           legend.title = element_text(size = 14),
#           legend.text = element_text(size = 12),
#           axis.text.x = element_text(angle = 0, hjust = 0, size = 12),
#           axis.title = element_text(size = 16),
#           plot.title = element_text(size = 16)) +
#     scale_x_continuous(breaks = seq(min(data_summarized$Ig_Year), max(data_summarized$Ig_Year), by = 2))
# }
# 
# # Combined plot with bar chart and line graph
# combined_line <- function(treat_data, burn_data) {
#   bar_plot <- bar_chart(treat_data) + 
#     theme(plot.margin = margin(b = 10,
#                                r = 20))
#   
#   line_plot <- line_graph_burned(burn_data) +
#     theme(legend.position = "bottom",
#           plot.margin = margin(t = 0,
#                                r = 20,
#                                l = 0),
#           axis.title.x = element_blank(),
#           plot.title = element_blank())
#   
#   
#   combined_line <- gridExtra::grid.arrange(bar_plot, line_plot, 
#                                       ncol = 1, 
#                                       heights = c(4, 2))
#   
#   # Add overall title
#   title <- textGrob("Postfire Reforestation Activities and Acres Burned by Ignition Year",
#                     gp = gpar(fontsize = 20, fontface = "bold"))
#   
#   gridExtra::grid.arrange(title, combined_line, ncol = 1, heights = c(1, 20))
# }
# 
# # Combined plot with bar chart and heatmap
# combined_heatmap <- function(treat_data, burn_data) {
#   bar_plot <- bar_chart(treat_data) + 
#     theme(plot.margin = margin(b = 10,
#                                r = 20))
#   
#   heatmap <- heatmap_burned_varied_scale(burn_data) +
#     theme(legend.position = "bottom",
#           plot.margin = margin(t = 0,
#                                r = 20,
#                                l = 60),
#           axis.title.x = element_blank(),
#           plot.title = element_blank())
#   
#   combined_heatmap <- gridExtra::grid.arrange(bar_plot, heatmap, 
#                                               ncol = 1, 
#                                               heights = c(4, 2))
#   
#   # Add overall title
#   title <- textGrob("Postfire Reforestation Activities and Acres Burned by Ignition Year",
#                     gp = gpar(fontsize = 20, fontface = "bold"))
#   
#   gridExtra::grid.arrange(title, combined_heatmap, ncol = 1, heights = c(1, 20))
# }
# 
# # Generate plots
# bar_plot <- bar_chart(activity_proportions_by_fire)
# line_plot <- line_graph_burned(burned_area_by_year)
# heatmap_varied_scale <- heatmap_burned_varied_scale(burned_area_by_year)
# combined_line_output <- combined_line(activity_proportions_by_fire, burned_area_by_year)
# combined_heatmap_output <- combined_heatmap(activity_proportions_by_fire, burned_area_by_year)
# 
# # Save plots
# ggsave("bar_chart.png", bar_plot, width = 12, height = 8)
# ggsave("line_graph.png", line_plot, width = 12, height = 4)
# ggsave("heatmap_varied_scale.png", heatmap_varied_scale, width = 12, height = 2)
# ggsave("Treated_Burned_line.png", combined_line_output, width = 12, height = 10)
# ggsave("Treated_Burned_heatmap.png", combined_heatmap_output, width = 12, height = 10)


treated_burned_heatmap <- function(treat_data, burn_data) {
  # Process treatment data
  data_long <- treat_data %>%
    select(Ig_Year, planted_acres_net,  
           replant_acres_net, tsi_release_acres_net) %>%
    pivot_longer(cols = -Ig_Year, 
                 names_to = "Activity", 
                 values_to = "Net_Acres") %>%
    mutate(Activity = case_when(
      Activity == "planted_acres_net" ~ "Planted",
      Activity == "replant_acres_net" ~ "Replanted",
      Activity == "tsi_release_acres_net" ~ "Released"
    )) %>%
    group_by(Ig_Year, Activity) %>%
    summarise(Net_Acres = sum(Net_Acres, na.rm = TRUE), .groups = "drop")
  
  # Process burn data
  data_summarized <- burn_data %>%
    group_by(Ig_Year) %>%
    summarise(acres_burned_r5 = sum(acres_burned, na.rm = TRUE))
  
  # Calculate quartiles for custom breaks
  quant <- quantile(data_summarized$acres_burned_r5, probs = seq(0, 1, 0.1), na.rm = TRUE)
  
  # Create the plot
  ggplot() +
    # Add burn data as a heatmap using alpha instead of fill
    geom_tile(data = data_summarized, aes(x = Ig_Year, y = 0, alpha = acres_burned_r5, height = Inf), fill = "red3") +
    scale_alpha_continuous(name = "Acres Burned", 
                           labels = comma(pretty(quant)),
                           breaks = breaks_pretty(n = 10),
                           range = c(0.2, 0.9)) +
    # Add treatment data as bars
    geom_col(data = data_long, aes(x = Ig_Year, y = Net_Acres, fill = Activity), position = "dodge") +
    # Color scales
    scale_fill_manual(values = c("Planted" = "forestgreen", "Replanted" = "black", "Released" = "royalblue"),
                      name = "Activity") +
    # Labels and theme
    labs(title = "R5 Postfire Reforestation and Acres Burned by Ignition Year",
         x = "Ignition Year", 
         y = "Net Activity Acres") +
    theme_bw(base_size = 12) +
    guides(fill = guide_legend(position = "bottom", vjust = 1.5),
           alpha = guide_legend(position = "right", hjust = -1)) +
    theme(legend.box = "vertical",
          legend.key.width = unit(1, "cm"),
          legend.title = element_text(size = 11),
          legend.text = element_text(size = 10),
          # axis.ticks.x = element_blank(),
          axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 11),
          plot.title = element_text(size = 13)) +
    scale_x_continuous(breaks = seq(min(treat_data$Ig_Year), max(treat_data$Ig_Year), by = 2)) +
    scale_y_continuous(labels = comma)
}

# Generate plot
combined_heatmap_output <- treated_burned_heatmap(activity_proportions_by_fire, burned_area_by_year)

# Save plot
ggsave("Treated_Burned_heatmap.png", combined_heatmap_output, width = 7, height = 5)



treated_burned_bubble <- function(treat_data, burn_data) {
  # Process treatment data
  data_long <- treat_data %>%
    select(Ig_Year, planted_acres_net, replant_acres_net, tsi_release_acres_net) %>%
    pivot_longer(cols = -Ig_Year,
                 names_to = "Activity",
                 values_to = "Net_Acres") %>%
    mutate(Activity = case_when(
      Activity == "planted_acres_net" ~ "Planted",
      Activity == "replant_acres_net" ~ "Replanted",
      Activity == "tsi_release_acres_net" ~ "Released"
    )) %>%
    group_by(Ig_Year, Activity) %>%
    summarise(Net_Acres = sum(Net_Acres, na.rm = TRUE), .groups = "drop")
  
  # Process burn data
  data_summarized <- burn_data %>%
    group_by(Ig_Year) %>%
    summarise(acres_burned_r5 = sum(acres_burned, na.rm = TRUE))
  
  # Create the plot
  ggplot() +
    # Add treatment data as bars
    geom_col(data = data_long, aes(x = Ig_Year, y = Net_Acres, fill = Activity), position = "dodge") +
    # Add burn data as bubbles
    geom_point(data = data_summarized, aes(x = Ig_Year, y = 20000, size = acres_burned_r5), 
               color = "red", alpha = 0.6) +
    # Add labels for acres burned in millions
    geom_text(data = data_summarized, 
              aes(x = Ig_Year, y = 25000,
                  label = sprintf("%.1fM", acres_burned_r5 / 1e6)),
              vjust = 0, size = 3) +
    # Color scales
    scale_fill_manual(values = c("Planted" = "forestgreen", "Replanted" = "black", "Released" = "royalblue"),
                      name = "Activity") +
    # Bubble size scale
    scale_size_continuous(name = "Acres Burned", 
                          range = c(6, 32),
                          labels = comma,
                          breaks = seq(0, max(data_summarized$acres_burned_r5), by = 500000)) +
    # Labels and theme
    labs(title = "R5 Postfire Reforestation and Acres Burned by Ignition Year",
         x = "Ignition Year",
         y = "Net Activity Acres") +
    theme_bw(base_size = 12) +
    guides(fill = guide_legend(position = "bottom"),
           size = guide_legend(position = "right")) +
    theme(legend.box = "vertical",
          legend.key.width = unit(1, "cm"),
          legend.title = element_text(size = 11),
          legend.text = element_text(size = 10),
          axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 11),
          plot.title = element_text(size = 13)) +
    scale_x_continuous(breaks = seq(min(treat_data$Ig_Year), max(treat_data$Ig_Year), by = 2)) +
    scale_y_continuous(labels = comma)
}

# Generate plot
combined_bubble_output <- treated_burned_bubble(activity_proportions_by_fire, burned_area_by_year)

# Save plot
ggsave("Treated_Burned_bubble.png", combined_bubble_output, width = 10, height = 7)




# Bar chart for net acres planted, certified, replanted, and released
treated_burned_dual <- function(treat_data, burn_data) {
  data_long <- treat_data %>%
    select(Ig_Year, planted_acres_net, certification_plant_acres_net,
           replant_acres_net, tsi_release_acres_net) %>%
    pivot_longer(cols = -Ig_Year,
                 names_to = "Activity",
                 values_to = "Net_Acres") %>%
    mutate(Activity = case_when(
      Activity == "planted_acres_net" ~ "Planted",
      Activity == "certification_plant_acres_net" ~ "Certified",
      Activity == "replant_acres_net" ~ "Replanted",
      Activity == "tsi_release_acres_net" ~ "Released"
    )) %>%
    group_by(Ig_Year, Activity) %>%
    summarise(Net_Acres = sum(Net_Acres, na.rm = TRUE), .groups = "drop")

  # Summarize acres burned by year
  data_summarized <- burn_data %>%
    group_by(Ig_Year) %>%
    summarise(acres_burned_r5 = sum(acres_burned, na.rm = TRUE))

  # Calculate the scaling factor
  scale_factor <- max(data_long$Net_Acres, na.rm = TRUE) / max(data_summarized$acres_burned_r5, na.rm = TRUE)

  ggplot() +
    geom_line(data = data_summarized, aes(x = Ig_Year, y = acres_burned_r5 * scale_factor), 
              alpha = 0.9, color = "red3", size = 2) +
    geom_col(data = data_long, aes(x = Ig_Year, y = Net_Acres, fill = Activity), position = "dodge") +
    scale_fill_manual(values = c("Planted" = "forestgreen", "Replanted" = "black", "Released" = "royalblue"),
                      name = "Activity") +
    scale_y_continuous(
      name = "Net Acres",
      labels = function(x) format(x, big.mark = ",", scientific = FALSE),
      sec.axis = sec_axis(~./scale_factor, name = "Acres Burned",
                          labels = function(x) format(x, big.mark = ",", scientific = FALSE))
    ) +
    labs(title = "Net Acres by Activity and Ignition Year",
         x = "Ignition Year") +
    theme_bw(base_size = 12) +
    theme(legend.position = "bottom",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 11),
          axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 13),
          plot.title = element_text(size = 14)) +
    scale_x_continuous(breaks = seq(min(treat_data$Ig_Year), max(treat_data$Ig_Year), by = 2))
}

# Generate plot
Treated_Burned_dualaxis <- treated_burned_dual(activity_proportions_by_fire, burned_area_by_year)

# Save plot
ggsave("Treated_Burned_dualaxis.png", Treated_Burned_dualaxis, width = 10, height = 7)




chart_usfs_nfs_r5burn <- function(budget_data, burn_data) {
  # Merge the data
  merged_data <- budget_data %>%
    select(Year, NFS_Total) %>%
    left_join(burn_data %>% select(Ig_Year, acres_burned), by = c("Year" = "Ig_Year"))
  
  # Calculate the scaling factor for the secondary axis
  scale_factor <- max(merged_data$NFS_Total, na.rm = TRUE) / max(merged_data$acres_burned, na.rm = TRUE)
  
  # Create the plot
  ggplot(merged_data, aes(x = Year)) +
    # NFS Total budget columns
    geom_col(aes(y = NFS_Total), fill = "forestgreen", alpha = 0.7) +
    # Region 5 acres burned line
    geom_line(aes(y = acres_burned * scale_factor), color = "red", size = 1.5) +
    # Scales and labels
    scale_y_continuous(
      name = "NFS Total Budget ($ Billions)",
      labels = scales::dollar_format(scale = 1, prefix = "$", suffix = "B"),
      sec.axis = sec_axis(~./scale_factor, 
                          name = "Region 5 Acres Burned",
                          labels = scales::comma_format())
    ) +
    scale_x_continuous(breaks = seq(min(merged_data$Year), max(merged_data$Year), by = 2)) +
    labs(title = "USFS NFS Total Budget vs. Region 5 Acres Burned (2002-2023)",
         x = "Year") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title.y.left = element_text(color = "forestgreen"),
      axis.title.y.right = element_text(color = "red")
    )
}

# Generate the plot
usfs_nfs_r5burn_plot <- chart_usfs_nfs_r5burn(usfs_budgets_burn, burned_area_by_year)

# Display the plot
print(usfs_nfs_r5burn_plot)

# Save the plot (optional)
ggsave("usfs_nfs_r5burn_plot.png", usfs_nfs_r5burn_plot, width = 12, height = 8, dpi = 300)

# Save the plot (optional)
# ggsave("usfs_budget_burn_plot.png", usfs_budget_burn_plot, width = 12, height = 8, dpi = 300)

# # Horizontal bar chart for net acres planted, certified, replanted, and released
# horizontal_bar_chart <- function(data) {
#   data_long <- data %>%
#     select(Ig_Year, planted_acres_net, certification_plant_acres_net, 
#            replant_acres_net, tsi_release_acres_net) %>%
#     pivot_longer(cols = -Ig_Year, 
#                  names_to = "Activity", 
#                  values_to = "Net_Acres") %>%
#     mutate(Activity = case_when(
#       Activity == "planted_acres_net" ~ "Planted",
#       Activity == "certification_plant_acres_net" ~ "Certified",
#       Activity == "replant_acres_net" ~ "Replanted",
#       Activity == "tsi_release_acres_net" ~ "Released"
#     ))
#   
#   ggplot(data_long, aes(x = Net_Acres, y = Ig_Year, fill = Activity)) +
#     geom_bar(stat = "identity", position = "dodge") +
#     scale_fill_brewer(palette = "Set2") +
#     labs(title = "Net Acres by Activity and Ignition Year",
#          x = "Net Acres", 
#          y = "Ignition Year") +
#     theme_bw() +
#     theme(legend.position = "bottom",
#           axis.text.y = element_text(angle = 90, hjust = 1)) +
#     scale_y_continuous(breaks = seq(min(data$Ig_Year), max(data$Ig_Year), by = 2)) +
#     scale_x_continuous(labels = comma)
# }
# 
# # Vertical heatmap for acres burned
# vertical_heatmap_burned <- function(data) {
#   ggplot(data, aes(x = 1, y = Ig_Year, fill = acres_burned_r5)) +
#     geom_tile() +
#     scale_fill_viridis(option = "plasma", name = "Acres Burned", labels = comma) +
#     labs(title = "Acres Burned",
#          x = NULL, 
#          y = NULL) +
#     theme_bw() +
#     theme(axis.text.x = element_blank(),
#           axis.ticks.x = element_blank(),
#           legend.position = "bottom") +
#     scale_y_continuous(breaks = seq(min(data$Ig_Year), max(data$Ig_Year), by = 2))
# }
# 
# # Combined plot with vertical heatmap on the left
# combined_plot_vertical <- function(data) {
#   heat_plot <- vertical_heatmap_burned(data) + 
#     theme(legend.position = "none",
#           plot.margin = margin(r = 0))
#   
#   bar_plot <- horizontal_bar_chart(data) + 
#     theme(axis.text.y = element_blank(),
#           axis.title.y = element_blank(),
#           plot.margin = margin(l = 0))
#   
#   # Determine the widths of the plots
#   heatmap_width <- 1
#   bar_chart_width <- 5
#   
#   grid.arrange(heat_plot, bar_plot, 
#                ncol = 2, 
#                widths = c(heatmap_width, bar_chart_width))
# }
# 
# # Generate plots
# bar_plot <- horizontal_bar_chart(activity_proportions)
# heat_plot <- vertical_heatmap_burned(activity_proportions)
# combined_vertical <- combined_plot_vertical(activity_proportions)
# 
# # Save plots
# ggsave("horizontal_bar_chart.png", bar_plot, width = 12, height = 8)
# ggsave("vertical_heatmap.png", heat_plot, width = 3, height = 8)
# ggsave("combined_vertical.png", combined_vertical, width = 15, height = 10)
# 
# 



combined_ignition_year_treat = readRDS("combined_ignition_year_treat.RDS")
combined_ignition_year_monitor = readRDS("combined_ignition_year_monitor.RDS")

combined_ignition_year_treat_5years = readRDS("combined_ignition_year_treat_5years.RDS")
combined_ignition_year_monitor_5years = readRDS("combined_ignition_year_monitor_5years.RDS")


fig = {
  jpeg(filename = "selected_treatments_ignition_year.jpg",
       width = 700, height = 500,
       quality = 100,
       bg = "white",
       symbolfamily="default")
  
  # Filter the data for selected treatment types
  selected_treatments <- c("Initial Planting", "Fill in or Replant", "TSI - Release", "TSI - Thin")
  
  filtered_data <- net_activities %>%
    filter(type_labels %in% selected_treatments)
  
  p = ggplot() +
    theme_bw() +
    geom_bar(data = filtered_data,
             aes(x = Ig_Year, y = net_acres, fill = type_labels),
             stat = "identity", position = "dodge") +
    ggtitle("R5 Postfire Reforestation Net Acres by Ignition Year, 2000 - 2022") +
    labs(x = "Ignition Year", y = "Net Activity Acres", fill = "Treatment Type") +
    scale_x_continuous(breaks = seq(2000, 2022, 4)) +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_brewer(palette = "Set2") +
    theme(legend.position="bottom", 
          legend.box = "horizontal", 
          legend.text = element_text(size=12),
          plot.title = element_text(size=15), 
          plot.margin = margin(t=10,r=20,b=10,l=10),
          axis.title = element_text(face="bold", size=13),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12)) +
    guides(fill=guide_legend(title=NULL, nrow=2))
  
  print(p)
  dev.off()
}





fig = {
  jpeg(filename = "combined_ignition_year_treat.jpg",
       width = 700, height = 500,
       quality = 100,
       bg = "white",
       symbolfamily="default")
  p = ggplot() +
    theme_bw() +
    geom_bar(data = combined_ignition_year_treat,
             aes(x = Ig_Year, y = gross_acres, fill = "Gross Acres"),
             stat = "identity", position = "dodge") +
    geom_bar(data = combined_ignition_year_treat, 
             aes(x = Ig_Year, y = net_acres, fill = "Net Acres"),
             stat = "identity", position = "dodge") +
    ggtitle("R5 Postfire Reforestation Acres by Ignition Year, 2000 - 2022 (Treatments)") +
    labs(x = "Ignition Year", y = "Activity Acres") +
    scale_x_continuous(breaks = seq(2000, 2022, 4)) +
    scale_y_continuous(labels = scales::comma) +
    facet_wrap(~ type_labels, ncol = 3, scales = "free_y") +
    scale_fill_manual(values = c("Gross Acres" = "forestgreen", "Net Acres" = "mediumseagreen")) +
    theme(legend.position="bottom", 
          legend.box = "horizontal", 
          legend.text = element_text(size=12),
          plot.title = element_text(size=15), 
          plot.margin = margin(t=10,r=20,b=10,l=10),
          axis.title = element_text(face="bold", size=13),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          strip.text = element_text(face="bold", size=13)) +
    guides(fill=guide_legend(title=NULL, nrow=1), color=guide_legend(title=NULL, nrow=1))
  print(p)
  dev.off()
}


fig = {
  jpeg(filename = "combined_ignition_year_monitor.jpg",
       width = 700, height = 500,
       quality = 100,
       bg = "white",
       symbolfamily="default")
  p = ggplot() +
    theme_bw() +
    geom_bar(data = combined_ignition_year_monitor,
             aes(x = Ig_Year, y = gross_acres, fill = "Gross Acres"),
             stat = "identity", position = "dodge") +
    geom_bar(data = combined_ignition_year_monitor, 
             aes(x = Ig_Year, y = net_acres, fill = "Net Acres"),
             stat = "identity", position = "dodge") +
    ggtitle("R5 Postfire Reforestation Acres by Ignition Year, 2000 - 2022 (Monitoring)") +
    labs(x = "Ignition Year", y = "Activity Acres") +
    scale_x_continuous(breaks = seq(2000, 2022, 4)) +
    scale_y_continuous(labels = scales::comma) +
    facet_wrap(~ type_labels, ncol = 3, scales = "free_y") +
    scale_fill_manual(values = c("Gross Acres" = "forestgreen", "Net Acres" = "mediumseagreen")) +
    theme(legend.position="bottom", 
          legend.box = "horizontal", 
          legend.text = element_text(size=12),
          plot.title = element_text(size=15), 
          plot.margin = margin(t=10,r=20,b=10,l=10),
          axis.title = element_text(face="bold", size=13),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          strip.text = element_text(face="bold", size=13)) +
    guides(fill=guide_legend(title=NULL, nrow=1), color=guide_legend(title=NULL, nrow=1))
  print(p)
  dev.off()
}


select_activities = combined_ignition_year %>%
  filter(type_labels %in% c("Initial Planting","Fill-in or Replant",
                      "TSI - Release","Certification - Plant"))
select_activities$type_labels = factor(select_activities$type_labels,
  levels = c("Initial Planting","Fill-in or Replant","TSI - Release","Certification - Plant"))

fig = {
  jpeg(filename = "combined_ignition_year_select.jpg",
       width = 700, height = 500,
       quality = 100,
       bg = "white",
       symbolfamily="default")
  p = ggplot() +
    theme_bw() +
    geom_col(data = select_activities,
             aes(x = Ig_Year, y = net_acres, fill = "Net Acres"), position = "dodge") +
    ggtitle("R5 Postfire Reforestation Acres by Ignition Year, 2000 - 2022 (Monitoring)") +
    labs(x = "Ignition Year", y = "Activity Acres") +
    scale_x_continuous(breaks = seq(2000, 2022, 4)) +
    scale_y_continuous(labels = scales::comma) +
    facet_wrap(~ type_labels, ncol = 2, scales = "free_y") +
    scale_fill_manual(values = c("Gross Acres" = "forestgreen", "Net Acres" = "mediumseagreen")) +
    theme(legend.position="bottom", 
          legend.box = "horizontal", 
          legend.text = element_text(size=12),
          plot.title = element_text(size=15), 
          plot.margin = margin(t=10,r=20,b=10,l=10),
          axis.title = element_text(face="bold", size=13),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          strip.text = element_text(face="bold", size=13)) +
    guides(fill=guide_legend(title=NULL, nrow=1), color=guide_legend(title=NULL, nrow=1))
  print(p)
  dev.off()
}



combined_ignition_df = combined_ignition_year %>% st_drop_geometry()
combined_activity_df = combined_activity_year %>% st_drop_geometry()

