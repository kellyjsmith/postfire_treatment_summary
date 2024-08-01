#

library(tidyverse)
library(sf)

setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output")

combined_cumulative_treat = readRDS("combined_cumulative_treat.RDS")
combined_cumulative_monitor = readRDS("combined_cumulative_monitor.RDS")

combined_cumulative_treat_5years = readRDS("combined_cumulative_treat_5years.RDS")
combined_cumulative_monitor_5years = readRDS("combined_cumulative_monitor_5years.RDS")

combined_ignition_year = readRDS("combined_ignition_year.RDS")
combined_ignition_select = combined_ignition_year %>%
  filter(type_labels %in% c("Plant Trees", "TSI - Release", "Survival Survey", "Stocking Survey"))

combined_activity_year = readRDS("combined_activity_year.RDS")

fig = {
  jpeg(filename = "combined_cumulative_treat.jpg",
       width = 700, height = 500,
       quality = 100,
       bg = "white",
       symbolfamily="default")
  p = ggplot() +
        theme_bw() +
        geom_area(data = combined_cumulative_treat, aes(x = end, y = gross_area_ac, fill = "Gross Acres to Date"), alpha = 0.9) +
        geom_area(data = combined_cumulative_treat, aes(x = end, y = net_area_ac, fill = "Net Acres to Date"), alpha = 0.9) +
        geom_line(data = combined_cumulative_5years_treat, 
                  aes(x = end, y = gross_area_ac, color = "Gross Acres <= 5 Years Postfire"), linewidth = 1.25) +
        geom_line(data = combined_cumulative_5years_treat, 
                  aes(x = end, y = net_area_ac, color = "Net Acres <= 5 Years Postfire"), linewidth = 1.25) +
        ggtitle("Cumulative Postfire Reforestation Activities in R5, 2002 - 2023 (Treatments)") +
        labs(x = "Year", y = "Activity Acres") +
        scale_x_continuous(breaks = seq(2000, 2024, 6)) +
        scale_y_continuous(labels = scales::comma) +
        facet_wrap(~ type_labels, ncol = 3, scales = "free_y") +
        scale_fill_manual(values = c("Gross Acres to Date" = "darkgray", "Net Acres to Date" = "lightgray")) +
        scale_color_manual(values = c("Gross Acres <= 5 Years Postfire" = "blue", "Net Acres <= 5 Years Postfire" = "red")) +
        theme(legend.position="bottom", 
              legend.box = "horizontal", 
              legend.text = element_text(size=11),
              plot.title = element_text(size=15), 
              plot.margin = margin(t=10,r=20,b=10,l=10),
              axis.title = element_text(face="bold", size=13),
              axis.text.x = element_text(size=11),
              axis.text.y = element_text(size=10),
              strip.text = element_text(size=12)) +
        guides(fill=guide_legend(title=NULL, nrow=1), color=guide_legend(title=NULL, nrow=1))
  print(p)
  dev.off()
}


fig = {
  jpeg(filename = "combined_cumulative_monitor.jpg",
       width = 700, height = 500,
       quality = 100, bg = "white")
  p = ggplot() +
    theme_bw() +
    geom_area(data = combined_cumulative_monitor, aes(x = end, y = gross_area_ac, fill = "Gross Acres to Date"), alpha = 0.9) +
    geom_area(data = combined_cumulative_monitor, aes(x = end, y = net_area_ac, fill = "Net Acres to Date"), alpha = 0.9) +
    geom_line(data = combined_cumulative_5years_monitor, 
              aes(x = end, y = gross_area_ac, color = "Gross Acres <= 5 Years Postfire"), linewidth = 1.25) +
    geom_line(data = combined_cumulative_5years_monitor, 
              aes(x = end, y = net_area_ac, color = "Net Acres <= 5 Years Postfire"), linewidth = 1.25) +
    ggtitle("Cumulative Postfire Reforestation Activities in R5, 2002 - 2023 (Monitoring)") +
    labs(x = "Year", y = "Activity Acres") +
    scale_x_continuous(breaks = seq(2000, 2024, 6)) +
    scale_y_continuous(labels = scales::comma) +
    facet_wrap(~ type_labels, ncol = 3, scales = "free_y") +
    scale_fill_manual(values = c("Gross Acres to Date" = "darkgray", "Net Acres to Date" = "lightgray")) +
    scale_color_manual(values = c("Gross Acres <= 5 Years Postfire" = "blue", "Net Acres <= 5 Years Postfire" = "red")) +
    theme(legend.position="bottom", 
          legend.box = "horizontal", 
          legend.text = element_text(size=11),
          plot.title = element_text(size=15), 
          plot.margin = margin(t=10,r=20,b=10,l=10),
          axis.title = element_text(face="bold", size=13),
          axis.text.x = element_text(size=11),
          axis.text.y = element_text(size=10),
          strip.text = element_text(size=12)) +
    guides(fill=guide_legend(title=NULL, nrow=1), color=guide_legend(title=NULL, nrow=1))
  print(p)
  dev.off()
}


combined_ignition_year_treat = readRDS("combined_ignition_year_treat.RDS")
combined_ignition_year_monitor = readRDS("combined_ignition_year_monitor.RDS")

combined_ignition_year_treat_5years = readRDS("combined_ignition_year_treat_5years.RDS")
combined_ignition_year_monitor_5years = readRDS("combined_ignition_year_monitor_5years.RDS")

fig = {
  jpeg(filename = "combined_ignition_year_treat.jpg",
       width = 800, height = 600,
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
    ggtitle("R5 Postfire Reforestation Acres by Ignition Year, 1993 - 2022 (Treatments)") +
    labs(x = "Ignition Year", y = "Activity Acres") +
    scale_x_continuous(breaks = seq(2000, 2024, 6)) +
    scale_y_continuous(labels = scales::comma) +
    facet_wrap(~ type_labels, ncol = 3, scales = "free_y") +
    scale_fill_manual(values = c("Gross Acres" = "gray", "Net Acres" = "#d1531d")) +
    theme(legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=13), plot.title = element_text(size=18),
          axis.title = element_text(size=14), axis.text = element_text(size=12), strip.text = element_text(size=14)) +
    guides(fill=guide_legend(title=NULL, nrow=1), color=guide_legend(title=NULL, nrow=1))
  print(p)
  dev.off()
}


fig = {
  jpeg(filename = "combined_ignition_year_monitor.jpg",
       width = 800, height = 600,
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
    ggtitle("R5 Postfire Reforestation Acres by Ignition Year, 1994 - 2022 (Monitoring)") +
    labs(x = "Ignition Year", y = "Activity Acres") +
    scale_x_continuous(breaks = seq(1990, 2024, 6)) +
    scale_y_continuous(labels = scales::comma) +
    facet_wrap(~ type_labels, ncol = 3, scales = "free_y") +
    scale_fill_manual(values = c("Gross Acres" = "gray", "Net Acres" = "darkred")) +
    theme(legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=13), plot.title = element_text(size=18),
          axis.title = element_text(size=14), axis.text = element_text(size=12), strip.text = element_text(size=14)) +
    guides(fill=guide_legend(title=NULL, nrow=1), color=guide_legend(title=NULL, nrow=1))
  print(p)
  dev.off()
}



fig = {
  jpeg(filename = "combined_ignition_year_select.jpg",
       width = 600, height = 400,
       quality = 100,
       bg = "white",
       symbolfamily="default")
  p = ggplot() +
    theme_bw() +
    geom_bar(data = combined_ignition_select,
             aes(x = Ig_Year, y = gross_acres, fill = "Gross Acres"),
             stat = "identity", position = "dodge") +
    geom_bar(data = combined_ignition_select,
             aes(x = Ig_Year, y = net_acres, fill = "Net Acres"),
             stat = "identity", position = "dodge") +
    ggtitle("Select Reforestation Activities in R5 by Fire Ignition Year, 1993 - 2022") +
    labs(x = "Ignition Year", y = "Activity Acres") +
    scale_x_continuous(breaks = seq(1990, 2024, 6)) +
    scale_y_continuous(labels = scales::comma) +
    facet_wrap(~ type_labels, ncol = 2, scales = "free_y") +
    scale_fill_manual(values = c("Gross Acres" = "gray", "Net Acres" = "darkred")) +
    theme(legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=13), plot.title = element_text(size=16),
          axis.title = element_text(size=14), axis.text = element_text(size=12), strip.text = element_text(face = "bold", size=14)) +
    guides(fill=guide_legend(title=NULL, nrow=1), color=guide_legend(title=NULL, nrow=1))
  print(p)
  dev.off()
}


fig = {
  jpeg(filename = "combined_ignition_year_monitor.jpg",
       width = 800, height = 600,
       quality = 100,
       bg = "white",
       symbolfamily="default")
ggplot() +
  theme_bw() +
  geom_area(data = gross_net_ignition_15years[gross_net_ignition_15years$nyears==15,], aes(x = Ig_Year, y = net_area_ac, fill = "Net Acres within 15 Years"),alpha = 0.9) +
  geom_area(data = gross_net_ignition_15years[gross_net_ignition_15years$nyears==10,], aes(x = Ig_Year, y = net_area_ac, fill = "Net Acres within 10 Years"),alpha = 0.9) +
  geom_area(data = gross_net_ignition_15years[gross_net_ignition_15years$nyears==5,], aes(x = Ig_Year, y = net_area_ac, fill = "Net Acres within 5 Years"),alpha = 0.9) +
  ggtitle("R5 Reforestation Activities Completed to Date, by Ignition Year") +
  labs(x = "Ignition Year", y = "Activity Acres") +
  scale_x_continuous(breaks = seq(1990, 2020, 6)) +
  facet_wrap(~ type_labels, ncol = 4, scales = "free_y") +
  scale_fill_manual(values = c("Net Acres within 5 Years" = "darkgray","Net Acres within 10 Years" = "gray","Net Acres within 15 Years" = "lightgray")) +
  theme(legend.position="bottom", legend.box = "horizontal", plot.title = element_text(size=22), text = element_text(size=18)) +
  guides(fill=guide_legend(title=NULL, ncol = 3))
dev.off()
}


combined_ignition_df = combined_ignition_year %>% st_drop_geometry()
combined_activity_df = combined_activity_year %>% st_drop_geometry()

