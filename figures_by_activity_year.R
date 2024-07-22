
library(sf)
library(tidyverse)

setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output")

gross_activities = readRDS("gross_activities.RDS")
net_activities = readRDS("net_activities.RDS")

combined_by_fire = readRDS("combined_by_fire.RDS")


fig = {
  jpeg(filename = "combined_activity_year_treat.jpg",
       width = 800, height = 600,
       quality = 100,
       bg = "white",
       symbolfamily="default")
  p = ggplot() +
    theme_bw() +
    geom_bar(data = combined_activity_year_treat,
             aes(x = year, y = gross_acres, fill = "Gross Acres"),
             stat = "identity", position = "dodge") +
    geom_bar(data = combined_activity_year_treat, 
             aes(x = year, y = net_acres, fill = "Net Acres"),
             stat = "identity", position = "dodge") +
    ggtitle("R5 Postfire Reforestation Acres by Activity Year, 1994 - 2022 - Treatments") +
    labs(x = "Activity Year", y = "Activity Acres") +
    scale_x_continuous(breaks = seq(1990, 2024, 6)) +
    scale_y_continuous(labels = scales::comma) +
    facet_wrap(~ type_labels, ncol = 3, scales = "free_y") +
    scale_fill_manual(values = c("Gross Acres" = "gray", "Net Acres" = "blue")) +
    theme(legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=13), plot.title = element_text(size=18),
          axis.title = element_text(size=14), axis.text = element_text(size=12), strip.text = element_text(size=14)) +
    guides(fill=guide_legend(title=NULL, nrow=1), color=guide_legend(title=NULL, nrow=1))
  print(p)
  dev.off()
}


fig = {
  jpeg(filename = "combined_activity_year_monitor.jpg",
       width = 800, height = 600,
       quality = 100,
       bg = "white",
       symbolfamily="default")
  p = ggplot() +
    theme_bw() +
    geom_bar(data = combined_activities_monitor,
             aes(x = year, y = gross_acres, fill = "Gross Acres"),
             stat = "identity", position = "dodge") +
    geom_bar(data = combined_activities_monitor, 
             aes(x = year, y = net_acres, fill = "Net Acres"),
             stat = "identity", position = "dodge") +
    ggtitle("R5 Postfire Reforestation Acres by Activity Year, 1994 - 2022 - Monitoring") +
    labs(x = "Activity Year", y = "Activity Acres") +
    scale_x_continuous(breaks = seq(1990, 2024, 6)) +
    scale_y_continuous(labels = scales::comma) +
    facet_wrap(~ type_labels, ncol = 3, scales = "free_y") +
    scale_fill_manual(values = c("Gross Acres" = "gray", "Net Acres" = "blue")) +
    theme(legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=13), plot.title = element_text(size=18),
          axis.title = element_text(size=14), axis.text = element_text(size=12), strip.text = element_text(size=14)) +
    guides(fill=guide_legend(title=NULL, nrow=1), color=guide_legend(title=NULL, nrow=1))
  print(p)
  dev.off()
}



