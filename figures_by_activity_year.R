
library(sf)
library(tidyverse)

setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output")

gross_activities = readRDS("gross_activities.RDS")
net_activities = readRDS("net_activities.RDS")

combined_by_fire = readRDS("combined_by_fire.RDS")
combined_by_fire_treat = readRDS("combined_by_fire_treat.RDS")
combined_by_fire_monitor = readRDS("combined_by_fire_monitor.RDS")

combined_activity_year_treat = readRDS("combined_activity_year_treat.RDS")
combined_activity_year_monitor = readRDS("combined_activity_year_monitor.RDS")


fig = {
  jpeg(filename = "combined_activity_year_treat.jpg",
       width = 700, height = 500,
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
    ggtitle("R5 Postfire Reforestation Acres by Activity Year, 2002 - 2022 (Treatments)") +
    labs(x = "Activity Year", y = "Activity Acres") +
    scale_x_continuous(breaks = seq(2000, 2024, 5)) +
    scale_y_continuous(labels = scales::comma) +
    facet_wrap(~ type_labels, ncol = 3, scales = "free_y") +
    scale_fill_manual(values = c("Gross Acres" = "royalblue3", "Net Acres" = "cornflowerblue")) +
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
  jpeg(filename = "combined_activity_year_monitor.jpg",
       width = 700, height = 500,
       quality = 100,
       bg = "white",
       symbolfamily="default")
  p = ggplot() +
    theme_bw() +
    geom_bar(data = combined_activity_year_monitor,
             aes(x = year, y = gross_acres, fill = "Gross Acres"),
             stat = "identity", position = "dodge") +
    geom_bar(data = combined_activity_year_monitor, 
             aes(x = year, y = net_acres, fill = "Net Acres"),
             stat = "identity", position = "dodge") +
    ggtitle("R5 Postfire Reforestation Acres by Activity Year, 2002 - 2022 (Monitoring)") +
    labs(x = "Activity Year", y = "Activity Acres") +
    scale_x_continuous(breaks = seq(2000, 2024, 5)) +
    scale_y_continuous(labels = scales::comma) +
    facet_wrap(~ type_labels, ncol = 3, scales = "free_y") +
    scale_fill_manual(values = c("Gross Acres" = "royalblue3", "Net Acres" = "cornflowerblue")) +
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


combined_activity_year_select = combined_activity_year %>%
  filter(type_labels %in% c("Plant Trees", "TSI - Release", "Survival Survey", "Stocking Survey"))
fig = {
  jpeg(filename = "combined_activity_year_select.jpg",
       width = 600, height = 400,
       quality = 100,
       bg = "white",
       symbolfamily="default")
  p = ggplot() +
    theme_bw() +
    geom_bar(data = combined_activity_year_select,
             aes(x = year, y = gross_acres, fill = "Gross Acres"),
             stat = "identity", position = "dodge") +
    geom_bar(data = combined_activity_year_select,
             aes(x = year, y = net_acres, fill = "Net Acres"),
             stat = "identity", position = "dodge") +
    ggtitle("Select Reforestation Activities in R5 by Activity Year, 1993 - 2022") +
    labs(x = "Activity Year", y = "Activity Acres") +
    scale_x_continuous(breaks = seq(1990, 2024, 6)) +
    scale_y_continuous(labels = scales::comma) +
    facet_wrap(~ type_labels, ncol = 2, scales = "free_y") +
    scale_fill_manual(values = c("Gross Acres" = "#b6c5d1", "Net Acres" = "#167ac9")) +
    theme(legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=13), plot.title = element_text(size=16),
          axis.title = element_text(size=14), axis.text = element_text(size=12), strip.text = element_text(face = "bold", size=14)) +
    guides(fill=guide_legend(title=NULL, nrow=1), color=guide_legend(title=NULL, nrow=1))
  print(p)
  dev.off()
}
