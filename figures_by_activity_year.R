
library(sf)
library(tidyverse)

setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output")

gross_activities = readRDS("gross_activities.RDS")
net_activities = readRDS("net_activities.RDS")

combined_activities = readRDS("combined_activities.RDS")

treat = c("Fuel Reduction", "Harvest - Non-Salvage", "Harvest - Salvage", "Plant Trees",
          "Prune", "Replant/Fill-in", "Site Prep - Chemical", "Site Prep (Manual)", "Thin", "TSI")
monitor = c("Plant Certification", "Reforestation Need (Failure)", "Reforestation Need (Fire)",
            "Silvicultural Prescription", "Stand Exam", "Stocking Survey", "Survey (Other)",
            "Survey (Pretreatment)", "Survival Survey", "TSI Certification")

combined_treat = combined_activities %>%
  filter(type_labels %in% treat)
combined_monitor = combined_activities %>%
  filter(type_labels %in% monitor)

jpeg(filename = "gross_net_activity_year.jpg",
     width = 900, height = 1200,
     quality = 75,
     bg = "white",
     symbolfamily="default")

ggplot() +
  theme_bw() +
  geom_bar(data = gross_activities,
           aes(x = ref_year, y = gross_acres, fill = "Gross Acres"),
           stat = "identity", position = "dodge") +
  geom_bar(data = net_activities, 
           aes(x = ref_year, y = net_acres, fill = "Net Acres"),
           stat = "identity", position = "dodge") +
  ggtitle("R5 Reforestation Activities Completed by Activity Year") +
  labs(x = "Activity Year", y = "Activity Acres") +
  scale_x_continuous(breaks = seq(1990, 2020, 10)) +
  facet_wrap(~ type_labels, ncol = 4, scales = "free_y") +
  scale_fill_manual(values = c("Net Acres" = "lightblue", "Gross Acres" = "blue")) +
  theme(legend.position="bottom", 
        legend.box = "horizontal", 
        plot.title = element_text(size=22), 
        text = element_text(size=18)) +
  guides(fill=guide_legend(title=NULL, ncol = 3))

dev.off()



jpeg(filename = "gross_net_activity_year1.jpg",
     width = 900, height = 1200,
     quality = 75,
     bg = "white",
     symbolfamily="default")

ggplot(combined_activities, aes(x = year)) +
  theme_bw() +
  geom_bar(aes(y = gross_acres, fill = "Gross Acres"),
           stat = "identity", position = position_dodge(width = 0.9), alpha = 0.75) +
  geom_bar(aes(y = net_acres, fill = "Net Acres"),
           stat = "identity", position = position_dodge(width = 0.9)) +
  ggtitle("R5 Reforestation Activities Completed by Activity Year") +
  labs(x = "Activity Year", y = "Activity Acres") +
  scale_x_continuous(breaks = seq(1990, 2020, 10)) +
  facet_wrap(~ type_labels, ncol = 4, scales = "free_y") +
  scale_fill_manual(values = c("Net Acres" = "lightblue", "Gross Acres" = "darkgreen")) +
  theme(legend.position="bottom", 
        legend.box = "horizontal", 
        plot.title = element_text(size=22), 
        text = element_text(size=18)) +
  guides(fill=guide_legend(title=NULL, ncol = 3))

dev.off()