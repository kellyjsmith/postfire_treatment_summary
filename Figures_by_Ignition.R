#

library(tidyverse)
library(sf)

setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output")

# Cumulative area since start
gross_net_cumulative = readRDS("gross_net_cumulative.RDS")
ggplot(gross_net_cumulative, aes(x = end)) +
  geom_line(aes(y = gross_area_ac, color = "Gross Acres")) +
  geom_line(aes(y = net_area_ac, color = "Net Acres")) +
  ggtitle("Cumulative Acres Treated in R5 within fires") +
  labs(x = "Ignition Year", y = "Treatment Acres", color = "Type of Acres") +
  scale_x_continuous(breaks = seq(1990, 2020, 6)) +
  facet_wrap(~ ACTIVITY_TYPE, ncol = 3,scales="free_y") +
  # facet_wrap(~ ACTIVITY_TYPE, ncol = 3,scales="free_y") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
  )


# Cumulative area within 5 years of ignition
gross_net_cumulative_5years = readRDS("gross_net_cumulative_5years.RDS")
jpeg(filename = "gross_net_cumulative_5years.jpg")
ggplot(gross_net_cumulative_5years, aes(x = end)) +
  geom_line(aes(y = gross_area_ac, color = "Gross Acres")) +
  geom_line(aes(y = net_area_ac, color = "Net Acres")) +
  ggtitle("R5 Cumulative Postfire Acres Treated within 5 Years of Ignition") +
  labs(x = "Ignition Year", y = "Treatment Acres", color = "Type of Acres") +
  scale_x_continuous(breaks = seq(1990, 2020, 6)) +
  facet_wrap(~ ACTIVITY_TYPE, ncol = 3,scales="free_y") +
  # facet_wrap(~ ACTIVITY_TYPE, ncol = 3,scales="free_y") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
  )
dev.off()

# Combined, total cumulative area and within 5 years, by ignition year
gross_net_cumulative = readRDS("gross_net_cumulative.RDS")
gross_net_cumulative_5years = readRDS("gross_net_cumulative_5years.RDS")
new_labels = data.frame(ACTIVITY_TYPE = c("Certified_Planted","Fuels","Plant","Stand_Exam",
      "Survey_Stocking","TSI","Harvest_NonSalv","Replant","Silv_Prescription","SitePrep_NonChem",
      "Thin","Harvest_Salvage","Need_by_Fire","Survey_Other","Survey_Pretreatment",
      "SitePrep_Chem","Survey_Survival","Prune","Need_by_Failure","Certified_TSI"),
type_labels = c("Certification - Plant","Fuel Reduction","Plant Trees","Stand Exam",
      "Stocking Survey","TSI","Harvest - Non-Salvage","Replant/Fill-in","Silvicultural Prescription","Site Prep - Manual",
      "Thin","Harvest - Salvage","Reforestation Need (Fire)","Survey (Other)","Survey (Pretreatment)",
      "Site Prep - Chemical","Survival Survey","Prune","Reforestation Need (Failure)","Certification - TSI"))

gross_net_cumulative = merge(gross_net_cumulative, new_labels, by = "ACTIVITY_TYPE", all.x = TRUE)
gross_net_cumulative_5years = merge(gross_net_cumulative_5years, new_labels, by = "ACTIVITY_TYPE", all.x = TRUE)

# cumulative activities mega figure
jpeg(filename = "gross_net_cumulative_todate_vs_5years.jpg",
     width = 900, height = 1200,
     quality = 75,
     bg = "white",
     symbolfamily="default")
ggplot() +
  theme_bw() +
  geom_area(data = gross_net_cumulative, aes(x = end, y = gross_area_ac, fill = "Gross Acres to Date"),alpha = 0.9) +
  geom_area(data = gross_net_cumulative, aes(x = end, y = net_area_ac, fill = "Net Acres to Date"),alpha = 0.9) +
  # geom_bar(data = gross_net_cumulative, aes(x = end, y = gross_area_ac, fill = "Gross Acres to Date"),
  #       stat = "identity", position = "identity",alpha = 0.9) +
  # geom_bar(data = gross_net_cumulative, aes(x = end, y = net_area_ac, fill = "Net Acres to Date"),
  #       stat = "identity", position = "identity",alpha = 0.9) +
  geom_line(data = gross_net_cumulative_5years, aes(x = end, y = gross_area_ac, color = "Gross Acres within 5 Years"), linewidth = 1.25) +
  geom_line(data = gross_net_cumulative_5years, aes(x = end, y = net_area_ac, color = "Net Acres within 5 Years"), linewidth = 1.25) +
  # geom_line(data = gross_net_time_5years_f1, aes(x = end, y = equal_acres, color = "5-yr Gross = 5-yr Net (~1 m^2)"), size = 1.25) +
  ggtitle("R5 Reforestation Activities Completed to Date, by Ignition Year") +
  labs(x = "Ignition Year", y = "Activity Acres") +
  scale_x_continuous(breaks = seq(1990, 2020, 6)) +
  facet_wrap(~ type_labels, ncol = 3, scales = "free_y") +
  scale_fill_manual(values = c("Gross Acres to Date" = "darkgray","Net Acres to Date" = "lightgray")) +scale_color_manual(values = c("Gross Acres within 5 Years" = "blue", "Net Acres within 5 Years" = "red", "5-yr Gross = 5-yr Net (~1 m^2)" = "green")) +
  theme(legend.position="bottom", legend.box = "horizontal", plot.title = element_text(size=22), text = element_text(size=18)) +
  guides(fill=guide_legend(title=NULL, nrow=2), color=guide_legend(title=NULL,nrow=2))
# theme(legend.position = "none") +
# guides(fill="none")
dev.off()

jpeg(filename = "combined_cumulative_treat.jpg",
     width = 900, height = 600,
     quality = 75,
     bg = "white",
     symbolfamily="default")

ggplot() +
  theme_bw() +
  geom_area(data = combined_cumulative_treat, aes(x = end, y = gross_area_ac, fill = "Gross Acres to Date"), alpha = 0.9) +
  geom_area(data = combined_cumulative_treat, aes(x = end, y = net_area_ac, fill = "Net Acres to Date"), alpha = 0.9) +
  geom_line(data = combined_cumulative_treat_5years, 
            aes(x = end, y = gross_area_ac, color = "Gross Acres <= 5 Years Postfire"), linewidth = 1.25) +
  geom_line(data = combined_cumulative_treat_5years, 
            aes(x = end, y = net_area_ac, color = "Net Acres <= 5 Years Postfire"), linewidth = 1.25) +
  ggtitle("R5 Postfire Reforestation - Treatment Acres Completed by Ignition Year") +
  labs(x = "Ignition Year", y = "Treatment Acres") +
  scale_x_continuous(breaks = seq(1990, 2020, 6)) +
  facet_wrap(~ type_labels, ncol = 3, scales = "free_y") +
  scale_fill_manual(values = c("Gross Acres to Date" = "darkgray", "Net Acres to Date" = "lightgray")) +
  scale_color_manual(values = c("Gross Acres <= 5 Years Postfire" = "blue", "Net Acres <= 5 Years Postfire" = "red")) +
  theme(legend.position="bottom", legend.box = "horizontal", plot.title = element_text(size=22), text = element_text(size=18)) +
  guides(fill=guide_legend(title=NULL, nrow=1), color=guide_legend(title=NULL, nrow=1))

dev.off()

jpeg(filename = "combined_cumulative_monitor.jpg",
     width = 900, height = 600,
     quality = 75,
     bg = "white",
     symbolfamily="default")

ggplot() +
  theme_bw() +
  geom_area(data = combined_cumulative_monitor, aes(x = end, y = gross_area_ac, fill = "Gross Acres to Date"), alpha = 0.9) +
  geom_area(data = combined_cumulative_monitor, aes(x = end, y = net_area_ac, fill = "Net Acres to Date"), alpha = 0.9) +
  geom_line(data = combined_cumulative_monitor_5years, 
            aes(x = end, y = gross_area_ac, color = "Gross Acres <= 5 Years Postfire"), linewidth = 1.25) +
  geom_line(data = combined_cumulative_monitor_5years, 
            aes(x = end, y = net_area_ac, color = "Net Acres <= 5 Years Postfire"), linewidth = 1.25) +
  ggtitle("R5 Postfire Reforestation - Monitoring Acres Completed by Ignition Year") +
  labs(x = "Ignition Year", y = "Treatment Acres") +
  scale_x_continuous(breaks = seq(1990, 2020, 6)) +
  facet_wrap(~ type_labels, ncol = 3, scales = "free_y") +
  scale_fill_manual(values = c("Gross Acres to Date" = "darkgray", "Net Acres to Date" = "lightgray")) +
  scale_color_manual(values = c("Gross Acres <= 5 Years Postfire" = "blue", "Net Acres <= 5 Years Postfire" = "red")) +
  theme(legend.position="bottom", legend.box = "horizontal", plot.title = element_text(size=22), text = element_text(size=18)) +
  guides(fill=guide_legend(title=NULL, nrow=1), color=guide_legend(title=NULL, nrow=1))

dev.off()


gross_net_ignition_15years = readRDS("gross_net_ignition_15years.RDS")

ggplot(gross_net_ignition_15years[gross_net_ignition_15years$nyears==5,], aes(x = Ig_Year)) +
  geom_line(aes(y = gross_area_ac, color = "Gross Acres")) +
  geom_line(aes(y = net_area_ac, color = "Net Acres")) +
  ggtitle( "R5 Postfire Acres Treated within 5 Years of Ignition") +
  labs(x = "Ignition Year", y = "Treatment Acres", color = "Type of Acres") +
  scale_x_continuous(breaks = seq(1992, 2020, 6)) +
  facet_wrap(~ ACTIVITY_TYPE, ncol = 3, scales="free_y") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
  )

ggplot(gross_net_ignition_15years[gross_net_ignition_15years$nyears==10,], aes(x = Ig_Year)) +
  geom_line(aes(y = gross_area_ac, color = "Gross Acres")) +
  geom_line(aes(y = net_area_ac, color = "Net Acres")) +
  ggtitle( "R5 Postfire Acres Treated within 10 Years of Ignition") +
  labs(x = "Ignition Year", y = "Treatment Acres", color = "Type of Acres") +
  scale_x_continuous(breaks = seq(1992, 2020, 6)) +
  facet_wrap(~ ACTIVITY_TYPE, ncol = 3,scales="free_y") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
  )

ggplot(gross_net_ignition_15years[gross_net_ignition_15years$nyears==15,], aes(x = Ig_Year)) +
  geom_line(aes(y = gross_area_ac, color = "Gross Acres")) +
  geom_line(aes(y = net_area_ac, color = "Net Acres")) +
  ggtitle( "R5 Postfire Acres Treated within 15 Years of Ignition") +
  labs(x = "Ignition Year", y = "Treatment Acres", color = "Type of Acres") +
  scale_x_continuous(breaks = seq(1992, 2020, 6)) +
  facet_wrap(~ ACTIVITY_TYPE, ncol = 3,scales="free_y") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
  )


gross_net_ignition_15years = readRDS("gross_net_ignition_15years.RDS")

new_labels = data.frame(ACTIVITY_TYPE = c("Certified_Planted","Fuels","Plant","Stand_Exam",
                                          "Survey_Stocking","TSI","Harvest_NonSalv","Replant","Silv_Prescription","SitePrep_NonChem",
                                          "Thin","Harvest_Salvage","Need_by_Fire","Survey_Other","Survey_Pretreatment",
                                          "SitePrep_Chem","Survey_Survival","Prune","Need_by_Failure","Certified_TSI"),
                        type_labels = c("Plant Certification","Fuel Reduction","Plant Trees","Stand Exam",
                                        "Stocking Survey","TSI","Harvest - Non-Salvage","Replant/Fill-in","Silvicultural Prescription","Site Prep (Manual)",
                                        "Thin","Harvest - Salvage","Reforestation Need (Fire)","Survey (Other)","Survey (Pretreatment)",
                                        "Site Prep - Chemical","Survival Survey","Prune","Reforestation Need (Failure)","TSI Certification"))
gross_net_ignition_15years = merge(gross_net_ignition_15years, new_labels, by = "ACTIVITY_TYPE", all.x = TRUE)

jpeg(filename = "gross_net_ignition_15years.jpg",
     width = 900, height = 1200,
     quality = 75,
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


jpeg(filename = "gross_net_ignition_todate.jpg",
     width = 900, height = 1200,
     quality = 75,
     bg = "white",
     symbolfamily="default")

ggplot() +
  theme_bw() +
  geom_bar(data = gross_net_ignition_30years, 
           aes(x = Ig_Year, y = net_area_ac, fill = "Net Acres To-Date"),
           stat = "identity", position = "identity", alpha = 0.8) +
  geom_bar(data = gross_net_ignition_30years[gross_net_ignition_30years$nyears==5,], 
           aes(x = Ig_Year, y = net_area_ac, fill = "Net Acres within 5 Years"),
           stat = "identity", position = "identity", alpha = 0.5) +
  ggtitle("R5 Reforestation Activities Completed to Date, by Ignition Year") +
  labs(x = "Ignition Year", y = "Activity Acres") +
  scale_x_continuous(breaks = seq(1990, 2020, 6)) +
  facet_wrap(~ type_labels, ncol = 4, scales = "free_y") +
  scale_fill_manual(values = c("Net Acres within 5 Years" = "darkblue", "Net Acres To-Date" = "gray")) +
  theme(legend.position="bottom", 
        legend.box = "horizontal", 
        plot.title = element_text(size=22), 
        text = element_text(size=18)) +
  guides(fill=guide_legend(title=NULL, ncol = 3))

dev.off()
