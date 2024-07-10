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
"Survey","TSI","Harvest_NonSalv","Prescription","Replant","SitePrep_NonChem","Thin","Harvest_Salvage",
"Need_by_Fire","SitePrep_Chem","Prune","Need_by_Failure","Certified_TSI","Mapping"),
ACTIVITY_TYPE_LABELS = c("Planting Certification","Fuels","Planting","Stand Exam",
                    "Survey","TSI","Harvest (Non-Salvage)","Prescription","Replanting/Fill-in","Site Prep (Non-Chemical)","Thin","Salvage",
                    "Reforestation Need by Fire","Site Prep (Chemical)","Prune","Reforestation Need by Failure","TSI Certification","Mapping"))
gross_net_cumulative = merge(gross_net_cumulative, new_labels, by = "ACTIVITY_TYPE", all.x = TRUE)
gross_net_cumulative_5years = merge(gross_net_cumulative_5years, new_labels, by = "ACTIVITY_TYPE", all.x = TRUE)
jpeg(filename = "gross_net_cumulative_5years_allyears.jpg",
     width = 900, height = 1200,
     quality = 75,
     bg = "white",
     symbolfamily="default")
ggplot() +
  theme_bw() +
  geom_area(data = gross_net_cumulative, aes(x = end, y = gross_area_ac, fill = "Gross Acres to Date"),alpha = 0.9) +
  geom_area(data = gross_net_cumulative, aes(x = end, y = net_area_ac, fill = "Net Acres to Date"),alpha = 0.9) +
  geom_line(data = gross_net_cumulative_5years, aes(x = end, y = gross_area_ac, color = "Gross Acres within 5 Years"), linewidth = 1.25) +
  geom_line(data = gross_net_cumulative_5years, aes(x = end, y = net_area_ac, color = "Net Acres within 5 Years"), linewidth = 1.25) +
  # geom_line(data = gross_net_time_5years_f1, aes(x = end, y = equal_acres, color = "5-yr Gross = 5-yr Net (~1 m^2)"), size = 1.25) +
  ggtitle("R5 Reforestation Activities Completed to Date, by Ignition Year") +
  labs(x = "Ignition Year", y = "Activity Acres") +
  scale_x_continuous(breaks = seq(1990, 2020, 6)) +
  facet_wrap(~ ACTIVITY_TYPE_LABELS, ncol = 3, scales = "free_y") +
  scale_fill_manual(values = c("Gross Acres to Date" = "gray","Net Acres to Date" = "lightgray")) +scale_color_manual(values = c("Gross Acres within 5 Years" = "blue", "Net Acres within 5 Years" = "red", "5-yr Gross = 5-yr Net (~1 m^2)" = "green")) +
  theme(legend.position="bottom", legend.box = "horizontal", plot.title = element_text(size=22), text = element_text(size=18)) +
  guides(fill=guide_legend(title=NULL, nrow=2), color=guide_legend(title=NULL,nrow=2))
# theme(legend.position = "none") +
# guides(fill="none")
dev.off()
