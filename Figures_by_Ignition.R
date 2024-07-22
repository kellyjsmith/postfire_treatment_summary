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

# gross_net_cumulative = merge(gross_net_cumulative, new_labels, by = "ACTIVITY_TYPE", all.x = TRUE)
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

combined_cumulative_treat = readRDS("combined_cumulative_treat.RDS")
combined_cumulative_monitor = readRDS("combined_cumulative_monitor.RDS")

combined_cumulative_treat_5years = readRDS("combined_cumulative_treat_5years.RDS")
combined_cumulative_monitor_5years = readRDS("combined_cumulative_monitor_5years.RDS")

fig = {
  jpeg(filename = "combined_cumulative_treat.jpg",
       width = 800, height = 600,
       quality = 100,
       bg = "white",
       symbolfamily="default")
  p = ggplot() +
        theme_bw() +
        geom_area(data = combined_cumulative_treat, aes(x = end, y = gross_area_ac, fill = "Gross Acres to Date"), alpha = 0.9) +
        geom_area(data = combined_cumulative_treat, aes(x = end, y = net_area_ac, fill = "Net Acres to Date"), alpha = 0.9) +
        geom_line(data = combined_cumulative_treat_5years, 
                  aes(x = end, y = gross_area_ac, color = "Gross Acres <= 5 Years Postfire"), linewidth = 1.25) +
        geom_line(data = combined_cumulative_treat_5years, 
                  aes(x = end, y = net_area_ac, color = "Net Acres <= 5 Years Postfire"), linewidth = 1.25) +
        ggtitle("Cumulative Postfire Reforestation Acres in R5, 1994 - 2022 (Treatments)") +
        labs(x = "Year", y = "Activity Acres") +
        scale_x_continuous(breaks = seq(1990, 2020, 6)) +
        scale_y_continuous(labels = scales::comma) +
        facet_wrap(~ type_labels, ncol = 3, scales = "free_y") +
        scale_fill_manual(values = c("Gross Acres to Date" = "darkgray", "Net Acres to Date" = "lightgray")) +
        scale_color_manual(values = c("Gross Acres <= 5 Years Postfire" = "blue", "Net Acres <= 5 Years Postfire" = "red")) +
        theme(legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=13), plot.title = element_text(size=18),
              axis.title = element_text(size=14), axis.text = element_text(size=12), strip.text = element_text(size=14)) +
        guides(fill=guide_legend(title=NULL, nrow=1), color=guide_legend(title=NULL, nrow=1))
  print(p)
  dev.off()
}

fig = {
  jpeg(filename = "combined_cumulative_monitor.jpg",
       width = 800, height = 600,
       quality = 100, bg = "white")
  p = ggplot() +
    theme_bw() +
    geom_area(data = combined_cumulative_monitor, aes(x = end, y = gross_area_ac, fill = "Gross Acres to Date"), alpha = 0.9) +
    geom_area(data = combined_cumulative_monitor, aes(x = end, y = net_area_ac, fill = "Net Acres to Date"), alpha = 0.9) +
    geom_line(data = combined_cumulative_monitor_5years, 
              aes(x = end, y = gross_area_ac, color = "Gross Acres <= 5 Years Postfire"), linewidth = 1.25) +
    geom_line(data = combined_cumulative_monitor_5years, 
              aes(x = end, y = net_area_ac, color = "Net Acres <= 5 Years Postfire"), linewidth = 1.25) +
    ggtitle("Cumulative Postfire Reforestation Acres in R5, 1994 - 2022 (Monitoring)") +
    labs(x = "Year", y = "Activity Acres") +
    scale_x_continuous(breaks = seq(1990, 2024, 6)) +
    scale_y_continuous(labels = scales::comma) +
    facet_wrap(~ type_labels, ncol = 3, scales = "free_y") +
    scale_fill_manual(values = c("Gross Acres to Date" = "darkgray", "Net Acres to Date" = "lightgray")) +
    scale_color_manual(values = c("Gross Acres <= 5 Years Postfire" = "blue", "Net Acres <= 5 Years Postfire" = "red")) +
    theme(legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=13), plot.title = element_text(size=18),
          axis.title = element_text(size=14), axis.text = element_text(size=12), strip.text = element_text(size=14)) +
    guides(fill=guide_legend(title=NULL, nrow=1), color=guide_legend(title=NULL, nrow=1))
  print(p)
  dev.off()
}


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
    ggtitle("R5 Postfire Reforestation Acres by Ignition Year, 1994 - 2022 (Treatments)") +
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
