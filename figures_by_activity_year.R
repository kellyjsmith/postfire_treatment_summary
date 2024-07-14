
gross_activities = readRDS("gross_activities.RDS")
net_activities = readRDS("net_activities.RDS")

jpeg(filename = "gross_net_activity_year.jpg",
     width = 900, height = 1200,
     quality = 75,
     bg = "white",
     symbolfamily="default")

ggplot() +
  theme_bw() +
  # geom_bar(data = gross_activities,
  #          aes(x = ref_year, y = gross_acres, fill = "Gross Acres"),
  #          stat = "identity", position = "identity", alpha = 0.9) +
  geom_bar(data = net_activities, 
           aes(x = ref_year, y = net_acres, fill = "Net Acres"),
           stat = "identity", position = "identity", alpha = 0.75) +
  ggtitle("R5 Reforestation Activities Completed by Activity Year") +
  labs(x = "Activity Year", y = "Activity Acres") +
  scale_x_continuous(breaks = seq(1990, 2020, 10)) +
  facet_wrap(~ type_labels, ncol = 4, scales = "free_y") +
  scale_fill_manual(values = c("Net Acres" = "darkgreen", "Gross Acres" = "gray")) +
  theme(legend.position="bottom", 
        legend.box = "horizontal", 
        plot.title = element_text(size=22), 
        text = element_text(size=18)) +
  guides(fill=guide_legend(title=NULL, ncol = 3))

dev.off()



jpeg(filename = "gross_net_activity_year.jpg",
     width = 900, height = 1200,
     quality = 75,
     bg = "white",
     symbolfamily="default")

ggplot() +
  theme_bw() +
  geom_bar(data = gross_net_summary1,
           aes(x = year, y = gross_acres, fill = "Gross Acres"),
           stat = "identity", position = "identity", alpha = 0.9) +
  geom_bar(data = gross_net_summary1, 
           aes(x = year, y = net_acres, fill = "Net Acres"),
           stat = "identity", position = "identity", alpha = 0.75) +
  ggtitle("R5 Reforestation Activities Completed by Activity Year") +
  labs(x = "Activity Year", y = "Activity Acres") +
  scale_x_continuous(breaks = seq(1990, 2020, 10)) +
  facet_wrap(~ type_labels, ncol = 4, scales = "free_y") +
  scale_fill_manual(values = c("Net Acres" = "darkgreen", "Gross Acres" = "gray")) +
  theme(legend.position="bottom", 
        legend.box = "horizontal", 
        plot.title = element_text(size=22), 
        text = element_text(size=18)) +
  guides(fill=guide_legend(title=NULL, ncol = 3))

dev.off()