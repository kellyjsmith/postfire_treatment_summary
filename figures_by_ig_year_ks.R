## Tables & Figures for summarizing postfire activities

library("sf")
library("dplyr")
library("terra")
library("tidyverse")
library("units")
library("data.table")
library("future")
library("furrr")

setwd("C:/Users/smithke3/OneDrive - Oregon State University/Kelly/Output")

assigned_activities = readRDS("assigned_activities_2024.RDS")
facts_fires = readRDS("facts_fires_2024.RDS")

# Summarize cumulative net and gross area for each combo
# of fire and activity type for a given timespan
net_gross_start_end <- function(start,end,assigned_activities){
  
  filtered <- assigned_activities |>
    filter(Ig_Year >= start & Ig_Year + diff_years <= end)
  
  if (dim(filtered)[1] == 0) {
    return(NULL)
  } else{
    filtered$gross_area <- st_area(filtered)
    net_gross <- filtered |> group_by(Event_ID, ACTIVITY_TYPE) |>
      summarize(
        geometry = st_union(geometry),
        gross_area = sum(gross_area)
      )
    net_gross$net_area <- st_area(net_gross)
    
    result <- net_gross |> group_by(ACTIVITY_TYPE) |>
      summarize(
        net_area = sum(net_area),
        gross_area = sum(gross_area)
      )
    result$start <- start
    result$end <- end
    
    return(result)
  }
}

# Map to function, cumulative area since 1994
gross_net_time <- map2_dfr(rep(1994,30),c(1994:2023),
                           net_gross_start_end,assigned_activities=assigned_activities)

gross_net_time$net_area_ac <- as.numeric(gross_net_time$net_area)/4046.86
gross_net_time$gross_area_ac <- as.numeric(gross_net_time$gross_area)/4046.86
saveRDS(gross_net_time, "gross_net_time.RDS")

# Cumulative area since 1994, only activities within 5 years of ignition
gross_net_time_5years <- map2_dfr(rep(1994,25),c(1994:2018),
                                  net_gross_start_end,
                                  assigned_activities=assigned_activities[assigned_activities$diff_years<=5,])

gross_net_time_5years$net_area_ac <- as.numeric(gross_net_time_5years$net_area)/4046.86
gross_net_time_5years$gross_area_ac <- as.numeric(gross_net_time_5years$gross_area)/4046.86
saveRDS(gross_net_time_5years, "gross_net_time_5years.RDS")

gross_net_time = readRDS("gross_net_time.RDS")
gross_net_time_5years = readRDS("gross_net_time_5years")

# Cumulative area since start
ggplot(gross_net_time, aes(x = end)) +
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


# use jpeg function
jpeg(filename = "figure1.jpg")
ggplot(gross_net_time_5years, aes(x = end)) +
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

# Filter the data frames for category groupings
filter1 = c("planting")
filter2 = c("prep","replant","release")
filter3 = c("prune")
filter4 = c("harvest_salvage","harvest","thin")
filter5 = c("cert_planted","cert_tsi","survey","review","need")

gross_net_time_f1 = gross_net_time %>%
  filter(ACTIVITY_TYPE %in% filter1)
gross_net_time_f2 = gross_net_time %>%
  filter(ACTIVITY_TYPE %in% filter2)
gross_net_time_f3 = gross_net_time %>%
  filter(ACTIVITY_TYPE %in% filter3)
gross_net_time_f4 = gross_net_time %>%
  filter(ACTIVITY_TYPE %in% filter4)
gross_net_time_f5 = gross_net_time %>%
  filter(ACTIVITY_TYPE %in% filter5)

gross_net_time_5years_f1 = gross_net_time_5years %>%
  filter(ACTIVITY_TYPE %in% filter1)
gross_net_time_5years_f2 = gross_net_time_5years %>%
  filter(ACTIVITY_TYPE %in% filter2)
gross_net_time_5years_f3 = gross_net_time_5years %>%
  filter(ACTIVITY_TYPE %in% filter3)
gross_net_time_5years_f4 = gross_net_time_5years %>%
  filter(ACTIVITY_TYPE %in% filter4)
gross_net_time_5years_f5 = gross_net_time_5years %>%
  filter(ACTIVITY_TYPE %in% filter5)

# Plot cumulative area by category grouping
ggplot() +
  theme_bw() +
  geom_area(data = gross_net_time_f1, aes(x = end, y = gross_area_ac, fill = "Gross Acres to Date"),alpha = 0.9) +
  geom_area(data = gross_net_time_f1, aes(x = end, y = net_area_ac, fill = "Net Acres to Date"),alpha = 0.9) +
  geom_line(data = gross_net_time_5years_f1, aes(x = end, y = gross_area_ac, color = "Gross Acres within 5 Years"), size = 1.25) +
  geom_line(data = gross_net_time_5years_f1, aes(x = end, y = net_area_ac, color = "Net Acres within 5 Years"), size = 1.25) +
  # geom_line(data = gross_net_time_5years_f1, aes(x = end, y = equal_acres, color = "5-yr Gross = 5-yr Net (~1 m^2)"), size = 1.25) +
  ggtitle("R5 Reforestation Activities Completed to Date, by Ignition Year") +
  labs(x = "", y = "Activity Acres") +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  facet_wrap(~ ACTIVITY_TYPE, ncol = 2) +
  # scale_fill_manual(values = c("Gross Acres to Date" = "darkgray", "Net Acres to Date" = "black")) +
  scale_fill_manual(values = c("Gross Acres to Date" = "gray","Net Acres to Date" = "lightgray")) +
  scale_color_manual(values = c("Gross Acres within 5 Years" = "blue", "Net Acres within 5 Years" = "red", "5-yr Gross = 5-yr Net (~1 m^2)" = "green")) +
  # theme(legend.position="bottom", legend.box = "horizontal", plot.title = element_text(size=12)) +
  # guides(fill=guide_legend(title=NULL, nrow=2), color=guide_legend(title=NULL,nrow=2))
  theme(legend.position = "none") +
  guides(fill="none")

ggplot() +
  theme_bw() +
  geom_area(data = gross_net_time_f2, aes(x = end, y = gross_area_ac, fill = "Gross Acres to Date"),alpha = 0.9) +
  geom_area(data = gross_net_time_f2, aes(x = end, y = net_area_ac, fill = "Net Acres to Date"),alpha = 0.9) +
  geom_line(data = gross_net_time_5years_f2, aes(x = end, y = gross_area_ac, color = "Gross Acres within 5 Years"), size = 1.25) +
  geom_line(data = gross_net_time_5years_f2, aes(x = end, y = net_area_ac, color = "Net Acres within 5 Years"), size = 1.25) +
  # geom_line(data = gross_net_time_5years_f1, aes(x = end, y = equal_acres, color = "5-yr Gross = 5-yr Net (~1 m^2)"), size = 1.25) +
  ggtitle("R5 Reforestation Activities Completed to Date, by Ignition Year") +
  labs(x = "", y = "Activity Acres") +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  facet_wrap(~ ACTIVITY_TYPE, ncol = 2) +
  # scale_fill_manual(values = c("Gross Acres to Date" = "darkgray", "Net Acres to Date" = "black")) +
  scale_fill_manual(values = c("Gross Acres to Date" = "gray","Net Acres to Date" = "lightgray")) +
  scale_color_manual(values = c("Gross Acres within 5 Years" = "blue", "Net Acres within 5 Years" = "red", "5-yr Gross = 5-yr Net (~1 m^2)" = "green")) +
  # theme(legend.position="bottom", legend.box = "horizontal", plot.title = element_text(size=12)) +
  # guides(fill=guide_legend(title=NULL, nrow=2), color=guide_legend(title=NULL,nrow=2))
  theme(legend.position = "none") +
  guides(fill="none")

ggplot() +
  theme_bw() +
  geom_area(data = gross_net_time_f3, aes(x = end, y = gross_area_ac, fill = "Gross Acres to Date"),alpha = 0.9) +
  geom_area(data = gross_net_time_f3, aes(x = end, y = net_area_ac, fill = "Net Acres to Date"),alpha = 0.9) +
  geom_line(data = gross_net_time_5years_f3, aes(x = end, y = gross_area_ac, color = "Gross Acres within 5 Years"), size = 1.25) +
  geom_line(data = gross_net_time_5years_f3, aes(x = end, y = net_area_ac, color = "Net Acres within 5 Years"), size = 1.25) +
  # geom_line(data = gross_net_time_5years_f1, aes(x = end, y = equal_acres, color = "5-yr Gross = 5-yr Net (~1 m^2)"), size = 1.25) +
  ggtitle("R5 Reforestation Activities Completed to Date, by Ignition Year") +
  labs(x = "", y = "Activity Acres") +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  facet_wrap(~ ACTIVITY_TYPE, ncol = 2) +
  # scale_fill_manual(values = c("Gross Acres to Date" = "darkgray", "Net Acres to Date" = "black")) +
  scale_fill_manual(values = c("Gross Acres to Date" = "gray","Net Acres to Date" = "lightgray")) +
  scale_color_manual(values = c("Gross Acres within 5 Years" = "blue", "Net Acres within 5 Years" = "red", "5-yr Gross = 5-yr Net (~1 m^2)" = "green")) +
  # theme(legend.position="bottom", legend.box = "horizontal", plot.title = element_text(size=12)) +
  # guides(fill=guide_legend(title=NULL, nrow=2), color=guide_legend(title=NULL,nrow=2))
  theme(legend.position = "none") +
  guides(fill="none")

ggplot() +
  theme_bw() +
  geom_area(data = gross_net_time_f4, aes(x = end, y = gross_area_ac, fill = "Gross Acres to Date"),alpha = 0.9) +
  geom_area(data = gross_net_time_f4, aes(x = end, y = net_area_ac, fill = "Net Acres to Date"),alpha = 0.9) +
  geom_line(data = gross_net_time_5years_f4, aes(x = end, y = gross_area_ac, color = "Gross Acres within 5 Years"), size = 1.25) +
  geom_line(data = gross_net_time_5years_f4, aes(x = end, y = net_area_ac, color = "Net Acres within 5 Years"), size = 1.25) +
  # geom_line(data = gross_net_time_5years_f1, aes(x = end, y = equal_acres, color = "5-yr Gross = 5-yr Net (~1 m^2)"), size = 1.25) +
  ggtitle("R5 Reforestation Activities Completed to Date, by Ignition Year") +
  labs(x = "", y = "Activity Acres") +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  facet_wrap(~ ACTIVITY_TYPE, ncol = 2) +
  # scale_fill_manual(values = c("Gross Acres to Date" = "darkgray", "Net Acres to Date" = "black")) +
  scale_fill_manual(values = c("Gross Acres to Date" = "gray","Net Acres to Date" = "lightgray")) +
  scale_color_manual(values = c("Gross Acres within 5 Years" = "blue", "Net Acres within 5 Years" = "red", "5-yr Gross = 5-yr Net (~1 m^2)" = "green")) +
  # theme(legend.position="bottom", legend.box = "horizontal", plot.title = element_text(size=12)) +
  # guides(fill=guide_legend(title=NULL, nrow=2), color=guide_legend(title=NULL,nrow=2))
  theme(legend.position = "none") +
  guides(fill="none")

ggplot() +
  theme_bw() +
  geom_area(data = gross_net_time_f5, aes(x = end, y = gross_area_ac, fill = "Gross Acres to Date"),alpha = 0.9) +
  geom_area(data = gross_net_time_f5, aes(x = end, y = net_area_ac, fill = "Net Acres to Date"),alpha = 0.9) +
  geom_line(data = gross_net_time_5years_f5, aes(x = end, y = gross_area_ac, color = "Gross Acres within 5 Years"), size = 1.25) +
  geom_line(data = gross_net_time_5years_f5, aes(x = end, y = net_area_ac, color = "Net Acres within 5 Years"), size = 1.25) +
  # geom_line(data = gross_net_time_5years_f1, aes(x = end, y = equal_acres, color = "5-yr Gross = 5-yr Net (~1 m^2)"), size = 1.25) +
  ggtitle("R5 Reforestation Activities Completed to Date, by Ignition Year") +
  labs(x = "", y = "Activity Acres") +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  facet_wrap(~ ACTIVITY_TYPE, ncol = 2) +
  # scale_fill_manual(values = c("Gross Acres to Date" = "darkgray", "Net Acres to Date" = "black")) +
  scale_fill_manual(values = c("Gross Acres to Date" = "gray","Net Acres to Date" = "lightgray")) +
  scale_color_manual(values = c("Gross Acres within 5 Years" = "blue", "Net Acres within 5 Years" = "red", "5-yr Gross = 5-yr Net (~1 m^2)" = "green")) +
  # theme(legend.position="bottom", legend.box = "horizontal", plot.title = element_text(size=12)) +
  # guides(fill=guide_legend(title=NULL, nrow=2), color=guide_legend(title=NULL,nrow=2))
  theme(legend.position = "none") +
  guides(fill="none")

# gridExtra::grid.arrange(c1, c2, c3, c4, c5, ncol = 1)

ggplot() +
  theme_bw() +
  geom_area(data = gross_net_time, aes(x = end, y = gross_area_ac, fill = "Gross Acres to Date"),alpha = 0.9) +
  geom_area(data = gross_net_time, aes(x = end, y = net_area_ac, fill = "Net Acres to Date"),alpha = 0.9) +
  geom_line(data = gross_net_time_5years, aes(x = end, y = gross_area_ac, color = "Gross Acres within 5 Years"), size = 1.25) +
  geom_line(data = gross_net_time_5years, aes(x = end, y = net_area_ac, color = "Net Acres within 5 Years"), size = 1.25) +
  # geom_line(data = gross_net_time_5years_f1, aes(x = end, y = equal_acres, color = "5-yr Gross = 5-yr Net (~1 m^2)"), size = 1.25) +
  ggtitle("R5 Reforestation Activities Completed to Date, by Ignition Year") +
  labs(x = "Ignition Year", y = "Activity Acres") +
  scale_x_continuous(breaks = seq(1990, 2020, 6)) +
  facet_wrap(~ ACTIVITY_TYPE, ncol = 3, scales = "free_y") +
  scale_fill_manual(values = c("Gross Acres to Date" = "gray","Net Acres to Date" = "lightgray")) +
  scale_color_manual(values = c("Gross Acres within 5 Years" = "blue", "Net Acres within 5 Years" = "red", "5-yr Gross = 5-yr Net (~1 m^2)" = "green")) +
  theme(legend.position="bottom", legend.box = "horizontal", plot.title = element_text(size=12)) +
  guides(fill=guide_legend(title=NULL, nrow=2), color=guide_legend(title=NULL,nrow=2))
  # theme(legend.position = "none") +
  # guides(fill="none")



net_gross_nyears <- function(nyears,assigned_activities){
  
  filtered <- assigned_activities %>%
    filter(diff_years<=nyears)
  
  if (dim(filtered)[1] == 0) {
    return(NULL)
  } else{
    filtered$gross_area <- st_area(filtered)
    result <- filtered %>% group_by(Ig_Year, ACTIVITY_TYPE) %>%
      summarize(
        geometry = st_union(geometry),
        gross_area = sum(gross_area)
      )
    result$net_area <- st_area(result)
    result$nyears <- nyears
    return(result)
  }
}
gross_net_nyears <- map_dfr(rep(1:15),net_gross_nyears,assigned_activities=assigned_activities)


net_gross_activity <- function(nyears,assigned_activities){
  
  filtered <- assigned_activities %>%
    filter(diff_years<=nyears)
  
  if (dim(filtered)[1] == 0) {
    return(NULL)
  } else{
    filtered$gross_area <- st_area(filtered)
    result <- filtered %>% group_by(Ig_Year, ACTIVITY) %>%
      summarize(
        geometry = st_union(geometry),
        gross_area = sum(gross_area)
      )
    result$net_area <- st_area(result)
    result$nyears <- nyears
    return(result)
  }
}
gross_net_15years <- map_dfr(rep(1:15),net_gross_activity,assigned_activities=assigned_activities)


gross_net_15years$net_area_ac <- as.numeric(gross_net_15years$net_area)/4046.86
gross_net_15years$gross_area_ac <- as.numeric(gross_net_15years$gross_area)/4046.86

saveRDS(gross_net_nyears, "gross_net_nyears.RDS")


ggplot(gross_net_nyears[gross_net_nyears$nyears==5,], aes(x = Ig_Year)) +
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


ggplot(gross_net_nyears[gross_net_nyears$nyears==10,], aes(x = Ig_Year)) +
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



comb_fire_diff <- expand.grid(fire_year = unique(assigned_activities$Ig_Year),
                              diff_years =unique(assigned_activities$diff_years))

net_activities <-
  map2_dfr(comb_fire_diff$fire_year, comb_fire_diff$diff_years,
           function(x, y, assigned_activities) {
             filtered <- assigned_activities |>
               filter(Ig_Year == x & diff_years <= y)
             
             if (dim(filtered)[1] == 0) {
               return(NULL)
             } else{
               result <- filtered |> group_by(Event_ID, ACTIVITY_TYPE) |>
                 summarize(
                   geometry = st_union(geometry),
                   n_dissolved = n(),
                   Ig_Year = x,
                   diff_years = y
                 )
               result$ref_year <-
                 result$Ig_Year + result$diff_years
               result$net_area <- st_area(result)
               return(result)
             }
           }, assigned_activities = assigned_activities)

gross_activities <-
  map2_dfr(comb_fire_diff$fire_year, comb_fire_diff$diff_years,
           function(x, y, assigned_activities) {
             filtered <- assigned_activities |>
               filter(Ig_Year == x &
                        diff_years <= y)
             
             if (dim(filtered)[1] == 0) {
               return(NULL)
             } else{
               result <- filtered |> group_by(Event_ID, ACTIVITY_TYPE) |>
                 summarize(
                   gross_area = sum(st_area(geometry)),
                   Ig_Year = x,
                   diff_years = y
                 )
               result$ref_year <-
                 result$Ig_Year + result$diff_years
               return(result)
             }
           }, assigned_activities = assigned_activities)


#() net_over_gross <- merge

# Convert sf object to data frame
gross_activities_df = st_drop_geometry(gross_activities)
net_activities_df = st_drop_geometry(net_activities)
assigned_df = st_drop_geometry(assigned_activities)
facts_df = st_drop_geometry(facts_fires$fires_activities)

# drop units
units(net_activities_df$net_area) <- NULL
units(gross_activities_df$gross_area) <- NULL
units(assigned_df$activity_area) <- NULL


gross_by_fire = gross_activities_df %>%
  group_by(Event_ID,ref_year) %>%
  summarize(total_treated_gross = sum(gross_area))
net_by_fire = net_activities_df %>%
  group_by(Event_ID,ref_year) %>%
  summarize(total_treated_net = sum(net_area))
total_by_fire = merge(gross_by_fire, net_by_fire, by = "Event_ID")
total_by_fire = total_by_fire %>%
  mutate(diff = total_treated_gross - total_treated_net)


acres_burned_by_fire = assigned_df %>%
  group_by(Ig_Year, Event_ID) %>%
  distinct(Event_ID, .keep_all = TRUE) %>%
  summarize(acres_burned = sum(sumBurnBndAc, na.rm = TRUE))
acres_burned_by_year = acres_burned_by_fire %>%
  group_by(Ig_Year) %>%
  summarize(acres_burned = sum(acres_burned, na.rm = TRUE))






#### Summarize Treatment Area by Category ####

# Group by ig_year and summarize acres
gross_10yr_by_ig_year = gross_activities_df %>%
  filter(diff_years <= 10) %>%
  group_by(Ig_Year, ACTIVITY_TYPE) %>%
  summarize(gross_acres = as.numeric(sum(gross_area)/4046.86), .groups = 'drop') %>%
  spread(key = ACTIVITY_TYPE, value = gross_acres)
gross_5yr_by_ig_year = gross_activities_df %>%
  filter(diff_years <= 5) %>%
  group_by(Ig_Year, ACTIVITY_TYPE) %>%
  summarize(gross_acres = as.numeric(sum(gross_area)/4046.86), .groups = 'drop') %>%
  spread(key = ACTIVITY_TYPE, value = gross_acres)
net_10yr_by_ig_year = net_activities_df %>%
  filter(diff_years <= 10) %>%
  group_by(Ig_Year, ACTIVITY_TYPE) %>%
  summarize(net_acres = as.numeric(sum(net_area)/4046.86), .groups = 'drop') %>%
  spread(key = ACTIVITY_TYPE, value = net_acres)
net_5yr_by_ig_year = net_activities_df %>%
  filter(diff_years <= 5) %>%
  group_by(Ig_Year, ACTIVITY_TYPE) %>%
  summarize(net_acres = as.numeric(sum(net_area)/4046.86), .groups = 'drop') %>%
  spread(key = ACTIVITY_TYPE, value = net_acres)

net_planting_ig_year = net_activities %>%
  filter(ACTIVITY_TYPE == "planting") %>%
  group_by(Ig_Year, ACTIVITY_TYPE) %>%
  summarize(net_acres = as.numeric(sum(net_area)/4046.86), .groups = 'drop') %>%
  spread(key = ACTIVITY_TYPE, value = net_acres)


# Filter the data frames for the specified columns
# net_10yr_by_ig_year_filter1 = net_10yr_by_ig_year[, c("Ig_Year", "planting", "release", "replant", "thin")]
# gross_10yr_by_ig_year_filter1 = gross_10yr_by_ig_year[, c("Ig_Year", "planting", "release", "replant", "thin")]
# net_5yr_by_ig_year_filter1 = net_5yr_by_ig_year[, c("Ig_Year", "planting", "release", "replant", "thin")]
# gross_5yr_by_ig_year_filter1 = gross_5yr_by_ig_year[, c("Ig_Year", "planting", "release", "replant", "thin")]

net_10yr_by_ig_year_filter1 = net_10yr_by_ig_year
gross_10yr_by_ig_year_filter1 = gross_10yr_by_ig_year
net_5yr_by_ig_year_filter1 = net_5yr_by_ig_year
gross_5yr_by_ig_year_filter1 = gross_5yr_by_ig_year

# Convert the data frames from wide to long format
net_10yr_by_ig_year_filter1_long = tidyr::gather(net_10yr_by_ig_year_filter1, ACTIVITY_TYPE, value, -Ig_Year)
gross_10yr_by_ig_year_filter1_long = tidyr::gather(gross_10yr_by_ig_year_filter1, ACTIVITY_TYPE, value, -Ig_Year)
net_5yr_by_ig_year_filter1_long = tidyr::gather(net_5yr_by_ig_year_filter1, ACTIVITY_TYPE, value, -Ig_Year)
gross_5yr_by_ig_year_filter1_long = tidyr::gather(gross_5yr_by_ig_year_filter1, ACTIVITY_TYPE, value, -Ig_Year)

# Add a new column to identify the type of acres
net_10yr_by_ig_year_filter1_long$type = "net_acres_10yr"
gross_10yr_by_ig_year_filter1_long$type = "gross_acres_10yr"
net_5yr_by_ig_year_filter1_long$type = "net_acres_5yr"
gross_5yr_by_ig_year_filter1_long$type = "gross_acres_5yr"

# Combine the data frames
combined_10yr_ig_year_filter1 = rbind(net_10yr_by_ig_year_filter1_long, gross_10yr_by_ig_year_filter1_long)
combined_5yr_ig_year_filter1 = rbind(net_5yr_by_ig_year_filter1_long, gross_5yr_by_ig_year_filter1_long)
combined_5yr_ig_year_filter1_long = rbind(net_5yr_by_ig_year_filter1_long, gross_5yr_by_ig_year_filter1_long)
# Spread the data frame to have gross_acres and net_acres in separate columns
combined_10yr_ig_year_filter1 = spread(combined_10yr_ig_year_filter1, key = type, value = value)
combined_5yr_ig_year_filter1 = spread(combined_5yr_ig_year_filter1, key = type, value = value)

# Add a new column for the difference between gross and net acres
combined_10yr_ig_year_filter1$difference = combined_10yr_ig_year_filter1$gross_acres_10yr - combined_10yr_ig_year_filter1$net_acres_10yr
combined_5yr_ig_year_filter1$difference = combined_5yr_ig_year_filter1$gross_acres_5yr - combined_5yr_ig_year_filter1$net_acres_5yr


# Plot difference
ggplot(combined_10yr_ig_year_filter1, aes(x = Ig_Year)) +
  geom_line(aes(y = difference, color = ACTIVITY_TYPE)) +
  labs(x = "Ignition Year", y = "Difference (Gross Acres - Net Acres)", color = "Activity Type") +
  scale_x_continuous(breaks = seq(1993, 2017, 3)) +
  facet_wrap(~ ACTIVITY_TYPE) +
  theme_classic()
ggplot(combined_5yr_ig_year_filter1, aes(x = Ig_Year)) +
  geom_line(aes(y = difference, color = ACTIVITY_TYPE)) +
  labs(x = "Ignition Year", y = "Difference (Gross Acres - Net Acres)", color = "Activity Type") +
  scale_x_continuous(breaks = seq(1993, 2017, 3)) +
  facet_wrap(~ ACTIVITY_TYPE) +
  theme_classic()


ggplot(combined_5yr_ig_year_filter1, aes(x = Ig_Year)) +
  geom_line(aes(y = gross_acres_5yr, color = "Gross Acres")) +
  geom_line(aes(y = net_acres_5yr, color = "Net Acres")) +
  ggtitle("Acres Treated in R5 within 5 Years of Fire") +
  labs(x = "Ignition Year", y = "Treatment Acres", color = "Type of Acres") +
  scale_x_continuous(breaks = seq(1992, 2020, 6)) +
  facet_wrap(~ ACTIVITY_TYPE, ncol = 3) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
  )

ggplot(combined_10yr_ig_year_filter1, aes(x = Ig_Year)) +
  geom_line(aes(y = gross_acres_10yr, color = "Gross Acres")) +
  geom_line(aes(y = net_acres_10yr, color = "Net Acres")) +
  ggtitle("Acres Treated in R5 within 10 Years of Fire") +
  labs(x = "Ignition Year", y = "Treatment Acres", color = "Type of Acres") +
  scale_x_continuous(breaks = seq(1992, 2020, 6)) +
  facet_wrap(~ ACTIVITY_TYPE, ncol = 3) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
  )


# Summarize net cert by ig_year
net_cert_by_igyear = net_activities_df %>%
  filter(ACTIVITY_TYPE == "cert_planted") %>%
  group_by(Ig_Year) %>%
  summarize(net_acres = as.numeric(sum(net_area)/4046.86), .groups = 'drop')

net_planting_by_igyear = net_activities_df %>%
  filter(ACTIVITY_TYPE == "planting") %>%
  group_by(Ig_Year) %>%
  summarize(net_acres = as.numeric(sum(net_area)/4046.86), .groups = 'drop')


net_plant_cert_fire = full_join(net_planting_by_igyear, net_cert_by_igyear, by = "Ig_Year") %>%
  full_join(acres_burned_by_year, by = "Ig_Year")
net_plant_cert_fire = rename(net_plant_cert_fire, "planting_acres" = net_acres.x, "cert_acres" = net_acres.y)

# Convert the data frames from wide to long format
net_plant_cert_fire = tidyr::gather(net_plant_cert_fire, type, net_acres, -Ig_Year)
  

# Plot net cert by ig_year
ggplot(net_cert_by_igyear, aes(x = Ig_Year)) +
  ggtitle("R5 Net Postfire Planting Acres Certified by Ignition Year, 1994 - 2018") +
  geom_bar(aes(y = net_acres), stat = "identity") +
  labs(x = "Ignition Year", y = "Net Acres") +
  scale_x_continuous(breaks = seq(1990, 2020, 4)) +
  theme_bw()+
  theme(legend.position="bottom", legend.box = "horizontal")

# Modify the type variable labels
net_plant_cert_fire$type <- recode(net_plant_cert_fire$type,
                                   acres_burned = "Burned",
                                   cert_acres = "Net Certified-Planted",
                                   planting_acres = "Net Planted")
net_plant_cert_fire$type = factor(net_plant_cert_fire$type,
                                  levels = c("Net Certified-Planted", "Net Planted","Burned"))

# Plot net planting + cert by ig_year
ggplot(net_plant_cert_fire, aes(x = Ig_Year, y = net_acres, fill = type)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(type), scales = "free_y") +
  ggtitle("R5 Burned, Net Planted, and Net Certified-Planted Acres by Ignition Year" , ) +
  labs(x = "Ignition Year", y = "Acres") +
  scale_x_continuous(breaks = seq(1990, 2020, 4)) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("Net Certified-Planted" = "blue", "Net Planted" = "green", "Burned" = "red")) +
  theme_bw() +
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(plot.title = element_text(size=12)) +
  guides(fill=guide_legend(title=NULL)) # Remove legend title
