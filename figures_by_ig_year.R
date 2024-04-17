## Tables & Figures for summarizing postfire activities

library("sf")
library("dplyr")
library("terra")
library("tidyverse")
library("units")

setwd("C:/Users/smithke3/OneDrive - Oregon State University/Kelly/Output")

assigned_activities = readRDS("assigned_activities_2022.RDS")
facts_fires = readRDS("facts_fires_2022.RDS")


comb_fire_diff <- expand.grid(fire_year = unique(assigned_activities$Ig_Year),
                              diff_years =unique(assigned_activities$diff_years))

net_activities <-
  map2_dfr(comb_fire_diff$fire_year, comb_fire_diff$diff_years,
           function(x, y, assigned_activities) {
             filtered <- assigned_activities |>
               filter(Ig_Year == x & diff_years == y)
             
             if (dim(filtered)[1] == 0) {
               return(NULL)
             } else{
               result <- filtered |> group_by(Event_ID, ACTIVITY_TYPE) |>
                 summarize(
                   geometry = st_union(geometry),
                   n_dissolved = n(),
                   Ig_Year = first(Ig_Year),
                   diff_years = first(diff_years)
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
                        diff_years == y)
             
             if (dim(filtered)[1] == 0) {
               return(NULL)
             } else{
               result <- filtered |> group_by(Event_ID, ACTIVITY_TYPE) |>
                 summarize(
                   gross_area = sum(st_area(geometry)),
                   Ig_Year = first(Ig_Year),
                   diff_years = first(diff_years)
                 )
               result$ref_year <-
                 result$Ig_Year + result$diff_years
               return(result)
             }
           }, assigned_activities = assigned_activities)



# net_over_gross <- merge()

# Convert sf object to data frame
gross_activities_df = st_drop_geometry(gross_activities)
net_activities_df = st_drop_geometry(net_activities)
assigned_df = st_drop_geometry(assigned_activities)
facts = st_drop_geometry(facts_fires$fires_activities)

# drop units
units(net_activities_df$net_area) <- NULL
units(gross_activities_df$gross_area) <- NULL
units(assigned_df$activity_fire_acres) <- NULL

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

# # Assuming df is your data frame and it has two y variables y1 and y2
# df$color <- ifelse(df$y1 == df$y2, "equal", "not equal")
# 
# ggplot(df, aes(x=x)) +
#   geom_line(aes(y=y1, color=color)) +
#   geom_line(aes(y=y2, color=color)) +
#   scale_color_manual(values=c("equal"="red", "not equal"="black"))


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
