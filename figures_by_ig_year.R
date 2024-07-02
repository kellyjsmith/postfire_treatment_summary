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
units(assigned_df$activity_fire_area) <- NULL

# create acres field
net_activities_df = net_activities_df %>%
  mutate(net_acres = net_area/4046.86)
gross_activities_df = gross_activities_df %>%
  mutate(gross_acres = gross_area/4046.86)
assigned_df = assigned_df %>%
  mutate(activity_fire_acres = activity_fire_area/4046.86)


# filter activities
assigned_planting = assigned_df %>%
  filter(ACTIVITY_TYPE == "planting")

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
gross_by_ig_year = gross_activities_df %>%
  group_by(Ig_Year, ACTIVITY_TYPE) %>%
  summarize(gross_acres = sum(gross_acres), .groups = 'drop') %>%
  spread(key = ACTIVITY_TYPE, value = gross_acres)

net_by_ig_year = net_activities_df %>%
  group_by(Ig_Year, ACTIVITY_TYPE) %>%
  summarize(net_acres = sum(net_acres), .groups = 'drop') %>%
  spread(key = ACTIVITY_TYPE, value = net_acres)

gross_5yr_by_ig_year = gross_activities_df %>%
  filter(diff_years <= 5) %>%
  group_by(Ig_Year, ACTIVITY_TYPE) %>%
  summarize(gross_acres = sum(gross_acres), .groups = 'drop') %>%
  spread(key = ACTIVITY_TYPE, value = gross_acres)

net_5yr_by_ig_year = net_activities_df %>%
  filter(diff_years <= 5) %>%
  group_by(Ig_Year, ACTIVITY_TYPE) %>%
  summarize(net_acres = sum(net_acres), .groups = 'drop') %>%
  spread(key = ACTIVITY_TYPE, value = net_acres)

# Convert the data frames from wide to long format
gross_by_ig_year_long = tidyr::gather(gross_by_ig_year, ACTIVITY_TYPE, value, -Ig_Year)
net_by_ig_year_long = tidyr::gather(net_by_ig_year, ACTIVITY_TYPE, value, -Ig_Year)
gross_5yr_by_ig_year_long = tidyr::gather(gross_5yr_by_ig_year, ACTIVITY_TYPE, value, -Ig_Year)
net_5yr_by_ig_year_long = tidyr::gather(net_5yr_by_ig_year, ACTIVITY_TYPE, value, -Ig_Year)

# Add a new column to identify the type of acres
gross_by_ig_year_long$type = "gross_acres"
net_by_ig_year_long$type = "net_acres"
gross_5yr_by_ig_year_long$type = "gross_acres_5yr"
net_5yr_by_ig_year_long$type = "net_acres_5yr"

# Combine the data frames
combined_by_ig_year = rbind(gross_by_ig_year_long, net_by_ig_year_long)
combined_5yr_by_ig_year = rbind(gross_5yr_by_ig_year_long, net_5yr_by_ig_year_long)

# Spread the data frame to have gross_acres and net_acres in separate columns
combined_by_ig_year = spread(combined_by_ig_year, key = type, value = value)
combined_5yr_by_ig_year = spread(combined_5yr_by_ig_year, key = type, value = value)

# Add a new column for the difference between gross and net acres
combined_by_ig_year$difference = combined_by_ig_year$gross_acres - combined_by_ig_year$net_acres
combined_5yr_by_ig_year$difference = combined_5yr_by_ig_year$gross_acres_5yr - combined_5yr_by_ig_year$net_acres_5yr

# Create a new variable that shows when gross_acres and net_acres are equal (or within 1 sq m)
combined_by_ig_year$equal_acres <- ifelse(
  abs(combined_by_ig_year$gross_acres - combined_by_ig_year$net_acres) <= 
    0.0075 * combined_by_ig_year$net_acres, combined_by_ig_year$gross_acres, NA)
combined_5yr_by_ig_year$equal_acres <- ifelse(
  abs(combined_5yr_by_ig_year$gross_acres_5yr - combined_5yr_by_ig_year$net_acres_5yr) <= 
    0.0075 * combined_5yr_by_ig_year$net_acres_5yr, combined_5yr_by_ig_year$gross_acres_5yr, NA)

combined_by_ig_year = combined_by_ig_year %>%
  drop_na(gross_acres)

# Plot Gross and Net
ggplot(combined_5yr_by_ig_year, aes(x = Ig_Year)) +
  geom_line(aes(y = net_acres_5yr, color = "Net Acres"), size = 1.25) +
  geom_line(aes(y = gross_acres_5yr, color = "Gross Acres"), size = 1.25) +
  # geom_line(aes(y = equal_acres, color = "Gross = Net (~1 m^2)"), size = 1.25) +
  ggtitle("R5 Reforestation Activities Completed within 5 Years, by Ignition Year") +
  labs(x = "Ignition Year", y = "Treatment Acres") +
  scale_x_continuous(breaks = seq(1992, 2020, 6)) +
  facet_wrap(~ ACTIVITY_TYPE, ncol = 3) +
  scale_color_manual(values = c("Gross Acres" = "blue", "Net Acres" = "red", "Gross = Net (~1 m^2)" = "green")) +
  theme_bw() +
  theme(legend.position="bottom", legend.box = "horizontal", plot.title = element_text(size=12)) +
  guides(color=guide_legend(title=NULL))


ggplot(combined_by_ig_year, aes(x = Ig_Year)) +
  geom_line(aes(y = net_acres, color = "Net Acres"), size = 1.25) +
  geom_line(aes(y = gross_acres, color = "Gross Acres"), size = 1.25) +
  geom_line(aes(y = equal_acres, color = "Gross = Net (~1 m^2)"), size = 1.25) +
  ggtitle("R5 Reforestation Activities Completed to Date, by Ignition Year") +
  labs(x = "Ignition Year", y = "Treatment Acres") +
  scale_x_continuous(breaks = seq(1992, 2020, 6)) +
  facet_wrap(~ ACTIVITY_TYPE, ncol = 3) +
  scale_color_manual(values = c("Gross Acres" = "blue", "Net Acres" = "red", "Gross = Net (~1 m^2)" = "green")) +
  theme_bw() +
  theme(legend.position="bottom", legend.box = "horizontal", plot.title = element_text(size=12)) +
  guides(color=guide_legend(title=NULL))


# Summarize net cert and planting by ig_year ####
net_cert_by_igyear = net_activities_df %>%
  filter(ACTIVITY_TYPE == "cert_planted") %>%
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

ggplot(net_cert_by_igyear, aes(x=Ig_Year,y=net_acres)) +
  stat_ecdf(geom = "line")


# Summarize planting by ig year ####
net_planting_by_igyear = net_activities_df %>%
  filter(ACTIVITY_TYPE == "planting") %>%
  group_by(Ig_Year) %>%
  summarize(net_acres = sum(net_area)/4046.86,.groups = 'drop')
net_planting_diff_by_igyear = net_activities_df %>%
  filter(ACTIVITY_TYPE == "planting") %>%
  group_by(Ig_Year) %>%
  summarize(mean_diff = mean(diff_years),.groups = 'drop')

planting_by_igyear = assigned_df %>%
  filter(ACTIVITY_TYPE == "planting") %>%
  group_by(Ig_Year) %>%
  summarize(net_acres = sum(activity_fire_acres),.groups = 'drop')
planting_diff_by_fire = assigned_df %>%
  filter(ACTIVITY_TYPE == "planting") %>%
  group_by(Ig_Year, Event_ID) %>%
  summarize(min_diff = min(diff_years), mean_diff = mean(diff_years),.groups = 'drop')
planting_diff_by_igyear = planting_diff_by_fire %>%
  group_by(Ig_Year) %>%
  summarize(avg_min_diff = mean(min_diff))

planting_acres_diff_igyear = merge(planting_by_igyear,planting_diff_by_igyear, by = "Ig_Year")

ggplot(planting_acres_diff_igyear, aes(x = Ig_Year)) +
  geom_line(aes(y = net_acres)) +
  geom_bar(aes(y = mean_diff), stat = "identity") +
  labs(title = "R5 Net Postfire Acres Planted and Mean Years Until Planting",
       x = "Ignition Year", y = "Net Acres") +
  theme_bw()
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Mean Years Until Planting"))
  
ggplot(planting_acres_diff_igyear, aes(x = Ig_Year)) +
  geom_line(aes(y = net_acres), color = "blue") +
  geom_bar(aes(y = avg_min_diff/1000), stat = "identity", fill = "red") +  # Scale mean_diff by 1000 for visibility
  scale_y_continuous(name = "Net Acres", sec.axis = sec_axis(~.*1000, name="Mean Years Until Planting")) +
  labs(title = "R5 Net Postfire Acres Planted and Mean Years Until Planting",
       x = "Ignition Year") +
  theme_bw()

# Create a new variable for binned data
planting_acres_diff_igyear <- planting_acres_diff_igyear %>%
  mutate(avg_min_diff_binned = cut_number(avg_min_diff, n = 5, labels = paste(1:5)))

# Plot
ggplot(planting_acres_diff_igyear, aes(x = Ig_Year, y = net_acres, fill = avg_min_diff_binned)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colorRampPalette(c("lightgreen", "darkgreen"))(5)) +
  scale_x_continuous(breaks = seq(1990,2020,4)) +
  labs(title = "Net Postfire Acres Planted and Mean Years Until Planting",
       x = "Ignition Year", y = "Net Acres Planted" , fill = "mean Years Until First Planting") +
  theme_bw() +
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(plot.title = element_text(size=12)) +
  guides(fill=guide_legend(title="Years Until First Planting"))
