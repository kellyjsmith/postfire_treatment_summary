

# Modify the net_activities and gross_activities calculations to include PRODUCTIVI and year
net_activities_productivity <- map2_dfr(comb_fire_diff$fire_year,comb_fire_diff$diff_years,
                              function(x,y,assigned_activities){
                                
                                filtered <- assigned_activities |> 
                                  filter(Ig_Year ==x & diff_years==y)
                                
                                if(dim(filtered)[1]==0){
                                  return(NULL)
                                }else{
                                  result <- filtered |> group_by(VB_ID,ACTIVITY_TYPE,PRODUCTIVI) |> 
                                    summarize(geometry=st_union(geometry),n_dissolved=n(),
                                              Ig_Year=first(Ig_Year),diff_years=first(diff_years),
                                              PRODUCTIVI=first(PRODUCTIVI))
                                  result$ref_year <-result$Ig_Year+result$diff_years
                                  result$net_area <- st_area(result)
                                  
                                  # Summarize net area by PRODUCTIVI in separate columns
                                  # result <- result |> pivot_wider(names_from = PRODUCTIVI, values_from = net_area, values_fill = 0)
                                  
                                  return(result)
                                }
                              },assigned_activities=assigned_activities)

gross_activities_productivity <- map2_dfr(comb_fire_diff$fire_year,comb_fire_diff$diff_years,
                             function(x,y,assigned_activities){
                               filtered <- assigned_activities |> 
                                 filter(Ig_Year ==x & diff_years==y)
                               if(dim(filtered)[1]==0){
                                 return(NULL)
                               }else{
                                 result <-filtered |> group_by(VB_ID,ACTIVITY_TYPE,PRODUCTIVI)|> 
                                   summarize(gross_area=sum(st_area(geometry)),
                                             Ig_Year=first(Ig_Year),diff_years=first(diff_years))
                                 result$ref_year <-result$Ig_Year+result$diff_years
                                 return(result)
                               }
                             },assigned_activities=assigned_activities)


#### Summarize Treatment Area by Productivity Class ####

# Convert sf object to data frame
gross_activities_prod_df = st_drop_geometry(gross_activities_productivity)
net_activities_prod_df = st_drop_geometry(net_activities_productivity)

# Group by ref_year and summarize acres
gross_planting_by_productivity = gross_activities_prod_df %>%
  filter(ACTIVITY_TYPE == "planting") %>%
  group_by(ref_year, PRODUCTIVI) %>%
  summarize(gross_planting_acres = as.numeric(sum(gross_area)/4046.86), .groups = 'drop')
net_planting_by_productivity = net_activities_prod_df %>%
  filter(ACTIVITY_TYPE == "planting") %>%
  group_by(ref_year, PRODUCTIVI) %>%
  summarize(net_planting_acres = as.numeric(sum(net_area)/4046.86), .groups = 'drop')


# Filter the data frames for the specified columns
net_by_prod_ref_year_filter1 = net_planting_by_productivity
gross_by_prod_ref_year_filter1 = gross_planting_by_productivity

# net_by_prod_ref_year_filter1 = net_by_prod_ref_year[, c("ref_year", "planting", "release", "replant", "thin")]
# gross_by_prod_ref_year_filter1 = gross_by_prod_ref_year[, c("ref_year", "planting", "release", "replant", "thin")]

# Convert the data frames from wide to long format
## in this case, they're already long
# net_by_prod_ref_year_filter1_long = tidyr::gather(net_by_prod_ref_year_filter1, net_planting_acres, value, -ref_year)
# gross_by_prod_ref_year_filter1_long = tidyr::gather(gross_by_prod_ref_year_filter1, gross_planting_acres, value, -ref_year)

# Add a new column to identify the type of acres and rename acreage columns
# net_by_prod_ref_year_filter1_long$type = "net_planting_acres"
# gross_by_prod_ref_year_filter1_long$type = "gross_planting_acres"
net_by_prod_ref_year_filter1$type = "net_planting_acres"
gross_by_prod_ref_year_filter1$type = "gross_planting_acres"

colnames(net_by_prod_ref_year_filter1)[colnames(net_by_prod_ref_year_filter1)=="net_planting_acres"] <- "value"
colnames(gross_by_prod_ref_year_filter1)[colnames(gross_by_prod_ref_year_filter1)=="gross_planting_acres"] <- "value"

# Combine the data frames
# combined_prod_ref_year_filter1 = rbind(net_by_prod_ref_year_filter1_long, gross_by_prod_ref_year_filter1_long)
combined_prod_ref_year_filter1 = rbind(net_by_prod_ref_year_filter1, gross_by_prod_ref_year_filter1)

# Spread the data frame to have gross_acres and net_acres in separate columns
combined_prod_ref_year_filter1 = spread(combined_prod_ref_year_filter1, key = type, value = value)

# Add a new column for the difference between gross and net acres
combined_prod_ref_year_filter1$difference = combined_prod_ref_year_filter1$gross_planting_acres - 
  combined_prod_ref_year_filter1$net_planting_acres

# Plot difference
ggplot(combined_prod_ref_year_filter1, aes(x = ref_year)) +
  geom_line(aes(y = difference, color = ACTIVITY_TYPE)) +
  labs(x = "Activity Year", y = "Difference (Gross Acres - Net Acres)", color = "Activity Type") +
  scale_x_continuous(breaks = seq(1992, 2022, 4)) +
  facet_wrap(~ ACTIVITY_TYPE, ncol = 2) +
  theme_classic() +
  theme(legend.position="bottom", legend.box = "horizontal")

# Plot gross and net with facets
ggplot(combined_prod_ref_year_filter1, aes(x = ref_year)) +
  geom_line(aes(y = gross_planting_acres, color = "Gross Planting Acres")) +
  geom_line(aes(y = net_planting_acres, color = "Net Planting Acres")) +
  ggtitle("Gross and Net Acres Planted by Productivity Class in R5") +
  labs(x = "Activity Year", y = "Treatment Acres", color = "Type of Acres") +
  scale_x_continuous(breaks = seq(1992, 2023, 4)) +
  facet_wrap(~ PRODUCTIVI, ncol = 2) +
  theme_bw()+
  theme(legend.position="bottom", legend.box = "horizontal")

# Plot gross and net with facets
ggplot(combined_prod_ref_year_filter1) +
  ggtitle("Net Postfire Acres Planted in R5 by Productivity Class, 1993 - 2022") +
  geom_bar(aes(x = PRODUCTIVI, y = net_planting_acres), stat = "identity") +
  labs(x = "Productivity Class", y = "Net Planted Acres") +
  theme_bw()+
  theme(legend.position="bottom", legend.box = "horizontal")


