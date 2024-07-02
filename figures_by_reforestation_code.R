assigned_activities = readRDS("assigned_activities_2022.RDS")

assigned_activities_filtered = assigned_activities %>%
  filter(diff_years >= 0)

# Modify the net_activities and gross_activities calculations to include REFORESTAT and year
net_activities_reforestation <- map2_dfr(
  comb_fire_diff$fire_year,
  comb_fire_diff$diff_years,
  function(x,y,assigned_activities){
    filtered <- assigned_activities |> 
      filter(Ig_Year ==x & diff_years==y)
    
    if(dim(filtered)[1]==0){
      return(NULL)
    }else{
      result <- filtered |> group_by(Event_ID,ACTIVITY_TYPE,REFORESTAT) |> 
        summarize(geometry=st_union(geometry),n_dissolved=n(),
                  Ig_Year=first(Ig_Year),diff_years=first(diff_years),
                  REFORESTAT=first(REFORESTAT))
      result$ref_year <-result$Ig_Year+result$diff_years
      result$net_area <- st_area(result)
      
      # Summarize net area by REFORESTAT in separate columns
      # result <- result |> pivot_wider(names_from = REFORESTAT, values_from = net_area, values_fill = 0)
      
      return(result)
    }
  },assigned_activities=assigned_activities)

gross_activities_reforestation <- map2_dfr(
  comb_fire_diff$fire_year,
  comb_fire_diff$diff_years,
  function(x,y,assigned_activities){
    filtered <- assigned_activities |> 
      filter(Ig_Year ==x & diff_years==y)
    if(dim(filtered)[1]==0){
      return(NULL)
    }else{
      result <-filtered |> group_by(Event_ID,ACTIVITY_TYPE,REFORESTAT)|> 
        summarize(gross_area=sum(st_area(geometry)),
                  Ig_Year=first(Ig_Year),diff_years=first(diff_years))
      result$ref_year <-result$Ig_Year+result$diff_years
      return(result)
    }
  },assigned_activities=assigned_activities)


#### Summarize Treatment Area by reforestation Class ####

# Convert sf object to data frame
gross_activities_refor_df = st_drop_geometry(gross_activities_reforestation)
net_activities_refor_df = st_drop_geometry(net_activities_reforestation)

# Group by igyear and summarize acres
net_refor_igyear = net_activities_refor_df %>%
  group_by(Ig_Year, REFORESTAT) %>%
  summarize(net_acres = as.numeric(sum(net_area)/4046.86), .groups = 'drop')
gross_refor_5yr = gross_activities_refor_df %>%
  filter(diff_years > 5) %>%
  group_by(Ig_Year, REFORESTAT) %>%
  summarize(gross_acres = as.numeric(sum(gross_area)/4046.86), .groups = 'drop')
net_refor_5yr = net_activities_refor_df %>%
  filter(diff_years > 5) %>%
  group_by(Ig_Year, REFORESTAT) %>%
  summarize(net_acres = as.numeric(sum(net_area)/4046.86), .groups = 'drop')

# Group by ref_year and summarize acres
net_refor_refyear = net_activities_refor_df %>%
  group_by(ref_year, REFORESTAT) %>%
  summarize(net_acres = as.numeric(sum(net_area)/4046.86), .groups = 'drop')
gross_refor_refyear_5yr = gross_activities_refor_df %>%
  filter(diff_years > 5) %>%
  group_by(ref_year, REFORESTAT) %>%
  summarize(gross_acres = as.numeric(sum(gross_area)/4046.86), .groups = 'drop')
net_refor_refyear_5yr = net_activities_refor_df %>%
  filter(diff_years > 5) %>%
  group_by(ref_year, REFORESTAT) %>%
  summarize(net_acres = as.numeric(sum(net_area)/4046.86), .groups = 'drop')


# Filter the data frames for the specified columns
net_refor_igyear_filter1 = net_refor_igyear
net_refor_refyear_filter1 = net_refor_refyear

# net_by_prod_ref_year_filter1 = net_by_prod_ref_year[, c("ref_year", "planting", "release", "replant", "thin")]
# gross_by_prod_ref_year_filter1 = gross_by_prod_ref_year[, c("ref_year", "planting", "release", "replant", "thin")]

# Convert the data frames from wide to long format
## in this case, they're already long
# net_by_prod_ref_year_filter1_long = tidyr::gather(net_by_prod_ref_year_filter1, net_planting_acres, value, -ref_year)
# gross_by_prod_ref_year_filter1_long = tidyr::gather(gross_by_prod_ref_year_filter1, gross_planting_acres, value, -ref_year)

# Add a new column to identify the type of acres and rename acreage columns
# net_by_prod_ref_year_filter1_long$type = "net_planting_acres"
# gross_by_prod_ref_year_filter1_long$type = "gross_planting_acres"
net_refor_filter1$type = "net_acres"
gross_by_refor_filter1$type = "gross_acres"

colnames(net_by_refor_filter1)[colnames(net_by_refor_filter1)=="net_acres"] <- "value"
colnames(gross_by_refor_filter1)[colnames(gross_by_refor_filter1)=="gross_acres"] <- "value"

# Combine the data frames
# combined_prod_ref_year_filter1 = rbind(net_by_prod_ref_year_filter1_long, gross_by_prod_ref_year_filter1_long)
combined_by_refor_filter1 = rbind(net_by_refor_filter1, gross_by_refor_filter1)

# Spread the data frame to have gross_acres and net_acres in separate columns
# combined_by_refor_filter1 = spread(combined_by_refor_filter1, key = type, value = value)

# Add a new column for the difference between gross and net acres
combined_by_refor_filter1$difference = combined_by_refor_filter1$gross_acres - 
  combined_by_refor_filter1$net_acres

combined_by_refor_filter1 = combined_by_refor_filter1 %>%
  drop_na()

net_refor_igyear_filter1 = net_refor_igyear_filter1 %>% 
  drop_na()
net_refor_refyear_filter1 = net_refor_refyear_filter1 %>% 
  drop_na()

# Summarize net planting cert by ig_year
net_cert_by_refor_igyear = net_activities_reforestation %>%
  filter(ACTIVITY_TYPE == "cert_planted") %>%
  group_by(Ig_Year, REFORESTAT) %>%
  summarize(net_acres = as.numeric(sum(net_area)/4046.86), .groups = 'drop')

net_cert_by_refor_igyear = net_cert_by_refor_igyear %>% 
  drop_na()
net_cert_by_refor_refyear = net_cert_by_refor_refyear %>% 
  drop_na()



# Plot difference
ggplot(combined_by_refor_filter1, aes(x = ref_year)) +
  geom_line(aes(y = difference, color = ACTIVITY_TYPE)) +
  labs(x = "Activity Year", y = "Difference (Gross Acres - Net Acres)", color = "Activity Type") +
  scale_x_continuous(breaks = seq(1992, 2022, 4)) +
  facet_wrap(~ ACTIVITY_TYPE, ncol = 2) +
  theme_classic() +
  theme(legend.position="bottom", legend.box = "horizontal")

# Plot gross and net with facets
ggplot(combined_by_refor_filter1, aes(x = ref_year)) +
  geom_line(aes(y = gross_acres, color = "Gross Acres")) +
  geom_line(aes(y = net_acres, color = "Net Acres")) +
  ggtitle("Gross and Net Acres Treated by Reforestation Class in R5") +
  labs(x = "Activity Year", y = "Treatment Acres", color = "Type of Acres") +
  scale_x_continuous(breaks = seq(1992, 2023, 4)) +
  facet_wrap(~ REFORESTAT, ncol = 2) +
  theme_bw()+
  theme(legend.position="bottom", legend.box = "horizontal")

# Plot gross and net with facets
ggplot(combined_by_refor_filter1) +
  ggtitle("R5 Net Postfire Treatment Acres by Reforestation Status, 1994 - 2023") +
  geom_bar(aes(x = REFORESTAT, y = value), stat = "identity") +
  labs(x = "Reforestation Status", y = "Net Acres") +
  scale_fill_binned() +
  theme_bw()+
  theme(legend.position="bottom", legend.box = "horizontal")

# Plot net treatment acres by igyear
ggplot(net_refor_igyear_filter1) +
  ggtitle("R5 Net Postfire Treatment Acres by Reforestation Status and Ignition Year, 1994 - 2018") +
  geom_bar(aes(x = Ig_Year, y = net_acres, fill = REFORESTAT), stat = "identity") +
  facet_wrap(~REFORESTAT,ncol = 2) +
  labs(x = "Ignition Year", y = "Net Acres") +
  scale_x_continuous(breaks=seq(1990,2020,4)) +
  theme_bw()+
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(plot.title = element_text(size=12)) +
  guides(fill=guide_legend(title="Reforestation Status"))

# Plot net treatment acres by refyear
ggplot(net_refor_refyear_filter1) +
  ggtitle("R5 Net Postfire Treatment Acres by Reforestation Status and Activity Year, 1994 - 2023") +
  geom_bar(aes(x = ref_year, y = net_acres, fill = REFORESTAT), stat = "identity") +
  facet_wrap(~REFORESTAT,ncol = 2) +
  labs(x = "Activity Year", y = "Net Acres") +
  scale_x_continuous(breaks=seq(1990,2024,4)) +
  theme_bw()+
  theme(legend.position="bottom", legend.box = "horizontal") +
  theme(plot.title = element_text(size=12)) +
  guides(fill=guide_legend(title="Reforestation Status"))

# Plot net by reforestation status
ggplot(net_cert_by_refor_igyear) +
  ggtitle("R5 Net Postfire Acres Planted by Reforestation Status, 1994 - 2023") +
  aes(x = cut(Ig_Year, breaks = seq(min(Ig_Year), max(Ig_Year)+5, by = 5), include.lowest = TRUE, 
              labels = paste(seq(min(Ig_Year), max(Ig_Year), by = 5), seq(min(Ig_Year)+4, max(Ig_Year)+4, by = 5), sep=" - ")), 
      y=REFORESTAT, fill=net_acres) + 
  geom_tile(stat = "identity", height=1,width=1,color="gray") + 
  scale_fill_gradient("Net Acres", low = "lightgray", high = "black") +
  scale_x_discrete() +
  xlab("Ignition Year") +
  ylab("Reforestation Status") +
  theme_bw()





