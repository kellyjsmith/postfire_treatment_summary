#
# TODO: 
# 
#
# Author: Paco Mauro & Kelly Smith
#################################################################################################


library(tidyr)
library(tidyverse)
library(plyr)
library(dplyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(sf)
library(raster)


#setwd("C:/Users/smithke3/OneDrive - Oregon State University/Kelly/")
setwd("~/Library/CloudStorage/OneDrive-OregonStateUniversity/Kelly")

#################################################################################################

# READ CSV OUTPUT FROM LAYER PRE-PROCESSING STEPS

## Postfire_Overlaps_v2 is exported from the joined overlap table, which links
## all overlapping polygons with their associated treatment records using the
## Count Overlapping Features tool in ArcGIS Pro.
## All records with the same OVERLAP_OID are co-located and represent identical areas.


overlap_table = read.csv("Data/Postfire_Overlaps.csv")
colnames(overlap_table)[1]<-"RECORD_ID"
# overlap_table<-overlap_table[overlap_table$Ig_Year<=overlap_table$FY_COMPLET, ]
treatment_table = read.csv("Data/postfire_treatments_final.csv")

pre_reclass = unique(overlap_table$ACTIVITY_N)
write.csv(pre_reclass, "Data/pre_reclass.csv")

reclass_table = read.csv("Data/reclass_table.csv")

overlap_table = merge(overlap_table,reclass_table,by="ACTIVITY_N",all=TRUE)
treatment_table = merge(treatment_table, reclass_table, by = "ACTIVITY_N", all = TRUE)

# overlap_table = overlap_table %>% 
#   filter(ACTIVITY_N != "Certification of Natural Regeneration without Site Prep")

#################################################################################################

# REMOVE DUPLICATE RECORDS FOR OVERLAPPING FIRES
# Leaves only one record (from the most recent fire) for each and OVERLAP_OID and CRC_VALUE

## In the layer pre-processing steps, duplicate records were generated when
## the FACTS layers were intersected with the MTBS layer. Each treatment record 
## retains a unique CRC_VALUE - the loop below returns the record for the most recent
## fire when there is >1 OVERLAP_OID per CRC_VALUE, which indicates a duplicate.

# split-apply-return
overlap_table_single_fires = ddply(overlap_table, c("OVERLAP_OID", "CRC_VALUE"), function(x){
  
  # if the dim of x in the grouped tibble is equal to 1, return x
  if(dim(x)[1] == 1){
    return(x)
    
  }else{
    
    # return only the most recent records when there are duplicate CRC_VALUEs
    diff_years = x$FY_COMPLET - x$Ig_Year
    x2<-x[diff_years == min(diff_years),]
    if(dim(x2)[1] == 1){
      return(x2)
      
    }else{
      
      # if overlapping fires occurred in the same year, return the biggest one
      x2[x2$Acres_within_Fire == max(x2$Acres_within_Fire),]
      x2<-x2[1,]
      return(x2)
    }
  }
})

write_csv(overlap_table_single_fires, "Output/overlap_table_single_fires.csv")

#################################################################################################

# RECLASSIFY ACTIVITY_N AND COLLAPSE OVERLAPS INTO A CONCATENATION OF TREATMENTS

## if a fire has any overlaps, assign to an activity category that is the concatenation of all present classes of ACTIVITY_N

#### this chooses the first record in the overlap. Is that what we want?
### seems like this could be done with more versatility in dplyr (arrange by year, 
## summarize by sum of overlap acres, calculate years since fire, etc.)

# overlap_table_collapsed1 = ddply(overlap_table_single_fires, c("Incid_Name","OVERLAP_OID"), function(x){
#   
#   # if the dimension of the vector is 1, return the vector
#   if(dim(x)[1] == 1){
#     x$ACTIVITY_N = x$NEW_ACTIVITY_N  
#     return(x)
#     
#   }else{
#     
#     x2<-x[1, ]   # take first row and all columns of x
#     x2$ACTIVITY_N = paste(sort(unique(x$NEW_ACTIVITY_N)), collapse = "_")
#     return(x2)
#   }})
# write.csv(overlap_table_collapsed1, "Output/overlap_table_collapsed1.csv", row.names = FALSE)
# 
# 
# ## Pivot with fires as rows; fields as Concatenated ACTIVITY_N, values from Overlap_Acres
# combined_footprint_area_by_fire = pivot_wider(overlap_table_collapsed1,
#   id_cols = c("Incid_Name","Ig_Year","BurnBndAc"), names_from="ACTIVITY_N",
#   values_from="Overlap_Acres", values_fn=sum, values_fill=0)
# write.csv(combined_footprint_area_by_fire,"Output/combined_footprint_area_by_fire.csv",row.names = FALSE)


################################################################################################

## Convert date fields
##
#overlap_table_single_fires$DATE_COMPL_ORIG <- overlap_table_single_fires$DATE_COMPL
overlap_table_single_fires$DATE_COMPL <- as.Date(overlap_table_single_fires$DATE_COMPL,tryFormats=c("%m/%d/%Y"))
treatment_table$DATE_COMPL = as.Date(treatment_table$DATE_COMPL, tryFormats = c("%m/%d/%Y"))

#overlap_table_single_fires$Ig_Date_Orig <- overlap_table_single_fires$Ig_Date
overlap_table_single_fires$Ig_Date <- as.Date(overlap_table_single_fires$Ig_Date,tryFormats=c("%m/%d/%Y"))
treatment_table$Ig_Date = as.Date(treatment_table$Ig_Date, tryFormats = c("%m/%d/%Y"))
#overlap_table_single_fires$Ig_Date = NA #remove when field is added






## Summarize overlapping treatment polygons using dplyR functions
## 
overlaps_by_fire_by_activity = overlap_table_single_fires %>%
  # select(ACTIVITY_N, RECORD_ID, OVERLAP_OID, ORIG_OID, SUID, NBR_UNITS1, DATE_COMPL, FY_COMPLET, CRC_VALUE, Incid_Name, 
  #        Ig_Date, Ig_Year, Fire_Acres_R5, Acres_within_Fire, COUNT_, Overlap_Acres, NEW_ACTIVITY_N) %>%
  group_by(Incid_Name, OVERLAP_OID) %>%
  add_count(OVERLAP_OID, name = "overlap_count") %>%
  #concatenate all overlap activity names
  mutate(ACTIVITY_N_COMBO = paste(sort(unique(NEW_ACTIVITY_N)), collapse = "_")) %>% 
  #mutate(years_since_fire = (DATE_COMPL - Ig_Date)/365) %>%
  arrange((DATE_COMPL), .by_group = TRUE) %>%
  mutate(overlap_order = row_number(DATE_COMPL)) %>%
  group_by(NEW_ACTIVITY_N, .add = TRUE) %>%
  add_count(NEW_ACTIVITY_N, name = "activity_overlap_count") %>%
  mutate(activity_overlap_order = dense_rank(FY_COMPLET)) %>%
  mutate(activity_acres_per_overlap = (activity_overlap_count * Overlap_Acres))
  ## the resulting tibble still contains multiple activities occurring in the same overlap on the same day

overlaps_by_suid = overlap_table_single_fires %>%
  group_by(Incid_Name, SUID, OVERLAP_OID) %>%
  add_count(OVERLAP_OID, name = "overlaps_in_suid") %>%
  ungroup()
  
no_overlaps_in_suid = overlaps_by_suid %>%
  filter(overlaps_in_suid == 1)

# overlaps_by_fire_by_activity$good <-overlaps_by_fire_by_activity$FY_COMPLET==year(overlaps_by_fire_by_activity$DATE_COMPL)
# check <- select(overlaps_by_fire_by_activity,good,FY_COMPLET,DATE_COMPL,DATE_COMPL2,OVERLAP_OID,Incid_Name)

## Select distinct activity when two activities occur in the same overlap on the same day
##
one_treatment_per_day = overlaps_by_fire_by_activity %>%
  group_by(DATE_COMPL, .add = TRUE) %>%
  add_count(DATE_COMPL, name = "treatments_per_day") %>%
  slice_max(Acres_within_Fire) %>%
  distinct(treatments_per_day, .keep_all = TRUE) %>%
  mutate(treatment_acres_per_day = treatments_per_day * Overlap_Acres) %>%
  ungroup()
write_csv(one_treatment_per_day, "Output/one_treatment_per_day.csv")

activity_combo_list = one_treatment_per_day %>%
  group_by(ACTIVITY_N_COMBO) %>%
  summarize(total_combo_acres = sum(Overlap_Acres))
  #distinct(sort(ACTIVITY_N_COMBO))
write_csv(activity_combo_list, "Data/activity_combo_list.csv")
  

combo_reclass_table = read.csv("Data/activity_combo_reclass.csv")
one_treatment_per_day = merge(one_treatment_per_day, combo_reclass_table, by="ACTIVITY_N_COMBO", all=TRUE)

  
# ACRES BURNED BY YEAR
acres_burned = one_treatment_per_day %>%
  group_by(Ig_Year, Incid_Name) %>%
  distinct(Incid_Name, .keep_all = TRUE) %>%
  ungroup() %>%
  summarize(annual_acres_burned = sum(Fire_Acres_R5), .by = Ig_Year)

#pivot_wider(acres_burned)


#################################################################################################






## ACTIVITY FOOTPRINT ACRES
##
one_activity_per_overlap_per_year = one_treatment_per_day %>%
  group_by(Incid_Name, FY_COMPLET, NEW_ACTIVITY_N) %>%
  mutate(activity_footprint_acres_per_year = sum(Overlap_Acres)) %>%
  filter(activity_footprint_acres_per_year<20000) %>%
  distinct(NEW_ACTIVITY_N, .keep_all = TRUE) %>%
  ungroup()


ggplot(one_activity_per_overlap_per_year, aes(x = FY_COMPLET, y = activity_footprint_acres_per_year, color = NEW_ACTIVITY_N)) +
  facet_wrap(~NEW_ACTIVITY_N, ncol = 3) +
  geom_line() +
  geom_point()


activity_footprint_by_year = pivot_wider(one_activity_per_overlap_per_year, 
  id_cols = c("FY_COMPLET"),
  names_from = "NEW_ACTIVITY_N",
  values_from = "activity_footprint_acres_per_year",
  values_fn = sum,
  values_fill = 0)
  
ggplot(activity_footprint_by_year, aes(x = FY_COMPLET, y = activity_footprint_acres_per_year,fill = NEW_ACTIVITY_N)) +
    geom_bar(stat = "identity")
    #geom_line(aes(y = ))
    #geom_line(y = activity_footprint_by_year$burned_ac)




## COMBO FOOTPRINT ACRES
## 
one_combo_per_overlap = one_treatment_per_day %>%
  group_by(Incid_Name, OVERLAP_OID) %>%
  distinct(NEW_COMBO, .keep_all = TRUE) %>%
  group_by(Incid_Name, FY_COMPLET) %>%
  reframe(overlap_combo_acres = sum(Overlap_Acres), across(c(NEW_COMBO)))


ggplot(one_combo_per_overlap, aes(x = FY_COMPLET, y = overlap_combo_acres, color = NEW_COMBO)) +
  facet_wrap(~NEW_COMBO, ncol = 2) +
  #geom_bar(stat = "identity")
  geom_line()

  

## TOTAL FOOTPRINT ACRES
##
one_overlap_per_year = one_treatment_per_day %>%
  group_by(Incid_Name, FY_COMPLET) %>%
  distinct(NEW_ACTIVITY_N, .keep_all = TRUE) %>%
  #reframe(footprint_ac = mean(Overlap_Acres), across(c(ACTIVITY_N_COMBO))) %>%
  mutate(Overlap_Acres_per_year = sum(Overlap_Acres)) %>%
  ungroup()

footprint_acres_per_year = pivot_wider(one_overlap_per_year,
   id_cols = c("FY_COMPLET"),
   names_from = "NEW_ACTIVITY_N",
   values_from = "Overlap_Acres_per_year",
   values_fn = sum,
   values_fill = 0)


ggplot(footprint_acres_per_year, aes(x = FY_COMPLET, y = Overlap_Acres, fill = NEW_ACTIVITY_N)) +
  geom_bar(stat = "identity") +
  ggtitle("FOOTPRINT ACRES")



# TOTAL ACTIVITY ACRES
## 

# activity_acres_per_fire_per_year = one_treatment_per_day %>%
#   group_by(Incid_Name, FY_COMPLET) %>%
#   reframe(activity_ac_per_year = sum(Overlap_Acres), across(c(NEW_ACTIVITY_N))) %>%
#   ungroup()

activity_acres_per_year = pivot_wider(overlap_table_single_fires, 
   id_cols = c("FY_COMPLET"),
   names_from = "NEW_ACTIVITY_N",
   values_from = "Overlap_Acres",
   values_fn = sum,
   values_fill = 0)

ggplot(treatment_table, aes(x = FY_COMPLET, y = Acres_within_Fire, color = ACTIVITY_N)) +
  #facet_wrap(~ACTIVITY_N, ncol = 2) +
  geom_line()



  
  
################################################################################################




## Pivot with fires as rows, fields as ACTIVITY_N, and values from Overlap_Acres
treatment_footprint_area_by_fire = pivot_wider(one_treatment_per_day,
  id_cols = c("Incid_Name", "Ig_Year", "Fire_Acres_R5"), names_from = "NEW_ACTIVITY_N",
  values_from = "Overlap_Acres", values_fn=sum, values_fill=0)
write.csv(treatment_footprint_area_by_fire,"Output/treatment_footprint_area_by_fire.csv",row.names = FALSE)

treatment_footprint_area_by_fire2 = pivot_wider(one_activity_per_overlap,
     id_cols = c("Incid_Name", "Ig_Year", "Fire_Acres_R5"), names_from = "NEW_ACTIVITY_N",
     values_from = "Overlap_Acres", values_fn=sum, values_fill=0)


# spread one_activity_per_overlap to display Activity Combo Area
combo_area_by_fire = pivot_wider(one_activity_per_overlap,
  id_cols = c("Incid_Name", "Ig_Year", "Fire_Acres_R5"), names_from = "ACTIVITY_N_COMBO",
  values_from = "Overlap_Acres", values_fn=sum, values_fill=0)
write.csv(treatment_footprint_area_by_fire,"Output/treatment_footprint_area_by_fire.csv",row.names = FALSE)
  

# Pivot with fires as rows, fields as year of treatment, and values from Overlap_Acres
activity_area_by_fire_by_year = pivot_wider(overlap_table_single_fires,
  id_cols = c("Incid_Name","Ig_Year","Fire_Acres_R5"), names_from = "FY_COMPLET",
  values_from="Overlap_Acres", values_fn=sum, values_fill=0)
write.csv(activity_area_by_fire_by_year,"Output/activity_area_by_fire_by_year.csv",row.names = FALSE)


#################################################################################################


# outputs singular treatment area by type for each fire 
treatment_by_fire <-pivot_wider(one_activity_per_overlap,
  id_cols = c("Incid_Name","Ig_Year","Fire_Acres_R5"),names_from="NEW_ACTIVITY_N",
  values_from="Overlap_Acres",values_fn=sum,values_fill=0)
write.csv(treatment_by_fire, "Output/treatment_by_fire.csv", row.names = FALSE)


#  ORIG_OID is from the merged (pre-overlap) treatment layer
#  output is joined back with the original layer & then zonal stats as table
#  used with burn severity as input value raster
treatment_by_object <-pivot_wider(one_activity_per_overlap,
  id_cols = c("ORIG_OID"),names_from="NEW_ACTIVITY_N",
  values_from="Overlap_Acres",values_fn=sum,values_fill=0)
write.csv(treatment_by_object, "Output/treatment_by_object.csv", row.names = FALSE)


#################################################################################################
