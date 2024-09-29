library(sf)
library(terra)
library(tidyverse)
library(lubridate)
library(foreach)
library(doParallel)
library(units)

# Define fields to keep from the FACTS database
keep <- c("year", "ACTIVITY", "METHOD", "EQUIPMENT","DATE_COMPL", "GIS_ACRES", "PURPOSE_CO", 
          "TREATMENT_", "REFORESTAT", "PRODUCTIVI", "LAND_SUITA",  "FUND_CODES", "COST_PER_U", 
          "ISWUI", "ACTIVITY_C", "ADMIN_FORE", "ADMIN_DIST","FACTS_ID", "SUID", "SUBUNIT", "CRC_VALUE")

# Define reforestation management categories ####

# Certification
Certified_Planted <- "Certification-Planted"
Certified_TSI_Release = "TSI Certification - Release/weeding"
Certification = c(Certified_Planted, Certified_TSI_Release)

# Harvest
Harvest_NonSalv = c("Stand Clearcut (EA/RH/FH)","Overstory Removal Cut (from advanced regeneration) (EA/RH/FH)",
                    "Sanitation Cut","Group Selection Cut (UA/RH/FH)","Overstory Removal Cut (from advanced regeneration) (EA/RH/FH)",
                    "Seed-tree Seed Cut (with and without leave trees) (EA/RH/NFH)")
Harvest_Salvage <- c("Salvage Cut (intermediate treatment, not regeneration)")
Harvest = c(Harvest_NonSalv, Harvest_Salvage)

# Reforestation Need
Need_by_Failure = "Reforestation Need created by Regeneration Failure"
Need_by_Fire = "Reforestation Need Created by Fire"
Need_by_Harvest = "Reforestation Need Created by Harvest"
Need = c(Need_by_Failure, Need_by_Fire, Need_by_Harvest)

# Reforestation
Plant <- "Plant Trees"
Replant <- "Fill-in or Replant Trees"
Reforestation = c(Plant, Replant)

# Site Prep
SitePrep_Chem = "Site Preparation for Planting - Chemical"
# SitePrep_Mech <- c("Site Preparation for Planting - Mechanical")
# SitePrep_Other = c("Site Preparation for Planting - Manual","Site Preparation for Planting - Burning",
#                    "Site Preparation for Planting - Other")
SitePrep_NonChem = c("Site Preparation for Planting - Manual","Site Preparation for Planting - Burning",
                     "Site Preparation for Planting - Other", "Site Preparation for Planting - Mechanical")
SitePrep = c(SitePrep_Chem, SitePrep_NonChem)

# Survey
Stand_Exam = c("Silvicultural Stand Examination", "Low Intensity Stand Examination")
Survey_Other = c("Vegetative Competition Survey", "Post Treatment Vegetation Monitoring",
                 "Stand Silviculture Prescription", "Stand Diagnosis Prepared",
                 "Pretreatment Exam for Release or Precommercial Thinning","Pretreatment Exam for Reforestation")
Survey_Stocking <- "Stocking Survey"
Survey_Survival = "Plantation Survival Survey"
Survey = c(Survey_Other, Stand_Exam, Survey_Stocking, Survey_Survival)

# Thin
# TSI_Prune <- c("Pruning to Raise Canopy Height and Discourage Crown Fire","Prune")
TSI_Release <- c("Tree Release and Weed", "Control of Understory Vegetation")
TSI_Thin <- c("Precommercial Thin","Commercial Thin","Single-tree Selection Cut (UA/RH/FH)")
TSI = c(TSI_Release, TSI_Thin)



treat = c(Harvest, Reforestation, SitePrep, TSI)
monitor = c(Certification, Need, Survey)
manage <- c(Plant, treat, monitor)
manage.except.plant = manage[manage != Plant]


# Function to prepare fire layer
prepare_fires <- function(fires, nfs_r5) {
  
  # Ensure valid geometries and filter for polygons
  fires <- fires %>%
    st_make_valid() %>%
    filter(st_is_valid(.), st_dimension(.) == 2)
  
  # Transform CRS
  # fires <- st_transform(fires, crs = 5070)
  # nfs_r5 <- st_transform(nfs_r5, crs = 5070)
  fires <- st_transform(fires, crs = 3310)
  nfs_r5 <- st_transform(nfs_r5, crs = 3310)
  
  # Clip fires to R5 NF layer
  nfs_r5_union <- st_union(nfs_r5)
  fires <- st_intersection(fires, nfs_r5_union)
  
  # Add fire area
  fires$fire_area <- st_area(fires)
  
  return(fires)
}


# Function to prepare FACTS layer
prepare_facts <- function(facts){
  # facts <- st_transform(facts,crs=5070)
  facts <- st_transform(facts,crs=3310)
  
  # Rename acreage fields
  facts$ACRES_COMPLETED = facts$NBR_UNITS1
  
  # Compute geometric parameters of facts polygons
  facts$activity_area <- as.numeric(st_area(facts))
  facts <- st_buffer(facts,0)
  facts.perimeters <- st_cast(facts,"MULTILINESTRING")
  facts$perim.lengths <- as.numeric(st_length(facts.perimeters))
  facts$p.a.ratio <- facts$perim.lengths/facts$activity_area
  
  # Check to determine feature is a valid polygon
  facts <- facts[st_is_valid(facts),]
  facts = facts[st_make_valid(facts),]
  facts <- facts[st_dimension(facts)==2,]
  
  # Create an ID to track where polygons in activities go
  facts$facts_polygon_id <- 1:nrow(facts)
  return(facts)
}


# Function to self intersect fire polygons
self_intersect <- function(polygons, precision=100, area_threshold=0){
  
  if(!is.null(precision)){
    st_precision(polygons)<-precision
  }
  polygons <- st_collection_extract(polygons)
  polygons <- st_make_valid(polygons)
  polygons<- st_intersection(polygons)
  
  # polygons <- polygons[st_dimension(polygons)==2,]
  polygons$area <- as.double(st_area(polygons))
  
  return(polygons[polygons$area>area_threshold,])
}


# Function to intersect facts_polygon_id values with self-intersected fire layer
cross_facts_fire<-function(polygon, fires_fires){
  tryCatch({
    i <- polygon$facts_polygon_id
    
    fire_activity <- st_intersection(fires_fires, polygon)
    
    fire_activity <- fire_activity[st_dimension(fire_activity)>=2,]
    if(nrow(fire_activity)==0){
      return(NULL)
    }else{
      fire_activity$intersecting_area <- st_area(fire_activity)
      return(fire_activity)
    }
  },error=function(e){
    return(NULL)
  })
}



# Function to intersect facts layer with fire layer
# call "precision" to remove topology errors and "cores" for parallel processing

intersect_activities <- function(activities, fires, precision, cores){
  
  on.exit(try(stopCluster(cl)))
  
  # Self-intersect fire layer and create ID to track facts polygon
  fires_fires<-self_intersect(fires, area_threshold = 0)
  facts_polygon_id <- activities$facts_polygon_id
  
  # Return all intersecting and non-intersecting facts polygons within the fire layer
  intersecting <- st_intersects(fires_fires, activities)
  intersecting <- sort(unique(unlist(intersecting)))
  
  # get only polygons that were detected by st_intersects
  activities <- activities[intersecting, ]
  intersecting <- activities$facts_polygon_id
  not_intersecting <- setdiff(facts_polygon_id, intersecting)
  
  # Split the data frame containing intersecting facts activities by facts_polygon_id
  activities <- activities %>% group_split(facts_polygon_id)
  
  print("Starting intersection")
  print(Sys.time())
  loaded  <-  .packages()
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  fires_activities  <-  foreach(
    x  =  activities,
    .packages  =  loaded,
    .export = "cross_facts_fire"
  )  %dopar%  {
    cross_facts_fire(x, fires_fires)
  }
  
  n  <-  length(fires_activities)
  group_ids  <-
    data.frame(ids  =  1:n, group  =  sample(1:cores, n, replace  =  TRUE))
  result_parts <- lapply(1:cores, function(x) {
    ids  <-  group_ids[group_ids$group  ==  x, "ids"]
    fires_activities[ids]
  })
  
  fires_activities <- foreach(
    x = result_parts,
    .packages = loaded,
    .combine = rbind) %dopar% {do.call(rbind, x)}
  
  stopCluster(cl)
  print("Intersection finished")
  print(Sys.time())
  
  fires_activities <- st_as_sf(fires_activities)
  missing_intersecting  <-
    intersecting[!intersecting  %in%  fires_activities$facts_polygon_id]
  
  return(
    list(
      activities  =  activities,
      fires =  fires,
      fires_fires = fires_fires,
      fires_activities  =  fires_activities,
      intersecting  =  intersecting,
      not_intersecting  =  not_intersecting,
      missing_intersecting  =  missing_intersecting
    )
  )
}


assign_activities <- function(fires_activities, fires){
  
  # Pre-compute the number of intersecting fires for each facts_polygon_id
  intersection_counts <- table(fires_activities$facts_polygon_id)
  
  for(i in 1:nrow(fires_activities)){
    # if activity year is NA there is no way to assign fires
    if(is.na(fires_activities$year[i])){
      fires_activities[i,"assigned_fire"] <- NA
      fires_activities[i,"flag"] <- 0
      next
    }
    
    fires_i <- fires[fires_activities$origins[[i]],]
    fires_i <- fires_i[fires_i$Ig_Year <= fires_activities$year[i],]
    
    if(nrow(fires_i)==0){
      fires_activities[i,"assigned_fire"] <- NA
      fires_activities[i,"flag"] <- 0
      next
    }
    
    # Set the flag based on the pre-computed intersection count
    fires_activities[i,"flag"] <- intersection_counts[as.character(fires_activities$facts_polygon_id[i])]
    
    if(nrow(fires_i)==1){
      fires_activities[i,"assigned_fire"] <- fires_i$Event_ID
      fires_activities[i,"assigned_fire_year"] <- fires_i$Ig_Year
    }else{
      fires_i$diff_time <- fires_activities$year[i] - fires_i$Ig_Year
      fires_i <- fires_i[which(fires_i$diff_time==min(fires_i$diff_time)),]
      fires_i <- fires_i[fires_i$fire_area==max(fires_i$fire_area),]
      fires_activities[i,"assigned_fire"] <- fires_i$Event_ID[1]
      fires_activities[i,"assigned_fire_year"] <- fires_i$Ig_Year[1]
    }
  }
  
  # Filter out duplicates, keeping the record with the max Ig_Year when flag > 1
  fires_activities <- fires_activities %>%
    group_by(facts_polygon_id) %>%
    filter(flag <= 1 | (flag > 1 & assigned_fire_year == max(assigned_fire_year))) %>%
    ungroup()
  
  return(fires_activities)
}




# Parallel processing the assignment function
assign_activities_parallel <- function(fires_activities, fires, cores){
  
  on.exit(try(stopCluster(cl)))
  n  <-  dim(fires_activities)[1]
  group_ids  <-
    data.frame(ids  =  1:n, group  =  sample(1:cores, n, replace  =  TRUE))
  result_parts <- lapply(1:cores, function(x) {
    ids  <-  group_ids[group_ids$group  ==  x, "ids"]
    fires_activities[ids,]
  })
  
  loaded  <-  .packages()
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  fires_activities <- foreach(
    x = result_parts,
    .packages = loaded,
    .combine = rbind,
    .export = c("assign_activities")
  ) %dopar% {
    assign_activities(x,fires)
  }
  stopCluster(cl)
  
  fires_activities
  
}






#### Read and prepare Fire and FACTS datasets ####

setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Data/")


# Read in Administrative Region layer, filtered for REGION == 05 in Arc
nfs_r5 = st_read(dsn = "../Data/CA_NFs_bounds.shp", stringsAsFactors = FALSE)

# fires = st_read(dsn = "../Data/Severity/California_Fires.shp", stringsAsFactors = FALSE)
facts <- st_read("../Data/facts_r5.shp")

fires = st_read("../Data/mtbs_perims_DD.shp")
fires$Ig_Year = year(fires$Ig_Date)


# Filter out fires outside the study period
fires = fires %>%
  mutate(Ig_Year = as.numeric(as.character(fires$Ig_Year))) %>%
  filter(Incid_Type == "Wildfire") %>%
  filter(Ig_Year>1999 & Ig_Year<2022)

# Run function to prepare fires
fires <- prepare_fires(fires, nfs_r5)


# Manage date fields and filter out activities completed outside the study period
# We are including activities through the end of 2022
facts <- facts %>%
  mutate(year = year(DATE_COMPL)) %>%
  filter(year > 2000 & year < 2023)

# Keep only reforestation-related activities and important fields
facts <- facts[facts$ACTIVITY %in% manage,]
facts <- facts[,keep]

# Run function to prepare dataset
facts <- prepare_facts(facts)



#### Conduct intersection and assign activities ####
facts_fires <- intersect_activities(facts, fires, precision = 100, cores = 10)
facts_fires$assigned_activities <- assign_activities_parallel(
  facts_fires$fires_activities, facts_fires$fires, 10)

# setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output/")

saveRDS(facts_fires,"facts_fires_new.RDS")


fires <- facts_fires$fires

st_write(fires, "R5_fires_00_21.shp", append = FALSE)
