library("sf")
library("jsonlite")
library("terra")
library("tidyverse")
library("mapview")
library("lubridate")
library("foreach")
library("doParallel")

keep <- c("FACTS_ID","SUID","CRC_VALUE","DATE_COMPL","GIS_ACRES","PURPOSE_CO",
          "ACTIVITY_C","ACTIVITY","LOCAL_QUAL","METHOD","FUND_CODES",
          "ISWUI","REFORESTAT","PRODUCTIVI","LAND_SUITA","FS_UNIT_ID")

# Define Reforestation Treatment Categories and associated FACTS activities
Plant <- "Plant Trees"
Harvest_Salvage <- c("Salvage Cut (intermediate treatment, not regeneration)")
Harvest_NonSalv = c("Stand Clearcut (EA/RH/FH)","Patch Clearcut (EA/RH/FH)","Overstory Removal Cut (from advanced regeneration) (EA/RH/FH)",
           "Sanitation Cut","Group Selection Cut (UA/RH/FH)","Overstory Removal Cut (from advanced regeneration) (EA/RH/FH)",
           "Seed-tree Seed Cut (with and without leave trees) (EA/RH/NFH)","Shelterwood Removal Cut (EA/NRH/FH)") 
SitePrep_NonChem <- c("Site Preparation for Planting - Mechanical","Site Preparation for Planting - Manual",
          "Site Preparation for Planting - Burning","Site Preparation for Planting - Other")
SitePrep_Chem = "Site Preparation for Planting - Chemical"
TSI <- c("Tree Release and Weed","Control of Understory Vegetation","Reforestation Enhancement") 
Thin <- c("Precommercial Thin","Commercial Thin","Thinning for Hazardous Fuels Reduction","Single-tree Selection Cut (UA/RH/FH)") 
Replant <- "Fill-in or Replant Trees"
Prune <- c("Pruning to Raise Canopy Height and Discourage Crown Fire","Prune") 
Fuels <- c("Piling of Fuels, Hand or Machine","Burning of Piled Material","Yarding - Removal of Fuels by Carrying or Dragging",
          "Rearrangement of Fuels","Chipping of Fuels","Compacting/Crushing of Fuels","Underburn - Low Intensity (Majority of Unit)",
          "Broadcast Burning - Covers a majority of the unit") 
Certified_Planted <- "Certification-Planted"
Certified_TSI = c("TSI Certification - Release/weeding",
          "TSI Certification - Thinning", "TSI Certification - Fertilizaiton", 
          "TSI Certification - Cleaning", "TSI Certification - Pruning") 
Survey <- c("Silvicultural Stand Examination","Stocking Survey", "Plantation Survival Survey", "Vegetative Competition Survey",
            "Post Treatment Vegetation Monitoring", "Low Intensity Stand Examination", "Stand Diagnosis Prepared",
            "Pretreatment Exam for Release or Precommercial Thinning","Pretreatment Exam for Reforestation",
            "Pretreatment Exam for Reforestation")
Review = c("Activity Review","Photo Stand Delineation","Remote Sensing Vegetation Mapping","Stand Silviculture Prescription")
Need = c("Reforestation Need Created by Fire","Reforestation Need created by Regeneration Failure","Reforestation Need Change due to Stocking Changes")
manage.except.plant <- c(Harvest_Salvage,Harvest_NonSalv,SitePrep_NonChem,SitePrep_Chem,TSI,Thin,Replant,Prune,Fuels,Survey,Certified_Planted,Certified_TSI,Review,Need)
manage <- c(planting,manage.except.plant)


# Function to prepare fire layer
prepare_fires <- function(fires, nfs_r5){
  
  # Add YearName ID, filter for years, check polygon validity, add geometric attributes
  # fires <- fires |> filter(Ig_Year > 1993 & Ig_Year < 2019)
  fires <- fires[st_is_valid(fires),]
  fires = fires[st_make_valid(fires),]
  fires <- fires[st_dimension(fires)==2,] 
  fires <- fires |>
    group_by(Event_ID) |> summarize(
      geometry = st_union(geometry),
      Ig_Year = first(Ig_Year),
      nIg_Years = n_distinct(Ig_Year),
      Incid_Name = first(Incid_Name),
      nIncid_Name = n_distinct(Incid_Name),
      sumBurnBndAc = sum(BurnBndAc),
      sumFire_Acres = sum(BurnBndAc)
    )
  fires <- st_transform(fires,crs=3310)
  nfs_r5 = st_transform(nfs_r5,crs = 3310)
  
  # Clip fires to R5 NF layer
  nfs_r5 = st_union(nfs_r5)
  fires = st_intersection(fires, nfs_r5)
  
  fires$fire_area <- st_area(fires)
  fires
  
}

# Function to prepare FACTS layer
prepare_facts <- function(facts){
  facts <- st_transform(facts,crs=3310)
  
  # Rename acreage fields
  facts$ACRES_COMPLETED = facts$NBR_UNITS1
  
  # Manage date fields -- only include records with a completed date and add "year"
  # facts <- facts |> filter(!is.na(DATE_C))
  facts$DATE_COMPL <- ymd(as.character(facts$DATE_COMPL))
  facts$year <- year(facts$DATE_COMPL)
  
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
  
  polygons<-st_make_valid(polygons)
  polygons<- st_intersection(polygons)
  # polygons<-st_buffer(polygons,0)
  if(!is.null(precision)){
    st_precision(polygons)<-precision
  }
  polygons<-st_make_valid(polygons)
  polygons<-polygons[st_is_valid(polygons),]
  polygons <- polygons[st_dimension(polygons)==2,]
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
  fires_fires <- self_intersect(fires, precision  =  precision)
  # fires_fires<-self_intersect(fires, area_threshold = 0)
  facts_polygon_id <- activities$facts_polygon_id
  
  # Return all intersecting and non-intersecting facts polygons within the fire layer
  # st_intersect does not return geometries, so must be added later
  intersecting <- st_intersects(fires_fires, activities)
  intersecting <- sort(unique(unlist(intersecting)))
  # get only polygons that were detected by st_intersects
  activities <- activities[intersecting, ]
  intersecting <- activities$facts_polygon_id
  not_intersecting <- setdiff(facts_polygon_id, intersecting)
  
  # Split the data frame containing intersecting facts activities by facts_polygon_id;
  # 
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
  
  
  # fires_activities <- foreach(
  #   x = result_parts,
  #   .packages = loaded,
  #   .combine = rbind,
  #   .export = c("fires", "assign_activities")) %dopar% {
  #     result <- do.call(rbind, x)
  #     result <- assign_activities(x, fires)
  #   }
  
  # Moved the on.exit line here
  on.exit(try(stopCluster(cl)))
  
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

# Function for summarizing fires_activities for overlapping fires
assign_activities <- function(fires_activities, fires){
  
  for(i in 1:nrow(fires_activities)){
    # if activity year is NA there is no way to assign fires
    if(is.na(fires_activities$year[i])){
      fires_activities[i,"assigned_fire"]<-NA
      fires_activities[i,"flag"]<-0
      next
    }
    
    fires_i<-fires[fires_activities$origins[[i]],]
    fires_i<-fires_i[fires_i$Ig_Year<= fires_activities$year[i],]
    
    if(nrow(fires_i)==0){
      fires_activities[i,"assigned_fire"]<-NA
      fires_activities[i,"flag"]<-0
      next
    }
    
    if(nrow(fires_i)==1){
      fires_activities[i,"assigned_fire"]<-fires_i$Event_ID
      fires_activities[i,"flag"]<-1
    }else{
      fires_i$diff_time<-fires_activities$year[i]-fires_i$Ig_Year
      fires_activities[i,"flag"]<-length(fires_i$Event_ID)
      fires_i<-fires_i[which(fires_i$diff_time==min(fires_i$diff_time)),]
      fires_i<-fires_i[fires_i$fire_area==max(fires_i$fire_area),]
      fires_activities[i,"assigned_fire"]<-fires_i$Event_ID[1]
    }
    
  }
  return(fires_activities)
}

# Parallel processing
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

#### TODO: ####

# Summarize severity




#### Read in and prepare Fire and FACTS datasets ####

# setwd("C:/Users/smithke3/OneDrive - Oregon State University/Kelly/Git/thesis_working/postfire_treatment_summary")

nfs_r5 = st_read(dsn = "../../Data/CA_NFs_bounds.shp", stringsAsFactors = FALSE)
fires <- st_read(dsn = "../../Data/Severity/California_Fires.shp", stringsAsFactors = FALSE)
fires$Ig_Year = as.numeric(as.character(fires$Ig_Year))
# fires$Ig_Date <- as.Date(fires$Ig_Date/(1000*24*60*60),origin="1970-01-01")
# fires$Ig_Year = as.numeric(as.character(fires$Ig_Year))
# fires <- st_read(dsn = "../../Data/Severity/mtbs_perims_DD.shp", stringsAsFactors = FALSE)

fires = fires %>%
  # mutate(Ig_Year = year(Ig_Date)) %>%
  filter(Incid_Type == "Wildfire") %>%
  filter(Ig_Year > 1993 & Ig_Year < 2020)

fires <- prepare_fires(fires,nfs_r5)

facts <- st_read("../../Data/facts_r5.shp")

# Filter out records from before the assessment period
facts <- facts %>%
  filter(FISCAL_Y_2 > 1993)

# Keep only reforestation-related activities and important fields
facts <- facts[facts$ACTIVITY %in% c(Harvest_Salvage,Harvest_NonSalv,SitePrep_NonChem,SitePrep_Chem,TSI,Thin,
                                     Replant,Prune,Fuels,Survey,Certified_Planted,Certified_TSI,Review,Need),]
facts <- facts[,keep]

# Run function to prepare dataset
facts <- prepare_facts(facts)

#### Conduct intersection and assign activities ####
facts_fires <- intersect_activities(facts,fires,precision=100,10)
facts_fires$assigned_activities<-assign_activities_parallel(facts_fires$fires_activities,
                                                            facts_fires$fires,10)

setwd("C:/Users/smithke3/OneDrive - Oregon State University/Kelly/Output")

saveRDS(facts_fires,"facts_fires_2024.RDS")


facts_fires <- readRDS("facts_fires_2024.RDS")
assigned_activities <- facts_fires$assigned_activities
fires <- facts_fires$fires
# CLEANING ASSIGNED ACTIVITIES
assigned_activities <- filter(assigned_activities,!is.na(assigned_fire))
assigned_activities <- assigned_activities[,c(keep,"activity_area","facts_polygon_id","year","assigned_fire")]
assigned_activities <- merge(assigned_activities,st_drop_geometry(fires),by.x="assigned_fire",by.y="Event_ID")
# putting  back assigned_fire as Event_ID 
colnames(assigned_activities)[which(colnames(assigned_activities)=="assigned_fire")]<-"Event_ID"
# CREATING difference between activity and fire year
assigned_activities$diff_years <- assigned_activities$year- assigned_activities$Ig_Year

# CREATING GEOMETRY FIELDS (activity_fire_area is the "split" area; this is the geometry we want)
assigned_activities$activity_fire_area <- st_area(assigned_activities)

assigned_activities<-assigned_activities[as.numeric(assigned_activities$activity_fire_area)>0,]
assigned_activities<-assigned_activities[!is.na(assigned_activities$activity_fire_area),]
assigned_activities <- assigned_activities[!st_geometry_type(assigned_activities)=="POINT",]
good<-sapply(assigned_activities$geometry,function(x){
  a<-try(st_cast(x,"MULTILINESTRING"))
  !inherits(a,"try-error")
})
assigned_activities <- assigned_activities[good,]
assigned_activities_perimeters <- st_sfc(lapply(assigned_activities$geometry,function(x){
  if(st_geometry_type(x)=="GEOMETRYCOLLECTION"){
    
    x <- lapply(x,"[")
    
    keep <- sapply(x,function(y){
      is_list(y)
    })
    print(keep)
    x <- x[keep]
    print(x)
    print(length(x))
    x <- st_multipolygon(x)
  }
  st_cast(x,"MULTILINESTRING")
  
}))

assigned_activities$activity_fire_perim_length <- as.numeric(st_length(assigned_activities_perimeters))
assigned_activities$activity_fire_p_a_ratio <- assigned_activities$activity_fire_perim_length/assigned_activities$activity_fire_area


# CREATE IS_* fields
fields <- c("planting","harvest_salvage","harvest","prep","prep_chem","release","thin","replant","survey",
            "prune","fuel","cert_planted","cert_tsi","review","need","manage.except.plant","manage")
for(i in fields){
  categories <- eval(parse(text=i))
  is_cat <- assigned_activities$ACTIVITY%in%categories
  assigned_activities[,paste0("IS_",i)]<-is_cat
} 

# CREATE ACTIVITY TYPE
types <- c("Harvest_Salvage","Harvest_NonSalv","SitePrep_NonChem","SitePrep_Chem","TSI","Thin","Replant",
           "Prune","Fuels","Survey","Certified_Planted","Certified_TSI","Review","Need")
assigned_activities$ACTIVITY_TYPE<-NA
for(i in types){
  print(i)
  categories <- eval(parse(text=i))
  is_cat <- assigned_activities$ACTIVITY%in%categories
  assigned_activities[,"ACTIVITY_TYPE"]<-ifelse(is_cat,i,assigned_activities$ACTIVITY_TYPE)
} 
saveRDS(assigned_activities,"assigned_activities_2024.RDS")

