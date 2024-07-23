library("sf")
library("jsonlite")
library("terra")
library("tidyverse")
library("mapview")
library("lubridate")
library("foreach")
library("doParallel")
library("units")

# Define fields to keep from the FACTS database
keep <- c("FACTS_ID","SUID","CRC_VALUE","DATE_COMPL","GIS_ACRES","PURPOSE_CO",
          "ACTIVITY_C","ACTIVITY","LOCAL_QUAL","METHOD","FUND_CODES",
          "ISWUI","REFORESTAT","PRODUCTIVI","LAND_SUITA","FS_UNIT_ID")

# Define reforestation management categories ####
Certified_Planted <- "Certification-Planted"
Certified_TSI_Release = "TSI Certification - Release/weeding"
Certified_TSI_Thin = "TSI Certification - Thinning"
Fuels_Fire <- c("Burning of Piled Material","Underburn - Low Intensity (Majority of Unit)",
           "Broadcast Burning - Covers a majority of the unit")
Fuels_Other <- c("Piling of Fuels, Hand or Machine","Yarding - Removal of Fuels by Carrying or Dragging",
                "Rearrangement of Fuels","Chipping of Fuels","Compacting/Crushing of Fuels","Thinning for Hazardous Fuels Reduction")
Harvest_NonSalv = c("Stand Clearcut (EA/RH/FH)","Overstory Removal Cut (from advanced regeneration) (EA/RH/FH)",
                    "Sanitation Cut","Group Selection Cut (UA/RH/FH)","Overstory Removal Cut (from advanced regeneration) (EA/RH/FH)",
                    "Seed-tree Seed Cut (with and without leave trees) (EA/RH/NFH)")
Harvest_Salvage <- c("Salvage Cut (intermediate treatment, not regeneration)")
Need_by_Failure = "Reforestation Need created by Regeneration Failure"
Need_by_Fire = "Reforestation Need Created by Fire"
Plant <- "Plant Trees"
Replant <- "Fill-in or Replant Trees"
SitePrep_Chem = "Site Preparation for Planting - Chemical"
SitePrep_Mech <- c("Site Preparation for Planting - Mechanical")
SitePrep_Other = c("Site Preparation for Planting - Manual","Site Preparation for Planting - Burning",
                   "Site Preparation for Planting - Other")
Stand_Exam = c("Silvicultural Stand Examination", "Low Intensity Stand Examination")
Survey_Stocking <- "Stocking Survey"
Survey_Survival = "Plantation Survival Survey"
Survey_Other = c("Vegetative Competition Survey","Post Treatment Vegetation Monitoring", "Stand Diagnosis Prepared")
Survey_Pretreatment = c("Pretreatment Exam for Release or Precommercial Thinning","Pretreatment Exam for Reforestation")
Silv_Prescription = "Stand Silviculture Prescription"
TSI_Thin <- c("Precommercial Thin","Commercial Thin","Single-tree Selection Cut (UA/RH/FH)") 
TSI_Release <- c("Tree Release and Weed", "Control of Understory Vegetation")
TSI_Prune <- c("Pruning to Raise Canopy Height and Discourage Crown Fire","Prune")

types = c("Certified_Planted","Certified_TSI_Thin","Certified_TSI_Release","Fuels_Fire","Fuels_Other",
          "Harvest_NonSalv","Harvest_Salvage","Need_by_Failure","Need_by_Fire","Plant","Replant",
          "SitePrep_Chem","SitePrep_NonChem","SitePrep_Other", "Stand_Exam","Survey_Other","Survey_Pretreatment",
          "Survey_Stocking","Survey_Survival","Silv_Prescription","TSI_Prune","TSI_Release","TSI_Thin")

treat = c(Fuels_Fire,Fuels_Other,Harvest_NonSalv,Harvest_Salvage,Replant,
          SitePrep_Chem,SitePrep_Mech,TSI_Prune,TSI_Release,TSI_Thin)
monitor = c(Certified_Planted,Certified_TSI_Thin,Certified_TSI_Release,Need_by_Failure,Need_by_Fire,Stand_Exam,
            Survey_Other,Survey_Pretreatment,Survey_Stocking,Survey_Survival,Silv_Prescription)
manage <- c(Plant,treat,monitor)
manage.except.plant = manage[manage != Plant]


# Function to prepare fire layer
prepare_fires <- function(fires, nfs_r5) {
  # Transform CRS
  # fires <- st_transform(fires, crs = 5070)
  # nfs_r5 <- st_transform(nfs_r5, crs = 5070)
  
  
  # Ensure valid geometries and filter for polygons
  fires <- fires %>%
    st_make_valid() %>%
    filter(st_is_valid(.), st_dimension(.) == 2)
  
  # Group and summarize fire data
  fires <- fires %>%
    group_by(Event_ID) %>%
    summarize(
      geometry = st_union(geometry),
      Ig_Year = first(Ig_Year),
      nIg_Years = n_distinct(Ig_Year),
      Incid_Name = first(Incid_Name),
      nIncid_Name = n_distinct(Incid_Name),
      sumBurnBndAc = sum(BurnBndAc),
      sumFire_Acres = sum(BurnBndAc)
    ) %>%
    ungroup()
  

  fires <- st_transform(fires, crs = 5070)
  nfs_r5 <- st_transform(nfs_r5, crs = 5070)
  
  # Clip fires to R5 NF layer
  nfs_r5_union <- st_union(nfs_r5)
  fires <- st_intersection(fires, nfs_r5_union)
  
  # Add fire area
  fires$fire_area <- st_area(fires)
  
  # fires <- st_transform(fires, crs = 3310)
  return(fires)
}

# Function to prepare FACTS layer
prepare_facts <- function(facts){
  facts <- st_transform(facts,crs=5070)
  
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

## NEW VERSION ##
# Function to self intersect fire polygons
# self_intersect <- function(polygons, precision=1, area_threshold=0){
#   tryCatch({
#     polygons <- st_make_valid(polygons)
#     
#     # Simplify geometries slightly to reduce complexity
#     polygons <- st_simplify(polygons, dTolerance = 1)  # Adjust tolerance as needed
#     
#     # Ensure consistent column structure
#     original_cols <- names(polygons)
#     
#     # Instead of full intersection, we'll use st_overlap to find overlapping polygons
#     overlaps <- st_overlaps(polygons)
#     
#     # For polygons that overlap, we'll use st_intersection
#     result <- lapply(seq_along(overlaps), function(i) {
#       if(length(overlaps[[i]]) > 0) {
#         intersected <- st_intersection(polygons[i,], polygons[overlaps[[i]],])
#         # Ensure consistent columns
#         intersected <- intersected[, original_cols]
#         return(rbind(polygons[i,], intersected))
#       } else {
#         return(polygons[i,])
#       }
#     })
#     
#     result <- do.call(rbind, result)
#     
#     if(!is.null(precision)){
#       st_precision(result) <- precision
#     }
#     
#     result <- st_make_valid(result)
#     result <- result[st_is_valid(result),]
#     result <- result[st_dimension(result)==2,]
#     result$area <- as.numeric(st_area(result))
#     
#     return(result[result$area > area_threshold,])
#   }, error = function(e) {
#     message("Error in self_intersect: ", e$message)
#     return(polygons)  # Return original polygons if processing fails
#   })
# }

## ORIGINAL VERSION ##
# Function to self intersect fire polygons
self_intersect <- function(polygons, precision=100, area_threshold=0){

  # polygons <- st_buffer(polygons,0)
  # polygons <- st_cast(polygons,"MULTIPOLYGON")
  # polygons<-st_make_valid(polygons)
  # polygons_a <- polygons[polygons$Ig_Year<2014,]
  # polygons_b <- polygons[!polygons$Ig_Year<2014,]
  # polygons <- rbind(polygons_a,polygons_b)
  # polygons<-st_make_valid(polygons)
  # polygons <- st_buffer(polygons,0)
  # polygons <- group_split(polygons,Ig_Year)
  # polygons <- lapply(polygons,function(x){
  #   res<-st_make_valid(x)
  #   res[st_is_valid(res),]
  # })
  # polygons <- do.call(rbind,polygons)
  
  # polygons<-st_make_valid(polygons)
  if(!is.null(precision)){
    st_precision(polygons)<-precision
  }
  polygons<-st_make_valid(polygons)
  # polygons<-polygons[st_is_valid(polygons),]
  polygons<- st_intersection(polygons)
  
  
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


## ORIGINAL VERSION ##
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


  # on.exit(try(stopCluster(cl)))

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


## NEW VERSION ##
# Function to intersect facts layer with fire layer
# call "precision" to remove topology errors and "cores" for parallel processing
# intersect_activities <- function(activities, fires, precision, cores){
#   on.exit(try(stopCluster(cl)))
#   
#   # Process fires in smaller chunks
#   chunk_size <- 50  # Adjust as needed
#   fire_chunks <- split(fires, ceiling(seq_len(nrow(fires))/chunk_size))
#   
#   fires_fires_list <- lapply(fire_chunks, function(chunk) {
#     tryCatch({
#       self_intersect(chunk, precision = precision)
#     }, error = function(e) {
#       message("Error processing chunk: ", e$message)
#       return(NULL)
#     })
#   })
#   
#   # Remove NULL results and combine
#   fires_fires_list <- fires_fires_list[!sapply(fires_fires_list, is.null)]
#   fires_fires <- do.call(rbind, fires_fires_list)
#   
#   if(is.null(fires_fires) || nrow(fires_fires) == 0) {
#     stop("No valid fire polygons after processing")
#   }
#   
#   print("Fire polygons processed")
#   print(Sys.time())
#   
#   facts_polygon_id <- activities$facts_polygon_id
#   
#   # Use st_intersects for initial filtering
#   intersecting <- st_intersects(fires_fires, activities)
#   intersecting <- sort(unique(unlist(intersecting)))
#   activities <- activities[intersecting, ]
#   intersecting <- activities$facts_polygon_id
#   not_intersecting <- setdiff(facts_polygon_id, intersecting)
#   
#   print("Initial filtering completed")
#   print(Sys.time())
#   
#   activities <- activities %>% group_split(facts_polygon_id)
#   
#   print("Starting intersection")
#   print(Sys.time())
#   loaded <- .packages()
#   cl <- makeCluster(cores)
#   registerDoParallel(cl)
#   
#   fires_activities <- foreach(
#     x = activities,
#     .packages = loaded,
#     .export = "cross_facts_fire"
#   ) %dopar% {
#     cross_facts_fire(x, fires_fires)
#   }
#   
#   stopCluster(cl)
#   print("Intersection finished")
#   print(Sys.time())
#   
#   print("Starting to combine results")
#   fires_activities <- tryCatch({
#     result <- do.call(rbind, fires_activities)
#     print("Results combined")
#     result
#   }, error = function(e) {
#     print(paste("Error combining results:", e$message))
#     return(NULL)
#   })
#   
#   if(!is.null(fires_activities)) {
#     print("Converting to sf object")
#     fires_activities <- tryCatch({
#       result <- st_as_sf(fires_activities)
#       print("Conversion to sf object completed")
#       result
#     }, error = function(e) {
#       print(paste("Error converting to sf object:", e$message))
#       return(NULL)
#     })
#   }
#   
#   print("Calculating missing intersecting")
#   missing_intersecting <- intersecting[!intersecting %in% fires_activities$facts_polygon_id]
#   
#   print("Preparing return list")
#   return_list <- list(
#     activities = activities,
#     fires = fires,
#     fires_fires = fires_fires,
#     fires_activities = fires_activities,
#     intersecting = intersecting,
#     not_intersecting = not_intersecting,
#     missing_intersecting = missing_intersecting
#   )
#   
#   print("Function completed")
#   print(Sys.time())
#   
#   return(return_list)
# }

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

setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/postfire_treatment_summary")

nfs_r5 = st_read(dsn = "../Data/CA_NFs_bounds.shp", stringsAsFactors = FALSE)
fires = st_read(dsn = "../Data/Severity/California_Fires.shp", stringsAsFactors = FALSE)
facts <- st_read("../Data/facts_r5.shp")

# fires <- st_read(dsn = "../../Data/Severity/S_USA.FirePerimeterFinal-selected/S_USA.FirePerimeterFinal.shp", stringsAsFactors = FALSE)
# fires <- fires %>% rename(Ig_Year="FIREYEAR",
#                           Incid_Name = "FIRENAME",
#                           BurnBndAc="GISACRES",
#                           Event_ID="GLOBALID")

# fires$Ig_Date<- as.Date(fires$Ig_Date)
fires = fires %>%
  mutate(Ig_Year = as.numeric(as.character(fires$Ig_Year))) %>%
  filter(Incid_Type == "Wildfire") %>%
  filter(Ig_Year>1992)

fires <- prepare_fires(fires,nfs_r5)



# Filter out activities completed before study period
facts <- facts %>%
  filter(FISCAL_Y_2 > 1992 & FISCAL_Y_2 < 2023)

# Keep only reforestation-related activities and important fields
facts <- facts[facts$ACTIVITY %in% manage,]
facts <- facts[,keep]

# Run function to prepare dataset
facts <- prepare_facts(facts)


#### Conduct intersection and assign activities ####
facts_fires <- intersect_activities(facts, fires, precision=100, cores=10)
facts_fires$assigned_activities<-assign_activities_parallel(facts_fires$fires_activities,
                                                            facts_fires$fires,10)

setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/Output/")

saveRDS(fires, "prepared_fires.RDS")
saveRDS(facts, "prepared_facts.RDS")

fires = readRDS("prepared_fires.RDS")
facts = readRDS("prepared_facts.RDS")

saveRDS(facts_fires,"facts_fires_2024.RDS")

facts_fires <- readRDS("facts_fires_2024_kelly.RDS")
facts_fires = readRDS("facts_fires_2024.RDS")

assigned_activities <- facts_fires$assigned_activities
fires <- facts_fires$fires

assigned_activities = read("assigned_activities.RDS")

# CLEANING ASSIGNED ACTIVITIES
assigned_activities <- filter(assigned_activities,!is.na(assigned_fire))

assigned_activities <- assigned_activities[,c(keep,"Event_ID","activity_area","facts_polygon_id","year","assigned_fire"),]

assigned_activities <- merge(assigned_activities,st_drop_geometry(fires),by.x="assigned_fire",by.y="Event_ID")

# Replace Event_ID with assigned_fire 
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
fields <- c(types,"treat","manage","monitor","manage.except.plant")
for(i in fields){
  categories <- eval(parse(text=i))
  is_cat <- assigned_activities$ACTIVITY%in%categories
  assigned_activities[,paste0("IS_",i)]<-is_cat
} 

# CREATE ACTIVITY TYPE
types <- types
assigned_activities$ACTIVITY_TYPE<-NA
for(i in types){
  print(i)
  categories <- eval(parse(text=i))
  is_cat <- assigned_activities$ACTIVITY%in%categories
  assigned_activities[,"ACTIVITY_TYPE"]<-ifelse(is_cat,i,assigned_activities$ACTIVITY_TYPE)
} 

new_labels = data.frame(
  ACTIVITY_TYPE = c("Certified_Planted", "Certified_TSI_Thin", "Certified_TSI_Release", "Fuels_Fire", "Fuels_Other", 
                    "Harvest_NonSalv", "Harvest_Salvage", "Need_by_Failure", "Need_by_Fire", "Plant", 
                    "Replant", "SitePrep_Chem", "SitePrep_NonChem", "SitePrep_Other", "Stand_Exam", 
                    "Survey_Other", "Survey_Pretreatment", "Survey_Stocking", "Survey_Survival", 
                    "Silv_Prescription", "TSI_Prune", "TSI_Release", "TSI_Thin"),
  type_labels = c("Certified - Plant", "Certified - Thin", "Certified - Release", 
                  "Fuel Reduction (Fire)", "Fuel Reduction (Other)", "Harvest - Non-Salvage", "Harvest - Salvage", 
                  "Reforestation Need (Failure)", "Reforestation Need (Fire)", "Plant Trees", 
                  "Fill-in or Replant Trees", "Site Prep - Chemical", "Site Prep - Non-Chemical", "Site Prep - Other", 
                  "Stand Exam", "Survey (Other)", "Survey (Pretreatment)", "Stocking Survey", "Survival Survey", 
                  "Silvicultural Prescription", "TSI - Prune", "TSI - Release", "TSI - Thin")
)

# Merge the new labels with assigned_activities
assigned_activities = merge(assigned_activities, new_labels, by = "ACTIVITY_TYPE", all.x = TRUE)
saveRDS(assigned_activities,"assigned_activities_2024.RDS")


#### Create plant year field ####

# Function to link each non-plant management activity the most recent planting that is
# spatially intersecting and populate a field called plant_year

process_assigned_activities <- function(assigned_activities) {

  # Function to process each group
  process_group <- function(group_data) {
    plant_data <- group_data %>% filter(IS_Plant == TRUE)
    non_plant_data <- group_data %>% filter(IS_Plant == FALSE)
    
    if (nrow(plant_data) > 0) {
      plant_data$plant_year <- plant_data$year  # Assign plant_year for plantings
      
      if (nrow(non_plant_data) > 0) {
        intersections <- st_intersects(non_plant_data, plant_data)
        
        non_plant_data$plant_year <- sapply(intersections, function(x) {
          if (length(x) > 0) {
            return(max(plant_data$year[x]))  # Use max() to get the most recent year
          } else {
            return(NA)
          }
        })
      }
    } else {
      if (nrow(non_plant_data) > 0) {
        non_plant_data$plant_year <- NA
      }
    }
    
    result <- bind_rows(plant_data, non_plant_data)
    return(result)
  }
  
  # Process each group
  result <- assigned_activities %>%
    group_by(Event_ID, Incid_Name) %>%
    group_modify(~process_group(.x)) %>%
    ungroup()
  
  # Diagnostic information
  cat("Total rows:", nrow(result), "\n")
  cat("Rows with non-NA plant_year:", sum(!is.na(result$plant_year)), "\n")
  cat("Unique Event_I      geometry = st_union(geometry),
      Ig_Year = first(Ig_Year),
      nIg_Years = n_distinct(Ig_Year),
      Incid_Name = first(Incid_Name),
      nIncid_Name = n_distinct(Incid_Name),
      sumBurnBndAc = sum(BurnBndAc),
      sumFire_Acres = sum(BurnBndAc)Ds:", length(unique(result$Event_ID)), "\n")
  cat("Rows with IS_Plant == TRUE:", sum(result$IS_Plant), "\n")
  cat("Rows with IS_Plant == FALSE:", sum(!result$IS_Plant), "\n")
  
  return(result)
}

# Usage
processed_activities <- process_assigned_activities(assigned_activities)


#### Calculate proportions ####

calculate_treatment_proportions <- function(data, select_treatments, type_labels) {
  
  # Calculate net acres for each treatment and plantation
  net_acres <- data %>%
    filter(type_labels %in% c(select_treatments, "Plant Trees")) %>%
    group_by(plant_year, Event_ID, type_labels) %>%
    summarise(
      geometry = st_union(geometry),
      .groups = "drop"
    ) %>%
    mutate(net_acres = st_area(geometry)/4046.86)
  
  # Calculate plantation areas
  plantation_areas <- net_acres %>%
    filter(type_labels == "Plant Trees") %>%
    group_by(plant_year) %>%
    summarise(
      plantation_geometry = st_union(geometry),
      plantation_area_acres = sum(net_acres),
      plantations_count = n(),
      .groups = "drop") %>%
    st_as_sf()
  
  # Calculate treatment areas and their intersection with plantations
  treatment_areas <- net_acres %>%
    filter(type_labels %in% select_treatments) %>%
    group_by(plant_year, type_labels) %>%
    summarise(
      treatment_geometry = st_union(geometry),
      treatment_area_acres = sum(net_acres),
      fires_treated = n_distinct(Event_ID),
      treatment_count = n(),
      .groups = "drop") %>%
    st_as_sf()
    
  treatment_areas %>%
    st_join(plantation_areas, join = st_intersects, left = FALSE) %>%
    mutate(
      intersection_acres = st_area(st_intersection(plantation_areas$plantation_geometry, treatment_geometry)/4046.86),
      prop_of_plantation_area = intersection_acres / plantation_area$plantation_area_acres
    ) %>%
    select(-treatment_geometry) %>%  # Remove the duplicate geometry column
    st_drop_geometry()  # Now we can safely drop the geometry
  
  # Combine results
  result <- treatment_areas %>%
    select(plant_year, type_labels, treatment_count, fires_treated, treatment_area_acres, 
           plantation_area_acres, plantations_count, prop_of_plantation_area) %>%
    pivot_wider(
      id_cols = c(plant_year, plantation_area_acres, plantations_count),
      names_from = type_labels,
      values_from = c(treatment_count, fires_treated, treatment_area_acres, prop_of_plantation_area),
      values_fill = list(treatment_count = 0, fires_treated = 0, treatment_area_acres = 0, prop_of_plantation_area = 0)
    ) %>%
    arrange(plant_year)
  
  return(result)
}

select_treatments <- c("Stocking Survey", "Survival Survey", "Certified - Plant", "TSI - Release", "Fill-in or Replant Trees")
treatment_proportions <- calculate_treatment_proportions(processed_activities, select_treatments, type_labels)


#### TODO: ####

# Summarize severity



