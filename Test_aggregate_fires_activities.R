library("sf")
library("jsonlite")
library("terra")
library("tidyverse")
library("mapview")
library("lubridate")
library("foreach")
library("doParallel")
library("parallel")

# Define fields to keep from the FACTS database
keep <- c("FACTS_ID","SUID","CRC_VALUE","DATE_COMPL","GIS_ACRES","PURPOSE_CO",
          "ACTIVITY_C","ACTIVITY","LOCAL_QUAL","METHOD","FUND_CODES",
          "ISWUI","REFORESTAT","PRODUCTIVI","LAND_SUITA","FS_UNIT_ID")

# Define reforestation management categories ####
Certified_Planted <- "Certification-Planted"
Certified_Thin = "TSI Certification - Thinning"
Certified_TSI = c("TSI Certification - Release/weeding",
                  "TSI Certification - Fertilizaiton", 
                  "TSI Certification - Cleaning", "TSI Certification - Pruning")
Fuels <- c("Piling of Fuels, Hand or Machine","Burning of Piled Material","Yarding - Removal of Fuels by Carrying or Dragging",
           "Rearrangement of Fuels","Chipping of Fuels","Compacting/Crushing of Fuels","Underburn - Low Intensity (Majority of Unit)",
           "Broadcast Burning - Covers a majority of the unit") 
Harvest_NonSalv = c("Stand Clearcut (EA/RH/FH)","Overstory Removal Cut (from advanced regeneration) (EA/RH/FH)",
                    "Sanitation Cut","Group Selection Cut (UA/RH/FH)","Overstory Removal Cut (from advanced regeneration) (EA/RH/FH)",
                    "Seed-tree Seed Cut (with and without leave trees) (EA/RH/NFH)")
Harvest_Salvage <- c("Salvage Cut (intermediate treatment, not regeneration)")
Need_by_Failure = "Reforestation Need created by Regeneration Failure"
Need_by_Fire = "Reforestation Need Created by Fire"
Plant <- "Plant Trees"
Prune <- c("Pruning to Raise Canopy Height and Discourage Crown Fire","Prune") 
Replant <- "Fill-in or Replant Trees"
SitePrep_Chem = "Site Preparation for Planting - Chemical"
SitePrep_NonChem <- c("Site Preparation for Planting - Mechanical","Site Preparation for Planting - Manual",
                      "Site Preparation for Planting - Burning","Site Preparation for Planting - Other")
Stand_Exam = c("Silvicultural Stand Examination", "Low Intensity Stand Examination")
Survey_Stocking <- "Stocking Survey"
Survey_Survival = "Plantation Survival Survey"
Survey_Other = c("Vegetative Competition Survey","Post Treatment Vegetation Monitoring", "Stand Diagnosis Prepared")
Survey_Pretreatment = c("Pretreatment Exam for Release or Precommercial Thinning","Pretreatment Exam for Reforestation")
Silv_Prescription = "Stand Silviculture Prescription"
Thin <- c("Precommercial Thin","Commercial Thin","Thinning for Hazardous Fuels Reduction","Single-tree Selection Cut (UA/RH/FH)") 
TSI <- c("Tree Release and Weed","Control of Understory Vegetation","Reforestation Enhancement")

types = c("Certified_Planted","Certified_Thin","Certified_TSI","Fuels","Harvest_NonSalv","Harvest_Salvage",
          "Need_by_Failure","Need_by_Fire","Plant","Prune","Replant","SitePrep_Chem","SitePrep_NonChem",
          "Stand_Exam","Survey_Other","Survey_Pretreatment","Survey_Stocking","Survey_Survival",
          "Silv_Prescription","Thin","TSI")
treat = c(Fuels,Harvest_NonSalv,Harvest_Salvage,Prune,Replant,
          SitePrep_Chem,SitePrep_NonChem,Thin,TSI)
monitor = c(Certified_Planted,Certified_Thin,Certified_TSI,Need_by_Failure,Need_by_Fire,Stand_Exam,
            Survey_Other,Survey_Pretreatment,Survey_Stocking,Survey_Survival,Silv_Prescription)
manage <- c(Plant,treat,monitor)
manage.except.plant = manage[manage != Plant]


# Function to prepare fire layer
prepare_fires <- function(fires, nfs_r5) {
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
  
  # Transform CRS
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

## NEW VERSION ##
# Function to self intersect fire polygons
self_intersect <- function(polygons, precision=1, area_threshold=0){
  tryCatch({
    polygons <- st_make_valid(polygons)
    
    # Simplify geometries slightly to reduce complexity
    polygons <- st_simplify(polygons, dTolerance = 1)  # Adjust tolerance as needed
    
    # Ensure consistent column structure
    original_cols <- names(polygons)
    
    # Instead of full intersection, we'll use st_overlap to find overlapping polygons
    overlaps <- st_overlaps(polygons)
    
    # For polygons that overlap, we'll use st_intersection
    result <- lapply(seq_along(overlaps), function(i) {
      if(length(overlaps[[i]]) > 0) {
        intersected <- st_intersection(polygons[i,], polygons[overlaps[[i]],])
        # Ensure consistent columns
        intersected <- intersected[, original_cols]
        return(rbind(polygons[i,], intersected))
      } else {
        return(polygons[i,])
      }
    })
    
    result <- do.call(rbind, result)
    
    if(!is.null(precision)){
      st_precision(result) <- precision
    }
    
    result <- st_make_valid(result)
    result <- result[st_is_valid(result),]
    result <- result[st_dimension(result)==2,]
    result$area <- as.numeric(st_area(result))
    
    return(result[result$area > area_threshold,])
  }, error = function(e) {
    message("Error in self_intersect: ", e$message)
    return(polygons)  # Return original polygons if processing fails
  })
}

## ORIGINAL VERSION ##
# Function to self intersect fire polygons
# self_intersect <- function(polygons, precision=0.0001, area_threshold=0){
# 
#   polygons<-st_make_valid(polygons)
#   polygons<- st_intersection(polygons)
#   # polygons<-st_buffer(polygons,0)
#   if(!is.null(precision)){
#     st_precision(polygons)<-precision
#   }
#   polygons<-st_make_valid(polygons)
#   polygons<-polygons[st_is_valid(polygons),]
#   polygons <- polygons[st_dimension(polygons)==2,]
#   polygons$area <- as.double(st_area(polygons))
# 
#   return(polygons[polygons$area>area_threshold,])
# }


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
# intersect_activities <- function(activities, fires, precision, cores){
#   
#   on.exit(try(stopCluster(cl)))
#   
#   # Self-intersect fire layer and create ID to track facts polygon
#   fires_fires <- self_intersect(fires, precision  =  precision)
#   # fires_fires<-self_intersect(fires, area_threshold = 0)
#   facts_polygon_id <- activities$facts_polygon_id
#   
#   # Return all intersecting and non-intersecting facts polygons within the fire layer
#   # st_intersect does not return geometries, so must be added later
#   intersecting <- st_intersects(fires_fires, activities)
#   intersecting <- sort(unique(unlist(intersecting)))
#   # get only polygons that were detected by st_intersects
#   activities <- activities[intersecting, ]
#   intersecting <- activities$facts_polygon_id
#   not_intersecting <- setdiff(facts_polygon_id, intersecting)
#   
#   # Split the data frame containing intersecting facts activities by facts_polygon_id;
#   # 
#   activities <- activities %>% group_split(facts_polygon_id)
#   
#   print("Starting intersection")
#   print(Sys.time())
#   loaded  <-  .packages()
#   cl <- makeCluster(cores)
#   registerDoParallel(cl)
#   fires_activities  <-  foreach(
#     x  =  activities,
#     .packages  =  loaded,
#     .export = "cross_facts_fire"
#   )  %dopar%  {
#     cross_facts_fire(x, fires_fires)
#   }
#   
#   n  <-  length(fires_activities)
#   group_ids  <-
#     data.frame(ids  =  1:n, group  =  sample(1:cores, n, replace  =  TRUE))
#   result_parts <- lapply(1:cores, function(x) {
#     ids  <-  group_ids[group_ids$group  ==  x, "ids"]
#     fires_activities[ids]
#   })
#   
#   
#   on.exit(try(stopCluster(cl)))
#   
#   fires_activities <- foreach(
#     x = result_parts,
#     .packages = loaded,
#     .combine = rbind) %dopar% {do.call(rbind, x)}
#   
#   stopCluster(cl)
#   print("Intersection finished")
#   print(Sys.time())
#   
#   fires_activities <- st_as_sf(fires_activities)
#   missing_intersecting  <-
#     intersecting[!intersecting  %in%  fires_activities$facts_polygon_id]
#   
#   
#   return(
#     list(
#       activities  =  activities,
#       fires =  fires,
#       fires_fires = fires_fires,
#       fires_activities  =  fires_activities,
#       intersecting  =  intersecting,
#       not_intersecting  =  not_intersecting,
#       missing_intersecting  =  missing_intersecting
#     )
#   )
# }


## NEW VERSION ##
# Function to intersect facts layer with fire layer
intersect_activities <- function(activities, fires, precision){

  message("Starting intersect_activities function")
  message(paste("Input activities:", nrow(activities), "rows"))
  message(paste("Input fires:", nrow(fires), "rows"))
  
  # Self-intersect fire layer and create ID to track facts polygon
  message("Self-intersecting fire layer...")
  tryCatch({
    fires_fires <- self_intersect(fires, precision = precision)
    message(paste("Self-intersection complete. Result:", nrow(fires_fires), "rows"))
  }, error = function(e) {
    stop(paste("Error in self-intersecting fires:", e$message))
  })
  
  facts_polygon_id <- activities$facts_polygon_id
  
  # Return all intersecting and non-intersecting facts polygons within the fire layer
  message("Performing spatial intersection...")
  tryCatch({
    intersecting <- st_intersects(fires_fires, activities)
    intersecting <- sort(unique(unlist(intersecting)))
    message(paste("Intersection complete. Found", length(intersecting), "intersecting polygons"))
  }, error = function(e) {
    stop(paste("Error in spatial intersection:", e$message))
  })
  
  # get only polygons that were detected by st_intersects
  activities <- activities[intersecting, ]
  intersecting <- activities$facts_polygon_id
  not_intersecting <- setdiff(facts_polygon_id, intersecting)
  message(paste("Non-intersecting polygons:", length(not_intersecting)))
  
  # Split the data frame containing intersecting facts activities by facts_polygon_id
  message("Splitting activities data...")
  activities_split <- activities %>% group_split(facts_polygon_id)
  message(paste("Split complete. Number of groups:", length(activities_split)))
  
  message("Starting cross_facts_fire process")
  print(Sys.time())
  
  # Use mclapply for parallelization
  message("Applying cross_facts_fire in parallel...")
  tryCatch({
    fires_activities <- mclapply(activities_split, function(x) {
      cross_facts_fire(x, fires_fires)
    }, mc.cores = 8)  # Use all 8 cores
    
    # Check for any errors in parallel execution
    errors <- Filter(function(x) inherits(x, "error"), fires_activities)
    if (length(errors) > 0) {
      stop(paste("Errors occurred in", length(errors), "parallel executions"))
    }
    
    fires_activities <- do.call(rbind, fires_activities)
    message(paste("cross_facts_fire complete. Result:", nrow(fires_activities), "rows"))
  }, error = function(e) {
    stop(paste("Error in cross_facts_fire process:", e$message))
  })
  
  message("cross_facts_fire process finished")
  print(Sys.time())
  
  message("Converting result to sf object...")
  tryCatch({
    fires_activities <- st_as_sf(fires_activities)
    message("Conversion to sf object complete")
  }, error = function(e) {
    stop(paste("Error in converting to sf object:", e$message))
  })
  
  missing_intersecting <- intersecting[!intersecting %in% fires_activities$facts_polygon_id]
  message(paste("Missing intersecting polygons:", length(missing_intersecting)))
  
  message("intersect_activities function complete")
  
  return(
    list(
      activities = activities,
      fires = fires,
      fires_fires = fires_fires,
      fires_activities = fires_activities,
      intersecting = intersecting,
      not_intersecting = not_intersecting,
      missing_intersecting = missing_intersecting
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

# Parallel processing the assignment function
# assign_activities_parallel <- function(fires_activities, fires, cores){
#   
#   on.exit(try(stopCluster(cl)))
#   n  <-  dim(fires_activities)[1]
#   group_ids  <-
#     data.frame(ids  =  1:n, group  =  sample(1:cores, n, replace  =  TRUE))
#   result_parts <- lapply(1:cores, function(x) {
#     ids  <-  group_ids[group_ids$group  ==  x, "ids"]
#     fires_activities[ids,]
#   })
#   
#   loaded  <-  .packages()
#   cl <- makeCluster(cores)
#   registerDoParallel(cl)
#   fires_activities <- foreach(
#     x = result_parts,
#     .packages = loaded,
#     .combine = rbind,
#     .export = c("assign_activities")
#   ) %dopar% {
#     assign_activities(x,fires)
#   }
#   stopCluster(cl)
#   
#   fires_activities
#   
# }

# Parallel processing use mcapply
assign_activities_parallel <- function(fires_activities, fires){
  
  n <- dim(fires_activities)[1]
  chunk_size <- ceiling(n / 8)  # Split into 8 chunks for 8 cores
  
  chunks <- split(fires_activities, ceiling(seq_len(n)/chunk_size))
  
  fires_activities <- mclapply(chunks, function(chunk) {
    assign_activities(chunk, fires)
  }, mc.cores = 8)
  
  fires_activities <- do.call(rbind, fires_activities)
  
  return(fires_activities)
}

#### Read and prepare Fire and FACTS datasets ####

setwd("C:/Users/smithke3/Box/Kelly_postfire_reforestation_project/postfire_treatment_summary")
setwd("~/Library/CloudStorage/Box-Box/Kelly_postfire_reforestation_project/postfire_treatment_summary")

nfs_r5 = st_read(dsn = "../Data/CA_NFs_bounds.shp", stringsAsFactors = FALSE)
# fires = st_read(dsn = "../Data/Severity/California_Fires.shp", stringsAsFactors = FALSE)
fires <- st_read(dsn = "../Data/Severity/mtbs_perims_DD.shp", stringsAsFactors = FALSE)
facts <- st_read("../Data/facts_r5.shp")

setwd("../Output")

# Prepare fires
fires$Ig_Year = year(fire$Ig_Date)
fires = fires %>%
  mutate(Ig_Year = as.numeric(as.character(fires$Ig_Year))) %>%
  filter(Incid_Type == "Wildfire") %>%
  filter(Ig_Year > 1993)
fires <- prepare_fires(fires,nfs_r5)


# Prepare FACTS
facts <- facts %>%
  filter(FISCAL_Y_2 > 1993)
# Keep only reforestation-related activities and important fields
facts <- facts[facts$ACTIVITY %in% manage,]
facts <- facts[,keep]
facts <- prepare_facts(facts)


saveRDS(fires, "prepared_fires.RDS")
saveRDS(facts, "prepared_facts.RDS")

fires = readRDS("prepared_fires.RDS")
facts = readRDS("prepared_facts.RDS")


#### Conduct intersection and assign activities ####
facts_fires <- intersect_activities(facts, fires, precision=1)
facts_fires$assigned_activities<-assign_activities_parallel(facts_fires$fires_activities,
                                                            facts_fires$fires)


saveRDS(facts_fires,"facts_fires_2024.RDS")

facts_fires <- readRDS("facts_fires_2024.RDS")

assigned_activities <- facts_fires$assigned_activities
fires <- facts_fires$fires

# CLEANING ASSIGNED ACTIVITIES
assigned_activities = unite(assigned_activities, "vb_id", Ig_Year:Incid_Name, remove = FALSE)
  
assigned_activities <- filter(assigned_activities,!is.na(assigned_fire))

assigned_activities <- assigned_activities[,c(keep,"vb_id","activity_area","facts_polygon_id","year","assigned_fire"),]

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



#### Create plant year field ####

# Function to link each non-plant management activity the most recent planting that is
# spatially intersecting and populate a field called plant_year

process_assigned_activities <- function(assigned_activities) {

  # Function to process each group
  process_group <- function(group_data) {
    plant_data <- group_data %>% filter(IS_Plant == TRUE)
    non_plant_data <- group_data %>% filter(IS_Plant == FALSE)
    
    if (nrow(plant_data) > 0 && nrow(non_plant_data) > 0) {
      intersections <- st_intersects(non_plant_data, plant_data)
      
      non_plant_data$plant_year <- sapply(intersections, function(x) {
        if (length(x) > 0) {
          return(plant_data$year[x[1]])
        } else {
          return(NA)
        }
      })
    } else {
      non_plant_data$plant_year <- NA
    }
    
    result <- bind_rows(plant_data, non_plant_data)

  }
  
  # Process each group
  result <- assigned_activities %>%
    group_by(Event_ID, Incid_Name) %>%
    group_modify(~process_group(.x)) %>%
    ungroup()
  
  # Diagnostic information
  cat("Total rows:", nrow(result), "\n")
  cat("Rows with non-NA plant_year:", sum(!is.na(result$plant_year)), "\n")
  cat("Unique Event_IDs:", length(unique(result$Event_ID)), "\n")
  cat("Rows with IS_Plant == TRUE:", sum(result$IS_Plant), "\n")
  cat("Rows with IS_Plant == FALSE:", sum(!result$IS_Plant), "\n")
  
  return(result)
}

# Usage
processed_activities <- process_assigned_activities(assigned_activities)



## Different method

# Subset assigned_activities into two data frames based on IS_Plant
plant_ <- assigned_activities[assigned_activities$IS_Plant == TRUE, ]
non_plant <- assigned_activities[assigned_activities$IS_Plant == FALSE, ]

# Perform spatial intersection between plant_df and non_plant_df
intersected_plant_non <- sf::st_intersection(plant, non_plant)

# Add a new field "plant_year" to non_plant_df and populate it with the "year" field from the plant data frame
non_plant$plant_year <- plant$year[match(intersected_plant_non$CRC_VALUE, plant$CRC_VALUE)]

# Join back to assigned_activities by CRC_VALUE
result <- merge(assigned_activities, non_plant, by = "CRC_VALUE", all.x = TRUE)


saveRDS(assigned_activities,"assigned_activities_2024.RDS")



#### TODO: ####

# Summarize severity



