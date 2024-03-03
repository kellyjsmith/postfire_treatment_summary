library("sf")
library("terra")
library("tidyverse")
library("mapview")
library("foreach")
library("doParallel")

# FACTS COLUMNS TO KEEP -- Kelly - NBR_UNITS1 IS ACRES COMPLETED; NBR_UNITS_ IS PLANNED
keep <- c("FACTS_ID","SU ID","CRC_VALUE","DATE_COMPL","GIS_ACRES","ACTIVITY_C","ACTIVITY",
          "ACTIVITY_R","ACTIVITY_S","ACTIVITY_U","LOCAL_QUAL","METHOD","NBR_UNITS_",
          "NBR_UNITS1","FUND_CODES","ISWUI","REFORESTAT","PRODUCTIVI","LAND_SUITA","FS_UNIT_ID")

# Function to prepare fire layer
prepare_fires <- function(fires, focal_fires){
  
  # Add YearName ID, filter for years, check polygon validity, add geometric attributes
  fires <- fires |> mutate(VB_ID = paste(Ig_Year, Incid_Name, sep = ""))|>
    filter(VB_ID %in% focal_fires$VB_ID) |>
    filter(Ig_Year > 1992 & Ig_Year < 2018) 
  fires <- fires[st_is_valid(fires),]
  fires <- fires[st_dimension(fires)==2,] 
  fires <- fires |>
    group_by(VB_ID) |> summarize(
      geometry = st_union(geometry),
      Ig_Year = first(Ig_Year),
      nIg_Years = n_distinct(Ig_Year),
      Incid_Name = first(Incid_Name),
      nIncid_Name = n_distinct(Incid_Name),
      sumBurnBndAc = sum(BurnBndAc),
      sumFire_Acres = sum(Fire_Acres)
    )
  fires <- st_transform(fires,crs=3310)
  
  fires$fire_area <- st_area(fires)
  fires
  
}

# Function to prepare FACTS layer
prepare_facts <- function(facts){
  facts <- st_transform(facts,crs=3310)
  
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
  facts <- facts[st_dimension(facts)==2,]
  
  # Create an ID to track where polygons in activities go
  facts$facts_polygon_id <- 1:nrow(facts)
  return(facts)
}

# Function to self intersect fire polygons
self_intersect <- function(polygons, precission=1000, area_threshold=0){
  
  polygons<-st_buffer(polygons,0)
  if(!is.null(precission)){
    st_precision(polygons)<-precission
  }
  
  polygons<-st_buffer(polygons,0) 
  polygons<-st_intersection(polygons)
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
# call "precission" to remove topology errors and "cores" for parallel processing
intersect_activities <- function(activities, fires, precission, cores){
  
  on.exit(try(stopCluster(cl)))
  
  # Self-intersect fire layer and create ID to track facts polygon
  fires_fires <- self_intersect(fires, precission  =  precission)
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
      fires_activities[i,"assigned_fire"]<-fires_i$VB_ID
      fires_activities[i,"flag"]<-1
    }else{
      fires_i$diff_time<-fires_activities$year[i]-fires_i$Ig_Year
      fires_activities[i,"flag"]<-length(fires_i$VB_ID)
      fires_i<-fires_i[which(fires_i$diff_time==min(fires_i$diff_time)),]
      fires_i<-fires_i[fires_i$fire_area==max(fires_i$fire_area),]
      fires_activities[i,"assigned_fire"]<-fires_i$VB_ID[1]
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



# NOT USED BUT USEFUL
generate_non_overlapping <- function(polygons,precision=NULL){
  
  polygons<-st_buffer(polygons,0)
  polygons<- st_make_valid(polygons)
  
  if(!is.null(precision)){
    st_precision(polygons)<-precision
  }
  
  pols_pols<-st_intersection(polygons)
  
  lut<-data.frame(intersection_id=c(),orig_id=c())
  for(i in 1:nrow(pols_pols)){
    
    lut<-rbind(lut,data.frame(intersection_id=i,
                              orig_id=pols_pols[["origins"]][[i]]))
    
  }
  
  geoms <- st_geometry(pols_pols[lut$intersection_id,])
  attributes_df<-polygons[lut$orig_id,]
  st_geometry(attributes_df)<-NULL
  result <- cbind(geoms,attributes_df)
  result <- st_sf(result)
  return(result)
  
}

planting <- c("Plant Trees") 
salvage <- c("Salvage Cut (intermediate treatment, not regeneration)","Stand Clearcut (EA/RH/FH)",
             "Patch Clearcut (EA/RH/FH)","Overstory Removal Cut (from advanced regeneration) (EA/RH/FH)",
             "Sanitation Cut","Group Selection Cut (UA/RH/FH)","Overstory Removal Cut (from advanced regeneration) (EA/RH/FH)",
             "Seed-tree Seed Cut (with and without leave trees) (EA/RH/NFH)","Shelterwood Removal Cut (EA/NRH/FH)") 
prep <- c("Piling of Fuels, Hand or Machine","Burning of Piled Material","Yarding - Removal of Fuels by Carrying or Dragging",
          "Site Preparation for Planting - Mechanical","Site Preparation for Planting - Manual",
          "Site Preparation for Planting - Burning","Site Preparation for Planting - Other",
          "Site Preparation for Natural Regeneration - Manual","Site Preparation for Natural Regeneration - Burning",
          "Rearrangement of Fuels","Chipping of Fuels","Compacting/Crushing of Fuels") 
release <- c("Tree Release and Weed","Control of Understory Vegetation") 
thin <- c("Precommercial Thin","Commercial Thin","Thinning for Hazardous Fuels Reduction","Single-tree Selection Cut (UA/RH/FH)") 
replant <- c("Fill-in or Replant Trees") 
prune <- c("Pruning to Raise Canopy Height and Discourage Crown Fire","Prune") 
fuel <- c("Piling of Fuels, Hand or Machine","Burning of Piled Material","Yarding - Removal of Fuels by Carrying or Dragging",
          "Rearrangement of Fuels","Chipping of Fuels","Compacting/Crushing of Fuels","Underburn - Low Intensity (Majority of Unit)",
          "Broadcast Burning - Covers a majority of the unit") 
cert <- c("Certification-Planted", "TSI Certification - Release/weeding",
         "TSI Certification - Thinning", "TSI Certification - Fertilizaiton", 
         "TSI Certification - Cleaning", "TSI Certification - Pruning") 
survey <- c("Stocking Survey", "Plantation Survival Survey", "Vegetative Competition Survey",
           "Post Treatment Vegetation Monitoring", "Low Intensity Stand Examination", "Stand Diagnosis Prepared")
manage.except.plant <- c(salvage,prep,release,thin,replant,prune,fuel,survey,cert)
manage <- c(planting,manage.except.plant)

##!! prep only if done during/before the first planting
##!! fuels only if done after the first planting


# Read in Fire and FACTS datasets
fires <- st_read(dsn = "../../Data/mtbs_wildfires_CA_1993_2017.shp", stringsAsFactors = FALSE)
focal.fires.input = read.csv("../../Data/focal_fires_ks.csv", stringsAsFactors=FALSE)
fires <- prepare_fires(fires,focal.fires.input)

facts <- st_read("../../Data/facts_r5.shp")

# Filter out old (pre-fire) records
facts <- facts %>%
  filter(FISCAL_Y_2 > 1992)

# Keep only reforestation-related activities and important fields
facts <- facts[facts$ACTIVITY %in% c(planting,salvage,prep,release,thin,replant,prune,fuel),]
facts <- facts[,keep]

# Run function to prepare dataset
facts <- prepare_facts(facts)

# Conduct intersection and assign activities
facts_fires <- intersect_activities(facts,fires,precission=1000,10)
facts_fires$assigned_activities<-assign_activities_parallel(facts_fires$fires_activities,
                                                   facts_fires$fires,10)

saveRDS(facts_fires,"facts_fires.RDS")

facts_fires <- readRDS("facts_fires.RDS")
assigned_activities <- facts_fires$assigned_activities
fires <- facts_fires$fires
# CLEANING ASSIGNED ACTIVITIES
assigned_activities <- filter(assigned_activities,!is.na(assigned_fire))
assigned_activities <- assigned_activities[,c(keep,"activity_area","facts_polygon_id","year","VB_ID")]
assigned_activities <- merge(assigned_activities,st_drop_geometry(fires),by="VB_ID")

# CREATING GEOMETRY FIELDS (activity_fire_area is the important AREA)
assigned_activities$activity_fire_area <- st_area(assigned_activities)
assigned_activities_perimeters <- st_cast(assigned_activities,"MULTILINESTRING")
assigned_activities$activity_fire_perim_length <- as.numeric(st_length(assigned_activities_perimeters))
assigned_activities$activity_fire_p_a_ratio <- assigned_activities$activity_fire_perim_length/assigned_activities$activity_fire_area
# CREATING difference between activity and fire year
assigned_activities$diff_years <- assigned_activities$year- assigned_activities$Ig_Year

# CREATE IS_* fields
fields <- c("planting","salvage","prep","release","thin","replant","prune","fuel","manage.except.plant","manage")
for(i in fields){
  categories <- eval(parse(text=i))
  is_cat <- assigned_activities$ACTIVITY%in%categories
  assigned_activities[,paste0("IS_",i)]<-is_cat
} 
saveRDS(assigned_activities,"assigned_activities.RDS")


assigned_activities<-readRDS("assigned_activities.RDS")
comb_fire_diff <- expand.grid(fire_year = unique(assigned_activities$Ig_Year),
                              diff_years =unique(assigned_activities$diff_years))
types <- c("planting","salvage","prep","release","thin","replant","prune","fuel")
assigned_activities$ACTIVITY_TYPE<-NA
for(i in types){
  print(i)
  categories <- eval(parse(text=i))
  is_cat <- assigned_activities$ACTIVITY%in%categories
  assigned_activities[,"ACTIVITY_TYPE"]<-ifelse(is_cat,i,assigned_activities$ACTIVITY_TYPE)
} 
net_activities <- map2_dfr(comb_fire_diff$fire_year,comb_fire_diff$diff_years,
                       function(x,y,assigned_activities){
                         
        filtered <- assigned_activities |> 
                      filter(Ig_Year ==x & diff_years==y)
        
        if(dim(filtered)[1]==0){
          return(NULL)
        }else{
          result <-filtered |> group_by(VB_ID,ACTIVITY_TYPE)|> 
            summarize(geometry=st_union(geometry),n_dissolved=n())
          result$Ig_Year<-x
          result$diff_years<-y
          result$ref_year <-x+y
          result$net_area <- st_area(result)
          return(result)
        }
                       },assigned_activities=assigned_activities)


gross_activities <- map2_dfr(comb_fire_diff$fire_year,comb_fire_diff$diff_years,
                           function(x,y,assigned_activities){
                             
                             filtered <- assigned_activities |> 
                               filter(Ig_Year ==x & diff_years==y)
                             
                             if(dim(filtered)[1]==0){
                               return(NULL)
                             }else{
                               result <-filtered |> group_by(VB_ID,ACTIVITY_TYPE)|> 
                                 summarize(gross_area=sum(st_area(geometry)))
                               result$Ig_Year<-x
                               result$diff_years<-y
                               result$ref_year <-x+y
                               return(result)
                             }
                           },assigned_activities=assigned_activities)

