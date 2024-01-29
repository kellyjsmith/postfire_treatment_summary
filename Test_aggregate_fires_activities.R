library("sf")
library("terra")
library("tidyverse")
library("mapview")
library("foreach")
library("doParallel")

# FACTS COLUMNS TO KEEP -- Kelly - NBR_UNITS1 IS ACRES COMPLETED; NBR_UNITS_ IS PLANNED
keep <- c("FACTS_ID","SUID","CRC_VALUE","DATE_COMPL","GIS_ACRES","ACTIVITY_C","ACTIVITY",
          "ACTIVITY_R","ACTIVITY_S","ACTIVITY_U","LOCAL_QUAL",
          "METHOD_COD","METHOD","NBR_UNITS_","NBR_UNITS1","FUND_CODES","ISWUI","REFORESTAT",
          "PRODUCTIVI","LAND_SUITA","FS_UNIT_ID")

prepare_fires <- function(fires,focal_fires){
  
  fires <- st_transform(fires,crs=3310) |>
    filter(fires$Ig_Year > 1992 & fires$Ig_Year <2018) |> 
    mutate(VB_ID = paste(Ig_Year, Incid_Name, sep = "")) |>
    filter(VB_ID%in%focal_fires$VB_ID)
  
  fires <- fires[st_is_valid(fires),]
  fires <- fires[st_dimension(fires)==2,]
  fires$fire_area <- st_area(fires)
  fires
  
}

prepare_facts <- function(facts){
  facts <- st_transform(facts,crs=3310)
  
  # Manage dates
  # Only include records with a completed date
  # facts <- facts |> filter(!is.na(DATE_C))
  facts$DATE_COMPL <- ymd(as.character(facts$DATE_COMPL))
  facts$year <- year(facts$DATE_COMPL)
  
  
  # compute geometric parameters of facts polygons
  facts$activity_area <- as.numeric(st_area(facts))
  facts <- st_buffer(facts,0)
  facts.perimeters <- st_cast(facts,"MULTILINESTRING")
  facts$perim.lengths <- as.numeric(st_length(facts.perimeters))
  facts$p.a.ratio <- facts$perim.lengths/facts$activity_area
  
  facts <- facts[st_is_valid(facts),]
  facts <- facts[st_dimension(facts)==2,]
  # creates an id to track where polygons in activities go
  facts$facts_polygon_id <- 1:nrow(facts)
  return(facts)
}

self_intersect <- function(polygons,precission=1000,area_threshold=1){
  
  polygons<-st_buffer(polygons,0)
  if(!is.null(precission)){
    st_precision(polygons)<-precission
  }
  
  polygons<-st_buffer(polygons,0) 
  polygons<-st_intersection(polygons)
  polygons<-st_make_valid(polygons)
  polygons<-polygons[st_is_valid(polygons),]
  polygons <- polygons[st_dimension(fires)==2,]
  polygons$area <- as.double(st_area(polygons))
  
  return(polygons[polygons$area>area_threshold,])
  
}

cross_facts_fire<-function(polygon,fires_fires){
  tryCatch({
    i <- polygon$facts_polygon_id
    fire_activity <- st_intersection(fires_fires,polygon)
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

assign_activities <- function(fires_activities,fires){
  
  for(i in 1:nrow(fires_activities)){
    # if activvity year is NA there is no way to assign fires
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
      fires_i<-fires_i[which(fires_i$diff_time==min(fires_i$diff_time)),]
      fires_i<-fires_i[fires_i$BurnBndAc==max(fires_i$BurnBndAc),]
      fires_activities[i,"assigned_fire"]<-fires_i$VB_ID[1]
      fires_activities[i,"flag"]<-length(fires_i$VB_ID)
    }
    
  }
  return(fires_activities)
}

assign_activities_parallel <- function(fires_activities,fires,cores){
  
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

intersect_activities<-function(activities,fires,precission,cores){
  
  on.exit(try(stopCluster(cl)))
  fires_fires  <-  self_intersect(fires, precission  =  precission)
  # fires_fires<-self_intersect(fires,area_threshold = 0)
  facts_polygon_id  <-  activities$facts_polygon_id
  # get polygons intersecting and not intersecting, uses st_intersects that does
  # not return geometries
  intersecting <- st_intersects(fires_fires, activities)
  intersecting <- sort(unique(unlist(intersecting)))
  # get only polygons that were detected by st_intersects
  activities <- activities[intersecting, ]
  intersecting <- activities$facts_polygon_id
  not_intersecting <- setdiff(facts_polygon_id, intersecting)
  
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

# setwd("C:/Users/Paco/CorvallisWS/Kelly")
planting <- c("Plant Trees")
salvage <- c("Salvage Cut (intermediate treatment, not regeneration)","Stand Clearcut (EA/RH/FH)","Patch Clearcut (EA/RH/FH)","Overstory Removal Cut (from advanced regeneration) (EA/RH/FH)","Sanitation Cut","Group Selection Cut (UA/RH/FH)","Overstory Removal Cut (from advanced regeneration) (EA/RH/FH)","Seed-tree Seed Cut (with and without leave trees) (EA/RH/NFH)","Shelterwood Removal Cut (EA/NRH/FH)")
prep <- c("Piling of Fuels, Hand or Machine","Burning of Piled Material","Yarding - Removal of Fuels by Carrying or Dragging","Site Preparation for Planting - Mechanical","Site Preparation for Planting - Manual","Site Preparation for Planting - Burning","Site Preparation for Planting - Other","Site Preparation for Natural Regeneration - Manual","Site Preparation for Natural Regeneration - Burning","Rearrangement of Fuels","Chipping of Fuels","Compacting/Crushing of Fuels")
release <- c("Tree Release and Weed","Control of Understory Vegetation")
thin <- c("Precommercial Thin","Commercial Thin","Thinning for Hazardous Fuels Reduction","Single-tree Selection Cut (UA/RH/FH)")
replant <- c("Fill-in or Replant Trees")
prune <- c("Pruning to Raise Canopy Height and Discourage Crown Fire","Prune")
fuel <- c("Piling of Fuels, Hand or Machine","Burning of Piled Material","Yarding - Removal of Fuels by Carrying or Dragging","Rearrangement of Fuels","Chipping of Fuels","Compacting/Crushing of Fuels","Underburn - Low Intensity (Majority of Unit)","Broadcast Burning - Covers a majority of the unit")

manage.except.plant <- c(salvage,prep,release,thin,replant,prune,fuel)
manage <- c(planting,manage.except.plant)

##!! prep only if done during/before the first planting
##!! fuels only if done after the first planting

fires <- st_read(dsn = "../../Data/mtbs_wildfires_CA_1993_2017.shp", stringsAsFactors = FALSE)
focal.fires.input = read.csv("../../Data/focal_fires_ks.csv", stringsAsFactors=FALSE)
fires <- prepare_fires(fires,focal.fires.input)

facts <- st_read("../../Data/facts_r5.shp")
facts <- facts[facts$ACTIVITY %in% c(planting,salvage,prep,release,thin,replant,prune,fuel),]
facts <- facts[,keep]
facts <- prepare_facts(facts)

# facts_fires <- prepare_intersect(fires,facts,1000)
facts_fires <- intersect_activities(facts,fires,precission=1000,50)
facts_fires$assigned_activities<-assign_activities_parallel(facts_fires$fires_activities,
                                                   facts_fires$fires,50)

saveRDS(facts_fires,"facts_fires.RDS")










# NOT USED BUT USEFUL
prepare_intersect2 <- function(fires,activities,precission){
  
  fires_fires<-self_intersect(fires,precission=precission)
  # fires_fires<-self_intersect(fires,area_threshold = 0)
  facts_polygon_id<-activities$facts_polygon_id
  # get polygons intersecting and not intersecting, uses st_intersects that does
  # not return geometries
  intersecting <- st_intersects(fires_fires,activities)
  intersecting <- sort(unique(unlist(intersecting)))
  # get only polygons that were detected by st_intersects
  activities <- activities[intersecting,]
  intersecting <- activities$facts_polygon_id
  not_intersecting <- setdiff(facts_polygon_id,intersecting)
  
  activities <- activities %>% group_split(facts_polygon_id)
  
  return(list(activities=activities,
              fires =fires,
              fires_fires = fires_fires,
              intersecting=intersecting,
              not_intersecting=not_intersecting))
  
}

intersect_activities2<-function(activities_prepared,cores){
  
  on.exit(try(stopCluster(cl)))
  
  fires_fires <- activities_prepared$fires_fires
  
  print("Starting intersection")
  print(Sys.time())
  loaded<-.packages()
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  fires_activities<-foreach(x=activities_prepared$activities,
                           .packages=loaded,
                           .export = "cross_facts_fire")%dopar%{
                             cross_facts_fire(x,fires_fires)
                           }
  stopCluster(cl)
  print("Intersection finished")
  print(Sys.time())
  
  fires_activities <- st_as_sf(do.call(rbind,fires_activities))
  
  missing_intersecting<-activities_prepared$intersecting[
    !activities_prepared$intersecting%in%fires_activities$facts_polygon_id]
  
  activities_prepared$fires_activities <- fires_activities
  activities_prepared$missing_intersecting <- missing_intersecting
  
  return(activities_prepared)
}

assign_activities2 <- function(fire_activities,fires){
  
  for(i in 1:nrow(fire_activities)){
    
    fires_i<-fires[fire_activities$origins[[i]],]
    if(nrow(fires_i)==1){
      # print("Single fire")
      # print(fires_i$Ig_Year)
      # print(fire_activities$year[i])
      if(fires_i$Ig_Year>fire_activities$year[i]){
        # print(NA)
        fire_activities[i,"assigned_fire"]<-NA
      }else{
        # print(fires_i$VB_ID)
        fire_activities[i,"assigned_fire"]<-fires_i$VB_ID
      }
      
    }else{
      print("Multiple fire, activities")
      print(fire_activities[i,])
      print("Fires")
      print(fires_i)
      fires_i<-fires_i[fires_i$Ig_Year<= fire_activities$year[i],]
      if(nrow(fires_i)==0){
        print(NA)
        fire_activities[i,"assigned_fire"]<-NA
      }else{
        print(fires_i)
        fires_i$diff_time<-fire_activities$year[i]-fires_i$Ig_Year
        fires_i<-fires_i[which(fires_i$diff_time==min(fires_i$diff_time)),]
        # print(dim(fires_i))
        fires_i<-fires_i[fires_i$BurnBndAc==max(fires_i$BurnBndAc),]
        print(fires_i)
        fire_activities[i,"assigned_fire"]<-fires_i$VB_ID
      }
      
    }
    
  }
}

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

assign_activity <- function(activity,fires){
  
  fires_i<-fires[activity$origins[[i]],]
  if(nrow(fires_i)==1){
    # print("Single fire")
    # print(fires_i$Ig_Year)
    # print(activity$year[i])
    if(fires_i$Ig_Year>activity$year[i]){
      # print(NA)
      activity[i,"assigned_fire"]<-NA
    }else{
      # print(fires_i$VB_ID)
      activity[i,"assigned_fire"]<-fires_i$VB_ID
    }
    
  }else{
    # print("Multiple fire, activities")
    # print(activity[i,])
    # print("Fires")
    # print(fires_i)
    fires_i<-fires_i[fires_i$Ig_Year<= activity$year[i],]
    if(nrow(fires_i)==0){
      # print(NA)
      activity[i,"assigned_fire"]<-NA
    }else{
      # print(fires_i)
      fires_i$diff_time<-activity$year[i]-fires_i$Ig_Year
      fires_i<-fires_i[which(fires_i$diff_time==min(fires_i$diff_time)),]
      # print(dim(fires_i))
      fires_i<-fires_i[fires_i$BurnBndAc==max(fires_i$BurnBndAc),]
      print(fires_i)
      activity[i,"assigned_fire"]<-fires_i$VB_ID
    }
    
  }
}

planting <- c("Plant Trees")
salvage <- c("Salvage Cut (intermediate treatment, not regeneration)","Stand Clearcut (EA/RH/FH)","Patch Clearcut (EA/RH/FH)","Overstory Removal Cut (from advanced regeneration) (EA/RH/FH)","Sanitation Cut","Group Selection Cut (UA/RH/FH)","Overstory Removal Cut (from advanced regeneration) (EA/RH/FH)","Seed-tree Seed Cut (with and without leave trees) (EA/RH/NFH)","Shelterwood Removal Cut (EA/NRH/FH)")
prep <- c("Piling of Fuels, Hand or Machine","Burning of Piled Material","Yarding - Removal of Fuels by Carrying or Dragging","Site Preparation for Planting - Mechanical","Site Preparation for Planting - Manual","Site Preparation for Planting - Burning","Site Preparation for Planting - Other","Site Preparation for Natural Regeneration - Manual","Site Preparation for Natural Regeneration - Burning","Rearrangement of Fuels","Chipping of Fuels","Compacting/Crushing of Fuels")
release <- c("Tree Release and Weed","Control of Understory Vegetation")
thin <- c("Precommercial Thin","Commercial Thin","Thinning for Hazardous Fuels Reduction","Single-tree Selection Cut (UA/RH/FH)")
replant <- c("Fill-in or Replant Trees")
prune <- c("Pruning to Raise Canopy Height and Discourage Crown Fire","Prune")
fuel <- c("Piling of Fuels, Hand or Machine","Burning of Piled Material","Yarding - Removal of Fuels by Carrying or Dragging","Rearrangement of Fuels","Chipping of Fuels","Compacting/Crushing of Fuels","Underburn - Low Intensity (Majority of Unit)","Broadcast Burning - Covers a majority of the unit")







fire_fire <- facts_fires$fire_fire
activities <- facts_fires$activities

facts_fires <- intersect_activities(facts_fires,20)
facts_fires <- intersect_activities(fires,slice_sample(facts,n=10000),1000,cores=50)
# facts_fires <- intersect_activities(fires,slice_sample(facts,n=1000),precission=100,cores=50)
# facts_fires <- intersect_activities(fires,facts[sample(1:nrow(facts),500),],1000,cores=10)
# assign = assign_activities(facts_fires$fire_activities,facts_fires$fires)

st_write(facts[facts_fires$missing_intersecting,],"missing_test.gpkg",delete_dsn=TRUE)



mapview(facts[facts_fires$missing_intersecting,],col.regions="red") + mapview(fires)

st_write(facts[facts_fires$missing_intersecting,],"missing_test.gpkg",delete_dsn=TRUE)

# STOPPED HERE
# ## For FACTS units from the Power Fire, we need to set completed date = accomplished date ##
# fire.power <- fires.focal[fires.focal$VB_ID == "2004POWER",]
# facts.overlap.power <- st_intersection(facts,fire.power)
# facts.overlap.power.ids <- unique(facts.overlap.power$id)
# #if it's on the Power fire, and if completed date is blank, make completed date equal to accomplished date. otherwise keep completed date as it was.
# facts$DATE_C <- ifelse((facts$id %in% facts.overlap.power.ids) & is.na(facts$DATE_C),facts$DATE_A,facts$DATE_C)
# 
## For FACTS units from some fires, do not exclude roadside management stringers
# PACO: Next three blocks are candidates to end up in a function 
# !!! stringer threshold is defined here PACO: If these fires overlap with other fires, those other fires might get remove as well?
facts$stringer.threshold <- 0.1
fires.exclude <- c("2004POWER","2001STAR","1992CLEVELAND","2004FREDS","1987INDIAN","1987CLARK","1989RACK")
fire.exclude <- fires.focal[fires.focal$VB_ID %in% fires.exclude,]
fire.exclude <- st_union(fire.exclude)
facts.overlap.exclude <- st_intersection(facts,fire.exclude)
facts.exclude.ids <- unique(facts.overlap.exclude$id)
facts[facts$id %in% facts.exclude.ids,"stringer.threshold"] <- 0.00

## For FACTS units from some fires, be more strict about what is a roadside stringer
fires.exclude <- c("2008GOVERNMENT","1994BIG_CREEK","1990STORMY","2008PIUTE","1992RUBY","2012READING", "2001CRATER")
fire.exclude <- fires.focal[fires.focal$VB_ID %in% fires.exclude,]
fire.exclude <- st_union(fire.exclude)
facts.overlap.exclude <- st_intersection(facts,fire.exclude)
facts.exclude.ids <- unique(facts.overlap.exclude$id)
facts[facts$id %in% facts.exclude.ids,"stringer.threshold"] <- 0.012

## For FACTS units from some fires, be more strict about what is a roadside stringer
fires.exclude <- c("2012CHIPS","2000STORRIE","2008RICH")
fire.exclude <- fires.focal[fires.focal$VB_ID %in% fires.exclude,]
fire.exclude <- st_union(fire.exclude)
facts <- st_buffer(facts,dist=0)
fire.exclude <- st_buffer(fire.exclude,dist=0)
facts.overlap.exclude <- st_intersection(facts,fire.exclude)
facts.exclude.ids <- unique(facts.overlap.exclude$id)
facts[facts$id %in% facts.exclude.ids,"stringer.threshold"] <- 0.03


## The important year is the year the management was completed

# drop areas managed before any of the focal fires
# facts <- facts[facts$year >= as.numeric(min(fires.focal$Ig_Year)),]


### Identify management that is spatial "stringers" (e.g., roadside salvage) so we can avoid putting plots there but not count it as dividing up planting units
### old approach: on buffered-out polygons (not buffering anymore)
# compute area
# compute perimeter



# then want to filter to area > 500 000, P-A ratio > .01
## alternative approach: which units completely disappear when buffered in by 70 m?

# they already have a unique ID (ID column)
# ids.pre <- unique(facts$id)
facts$area.prebuffer <- as.numeric(st_area(facts))
ids.pre <- facts[facts$area.prebuffer != 0,]$id
facts.buffin <- st_buffer(facts,-60)
facts$area.postbuffer <- as.numeric(st_area(facts.buffin))
facts$area.post.pre.ratio <- facts$area.postbuffer/facts$area.prebuffer

# other slivers are ones with super high perimeter to area ratio
facts$sliver <- "no"
facts = na.omit(facts)
facts[facts$area.post.pre.ratio < facts$stringer.threshold,"sliver"] <- "YES"
facts$sliver <- as.character(facts$sliver)





fires_activities<-assign_activities(fires,facts)
fire_activities$area_intersection <- as.double(st_area(fire_activities))
fire_activities<-fire_activities[fire_activities$area_intersection>1,]




non_overlapping_fires<-cross_single(fire_fire,facts,precision=10000)

# st_write(facts,"../CA_activity_merged_slivers_fixed.shp",delete_dsn=TRUE)


### for each fire, get management history, separately for each area that did not have all the same management applied to it
planting.management <- NULL

for(i in 1:nrow(fires.focal))  {
  
  fire.focal <- fires.focal[i,]
  year.focal <- as.numeric(fire.focal$Ig_Year)
  fire.name <- fire.focal$Incid_Name
  
  cat("\nProcessing management history of fire:",fire.name,"(",i,"of",nrow(fires.focal),")\n")
  
  # remove areas of the fire that burned later (or the same year)
  fires.later <- fires[fires$Ig_Year >= year.focal & fires$VB_ID!=fire.focal$VB_ID,]
  fires.later <- st_combine(fires.later)
  fires.later <- st_buffer(fires.later,0)
  fire.focal <- st_buffer(fire.focal,0)
  fire.focal <- st_difference(fire.focal,fires.later) # take only the part that was not burned later PACO: This can remove counting activities that were completed but burned later
  
  ## get all facts units overlapping it
  facts.fire <- st_intersection(facts, fire.focal)
  facts.fire <- st_buffer(facts.fire, 0)
  
  facts.fire <- as(facts.fire, "Spatial")
  fire.focal <- as(fire.focal, "Spatial")
  
  setScale(100)
  
  # facts.fire <- gBuffer(facts.fire,width=0,byid=TRUE)
  # facts.fire <- st_buffer(facts.fire, 0)
  
  
  ## for all of these, except for planting, remove roadside stringers (mostly salvage). This is so we don't consider that as a type of management by which to divide the planting units (thus considering them incomplete and not candidates for survey)
  ## later on, when assigning survey points, we do take into account whether points are going in to roadside salvage
  
  
  ## pull out all FACTS management of interest (defined above)
  facts.fire.management <- facts.fire[facts.fire$ACTIVITY_N %in% manage,]
  facts.fire.management <- facts.fire.management[facts.fire.management$year >= year.focal,]
  
  ## pull out planting units only
  facts.fire.planting <- facts.fire[facts.fire$ACTIVITY_N %in% planting,]
  facts.fire.planting <- facts.fire.planting[facts.fire.planting$year > year.focal,] # must have been planted after fire #! should we also exclude areas that were planted before the fire?
  
  ## pull out all other relevant management (except planting)
  facts.fire.othermanagement <- facts.fire[facts.fire$ACTIVITY_N %in% manage.except.plant,]
  facts.fire.othermanagement <- facts.fire.othermanagement[facts.fire.othermanagement$year >= year.focal,] # management must have occurred the same year as the fire or later
  
  ## pull out all salvage
  facts.fire.salvage <- facts.fire[facts.fire$ACTIVITY_N %in% c(salvage,"Commercial Thin"),]
  facts.fire.salvage <- facts.fire.salvage[(facts.fire.salvage$year >= year.focal) & (facts.fire.salvage$year < (year.focal+10)),] #  +10 is because we included salvage coded as comm. thin# management must have occurred the same year as the fire or later
  
  facts.fire.salvage.planting <- rbind(facts.fire.salvage,facts.fire.planting)
  
  ## pull out all salvage + planting
  #facts.fire.salvage.planting <- facts.fire[(facts.fire$ACTIVITY_N %in% c(planting,salvage) | (facts.fire$ACTIVITY_N == "Commercial Thin") & facts.fire.salvage$year < (year.focal+10)) & (facts.fire.salvage$year >= year.focal),]
  #facts.fire.salvage.planting <- facts.fire.salvage.planting[(facts.fire.salvage$year >= year.focal) & (facts.fire.salvage$year < (year.focal+10)),] # management must have occurred the same year as the fire or later
  
  ## pull out all prep
  facts.fire.prep <- facts.fire[facts.fire$ACTIVITY_N %in% prep,]
  facts.fire.prep <- facts.fire.prep[facts.fire.prep$year >= year.focal,] # management must have occurred the same year as the fire or later
  
  ## pull out all release
  facts.fire.release <- facts.fire[facts.fire$ACTIVITY_N %in% release,]
  facts.fire.release <- facts.fire.release[facts.fire.release$year > year.focal,] # management must have occurred the same year as the fire or later
  
  ## pull out all thin
  facts.fire.thin <- facts.fire[facts.fire$ACTIVITY_N %in% thin,]
  facts.fire.thin <- facts.fire.thin[facts.fire.thin$year > year.focal,] # management must have occurred the same year as the fire or later
  
  ## pull out all replant
  facts.fire.replant <- facts.fire[facts.fire$ACTIVITY_N %in% replant,]
  facts.fire.replant <- facts.fire.replant[facts.fire.replant$year > year.focal,] # management must have occurred the same year as the fire or later
  
  ## pull out all prune
  facts.fire.prune <- facts.fire[facts.fire$ACTIVITY_N %in% prune,]
  facts.fire.prune <- facts.fire.prune[facts.fire.prune$year > year.focal,] # management must have occurred the same year as the fire or later
  
  ## pull out all fuel
  facts.fire.fuel <- facts.fire[facts.fire$ACTIVITY_N %in% fuel,]
  facts.fire.fuel <- facts.fire.fuel[facts.fire.fuel$year >= year.focal,] # management must have occurred the same year as the fire or later
  
  ## split the planting units along the boundaries of the all management plygons (including planting, in case there were multiple overlapping plantings)
  facts.fire.management.lines <- as(facts.fire.management,"SpatialLines")
  facts.fire.management.lines.buffer <- gBuffer(facts.fire.management.lines,width=0.1,byid=TRUE)
  facts.fire.planting.union <- gBuffer(facts.fire.salvage.planting,width=0)
  
  setScale(10)
  
  facts.fire.management.lines.buffer <- gBuffer(facts.fire.management.lines.buffer,width=0) #PACO: These lines seem to be replicated three times and I am not sure they do anything
  facts.fire.planting.union <- gBuffer(facts.fire.planting.union,width=0)
  facts.fire.management.lines.buffer <- gBuffer(facts.fire.management.lines.buffer,width=0)
  facts.fire.planting.union <- gBuffer(facts.fire.planting.union,width=0)
  facts.fire.management.lines.buffer <- gBuffer(facts.fire.management.lines.buffer,width=0)
  facts.fire.planting.union <- gBuffer(facts.fire.planting.union,width=0)
  
  facts.fire.planting.split <- gDifference(facts.fire.planting.union,facts.fire.management.lines.buffer)
  facts.fire.planting.split <- disaggregate(facts.fire.planting.split)
  
  data <- data.frame(slice.id=seq(from=1,to=length(facts.fire.planting.split)))
  facts.fire.planting.split <- SpatialPolygonsDataFrame(facts.fire.planting.split,data=data)
  
  #get rid of small slivers < 10 sq m
  facts.fire.planting.split$area.sqm <- gArea(facts.fire.planting.split,byid=TRUE)
  facts.fire.planting.split <- facts.fire.planting.split[facts.fire.planting.split$area.sqm > 10,]
  
  pl.spl <- facts.fire.planting.split
  
  #remove topology errors PACO: for these 8 maybe put the topology check after the objects are created in lines 219-269?
  facts.fire.planting <- gBuffer(facts.fire.planting,width=0,byid=TRUE)
  facts.fire.salvage <- gBuffer(facts.fire.salvage,width=0,byid=TRUE)
  facts.fire.prep <- gBuffer(facts.fire.prep,width=0,byid=TRUE)
  facts.fire.release <- gBuffer(facts.fire.release,width=0,byid=TRUE)
  facts.fire.thin <- gBuffer(facts.fire.thin,width=0,byid=TRUE)
  facts.fire.replant <- gBuffer(facts.fire.replant,width=0,byid=TRUE)
  facts.fire.prune <- gBuffer(facts.fire.prune,width=0,byid=TRUE)
  facts.fire.fuel <- gBuffer(facts.fire.fuel,width=0,byid=TRUE)
  
  facts.fire.planting <- gBuffer(facts.fire.planting,width=0,byid=TRUE)
  
  
  
  ###!!! Identify split planting units that were in roadside stringers; later on, set first planting unit ID to blank so we do not consider it to be taking area away from the full planting unit
  stringers <- facts.fire.management[facts.fire.management$sliver == "YES",]
  stringers <- as(stringers,"sf")
  stringers <- st_buffer(stringers,-0.1) # so when we intersect, it's because it's overlapping, not just touching
  stringers <- st_union(stringers)
  pl.spl <- as(pl.spl,"sf")
  #st_crs(pl.spl) <- st_crs(stringers)
  pl.spl <- st_buffer(pl.spl,0)
  stringers <- st_buffer(stringers,0)
  pl.spl.stringers <- st_intersection(pl.spl,stringers)
  
  #st_write(pl.spl.stringers,"../stringer_intersection.gpkg",delete_dsn=TRUE)
  
  #get the IDs of the stringers
  stringer.ids <- unique(pl.spl.stringers$slice.id)
  
  #for the slices that intersected with stringers, call them stringers
  pl.spl$stringer <- "no"
  pl.spl[pl.spl$slice.id %in% stringer.ids,"stringer"] <- "YES"
  pl.spl <- as(pl.spl,"Spatial")
  
  # writeOGR(pl.spl, dsn="Output", layer="pl_spl", driver="ESRI Shapefile")
  
  for(j in 1:nrow(pl.spl)) {
    
    cat("\r--- Planting unit slice",j,"of",nrow(pl.spl))
    
    planting.slice <- pl.spl[j,]
    
    planting.slice <- gBuffer(planting.slice,width=0)
    
    
    # get all overlapping planting units and their associated info
    if(is.null(facts.fire.planting)) planting.over <- NULL else planting.over <- raster::intersect(facts.fire.planting,planting.slice)
    # PACO: if planting.over is NULL, is it necessary to do the next 23 lines? Does it make the script to crash? same for the next 5-blocks of code
    planting.years <- planting.over$year
    planting.years.post <- planting.years - year.focal
    years.order <- order(planting.years.post)
    planting.years.post <- planting.years.post[years.order]
    planting.suids <- planting.over$SUID[years.order]
    planting.methods <- planting.over$METHOD[years.order]
    planting.nyears <- length(planting.years)
    planting.n.unique.years <- length(unique(planting.years))
    planting.unit.name <- planting.over$SUBUNIT_N
    
    pl.spl[j,"planting.years.post"] <- paste(planting.years.post,collapse=", ")
    pl.spl[j,"planting.suids"] <- paste(planting.suids,collapse=", ")
    pl.spl[j,"planting.methods"] <- paste(planting.methods,collapse=", ")
    pl.spl[j,"planting.nyears"] <- paste(planting.nyears,collapse=", ")
    pl.spl[j,"planting.n.unique.years"] <- paste(planting.n.unique.years,collapse=", ")
    pl.spl[j,"planting.unit.names"] <- paste(planting.unit.name,collapse=", ")
    pl.spl[j,"planting.reporting.discrepancy"] <- any(planting.over$reporting_discrepancy)
    # get suids excluding slivers (stringers)
    mgmt.over.noslivers <- planting.over[planting.over$sliver=="no",]
    mgmt.years <- mgmt.over.noslivers$year
    mgmt.years.post <- mgmt.years - year.focal
    years.order <- order(mgmt.years.post)
    mgmt.suids.noslivers <- mgmt.over.noslivers$SUID[years.order]
    pl.spl[j,"planting.suids.noslivers"] <-  paste(mgmt.suids.noslivers,collapse=", ")
    
    
    # get all overlapping salvage units and their associated info PACO: These blocks are candidates to be moved to a function.
    
    if(is.null(facts.fire.salvage)) mgmt.over <- NULL else mgmt.over <- raster::intersect(facts.fire.salvage,planting.slice)
    mgmt.years <- mgmt.over$year
    mgmt.years.post <- mgmt.years - year.focal
    years.order <- order(mgmt.years.post)
    mgmt.years.post <- mgmt.years.post[years.order]
    mgmt.suids <- mgmt.over$SUID[years.order]
    # if(length(mgmt.suids) > 0 && mgmt.suids == 0511022080085000000) browser()
    mgmt.methods <- mgmt.over$METHOD[years.order]
    mgmt.nyears <- length(mgmt.years)
    mgmt.n.unique.years <- length(unique(mgmt.years))
    pl.spl[j,"salvage.years.post"] <- paste(mgmt.years.post,collapse=", ")
    pl.spl[j,"salvage.suids"] <- paste(mgmt.suids,collapse=", ")
    pl.spl[j,"salvage.methods"] <- paste(mgmt.methods,collapse=", ")
    pl.spl[j,"salvage.nyears"] <- paste(mgmt.nyears,collapse=", ")
    pl.spl[j,"salvage.n.unique.years"] <- paste(mgmt.n.unique.years,collapse=", ")
    pl.spl[j,"salvage.reporting.discrepancy"] <- any(mgmt.over$reporting_discrepancy)
    
    # get suids excluding slivers (stringers)
    mgmt.over.noslivers <- mgmt.over[mgmt.over$sliver=="no",]
    mgmt.years <- mgmt.over.noslivers$year
    mgmt.years.post <- mgmt.years - year.focal
    years.order <- order(mgmt.years.post)
    mgmt.suids.noslivers <- mgmt.over.noslivers$SUID[years.order]
    pl.spl[j,"salvage.suids.noslivers"] <-  paste(mgmt.suids.noslivers,collapse=", ")
    
    
    # get all overlapping prep units and their associated info
    
    
    
    if(is.null(facts.fire.prep)) mgmt.over <- NULL else mgmt.over <- raster::intersect(facts.fire.prep,planting.slice)
    
    # only count prep that happened during or before the first year of planting
    first.planting.year <- min(planting.over$year,na.rm=TRUE)
    mgmt.over <- mgmt.over[mgmt.over$year <= first.planting.year,]
    
    ##NOTE that if this was not a planting unit (but rather a salvage unit), first planting year will be Inf so all prep will be counted. Doesn't really matter because it was not planted so we are not using that info.
    
    
    mgmt.years <- mgmt.over$year
    mgmt.years.post <- mgmt.years - year.focal
    years.order <- order(mgmt.years.post)
    mgmt.years.post <- mgmt.years.post[years.order]
    mgmt.suids <- mgmt.over$SUID[years.order]
    mgmt.methods <- mgmt.over$METHOD[years.order]
    mgmt.nyears <- length(mgmt.years)
    mgmt.n.unique.years <- length(unique(mgmt.years))
    pl.spl[j,"prep.years.post"] <- paste(mgmt.years.post,collapse=", ")
    pl.spl[j,"prep.suids"] <- paste(mgmt.suids,collapse=", ")
    pl.spl[j,"prep.methods"] <- paste(mgmt.methods,collapse=", ")
    pl.spl[j,"prep.nyears"] <- paste(mgmt.nyears,collapse=", ")
    pl.spl[j,"prep.n.unique.years"] <- paste(mgmt.n.unique.years,collapse=", ")
    pl.spl[j,"prep.reporting.discrepancy"] <- any(mgmt.over$reporting_discrepancy)
    
    # get suids excluding slivers (stringers)
    mgmt.over.noslivers <- mgmt.over[mgmt.over$sliver=="no",]
    mgmt.years <- mgmt.over.noslivers$year
    mgmt.years.post <- mgmt.years - year.focal
    years.order <- order(mgmt.years.post)
    mgmt.suids.noslivers <- mgmt.over.noslivers$SUID[years.order]
    pl.spl[j,"prep.suids.noslivers"] <-  paste(mgmt.suids.noslivers,collapse=", ")
    
    # get all overlapping release units and their associated info
    
    if(is.null(facts.fire.release)) mgmt.over <- NULL else mgmt.over <- raster::intersect(facts.fire.release,planting.slice)
    mgmt.years <- mgmt.over$year
    mgmt.years.post <- mgmt.years - year.focal
    years.order <- order(mgmt.years.post)
    mgmt.years.post <- mgmt.years.post[years.order]
    mgmt.suids <- mgmt.over$SUID[years.order]
    mgmt.methods <- mgmt.over$METHOD[years.order]
    mgmt.nyears <- length(mgmt.years)
    mgmt.n.unique.years <- length(unique(mgmt.years))
    pl.spl[j,"release.years.post"] <- paste(mgmt.years.post,collapse=", ")
    pl.spl[j,"release.suids"] <- paste(mgmt.suids,collapse=", ")
    pl.spl[j,"release.methods"] <- paste(mgmt.methods,collapse=", ")
    pl.spl[j,"release.nyears"] <- paste(mgmt.nyears,collapse=", ")
    pl.spl[j,"release.n.unique.years"] <- paste(mgmt.n.unique.years,collapse=", ")
    pl.spl[j,"release.reporting.discrepancy"] <- any(mgmt.over$reporting_discrepancy)
    
    # get suids excluding slivers (stringers)
    mgmt.over.noslivers <- mgmt.over[mgmt.over$sliver=="no",]
    mgmt.years <- mgmt.over.noslivers$year
    mgmt.years.post <- mgmt.years - year.focal
    years.order <- order(mgmt.years.post)
    mgmt.suids.noslivers <- mgmt.over.noslivers$SUID[years.order]
    pl.spl[j,"release.suids.noslivers"] <-  paste(mgmt.suids.noslivers,collapse=", ")
    
    # get all overlapping thin units and their associated info
    
    if(is.null(facts.fire.thin)) mgmt.over <- NULL else mgmt.over <- raster::intersect(facts.fire.thin,planting.slice)
    
    facts.fire.thin <- as(facts.fire.thin, "SpatialPolygons")
    if(is.null(facts.fire.thin)) mgmt.over <- NULL else mgmt.over <- raster::intersect(as.numeric(facts.fire.thin),as.numeric(planting.slice))
    
    mgmt.years <- mgmt.over$year
    mgmt.years.post <- mgmt.years - year.focal
    years.order <- order(mgmt.years.post)
    mgmt.years.post <- mgmt.years.post[years.order]
    mgmt.suids <- mgmt.over$SUID[years.order]
    mgmt.methods <- mgmt.over$METHOD[years.order]
    mgmt.nyears <- length(mgmt.years)
    mgmt.n.unique.years <- length(unique(mgmt.years))
    pl.spl[j,"thin.years.post"] <- paste(mgmt.years.post,collapse=", ")
    pl.spl[j,"thin.suids"] <- paste(mgmt.suids,collapse=", ")
    pl.spl[j,"thin.methods"] <- paste(mgmt.methods,collapse=", ")
    pl.spl[j,"thin.nyears"] <- paste(mgmt.nyears,collapse=", ")
    pl.spl[j,"thin.n.unique.years"] <- paste(mgmt.n.unique.years,collapse=", ")
    pl.spl[j,"thin.reporting.discrepancy"] <- any(mgmt.over$reporting_discrepancy)
    
    # get suids excluding slivers (stringers)
    mgmt.over.noslivers <- mgmt.over[mgmt.over$sliver=="no",]
    mgmt.years <- mgmt.over.noslivers$year
    mgmt.years.post <- mgmt.years - year.focal
    years.order <- order(mgmt.years.post)
    mgmt.suids.noslivers <- mgmt.over.noslivers$SUID[years.order]
    pl.spl[j,"thin.suids.noslivers"] <-  paste(mgmt.suids.noslivers,collapse=", ")
    
    # get all overlapping replant units and their associated info
    
    if(is.null(facts.fire.replant)) mgmt.over <- NULL else mgmt.over <- raster::intersect(facts.fire.replant,planting.slice)
    
    ## add to mgmt.over (for replanting) all planting records except for the record from the first year of planting (since they most likely represented replanting)
    
    planting.over.notfirst <- planting.over[planting.over$year > min(planting.over$year,na.rm=TRUE),]
    
    if(!is.null(planting.over.notfirst) && (nrow(planting.over.notfirst)>0)) {
      if(is.null(mgmt.over)) {
        mgmt.over <- planting.over.notfirst
      } else {
        mgmt.over <- rbind(mgmt.over,planting.over.notfirst)
      }
    }
    
    mgmt.years <- mgmt.over$year
    mgmt.years.post <- mgmt.years - year.focal
    years.order <- order(mgmt.years.post)
    mgmt.years.post <- mgmt.years.post[years.order]
    mgmt.suids <- mgmt.over$SUID[years.order]
    mgmt.methods <- mgmt.over$METHOD[years.order]
    mgmt.nyears <- length(mgmt.years)
    mgmt.n.unique.years <- length(unique(mgmt.years))
    pl.spl[j,"replant.years.post"] <- paste(mgmt.years.post,collapse=", ")
    pl.spl[j,"replant.suids"] <- paste(mgmt.suids,collapse=", ")
    pl.spl[j,"replant.methods"] <- paste(mgmt.methods,collapse=", ")
    pl.spl[j,"replant.nyears"] <- paste(mgmt.nyears,collapse=", ")
    pl.spl[j,"replant.n.unique.years"] <- paste(mgmt.n.unique.years,collapse=", ")
    pl.spl[j,"replant.reporting.discrepancy"] <- any(mgmt.over$reporting_discrepancy)
    
    # get suids excluding slivers (stringers)
    mgmt.over.noslivers <- mgmt.over[mgmt.over$sliver=="no",]
    mgmt.years <- mgmt.over.noslivers$year
    mgmt.years.post <- mgmt.years - year.focal
    years.order <- order(mgmt.years.post)
    mgmt.suids.noslivers <- mgmt.over.noslivers$SUID[years.order]
    pl.spl[j,"replant.suids.noslivers"] <-  paste(mgmt.suids.noslivers,collapse=", ")
    
    
    # get all overlapping fuel units and their associated info
    
    if(is.null(facts.fire.fuel)) mgmt.over <- NULL else mgmt.over <- raster::intersect(facts.fire.fuel,planting.slice)
    
    # get the first planting year
    first.planting.year <- min(planting.over$year,na.rm=TRUE)
    
    # only count fuel treatment that happened at least the year after planting, or later
    mgmt.over <- mgmt.over[mgmt.over$year > first.planting.year,]
    
    ##NOTE that if this was not a planting unit (but rather a salvage unit), first planting year will be Inf so no prescribed fire will be counted
    
    mgmt.years <- mgmt.over$year
    mgmt.years.post <- mgmt.years - year.focal
    years.order <- order(mgmt.years.post)
    mgmt.years.post <- mgmt.years.post[years.order]
    mgmt.suids <- mgmt.over$SUID[years.order]
    mgmt.methods <- mgmt.over$METHOD[years.order]
    mgmt.nyears <- length(mgmt.years)
    mgmt.n.unique.years <- length(unique(mgmt.years))
    pl.spl[j,"fuel.years.post"] <- paste(mgmt.years.post,collapse=", ")
    pl.spl[j,"fuel.suids"] <- paste(mgmt.suids,collapse=", ")
    pl.spl[j,"fuel.methods"] <- paste(mgmt.methods,collapse=", ")
    pl.spl[j,"fuel.nyears"] <- paste(mgmt.nyears,collapse=", ")
    pl.spl[j,"fuel.n.unique.years"] <- paste(mgmt.n.unique.years,collapse=", ")
    pl.spl[j,"fuel.reporting.discrepancy"] <- any(mgmt.over$reporting_discrepancy)
    
    # get suids excluding slivers (stringers)
    mgmt.over.noslivers <- mgmt.over[mgmt.over$sliver=="no",]
    mgmt.years <- mgmt.over.noslivers$year
    mgmt.years.post <- mgmt.years - year.focal
    years.order <- order(mgmt.years.post)
    mgmt.suids.noslivers <- mgmt.over.noslivers$SUID[years.order]
    pl.spl[j,"fuel.suids.noslivers"] <-  paste(mgmt.suids.noslivers,collapse=", ")
    
    
    # get all overlapping prune units and their associated info
    
    if(is.null(facts.fire.prune)) mgmt.over <- NULL else mgmt.over <- raster::intersect(facts.fire.prune,planting.slice)
    mgmt.years <- mgmt.over$year
    mgmt.years.post <- mgmt.years - year.focal
    years.order <- order(mgmt.years.post)
    mgmt.years.post <- mgmt.years.post[years.order]
    mgmt.suids <- mgmt.over$SUID[years.order]
    mgmt.methods <- mgmt.over$METHOD[years.order]
    mgmt.nyears <- length(mgmt.years)
    mgmt.n.unique.years <- length(unique(mgmt.years))
    pl.spl[j,"prune.years.post"] <- paste(mgmt.years.post,collapse=", ")
    pl.spl[j,"prune.suids"] <- paste(mgmt.suids,collapse=", ")
    pl.spl[j,"prune.methods"] <- paste(mgmt.methods,collapse=", ")
    pl.spl[j,"prune.nyears"] <- paste(mgmt.nyears,collapse=", ")
    pl.spl[j,"prune.n.unique.years"] <- paste(mgmt.n.unique.years,collapse=", ")
    pl.spl[j,"prune.reporting.discrepancy"] <- any(mgmt.over$reporting_discrepancy)
    
    # get suids excluding slivers (stringers)
    mgmt.over.noslivers <- mgmt.over[mgmt.over$sliver=="no",]
    mgmt.years <- mgmt.over.noslivers$year
    mgmt.years.post <- mgmt.years - year.focal
    years.order <- order(mgmt.years.post)
    mgmt.suids.noslivers <- mgmt.over.noslivers$SUID[years.order]
    pl.spl[j,"prune.suids.noslivers"] <-  paste(mgmt.suids.noslivers,collapse=", ")
    
    
  }
  
  pl.spl$fire.name <- fire.name
  pl.spl$fire.year <- year.focal
  
  
  if(is.null(planting.management)) {
    planting.management <- pl.spl
  } else {
    planting.management <- rbind(planting.management,pl.spl)
  }
  
}


## remove the (small) polygons where there was a topology exception that caused the script to think the slice did not fall within a planted area when in fact it did
errors <- which(planting.management$planting.nyears == 0 & planting.management$salvage.nyears == 0)
if(length(errors) > 0) {
  planting.management <- planting.management[-errors,]
}


### There are some polygons with identical management that are considered separate polygons; spatially merge them together.
# assign the same ID number to all DF rows that are identical
a <- as(planting.management,"sf")
a <- st_buffer(a,0)
# PACO: Are these cols correlative?, make sure not to include undesired columns
first.col.index <- which(names(a) == "planting.years.post")
last.col.index <- which(names(a) == "fire.year")
cols.to.group.by <- names(a)[first.col.index:last.col.index]

## create a single column that is unique for each unique set of management
#   must do it here because group_by.sf (used in pipeline below) only works when given a single column

#! first remove the planting suids for planting slices that are in a sliver so it is not considered the same as the part of the planting slice outside the sliver
# planting.management[planting.management$sliver=="YES","planting.suids.noslivers"] <- ""
unique.id = group_indices(as.data.frame(planting.management),planting.suids.noslivers,salvage.suids.noslivers,prep.suids.noslivers,release.suids.noslivers,thin.suids.noslivers,replant.suids.noslivers,prune.suids.noslivers,fuel.suids.noslivers)
planting.management$unique.id <- unique.id


####!!! resume here searching for how "first FACTS planting suid" is found and set it to null for planting slivers.

pl.mgt.sf <- as(planting.management,"sf")
pl.mgt.sf <- st_buffer(pl.mgt.sf,0)

## for each different unique id, merge together
## spatial merging
pl.mgt.polys <- pl.mgt.sf %>%
  group_by(unique.id) %>%
  summarize()

## add back in the data based on the unique ID number
pl.mgt.data <- as.data.frame(pl.mgt.sf) %>%
  select(-slice.id,-area.sqm,-geometry) %>%
  filter(!duplicated(unique.id))
pl.mgt <- left_join(pl.mgt.polys,pl.mgt.data)

st_write(pl.mgt,"../merged.shp",delete_dsn=TRUE)

planting.management.backup <- planting.management




### add column indicating whether the FACTS planting unit had multiple parts (split by different management)



####!!! here, for each planting suid, get all other suids and their unique ids

library(tidyverse)
# get the first planting SUID of each slice
suids.split <- strsplit(planting.management$planting.suids,", ")
planting.management$first.planting.suid <- map_chr(suids.split,1,.default=NA)

planting.management$planting.slice.split <- "no"

planting.management <- as(planting.management,"sf")
planting.management$area <- st_area(planting.management)

### Commented out because no longer excluding split planting units.
# for(i in 1:nrow(planting.management)) {
#   
#   cat("\r Checking for split planting units: ",i,"of",nrow(planting.management),"      ")
#   
#   planting.row <- planting.management[i,]
# 
#   # if(planting.row$unique.id == 2181) {
#   #   browser()
#   # }
# 
#   focal.area <- planting.row$area
#   
#   first.suid <-planting.row$first.planting.suid
#   unique.id <- planting.row$unique.id
#   
#   suid.matches <- which(planting.management$first.planting.suid == first.suid)
#   id.matches <- which(planting.management$unique.id == unique.id)
#   
#   non.matching.ids.with.same.suid <- suid.matches[!(suid.matches %in% id.matches) & (suid.matches != i)]
#   matching.ids.with.same.suid <- suid.matches[(suid.matches %in% id.matches) & (suid.matches != i)]
#   
#   # if there are areas with the same suid but a different unique.id, it means the planting suid was split into multiple types of follow-up management
#   if(length(non.matching.ids.with.same.suid) > 0) {
# 
#     # get the total area of the non-matching ids (different management--but excluding polygons in stringers)
#     non.matching.rows <- planting.management[non.matching.ids.with.same.suid,]
#     non.matching.rows <- non.matching.rows[non.matching.rows$stringer == "no",] # make sure the non-matching rows are not slivers
#     area.other.management <- sum(non.matching.rows$area)
#     
#     # get the total area of the matching ids (same management)
#     matching.rows <- planting.management[matching.ids.with.same.suid,]
#     area.focal.management <- sum(matching.rows$area) + focal.area
#     
#     prop.focal.area <- area.focal.management/(area.focal.management + area.other.management)
#     
#     ## if the planting unit was split but there was a piece that had >95% of the area (i.e., split by some small misalignments that are probably just human spatial error), then consider the large piece to not be split.
#     if(as.numeric(prop.focal.area) > 0.95) {
#       planting.management[i,"planting.slice.split"] <- "no"
#     } else {
#       planting.management[i,"planting.slice.split"] <- "YES"    
#     }
#     
# 
#     
#     
#   }
#   
# }

out <- planting.management

#add an overall ID and write to file
out$id <- paste0(out$fire.year,out$fire.name,out$slice.id)

# out <- as(planting.management,"sf")
st_write(out,dsn="data/site-selection/output/aggregated-management-history/shapefiles/management_history.gpkg",driver="GPKG",delete_dsn=TRUE)

# remove geometry columns
out.nogeom <- out %>%
  as.data.frame() %>%
  select(-geometry)

write.csv(out.nogeom,"data/site-selection/output/aggregated-management-history/aggregated_management_history.csv",row.names=FALSE)