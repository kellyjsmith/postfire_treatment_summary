setwd("C:/Users/smithke3/OneDrive - Oregon State University/Kelly")
# setwd("~/Library/CloudStorage/OneDrive-OregonStateUniversity/Kelly")

library(sf)
library(rgeos)
library(sp)
library(rgdal)
library(raster)
library(dplyr)
library(bigmemory)


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
cert = c("Certification-Planted", "TSI Certification - Release/weeding",
         "TSI Certification - Thinning", "TSI Certification - Fertilizaiton", "TSI Certification - Cleaning",
         "TSI Certification - Pruning")
survey = c("Stocking Survey", "Plantation Survival Survey", "Vegetative Competition Survey", 
           "Post Treatment Vegetation Monitoring", "Low Intensity Stand Examination", "Stand Diagnosis Prepared")

manage.except.plant <- c(salvage,prep,release,thin,replant,prune,fuel)
manage <- c(planting,manage.except.plant)


##!! prep only if done during/before the first planting
##!! fuels only if done after the first planting


### Load fire data ###
fires <- st_read(dsn = "Data/Spatial/mtbs_wildfires_CA_1993_2017.shp", stringsAsFactors = FALSE)
st_precision(fires) <- 100000
fires <- st_transform(fires,crs=3310)
st_precision(fires) <- 100000
fires <- fires[fires$Ig_Year > 1992 & fires$Ig_Year <2018,] # only fires between 1993 - 2017

# st_write(fires, "Output/fires.shp")

#optional output list of fires and years
# write.csv(as.data.frame(fires)[,c("Incid_Name", "Ig_Year")],"Data/veg_severity_perimeters.csv",row.names=FALSE)
# veg_severity_parameters = read.csv("Data/veg_severity_perimeters.csv", stringsAsFactors = FALSE)

fires <- fires %>%
  mutate(VB_ID = paste(Ig_Year, Incid_Name, sep = ""))

# write.csv(as.data.frame(fires)[,c("VB_ID")], "Data/focal_fires.csv", row.names = FALSE)

# read in focal fires
focal.fires.input = read.csv("Data/focal_fires_ks.csv", stringsAsFactors=FALSE)
# focal_fires_ks.csv is manually created from the list of unique fires in postfire_treatments_final.shp
colnames(focal.fires.input)[colnames(focal.fires.input) == "x"] <- "VB_ID"
fires.focal.names <- unique(focal.fires.input$VB_ID)

fires.focal <- fires[fires$VB_ID %in% fires.focal.names,]
fires.focal = st_buffer(fires.focal, 0)
fires.focal$Ig_Year <- substr(fires.focal$VB_ID, 1, 4)
fires.focal$Incid_Name <- substr(fires.focal$VB_ID, 5, nchar(fires.focal$VB_ID))
fires.focal.names <- focal.fires.input$VB_ID



### Load FACTS data ###
# facts <- st_read(dsn = "Data/Spatial/postfire_treatments_final.shp", stringsAsFactors = FALSE)

facts_path <- "Data/Spatial/facts_r5.shp"
# facts_path <- "D:/Thesis/EDW_Activity/July2023/S_USA.Actv_CommonAttribute_PL.gdb"
# r5_path <- "Data/Spatial/Admin_Region_R5.shp"

# Read in the facts and R5 layers
# facts = readOGR(dsn = facts_path, layer = "Actv_CommonAttribute_PL")
facts <- st_read(facts_path)
# facts = st_read(dsn = "D:/Thesis/EDW_Activity/July2023/S_USA.Actv_CommonAttribute_PL.gdb/Actv_CommonAttribute_PL", stringsAsFactors = FALSE)
# r5 <- st_read(r5_path)

# Clip the FACTS layer using the R5 shapefile
# facts <- st_intersection(facts, r5)

st_precision(facts) <- 100000 #this seems to translate to about one meter. make number larger for more precision
facts <- st_transform(facts,crs=3310)
facts$id <- 1:nrow(facts)
facts <- st_buffer(facts,0)
facts$DATE_A <- as.character(facts$DATE_A)
facts$DATE_C <- as.character(facts$DATE_C)
# facts$DATE_P <- as.character(facts$DATE_P)

## thin to just the FACTS that overlap focal fires
facts <- st_buffer(facts,0)
fires.focal <- st_buffer(fires.focal,0)
fires.focal <- st_buffer(fires.focal,0)
fires.focal.singlepoly <- st_union(fires.focal) #PACO: Union by geometry?
fires.focal.singlepoly <- st_buffer(fires.focal.singlepoly,0)
facts <- st_intersection(facts,fires.focal.singlepoly) 
# the layer "postfire_treatments_final" is already intersected with the fire layer


# st_write(facts, "Data/Spatial/facts.shp")
# st_write(fires.focal, "Data/Spatial/fires_focal.shp")
# st_write(fires.focal.singlepoly, "Output/fires_focal_singlepoly.shp")


## Check for reporting discrepancies in reported size vs. actual size
## remove those units where reported and GIS acres do not match. Check either NBR_UNTIS or SUBUNIT_S and allow either to match
# now just making sure GIS acres is not much larger than reported acres
### KS 6/9/23 - changed from 25 to .25
### KS - removed subunit_si; not in common attributes layer

facts <- facts %>%
  mutate( reporting_discrepancy = ! (((((NBR_UNITS_ > (GIS_ACRES*.75)) ) | ((NBR_UNITS_ > (GIS_ACRES*.25)) ))))) 
# | ((((SUBUNIT_SI > (GIS_ACRES*.75)) ) | ((SUBUNIT_SI > (GIS_ACRES*.25)) )))))


# st_write(facts,"Output/facts_reporting_discrepancies.gpkg")


# ## For FACTS units from the Power Fire, we need to set completed date = accomplished date ##
# fire.power <- fires.focal[fires.focal$VB_ID == "2004POWER",]
# facts.overlap.power <- st_intersection(facts,fire.power)
# facts.overlap.power.ids <- unique(facts.overlap.power$id)
# #if it's on the Power fire, and if completed date is blank, make completed date equal to accomplished date. otherwise keep completed date as it was.
# facts$DATE_C <- ifelse((facts$id %in% facts.overlap.power.ids) & is.na(facts$DATE_C),facts$DATE_A,facts$DATE_C)
# 

# !!! stringer threshold is defined here 
facts$stringer.threshold <- 0.1

# PACO: If these fires overlap with other fires, those other fires might get remove as well?

## For FACTS units from some fires, do not exclude roadside management stringers
# PACO: Next three blocks are candidates to end up in a function 
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

# 



## The important year is the year the management was completed
facts$year <- as.numeric(substr(facts$DATE_C,1,4)) #! or do we want accomplished?
#remove management that was not actually performed (e.g., just put up for contract but never logged)
facts <- facts[!is.na(facts$year),]

# drop areas managed before any of the focal fires
# facts <- facts[facts$year >= as.numeric(min(fires.focal$Ig_Year)),]


### Identify management that is spatial "stringers" (e.g., roadside salvage) so we can avoid putting plots there but not count it as dividing up planting units

### old approach: on buffered-out polygons (not buffering anymore)
# compute area
# compute perimeter

facts$area <- as.numeric(st_area(facts))
facts.buffout <- st_buffer(facts,0)
facts.perimeters <- st_cast(facts.buffout,"MULTILINESTRING")
facts$perim.lengths <- as.numeric(st_length(facts.perimeters))

facts$p.a.ratio <- facts$perim.lengths/facts$area


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
  
  facts.fire <- gBuffer(facts.fire,width=0,byid=TRUE)
  # facts.fire <- st_buffer(facts.fire, 0)
  
  
  ## for all of these, except for planting, remove roadside stringers (mostly salvage). This is so we don't consider that as a type of management by which to divide the planting units (thus considering them incomplete and not candidates for survey)
  ## later on, when assigning survey points, we do take into account whether points are going in to roadside salvage
  
  
  ## pull out all FACTS management of interest (defined above)
  facts.fire.management <- facts.fire[facts.fire$ACTIVITY %in% manage,]
  facts.fire.management <- facts.fire.management[facts.fire.management$year >= year.focal,]
  
  ## pull out planting units only
  facts.fire.planting <- facts.fire[facts.fire$ACTIVITY %in% planting,]
  facts.fire.planting <- facts.fire.planting[facts.fire.planting$year > year.focal,] # must have been planted after fire #! should we also exclude areas that were planted before the fire?
  
  ## pull out all other relevant management (except planting)
  facts.fire.othermanagement <- facts.fire[facts.fire$ACTIVITY %in% manage.except.plant,]
  facts.fire.othermanagement <- facts.fire.othermanagement[facts.fire.othermanagement$year >= year.focal,] # management must have occurred the same year as the fire or later
  
  ## pull out all salvage
  facts.fire.salvage <- facts.fire[facts.fire$ACTIVITY %in% c(salvage,"Commercial Thin"),]
  facts.fire.salvage <- facts.fire.salvage[(facts.fire.salvage$year >= year.focal) & (facts.fire.salvage$year < (year.focal+10)),] #  +10 is because we included salvage coded as comm. thin# management must have occurred the same year as the fire or later
  
  facts.fire.salvage.planting <- rbind(facts.fire.salvage,facts.fire.planting)
  
  ## pull out all salvage + planting
  #facts.fire.salvage.planting <- facts.fire[(facts.fire$ACTIVITY %in% c(planting,salvage) | (facts.fire$ACTIVITY == "Commercial Thin") & facts.fire.salvage$year < (year.focal+10)) & (facts.fire.salvage$year >= year.focal),]
  #facts.fire.salvage.planting <- facts.fire.salvage.planting[(facts.fire.salvage$year >= year.focal) & (facts.fire.salvage$year < (year.focal+10)),] # management must have occurred the same year as the fire or later
  
  ## pull out all prep
  facts.fire.prep <- facts.fire[facts.fire$ACTIVITY %in% prep,]
  facts.fire.prep <- facts.fire.prep[facts.fire.prep$year >= year.focal,] # management must have occurred the same year as the fire or later
  
  ## pull out all release
  facts.fire.release <- facts.fire[facts.fire$ACTIVITY %in% release,]
  facts.fire.release <- facts.fire.release[facts.fire.release$year > year.focal,] # management must have occurred the same year as the fire or later
  
  ## pull out all thin
  facts.fire.thin <- facts.fire[facts.fire$ACTIVITY %in% thin,]
  facts.fire.thin <- facts.fire.thin[facts.fire.thin$year > year.focal,] # management must have occurred the same year as the fire or later
  
  ## pull out all replant
  facts.fire.replant <- facts.fire[facts.fire$ACTIVITY %in% replant,]
  facts.fire.replant <- facts.fire.replant[facts.fire.replant$year > year.focal,] # management must have occurred the same year as the fire or later
  
  ## pull out all prune
  facts.fire.prune <- facts.fire[facts.fire$ACTIVITY %in% prune,]
  facts.fire.prune <- facts.fire.prune[facts.fire.prune$year > year.focal,] # management must have occurred the same year as the fire or later
  
  ## pull out all fuel
  facts.fire.fuel <- facts.fire[facts.fire$ACTIVITY %in% fuel,]
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
    if(is.null(facts.fire.planting)) {
      planting.over <- NULL
      # pl.spl
    } else {
      planting.over <- raster::intersect(facts.fire.planting,planting.slice)
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
      
    }
    
    
    
    
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
    
    if(is.null(facts.fire.thin)) {
      mgmt.over <- NULL
      print("facts fire thin is null")
    } else {
      print("hello")
      print(facts.fire.thin)
      print(planting.slice)
      mgmt.over <- raster::intersect(facts.fire.thin,planting.slice)
    }
    
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