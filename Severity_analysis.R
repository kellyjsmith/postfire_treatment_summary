library("terra")
library("sf")
library("tidyverse")
# library("mapview")
files <- list.files("../../Data/Severity",".tif$",full.names=TRUE)
# severity <- vrt(severity,"../../Data/Severity/severity.vrt",overwrite=TRUE)

# Fires california
severity <- rast("../../Data/Severity/severity.vrt")
xr <- rast(files[1])
names(severity)<-names(xr)
Fires <- st_read("../../Data/Severity/California_Fires.shp")
Fires <- project(Fires,crs(severity))
summaries <- map_dfr(1998:2021,function(year){
  print(year)
  fires_year <- Fires[Fires$Ig_Year==year,]
  severity_year <- severity[[paste0("mtbs_CONUS_",year,"_Severity")]]
  map_dfr(1:nrow(fires_year),function(x){
    print(x)
    fire_year_x <- fires_year[x,]
    fire_year_x_severity <- crop(severity_year,ext(fire_year_x))
    fire_year_x_severity <- mask(fire_year_x_severity,fire_year_x)
    levels(fire_year_x_severity)<- data.frame(id=c(0:7),severity=c(0:7))
    res <- data.frame(table(fire_year_x_severity[]))
    names(res)[1]<-"Severity"
    res[,"Map_ID"]<-fire_year_x$Map_ID[1]
    res
  })
})
write.csv(summaries,"summaries_severity_fires_EE.csv",row.names=FALSE)

assigned_activities <- readRDS("assigned_activities.RDS")
assigned_activities$intersection_id
# assigned_activities <- st_as_sf(assigned_activities)
summaries <- map_dfr(unique(assigned_activities$Ig_Year),function(year){
  print(year)
  activities_year <- assigned_activities[assigned_activities$Ig_Year==year,]
  severity_year <- severity[[paste0("mtbs_CONUS_",year,"_Severity")]]
  map_dfr(1:nrow(assigned_activities),function(x){
    print(x)
    activitiy_year_x <- activities_year[x,]
    activitiy_year_x_severity <- crop(severity_year,ext(activitiy_year_x))
    activitiy_year_x_severity <- mask(activitiy_year_x_severity,activitiy_year_x)
    levels(activitiy_year_x_severity)<- data.frame(id=c(0:7),severity=c(0:7))
    res <- data.frame(table(activitiy_year_x_severity[]))
    names(res)[1]<-"Severity"
    res[,"intersection_id"]<-activitiy_year_x$intersection_id[1]
    res
  })
})
write.csv(summaries,"summaries_severity_activities.csv",row.names=FALSE)


