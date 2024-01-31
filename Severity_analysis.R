library("terra")
library("tidyverse")
files <- list.files("~/Data/Severity",".tif$",full.names=TRUE)
severity <- lapply(files,rast)
severity <- do.call(mosaic,severity)
names(severity)
writeRaster(severity,filename="~/Data/Severity/severity_mtbs.tif",overwrite=TRUE)
# Fires california

Fires <- vect("~/Data/Severity/California_Fires.shp")
summaries <- map_dfr(1998:2022,function(x){
  fires_x <- Fires[Fires$Ig_Year==x,]
  severity_x <- severity[[paste0("mtbs_CONUUS_",x,"_Severity")]]
  fires_x <-
  map_dfr(1:nr)
})

