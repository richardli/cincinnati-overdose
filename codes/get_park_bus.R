library(sf)
library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(maptools)
library(reshape2)
library(gridExtra)
library(ggplot2)
library(SUMMER)

########################### Create Parks
dir <- "../data/"
geo1 <- readOGR("../data/blockgroup/blockgroups.shp")
geo2 <- readOGR("../data/park_and_bus/Cincy_Parks_dissolved.shp")
geo2 <- spTransform(geo2, CRS("+proj=longlat +datum=WGS84"))
# join the two maps
geo <- union(geo1, geo2)
geo$area <- area(geo) 
geo1$area <- area(geo1)
# For each field, get area per soil type
tab1 <- aggregate(area~Id + GEOID, data=geo, FUN=sum, drop = FALSE)
tab2 <- aggregate(area~GEOID, data=geo1, FUN=sum)
colnames(tab2)[2] <- "total"
tab1 <- merge(tab1, tab2, all = TRUE)
tab1$prop <- tab1$area / tab1$total
tab1$prop[is.na(tab1$prop)] <- 0
tab1 <- tab1[, -2]
colnames(tab1) <- c("bg", "ParkArea", "bgArea", "PropPark")
tab1 <- tab1[, c(1, 3, 2, 4)]
write.csv(tab1, file = paste0(dir, "cin.parks.bg.csv"), row.names=FALSE)
# mapPlot(data = tab1, geo = geo1, by.data = "GEOID", by.geo = "GEOID", variables = c("prop"))

########################### Create Buses
geo3 <- readOGR("../data/park_and_bus/Bus_Stops.shp")
geo3 <- spTransform(geo3, CRS("+proj=longlat +datum=WGS84 +no_defs"))
geo3 <- spTransform(geo3, CRS("+proj=utm +datum=WGS84"))
# quarter mile
geo3 <- gBuffer(geo3, width = 402.336,  byid=TRUE)
geo3 <- spTransform(geo3, CRS("+proj=longlat +datum=WGS84"))
geo3$unitID <- "bus"
geo4 <- unionSpatialPolygons(geo3, geo3$unitID)
geo4 <- SpatialPolygonsDataFrame(geo4,  data.frame(ID = 1, row.names = sapply(slot(geo4, "polygons"), function(x) slot(x, "ID"))))
geo <- union(geo1, geo4)
geo$area <- area(geo) 
tab1 <- aggregate(area~ID + GEOID, data=geo, FUN=sum)
tab2 <- aggregate(area~GEOID, data=geo1, FUN=sum)
colnames(tab2)[2] <- "total"
tab1 <- merge(tab1, tab2, all = TRUE)
tab1$prop <- tab1$area / tab1$total
tab1$prop[is.na(tab1$prop)] <- 0
tab_quart <- tab1
# write.csv(tab1, file = "../Cin_data/bus_quarter_mile.csv", row.names=FALSE)
# if(FALSE){
	# plot(geo4, col = "gray80", border = "gray80")
	# plot(geo1, add = TRUE)
# }

########################### Create Buses
geo3 <- readOGR("../data/park_and_bus/Bus_Stops.shp")
geo3 <- spTransform(geo3, CRS("+proj=longlat +datum=WGS84 +no_defs"))
geo3 <- spTransform(geo3, CRS("+proj=utm +datum=WGS84"))
# half mile
geo3 <- gBuffer(geo3, width = 402.336*2,  byid=TRUE)
geo3 <- spTransform(geo3, CRS("+proj=longlat +datum=WGS84"))
geo3$unitID <- "bus"
geo4 <- unionSpatialPolygons(geo3, geo3$unitID)
geo4 <- SpatialPolygonsDataFrame(geo4,  data.frame(ID = 1, row.names = sapply(slot(geo4, "polygons"), function(x) slot(x, "ID"))))
geo <- union(geo1, geo4)
geo$area <- area(geo) 
tab1 <- aggregate(area~ID + GEOID, data=geo, FUN=sum)
tab2 <- aggregate(area~GEOID, data=geo1, FUN=sum)
colnames(tab2)[2] <- "total"
tab1 <- merge(tab1, tab2, all = TRUE)
tab1$prop <- tab1$area / tab1$total
tab1$prop[is.na(tab1$prop)] <- 0
tab_half <- tab1
# write.csv(tab1, file = "../Cin_data/bus_half_mile.csv", row.names=FALSE)
# if(FALSE){
# 	plot(geo4, col = "gray80", border = "gray80")
# 	plot(geo1, add = TRUE)
# }

########################### Create Buses
geo3 <- readOGR("../data/park_and_bus/Bus_Stops.shp")
geo3 <- spTransform(geo3, CRS("+proj=longlat +datum=WGS84 +no_defs"))
data <- data.frame(lon = geo3$LON, lat = geo3$LAT)
overlap <- over(geo3, geo1)
d <- data.frame(count=1:length(overlap$GEOID), 
				GEOID=overlap$GEOID)
counts <- aggregate(count~GEOID, data = d, FUN=length, drop = FALSE)
counts[is.na(counts[,2]), 2] <- 0
tab <- counts
tab <- merge(tab, tab_quart[, c("GEOID", "prop")])
colnames(tab)[2:3] <- c("bus_count", "propBuffQuarter")
tab <- merge(tab, tab_half[, c("GEOID", "prop")])
colnames(tab)[4] <- "propBuffHalf"
write.csv(tab, file = paste0(dir, "cin.bg.bus.csv"), row.names=FALSE)
 

 