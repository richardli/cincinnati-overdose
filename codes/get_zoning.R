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
library(reshape2)

########################### Create Parks
dir <- "../data/"
mapdir <- "../data/"
geo1<-readOGR(paste0(mapdir, 'SNA/Cincinnati_SNA_Boundary.shp'))  
geo2 <- readOGR(paste0(mapdir, 'blockgroup/blockgroups.shp'))
geo0 <- readOGR("../data/zoning/cintzone.shp")

# Classification based on 
# 1. https://www.cincinnati-oh.gov/planning/assets/File/CFBC_1703_FBC_FinalDraft_021513_web(1).pdf
# 2. https://library.municode.com/oh/cincinnati/codes/code_of_ordinances?nodeId=TIXIZOCOCI_CH1400GEPRRUME_S1400-17ZOMA
# Caveats:
# T4N can contains a mix of single family house as well.
# T5MS can be either residential or commercial, put in residential here.
# T5F can also be light-industrial, put in commercial here.

geo0$TYPE <- "Other"
geo0$TYPE[geo0$ZONE_TYPE %in% c("SF-20", "SF-10", "SF-6", "SF-4", "SF-2", "T3E", "T3N")] <- "SingleFamily"
geo0$TYPE[geo0$ZONE_TYPE %in% c("RMX", "RM-2.0", "RM-1.2", "RM-0.7", "T4N.MF", "T4N.MF-O", "T4N.SF", "T4N.SF-O", "T5MS", "T5MS-O", "T5N.LS", "T5N.LS-O", "T5N.SS", "T5N.SS-O", "IR")] <- "OtherResidential"
geo0$TYPE[geo0$ZONE_TYPE %in% c("OL", "OG")] <- "Office"
geo0$TYPE[geo0$ZONE_TYPE %in% c("CN-P", "CC-P", "CN-M", "CC-M", "CC-A", "CG-A",  "T5F")] <- "Commercial"
geo0$TYPE[geo0$ZONE_TYPE %in% c("DD")] <- "Downtown"
geo0$TYPE[substr(geo0$ZONE_TYPE, 1,2) == c("PD")] <- "Planned Development"
geo0$TYPE[geo0$ZONE_TYPE %in% c("MA", "ML", "ME", "MG")] <- "Manufacturing"
geo0$TYPE[geo0$ZONE_TYPE %in% c( "RF-M", "RF-C", "RF-R")] <- "Riverfront"
geo0$TYPE[geo0$ZONE_TYPE %in% c("UM")] <- "Urban Mixed"
geo0$TYPE[geo0$ZONE_TYPE %in% c("PR")] <- "Recreation"
geo0$area <- area(geo0) 
agg <- aggregate(area ~ TYPE, geo0, sum)
print(agg[order(agg[,2]), ])
# (clean the polygon a bit for the union option)
gIsValid(geo0, reason = T)
# geo0c <- gSimplify(geo0, tol = 0.00001)
geo0c <- gBuffer(geo0, byid=TRUE, width=0)
gIsValid(geo0c, reason = T)
geo0c <- spTransform(geo0c, CRS("+proj=longlat +datum=WGS84"))

# join the two maps
geo1A <- union(geo1, geo0c)
geo1A$area <- area(geo1A)
tab1 <- aggregate(area~TYPE + SNA_NAME, data=geo1A, FUN=sum, drop = FALSE)
tab2 <- aggregate(area~SNA_NAME, data=geo1A, FUN=sum)
# tab3 <- aggregate(area~TYPE, data=geo1A, FUN=sum)
# print(tab3[order(tab3[, 2]), ])
colnames(tab2)[2] <- "total"
tab1 <- merge(tab1, tab2, all = TRUE)
tab1[, c(3,4)] <- tab1[, c(3,4)] * 0.00000038610
tab1$prop <- tab1$area / tab1$total
tab1$prop[is.na(tab1$prop)] <- 0
tab2 <- dcast(tab1, SNA_NAME ~ TYPE, value.var="prop")
write.csv(tab2, file = paste0(dir, "cin.SNA.zoning.csv"), row.names=FALSE)

# join the two maps 
geo2A <- union(geo2, geo0c)
geo2A$area <- area(geo2A)
tab1 <- aggregate(area~TYPE + GEOID, data=geo2A, FUN=sum, drop = FALSE)
tab2 <- aggregate(area~GEOID, data=geo2A, FUN=sum)
colnames(tab2)[2] <- "total"
tab1 <- merge(tab1, tab2, all = TRUE)
tab1[, c(3,4)] <- tab1[, c(3,4)] * 0.00000038610
tab1$prop <- tab1$area / tab1$total
tab1$prop[is.na(tab1$prop)] <- 0
tab2 <- dcast(tab1, GEOID ~ TYPE, value.var="prop")
write.csv(tab2, file = paste0(dir, "cin.bg.zoning.csv"), row.names=FALSE)
