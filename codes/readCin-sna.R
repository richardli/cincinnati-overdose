
library(spdep)
library(maptools)
library(spatstat)
library(rgeos)
library(splines)
library(rgdal)
library(plyr)
library(ggplot2)
library(SUMMER)
library(gridExtra)
library(lubridate)
library(RColorBrewer)
library(ggthemes)
library(geosphere)
library(data.table)

source("countPoints.R")   
dir <- "../data/"
mapdir <- "../data/SNA/"
time_start <- "2015-08-01"
time_end <- "2019-02-01"
#########################################################
# Read Map
#########################################################     
geo<-readOGR(paste0(mapdir, 'Cincinnati_SNA_Boundary.shp'))   
nb.r <- poly2nb(geo, queen=F, 
    row.names = geo$SNA_NAME)
mat <- nb2mat(nb.r, style="B",zero.policy=TRUE)
colnames(mat) <- rownames(mat) 
mat <- as.matrix(mat[1:dim(mat)[1], 1:dim(mat)[1]])
N <- dim(mat)[1]
N.full <- N + 2

if(FALSE){
    pdf("../figures/Map.pdf", width=5, height=5)
    plot(geo, col = "gray")
    geo2 <- fortify(geo, region = "SNA_NAME")
    geo2 <- by(geo2, geo2$id, function(x) {Polygon(x[c('long', 'lat')])@labpt})
    centroids <- setNames(do.call("rbind.data.frame", geo2), c('long', 'lat'))
    rownames(centroids) <- names(geo2)  
    text(centroids[,1], centroids[,2], rownames(centroids), cex=.2)
    dev.off()
}


#########################################################
# Read the compiled neighborhood covariates
#########################################################     
cin.neighbors <- read.csv(paste0(dir, 'cin.neighbors.csv'))[, -c(1, 3)]
cin.neighbors<-cin.neighbors[cin.neighbors$NEIGHBORHOOD %in% c("N/a", "N/A") == FALSE,]


#########################################################
# Read the EMS data with recoded neighborhood based on GPS
#  - Count by neighborhood and month index
#########################################################  
Data <- fread(paste0(dir, "Cincinnati_Fire_Incidents__CAD___including_EMS__ALS_BLS-ODonly.csv"))
Data <- data.frame(Data)
Data <- Data[Data$CFD_INCIDENT_TYPE_GROUP %in% c("HEROIN OVERDOSE"), ]
# "OVERDOSE / POSIONING (INGESTION)"
delete <- c("CANCEL", "NOTHING TO REPORT", "DUP: DUPLICATE", "CAN - CANCEL", "TEST: TEST", "CAN:CANCEL", "DUPLICATE INCIDENT - CANCEL", "DUP - DUPLICATE INCIDENT - CAN", "NOT - NOT A DISPOSITION - TEST", "NOT NOT A DISPOSITION", "NV:NO VIOLATION", "315: FALSE ALARM REPORT,HBF: H", "NV - NO VIOLATION", "FALSE ALARM", "315: FALSE ALARM REPORT", "TEST - TEST DISPOSITION", "FALSE ALARM REPORT", "CAN:CANCEL,DUP: DUPLICATE", "ERR: ERROR INCIDENT",          
     "EMSF: FALSE",  "MEDF: MT RESPONSE - FALSE",     
     "FD: FIRE DISREGARD", "",                              
     "EMSD: DISREGARD",  "CN: CANCEL",                    
     "AV: ADVISED", "GI: GOOD INTENT",               
     "DUPF: DUPLICATE", "CN: CANCEL,DUPF: DUPLICATE",    
     "AV: ADVISED,CN: CANCEL",        
     "USED CLEAR BUTTON",
     # "MEDIC DISREGARD", 
     # "EMS DISREGARD",                           
     "DEF: DEFAULT", # only 2
     "DEF: DEFAULT,IN: INVESTIGATION", # only 1
     #"MEDIC TR RESPONDED FALSE",      
     "DUPLICATE INCIDENT", 
     "CN: CANCEL,MEDD: MT DISREGARDE",
     "MALT: SYS MAL W/TRANS", 
     # "NC: NOT CLASSIFIED",            
     "CN: CANCEL,FALA: FIRE FALSE AC", 
     "CN: CANCEL,DEF: DEFAULT",      
     "EXT: EXTINGUISH", "FALA: FIRE FALSE ACCIDENTAL", 
     "EMSF: FALSE,MEDD: MT DISREGARD", "EMS FALSE",
     "FADV: FIRE ADVISED", "REASF: REASSIGNED" ,            
     "MED: MT RESPONSE NO TRANSPORT,", "UNDT: UNDETERMINED W/TRANSPORT", 
     "CANCEL INCIDENT")
Data <- Data[as.character(Data$DISPOSITION_TEXT) %in% delete == FALSE, ]
message("Total number of cases without GPS:  ", sum(is.na(Data$LATITUDE_X)))
Data <- Data[!is.na(Data$LATITUDE_X), ]
date_time <- as.character(Data$CREATE_TIME_INCIDENT)
date <- as.Date(date_time, "%m/%d/%Y %H:%M:%S %p")
Data$date <- date
Data <- Data[which(Data$date >= as.Date(time_start)), ]
Data <- Data[which(Data$date < as.Date(time_end)), ]
message("Types of incidents")
print(sort(table(Data$DISPOSITION_TEXT)))
date <- Data$date
Data$month <- as.numeric(format(date, "%m"))
Data$year <- as.numeric(format(date, "%Y"))
Data$time <- (Data$year - min(Data$year)) * 12 + Data$month 
offset <- min(Data$time)
Data$time <- Data$time - offset  + 1
EMS <- countPoints(Data, "LONGITUDE_X", "LATITUDE_X", geo, "SNA_NAME", "time")$counts
colnames(EMS) <- c("regionname", "time", "counts")
EMS$counts[is.na(EMS$counts)] <- 0
EMS$region <- match(EMS$regionname, colnames(mat))
message(paste("Total incidents:", sum(EMS$counts, na.rm = TRUE)))
if(FALSE){
    pdf("../figures/data-points.pdf", width = 6, height = 4)
    g <- ggplot() + geom_polygon(data=geo, aes(x = long, y = lat, group = group), color = "gray25", fill = "gray90", alpha = .9)+ coord_map() + theme_map() + scale_fill_discrete(guide = FALSE) + geom_point(data = Data, aes(x = LONGITUDE_X, y = LATITUDE_X), size = .4, color = "red", alpha = .2)
    print(g)
    dev.off()
}



#########################################################
# Read and process additional data
#########################################################


#------------------------
# Crime counts - time varying
#------------------------
crime <- fread(paste0(dir, "PDI__Police_Data_Initiative__Crime_Incidents.csv"))
crime <- data.frame(crime)
crime <- crime[!is.na(crime$LATITUDE_X), ]
date_time <- as.character(crime$DATE_REPORTED)
date <- as.Date(date_time, "%m/%d/%Y %H:%M:%S %p")
crime$date <- date
crime <- crime[which(crime$date >= as.Date(time_start)), ]
crime <- crime[which(crime$date < as.Date(time_end)), ]
crime$month <- as.numeric(format(crime$date, "%m"))
crime$year <- as.numeric(format(crime$date, "%Y"))
crime$time <- (crime$year -  min(crime$year)) * 12 + crime$month
offset <- min(crime$time)
crime$time <- crime$time - offset  + 1
Crime <- countPoints(crime, "LONGITUDE_X", "LATITUDE_X", geo, "SNA_NAME", "time")$counts
Crime <- subset(Crime, group > 0 & group <= max(EMS$time))
colnames(Crime) <- c("regionname", "time", "crime")
Crime$region <- match(Crime$regionname, colnames(mat))
Crime$crime[is.na(Crime$crime)] <- 0

#------------------------
# Fast food counts
#------------------------
ff <- read.csv(paste0(dir, "fastfood_latlong.csv"))
count <- countPoints(ff, "X", "Y", geo, "SNA_NAME")
fastfood <- count$counts
colnames(fastfood)[2] <- "fastfood"
cin.neighbors <- merge(cin.neighbors, fastfood, by.x="NEIGHBORHOOD", by.y = "regionname", all = TRUE)
cin.neighbors$fastfood[is.na(cin.neighbors$fastfood)] <- 0


#------------------------
# Fire department counts
#   Note: there is one in Hartwell, GPS lcoation says outside city region
#------------------------
# fire <- read.csv(paste0(dir, "cin.firedepartments.csv"))
# count <- countPoints(fire, "Longitude", "Latitude", geo, "SNA_NAME")
# message(paste("Number of unmappable fire departments: ", sum(is.na(count$raw))))
# fire <- count$counts
# # Manual fix the two points
# fire <- rbind(fire, data.frame(
#   regionname = c("Hartwell", "North Avondale - Paddock Hills"),
#   count = c(1, 1)))
# message("Manually assigned 2 fire departments")
# colnames(fire)[2] <- "fire"
# cin.neighbors <- merge(cin.neighbors, fire, by.x="NEIGHBORHOOD", by.y = "regionname", all = TRUE)
# cin.neighbors$fire[is.na(cin.neighbors$fire)] <- 0
fire <- read.csv(paste0(dir, "cin.firedepartments.csv"))
fire <- mindistPoints(fire, "Longitude", "Latitude", geo, "SNA_NAME")
colnames(fire)[2] <- "fire"
cin.neighbors <- merge(cin.neighbors, fire, by.x="NEIGHBORHOOD", by.y = "regionname", all = TRUE)
# g1 = mapPlot(data=cin.neighbors, geo = geo, by.data = "NEIGHBORHOOD", by.geo = "SNA_NAME", variables = c("fire"))
# g2 = mapPlot(data=cin.neighbors, geo = geo, by.data = "NEIGHBORHOOD", by.geo = "SNA_NAME", variables = c("fire2"))
# grid.arrange(g1, g2, ncol = 2)


#------------------------
# Park proportion
#------------------------
park <- read.csv(paste0(dir, "cin.parks.neighbors.csv"))
cin.neighbors <- merge(cin.neighbors, park, by.x="NEIGHBORHOOD", by.y = "SNA_NAME")


#------------------------
# Pharmacy & hospital counts
#------------------------
pharm <- read.csv(paste0(dir, "cin.pharmacy.hospital.csv"), fileEncoding="latin1")
pharm <- pharm[pharm$Type == "Pharmacy", ]
dist <- mindistPoints(pharm, "X", "Y", geo, "SNA_NAME")
colnames(dist)[2] <- "pharm"
cin.neighbors <- merge(cin.neighbors, dist, by.x="NEIGHBORHOOD", by.y = "regionname", all = TRUE)


hospital <- read.csv(paste0(dir, "cin.pharmacy.hospital.csv"), fileEncoding="latin1")
hospital <- hospital[hospital$Type == "Hospital", ]
dist <- mindistPoints(hospital, "X", "Y", geo, "SNA_NAME")
colnames(dist)[2] <- "pharm"
cin.neighbors <- merge(cin.neighbors, dist, by.x="NEIGHBORHOOD", by.y = "regionname", all = TRUE)



#------------------------
# Federally Qualified Health Center counts
#------------------------
fqhc <- read.csv(paste0(dir, "oh.ky.fqhc.csv"))
dist <- mindistPoints(fqhc, "long", "lat", geo, "SNA_NAME")
colnames(dist)[2] <- "fqhc"
cin.neighbors <- merge(cin.neighbors, dist, by.x="NEIGHBORHOOD", by.y = "regionname", all = TRUE)


#------------------------
# Opioid Treatment Programs
#------------------------
otp<- read.csv(paste0(dir, "oh.ky.otp.csv"))
dist <- mindistPoints(otp, "long", "lat", geo, "SNA_NAME") 
colnames(dist)[2] <- "otp"
cin.neighbors <- merge(cin.neighbors, dist, by.x="NEIGHBORHOOD", by.y = "regionname", all = TRUE)


#------------------------
# Buprenorphine prescribing physicians counts
#------------------------
bup <- read.csv(paste0(dir, "buprenorphinephysicians.csv"))
dist <- mindistPoints(bup, "longitude", "latitude", geo, "SNA_NAME")
colnames(dist)[2] <- "bup"
cin.neighbors <- merge(cin.neighbors, dist, by.x="NEIGHBORHOOD", by.y = "regionname", all = TRUE)


#------------------------
# zoning
#------------------------
zone <- read.csv(paste0(dir, "cin.SNA.zoning.csv"))
colnames(zone)[-1] <- paste0("pc_", tolower(colnames(zone)[-1]))
colnames(zone)[which(colnames(zone) == "pc_urban.mixed")] <- "pc_urban_mixed"
colnames(zone)[which(colnames(zone) == "pc_planned.development")] <- "pc_development"
colnames(zone)[which(colnames(zone) == "pc_otherresidential")] <- "pc_residential_other"
cin.neighbors <- merge(cin.neighbors, zone, by.x="NEIGHBORHOOD", by.y = "SNA_NAME", all = TRUE)

# #------------------------
# # Pharmacy & hospital counts
# #------------------------
# pharm <- read.csv(paste0(dir, "cin.pharmacy.hospital.csv"), fileEncoding="latin1")
# count <- countPoints(pharm, "X", "Y", geo, "SNA_NAME", "Type")
# pharm <- subset(count$counts, group == "Pharmacy")[, -2]
# colnames(pharm)[2] <- "pharm"
# cin.neighbors <- merge(cin.neighbors, pharm, by.x="NEIGHBORHOOD", by.y = "regionname", all = TRUE)
# cin.neighbors$pharm[is.na(cin.neighbors$pharm)] <- 0

# hospital <- subset(count$counts, group == "Hospital")[, -2]
# colnames(hospital)[2] <- "hospital"
# cin.neighbors <- merge(cin.neighbors, hospital, by.x="NEIGHBORHOOD", by.y = "regionname", all = TRUE)
# cin.neighbors$hospital[is.na(cin.neighbors$hospital)] <- 0


# #------------------------
# # Federally Qualified Health Center counts
# #------------------------
# fqhc <- read.csv(paste0(dir, "oh.ky.fqhc.csv"))
# fqhc <- countPoints(fqhc, "long", "lat", geo, "SNA_NAME", NULL)$counts
# colnames(fqhc)[2] <- "fqhc"
# cin.neighbors <- merge(cin.neighbors, fqhc, by.x="NEIGHBORHOOD", by.y = "regionname", all = TRUE)
# cin.neighbors$fqhc[is.na(cin.neighbors$fqhc)] <- 0


# #------------------------
# # Opioid Treatment Programs
# #------------------------
# otp<- read.csv(paste0(dir, "oh.ky.otp.csv"))
# otp <- countPoints(otp, "long", "lat", geo, "SNA_NAME", NULL)$counts
# colnames(otp)[2] <- "otp"
# cin.neighbors <- merge(cin.neighbors, otp, by.x="NEIGHBORHOOD", by.y = "regionname", all = TRUE)
# cin.neighbors$otp[is.na(cin.neighbors$otp)] <- 0


# #------------------------
# # Buprenorphine prescribing physicians counts
# #------------------------
# bup <- read.csv(paste0(dir, "buprenorphinephysicians.csv"))
# bup <- countPoints(bup, "longitude", "latitude", geo, "SNA_NAME", NULL)$counts
# colnames(bup)[2] <- "bup"
# cin.neighbors <- merge(cin.neighbors, bup, by.x="NEIGHBORHOOD", by.y = "regionname", all = TRUE)
# cin.neighbors$bup[is.na(cin.neighbors$bup)] <- 0


#------------------------
# temperature
#------------------------
temp <- read.csv(paste0(dir, "monthly_avg_temperature.csv"))
temp <- as.matrix(temp[1:5, 1:13])
temp <- as.numeric(t(temp[, -1]))
temp <- temp[offset:(max(EMS$time) + offset - 1)]
rain <- read.csv(paste0(dir, "monthly_total_precipitation.csv"))
rain <- as.matrix(rain[1:5, 1:13])
rain <- as.numeric(t(rain[, -1]))
rain <- rain[offset:(max(EMS$time) + offset - 1)]

#################################################
## Organize data
#################################################
Y <- EMS
#### merge all covariates
Y <- merge(Y, cin.neighbors, by.x = "regionname", by.y = "NEIGHBORHOOD")
Y <- merge(Y, Crime, by = c("regionname", "time", "region"))
Y$temperature <- temp[Y$time]
Y$precipitation <- rain[Y$time]




#### Organize covariates
Y <- Y[with(Y, order(Y$region, Y$time)), ]
Y$male = Y$POPULATION - Y$FPOP
Y$pc_white <- Y$WHITE/Y$POPULATION
Y$pc_black <- Y$BLACK/Y$POPULATION
Y$pc_hispanic <- Y$HISPANIC/Y$POPULATION
Y$pc_asian <- Y$ASIAN/Y$POPULATION
Y$pc_ind_alaska <- Y$IND_ALASKA/Y$POPULATION
Y$pc_hawaiian <- Y$HAWAIIAN/Y$POPULATION
Y$pc_other <- Y$OTHER/Y$POPULATION
Y$pc_two_races <- Y$TWO_RACES/Y$POPULATION

# Y$pharm_hospital_fqhc_otp <- Y$pharm + Y$hospital + Y$fqhc + Y$otp

# Update some denominators
#
# !!!
# Redefine PC_BACH and Mratio25 as divided by POPULATION 
# !!! 
Y$pc_bachelor <- Y$BACHELORS / Y$POPULATION
Y$pc_male25 <- Y$MPOP25 / Y$POPULATION
Y$pc_male <- Y$MPOP / Y$POPULATION
Y$pc_age25 <- Y$POP25 / Y$POPULATION
Y$crime_rate <- Y$crime / Y$POPULATION
colnames(Y)[which(colnames(Y) == "POPULATION")] <- "population"
colnames(Y)[which(colnames(Y) == "PC_POVERTY")] <- "pc_poverty"
colnames(Y)[which(colnames(Y) == "MED_HOUSE_INCOME")] <- "household_income"
colnames(Y)[which(colnames(Y) == "PER_CAP_INCOME")] <- "per_cap_income"
colnames(Y)[which(colnames(Y) == "PropBuffHalf")] <- "pc_bus_half"
colnames(Y)[which(colnames(Y) == "PropBuffQuarter")] <- "pc_bus_quarter"
colnames(Y)[which(colnames(Y) == "bus_count")] <- "bus_count"
colnames(Y)[which(colnames(Y) == "PropPark")] <- "pc_park"





# Deal with the holes in the map
N <- dim(mat)[1]
mat2 <- matrix(0, N+2, N+2)
colnames(mat2) <- rownames(mat2) <- c(colnames(mat), "St.Bernard", "Norwood")
mat2[1:N, 1:N] <- mat
set1 <- c("Winton Hills", "Carthage", "Roselawn", "Bond Hill", "North Avondale - Paddock Hills", "Avondale", "Clifton", "Spring Grove Village")
set2 <- c("Bond Hill", "Pleasant Ridge", "Oakley", "Hyde Park", "Evanston", "North Avondale - Paddock Hills")
mat2[set1, "St.Bernard"] <- 1
mat2["St.Bernard", set1] <- 1
mat2[set2, "Norwood"] <- 1
mat2["Norwood", set2] <- 1
mat <- mat2
N.full <- N+2
add <- Y[1:(2*max(Y$time)), ]
add[1:dim(add)[1], ] <- NA
add$regionname <- rep(c("St.Bernard", "Norwood"), each = max(Y$time))
add$time <- rep(1:max(Y$time), 2)
add$region <- rep(c(51, 52), each = max(Y$time))
add$population <- 1 # avoid specifying -Inf offset
Y <- rbind(Y, add)
Y$popdens <- Y$population / Y$NeighbArea # population per sq mile


message(paste("Range of data:", min(Data$date), "-", max(Data$date)))
Y$Date <- (min(Data$date)) %m+% months(Y$time-1)
Y$Year <- year(Y$Date)
## Backward compatibility
Y$value <- Y$counts