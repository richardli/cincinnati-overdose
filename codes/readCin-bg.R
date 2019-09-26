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
load("../data/Cin_block_group.rda")   
geofull<-geobg$geofull
nb.r <- poly2nb(geofull, queen=F, row.names = geofull$GEOID)
mat <- nb2mat(nb.r, style="B",zero.policy=TRUE)
colnames(mat) <- rownames(mat) 
mat <- as.matrix(mat[1:dim(mat)[1], 1:dim(mat)[1]])
geo <- geobg$geo
# rgdal::writeOGR(obj = geo, dsn = "../Cin_data/", layer="blockgroups", driver="ESRI Shapefile")
# geo1 <- readOGR("../Cin_data/blockgroups.shp")

hole <- which(colnames(mat) %in% geo$GEOID == FALSE)
mat <- rbind(cbind(mat[-hole, -hole], mat[-hole, hole]), 
             cbind(mat[hole, -hole], mat[hole, hole]))
N.full <- dim(mat)[1]
N <- dim(mat)[1] - length(hole)

geoA <- geo
geoA$neighborhood <- geobg$table$SNA_NAME[match(geoA$GEOID, geobg$table$bg)]
geoA <- fortify(geoA, region = "neighborhood")

if(FALSE){
    pdf("../figures/Map-bg.pdf", width=10, height=10)
    plot(geo, col = "gray")
    geo2 <- fortify(geo, region = "GEOID")
    geo2 <- by(geo2, geo2$id, function(x) {Polygon(x[c('long', 'lat')])@labpt})
    centroids <- setNames(do.call("rbind.data.frame", geo2), c('long', 'lat'))
    rownames(centroids) <- names(geo2)  
    text(centroids[,1], centroids[,2], rownames(centroids), cex=.2)
    dev.off()
}


f <- read.csv(paste0(dir, "2017.acs.covariates2.csv"), check.names = FALSE)
f1 <- read.csv(paste0(dir, "2009-2013.acs.home.value.csv"), check.names = FALSE)
f$home_value_13 <- f1[match(f[,1], f1[,1]), "median.home.value"]
f$home_value_13 <- as.numeric(as.character(f$home_value_13))
f$id2 <- gsub("1500000US", "", as.character(f$id))
f$id2 <- as.character(f$id2)
f <- f[match(colnames(mat), f$id2), ]
colnames(f)[2] <- "GEOID"
cin.neighbors <- data.frame(f[match(colnames(mat), f$GEOID), ])
cin.neighbors$home_value <- as.numeric(as.character(cin.neighbors$med.home.val))
cin.neighbors$home_value_change <- cin.neighbors$home_value - cin.neighbors$home_value_13
cin.neighbors$MPOP <- cin.neighbors$mpop
cin.neighbors$FPOP <- cin.neighbors$fpop 
cin.neighbors$POP18_24 <- apply(cin.neighbors[, c("m.18to19", "m.20", "m.21", "m.22to24", "f.18to19", "f.20", "f.21", "f.22to24")], 1, sum)
cin.neighbors$POP25_34 <- apply(cin.neighbors[, c("m.25to29", "m.30to34", "f.25to29", "f.30to34")], 1, sum) 
cin.neighbors$POP35_49 <- apply(cin.neighbors[, c("m.35to39", "f.35to39", "m.40to44", "f.40to44", "m.45to49", "f.45to49" )], 1, sum) 
cin.neighbors$POP50_64 <- apply(cin.neighbors[, c("m.50to54", "f.50to54", "m.55to59", "f.55to59", "m.60to61", "f.60to61", "m.62to64", "f.62to64")], 1, sum) 
cin.neighbors$POP65 <- apply(cin.neighbors[, c("m.65to66", "f.65to66", "m.67to69", "f.67to69", "m.70to74", "f.70to74", "m.75to79", "f.75to79", "m.80to84", "f.80to84", "m.85up", "f.85up")], 1, sum) 


#########################################################
# Read the EMS data with recoded neighborhood based on GPS
#  - Count by neighborhood and month index
#########################################################  
Data <- fread(paste0(dir, "Cincinnati_Fire_Incidents__CAD___including_EMS__ALS_BLS-ODonly.csv"))
Data <- data.frame(Data)
if(prefix == "broad"){
    Data <- Data[Data$CFD_INCIDENT_TYPE_GROUP %in% c("HEROIN OVERDOSE","OVERDOSE / POSIONING (INGESTION)"), ]
}else{
    Data <- Data[Data$CFD_INCIDENT_TYPE_GROUP %in% c("HEROIN OVERDOSE"), ]
}
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
EMS <- countPoints(Data, "LONGITUDE_X", "LATITUDE_X", geo, "GEOID", "time")$counts

colnames(EMS) <- c("regionname", "time", "counts")
EMS$counts[is.na(EMS$counts)] <- 0
EMS$region <- match(EMS$regionname, colnames(mat))
for(i in 1:(dim(mat)[1] - length(hole))){
    if(i %in% EMS$region == FALSE){
        EMS <- rbind(EMS, data.frame(regionname = colnames(mat)[i], time = 1:max(EMS$time), counts = 0, region = i))
    }
}
for(i in (dim(mat)[1] - length(hole)+1) : dim(mat)[1]){
    if(i %in% EMS$region == FALSE){
        EMS <- rbind(EMS, data.frame(regionname = colnames(mat)[i], time = 1:max(EMS$time), counts = NA, region = i))
    }
}
message(paste("Total incidents:", sum(EMS$counts, na.rm = TRUE)))
if(FALSE){
    pdf("../figures/data-points-bg.pdf", width = 6, height = 4)
    g <- ggplot() + geom_polygon(data=geo, aes(x = long, y = lat, group = group), color = "gray50", fill = "gray90", alpha = .9)+ coord_map() + theme_map() + scale_fill_discrete(guide = FALSE) + geom_point(data = Data, aes(x = LONGITUDE_X, y = LATITUDE_X), size = .4, color = "red", alpha = .2) 
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
Crime <- countPoints(crime, "LONGITUDE_X", "LATITUDE_X", geo, "GEOID", "time")$counts
Crime <- subset(Crime, group > 0 & group <= max(EMS$time))
colnames(Crime) <- c("regionname", "time", "crime")
Crime$region <- match(Crime$regionname, colnames(mat))
Crime$crime[is.na(Crime$crime)] <- 0

#------------------------
# Fast food counts
#------------------------
ff <- read.csv(paste0(dir, "fastfood_latlong.csv"))
count <- countPoints(ff, "X", "Y", geo, "GEOID")
fastfood <- count$counts
colnames(fastfood)[2] <- "fastfood"
cin.neighbors <- merge(cin.neighbors, fastfood, by.x="GEOID", by.y = "regionname", all = TRUE)
cin.neighbors$fastfood[is.na(cin.neighbors$fastfood)] <- 0


#------------------------
# Fire department distance
#   Note: there is one in Hartwell, GPS lcoation says outside city region
#------------------------
fire <- read.csv(paste0(dir, "cin.firedepartments.csv"))
fire <- mindistPoints(fire, "Longitude", "Latitude", geo, "GEOID")
colnames(fire)[2] <- "fire"
cin.neighbors <- merge(cin.neighbors, fire, by.x="GEOID", by.y = "regionname", all = TRUE)

#------------------------
# Park proportion TODO
#------------------------
park <- read.csv(paste0(dir, "cin.parks.bg.csv"))
cin.neighbors <- merge(cin.neighbors, park, by.x="GEOID", by.y = "bg", all = TRUE)
bus <- read.csv(paste0(dir, "cin.bg.bus.csv"))
cin.neighbors <- merge(cin.neighbors, bus, by="GEOID", all = TRUE)

#------------------------
# Pharmacy & hospital counts
#------------------------
pharm <- read.csv(paste0(dir, "cin.pharmacy.hospital.csv"), fileEncoding="latin1")
pharm <- pharm[pharm$Type == "Pharmacy", ]
dist <- mindistPoints(pharm, "X", "Y", geo, "GEOID")
colnames(dist)[2] <- "pharm"
cin.neighbors <- merge(cin.neighbors, dist, by.x="GEOID", by.y = "regionname", all = TRUE)


hospital <- read.csv(paste0(dir, "cin.pharmacy.hospital.csv"), fileEncoding="latin1")
hospital <- hospital[hospital$Type == "Hospital", ]
dist <- mindistPoints(hospital, "X", "Y", geo, "GEOID")
colnames(dist)[2] <- "hospital"
cin.neighbors <- merge(cin.neighbors, dist, by.x="GEOID", by.y = "regionname", all = TRUE)


#------------------------
# Federally Qualified Health Center counts
#------------------------
fqhc <- read.csv(paste0(dir, "oh.ky.fqhc.csv"))
dist <- mindistPoints(fqhc, "long", "lat", geo, "GEOID")
colnames(dist)[2] <- "fqhc"
cin.neighbors <- merge(cin.neighbors, dist, by.x="GEOID", by.y = "regionname", all = TRUE)


#------------------------
# Opioid Treatment Programs
#------------------------
otp<- read.csv(paste0(dir, "oh.ky.otp.csv"))
dist <- mindistPoints(otp, "long", "lat", geo, "GEOID") 
colnames(dist)[2] <- "otp"
cin.neighbors <- merge(cin.neighbors, dist, by.x="GEOID", by.y = "regionname", all = TRUE)


#------------------------
# Buprenorphine prescribing physicians counts
#------------------------
bup <- read.csv(paste0(dir, "buprenorphinephysicians.csv"))
dist <- mindistPoints(bup, "longitude", "latitude", geo, "GEOID")
colnames(dist)[2] <- "bup"
cin.neighbors <- merge(cin.neighbors, dist, by.x="GEOID", by.y = "regionname", all = TRUE)


#------------------------
# zoning
#------------------------
zone <- read.csv(paste0(dir, "cin.bg.zoning.csv"))
colnames(zone)[-1] <- paste0("pc_", tolower(colnames(zone)[-1]))
colnames(zone)[which(colnames(zone) == "pc_urban.mixed")] <- "pc_urban_mixed"
colnames(zone)[which(colnames(zone) == "pc_planned.development")] <- "pc_development"
colnames(zone)[which(colnames(zone) == "pc_otherresidential")] <- "pc_residential_other"
cin.neighbors <- merge(cin.neighbors, zone, by.x="GEOID", by.y = "GEOID", all = TRUE)


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
Y <- merge(Y, cin.neighbors, by.x = "regionname", by.y = "GEOID", all = TRUE)
Y <- merge(Y, Crime, by = c("regionname", "time", "region"), all = TRUE)
Y$temperature <- temp[Y$time]
Y$precipitation <- rain[Y$time]


#### Organize covariates
Y <- Y[with(Y, order(Y$region, Y$time)), ]
Y$male = Y$population - Y$FPOP
Y$pc_white <- Y$white/Y$population
Y$pc_black <- Y$black/Y$population
Y$pc_hispanic <- Y$hispanic/Y$population
Y$pc_asian<- Y$asian/Y$population
Y$pc_ind_alaska <- Y$ind.alaska/Y$population
Y$pc_hawaiian <- Y$hawaiian/Y$population
Y$pc_other <- Y$other/Y$population
Y$pc_two_races <- Y$two.races/Y$population

Y$pc_bachelor <- (Y$bachelors + Y$masters + Y$professional + Y$doctorate) / Y$population
# Y$pc_male25 <- Y$MPOP25 / Y$population
Y$pc_male <- Y$male / Y$population
Y$pc_age18_24 <- Y$POP18_24 / Y$population
Y$pc_age25_34 <- Y$POP25_34 / Y$population
Y$pc_age35_49 <- Y$POP35_49 / Y$population
Y$pc_age50_64 <- Y$POP50_64 / Y$population
Y$pc_age65up <- Y$POP65 / Y$population
Y$crime_rate <- Y$crime / Y$population
Y$pc_poverty <- Y$poverty / Y$fam.houses

# Define Appalachian (SAS book)
# 1. Greater than 23% of the families are below the poverty level
# 2. Less than 41.0% of families are African American
# 3. Less than 80% of the persons 25 years or older are high school graduates
# 4. More than 7% of the persons 16-19 years old who are not in school are not high school graduates
# 5. More than 62% of the persons 16-19 years old are jobless (includes those unemployed and those not in the civilian labor force)
# 6. More than 3 persons per average family
# If at least four criteria were met, the neighborhood was identified as having a significant Appalachian population, but not as long as the African American population was more than 41.0 (the city wide) percentage.
Y$Appalachian <- 0
Y$Appalachian <- Y$Appalachian + as.numeric(Y$pc_poverty > .23)
Y$Appalachian <- Y$Appalachian + as.numeric(Y$pc_black < .41)
Y$Appalachian <- Y$Appalachian + as.numeric(Y$pc_high_school_graduates_25older < 0.8)
Y$Appalachian <- Y$Appalachian + as.numeric(Y$pc_not_high_school_graduates_16to19 > 0.07)
Y$Appalachian <- Y$Appalachian + as.numeric(Y$jobless_16_19 > 0.62)
Y$Appalachian <- Y$Appalachian + Y$median_size_over_3
table(Y$Appalachian)
Y$Appalachian <- as.numeric(Y$Appalachian > 3)
Y$Appalachian[Y$pc_black >= 0.41] <- 0


colnames(Y)[which(colnames(Y) == "med.income")] <- "household_income"
colnames(Y)[which(colnames(Y) == "pc.income")] <- "per_cap_income"
colnames(Y)[which(colnames(Y) == "PropPark")] <- "pc_park"
colnames(Y)[which(colnames(Y) == "propBuffQuarter")] <- "pc_bus_quarter"
colnames(Y)[which(colnames(Y) == "propBuffHalf")] <- "pc_bus_half"
Y$household_income <- as.numeric(as.character(Y$household_income))
Y$popdens <- Y$population / (Y$bgArea * 0.00000038610) # population per sq mile (raw data in square meter)

message(paste("Range of data:", min(Data$date), "-", max(Data$date)))
Y$Date <- (min(Data$date)) %m+% months(Y$time-1)
Y$Year <- year(Y$Date)
## Backward compatibility
Y$value <- Y$counts


better_names <- function(x){
    require('plyr')
    x = revalue(x, c("log_population"      = "Log of Population", 
                                      "pc_male"             = "Proportion Male", 
                                      "pc_age18_24"         = "Proportion Aged 18-24",
                                      "pc_age25_34"         = "Proportion Aged 25-34",
                                      "pc_age35_49"         = "Proportion Aged 35-49",
                                      "pc_age50_64"         = "Proportion Aged 50-64",
                                      "pc_age65up"          = "Proportion Aged 65 up",
                                      "pc_white"            = "Proportion White",
                                      "pc_black"            = "Proportion Black",
                                      "pc_asian"            = "Proportion Asia",
                                      "pc_two_races"        = "Proportion Two or More Races",
                                      "pc_hispanic"         = "Proportion Hispanic",
                                      "household_income"    = "Median Household Income ($100K)",
                                      "per_cap_income"      = "Per Capita Income ($100K)",
                                      "pc_poverty"          = "Proportion in Poverty",
                                      "home_value"          = "Median Home Value ($1M)",
                                      "home_value_change"   = "Change in Median Home Value ($1M)",
                                      "pc_bachelor"         = "Proportion Bachelor's Degree or Higher",
                                      "pc_park"             = "Proportion Parks",
                                      "pc_bus_quarter"      = "Proportion 1/4-mile Bus Coverage",
                                      "fire"                = "Distance to Fire Departments (10 miles)",
                                      "pharm"               = "Distance to Pharmacies (10 miles)",
                                      "hospital"            = "Distance to Hospitals (10 miles)",
                                      "fqhc"                = "Distance to FQHC (10 miles)",
                                      "otp"                 = "Distance to OTP (10 miles)",
                                      "bup"                 = "Distance to Buprenorphine Practitioners (10 miles)",
                                      "fastfood"            = "Number of Fast Food Restaurants",
                                      "pc_singlefamily"     = "Proportion Single Family Zoning",
                                      "pc_residential_other"= "Proportion Other Residential Zoning",
                                      "pc_office"           = "Proportion Office Zoning",
                                      "pc_commercial"       = "Proportion Commercial Zoning",
                                      "pc_urban_mixed"      = "Proportion Urban Mixed Zoning",
                                      "pc_downtown"         = "Proportion Downtown Development Zoning",
                                      "pc_manufacturing"    = "Proportion Manufacturing Zoning",
                                      "pc_riverfront"       = "Proportion Riverfront Zoning",
                                      "pc_development"      = "Proportion Planned Development Zoning",
                                      "temperature"         = "Temperature (Fahrenheit)",
                                      # "temperature"         = expression(paste("Temperature, ",degree, "F")),
                                      "precipitation"       = "Precipitation (Inch)",
                                      "crime_rate"          = "Crime Rate Per Population"))
    return(x)
}
