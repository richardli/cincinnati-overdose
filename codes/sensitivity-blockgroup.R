###################################################################
# Read 2 Maps and construct the correspondence 
# For sensitivity analysis, change overlap to only completely within
####################################################################     
library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(maptools)
library(reshape2)
library(gridExtra)
library(ggplot2)
library(ggthemes)
map.theme <- theme_bw() + theme(legend.title=element_text(size=rel(0.7)), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
line.theme <- theme_bw() + theme(legend.title=element_text(size=rel(0.7)))  


dir <- "../data/"
mapdir <- "../data/SNA/"
geo1<-readOGR(paste0(mapdir, 'Cincinnati_SNA_Boundary.shp'))   
geo2 <- readOGR("../data/blockgroup/cb_2017_39_bg_500k.shp")
projection(geo2) <- projection(geo1)
geo2$GEOID <- as.character(geo2$GEOID)

# join the two maps
geo <- union(geo1, geo2)
levels(geo$SNA_NAME) <- c(levels(geo$SNA_NAME), "Other")
geo$SNA_NAME[is.na(geo$SNA_NAME)] <- "Other"
# ggplot(geo, aes(x = long, y = lat, group = group)) + geom_polygon()  + coord_map()
# Find area and find max overlap
geo$area <- area(geo) 
# For each field, get area per soil type
tab1 <- aggregate(area~SNA_NAME + GEOID, data=geo, FUN=sum)
tab1 <- dcast(tab1, GEOID ~ SNA_NAME, value.var = "area")
tab1[is.na(tab1)] <- 0
prop <- data.frame(t(apply(tab1[, -1], 1, function(x){x / sum(x)})), check.names = FALSE)
prop$GEOID <- tab1$GEOID 


glist <- NULL
counter <- 1
subsetlist <- NULL
for(p in rev(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))){
	subset <- prop[prop$Other < p, ]
	max <- apply(subset[, 1:51], 1, max)
	SNA <- colnames(subset)[apply(subset[, 1:50], 1, which.max)]
	bg <- subset$GEOID
	geo3 <- subset(geo2, GEOID %in% subset$GEOID) 
	geo3A <- fortify(geo3, region = "GEOID")
	geo3A$neighborhood <- SNA[match(geo3A$id, bg)]
	# Get block groups in the holes
	box <- rbind(crop(geo2, extent(-84.515, -84.482, 39.14089, 39.19)), 
				 crop(geo2, extent(-84.482, -84.433, 39.14089, 39.18))) 
	box <- subset(box, box$GEOID %in% geo3$GEOID == FALSE)
	nn <- length(bg) 
	glist[[counter]] <- ggplot() + geom_polygon(data=geo3A, aes(x = long, y = lat, group = group, fill = neighborhood), color = "gray30", alpha = 1) + geom_polygon(data=box, aes(x = long, y = lat, group = group), fill = "gray70", color = "gray50", alpha = 0.8) + coord_map() + theme_map() + scale_fill_discrete(guide = FALSE) + ggtitle(paste0("Overlap > ", (1-p)*100, "%: ", nn, " block groups")) + theme(title = element_text(size=16, vjust = -1, hjust = 20))
	subsetlist[[counter]] <- subset$GEOID
	counter <- counter + 1
}

pdf("../figures/map-hole2.pdf", width = 5.5*3, height = 4*3)
grid.arrange(grobs = glist)
dev.off()


#######################################################################
#
#		Repeat analysis on different maps: mapItr
#
#########################################################################
prefix <- ""
VIS <- FALSE
is.bg <- TRUE
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
for(mapItr in 1:9){
	geofull<-subset(geo2, GEOID %in% c(subsetlist[[mapItr]], box$GEOID))
	nb.r <- poly2nb(geofull, queen=F, row.names = geofull$GEOID)
	mat <- nb2mat(nb.r, style="B",zero.policy=TRUE)
	colnames(mat) <- rownames(mat) 
	mat <- as.matrix(mat[1:dim(mat)[1], 1:dim(mat)[1]])
	geo <- subset(geo2, GEOID %in% c(subsetlist[[mapItr]]))
	hole <- which(colnames(mat) %in% subsetlist[[mapItr]] == FALSE)
	mat <- rbind(cbind(mat[-hole, -hole], mat[-hole, hole]), 
	             cbind(mat[hole, -hole], mat[hole, hole]))
	N.full <- dim(mat)[1]
	N <- dim(mat)[1] - length(hole)

	geoA <- geo
	geoA$neighborhood <- geobg$table$SNA_NAME[match(geoA$GEOID, geobg$table$bg)]
	geoA <- fortify(geoA, region = "neighborhood")
	geoA <- geo
	geoA$neighborhood <- geobg$table$SNA_NAME[match(geoA$GEOID, geobg$table$bg)]
	geoA <- fortify(geoA, region = "neighborhood")

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
	cin.neighbors <- subset(cin.neighbors, GEOID %in% colnames(mat))
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
	data <- cbind(Intercept = 1, Y)
	data$Queensgate <- 0
	data$Queensgate[data$regionname == "390610263001"] <- 1
	T <- max(data$time)
	N.full <- max(data$region)
	N<- dim(mat)[1] - length(hole)
	byGEO <- "GEOID"
	byDATA <- "regionname"
	postfix <- "-bg"
	fixed.group <- 2

	##################################################
	##            Model fitting
	##################################################
	data <- cbind(Intercept = 1, Y)
	data$Queensgate <- 0
	data$Queensgate[data$regionname == "390610263001"] <- 1
	data$log_population <- log(data$population)
	data$population_1e3 <- data$population / 1000
	t_use <- T
	library(INLA)
	inla.setOption(mkl=TRUE) 
	data[data$time > t_use, "counts"] <- NA
	data$time.iid <- data$time
	data$time.int <- data$time
	data$region.iid <- data$region
	data$region.int <- data$region
	regiontime <- data.frame(time = rep(1:T, each=N), 
		region = rep(1:N, T), regiontime = 1:(N*T))
	data <- merge(data, regiontime, by = c("time", "region"))
	data <- data[order(data$regiontime), ]

	# Scale down the two large covariates
	scale_med_house_income <- 1e5 # make it 0.07-1.16
	data$household_income <- data$household_income / scale_med_house_income
	scale_per_cap_income <- 1e5 # make it 0.03-0.80
	data$per_cap_income <- data$per_cap_income / scale_per_cap_income
	scale_home_value <- 1e6 # make it 0.02-0.60
	data$home_value <- data$home_value / scale_home_value  
	scale_home_value_change <- 1e6 # make it -0.16-0.27
	data$home_value_change <- data$home_value_change / scale_home_value_change 
	data[, colnames(data) %in% c("pharm", "hospital", "fqhc", "otp", "bup")] <- data[, colnames(data) %in% c("pharm", "hospital",  "fqhc", "otp", "bup")] / 10

	formula.group <- 5
	source("formulalist.R")
	formula0 <- formula[["AR1-PC-S"]]


	tt <- Sys.time()
	fit0 = inla(formula0, data = data, family = c("poisson"), control.predictor = list(compute = TRUE, link=1),control.compute = list(config=TRUE), verbose = TRUE, control.inla = list(int.strategy = "ccd"))#, lincomb = all.lc)
	fit0$computation_time <- Sys.time() - tt
	print(fit0$computation_time)
	out <- list(fixed = fit0$summary.fixed, random = fit0$summary.random, mat = mat, geo = geo)
	save(out, file = paste0("rda/map", mapItr, ".rda"))

}

if(FALSE){
	library(SUMMER)
	library(ggplot2)
	library(gridExtra)
	library(scales)
	map.theme <- theme_bw() + theme(legend.title=element_text(size=rel(0.7)), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
	line.theme <- theme_bw() + theme(legend.title=element_text(size=rel(0.7)))  
	

	fixed <- space <- time <- NULL
	overlap <- c(1:9)/10
	maps <- NULL
	xlim <- ylim <- NA
	for(mapItr in 1:9){
		load(paste0("rda/map", mapItr, ".rda"))
		N <- length(out$geo$GEOID)
		N.full <- dim(out$mat)[1]
		fixed <- rbind(fixed, data.frame(out$fixed, overlap = overlap[mapItr], covariate = rownames(out$fixed)))
		tmp <- data.frame(out$random[["region"]][1:N, ], overlap = overlap[mapItr], regionname = rownames(out$mat)[1:N])
		tmp$rr <- exp(tmp$mean)
		space <- rbind(space, tmp)
		time <- rbind(time, data.frame(out$random[["time"]], overlap = overlap[mapItr]))
		maps[[mapItr]] <- mapPlot(data = tmp, geo = out$geo, variables = c("rr"), labels = c("Relative Risk"), by.data = "regionname", by.geo = "GEOID", border = "gray20", removetab=TRUE) + map.theme + coord_map() + theme(panel.border = element_blank()) + theme(strip.background = element_blank(), strip.text.x = element_text( vjust = -0.1, size = 16)) + theme(legend.position = c(0.15, 0.72), legend.key.size = unit(.85,"line"), plot.margin=margin(t=-1,b=-1,l=0,r=0, unit="cm"), legend.title = element_text(size = 9)) 
	}


	fixed$label <- paste0("Overlap > ", fixed$overlap*100, "%")
	fixed$mean <- exp(fixed$mean)
	fixed$`X0.025quant` <- exp(fixed$`X0.025quant`)
	fixed$`X0.975quant` <- exp(fixed$`X0.975quant`)
	fixed <- subset(fixed, covariate != "Intercept")
	fixed$overlap <- factor(fixed$overlap)
	fixed$covariate <- factor(fixed$covariate, levels = rev(rownames(out$fixed)[-1]))
	g1 <- ggplot(data = fixed, aes(x = covariate, y = mean, color = label)) + 
		geom_errorbar(data = subset(fixed, overlap == 0.1), aes(x = covariate, ymax = `X0.975quant`, ymin = `X0.025quant`), position=position_dodge(width=-0.5), alpha = 0.5, width = 0, color = "black") + 
		geom_point(data = fixed, position=position_dodge(width=-0.5), alpha = 0.8, size = 1) +
		geom_point(data = subset(fixed, overlap == 0.1), position=position_dodge(width=-0.5), size = 1, color = "black", alpha = 0.5) + 
		geom_errorbar(data = fixed, aes(x = covariate, ymax = `X0.975quant`, ymin = `X0.025quant`), position=position_dodge(width=-0.5), alpha = 0.8, width = 0, size = 0.2)
	g1 <- g1 + coord_flip() + line.theme  + 
	      guides(color = guide_legend(reverse = FALSE)) + theme(legend.key.height=unit(1.5,"line")) + xlab("Coefficients") + ylab("Relative Risk")+ scale_color_manual("Inclusion Criterion", values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#fdbf6f','#a65628','#f781bf','#999999')) + scale_y_continuous(trans = "log") + geom_hline(yintercept = 1, alpha = 0.5, linetype = "dashed")

	ggsave(plot = g1, file = "../figures/inclusion-fixed.pdf", width = 10, height = 12)
	 

	breaks <- exp(seq(min(space$mean), max(space$mean), length.out = 5))
	for(i in 1:length(maps)){
		maps[[i]] <- maps[[i]] + scale_fill_distiller("Relative Risk", palette = "RdBu", trans = "log", breaks = breaks, lim=range(breaks)+c(-0.01, 0.01), label = function(x){options( digits = 2);format( x, scientific = FALSE)})+ ggtitle(paste0("Overlap > ", i*10, "%")) 
	}
	g2 <- grid.arrange(grobs = maps, ncol = 3)
	ggsave(plot = g2, file = "../figures/inclusion-space.pdf", width = 14, height = 10)

	load("../slurm/datastep.RData")	
	time$Date <- Y$Date[match(time$ID, Y$time)]
	time$label <- paste0("Overlap > ", time$overlap*100, "%")
	g2 <- ggplot(time, aes(x = Date, y = mean, ymax = `X0.975quant`, ymin = `X0.025quant`), alpha = 0.8) + geom_point(color = "darkred") + geom_line(color = "darkred") + 
		geom_errorbar(width = 0.3, alpha = 0.5, color = "darkred") + 
		line.theme  + theme(legend.key.height=unit(1.5,"line"), legend.position = "bottom")  + xlab("") + ylab("Relative Risk")+ scale_color_brewer(palette = "Set1")+ scale_x_date(breaks = "6 month", labels = date_format("%b %Y"), lim = c(min(Y$Date)-10, max(Y$Date)+10)) + facet_wrap(~label, ncol = 3)
	ggsave(plot = g2, file = "../figures/inclusion-time.pdf", width = 16, height = 8)


}

