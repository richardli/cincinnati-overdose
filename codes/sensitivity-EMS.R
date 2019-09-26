#######################################################################
## This script conducts come additional regression and compare the results with the main regression in the study. See details in sensitivity analysis
######################################################################

library(sf)
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
library(scales)
prefix <- ""
VIS <- FALSE
is.bg <- TRUE
## Read all data to be used
prefix <- "broad"
source("readCin-bg.R")
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
map.theme <- theme_bw() + theme(legend.title=element_text(size=rel(0.7)), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
line.theme <- theme_bw() + theme(legend.title=element_text(size=rel(0.7)))  
#########################################################################
## Additional Data
#########################################################################
Data <- fread("../data/Cincinnati_Fire_Incidents__CAD___including_EMS__ALS_BLS.csv")
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
     "CANCEL INCIDENT",
     # Additional  
     "EMS DISREGARD", 
     "FALSE FIRE ALARM - ACCIDENTAL", 
     "SYSTEM MALFUNCTION", 
     "MAL: SYSTEM MALFUNCTION", 
     "FIRE DISREGARD",
     "FALSE FIRE ALARM - MALICIOUS",
     "FALM: FIRE FALSE MALICIOUS"
     )
Data <- Data[as.character(Data$DISPOSITION_TEXT) %in% delete == FALSE, ]
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
Data <- data.frame(Data)
#########################################################################
## Non-heroin overdose
Nonheroin <- countPoints(subset(Data, CFD_INCIDENT_TYPE_GROUP == "OVERDOSE / POSIONING (INGESTION)"), "LONGITUDE_X", "LATITUDE_X", geo, "GEOID", "time")$counts
colnames(Nonheroin) <- c("regionname", "time", "counts")
Nonheroin$counts[is.na(Nonheroin$counts)] <- 0
Nonheroin$region <- match(Nonheroin$regionname, colnames(mat))
for(i in 1:(dim(mat)[1] - length(hole))){
    if(i %in% Nonheroin$region == FALSE){
        Nonheroin <- rbind(Nonheroin, data.frame(regionname = colnames(mat)[i], time = 1:max(Nonheroin$time), counts = 0, region = i))
    }
}
for(i in (dim(mat)[1] - length(hole)+1) : dim(mat)[1]){
    if(i %in% Nonheroin$region == FALSE){
        Nonheroin <- rbind(Nonheroin, data.frame(regionname = colnames(mat)[i], time = 1:max(Nonheroin$time), counts = NA, region = i))
    }
}
message(paste("Total Non-heroin overdose incidents:", sum(Nonheroin$counts, na.rm = TRUE)))
colnames(Nonheroin)[3] <- "NonHeroin"

#########################################################################
## Person Down
Down <- countPoints(subset(Data, CFD_INCIDENT_TYPE_GROUP %in% c("PERSON DOWN", "UNKNOWN PROBLEM (PERSON DOWN)")), "LONGITUDE_X", "LATITUDE_X", geo, "GEOID", "time")$counts
colnames(Down) <- c("regionname", "time", "counts")
Down$counts[is.na(Down$counts)] <- 0
Down$region <- match(Down$regionname, colnames(mat))
for(i in 1:(dim(mat)[1] - length(hole))){
    if(i %in% Down$region == FALSE){
        Down <- rbind(Down, data.frame(regionname = colnames(mat)[i], time = 1:max(Down$time), counts = 0, region = i))
    }
}
for(i in (dim(mat)[1] - length(hole)+1) : dim(mat)[1]){
    if(i %in% Down$region == FALSE){
        Down <- rbind(Down, data.frame(regionname = colnames(mat)[i], time = 1:max(Down$time), counts = NA, region = i))
    }
}
message(paste("Total Person Down incidents:", sum(Down$counts, na.rm = TRUE)))
colnames(Down)[3] <- "Down"


#########################################################################
## Other Case: a falsification test
Other <- countPoints(subset(Data, CFD_INCIDENT_TYPE_GROUP %in% c("PREGNANCY / CHILDBIRTH / MISCARRIAGE")), "LONGITUDE_X", "LATITUDE_X", geo, "GEOID", "time")$counts
colnames(Other) <- c("regionname", "time", "counts")
Other$counts[is.na(Other$counts)] <- 0
Other$region <- match(Other$regionname, colnames(mat))
for(i in 1:(dim(mat)[1] - length(hole))){
    if(i %in% Other$region == FALSE){
        Other <- rbind(Other, data.frame(regionname = colnames(mat)[i], time = 1:max(Other$time), counts = 0, region = i))
    }
}
for(i in (dim(mat)[1] - length(hole)+1) : dim(mat)[1]){
    if(i %in% Other$region == FALSE){
        Other <- rbind(Other, data.frame(regionname = colnames(mat)[i], time = 1:max(Other$time), counts = NA, region = i))
    }
}
message(paste("Total falsification incidents:", sum(Other$counts, na.rm = TRUE)))
colnames(Other)[3] <- "Other"


##################################################
##            Model fitting
##################################################
data <- cbind(Intercept = 1, Y)
data$Queensgate <- 0
data$Queensgate[data$regionname == "390610263001"] <- 1
data$log_population <- log(data$population)
data$population_1e3 <- data$population / 1000
##-----------------------------------
## Parameters currently not used, 
## may become useful for customization
##
t_use <- T
##------------------------------------
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
# data$home_value[data$pc_singlefamily < 0.8] <- NA
# data$home_value_change[data$pc_singlefamily < 0.8] <- NA
# scale_bus_count <- 200 # ??? not sure what scale to use
# data$bus_count <- data$bus_count / scale_bus_count
data[, colnames(data) %in% c("pharm", "hospital", "fqhc", "otp", "bup")] <- data[, colnames(data) %in% c("pharm", "hospital",  "fqhc", "otp", "bup")] / 10

formula.group <- 5
source("formulalist.R")
formula0 <- formula[["AR1-PC-S"]]

data <- merge(data, Nonheroin, by = c("regionname", "time", "region"), all.x = TRUE)
data <- merge(data, Down, by = c("regionname", "time", "region"), all.x = TRUE)
data <- merge(data, Other, by = c("regionname", "time", "region"), all.x = TRUE)
data$Heroin <- data$counts

# Case 0: Heroin overdose
data$counts <- data$Heroin
tt <- Sys.time()
fit0 = inla(formula0, data = data, family = c("poisson"), control.predictor = list(compute = TRUE, link=1),control.compute = list(config=TRUE), verbose = TRUE, control.inla = list(int.strategy = "ccd"))#, lincomb = all.lc)
fit0$computation_time <- Sys.time() - tt
print(fit0$computation_time)

# Case 1: Non-heroin overdose + Heroin Overdose
data$counts <- data$NonHeroin + data$Heroin
tt <- Sys.time()
fit1 = inla(formula0, data = data, family = c("poisson"), control.predictor = list(compute = TRUE, link=1),control.compute = list(config=TRUE), verbose = TRUE, control.inla = list(int.strategy = "ccd"))#, lincomb = all.lc)
fit1$computation_time <- Sys.time() - tt
print(fit1$computation_time)

# Case 2: Non-heroin overdose + Heroin Overdose + Person Down
data$counts <- data$NonHeroin + data$Heroin + data$Down
tt <- Sys.time()
fit2 = inla(formula0, data = data, family = c("poisson"), control.predictor = list(compute = TRUE, link=1),control.compute = list(config=TRUE), verbose = TRUE, control.inla = list(int.strategy = "ccd"))#, lincomb = all.lc)
fit2$computation_time <- Sys.time() - tt
print(fit2$computation_time)

# Case 3: Non-heroin overdose  
data$counts <- data$NonHeroin 
tt <- Sys.time()
fit3 = inla(formula0, data = data, family = c("poisson"), control.predictor = list(compute = TRUE, link=1),control.compute = list(config=TRUE), verbose = TRUE, control.inla = list(int.strategy = "ccd"))#, lincomb = all.lc)
fit3$computation_time <- Sys.time() - tt
print(fit3$computation_time)

# Case 4: Person Down
data$counts <- data$Down
tt <- Sys.time()
fit4 = inla(formula0, data = data, family = c("poisson"), control.predictor = list(compute = TRUE, link=1),control.compute = list(config=TRUE), verbose = TRUE, control.inla = list(int.strategy = "ccd"))#, lincomb = all.lc)
fit4$computation_time <- Sys.time() - tt
print(fit4$computation_time)


# Case 5: Non-heroin overdose + Person Down
data$counts <- data$NonHeroin + data$Down
tt <- Sys.time()
fit5 = inla(formula0, data = data, family = c("poisson"), control.predictor = list(compute = TRUE, link=1),control.compute = list(config=TRUE), verbose = TRUE, control.inla = list(int.strategy = "ccd"))#, lincomb = all.lc)
fit5$computation_time <- Sys.time() - tt
print(fit5$computation_time)


# Case 6: Falsification
data$counts <- data$Other
tt <- Sys.time()
fit6 = inla(formula0, data = data, family = c("poisson"), control.predictor = list(compute = TRUE, link=1),control.compute = list(config=TRUE), verbose = TRUE, control.inla = list(int.strategy = "ccd"))#, lincomb = all.lc)
fit6$computation_time <- Sys.time() - tt
print(fit6$computation_time)


allfits <- list(data = data, fit0 = fit0, fit1 = fit1, fit2 = fit2, fit3 = fit3, fit4 = fit4, fit5 = fit5, fit6 = fit6)
# save(allfits, file = "rda/sensitivity.rda")


allfits <- list(data = data, fit0 = fit0, fit1 = fit1, fit2 = fit2, fit3 = fit3, fit4 = fit4, fit5 = fit5, fit6 = fit6)
# save(allfits, file = "rda/sensitivity.rda")


########################################
## Fixed Effects
########################################
fitall <- NULL
fitall[[1]] <- fit0; names(fitall)[[1]] <- "Heroin-related Overdose"
fitall[[2]] <- fit1; names(fitall)[[2]] <- "All Overdose"
fitall[[3]] <- fit2; names(fitall)[[3]] <- "All Overdose and Person Down"
fitall[[4]] <- fit3; names(fitall)[[4]] <- "Non Heroin-related Overdose"
fitall[[5]] <- fit4; names(fitall)[[5]] <- "Person Down"
fitall[[6]] <- fit5; names(fitall)[[6]] <- "Non Heroin-related Overdose and Person Down"
fitall[[7]] <- fit6; names(fitall)[[7]] <- "Pregnancy / Childbirth / Miscarriage"

fixedall <- NULL
for(i in 1:7){
	tmp <- fitall[[i]]$summary.fixed[, c(1, 3, 5)]
	colnames(tmp) <- c("mean", "lower", "upper")
	tmp$model <- names(fitall)[[i]]
	tmp$covariate <- rownames(tmp)
	fixedall <- rbind(fixedall, tmp)
}
fixedall$covariate <- factor(fixedall$covariate, levels = rev(rownames(tmp)))
fixedall$lower <- exp(fixedall$lower)
fixedall$upper <- exp(fixedall$upper)
fixedall$mean <- exp(fixedall$mean)
########################################
## Temporal Effects
########################################
timeall <- NULL
for(i in 1:7){
	tmp <- exp(fitall[[i]]$summary.random[["time"]][, 1+c(1, 3, 5)])
	colnames(tmp) <- c("mean", "lower", "upper")
	tmp$model <- names(fitall)[[i]]
	tmp$covariate <- 1:42
	timeall <- rbind(timeall, tmp)
}
timeall$Date <- Y$Date[match(timeall$covariate, Y$time)]
timeall$covariate <- factor(timeall$covariate, levels = 1:42)

########################################
## Spatial Effects
########################################
spaceall <- NULL
for(i in 1:7){
	tmp <- exp(fitall[[i]]$summary.random[["region"]][1:280, 1+c(1, 3, 5)])
	colnames(tmp) <- c("mean", "lower", "upper")
	tmp$model <- names(fitall)[[i]]
	tmp$covariate <- colnames(mat)[1:280]
	spaceall <- rbind(spaceall, tmp)
}


# Sensitivity analysis: showing range of estimated coefs
compare1 <- names(fitall)[c(1:2)]
sub <- subset(fixedall, model %in% compare1)
sub$model <- factor(sub$model, levels = compare1)
g1 <- ggplot(subset(sub, covariate != "Intercept"), aes(x = covariate, y = mean, ymax = upper, ymin = lower, color = model), alpha = 0.8) + geom_point(position=position_dodge(width=-0.5)) + geom_errorbar(position=position_dodge(width=-0.5)) + coord_flip() + line.theme  + 
      guides(color = guide_legend(reverse = FALSE)) + theme(legend.key.height=unit(1.5,"line")) + xlab("Coefficients") + ylab("Relative Risk")+ scale_color_brewer("Incident Types", palette = "Set1") + scale_y_continuous(trans = "log") + geom_hline(yintercept = 1, alpha = 0.5, linetype = "dashed")
ggsave(plot = g1, file = "../figures/sensitivity-fixed.pdf", width = 10, height = 12)
 

sub <- subset(timeall, model %in% compare1)
sub$model <- factor(sub$model, levels = compare1)
g2 <- ggplot(sub, aes(x = Date, y = mean, ymax = upper, ymin = lower, color = model), alpha = 0.8) + geom_point(position=position_dodge(width=10)) + geom_line(aes(group = model), position=position_dodge(width=10)) + 
	geom_errorbar(position=position_dodge(width=10), width = 0.3, alpha = 0.5) + 
	line.theme  + theme(legend.key.height=unit(1.5,"line"), legend.position = "bottom")  + xlab("") + ylab("Relative Risk")+ scale_color_brewer(palette = "Set1")+ scale_x_date(breaks = "6 month", labels = date_format("%b %Y"), lim = c(min(Y$Date)-10, max(Y$Date)+10))
ggsave(plot = g2, file = "../figures/sensitivity-time.pdf", width = 10, height = 5)



sub <- subset(spaceall, model %in% compare1)
sub$model <- factor(sub$model, levels = (compare1))
max <- max(abs(log(sub$mean)))
breaks <- exp(seq(-max, max, length.out = 5))
g3 <- mapPlot(data = sub, geo = geo, variables = c("model"), values = c("mean"), by.data = "covariate", by.geo = "GEOID", is.long = TRUE, border = "gray50", ncol=3) + scale_fill_distiller("Relative Risk", palette = "RdBu", trans = "log", breaks = breaks, lim = range(breaks)+c(-.01, .01), label = function(x){options( digits = 2);format( x, scientific = FALSE)})  + map.theme + theme_void() + coord_map() + theme(strip.background = element_blank(), strip.text.x = element_text( vjust = 1, size = 10)) + theme(plot.margin=margin(t=-1,b=-1,l=0,r=0, unit="cm"), legend.title = element_text(size = 9))  
ggsave(plot = g3, file = "../figures/sensitivity-space.pdf", width = 15, height = 5)



# Sensitivity analysis: differential reporting
compare2 <- names(fitall)[c(1, 4)]
sub <- subset(fixedall, model %in% compare2)
sub$model <- factor(sub$model, levels = compare2)
g1 <- ggplot(subset(sub, covariate != "Intercept"), aes(x = covariate, y = mean, ymax = upper, ymin = lower, color = model), alpha = 0.8) + geom_point(position=position_dodge(width=-0.5)) + geom_errorbar(position=position_dodge(width=-0.5), width = 0.5, alpha = .5) + coord_flip() + line.theme  + 
      guides(color = guide_legend(reverse = FALSE)) + theme(legend.key.height=unit(1.5,"line")) + xlab("Relative Risk") + ylab("Fixed Effects")+ scale_color_brewer("Incident Types", palette = "Set1")+ scale_y_continuous(trans = "log") + geom_hline(yintercept = 1, alpha = 0.5, linetype = "dashed")
ggsave(plot = g1, file = "../figures/sensitivity-fixed-sep.pdf", width = 10, height = 12)


sub <- subset(timeall, model %in% compare2)
sub$model <- factor(sub$model, levels = compare2)
g2 <- ggplot(sub, aes(x = Date, y = mean, ymax = upper, ymin = lower, color = model), alpha = 0.8) + geom_point(position=position_dodge(width=10)) + geom_line(aes(group = model), position=position_dodge(width=10)) + 
	geom_errorbar(position=position_dodge(width=10), width = 0.3, alpha = 0.5) + 
	line.theme  + theme(legend.key.height=unit(1.5,"line"), legend.position = "bottom")  + xlab("") + ylab("Relative Risk")+ scale_color_brewer(palette = "Set1")+ scale_x_date(breaks = "6 month", labels = date_format("%b %Y"), lim = c(min(Y$Date)-10, max(Y$Date)+10))
ggsave(plot = g2, file = "../figures/sensitivity-time-sep.pdf", width = 10, height = 5)


sub <- subset(spaceall, model %in% compare2)
sub$model <- factor(sub$model, levels = (compare2))
max <- max(abs(log(sub$mean)))
breaks <- exp(seq(-max, max, length.out = 5))
g3 <- mapPlot(data = sub, geo = geo, variables = c("model"), values = c("mean"), by.data = "covariate", by.geo = "GEOID", is.long = TRUE, border = "gray70", ncol=3) + scale_fill_distiller("Relative Risk", palette = "RdBu", trans = "log", breaks = breaks, lim = range(breaks)+c(-.01, .01), label = function(x){options( digits = 2);format( x, scientific = FALSE)})  + map.theme + theme_void() + coord_map() + theme(strip.background = element_blank(), strip.text.x = element_text( vjust = 0, size = 10)) + theme(plot.margin=margin(t=-1,b=-1,l=0,r=0, unit="cm"), legend.title = element_text(size = 9))  
ggsave(plot = g3, file = "../figures/sensitivity-space-sep.pdf", width = 15, height = 5)


sub$covariate2 <- sub$covariate
ordered <- sub[sub$model == compare2[1], ]
sub$covariate2 <- factor(sub$covariate2, levels = ordered$covariate[order(ordered$mean)])
g4 <- ggplot(data = sub, aes(ymin = lower, ymax = upper, x = covariate2, y = mean, color = model)) + geom_hline(yintercept = 1, linetype = "dashed") + geom_errorbar(position=position_dodge(width=.5), alpha = 0.6) + geom_point(position=position_dodge(width=.5), size = 0.5) + line.theme + theme(axis.text.x = element_blank()) + xlab("Block Groups") + ylab("Relative Risk") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom") + scale_color_brewer("Incident Types", palette = "Set1")
ggsave(plot = g4, file = "../figures/sensitivity-space-sep2.pdf", width = 12, height = 5)


# Sensitivity analysis: falsification test
compare3 <- names(fitall)[c(1, 7)]
sub <- subset(fixedall, model %in% compare3)
sub$model <- factor(sub$model, levels = compare3)
g1 <- ggplot(subset(sub, covariate != "Intercept"), aes(x = covariate, y = mean, ymax = upper, ymin = lower, color = model), alpha = 0.8) + geom_point(position=position_dodge(width=-0.5)) + geom_errorbar(position=position_dodge(width=-0.5), alpha = 0.5, width = 0.4) + coord_flip() + line.theme + theme(legend.key.height=unit(1.5,"line")) + xlab("Coefficients") + ylab("Relative Risk") + scale_color_brewer("Incident types", palette = "Set1")+ scale_y_continuous(trans = "log") + geom_hline(yintercept = 1, alpha = 0.5, linetype = "dashed")
ggsave(plot = g1, file = "../figures/sensitivity-fixed-preg.pdf", width = 10 * .8, height = 12* .8)



sub <- subset(timeall, model %in% compare3)
sub$model <- factor(sub$model, levels = compare3)
g2 <- ggplot(sub, aes(x = Date, y = mean, ymax = upper, ymin = lower, color = model), alpha = 0.8) + geom_point(position=position_dodge(width=10)) + geom_line(aes(group = model), position=position_dodge(width=10)) + 
	geom_errorbar(position=position_dodge(width=10), width = 0.3, alpha = 0.5) + 
	line.theme  + theme(legend.key.height=unit(1.5,"line"), legend.position = "bottom")  + xlab("") + ylab("Relative Risk")+ scale_color_brewer(palette = "Set1")+ scale_x_date(breaks = "6 month", labels = date_format("%b %Y"), lim = c(min(Y$Date)-10, max(Y$Date)+10))
ggsave(plot = g2, file = "../figures/sensitivity-time-preg.pdf", width = 10, height = 5)

sub <- subset(spaceall, model %in% compare3)
sub$model <- factor(sub$model, levels = (compare3))
max <- max(abs(log(sub$mean)))
breaks <- exp(seq(-max, max, length.out = 5))
g3 <- mapPlot(data = sub, geo = geo, variables = c("model"), values = c("mean"), by.data = "covariate", by.geo = "GEOID", is.long = TRUE, border = "gray50", ncol=3) + scale_fill_distiller("Relative Risk", palette = "RdBu", trans = "log", breaks = breaks, lim = range(breaks)+c(-.01, .01), label = function(x){options( digits = 2);format( x, scientific = FALSE)})  + map.theme + theme_void() + coord_map() + theme(strip.background = element_blank()) + theme(plot.margin=margin(t=-1,b=-1,l=0,r=0, unit="cm"), legend.title = element_text(size = 9))  
ggsave(plot = g3, file = "../figures/sensitivity-space-preg.pdf", width = 10, height = 5)




sub$covariate2 <- sub$covariate
ordered <- sub[sub$model == compare3[1], ]
sub$covariate2 <- factor(sub$covariate, levels = ordered$covariate[order(ordered$mean)])
g4 <- ggplot(data = sub, aes(ymin = lower, ymax = upper, x = covariate2, y = mean, color = model)) + geom_hline(yintercept = 1, linetype = "dashed") + geom_errorbar(position=position_dodge(width=.5), alpha = 0.6) + geom_point(position=position_dodge(width=.5), size = 0.5) + line.theme + theme(axis.text.x = element_blank()) + xlab("Block Groups") + ylab("Relative Risk") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom") + scale_color_brewer("Incident Types", palette = "Set1")
ggsave(plot = g4, file = "../figures/sensitivity-space-preg2.pdf", width = 12, height = 5)



#####################################################

# Case 7: Non-differentiating
data$counts <- data$NonHeroin
data$offset <- log(data$NonHeroin + data$Heroin)
data1 <- data[which(!is.infinite(data1$offset)), ]
tt <- Sys.time()
fit7 = inla(formula0, data = data1, family = c("poisson"), control.predictor = list(compute = TRUE, link=1),control.compute = list(config=TRUE), verbose = TRUE, control.inla = list(int.strategy = "ccd"), offset = offset)#, lincomb = all.lc)
fit7$computation_time <- Sys.time() - tt
print(fit7$computation_time)

sub <- data.frame(fit7$summary.fixed)
colnames(sub)[c(1, 3, 5)] <- c("mean", "lower", "upper")
sub[, c(1, 3, 5)] <- exp(sub[, c(1, 3, 5)])
sub$covariate <- rownames(sub)
sub$covariate <- factor(sub$covariate, levels = rev(rownames(sub)))
g1 <- ggplot(subset(sub, covariate != "Intercept"), aes(x = covariate, y = mean, ymax = upper, ymin = lower), alpha = 0.8) + geom_point(position=position_dodge(width=-0.5)) + geom_errorbar(position=position_dodge(width=-0.5), width = 0.5, alpha = .5) + coord_flip() + line.theme  + 
      guides(color = guide_legend(reverse = FALSE)) + theme(legend.key.height=unit(1.5,"line")) + xlab("Covariates") + ylab("Relative Risk")  + scale_color_brewer("Incident Types", palette = "Set1")+ scale_y_continuous(trans = "log") + geom_hline(yintercept = 1, alpha = 0.5, linetype = "dashed")
ggsave(g1, file = "../figures/sensitivity-fixed-nonheroin-ratio.pdf", width = 10 * .8, height = 12* .8)

sub <- data.frame(fit7$summary.random[["time"]])
colnames(sub)[1+c(1, 3, 5)] <- c("mean", "lower", "upper")
sub[, 1+c(1, 3, 5)] <- exp(sub[, 1+c(1, 3, 5)])
sub$Date <- Y$Date[match(sub$ID, Y$time)]
g3 <- ggplot(data = sub, aes(ymin = lower, ymax = upper, x = Date, y = mean)) + geom_hline(yintercept = 1, linetype = "dashed", color = "red") + geom_errorbar(position=position_dodge(width=.5), alpha = 0.6, width = 0.2) + geom_point(position=position_dodge(width=.5), size = 0.5) + line.theme   + xlab("Month") + ylab("Temporal Relative Risk") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom") + scale_color_brewer("Incident Types", palette = "Set1")+ scale_x_date(breaks = "6 month", labels = date_format("%b %Y"), lim = c(min(Y$Date)-10, max(Y$Date)+10))


sub <- data.frame(fit7$summary.random[["region"]][1:280, ])
sub$ID <- factor(sub$ID, levels = sub$ID[order(sub$mean)])
colnames(sub)[1+c(1, 3, 5)] <- c("mean", "lower", "upper")
sub[, 1+c(1, 3, 5)] <- exp(sub[, 1+c(1, 3, 5)])
g4 <- ggplot(data = sub, aes(ymin = lower, ymax = upper, x = ID, y = mean)) + geom_hline(yintercept = 1, linetype = "dashed", color = "red") + geom_errorbar(position=position_dodge(width=.5), alpha = 0.6) + geom_point(position=position_dodge(width=.5), size = 0.5) + line.theme + theme(axis.text.x = element_blank()) + xlab("Block Groups") + ylab("Spatial Relative Risk") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom") + scale_color_brewer("Incident Types", palette = "Set1")
g2 <- grid.arrange(g3, g4, ncol = 1)
ggsave(plot = g2, file = "../figures/sensitivity-random-nonheroin-ratio.pdf", width = 12, height = 8)