##################################################
### This is the main script that
###	1. grabs covariates data, visualize, summarize
### 2. fit a baseline space-time model
### 3. visualize and summarize model fits
##################################################

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

prefix <- ""
VIS <- TRUE
is.bg <- TRUE
##################################################
##            Block Group or SNA
##################################################
if(is.bg){
	## Read all data to be used
	# prefix <- "broad"
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
}else{
	source("readCin-sna.R")
	data <- cbind(Intercept = 1, Y)
	data$Queensgate <- 0
	data$Queensgate[data$regionname == "Queensgate"] <- 1
	T <- max(data$time)
	N.full <- max(data$region)
	N<- dim(mat)[1] - 2
	byGEO <- "SNA_NAME"
	byDATA <- "regionname"
	postfix <- ""
	fixed.group <- 1
}
map.theme <- theme_bw() + theme(legend.title=element_text(size=rel(0.7)), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
line.theme <- theme_bw() + theme(legend.title=element_text(size=rel(0.7)))  
if(VIS){
	source("vis-data.R")
}

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
data[, colnames(data) %in% c("pharm", "hospital", "fqhc", "otp", "bup")] <- data[, colnames(data) %in% c("pharm", "hospital",  "fqhc", "otp", "bup")] / 10

formula.group <- 5
source("formulalist.R")
formula0 <- formula[["AR1-PC-S"]]

tt <- Sys.time()
fit0 = inla(formula0, data = data, family = c("poisson"), control.predictor = list(compute = TRUE, link=1),control.compute = list(config=TRUE), verbose = TRUE, control.inla = list(int.strategy = "ccd"))#, lincomb = all.lc)
fit0$computation_time <- Sys.time() - tt
print(fit0$computation_time)
save.image("../fitted-model/workingmodel.RData")

##################################################
##            Model Summary
##################################################
if(VIS) source("vis-fit.R")
