##################################################
### This is the main script that
###	 1. does model comparison
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

VIS <- FALSE
is.bg <- TRUE
prefix <- ""
##################################################
##            Block Group or SNA
##################################################
if(is.bg){
	## Read all data to be used
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
INLA:::inla.dynload.workaround() 

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
data[data$time >= t_use, "value"] <- NA
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
data[, colnames(data) %in% c("pharm", "fqhc", "otp", "bup")] <- data[, colnames(data) %in% c("pharm", "fqhc", "otp", "bup")] / 10

formula.group <- 5
source("formulalist.R") 

##################################################
##            Model Selection Long
##################################################
## fixed effect only
## time i.i.d and space i.i.d
## interaction i.i.d
## time AR1 and interaction i.i.d
## Main model with default prior
## Main model
## Compare using population as offset and logpop as covariate
fitall <- NULL
counter <- 1
for(formula.group in c(1, 5)){
	source("formulalist.R")
	if(formula.group %in% c(1,2)){
		data$offset <- data$log_population
	}else{
		data$offset <- 0
	}
	for(name in c("fixed","random", "AR1-PC-random", "AR1-PC-S")){
		tt <- Sys.time()
		fitall[[counter]] <- inla(formula[[name]], data = data, family = c("poisson"), control.predictor = list(compute = TRUE, link=1), control.compute=list(dic=T,mlik=T,cpo=T, waic=T, openmp.strategy = "large", smtp = "taucs", config = TRUE), control.inla = list(int.strategy = "ccd"), offset = offset)
		fitall[[counter]]$computation_time <- Sys.time() - tt
		names(fitall)[counter] <- paste0("Type", formula.group, "-", name)
		print(fitall[[counter]]$computation_time)
		print(names(fitall)[counter])
		message(paste0("DIC:   ", 
			summary(fitall[[counter]])$dic$dic, 
			"\nPd:   ", 
			summary(fitall[[counter]])$dic$p.eff, 
			"\nWAIC:   ", 
			summary(fitall[[counter]])$waic$waic
		))
		counter <- counter + 1
	}
}
# for(i in 7:12) fitall[[i]] <- ff[[i]]
metric <- matrix(NA, length(fitall), 4)
colnames(metric) <- c("DIC", "neffp", "WAIC", "neffp")
for(i in 1:dim(metric)[1]){
	# cpo <- inla.cpo(fitall[[i]])
	# logCPO <- log(summary(cpo)$cpo$cpo)
	# logCPO[!is.finite(logCPO)] <- NA
	metric[i, ] <- c(summary(fitall[[i]])$dic$dic,
		# sum(logCPO, na.rm=TRUE), 
		summary(fitall[[i]])$dic$p.eff,
		summary(fitall[[i]])$waic$waic,
		summary(fitall[[i]])$waic$p.eff
		)
}
library(xtable)
out <- data.frame(pop = c("As offset", rep(NA, 3), "As covariate", rep(NA, 3)), method = rep(c("Fixed effects only", "Fixed and independent random effects", "Fixed, temporal, and independent random effects", "Full space-time smoothing model"), 2))
out1 <- cbind(paste0(metric[, 1], " (", round(metric[, 2], 1), ")"),
	paste0(metric[, 3], " (", round(metric[, 4],1), ")"))
colnames(out1) <- c("DIC", "WAIC")
out <- cbind(out, out1)
print(xtable(out), include.rownames = FALSE)
save(fitall, file = "compareModels.RData")
 
########################################################
# Results for the risk model
########################################################
fit0 <-  fitall[[4]]
postfix <- "-bgrisk"

library(ggrepel)
library(scales)
library(gridExtra)
library(xtable)
fixed <- data.frame(fit0$summary.fixed)
fixed$index <- 1:dim(fixed)[1]
race <- c(6:12)
fixed$label <- factor(rownames(fixed), levels=rownames(fixed))
fixed$label <- better_names(fixed$label)

covariates <- data[, match(rownames(fixed), colnames(data))]
covariates <- covariates[data$time ==1, ]
covtab <- t(apply(covariates, 2, function(x){
	return(c(mean(x, na.rm = T), 
			 sd(x, na.rm = T), 
			 quantile(x, c(0, 1), na.rm=T)))
}))
colnames(covtab) <- c("mean", "sd", "min", "max")
covtab <- data.frame(Covariate = better_names(rownames(covtab)), 
					 Mean = covtab[,1], 
					 SD = covtab[,2],
					 Source = "ACS 2013–2017")
covtab$Mean[rownames(covtab) == "crime_rate"] <- mean(data$crime_rate, na.rm=T)
covtab$SD[rownames(covtab) == "crime_rate"] <- sd(data$crime_rate, na.rm=T)
sub <- Y[Y$region == 1, ]
covtab$Mean[rownames(covtab) == "temperature"] <- mean(sub$temperature, na.rm=T)
covtab$SD[rownames(covtab) == "temperature"] <- sd(sub$temperature, na.rm=T)
covtab$Mean[rownames(covtab) == "precipitation"] <- mean(sub$precipitation, na.rm=T)
covtab$SD[rownames(covtab) == "precipitation"] <- sd(sub$precipitation, na.rm=T)
# print(xtable(covtab, digits = 3), include.rownames=F)


fixed <- fixed[-1, ]

ggforest <- function(met, tab, mid = 0, breaks = 5, leftsep = 0.5, leftmar = 15, leftskip = 17, log = FALSE, xlab = ""){
	require(scales)
	met <- data.frame(met)
	colnames(met)[1:3] <- c("mean", "lower", "upper")
	met$y <- dim(met)[1] : 1
	mx <- max(met$upper - mid, mid - met$lower)

	gtab <- data.frame(lab = array(as.character(unlist(tab[[1]]))), 
					   mean = as.character(tab[[2]]), 
					   ci = as.character(tab[[3]]), stringsAsFactors = FALSE)
	gtab$mean[1] <- paste0("  ", gtab$mean[1])
	tmp1 <- max(nchar(gtab$lab)) + 6
	tmp2 <- max(nchar(gtab$mean)) + 4
	tmp3 <- max(nchar(gtab$ci)) + 2
	gtab$text <- ""
	for(i in 1:dim(gtab)[1]) {
		# gtab$text[i] <- paste0(gtab$text[i], paste(
		# 	rep(" ", tmp1 - nchar(gtab$lab[i])), collapse = ""))
		gtab$text[i] <- paste0(gtab$text[i], paste(c(gtab$mean[i], 
			rep(" ", tmp2 - nchar(gtab$mean[i]))), collapse = ""))			
		gtab$text[i] <- paste0(gtab$text[i], paste(c(gtab$ci[i], 
			rep(" ", tmp3 - nchar(gtab$ci[i]))), collapse = ""))
	}
	gtab$y <- (dim(met)[1] + 1) : 1
	gtab$x <- min(met$lower) - leftsep 

	g <- ggplot(data = met) + geom_segment(x = 0, xend = 0, y = 0, yend = dim(met)[1] + 0.5, color = 'gray30', linetype = "dashed")  + geom_errorbarh(aes(xmin = lower, xmax = upper, x = mean, y = y), color = "#56106EFF", height = 0.1, size = 0.5) + geom_point(aes(x = mean, y = y), size = 0.8, color = "#BB3754FF")+ theme_void() + xlab(xlab)
	if(log){
		g <- g + scale_x_continuous(trans = "log", breaks= trans_breaks("log", "exp", n = breaks), labels=function(x) sprintf("%.1f", x))
	}else{
		g <- g + scale_x_continuous(breaks= trans_breaks(identity, identity, n = breaks))
	}
	g <- g + theme(plot.margin=unit(c(1,1,1,leftmar),"cm"), 
				   axis.text.x = element_text(vjust = -1),
				   axis.title = element_text(vjust = -1.2,face="bold"),
				   axis.text.y = element_blank(),
				   axis.ticks.x = element_line(size = .5, color = "black"),
				   axis.ticks.length = unit(.15, "cm"),
				   axis.line.x = element_line(size = .5, color = "black")
				   )
	g <- g + coord_cartesian(xlim = range(met[, 1:3]), clip = "off")


	highlight <- c(ifelse((met$upper - mid) * (met$lower - mid) > 0, 2, 1))

	g <- g + annotate("text",  x = gtab$x[-1], y = gtab$y[-1], label = gtab$text[-1], size = 3, hjust = 1, fontface = highlight, family="Courier") 
	g <- g +  annotate("text",  x = gtab$x[1], y = gtab$y[1], label = gtab$text[1], size = 3.4, hjust = 1, fontface =2, family="Courier") 
	if(log){
		xx <- exp(log(gtab$x[1]) - leftskip)
	}else{
		xx <- gtab$x[1] - leftskip
	}
	g <- g + annotate("text",  x = xx, y = gtab$y[-1], label = gtab$lab[-1], size = 3, hjust = 0, fontface = highlight, family="Courier")  
	g <- g +  annotate("text",  x = xx, y = gtab$y[1], label = gtab$lab[1], size = 3.4, hjust = 0, fontface =2, family="Courier") 
	if(log){
		g <- g + geom_segment(x = log(xx), xend = log(gtab$x[1]), y = gtab$y[1] - 0.5, yend = gtab$y[1] - 0.5, size = 0.2, color = "gray50")
	}else{
		g <- g + geom_segment(x = xx, xend = gtab$x[1], y = gtab$y[1] - 0.5, yend = gtab$y[1] - 0.5, size = 0.2, color = "gray50")
	}
	return(g)
}


metrics <- data.frame(
    mean  = fixed$mean, 
    median  = fixed$mean, 
    lower = fixed$X0.025quant,
    upper = fixed$X0.975quant, 
    rrmean = exp(fixed$mean), 
    rrmedian  = exp(fixed$mean), 
    rrlower = exp(fixed$X0.025quant),
    rrupper = exp(fixed$X0.975quant))

tocharacter2 <- function(x){
	# dot1 <- lapply(strsplit(as.character(x[1]), ''), function(x) which(x == '.'))[[1]]
	dot2 <- lapply(strsplit(as.character(x[2]), ''), function(x) which(x == '.'))[[1]]
	pad1 <- pad2 <- ""
	if(dot2 < 4) pad2 <- paste(rep(" ", 4-dot2), collapse="")
	paste0("(", x[1], ", ", pad2, x[2], ")")
}
tab <- data.frame(label = fixed$label,metrics[, 5:8])
for(i in 2:5) tab[, i] <- sprintf("%.3f",tab[, i])
tab <- as.matrix(tab)
# only work with for numbers < 10	
for(i in 1:dim(tab)[1]){
	dot1 <- lapply(strsplit(as.character(tab[i, 2]), ''), function(x) which(x == '.'))[[1]]
	if(dot1 < 3) tab[i, 2] <- paste0(" ", tab[i, 2])
}
tab[, 4] <- apply(tab[, c(4, 5)], 1, tocharacter2)	
tab <- as.matrix(rbind(c("Fixed effect", "exp(Mean)", "95% Posterior CI"), tab[, c(1, 2, 4)]))
tab <- list(tab[,1], tab[, 2], tab[, 3])
special <- which(tab[[1]] == "Temperature (Fahrenheit)")
tab[[1]][[special]] <- "Temperature (F)"


met <- metrics[, c(1, 3, 4) + 4]
out <- ggforest(met = met, tab = tab, mid = 1, breaks = 5, leftsep = 0, leftmar = 15.5, leftskip = 19.5, log = TRUE, xlab = "Relative Risk")
ggsave(plot = out, filename = paste0("../figures/", prefix, "fixed_effects", postfix, "-v-rr.pdf"), width = 11, height = 9)



#-------------------------------
# Plot of random effects
#-------------------------------
space1 <- data.frame(fit0$summary.random[["region"]][1:N, ])
space1$region <- colnames(mat)[1:N]
time1 <- data.frame(fit0$summary.random[["time"]])
time2 <- data.frame(fit0$summary.random[["time.iid"]])
time1$effect <- "Structured"
time2$effect <- "IID"
time <- rbind(time1, time2)
time$effect <- factor(time$effect, levels = c("Structured", "IID"))
time$Date <- Y$Date[match(time$ID, Y$time)]

#-------------------------------
# Plot of spatial and temporal random effects
#-------------------------------
time1 <- data.frame(fit0$summary.random[["time"]])
time1$Date <- Y$Date[match(time1$ID, Y$time)]
time1$effect <- "Temporal Random Effects"
time1$spike <- "1"
time1$spike[c(14, 20, 36)] <- "2"
space1$rr <- exp(space1$mean)
time1$rr <- exp(time1$mean)
time1$`X0.025quantrr` <- exp(time1$X0.025quant)
time1$`X0.975quantrr` <- exp(time1$X0.975quant)
breaks <- exp(seq(min(space1$mean), max(space1$mean), length.out = 5))
g1 <- mapPlot2(data = space1, geo = geo, variables = c("rr"), labels = c("Relative Risk"), by.data = "region", by.geo = byGEO, removetab=TRUE) + map.theme + coord_map() + scale_fill_distiller("Relative Risk", palette = "RdBu", trans = "log", breaks = breaks, lim = range(breaks) + c(-0.01, 0.01), label = function(x){options( digits = 2);format( x, scientific = FALSE)}) + theme(panel.border = element_blank()) + theme(strip.background = element_blank(), strip.text.x = element_text( vjust = -0.1, size = 16)) + theme(legend.position = c(0.15, 0.72), legend.key.size = unit(.85,"line"), plot.margin=margin(t=-1,b=-1,l=0,r=0, unit="cm"), legend.title = element_text(size = 9)) 
g2 <- ggplot(time1, aes(x = Date, y = rr, ymin = `X0.025quantrr`, ymax = `X0.975quantrr`,  group = effect, shape = spike)) + geom_point(color = "darkred", size=2)  + geom_errorbar(width = .1, color = "darkred", size = .3) + line.theme  + ylab("Relative Risk")  + ggtitle("") + xlab(NULL)  + scale_x_date(breaks = "6 month", labels = date_format("%b %Y"), lim = c(min(Y$Date), max(Y$Date)))+ scale_shape_manual(values = c(19, 17),  guide = FALSE)+ theme(panel.border = element_blank(), plot.margin=margin(t=1, b=1, l=0, r=1, unit="cm")) 
out <- grid.arrange(grobs = list(g1, g2), ncol = 2, widths = c(1, 1.15))
ggsave(plot = out, filename = paste0("../figures/", prefix, "space_and_time_effects", postfix, "-rr.pdf"), width = 10, height = 4)
