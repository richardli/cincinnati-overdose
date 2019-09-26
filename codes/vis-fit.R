#---------------------------
# Plot of fixed effects
#---------------------------
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
print(xtable(covtab, digits = 3), include.rownames=F)


fixed <- fixed[-1, ]
if(VIS){

	# Cochrane data from the 'rmeta'-package
	metrics <- data.frame(
	    mean  = fixed$mean, 
	    median  = fixed$mean, 
	    lower = fixed$X0.025quant,
	    upper = fixed$X0.975quant, 
	    rrmean = exp(fixed$mean), 
	    rrmedian  = exp(fixed$mean), 
	    rrlower = exp(fixed$X0.025quant),
	    rrupper = exp(fixed$X0.975quant))
	tab <- data.frame(label = fixed$label,metrics[, 1:4])
	for(i in 2:5) tab[, i] <- sprintf("%.3f",tab[, i])
	tab <- as.matrix(tab)
	# only work with for numbers < 10	
	for(i in 1:dim(tab)[1]){
		dot1 <- lapply(strsplit(as.character(tab[i, 2]), ''), function(x) which(x == '.'))[[1]]
		if(dot1 < 3) tab[i, 2] <- paste0(" ", tab[i, 2])
	}
	# only work with for numbers < 10	
	tocharacter <- function(x){
		dot1 <- lapply(strsplit(as.character(x[1]), ''), function(x) which(x == '.'))[[1]]
		dot2 <- lapply(strsplit(as.character(x[2]), ''), function(x) which(x == '.'))[[1]]
		pad1 <- pad2 <- ""
		if(dot1 < 3) pad1 <- rep(" ", 3-dot1)
		if(dot2 < 3) pad2 <- rep(" ", 3-dot2)
		paste0("(", pad1, x[1], ", ", pad2, x[2], ")")
	}
	tab[, 4] <- apply(tab[, c(4, 5)], 1, tocharacter)	
	tab <- as.matrix(rbind(c("Fixed effect", " Mean", "95% Posterior CI"), tab[, c(1, 2, 4)]))
	tab <- list(tab[,1], tab[, 2], tab[, 3])
	special <- which(tab[[1]] == "Temperature (Fahrenheit)")
	tab[[1]][special] <- "Temperature (F)"
	met <- metrics[, c(1, 3, 4)]
	
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

	out <- ggforest(met = met, tab = tab, mid = 0, breaks = 5, leftsep = 0, leftmar = 15.5, leftskip = 18.5, xlab = "Log Relative Risk")
	 ggsave(plot = out, filename = paste0("../figures/", prefix, "fixed_effects", postfix, "-v.pdf"), width = 10.5, height = 9)
	 ggsave(plot = out, filename = paste0("../figures/", prefix, "fixed_effects", postfix, "-v.tiff"), width = 10.5, height = 9)



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
	 ggsave(plot = out, filename = paste0("../figures/", prefix, "fixed_effects", postfix, "-v-rr.pdf"), width = 10.5, height = 9)
	 ggsave(plot = out, filename = paste0("../figures/", prefix, "fixed_effects", postfix, "-v-rr.tiff"), width = 10.5, height = 9)

}


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
if(VIS){
	time1 <- data.frame(fit0$summary.random[["time"]])
	time1$Date <- Y$Date[match(time1$ID, Y$time)]
	time1$effect <- "Temporal Random Effects"
	time1$spike <- "1"
	time1$spike[c(14, 20, 36)] <- "2"
	g1 <- mapPlot2(data = space1, geo = geo, variables = c("mean"), labels = c("Spatial effect"), by.data = "region", by.geo = byGEO, removetab=TRUE) + map.theme + coord_map() + scale_fill_distiller("Spatial Effect", palette = "RdBu") + theme(panel.border = element_blank()) + theme(strip.background = element_blank(), strip.text.x = element_text( vjust = -0.1, size = 16)) + theme(legend.position = c(0.15, 0.72), legend.key.size = unit(.85,"line"), plot.margin=margin(t=-1,b=-1,l=0,r=0, unit="cm"), legend.title = element_text(size = 11)) 
	g2 <- ggplot(time1, aes(x = Date, y = `mean`, ymin = `X0.025quant`, ymax = `X0.975quant`,  group = effect, shape = spike)) + geom_point(color = "darkred", size=2)  + geom_errorbar(width = .1, color = "darkred", size = .3) + line.theme  + ylab("Temporal Effect")  + ggtitle("") + xlab(NULL)  + scale_x_date(breaks = "6 month", labels = date_format("%b %Y"), lim = c(min(Y$Date), max(Y$Date)))+ scale_shape_manual(values = c(19, 17),  guide = FALSE)+ theme(panel.border = element_blank(), plot.margin=margin(t=1, b=1, l=0, r=1, unit="cm")) 
	out <- grid.arrange(grobs = list(g1, g2), ncol = 2, widths = c(1, 1.15))
	 ggsave(plot = out, filename = paste0("../figures/", prefix, "space_and_time_effects", postfix, ".pdf"), width = 10, height = 4)
	 ggsave(plot = out, filename = paste0("../figures/", prefix, "space_and_time_effects", postfix, ".tiff"), width = 10, height = 4)

	space1$rr <- exp(space1$mean)
	time1$rr <- exp(time1$mean)
	time1$`X0.025quantrr` <- exp(time1$X0.025quant)
	time1$`X0.975quantrr` <- exp(time1$X0.975quant)
	breaks <- exp(seq(min(space1$mean), max(space1$mean), length.out = 5))
	g1 <- mapPlot2(data = space1, geo = geo, variables = c("rr"), labels = c("Relative Risk"), by.data = "region", by.geo = byGEO, removetab=TRUE) + map.theme + coord_map() + scale_fill_distiller("Relative Risk", palette = "RdBu", trans = "log", breaks = breaks, label = function(x){options( digits = 2);format( x, scientific = FALSE)}) + theme(panel.border = element_blank()) + theme(strip.background = element_blank(), strip.text.x = element_text( vjust = -0.1, size = 16)) + theme(legend.position = c(0.15, 0.72), legend.key.size = unit(.85,"line"), plot.margin=margin(t=-1,b=-1,l=0,r=0, unit="cm"), legend.title = element_text(size = 9)) 
	g2 <- ggplot(time1, aes(x = Date, y = rr, ymin = `X0.025quantrr`, ymax = `X0.975quantrr`,  group = effect, shape = spike)) + geom_point(color = "darkred", size=2)  + geom_errorbar(width = .1, color = "darkred", size = .3) + line.theme  + ylab("Relative Risk")  + ggtitle("") + xlab(NULL)  + scale_x_date(breaks = "6 month", labels = date_format("%b %Y"), lim = c(min(Y$Date), max(Y$Date)))+ scale_shape_manual(values = c(19, 17),  guide = FALSE)+ theme(panel.border = element_blank(), plot.margin=margin(t=1, b=1, l=0, r=1, unit="cm")) 
	out <- grid.arrange(grobs = list(g1, g2), ncol = 2, widths = c(1, 1.15))
	 ggsave(plot = out, filename = paste0("../figures/", prefix, "space_and_time_effects", postfix, "-rr.pdf"), width = 10, height = 4)
	 ggsave(plot = out, filename = paste0("../figures/", prefix, "space_and_time_effects", postfix, "-rr.tiff"), width = 10, height = 4)

	time_12month <- time1
	time_12month$month <- format(time_12month$Date, "%m")
	time_12month$year <- format(time_12month$Date, "%Y")
	time_12month_mean <- aggregate(data = data.frame(time_12month), mean~month, FUN = mean)

	g3 <- ggplot(time_12month_mean, aes(x = month, y = mean)) + geom_line(group = 1, color = "black", size = 1.5, linetype = "dashed") + geom_point(color = "black", size=2)  + line.theme  + ylab("Temporal Effect")  + ggtitle("") + xlab("Month") + geom_point(data = time_12month, aes(x = month, y = mean, color = year, group = year), position = position_dodge(0.2)) + geom_errorbar(data = time_12month, aes(x = month, y = mean, color = year, group = year, ymin = `X0.025quant`, ymax = `X0.975quant`), width = 0.2, position = position_dodge(0.2))  + geom_line(data = time_12month, aes(x = month, y = mean, color = year, group = year), position = position_dodge(0.2), alpha = 0.8)+ scale_colour_brewer(palette = "Set1")
	 ggsave(plot = g3, filename = paste0("../figures/seasonality", postfix, ".pdf"), width = 10, height = 4)

	## Violin graph 
	library(ggridges)
	samp <- NULL
	for(i in 1:dim(fit0$summary.random[["time"]])[1]){
		tmp <- inla.rmarginal(1e4, fit0$marginals.random[["time"]][[i]])
		samp <- rbind(samp, data.frame(sample = tmp, month = format(time_12month$Date, "%b")[i], month.num = as.numeric(format(time_12month$Date, "%m")[i]), year = time_12month$year[i]))
	}
	# samp$sample <- exp(samp$sample)
	samp$month <- factor(samp$month, levels = rev(samp$month[match(1:12, samp$month.num)]))
	g <- ggplot(samp, aes(x =sample, y = month)) + geom_density_ridges2(quantile_lines = TRUE, quantiles = 2, fill = "darkblue", alpha = 0.5, scale = 0.9) + xlab("Temporal Log Relative Risks") + line.theme + ylab("Month")
	 ggsave(plot = g, filename = paste0("../figures/seasonality2", postfix, ".pdf"), width = 8, height = 9)

}



#-------------------------------
# Plot of spatial and temporal variance
#-------------------------------
if(VIS){
	dens <- NULL
	for(i in 1:length(fit0$marginals.hyperpar)){
		name <- names(fit0$marginals.hyperpar)[i]
		if(strsplit(name, " ")[[1]][1] == "Precision"){
			tmp <- inla.tmarginal(function(x)1/x, fit0$marginals.hyperpar[[i]])	
			name <- gsub("Precision", "Standard deviation", name)		
		}else{
			tmp <- (fit0$marginals.hyperpar[[i]])
		}
		tmp <- data.frame(tmp)
		tmp$name <- name
		dens <- rbind(dens, tmp)
	}
	ordered <- unique(dens$name)
	last <- which(ordered == "Standard deviation for regiontime")
	ordered <- c(as.character(ordered[-last]), as.character(ordered[last]))
	dens$name <- factor(dens$name, levels = ordered)
	pdf(paste0("../figures/", prefix, "posterior-var", postfix, ".pdf"), width = 9, height = 4.5)
	g <- ggplot(dens, aes(x = x, y = y)) + geom_line() + facet_wrap(~name, scales = 'free') + xlab("") + ylab("density") + theme_bw()
	print(g)
	dev.off()
}


#-------------------------------------
# Plot of interaction
# by space/time, add on main space-time
#--------------------------------------
if("regiontime" %in% names(fit0$summary.random)){
	st <- data.frame(fit0$summary.random[["regiontime"]])
	st$regiontime <- 1:dim(st)[1]
	st <- merge(st, regiontime, by = "regiontime")
}else if("region.int" %in% names(fit0$summary.random)){
	st <- data.frame(fit0$summary.random[["region.int"]])
	st$region <- st$ID
	st$time <- rep(1:T, each = dim(mat)[1])
}else{
	st <- data.frame(fit0$summary.random[["time.int"]])
	st$time <- st$ID
	st$region <- rep(1:dim(mat)[1], each =T)
}
st$regionname <- colnames(mat)[st$region]
# if(VIS){
# 	pdf(paste0("../figures/", prefix, "spacetime_effects", postfix, ".pdf"), width = 12, height = 10)
# 	g <- mapPlot2(data = st, geo = geo, variables = c("time"), values = c("mean"), by.data = "regionname", by.geo = byGEO, is.long = TRUE) + scale_fill_viridis_c("effect")+ map.theme + coord_map()
# 	print(g)
# 	dev.off()


# 	pdf(paste0("../figures/", prefix, "spacetime_effects2", postfix, ".pdf"), width = 9, height = 5)
# 	g <- ggplot(st, aes(x = time, y = `mean`, color = regionname, group = regionname)) + geom_line(alpha = 0.3) + theme_bw() + scale_colour_manual(values=colorRampPalette(brewer.pal(name="Set1", n = 8)[-6])(311), guide=FALSE) + ylab("Relative Risk")
# 	# g + geom_line(data=subset(st, regionname %in% st$regionname[which(st$mean > 0.7)]), aes(x = time, y = `mean`, color = regionname, group = regionname))
# 	print(g)
# 	dev.off()
# }
nsim <- 1e5
marg.s <- matrix(NA, nsim, N)
marg.t <- matrix(NA, nsim, T)
st2 <- data.frame(region = rep(1:N, T), 
				  time = rep(1:T, each = N),
				  mean = NA, med = NA, low = NA, high = NA)
for(i in 1:dim(marg.s)[2]){
	marg.s[, i] <- inla.rmarginal(nsim, fit0$marginals.random[["region"]][[i]])
}
for(i in 1:dim(marg.t)[2]){
	marg.t[, i] <- inla.rmarginal(nsim, fit0$marginals.random[["time"]][[i]]) + inla.rmarginal(nsim, fit0$marginals.random[["time.iid"]][[i]])
}
if("regiontime" %in% names(fit0$summary.random)){
	counter <- 1
	for(j in 1:dim(marg.t)[2]){
		for(i in 1:dim(marg.s)[2]){
			# make sure to not count the added regions
			k <- regiontime$regiontime[intersect(which(regiontime$time == j), which(regiontime$region == i))]
			which <- which(fit0$summary.random[["regiontime"]]$ID == k)
			tmp <- inla.rmarginal(nsim, fit0$marginals.random[["regiontime"]][[which]])
			# tmp <- tmp + marg.s[, i] + marg.t[, j]
			st2[counter, "mean"] <- mean(tmp) 
			st2[counter, "med"] <- median(tmp) 
			st2[counter, "low"] <- quantile(tmp, .025) 
			st2[counter, "high"] <- quantile(tmp, .975) 
			counter <- counter + 1
		}
	}
}else{
	counter <- 1
	for(j in 1:dim(marg.t)[2]){
		for(i in 1:dim(marg.s)[2]){
			# make sure to not count the added regions
			which <- which(fit0$summary.random[["region.int"]]$ID == i)[j]
			tmp <- inla.rmarginal(nsim, fit0$marginals.random[["region.int"]][[which]])
			# tmp <- tmp + marg.s[, i] + marg.t[, j]
			st2[counter, "mean"] <- mean(tmp) 
			st2[counter, "med"] <- median(tmp) 
			st2[counter, "low"] <- quantile(tmp, .025) 
			st2[counter, "high"] <- quantile(tmp, .975) 
			counter <- counter + 1
		}
	}
}
st2$regionname <- colnames(mat)[st2$region]
# if(VIS){
# 	pdf(paste0("../figures/", prefix, "random_effects", postfix, ".pdf"), width = 14, height = 10)
# 	g <- mapPlot2(data = st2, geo = geo, variables = c("time"), values = c("mean"), by.data = "regionname", by.geo = byGEO, is.long = TRUE) + scale_fill_viridis_c("effect")+ map.theme + coord_map() + theme(legend.position = c(0.9, 0.05), legend.direction="horizontal")
# 	print(g)
# 	dev.off()

# 	pdf(paste0("../figures/", prefix, "random_effects2", postfix, ".pdf"), width = 9, height = 5)
# 	g <- ggplot(st2, aes(x = time, y = mean, color = regionname, group = regionname)) + geom_line() + theme_bw() + scale_colour_manual(values=colorRampPalette(brewer.pal(name="Set1", n = 8)[-6])(N), guide=FALSE) + ylab("Total random effects")
# 	print(g)
# 	dev.off()
# }
st3 <- st2
# insig <- which(st2$low * st2$high< 0)
# st3[insig, "mean"] <- NA
st3$date <- data$Date[match(st3$time, data$time)]
st3$date <- format(st3$date, "%b %Y")
tmp <- unique(st3$date)
st3$date <- factor(st3$date, levels = tmp)
st3sub <- subset(st3, time %% 5 == 4)
# st3sub <- subset(st3sub, time > min(st3sub$time))
st3sub$rr <- exp(st3sub$mean)
if(VIS){
	max <- max(abs(min(st3sub$mean)), max(st3sub$mean))
	breaks <- exp(seq(-max, max, length.out = 5))
	breaks[1] <- breaks[1] + 1e-5
	breaks[length(breaks)] <- breaks[length(breaks)] - 1e-5
	g <- mapPlot2(data = st3sub, geo = geo, variables = c("date"), values = c("rr"), 
			by.data = "regionname", by.geo = byGEO, is.long = TRUE, border = "gray80", ncol=4) + 
			scale_fill_distiller("Relative Risk", palette = "RdBu", trans = "log", breaks = breaks, lim = exp(c(-max, max)), label = function(x){options( digits = 2);format(x, scientific = FALSE)}) +
			map.theme + coord_map() + 
			theme(legend.position = "bottom", 
			legend.title = element_text(size=6, vjust = 1), 
			legend.text = element_text(size=4), 
			legend.key.width = unit(1, "cm"), 
			legend.key.height = unit(.15, "cm"))+ 
			theme(panel.border = element_blank()) + theme(strip.background = element_blank(), strip.text.x = element_text(vjust = -0.1, size = 6, hjust = .7), panel.spacing = unit(0, "lines")) 
	 ggsave(plot=g, filename=paste0("../figures/", prefix, "random_effects_sub_sig", postfix, ".pdf"), width = 8/1.5, height = 4.5/1.5)
	 ggsave(plot=g, filename=paste0("../figures/", prefix, "random_effects_sub_sig", postfix, ".tiff"), width = 8/1.5, height = 4.5/1.5)

	
	g <- mapPlot2(data = st3, geo = geo, variables = c("date"), values = c("mean"), 
			by.data = "regionname", by.geo = byGEO, is.long = TRUE, border = "gray80") + 
			scale_fill_distiller("Independent Space-Time Effect", palette = "RdBu", na.value = "white", limits = c(-1,1)*max(abs(st3sub$mean)))+ 
			map.theme + coord_map() + 
			theme(legend.position = "bottom", 
			legend.title = element_text(size=12, vjust = 1), 
			legend.key.width = unit(2, "cm"), 
			legend.key.height = unit(.5, "cm"))+ 
			theme(panel.border = element_blank()) + theme(strip.background = element_blank(), strip.text.x = element_text(vjust = -0.1, size = 12, hjust = .7), panel.spacing = unit(0, "lines")) 
	 ggsave(plot=g, filename=paste0("../figures/", prefix, "random_effects_sig", postfix, ".pdf"), width = 12, height = 9)
	 ggsave(plot=g, filename=paste0("../figures/", prefix, "random_effects_sig", postfix, ".tiff"), width = 12, height = 9)
}



if(VIS){
	#-------------------------------
	# Plot of prediction
	#-------------------------------
	data2 <- data	
	data2$pred <- fit0$summary.fitted.values[1:dim(data)[1], "mean"]
	data2$var <- fit0$summary.fitted.values[1:dim(data)[1], "sd"]^2
	data2$low <- fit0$summary.fitted.values[1:dim(data)[1], "0.025quant"]
	data2$high <- fit0$summary.fitted.values[1:dim(data)[1], "0.975quant"]
	data2 <- data2[, c("region", "Date", "regionname", "counts", "pred", "low", "high", "var", "population", "Year", "time")]
	total <- aggregate(counts~regionname, data2, FUN = sum)
	data2$regionname <- factor(data2$regionname, levels = as.character(total$regionname[order(total$counts, decreasing = TRUE)]))
	pdf(paste0("../figures/", prefix, "prediction-vs-truth", postfix, ".pdf"), width = 12, height = 16)
	pages <- 6
	if(N <= 50) pages <- 1
	for(i in 1:pages){
		subset <- 50 * (i-1) + (1 : 50)
		subset <- levels(data2$regionname)[subset[subset <= N]]
		g <- ggplot(subset(data2, regionname %in% subset), aes(x = Date, group = regionname)) + geom_point(aes(y = counts), color = "black", size = .5) + geom_point(aes(y = pred), color = "red", size = .5, alpha = .5)+ geom_errorbar(aes(ymin = low, ymax = high), color = "red", size = .5, alpha = .5)  + line.theme + facet_wrap(~regionname, scale = "free", ncol = 5) + ylab("Posterior")+ 
			theme(panel.border = element_blank()) + theme(strip.background = element_blank(), strip.text.x = element_text(vjust = -0.1, size = 12, hjust = .7), panel.spacing = unit(0, "lines")) 
		print(g)
	}
	dev.off()



	data2$time.index <- data2$time
	data2$time <- format(data2$Date, "%b %Y")
	tmp <- unique(data2$time)
	data2$time <- factor(data2$time, levels = tmp)
	data2sub <- subset(data2, time.index %% 5 == 4)
	# data2sub <- subset(data2sub, time.index > min(data2sub$time.index))
	data2sub$time <- factor(data2sub$time, levels=unique(data2sub$time))
	pdf(paste0("../figures/", prefix, "prediction-map", postfix, ".pdf"), width = 12*1.5, height = 9*1.5)
		g <-  mapPlot2(data = data2, geo = geo, variables = c("time"), values = c("pred"), by.data = "regionname", by.geo = byGEO, is.long = TRUE, border = "gray70") + scale_fill_viridis_c("posterior means of smoothed counts", option = "A")+ map.theme + coord_map() + theme(legend.position = "bottom", 
			legend.title = element_text(size=16, vjust = 1), 
			legend.key.width = unit(2, "cm"), 
			legend.key.height = unit(.5, "cm"))+ 
			theme(panel.border = element_blank()) + theme(strip.background = element_blank(), strip.text.x = element_text(vjust = -0.1, size = 16, hjust = .7), panel.spacing = unit(0, "lines")) 
		print(g)
	dev.off()

	pdf(paste0("../figures/", prefix, "prediction-map2", postfix, ".pdf"), width = 12*1.5, height = 9*1.5)
		g <-  g + scale_fill_viridis_c("posterior means of smoothed counts", option = "A", direction = -1)
		print(g)
	dev.off()

	g <-  mapPlot2(data = data2sub, geo = geo, variables = c("time"), values = c("pred"), by.data = "regionname", by.geo = byGEO, is.long = TRUE, ncol = 4, border = "gray60") + scale_fill_viridis_c("posterior means of smoothed counts", option = "A", trans='sqrt', breaks = c(1, 2, 3, 5, 10, 20))+ map.theme + coord_map() + theme(legend.position = "bottom", 
			legend.title = element_text(size=6, vjust = 1), 
			legend.text = element_text(size=4), 
			legend.key.width = unit(1, "cm"), 
			legend.key.height = unit(.15, "cm"))+ 
			theme(panel.border = element_blank()) + theme(strip.background = element_blank(), strip.text.x = element_text(vjust = -0.1, size = 6, hjust = .7), panel.spacing = unit(0, "lines")) 
	 ggsave(plot=g, filename=paste0("../figures/", prefix, "prediction-subset-map", postfix, ".pdf"), width = 8/1.5, height = 4.5/1.5)
	 ggsave(plot=g, filename=paste0("../figures/", prefix, "prediction-subset-map", postfix, ".tiff"), width = 8/1.5, height = 4.5/1.5)

	g <- g +  scale_fill_viridis_c("posterior means of smoothed counts", option = "A", trans='sqrt', breaks = c(1, 2, 3, 5, 10, 20), direction = -1)
	 ggsave(plot=g, filename=paste0("../figures/", prefix, "prediction-subset-map2", postfix, ".pdf"), width = 8/1.5, height = 4.5/1.5)
	 ggsave(plot=g, filename=paste0("../figures/", prefix, "prediction-subset-map2", postfix, ".tiff"), width = 8/1.5, height = 4.5/1.5)


	# data2$pred[data2$high < 1] <- NA
	# data2sub$pred[data2sub$high < 1] <- NA
	# pdf(paste0("../figures/", prefix, "prediction-map-r0",  postfix, ".pdf"), width = 12, height = 9)
	# 	g <-  mapPlot2(data = data2, geo = geo, variables = c("time"), values = c("pred"), by.data = "regionname", by.geo = byGEO, is.long = TRUE, border = "gray80") + scale_fill_viridis_c("posterior means of smoothed counts",na.value="white")+ map.theme + coord_map() + theme(legend.position = "bottom", 
	# 		legend.title = element_text(size=12), 
	# 		legend.key.width = unit(2, "cm"), 
	# 		legend.key.height = unit(.5, "cm"))
	# 	print(g)
	# dev.off()
	# pdf(paste0("../figures/", prefix, "prediction-subset-map-r0",  postfix, ".pdf"), width = 12, height = 4.7)
	# 	g <-  mapPlot2(data = data2sub, geo = geo, variables = c("time"), values = c("pred"), by.data = "regionname", by.geo = byGEO, is.long = TRUE, border = "gray80", ncol=7) + scale_fill_viridis_c("posterior means of smoothed counts",na.value="white")+ map.theme + coord_map() + theme(legend.position = "bottom", 
	# 		legend.title = element_text(size=12), 
	# 		legend.key.width = unit(2, "cm"), 
	# 		legend.key.height = unit(.4, "cm"))
	# 	print(g)
	# dev.off()

	# data2$pred[data2$high < 3] <- NA
	# data2sub$pred[data2sub$high < 3] <- NA
	# pdf(paste0("../figures/", prefix, "prediction-map-r2",  postfix, ".pdf"), width = 12, height = 9)
	# 	g <-  mapPlot2(data = data2, geo = geo, variables = c("time"), values = c("pred"), by.data = "regionname", by.geo = byGEO, is.long = TRUE) + scale_fill_viridis_c("posterior means of smoothed counts",na.value="white")+ map.theme + coord_map() + theme(legend.position = "bottom", 
	# 		legend.title = element_text(size=12), 
	# 		legend.key.width = unit(2, "cm"), 
	# 		legend.key.height = unit(.5, "cm"))
	# 	print(g)
	# dev.off()
	# pdf(paste0("../figures/", prefix, "prediction-subset-map-r2",  postfix, ".pdf"), width = 12, height = 4.7)
	# 	g <-  mapPlot2(data = data2sub, geo = geo, variables = c("time"), values = c("pred"), by.data = "regionname", by.geo = byGEO, is.long = TRUE, ncol=7) + scale_fill_viridis_c("posterior means of smoothed counts",na.value="white")+ map.theme + coord_map() + theme(legend.position = "bottom", 
	# 		legend.title = element_text(size=12), 
	# 		legend.key.width = unit(2, "cm"), 
	# 		legend.key.height = unit(.4, "cm"))
	# 	print(g)
	# dev.off()

	# data2$counts[data2$counts < 1] <- NA
	# data2sub$counts[data2sub$counts < 1] <- NA
	# pdf(paste0("../figures/", prefix, "prediction-map-r0-observed",  postfix, ".pdf"), width = 12, height = 9)
	# 	g <-  mapPlot2(data = data2, geo = geo, variables = c("time"), values = c("counts"), by.data = "regionname", by.geo = byGEO, is.long = TRUE, border = "gray80") + scale_fill_viridis_c("observed counts",na.value="white")+ map.theme + coord_map() + theme(legend.position = "bottom", 
	# 		legend.title = element_text(size=12), 
	# 		legend.key.width = unit(2, "cm"), 
	# 		legend.key.height = unit(.5, "cm"))
	# 	print(g)
	# dev.off()
	# pdf(paste0("../figures/", prefix, "prediction-map-r0-observed",  postfix, ".pdf"), width = 12, height = 4.7)
	# 	g <-  mapPlot2(data = data2sub, geo = geo, variables = c("time"), values = c("counts"), by.data = "regionname", by.geo = byGEO, is.long = TRUE, border = "gray80", ncol=7) + scale_fill_viridis_c("observed counts",na.value="white")+ map.theme + coord_map() + theme(legend.position = "bottom", 
	# 		legend.title = element_text(size=12), 
	# 		legend.key.width = unit(2, "cm"), 
	# 		legend.key.height = unit(.4, "cm"))
	# 	print(g)
	# dev.off()
	if(byGEO == "GEOID" && FALSE){
		data2$SNA <- geobg$table[match(as.character(data2[, byDATA]), geobg$table$bg), 1]
		data2$groups = data2[, byDATA]
		pdf("../figures/", prefix, "neighborhoods-1.pdf", width = 10, height = 3)
		for(ii in 1:length(unique(data2$SNA))){
			toplot <- unique(data2$SNA)[ii]
			g1 <- ggplot(subset(data2, SNA %in% toplot), aes(x = Date, y = pred, group = groups, color = SNA, ymin = low, ymax=high))  + geom_point(aes(y = counts), color = "gray30")+ geom_line(alpha=.5) + geom_errorbar(alpha=.3) + scale_colour_manual(values=colorRampPalette(brewer.pal(name="Set1", n = 8)[-6])(length(unique(data2$SNA))), guide=FALSE) + line.theme + ggtitle(" ") + ylab(" ")+ xlab("Month") + facet_wrap(~regionname, nrow = 1) + ggtitle(c(as.character(toplot)))
			print(g1)
		}
		dev.off()
	}else{
		data2$SNA <- data2[, byDATA]
	}


	# data2$risk <- data2$counts/data2$population
	# data2$risk.smooth <- data2$pred/data2$population
	# data2$risk.low <- log(data2$low/data2$population)
	# data2$risk.high <- log(data2$high/data2$population)
	# # range <- range(c(data2$risk, data2$risk.smooth))
	# ggplot(data2, aes(x = counts, y = pred, color = Date), alpha = 0.05) + geom_point() + geom_abline(intercept = 0, slope = 1) + facet_wrap(~Date)
	# pdf(paste0("../figures/", prefix, "prediction-vs-truth-map", postfix, ".pdf"), width = 12, height = 16)
	# data2$Datetext <- as.character(data2$Date)
	# g <- mapPlot(data = subset(data2, Year == 2017), geo = geo, variables = c("Datetext"), values = c("pred"), by.data = "regionname", by.geo = "GEOID", is.long = TRUE) + scale_fill_viridis_c("counts")+ map.theme + coord_map() + theme(legend.position = c(0.9, 0.05), legend.direction="horizontal")
	# g0 <- mapPlot(data = subset(data2, Year == 2017), geo = geo, variables = c("Datetext"), values = c("counts"), by.data = "regionname", by.geo = "GEOID", is.long = TRUE) + scale_fill_viridis_c("counts")+ map.theme + coord_map() + theme(legend.position = c(0.9, 0.05), legend.direction="horizontal")

	# dev.off()
	# data2$logrisk <- log(data2$risk)
	# data2$logrisk.smooth <- log(data2$risk.smooth)
	# data2$sd.naive <- sqrt(data2$counts / data2$population^2)
	# data2$sd.smooth <- sqrt(data2$var / data2$population^2)
	# range <- range(c(data2$sd.naive, data2$sd.smooth))
	# pdf(paste0("../figures/", prefix, "SD.pdf", width = 7, height = 5)
	# g <- ggplot(data2, aes(x = sd.naive, y = sd.smooth, size = population)) + geom_point(alpha = 0.2, color = "black") + geom_abline(intercept = 0, slope = 1) + theme_bw() + xlim(range) + ylim(range) + xlab("Naive SE") + ylab("Posterior SD")
	# print(g)
	# dev.off()
	# g1 <- ggplot(data2, aes(x = risk.smooth, y = sd.smooth, size = population)) + geom_point(alpha = 0.2, color = "gray10") + geom_abline(intercept = 0, slope = 1) + theme_bw()  + ylim(range)
	# g2 <- ggplot(data2, aes(x = risk, y = sd.naive, size = population)) + geom_point(alpha = 0.2, color = "gray10") + geom_abline(intercept = 0, slope = 1) + theme_bw() + ylim(range)
	# grid.arrange(g1,g2, ncol = 2) 
	# if(byGEO == "GEOID"){

	# pdf(paste0("../figures/", prefix, "neighborhoods-1.pdf", width = 10, height = 3)
	# for(ii in 1:length(unique(data2$SNA))){
	# 	toplot <- unique(data2$SNA)[ii]
	# 	g1 <- ggplot(subset(data2, SNA %in% toplot), aes(x = Date, y = logrisk.smooth, group = group, color = SNA, ymin = risk.low, ymax=risk.high))  + geom_point(aes(y = logrisk), color = "gray30")+ geom_line(alpha=.5) + geom_errorbar(alpha=.3) + scale_colour_manual(values=colorRampPalette(brewer.pal(name="Set1", n = 8)[-6])(length(unique(data2$SNA))), guide=FALSE) + line.theme + ggtitle(" ") + ylab(" ")+ xlab("Month") + facet_wrap(~regionname, nrow = 1) + ggtitle(c(as.character(toplot)))
	# 	print(g1)
	# }
	# dev.off()
	# pdf(paste0("../figures/", prefix, "neighborhoods-2.pdf", width = 10, height = 3)
	# for(ii in 1:length(unique(data2$SNA))){
	# 	toplot <- unique(data2$SNA)[ii]
	# 	total <- aggregate(counts~regionname+SNA+Date, data2, FUN = sum)
	# 	ordered <- as.character(format(sort(unique(total$Date)), "%b %Y"))
	# 	total$Date <-as.character(format(total$Date, "%b %Y"))
	# 	total$Date <- factor(total$Date, ordered)
	# 	total$risk <- total$count / data2$population[match(total$regionname, data2$regionname)]
	# 	total$logrisk <- log(total$risk)
	# 	# total$logrisk[total$SNA %in% toplot == FALSE] <- NA
	# 	geo1 <- subset(geo, GEOID %in% total$regionname[total$SNA %in% toplot])
	# 	geo2 <- fortify(geo1, region = "GEOID")
	#     geo2 <- by(geo2, geo2$id, function(x) {Polygon(x[c('long', 'lat')])@labpt})
	#     centroids <- setNames(do.call("rbind.data.frame", geo2), c('long', 'lat'))
	#     centroids$label <- names(geo2) 
	# 	g2 <- mapPlot(data = total, geo = geo1, values = c("logrisk"), variables = "Date",  by.data = "regionname", by.geo = "GEOID" ,is.long = TRUE)+ggplot2::facet_wrap(~variable, nrow = 3) + scale_fill_viridis_c("log total incidents per resident")+ map.theme + coord_map() + theme(legend.position="bottom") + ggtitle(c(as.character(toplot)))#+ geom_label_repel(data = centroids, aes(x = long, y = lat, label = label), force = 100, alpha = 0.5, size = 0.3) 
	# 	print(g2)
	# }
	# dev.off()


}


