library(ggrepel)
library(scales)
library(gridExtra)
mapPlot2 <- function(data, variables, values = NULL, labels = NULL, geo, by.data, by.geo, is.long = FALSE, thin = TRUE, removetab = FALSE, border = "gray20", ncol = NULL){
    value <- group <- lat <- long <- NULL
    if (is.null(labels) & !is.long) {
        labels <- variables
    }
    if (is.null(labels) & is.long) {
        labels <- sort(unique(data[, variables]))
    }
    if (is.null(values) & is.long) {
        stop("values need to be specified for long format input.")
    }
    geo <- ggplot2::fortify(geo, region = by.geo)
    if (!is.long) {
        data <- data[, c(variables, by.data)]
        data <- reshape2::melt(data)
        data$variable <- factor(data$variable, levels = variables)
        levels(data$variable) <- labels
    }
    else {
        data$value <- data[, values]
        data$variable <- data[, variables]
        data$variable <- as.character(data$variable)
        data$variable <- factor(data$variable, levels = labels)
    }
    geo2 <- merge(geo, data, by = "id", by.y = by.data)
    g <- ggplot2::ggplot(geo2)
    g <- g + ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, 
        group = group, fill = value), color = border, size = ifelse(thin, 0.2, 0.5))
    if(length(unique(data$variable)) > 1 || removetab == FALSE){
	    if(is.null(ncol)){
		    g <- g + ggplot2::facet_wrap(~variable)
	    }else{
		    g <- g + ggplot2::facet_wrap(~variable, ncol = ncol)
	    }
    }
    g <- g + ggplot2::scale_fill_viridis_c()
    return(g)
}


## Visualize covariates (Space-invariant)
sub <- Y[Y$region == 1, ]
sub <- sub[sub$region <= N, ]
g1 <-  ggplot(sub, aes(x = Date, y = temperature)) + geom_point(alpha=.6) + geom_line(alpha=1) + ylab("Temperature") + xlab(NULL)  + scale_x_date(breaks = "6 month", labels = date_format("%b %Y"), lim = c(min(Y$Date), max(Y$Date))) + line.theme + theme(panel.border = element_blank())
g2 <-  ggplot(sub, aes(x = Date, y = precipitation)) + geom_point(alpha=.6) + geom_line(alpha=1) + ylab("Precipitation") + xlab(NULL)  + scale_x_date(breaks = "6 month", labels = date_format("%b %Y"), lim = c(min(Y$Date), max(Y$Date))) + line.theme + theme(panel.border = element_blank())
out <- grid.arrange(g1, g2, ncol = 1)
ggsave(plot = out, filename = paste0("../figures/weather", postfix, ".pdf"), width = 12, height = 4)

## Visualize covariates (Time-invariant)
sub <- Y[Y$time == 1, ]
sub <- sub[sub$region <= N, ]
toplot <- NULL

if("pc_age25" %in% colnames(Y)){
	toplot$variables <- c("pc_age25", "pc_male", "pc_male25", "pc_white", "pc_black",  "pc_asian", 
		# "pc_ind_alaska", "pc_hawaiian", 
		"pc_two_races", "pc_hispanic")
	toplot$labels <- c( "25 Yrs or Older", "Male", "Male 25 Yrs or Older", "White", "Black", "Asian", 
		# "American Indian or Alaska Native", "Hawaiian", 
		"Two or More Races", "Hispanic" )
	longfixed <- FALSE
}else{
	toplot$variables <- c("pc_male", "pc_age18_24", "pc_age25_34", "pc_age35_49", "pc_age50_64", "pc_age65up",  "pc_white", "pc_black",  "pc_asian", 
		# "pc_ind_alaska", "pc_hawaiian", 
		"pc_two_races", "pc_hispanic")
	toplot$labels <- c( "Male", "Age 18 to 24", "Age 25 to 34", "Age 35 to 49", "Age 50 to 64", "Age 65 or Older", "White", "Black", "Asian", 
		# "American Indian or Alaska Native", "Hawaiian", 
		"Two or More Races", "Hispanic" )
	longfixed <- TRUE
}

# whichdirection = -1
# whichviridis = "A"
whichviridis = "D"
whichdirection = 1

glist <- NULL
glist[[1]] <- mapPlot2(data = sub, 
		variables = "population", 
		labels = "Population", 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Population", option = whichviridis, direction = whichdirection)+ map.theme + coord_map() 

for(i in 1:length(toplot$variables)){
	glist[[i+1]] <-  mapPlot2(data = sub, 
		variables = toplot$variables[i], 
		labels = toplot$labels[i], 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Proportion", option = whichviridis, direction = whichdirection)+ map.theme + coord_map() 
}
i <- length(glist) + 1

glist[[i]] <- mapPlot2(data = sub, 
		variables = "household_income", 
		labels = "Median Household Income", 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Dollars", option = whichviridis, direction = whichdirection)+ map.theme + coord_map() 
i <- i+1
glist[[i]]  <- mapPlot2(data = sub, 
		variables = "per_cap_income", 
		labels = "Per Capita Income", 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Dollars", option = whichviridis, direction = whichdirection) + map.theme + coord_map()
i <- i+1
glist[[i]]  <- mapPlot2(data = sub, 
		variables = "pc_poverty", 
		labels = "Poverty Status", 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Proportion", option = whichviridis, direction = whichdirection)+ map.theme + coord_map() 
i <- i+1
if(longfixed){
	glist[[i]]  <- mapPlot2(data = sub, 
		variables = "home_value", 
		labels = "Median Home Value", 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Dollars", option = whichviridis, direction = whichdirection)+ map.theme + coord_map() 
	i <- i+1
	glist[[i]]  <- mapPlot2(data = sub, 
		variables = "home_value_change", 
		labels = "Median Home Value Change Since 2013", 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Dollars", option = whichviridis, direction = whichdirection)+ map.theme + coord_map() 
	i <- i+1
}

glist[[i]]  <- mapPlot2(data = sub, 
		variables = "pc_bachelor", 
		labels = "Bachelor Degree", 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Proportion", option = whichviridis, direction = whichdirection) + map.theme + coord_map()
i <- i+1
# pdf(paste0("../figures/income_education", postfix, ".pdf"), width = 11, height = 6)
# grid.arrange(grobs = list(g1, g2, g3, g4), ncol = 2)
# dev.off()

glist[[i]]  <- mapPlot2(data = sub, 
		variables = "pc_park", 
		labels = "Parks", 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Proportion", option = whichviridis, direction = whichdirection)+ map.theme + coord_map()
i <- i+1
# glist[[i]]  <- mapPlot2(data = sub, 
# 		variables = "pc_bus_half", 
# 		labels = "Areas Covered by Half-mile \nRadius of Bus Stops", 
# 		geo = geo, by.data = byDATA, 
# 		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Proportion")+ map.theme + coord_map()
# i <- i+1
glist[[i]]  <- mapPlot2(data = sub, 
		variables = "pc_bus_quarter", 
		labels = "Bus Stops Coverage", 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Proportion", option = whichviridis, direction = whichdirection)+ map.theme + coord_map()
i <- i+1
# glist[[i]]  <- mapPlot2(data = sub, 
# 		variables = "bus_count", 
# 		labels = "Number of Bus Stops", 
# 		geo = geo, by.data = byDATA, 
# 		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Incidents")+ map.theme + coord_map()
# i <- i+1
glist[[i]]  <- mapPlot2(data = sub, 
		variables = "fire", 
		labels = "Distance to Closest Fire Station", 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Miles", option = whichviridis, direction = whichdirection)+ map.theme + coord_map()

i <- i+1

glist[[i]]  <- mapPlot2(data = sub, 
		variables = "pharm", 
		labels = "Distance to Closest Pharmacy", 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Miles", option = whichviridis, direction = whichdirection)+ map.theme + coord_map()

i <- i+1

glist[[i]]  <- mapPlot2(data = sub, 
		variables = "hospital", 
		labels = "Distance to Closest Hospital", 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Miles", option = whichviridis, direction = whichdirection)+ map.theme + coord_map()

i <- i+1
glist[[i]]  <- mapPlot2(data = sub, 
		variables = "fqhc", 
		labels = "Distance to Closest FQHC", 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Miles", option = whichviridis, direction = whichdirection)+ map.theme + coord_map()
i <- i+1
glist[[i]]  <- mapPlot2(data = sub, 
		variables = "otp", 
		labels = "Distance to Closest OTP", 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Miles", option = whichviridis, direction = whichdirection)+ map.theme + coord_map()

i <- i+1

glist[[i]]  <- mapPlot2(data = sub, 
		variables = "bup", 
		labels = "Distance to Closest Buprenorphine \nPrescribing Physicians", 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Miles", option = whichviridis, direction = whichdirection) + map.theme + coord_map() 

i <- i+1
glist[[i]]  <- mapPlot2(data = sub, 
		variables = "fastfood", 
		labels = "Number of Fast Food Restaurants", 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Numbers", option = whichviridis, direction = whichdirection) + map.theme + coord_map() 
i <- i+1

# Zoning information
glist[[i]]  <- mapPlot2(data = sub, 
		variables = "pc_singlefamily", 
		labels = "Zoning: Single-family", 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Proportion", option = whichviridis, direction = whichdirection) + map.theme + coord_map() 
i <- i+1
glist[[i]]  <- mapPlot2(data = sub, 
		variables = "pc_residential_other", 
		labels = "Zoning: Other residential", 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Proportion", option = whichviridis, direction = whichdirection) + map.theme + coord_map() 
i <- i+1

glist[[i]]  <- mapPlot2(data = sub, 
		variables = "pc_office", 
		labels = "Zoning: Office", 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Proportion", option = whichviridis, direction = whichdirection) + map.theme + coord_map() 
i <- i+1
glist[[i]]  <- mapPlot2(data = sub, 
		variables = "pc_commercial", 
		labels = "Zoning: Commercial", 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Proportion", option = whichviridis, direction = whichdirection) + map.theme + coord_map() 
i <- i+1
glist[[i]]  <- mapPlot2(data = sub, 
		variables = "pc_urban_mixed", 
		labels = "Zoning: Urban Mixed", 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Proportion", option = whichviridis, direction = whichdirection) + map.theme + coord_map() 
i <- i+1
glist[[i]]  <- mapPlot2(data = sub, 
		variables = "pc_downtown", 
		labels = "Zoning: Downtown", 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Proportion", option = whichviridis, direction = whichdirection) + map.theme + coord_map() 
i <- i+1

glist[[i]]  <- mapPlot2(data = sub, 
		variables = "pc_manufacturing", 
		labels = "Zoning: Manufacturing", 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Proportion", option = whichviridis, direction = whichdirection) + map.theme + coord_map() 
i <- i+1
glist[[i]]  <- mapPlot2(data = sub, 
		variables = "pc_riverfront", 
		labels = "Zoning: River front", 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Proportion", option = whichviridis, direction = whichdirection) + map.theme + coord_map() 
i <- i+1
glist[[i]]  <- mapPlot2(data = sub, 
		variables = "pc_development", 
		labels = "Zoning: Planned Development", 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Proportion", option = whichviridis, direction = whichdirection) + map.theme + coord_map() 
i <- i+1
# glist[[i]]  <- mapPlot2(data = sub, 
# 		variables = "pc_recreation", 
# 		labels = "Zoning: Parks and recreational", 
# 		geo = geo, by.data = byDATA, 
# 		by.geo = byGEO, is.long = FALSE) + scale_fill_viridis_c("Proportion") + map.theme + coord_map() 
# i <- i+1

glist1 <- NULL
for(i in 1:length(glist)){
	glist1[[i]] <- glist[[i]] + theme(panel.border = element_blank()) + theme(strip.background = element_blank(), strip.text.x = element_text( vjust = -0.1, size = 16)) + theme(legend.position = c(0.1, 0.72), legend.key.size = unit(.85,"line"), plot.margin=margin(0,0,0,0, unit="cm")) 

}

if(longfixed){
	out <- 	grid.arrange(grobs = glist1, ncol = 6)
	ggsave(plot = out, width = 24, height = 18.5, filename = paste0("../figures/covariates", postfix, ".pdf"))

}else{
	pdf(paste0("../figures/covariates", postfix, ".pdf"), width = 17/4*5, height = 24/8*6)
	grid.arrange(grobs = glist1, ncol = 6)
	dev.off()	

}


out <- grid.arrange(grobs = list(
	glist1[[2]], glist1[[3]], glist1[[5]], glist1[[13]],
	glist1[[18]], glist1[[19]], glist1[[31]], glist1[[34]]) 
	, ncol = 4)
ggsave(plot = out, width = 16, height = 6*.95, filename = paste0("../figures/covariates-subset", postfix, ".pdf"))



out <- grid.arrange(grobs = list(
	glist1[[2]] + scale_fill_viridis_c("Proportion", direction = -1), 
	glist1[[3]] + scale_fill_viridis_c("Proportion", direction = -1), 
	glist1[[5]] + scale_fill_viridis_c("Proportion", direction = -1), 
	glist1[[13]] + scale_fill_viridis_c("Dollars", direction = -1),
	glist1[[18]] + scale_fill_viridis_c("Proportion", direction = -1), 
	glist1[[19]] + scale_fill_viridis_c("Proportion", direction = -1), 
	glist1[[31]] + scale_fill_viridis_c("Proportion", direction = -1), 
	glist1[[34]] + scale_fill_viridis_c("Proportion", direction = -1)) 
	, ncol = 4)
ggsave(plot = out, filename = paste0("../figures/covariates-subset-color1", postfix, ".pdf"), width = 16, height = 6*.95)

whichviridis2 <- "A"
out <- grid.arrange(grobs = list(
	glist1[[2]] + scale_fill_viridis_c("Proportion", direction = -1, option=whichviridis2), 
	glist1[[3]] + scale_fill_viridis_c("Proportion", direction = -1, option=whichviridis2), 
	glist1[[5]] + scale_fill_viridis_c("Proportion", direction = -1, option=whichviridis2), 
	glist1[[13]] + scale_fill_viridis_c("Dollars", direction = -1, option=whichviridis2),
	glist1[[18]] + scale_fill_viridis_c("Proportion", direction = -1, option=whichviridis2), 
	glist1[[19]] + scale_fill_viridis_c("Proportion", direction = -1, option=whichviridis2), 
	glist1[[31]] + scale_fill_viridis_c("Proportion", direction = -1, option=whichviridis2), 
	glist1[[34]] + scale_fill_viridis_c("Proportion", direction = -1, option=whichviridis2)) 
	, ncol = 4)
ggsave(plot = out, filename=paste0("../figures/covariates-subset-color2", postfix, ".pdf"), width = 16, height = 6*.95)


#####################################################
### Crime count line plot
#####################################################
total <- aggregate(crime~regionname, Y, sum)
total$crime_rate <- total$crime / sub$population[match(total[, byDATA], sub[, byDATA])]
tmp <- Y[Y$region <= N, ]
for(i in 1:dim(tmp)[1]) tmp$cumcrime[i] <- sum(tmp$crime[intersect(which(tmp$region == tmp$region[i]), which(tmp$time <= tmp$time[i]))])
if(byGEO == "GEOID"){
	tmp$SNA <- gsub("390610", "", tmp[, byDATA])
}else{
	tmp$SNA <- tmp[, byDATA]
}
tmp$label <- tmp$SNA
tmp$label[tmp$time != max(tmp$time)] <- NA
thres <- sort(tmp$cumcrime[tmp$time == max(tmp$time)], decreasing = TRUE)[5 + 5 * (dim(mat)[1] > 100)]
tmp$label[tmp$cumcrime < thres] <- NA
# tmp$label2c <- tmp$label
# tmp$label2c[!is.na(tmp$label2c)][order(tmp$cumcrime[!is.na(tmp$label2c)], decreasing = TRUE)] <- (1:10)
tmp$group = tmp[, byDATA]
labeled.crime <- unique(tmp$label[tmp$cumcrime >= thres])
g2 <- ggplot(tmp, aes(x = Date, y = cumcrime, group = group, color = SNA, label = label)) + geom_line(alpha=.5) + scale_colour_manual(values=colorRampPalette(brewer.pal(name="Set1", n = 8)[-6])(length(unique(tmp$SNA))), guide=FALSE) + line.theme + ggtitle("Cumulative Number of Crimes by Block Group") + ylab("Cumulative Crime Incidents")+ scale_x_date(breaks = "6 month", labels = date_format("%b %Y"), lim = c(min(Y$Date), max(Y$Date)+40*0))+ theme(panel.border = element_blank()) + xlab(NULL)# + geom_text_repel(aes(label = label), nudge_x = 40, vjust = "bottom", hjust = "left", alpha = .9, size = 3, direction="y", segment.size = 0.5, min.segment.length = 0.01, force=0.1, segment.color = "gray50", box.padding = .01)

#####################################################
### OD count line plot avg
#####################################################
total <- aggregate(counts~Date + regionname, Y, sum)
avg <- aggregate(counts~Date, data = Y, FUN = sum) 
avg$counts <- avg$counts/N
if(byGEO == "GEOID"){
	total$SNA <- paste0(total[, byDATA], "-", geobg$table[match(as.character(total[, byDATA]), geobg$table$bg), 1])
}else{
	total$SNA <- total[, byDATA]
}
total$Date <- as.Date(total$Date)
g3 <- ggplot(total, aes(x = Date, y = counts)) + geom_line(alpha=.1, aes(group = regionname, color = SNA)) + geom_line(data = avg, aes(x = Date, y = counts), color = "darkblue", size = .6) + scale_colour_manual(values=colorRampPalette(brewer.pal(name="Set1", n = 8)[-6])(N), guide=FALSE) + line.theme + ylab("Monthly Incidents") + xlab(NULL)  + scale_x_date(breaks = "6 month", labels = date_format("%b %Y"), lim = c(min(Y$Date), max(Y$Date)+40)) + xlab(NULL) + theme(panel.border = element_blank())


#####################################################
### OD count line cumulative
#####################################################
for(i in 1:dim(tmp)[1]) tmp$cumcount[i] <- sum(tmp$count[intersect(which(tmp$region == tmp$region[i]), which(tmp$time <= tmp$time[i]))])
tmp$label <- gsub("390610", "", tmp[, byDATA])	
tmp$label2 <- geobg$table[match(tmp$regionname, geobg$table$bg), "SNA_NAME"]
tmp$SNA <- tmp$label2
tmp$label2[tmp$time != max(tmp$time)] <- NA
thres <- sort(tmp$cumcount[tmp$time == max(tmp$time)], decreasing = TRUE)[5]
tmp$label2 <- tmp$label2
# tmp$label2[!is.na(tmp$label2)][order(tmp$cumcount[!is.na(tmp$label2)], decreasing = TRUE)] <- (1:10)
tmp$label2[tmp$cumcount <= thres] <- NA
tmp$label2 <- as.character(tmp$label2)
for(sn in unique(tmp$label2)){
	if(is.na(sn)) next
	if(length(which(tmp$label2==sn)) > 1){
		loc <- which(tmp$label2==sn)
		order <- order(tmp$cumcount[loc], decreasing = TRUE)
		tmp$label2[loc[order]] <- paste(sn, 1:length(order), sep = "-")
	}
}
labeled.od2 <- unique(tmp$label2)
labeled.od <-tmp$label[!is.na(tmp$label2)]
tmp0 <- tmp
tmp0 <- tmp0[!is.na(tmp0$label2), ]
g4 <- ggplot(tmp, aes(x = Date, y = cumcount, group = regionname, color = SNA)) + geom_line(alpha=.5) + scale_colour_manual(values=colorRampPalette(brewer.pal(name="Set1", n = 8)[-6])(length(unique(tmp$SNA))), guide=FALSE) + line.theme + ggtitle(NULL) + 
	ylab("Cumulative Incidents") + xlab(NULL)  + scale_x_date(breaks = "6 month", labels = date_format("%b %Y"), lim = c(min(Y$Date), max(Y$Date)+40*0))+ theme(panel.border = element_blank()) +	geom_text_repel(aes(label = label2), nudge_x = -100, nudge_y = 0, vjust = "bottom", hjust = "left", alpha = .9, size = 5, direction="both", segment.size = 0.5, min.segment.length = 0.01, segment.color = "gray60", box.padding = .05) 

#####################################################
### OD count map
#####################################################
## Visualize counts (Time-variant)
total <- aggregate(counts~regionname, Y, sum)
total$od_rate <- total$counts / sub$population[match(total[, byDATA], sub[, byDATA])]
tmp <- fortify(geo, region = byGEO)
tmp <- by(tmp, tmp$id, function(x) {Polygon(x[c('long', 'lat')])@labpt})
centroids <- setNames(do.call("rbind.data.frame", tmp), c('long', 'lat'))
rownames(centroids) <- names(tmp)
if(byGEO == "GEOID"){
	centroids$label <- gsub("390610", "", rownames(centroids))
	centroids$label2 <- tmp0$label2[match(centroids$label, tmp0$label)]
}else{
	centroids$label <- rownames(centroids)
}
g5 <-  mapPlot2(data = total, 
		variables = c("counts"),
		label = c("Total number of incidents"), 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE, removetab = TRUE, border = "gray50") + scale_fill_viridis_c("Incidents", option = "A", trans = "sqrt") + map.theme + coord_map()  + theme(legend.position = c(0.1, 0.75), legend.title =  element_text(size = 12)) + theme(panel.border = element_blank())+ theme(plot.margin=margin(b=-3, t=-2.5, unit="cm"))  

centroids1 <- centroids[centroids$label %in% labeled.crime, ]
nudge_y <- 39.41 - centroids1$lat  
nudge_y[centroids1$lat < 39.1432] <- 39.06 - centroids1$lat[centroids1$lat < 39.1432]
nudge_y <- nudge_y + runif(length(nudge_y), 0, 0.01)
g5crime <- g5 + geom_text_repel(data = centroids1, aes(x=long, y = lat, label = label), alpha = .9, size = 3, segment.size = .5, min.segment.length = 0, force =5, color = 'darkred', seed = 1, nudge_y = nudge_y, direction = "x")  

centroids2 <- centroids[centroids$label %in% labeled.od, ]
nudge_y <- 39.5 - centroids2$lat  
nudge_y[centroids2$lat < 39.1432] <- (39.06 - centroids2$lat[centroids2$lat < 39.1432])+ 0.01 #+ 0.5 * (84.52 + centroids2$long)
# nudge_y <- nudge_y + 0.005 + runif(length(nudge_y), 0, 0.02)
nudge_x <- (-84.515 - centroids2$long)
nudge_y[which.min(nudge_x)] <- -0.02
nudge_x[nudge_x < 0] <- -0.01
g5od <- g5 + geom_text_repel(data = centroids2, aes(x=long, y = lat, label = label2), alpha = .9, size = 4, segment.size = .5, min.segment.length = 0, force =1, color = 'black', seed = 1, nudge_y = nudge_y, direction = "x", nudge_x = nudge_x, box.padding =.3, segment.color = 'gray50')
g5rate <-  mapPlot2(data = total, 
		variables = c("od_rate"),
		label = c("Incidents per resident"), 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE, removetab = TRUE) + scale_fill_viridis_c("Incidents per resident") + map.theme + coord_map()    + theme(legend.position = c(0.15, 0.75))
total[total[, byDATA]== "390610263001", "od_rate"] <- NA
total[total[, byDATA] == "Queensgate", "od_rate"] <- NA
g5rate.rm <-  mapPlot2(data = total, 
		variables = c("od_rate"),
		label = c("Incidents per resident"), 
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = FALSE, removetab = TRUE) + scale_fill_viridis_c("Incidents per resident") + map.theme + coord_map()   + theme(legend.position = c(0.15, 0.75)) 

#####################################################
### OD count line total
#####################################################
avg <- aggregate(counts~Date, data = Y, FUN = sum) 
avg$Date <- (as.Date(avg$Date)) 
g6 <-  ggplot(avg, aes(x = Date, y = counts)) + geom_point(alpha=.3, size = .5) + geom_line(data = avg, color = "darkblue", size = .6)  + ylab("Monthly Incidents") + xlab(NULL)  + scale_x_date(breaks = "6 month", labels = date_format("%b %Y"), lim = c(min(Y$Date), max(Y$Date)+40)) + theme_bw()

g7 <- ggplot() + geom_polygon(data=geo, aes(x = long, y = lat, group = group), color = "gray50", fill = "gray90", alpha = .9)+ coord_map() + theme_map() + scale_fill_discrete(guide = FALSE) + geom_point(data = Data, aes(x = LONGITUDE_X, y = LATITUDE_X), size = .4, color = "red", alpha = .2) + annotate("text", x = -84.7, y = 39.21, label = "(A)", size=8, family="Courier", fontface="bold") + annotate("text", x = -84.7, y = 39.06, label = "(B)", size=8, family="Courier", fontface="bold") + theme(plot.margin=margin(b=-3, t=-2.5, unit="cm"))  

g6b <- g6 + theme(plot.margin = margin(b=0.5,t=0.5))+ theme(panel.border = element_blank()) + ylim(range(c(avg$counts, 0)))

out <- grid.arrange(g7, g6b, ncol = 1, heights=c(3,1))
ggsave(plot = out, filename = paste0("../figures/od-timeseries-and-map", postfix, ".pdf"), width = 8, height = 6)

out <- grid.arrange(grobs=list(g4, g5od), ncol = 2)
ggsave(plot = out, filename = paste0("../figures/od-data", postfix, ".pdf"), width = 13, height = 4.2)

g5od <- g5od + scale_fill_viridis_c("Incidents", option = "A", trans = "sqrt", direction = -1)
out <- grid.arrange(grobs=list(g4, g5od), ncol = 2)
ggsave(plot = out, filename = paste0("../figures/od-data2", postfix, ".pdf"), width = 13, height = 4.2)


sub <- Y
sub$monthyear <- format(sub$Date, "%b %Y")
sub$monthyear <- factor(as.character(sub$monthyear), levels = unique(sub$monthyear))
sub$period6[sub$time %in% 1:6] <- "Aug 2015 - Jan 2016"
sub$period6[sub$time %in% 7:12] <- "Feb 2016 - Jul 2016"
sub$period6[sub$time %in% 13:18] <- "Aug 2016 - Jan 2017"
sub$period6[sub$time %in% 19:24] <- "Feb 2017 - Jul 2017"
sub$period6[sub$time %in% 25:30] <- "Aug 2017 - Jan 2018"
sub$period6[sub$time %in% 31:36] <- "Jan 2018 - Jul 2018"
sub$period6[sub$time %in% 37:42] <- "Aug 2018 - Jan 2019"
sub$period6 <- factor(sub$period6, levels = unique(sub$period6))
g7 <-  mapPlot2(data = sub, 
		variables = c("period6"),
		values = c("counts"),
		geo = geo, by.data = byDATA, 
		by.geo = byGEO, is.long = TRUE) + facet_wrap(~variable, ncol = 4) + map.theme + coord_map() + scale_fill_viridis_c("Incidents", option = "A", trans = "sqrt", breaks = c(0, 5, 10, 15)) + map.theme + coord_map()  + theme(legend.title =  element_text(size = 14), legend.position = c(0.84, 0.25)) + theme(panel.border = element_blank(), strip.background = element_blank(), strip.text.x = element_text(vjust = -0.1, size = 15, hjust = .7))+ theme(plot.margin=margin(b=-3, t=-2.5, unit="cm"))  
ggsave(plot = g7, filename = paste0("../figures/data-6month", postfix, ".pdf"), width = 14, height = 6)


g7 <- g7 + scale_fill_viridis_c("Incidents", option = "A", trans = "sqrt", breaks = c(0, 5, 10, 15), direction = -1)
ggsave(plot = g7, filename = paste0("../figures/data-6month-color2", postfix, ".pdf"), width = 14, height = 6)
