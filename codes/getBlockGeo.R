#########################################################
# Read 2 Maps and construct the correspondence 
#########################################################     
library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(maptools)
library(reshape2)
library(gridExtra)
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


subset <- prop[prop$Other < 0.5, ]
max <- apply(subset[, 1:51], 1, max)
SNA <- colnames(subset)[apply(subset[, 1:51], 1, which.max)]
bg <- subset$GEOID


geo3 <- subset(geo2, GEOID %in% subset$GEOID) 
geo4 <- subset(geo2, GEOID %in% bg[which(max < 0.7)]) 
geo4 <- fortify(geo4, region = "GEOID")
geo5 <- subset(geo2, GEOID %in% bg[which(subset$Other > 0.2)]) 
geo5 <- fortify(geo5, region = "GEOID")

ggplot() + geom_polygon(data=geo5, aes(x = long, y = lat, group = group, fill = id), color = "red")+ geom_polygon(data=geo1, aes(x = long, y = lat, group = group), color = "black", fill = "gray", alpha = 0.5) + coord_map() + theme_bw() + ggtitle("Block groups with > 20% area outside of Cincinnati city")

ggplot() + geom_polygon(data=geo4, aes(x = long, y = lat, group = group, fill = id), color = "red")+ geom_polygon(data=geo1, aes(x = long, y = lat, group = group), color = "black", fill = "gray", alpha = 0.5) + coord_map() + theme_bw() + ggtitle("Block groups with < 70% area outside of the assigned neighborhood")

geo3A <- fortify(geo3, region = "GEOID")
geo3A$neighborhood <- SNA[match(geo3A$id, bg)]
geo3B <- unionSpatialPolygons(geo3, SNA[match(geo3$GEOID, bg)])
geo3C <- fortify(geo1, region = "SNA_NAME")
g1 <- ggplot(geo3A) + geom_polygon(data=geo3A, aes(x = long, y = lat, group = group, fill = neighborhood), color = "gray80")+ geom_polygon(data=geo3B, aes(x = long, y = lat, group = group), color = "gray30", fill = "white", alpha = 0) + coord_map() + theme_map() + scale_fill_discrete(guide = FALSE)
g2 <- ggplot() + geom_polygon(data=geo3C, aes(x = long, y = lat, group = group, fill = id), color = "gray30") + coord_map() + theme_map() + scale_fill_discrete(guide = FALSE) 
pdf("../figures/map-compare.pdf", width = 11, height = 4)
grid.arrange(g2, g1, ncol = 2)
dev.off()
pdf("../figures/map-compare-1.pdf", width = 6, height = 4)
print(g2)
dev.off()
pdf("../figures/map-compare-2.pdf", width = 6, height = 4)
print(g1)
dev.off()


# Get block groups in the holes
box <- rbind(crop(geo2, extent(-84.515, -84.482, 39.14089, 39.19)), 
			 crop(geo2, extent(-84.482, -84.426, 39.14089, 39.18))) 
box <- subset(box, box$GEOID %in% geo3$GEOID == FALSE)
g <- ggplot() + geom_polygon(data=geo3A, aes(x = long, y = lat, group = group, fill = neighborhood), color = "gray80", alpha = .3) + geom_polygon(data=box, aes(x = long, y = lat, group = group), fill = "gray50", color = "gray20") + coord_map() + theme_map() + scale_fill_discrete(guide = FALSE) 
pdf("../figures/map-hole.pdf", width = 5.5, height = 4)
print(g)
dev.off()

# Neighborhood map with names
library(shadowtext)
geo3D <- by(geo3C, geo3C$id, function(x) {Polygon(x[c('long', 'lat')])@labpt})
centroids <- setNames(do.call("rbind.data.frame", geo3D), c('long', 'lat'))
centroids$name <- names(geo3D) 
centroids$name[centroids$name == "Villages at Roll Hill"] <- "Villages at\n Roll Hill"
centroids$name[centroids$name == "South Cumminsville"] <-"South\nCumminsville"
centroids$name[centroids$name == "East Westwood"] <- "East\nWestwood"
centroids$name[centroids$name == "Camp Washington"] <- "Camp\nWashington"
centroids$name[centroids$name == "English Woods"] <- "English\nWoods"
centroids$name[centroids$name == "Lower Price Hill"] <- "Lower\nPrice\nHill"
centroids$name[centroids$name == "West End"] <- "West\nEnd"
centroids$name[centroids$name == "Spring Grove Village"] <- "Spring\nGrove Village"
centroids$name[centroids$name == "North Avondale - Paddock Hills"] <- "North Avondale \n- Paddock Hills"
centroids$name[centroids$name == "Walnut Hills"] <- "Walnut\nHills"
centroids$name[centroids$name == "East Walnut Hills"] <- "East\nWalnut Hills"
centroids$name[centroids$name == "Columbia Tusculum"] <- "Columbia\nTusculum"
centroids$lat[centroids$name == "Riverside"] <- centroids$lat[centroids$name == "Riverside"] - 0.01
centroids$lat[centroids$name == "East End"] <- centroids$lat[centroids$name == "East End"] + 0.005
centroids <- rbind(centroids, 
				data.frame(name = c("St Bernard", "Elmwood\nPlace", "Norwood"),
						   lat = c(39.1707, 39.1854, 39.1625),
						   long = c(-84.4954, -84.4886, -84.4539) 
				))

g <- ggplot() + geom_polygon(data=geo3C, aes(x = long, y = lat, group = group, fill = id), color = "gray20", alpha = .3) + coord_map() + theme_map() + scale_fill_discrete(guide = FALSE) + geom_shadowtext(data = centroids, aes(x = long, y = lat, label = name), check.overlap = TRUE, size = 2, color = "gray20", bg.colour='white')

pdf("../figures/map-names.pdf", width = 5.5*1.8, height = 4*1.8)
print(g)
dev.off()




geo3full <- subset(geo2, GEOID %in% as.character(c(subset$GEOID, box$GEOID))) 
table <- data.frame(SNA_NAME = SNA, bg = bg)
table <- table[order(table[, 1]), ]
geobg <- list(geo = geo3, geofull = geo3full, geodf = geo3A, table = table)
save(geobg, file = "../data/Cin_block_group.rda")