# Function to count points by 
# - a map
# - a matrix of (Long, Lat)
# - a variable to group by (optional)

countPoints <- function(data, long, lat, geo, region = "SNA_NAME", group=NULL){
	subset <- which(!is.na(data[, long]))
	data <- data[subset, ]
	if(!is.null(group)){
		group <- data[, which(colnames(data) == group)]
	}
	N <- dim(data)[1]
	coordinates(data) = as.formula(paste("~", long, "+", lat))
	proj4string(data) <- proj4string(geo)
	overlap <- over(data, geo)
	neighborhood <- as.character(overlap[[region]])
	d <- data.frame(count=1:length(neighborhood), 
					regionname=neighborhood)
	if(!is.null(group)){
		d$group = group
	}
	N0 <- dim(d)[1]
	if(N > N0)	warning(paste0("Number of points projected outside of map: ", N - N0))
	if(is.null(group)){
		counts <- aggregate(count~regionname, data = d, FUN=length, drop = FALSE)		
	}else{
		counts <- aggregate(count~regionname+group, data = d, FUN=length, drop = FALSE)
	}
	return(list(counts = counts, raw = neighborhood))
}

# Function to find minimum distance to points by 
# - a map
# - a matrix of (Long, Lat)
# - a variable to group by (optional)

mindistPoints <- function(data, long, lat, geo, region = "SNA_NAME"){
	subset <- which(!is.na(data[, long]))
	data <- data[subset, ]
	N <- dim(data)[1]

    geo2 <- fortify(geo, region = region)
    geo2 <- by(geo2, geo2$id, function(x) {Polygon(x[c('long', 'lat')])@labpt})
    centroids <- setNames(do.call("rbind.data.frame", geo2), c('long', 'lat'))
    rownames(centroids) <- names(geo2)  
    centroids$dist <- NA
    dist <- distm(centroids[, c(1:2)], data[, c(long, lat)], fun = distHaversine)/1609.344 
    if(!is.null(dim(dist))){
		centroids$dist <- apply(dist, 1, min, na.rm = TRUE)
    }else{
    	centroids$dist <- dist
    }
    centroids$regionname <- rownames(centroids)
    centroids <- centroids[, c("regionname", "dist")]
	return(centroids)
}
