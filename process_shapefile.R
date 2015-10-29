library(rgdal)
library(ggplot2)
library(stringr)
library(maptools)

qplot_map <- function(spData){
    ggplot(data = fortify(spData)) + geom_polygon(aes(x = long, y = lat, group = id), fill = "white", color = "black") + coord_map()
}

zillowNY <- readOGR(dsn = "../../../data/zillow/ZillowNeighborhoods-NY", layer = "ZillowNeighborhoods-NY") # DC 2014 Zillow shapefile

ggplot(data = subset(fortify(zillowNY), lat < 42)) + geom_polygon(aes(x = long, y = lat, group = id), fill = "white", color = "black") + coord_map()


# zillowDCName <- data.frame(gCentroid(zillowDC, byid= TRUE))
zillowNY[zillowNY@data$REGIONID == 197427,] # corrupted shape
zillowNY[zillowNY@data$REGIONID == 194318,] # good shape

# Ill-formed polygons: 
# 197427 Red Hook
# 193974 Coney Island
# 195489 Howard Beach
# 275247 Rosedale
# 198200 Steinway
# 193942 College Point
# 276060 Whitestone
# 343224 Eastchester
zillowNY <- zillowNY[!(zillowNY@data$REGIONID %in% c(197427, 193974, 195489, 275247, 198200, 193942, 276060, 343224)),]

zillowNY <- zillowNY[str_sub(zillowNY@data$CITY, 1, 13) == "New York City",]
qplot_map(zillowNY)

writeSpatialShape(x = zillowNY, fn = "ZillowNeighborhoods-NY-subset")

zillowNY2 <- readOGR(dsn = "../../../data/zillow/ZillowNeighborhoods-NY-subset/", layer = "ZillowNeighborhoods-NY-subset") # DC 2014 Zillow shapefile

identical(zillowNY, zillowNY2)

zillowNYName <- aggregate(cbind(long, lat) ~ id, data=zillowNY, FUN=mean)

p <- ggplot()
p <- p + geom_path(data = zillowNY, aes(x = long, y = lat, group = group), color = "black", fill = NA)
p <- p + geom_text(data = zillowNYName, aes(x = long, y = lat, label = id), size = 4)
p <- p + labs(title = "New York City Neighborhoods (from Zillow.com)", x = "longitude", y = "latitude")
p <- p + coord_map() + theme_minimal()
p
