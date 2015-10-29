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



qplot_map(zillowNY[zillowNY@data$REGIONID == 197427,])
qplot_map(zillowNY[zillowNY@data$REGIONID == 194318,])

zillowNY <- zillowNY[zillowNY@data$REGIONID != 197427,]

nchar("New York City")
str_sub(levels(zillowNY@data$CITY), 1, 13)



zillowNY <- zillowNY[str_sub(zillowNY@data$CITY, 1, 13) == "New York City",]
qplot_map(zillowNY)








zillowNY <- fortify(zillowNY, region = "NAME")

zillowNYName <- aggregate(cbind(long, lat) ~ id, data=zillowNY, FUN=mean)

p <- ggplot()
p <- p + geom_path(data = zillowNY, aes(x = long, y = lat, group = group), color = "black", fill = NA)
p <- p + geom_text(data = zillowNYName, aes(x = long, y = lat, label = id), size = 4)
p <- p + labs(title = "Washington DC Neighborhoods (from Zillow.com)", x = "longitude", y = "latitude")
p <- p + coord_map() + theme_minimal()
p
