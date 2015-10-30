library(rgdal)
library(ggplot2)
library(stringr)
library(maptools)
library(leaflet)
library(RColorBrewer)

qplot_map <- function(spData){
    ggplot(data = fortify(spData)) + geom_polygon(aes(x = long, y = lat, group = id), fill = "white", color = "black") + coord_map()
}

zillowNY <- readOGR(dsn = "../../../data/zillow/ZillowNeighborhoods-NY", layer = "ZillowNeighborhoods-NY") # DC 2014 Zillow shapefile

ggplot(data = subset(fortify(zillowNY), lat < 42)) + geom_polygon(aes(x = long, y = lat, group = id), fill = "white", color = "black") + coord_map()


# zillowDCName <- data.frame(gCentroid(zillowDC, byid= TRUE))
zillowNY[zillowNY@data$REGIONID == 194318,] # good shape

zillowNY[zillowNY@data$REGIONID == 197427,] # Red Hook
zillowNY[zillowNY@data$REGIONID == 193974,] # Coney Island
zillowNY[zillowNY@data$REGIONID == 195489,] # Howard Beach
zillowNY[zillowNY@data$REGIONID == 275247,] # Rosedale
zillowNY[zillowNY@data$REGIONID == 198200,] # Steinway
zillowNY[zillowNY@data$REGIONID == 193942,] # College Point
zillowNY[zillowNY@data$REGIONID == 276060,] # Whitestone
zillowNY[zillowNY@data$REGIONID == 343224,] # Eastchester

qplot_map(zillowNY[zillowNY@data$REGIONID == 197427,]) # Red Hook
qplot_map(zillowNY[zillowNY@data$REGIONID == 193974,]) # Coney Island
qplot_map(zillowNY[zillowNY@data$REGIONID == 195489,]) # Howard Beach
qplot_map(zillowNY[zillowNY@data$REGIONID == 275247,]) # Rosedale
qplot_map(zillowNY[zillowNY@data$REGIONID == 198200,]) # Steinway
qplot_map(zillowNY[zillowNY@data$REGIONID == 193942,]) # College Point
qplot_map(zillowNY[zillowNY@data$REGIONID == 276060,]) # Whitestone
qplot_map(zillowNY[zillowNY@data$REGIONID == 343224,]) # Eastchester

palette <- brewer.pal(8, "Set1")

leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -74.0, lat = 40.7, zoom = 10) %>%
    addPolygons(data = zillowNY[zillowNY@data$REGIONID == 197427,], group = "Red Hook") %>%
    addPolygons(data = zillowNY[zillowNY@data$REGIONID == 193974,], group = "Coney Island") %>%
    addPolygons(data = zillowNY[zillowNY@data$REGIONID == 195489,], group = "Howard Beach") %>%
    addPolygons(data = zillowNY[zillowNY@data$REGIONID == 275247,], group = "Rosedale") %>%
    addPolygons(data = zillowNY[zillowNY@data$REGIONID == 198200,], group = "Steinway") %>%
    addPolygons(data = zillowNY[zillowNY@data$REGIONID == 193942,], group = "College Point") %>%
    addPolygons(data = zillowNY[zillowNY@data$REGIONID == 276060,], group = "Whitestone") %>%
    addPolygons(data = zillowNY[zillowNY@data$REGIONID == 343224,], group = "Eastchester") %>%
    addLayersControl(
        baseGroups = c("Red Hook", "Coney Island", "Howard Beach", "Rosedale", "Steinway", "College Point", "Whitestone", "Eastchester"),
        options = layersControlOptions(collapsed = FALSE)
    )

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

writeSpatialShape(x = zillowNY, fn = "../../../data/zillow/ZillowNeighborhoods-NY-subset/ZillowNeighborhoods-NY-subset")

zillowNYName <- aggregate(cbind(long, lat) ~ id, data=zillowNY, FUN=mean)

p <- ggplot()
p <- p + geom_path(data = zillowNY, aes(x = long, y = lat, group = group), color = "black", fill = NA)
p <- p + geom_text(data = zillowNYName, aes(x = long, y = lat, label = id), size = 4)
p <- p + labs(title = "New York City Neighborhoods (from Zillow.com)", x = "longitude", y = "latitude")
p <- p + coord_map() + theme_minimal()
p
