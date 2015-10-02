# Import libraries
library(rgdal)
library(plyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(ggmap)
library(sp)
library(rgeos)
# library(acs)
library(gpclib)
library(maptools)
# library(choroplethr)
# library(choroplethrZip)
gpclibPermit()
# library(UScensus2010)
# library(UScensus2010tract)
# library(UScensus2010blkgrp)
# library(UScensus2010blk)
# library(zipcode)
# library(jsonlite)
library(tools)
library(leaflet)
library(stringr)
library(magrittr)

censusDataFolder <- "../../../../data/census/census2010/"
shapeDataFolder <- "../../../../data/census/shapefiles/geojson/"
state_fips <- read.csv("../../../../data/census/shapefiles/fips.csv", stringsAsFactors = F)

# Load census data ----

mergeData <- function(state = "DC", table = c("P1", "P12"), geolevel = "tract"){
    data <- read.table(file = paste0(censusDataFolder, "/", state, "_", table[1], "_", geolevel, ".txt"), header = T, sep = "\t", stringsAsFactors = F)
    for (i in 2:length(table)){
#         print(paste0(state, "_", t, "_", geolevel, ".txt"))
        data2 <- read.table(file = paste0(censusDataFolder, "/", state, "_", table[i], "_", geolevel, ".txt"), header = T, sep = "\t", stringsAsFactors = F)
        data <- merge(data, data2)
    }
    
    fips <-  paste0(sprintf("%02s", data$State.FIPS))
    
    if (sum(is.na(data$County.FIPS)) == 0){
        fips <- paste0(fips, sprintf("%03s", data$County.FIPS)) }
    
    if (sum(is.na(data$Tract)) == 0){
        fips <- paste0(fips, sprintf("%06s", data$Tract)) }
    
    if (sum(is.na(data$Block.Group)) == 0){
        fips <- paste0(fips, sprintf("%01s", data$Block.Group)) }

    data$fips <- fips
    return(data)

}

DC_tract <- mergeData(state = "DC", geolevel = "tract")
DC_bg <- mergeData(state = "DC", geolevel = "blockgroup")
MD_tract <- mergeData(state = "MD", geolevel = "tract")
MD_bg <- mergeData(state = "MD", geolevel = "blockgroup")
VA_tract <- mergeData(state = "VA", geolevel = "tract")
VA_bg <- mergeData(state = "VA", geolevel = "blockgroup")

# DC_tract_P1 <- read.table(file = paste0(censusDataFolder, 'DC_P1_tract.txt'), header = T, sep = "\t", stringsAsFactors = F)
# str(DC_tract_P1)
# DC_tract_P12 <- read.table(file = paste0(censusDataFolder, 'DC_P12_tract.txt'), header = T, sep = "\t", stringsAsFactors = F)
# str(DC_tract_P12)
# 
# intersect(names(DC_tract_P1), names(DC_tract_P12))
# 
# str(merge(DC_tract_P1, DC_tract_P12))

# Static Visualization with ggplot2 ----

list.files("../../../../data/census/shapefiles/geojson/")
ogrInfo("../../../../data/census/shapefiles/geojson/tl_2010_24_tract10.geojson", layer="OGRGeoJSON")
test <- readOGR(dsn = "../../../../data/census/shapefiles/geojson/tl_2013_24_tract.geojson", layer = "OGRGeoJSON")
test <- fortify(test, region = "GEOID")
head(test)

pop_tract = rbind.fill(DC_tract, MD_tract, VA_tract)
pop_bg = rbind.fill(DC_bg, MD_bg, VA_bg)
str(pop_tract)

readGeoJSON <- function(state = "DC", year = 2011, geolevel = c("tract", "bg"), format = c("sp", "ggplot2")){
    statecode <- sprintf("%02s", state_fips[state_fips$twoletter == state,]$code)
    filesuffix <- if (year == 2010) '10.geojson' else '.geojson'
    regionID = if (year == 2010) "GEOID10" else "GEOID"
    
    filename <- paste0(shapeDataFolder, "tl_", as.character(year), "_", statecode, "_", geolevel, filesuffix)
    data <- readOGR(dsn = filename, layer = "OGRGeoJSON")
    
    if (format == "ggplot2"){
        data <- fortify(data, region = regionID)
    } else if (format == "sp"){
        data <- spChFIDs(data, as.character(data@data[, regionID]))
    }
    
    return(data)
    
}

DC_tract_shape <- readGeoJSON(state = "DC", year = 2010, geolevel = "tract", format = "ggplot2")
MD_tract_shape <- readGeoJSON(state = "MD", year = 2010, geolevel = "tract", format = "ggplot2")
VA_tract_shape <- readGeoJSON(state = "VA", year = 2010, geolevel = "tract", format = "ggplot2")

DC_bg_shape <- readGeoJSON(state = "DC", year = 2010, geolevel = "bg", format = "ggplot2")
MD_bg_shape <- readGeoJSON(state = "MD", year = 2010, geolevel = "bg", format = "ggplot2")
VA_bg_shape <- readGeoJSON(state = "VA", year = 2010, geolevel = "bg", format = "ggplot2")

MD_tract_shape_subset <- MD_tract_shape[as.integer(str_sub(MD_tract_shape$id, start = 3, end = 5)) %in% c(31, 33),] # Montgomery and Prince George's Counties
MD_bg_shape_subset <- MD_bg_shape[as.integer(str_sub(MD_bg_shape$id, start = 3, end = 5)) %in% c(31, 33),] # Montgomery and Prince George's Counties

VA_tract_shape_subset <- VA_tract_shape[as.integer(str_sub(VA_tract_shape$id, start = 3, end = 5)) %in% c(510, 13, 59, 600, 610),] # Alexandria, Arlington, Falls Church, Fairfax
VA_bg_shape_subset <- VA_bg_shape[as.integer(str_sub(VA_bg_shape$id, start = 3, end = 5)) %in% c(510, 13, 59, 600, 610),] # Alexandria, Arlington, Falls Church, Fairfax

DCMetro_tract_shape <- rbind.fill(DC_tract_shape, MD_tract_shape_subset, VA_tract_shape_subset)
DCMetro_bg_shape <- rbind.fill(DC_bg_shape, MD_bg_shape_subset, VA_bg_shape_subset)

DC_tract_map <- merge(DC_tract_shape, DC_tract, by.x = c("id"), by.y = c("fips"), all = TRUE, sort = FALSE)

DCMetro_tract_map <- merge(DCMetro_tract_shape, rbind.fill(DC_tract, MD_tract, VA_tract), by.x = c("id"), by.y = c("fips"), all.x = T, all.y = F, sort = FALSE)

head(MD_tract_shape)


p <- ggplot(data = DCMetro_bg_shape)
p <- p + geom_path(aes(x = long, y = lat, group = id))
p <- p + theme_minimal() + coord_map()
p

p <- ggplot()
p <- p + geom_polygon(data = plotdata, aes(x = long, y = lat, group = id, fill = color_scale), color = "grey90")
p <- p + scale_fill_brewer(palette = "YlOrRd", name = var)
p <- p + coord_map() + theme_minimal()

head(MD_tract)

head(DC_tract_map)

chomapNonZero <- function(data, var = "P0010001", cut = "number", cut_n = 5, title = ""){
    plotdata <- data[data[,var] != 0,]
    zerodata <- data[data[,var] == 0,]
    
    if (cut == "number"){
        plotdata$color_scale <- cut_number(plotdata[, var], cut_n)
    } else if (cut == "interval"){
        plotdata$color_scale <- cut_interval(plotdata[, var], cut_n)
    } else {
        print("cut argument only takes two choices: number or interval")
        break
    }
    
    p <- ggplot()
    p <- p + geom_polygon(data = zerodata, aes(x = long, y = lat, group = id), fill = NA, color = "grey90")
    p <- p + geom_polygon(data = plotdata, aes(x = long, y = lat, group = id, fill = color_scale), color = "grey90")
    p <- p + scale_fill_brewer(palette = "YlOrRd", name = var)
    
    if (title != ""){
        ggtitle(title)
    }
    
    p <- p + coord_map() + theme_minimal()
    p <<- p
    return(p)
}

chomapNonZero(data = DC_tract_map, title = "Total Population by Census Tracts - Washington DC (2010 Census)")

chomapNonZero(data = DCMetro_tract_map, title = "Total Population by Census Tracts - Washington DC (2010 Census)")



# Interactive Visualization with leaflet ----

m <- leaflet(DCMetro_tract_map) %>% 
    setView(lng = -77.1, lat = 38.9, zoom = 9) %>% 
    addTiles() %>%
    addPolygons(lng = ~long, lat = ~lat, weight = 3, color = "#444444", stroke = T, fill = F)
m

DC_tract_shape <- readOGR(dsn = paste0(shapeDataFolder, "/tl_2010_11_tract10.geojson"), layer = "OGRGeoJSON") 

m <- leaflet(DC_tract_shape) %>% 
    setView(lng = -77.1, lat = 38.9, zoom = 9) %>% 
    addTiles() %>%
    addPolygons()
m

# DCMetro_tract_map <- merge(DCMetro_tract_shape, rbind.fill(DC_tract, MD_tract, VA_tract), by.x = c("id"), by.y = c("fips"), all.x = T, all.y = F, sort = FALSE)

DC_tract_shape_sp <- readGeoJSON(state = "DC", year = 2010, geolevel = "tract", format = "sp")
MD_tract_shape_sp <- readGeoJSON(state = "MD", year = 2010, geolevel = "tract", format = "sp")
VA_tract_shape_sp <- readGeoJSON(state = "VA", year = 2010, geolevel = "tract", format = "sp")

mergeSpDataFrame <- function(spDF, DF, matchvar = "fips"){
    o <- match(rownames(as(spDF, "data.frame")), DF[,matchvar])
    DF <- DF[o,]
    row.names(DF) <- DF[,matchvar]
    return(spCbind(spDF, DF))
}

DC_tract_map <- mergeSpDataFrame(spDF = DC_tract_shape_sp, DF = DC_tract)
MD_tract_map <- mergeSpDataFrame(spDF = MD_tract_shape_sp, DF = MD_tract)
VA_tract_map <- mergeSpDataFrame(spDF = VA_tract_shape_sp, DF = VA_tract)


MD_tract_map <- MD_tract_map[as.integer(str_sub(rownames(as.data.frame(MD_tract_map)), start = 3, end = 5)) %in% c(31, 33),] # Montgomery and Prince George's Counties
VA_tract_map <- VA_tract_map[as.integer(str_sub(rownames(as.data.frame(VA_tract_map)), start = 3, end = 5)) %in% c(510, 13, 59, 600, 610),] # Alexandria, Arlington, Falls Church, Fairfax
DCMetro_tract_map_sp <- spRbind(MD_tract_map, VA_tract_map)
DCMetro_tract_map_sp <- spRbind(DC_tract_map, DCMetro_tract_map_sp)

# o <- match(rownames(as(DC_tract_shape_sp, "data.frame")), DC_tract$fips)
# DC_tract2 <- DC_tract[o,]
# row.names(DC_tract2) <- DC_tract2$fips
# DC_tract_shape_sp <- spCbind(DC_tract_shape_sp, DC_tract2)

pal <- colorNumeric(
    palette = "YlOrRd",
    domain = DCMetro_tract_map_sp@data$P0010001
)

m <- leaflet(DCMetro_tract_map_sp) %>% 
    setView(lng = -77.1, lat = 38.9, zoom = 9) %>% 
#     addTiles() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(stroke = F, fillOpacity  = 0.75, color = ~pal(P0010001))
m

binpal <- colorBin("YlOrRd", DCMetro_tract_map_sp@data$P0010001, 7, pretty = FALSE)

m <- leaflet(DCMetro_tract_map_sp) %>% 
    setView(lng = -77.1, lat = 38.9, zoom = 9) %>% 
    #     addTiles() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(stroke = F, fillOpacity  = 0.75, color = ~binpal(P0010001), popup = paste0("<strong>Population: </strong>", DCMetro_tract_map_sp@data$P0010001))
m

names(DCMetro_tract_map_sp@data)
sort(DCMetro_tract_map_sp@data$P0010001)
?