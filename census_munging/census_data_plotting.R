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
library(gpclib)
library(maptools)
gpclibPermit()
library(tools)
library(leaflet)
library(stringr)
library(magrittr)

censusDataFolder <- "../../../../data/census/census2010/"
acsDataFolder <-  "../../../../data/census/acs/"
shapeDataFolder <- "../../../../data/census/shapefiles/geojson/"
state_fips <- read.csv("../../../../data/census/shapefiles/fips.csv", stringsAsFactors = F)
county_fips <- read.csv("../../../../data/census/shapefiles/county_fips.csv", stringsAsFactors = F)


# Load census data ----

mergeCensusData <- function(state = "DC", table = c("P1", "P12"), geolevel = "tract"){
    
    # This function will read in the census data files and merge the tables into a single dataframe and fill in the fips column by concaternating state, county, tract, and blockgroup fips codes. Finally it will return the dataframe.
    
    data <- read.table(file = paste0(censusDataFolder, "/", state, "_", table[1], "_", geolevel, ".txt"), header = T, sep = "\t", stringsAsFactors = F)
    for (i in 2:length(table)){
#         print(paste0(state, "_", t, "_", geolevel, ".txt"))
        data2 <- read.table(file = paste0(censusDataFolder, "/", state, "_", table[i], "_", geolevel, ".txt"), header = T, sep = "\t", stringsAsFactors = F)
        data <- merge(data, data2)
    }
    
    # make fips column by concaternation
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

DC_tract <- mergeCensusData(state = "DC", geolevel = "tract")
DC_bg <- mergeCensusData(state = "DC", geolevel = "blockgroup")
MD_tract <- mergeCensusData(state = "MD", geolevel = "tract")
MD_bg <- mergeCensusData(state = "MD", geolevel = "blockgroup")
VA_tract <- mergeCensusData(state = "VA", geolevel = "tract")
VA_bg <- mergeCensusData(state = "VA", geolevel = "blockgroup")


# Static Visualization with ggplot2 ----

# Load shape files
list.files("../../../../data/census/shapefiles/geojson/")
# [1] "tl_2010_11_bg10.geojson"    "tl_2010_11_tract10.geojson"
# [3] "tl_2010_24_bg10.geojson"    "tl_2010_24_tract10.geojson"
# [5] "tl_2010_51_bg10.geojson"    "tl_2010_51_tract10.geojson"
# [7] "tl_2011_11_bg.geojson"      "tl_2011_11_tract.geojson"  
# [9] "tl_2011_24_bg.geojson"      "tl_2011_24_tract.geojson"  
# [11] "tl_2011_51_bg.geojson"      "tl_2011_51_tract.geojson"  
# [13] "tl_2012_11_bg.geojson"      "tl_2012_11_tract.geojson"  
# [15] "tl_2012_24_bg.geojson"      "tl_2012_24_tract.geojson"  
# [17] "tl_2012_51_bg.geojson"      "tl_2012_51_tract.geojson"  
# [19] "tl_2013_11_bg.geojson"      "tl_2013_11_tract.geojson"  
# [21] "tl_2013_24_bg.geojson"      "tl_2013_24_tract.geojson"  
# [23] "tl_2013_51_bg.geojson"      "tl_2013_51_tract.geojson"  
# [25] "tl_2014_11_bg.geojson"      "tl_2014_11_tract.geojson"  
# [27] "tl_2014_24_bg.geojson"      "tl_2014_24_tract.geojson"  
# [29] "tl_2014_51_bg.geojson"      "tl_2014_51_tract.geojson"  

# Function to read-in GeoJSON information.
# format variable: sp = SpatialPolygonDataFrame (for leaflet)
#                  ggplot2 = fortify(SpatialPolygonDataFrame), to be used in ggplot2
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

# Subsetting the shape files based on county fips
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

# Read in the GeoJSON as SpatialPolygonDataFrame
DC_tract_shape_sp <- readGeoJSON(state = "DC", year = 2010, geolevel = "tract", format = "sp")
MD_tract_shape_sp <- readGeoJSON(state = "MD", year = 2010, geolevel = "tract", format = "sp")
VA_tract_shape_sp <- readGeoJSON(state = "VA", year = 2010, geolevel = "tract", format = "sp")

# Merge the SpatialPolygonDataFrame and the dataframe based on row.names
# see 
mergeSpDataFrame <- function(spDF, DF, matchvar = "fips"){
    o <- match(rownames(as(spDF, "data.frame")), DF[,matchvar])
    DF <- DF[o,]
    row.names(DF) <- DF[,matchvar]
    return(spCbind(spDF, DF))
}

DC_tract_map <- mergeSpDataFrame(spDF = DC_tract_shape_sp, DF = DC_tract)
MD_tract_map <- mergeSpDataFrame(spDF = MD_tract_shape_sp, DF = MD_tract)
VA_tract_map <- mergeSpDataFrame(spDF = VA_tract_shape_sp, DF = VA_tract)

# Subsetting the data by county fips code
MD_tract_map <- MD_tract_map[as.integer(str_sub(rownames(as.data.frame(MD_tract_map)), start = 3, end = 5)) %in% c(31, 33),] # Montgomery and Prince George's Counties
VA_tract_map <- VA_tract_map[as.integer(str_sub(rownames(as.data.frame(VA_tract_map)), start = 3, end = 5)) %in% c(510, 13, 59, 600, 610),] # Alexandria, Arlington, Falls Church, Fairfax
DCMetro_tract_map_sp <- spRbind(MD_tract_map, VA_tract_map)
DCMetro_tract_map_sp <- spRbind(DC_tract_map, DCMetro_tract_map_sp)

# o <- match(rownames(as(DC_tract_shape_sp, "data.frame")), DC_tract$fips)
# DC_tract2 <- DC_tract[o,]
# row.names(DC_tract2) <- DC_tract2$fips
# DC_tract_shape_sp <- spCbind(DC_tract_shape_sp, DC_tract2)

# Playing with different color functions in leaflet
pal <- colorNumeric(
    palette = "YlOrRd",
    domain = DCMetro_tract_map_sp@data$P0010001
)
binpal <- colorBin("YlOrRd", DCMetro_tract_map_sp@data$P0010001, 7, pretty = FALSE)
qpal <- colorQuantile("YlOrRd", DCMetro_tract_map_sp@data$P0010001, 5)

# Let's plot some maps! 
# CartoDB.Positron works better for my purpuse because it is greyscale.
m <- leaflet(DCMetro_tract_map_sp) %>% 
    setView(lng = -77.1, lat = 38.9, zoom = 9) %>% 
#     addTiles() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(stroke = F, fillOpacity  = 0.75, color = ~pal(P0010001)) %>%
    addLegend(pal = pal, values = ~P0010001, opacity = 1)
m

# 2nd iteration of leaflet map
m <- leaflet(DCMetro_tract_map_sp) %>% 
    setView(lng = -77.1, lat = 38.9, zoom = 9) %>% 
    #     addTiles() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(stroke = F, fillOpacity  = 0.5, color = ~qpal(P0010001), 
                popup = paste(sep = "<br/>", 
                              paste0("<strong>State: </strong>", DCMetro_tract_map_sp@data$State.FIPS),
                              paste0("<strong>County: </strong>", DCMetro_tract_map_sp@data$County.FIPS),
                              paste0("<strong>Name: </strong>", DCMetro_tract_map_sp@data$Name),
                              paste0("<strong>FIPS: </strong>", DCMetro_tract_map_sp@data$fips),
                              paste0("<strong>Population: </strong>", DCMetro_tract_map_sp@data$P0010001)
                              )
#                 popup = paste0("<strong>Population: </strong>", DCMetro_tract_map_sp@data$P0010001, "<br/>", "<strong>FIPS: </strong>: ", DCMetro_tract_map_sp@data$fips)
                ) %>%
    addLegend(pal = qpal, values = ~P0010001, opacity = 1)
m



# Convert age/sex variables to population pyramids ----


pop_table <- read.csv("census_sexbyage_var_converted.csv", stringsAsFactors = F)
str(pop_table)
pop_table


getPopPyramidDF <- function(data, fips = "11001002201"){
    # This function will return a dataframe used for the plotting of population pyramids, based in the specified fips.
    
    DF <- data@data[,str_sub(names(data@data), start = 1, end = 3) == "P01"]
    DF$fips <- rownames(DF)
    
    single_fips <- DF[fips,]
    single_fips <- data.frame(t(single_fips))
    
    names(single_fips) <- "population"
    single_fips$population <- as.integer(as.character(single_fips$population))
    
    # 15-17 + 18-19 -> 15-19
    single_fips["P0120007a",] <- single_fips["P0120006",] + single_fips["P0120007",]
    single_fips["P0120031a",] <- single_fips["P0120030",] + single_fips["P0120031",]
    
    # 20 + 21 + 22-24 -> 20-24
    single_fips["P0120010a",] <- single_fips["P0120008",] + single_fips["P0120009",] + single_fips["P0120010",]
    single_fips["P0120034a",] <- single_fips["P0120032",] + single_fips["P0120033",] + single_fips["P0120034",]
    
    # 60-61 + 62-64 -> 60-64
    single_fips["P0120019a",] <- single_fips["P0120018",] + single_fips["P0120019",]
    single_fips["P0120043a",] <- single_fips["P0120042",] + single_fips["P0120043",]
    
    # 65-66 + 67-69 -> 65-69
    single_fips["P0120021a",] <- single_fips["P0120020",] + single_fips["P0120021",]
    single_fips["P0120045a",] <- single_fips["P0120044",] + single_fips["P0120045",]
    
    single_fips$var <- row.names(single_fips)
    
    single_fips <- merge(single_fips, pop_table, by.x = "var", by.y = "variable")
    single_fips <- subset(single_fips, age != "Total")
    single_fips$age <- factor(single_fips$age, levels = single_fips$age, labels = single_fips$age)
    single_fips[single_fips$gender == "Male",]$population = -1 * single_fips[single_fips$gender == "Male",]$population
    
    return(single_fips)
}

# https://rpubs.com/walkerke/pyramids_ggplot2
plotPopPyramid <- function(data){
    # This function will plot the population pyramid based on the getPopPyramidDF function above.
    p <- ggplot(data, aes(x = age, y = population, fill = gender))
    p <- p + geom_bar(subset = .(gender == "Female"), stat = "identity")
    p <- p + geom_bar(subset = .(gender == "Male"), stat = "identity")
    p <- p + coord_flip()
    p <- p + scale_fill_brewer(palette = "Set1")
    p <- p + theme_bw()
    p
}

getPopPyramidDF(data = DCMetro_tract_map_sp)

plotPopPyramid(getPopPyramidDF(data = DCMetro_tract_map_sp, fips = "51510200107"))

# Add county and state names for human comprehension
DCMetro_tract_map_sp@data <- merge(DCMetro_tract_map_sp@data, county_fips, by.x = c("State.FIPS", "County.FIPS"), by.y = c("statefp", "countyfp"), all.x = TRUE, sort = FALSE)
row.names(DCMetro_tract_map_sp@data) <- DCMetro_tract_map_sp@data$fips

# Save the RDS object for use in shiny app
saveRDS(DCMetro_tract_map_sp, file = "../census_shiny/DCMetro_tract_map_sp.rds")
