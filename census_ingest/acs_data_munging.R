# Import libraries ----
library(rgdal)
library(maptools)
library(stringr)
library(plyr)

censusDataFolder <- "../../../../data/census/census2010/"
acsDataFolder <-  "../../../../data/census/acs/"
shapeDataFolder <- "../../../../data/census/shapefiles/geojson/"
state_fips <- read.csv("../../../../data/census/shapefiles/fips.csv", stringsAsFactors = F)


# Load ACS data ----

mergeAcsData <- function(state = "DC", geolevel = "tract", year = 2010:2013, table = c("B01001", "B01003", "B19001", "B19013")){
    
    acsData <- list()
    
    for (y in year){
        
        acsData[[as.character(y)]] <- read.table(file = paste0(acsDataFolder, "/", state, "_", geolevel, "_", as.character(y-4), "-", as.character(y), "_", table[1], ".txt"), header = T, sep = "\t", stringsAsFactors = F)
        
        for (i in 2:length(table)){
            acsData2 <- read.table(file = paste0(acsDataFolder, "/", state, "_", geolevel, "_", as.character(y-4), "-", as.character(y), "_", table[i], ".txt"), header = T, sep = "\t", stringsAsFactors = F)
            acsData[[as.character(y)]] <- merge(acsData[[as.character(y)]], acsData2)
        }
        
        acsData[[as.character(y)]]$endyear <- y
    }
    
    return(rbind.fill(acsData))
    
}

# Read shape files

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

mergeSpDataFrame <- function(spDF, DF, matchvar = "fips"){
    
    # Test to see if I can have multi year data in the same SpatialPolygonDataFrame
    #     spDF <- readGeoJSON(state = "MD", year = 2010, geolevel = "tract", format = "sp")
    #     DF <- mergeAcsData(state = "MD", geolevel = "tract", year = 2011)
    #     matchvar = "fips"
    
    o <- match(rownames(as(spDF, "data.frame")), DF[,matchvar])
    DF <- DF[o,]
    row.names(DF) <- DF[,matchvar]
    #     spDF <- spCbind(spDF, DF)
    return(spCbind(spDF, DF))
}

# This code block will merge data from all year ranges into the a list of SpatialPolygonDataFrames; each spDF is indexed by the as.character(year).
# Had to revert to the list of SpatialPolygonDataFrames because for CA and NY, the census tract changed between 2010 and 2013, and it is not possible to use a single shapefile anymore.

mergeAcsSpData <- function(state = "DC", year = 2010, geolevel = "tract"){
    spDF_ <- readGeoJSON(state, year, geolevel, format = "sp")
    DF_ <- mergeAcsData(state, geolevel = "tract", year)
    return(mergeSpDataFrame(spDF = spDF_, DF = DF_))
}


mergeMultiYearAcsSpData <- function(state = "DC", year.range = 2010:2013, geolevel = "tract", county_subset = NULL){
    
    spDF_list <- list()
    
    for (y in year.range){
        spDF <- mergeAcsSpData(state = state, year = y)
        if (is.numeric(county_subset)){
            spDF <- spDF[as.integer(str_sub(rownames(as.data.frame(spDF)), start = 3, end = 5)) %in% county_subset,]
        }
        spDF_list[[as.character(y)]] <- spDF
        print(paste("Year", y, "... Done"))
        
    }
    
    return(spDF_list)
}

# NY and SF
NYMetro_tract_map <- mergeMultiYearAcsSpData(state = "NY", county_subset = c(5, 47, 61, 81, 85))
SFMetro_tract_map <- mergeMultiYearAcsSpData(state = "CA", county_subset = c(1, 75, 81, 85))

# DC Metro requires special case because the need to combine data from multiple states
DCMetro_tract_map <- list()

for (y in 2010:2013){
    #     y = 2011
    DC_tract_map <- mergeAcsSpData(state = "DC", year = y)
    MD_tract_map <- mergeAcsSpData(state = "MD", year = y)
    VA_tract_map <- mergeAcsSpData(state = "VA", year = y)
    
    MD_tract_map <- MD_tract_map[as.integer(str_sub(rownames(as.data.frame(MD_tract_map)), start = 3, end = 5)) %in% c(31, 33),] # Montgomery and Prince George's Counties
    VA_tract_map <- VA_tract_map[as.integer(str_sub(rownames(as.data.frame(VA_tract_map)), start = 3, end = 5)) %in% c(510, 13, 59, 600, 610),] # Alexandria, Arlington, Falls Church, Fairfax
    
    DCMetro_tract_map_singleyear <- spRbind(MD_tract_map, VA_tract_map)
    DCMetro_tract_map_singleyear <- spRbind(DC_tract_map, DCMetro_tract_map_singleyear)
    
    DCMetro_tract_map[[as.character(y)]] <- DCMetro_tract_map_singleyear
    
    print(paste("Year", y, "... Done"))
    rm(DC_tract_map, MD_tract_map, VA_tract_map, DCMetro_tract_map_singleyear)
}

# A better version of the function to merge data from all year ranges into a single SpatialPolygonDataFrame; rename the variables with year suffixes (eg: var.2010)
# This version no longer works because for CA and NY, the census tract changed between 2010 and 2013, and it is not possible to use a single shapefile anymore.

mergeAllYearsSpaceTimeDF <- function(state = "DC", shapeyear = 2010, year = c(2010:2013), geolevel = "tract", county_subset = NULL){
    
    # Test on NY
#     state = "NY"
#     shapeyear = 2010
#     year = c(2010:2013)
#     geolevel = "tract"
#     county_subset = c(5, 47, 61, 81, 85)
    # rm(state, shapeyear, year, geolevel, county_subset)
    
    spDF_ <- readGeoJSON(state, year = shapeyear, geolevel, format = "sp")
    
    if (is.numeric(county_subset)){
        spDF_ <- spDF_[spDF_@data$COUNTYFP10 %in% sprintf("%03d", county_subset),]
    }
    
    for (y in year){
        DF_ <- mergeAcsData(state, geolevel = geolevel, year = y)
        
        if (is.numeric(county_subset)){
            DF_ <- subset(DF_, county %in% county_subset)
        }
        
        names(DF_)[-(1:5)] <- paste0(names(DF_)[-(1:5)], ".", y)
        spDF_ <- mergeSpDataFrame(spDF_, DF_)
    }
    
    return(spDF_)
}

DC_tract_map <- mergeAllYearsSpaceTimeDF(state = "DC")
MD_tract_map <- mergeAllYearsSpaceTimeDF(state = "MD")
VA_tract_map <- mergeAllYearsSpaceTimeDF(state = "VA")
NY_tract_map <- mergeAllYearsSpaceTimeDF(state = "NY", shapeyear = 2013, year = c(2013))
# CA_tract_map <- mergeAllYearsSpaceTimeDF(state = "CA", shapeyear = 2011, year = c(2011)) # Encounter error in binding DF to spDF, not sure why - only for CA.

MD_tract_map <- MD_tract_map[as.integer(str_sub(rownames(as.data.frame(MD_tract_map)), start = 3, end = 5)) %in% c(31, 33),] # Montgomery and Prince George's Counties
VA_tract_map <- VA_tract_map[as.integer(str_sub(rownames(as.data.frame(VA_tract_map)), start = 3, end = 5)) %in% c(510, 13, 59, 600, 610),] # Alexandria, Arlington, Falls Church, Fairfax
NY_tract_map <- NY_tract_map[as.integer(str_sub(rownames(as.data.frame(NY_tract_map)), start = 3, end = 5)) %in% c(5, 47, 61, 81, 85),] # Bronx, Brooklyn (King County), Manhattan (New York County), Queens, Staten Island (Richmond County)
# CA_tract_map <- CA_tract_map[as.integer(str_sub(rownames(as.data.frame(CA_tract_map)), start = 3, end = 5)) %in% c(1, 75, 81, 85),] # Alameda, San Francisco, Santa Clara, San Mateo

DCMetro_tract_map_acs <- spRbind(MD_tract_map, VA_tract_map)
DCMetro_tract_map_acs <- spRbind(DC_tract_map, DCMetro_tract_map_acs)

saveRDS(DCMetro_tract_map_acs, file = "../census_shiny/data/DCMetro_tract_map_acs.rds")
saveRDS(NY_tract_map, file = "../census_shiny/data/NYMetro_tract_map_acs.rds")
