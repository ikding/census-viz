# Import libraries
library(rgdal)
library(maptools)
library(stringr)

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


# Read in the GeoJSON as SpatialPolygonDataFrame ----

readGeoJSON <- function(state = "DC", year = 2011, geolevel = c("tract", "bg"), format = c("sp", "ggplot2")){
    # Function to read-in GeoJSON information.
    # format variable: sp = SpatialPolygonDataFrame (for leaflet)
    #                  ggplot2 = fortify(SpatialPolygonDataFrame), to be used in ggplot2
    
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


# Merge the SpatialPolygonDataFrame and the dataframe based on row.names ----
mergeSpDataFrame <- function(spDF, DF, matchvar = "fips"){
    o <- match(rownames(as(spDF, "data.frame")), DF[,matchvar])
    DF <- DF[o,]
    row.names(DF) <- DF[,matchvar]
    return(spCbind(spDF, DF))
}

# Wrapper function to tie all three steps together

mergeSpDataFrameByState <- function(state = "DC", table = c("P1", "P12"), geolevel = "tract", year = 2010, matchvar = "fips", county_subset = NULL){
    spDF_ = readGeoJSON(state = state, year = year, geolevel = geolevel, format = "sp")
    DF_ = mergeCensusData(state = state, geolevel = geolevel)
    county_fips <- read.csv("../../../../data/census/shapefiles/county_fips.csv", stringsAsFactors = F)
    
    if (is.null(county_subset)){
        
        spDF_all <- mergeSpDataFrame(spDF = spDF_, DF = DF_)
        # Add county and state names for human comprehension
        spDF_all@data <- merge(spDF_all@data, county_fips, by.x = c("State.FIPS", "County.FIPS"), by.y = c("statefp", "countyfp"), all.x = TRUE, sort = FALSE)
        row.names(spDF_all@data) <- spDF_all@data$fips
        return(spDF_all)
        
    } else if (is.numeric(county_subset)){
        
        spDF_all <- mergeSpDataFrame(spDF = spDF_, DF = DF_)
        spDF_subset <- spDF_all[spDF_all@data$County.FIPS %in% county_subset,]
        # Add county and state names for human comprehension
        spDF_subset@data <- merge(spDF_subset@data, county_fips, by.x = c("State.FIPS", "County.FIPS"), by.y = c("statefp", "countyfp"), all.x = TRUE, sort = FALSE)
        row.names(spDF_subset@data) <- spDF_subset@data$fips
        return(spDF_subset)
        
    } else {
        print("county_fips should be a list of integers that sets the counties you want to keep. Ex: c(510, 13, 59, 600, 610)")
    }
}

# Merge census and shape data ----

DC_tract_map <- mergeSpDataFrameByState(state = "DC")
MD_tract_map <- mergeSpDataFrameByState(state = "MD", county_subset = c(31, 33))
VA_tract_map <- mergeSpDataFrameByState(state = "VA", county_subset = c(510, 13, 59, 600, 610))
NY_tract_map <- mergeSpDataFrameByState(state = "NY", county_subset = c(5, 47, 61, 81, 85)) # Bronx, Brooklyn (King County), Manhattan (New York County), Queens, Staten Island (Richmond County)
CA_tract_map <- mergeSpDataFrameByState(state = "CA", county_subset = c(1, 75, 81, 85)) # Alameda, San Francisco, Santa Clara, San Mateo

# Subsetting the data by county fips code
DCMetro_tract_map <- spRbind(MD_tract_map, VA_tract_map)
DCMetro_tract_map <- spRbind(DC_tract_map, DCMetro_tract_map)
NYMetro_tract_map <- NY_tract_map
SFMetro_tract_map <- CA_tract_map

# Save the RDS object for use in shiny app
saveRDS(DCMetro_tract_map, file = "../census_shiny/data/DCMetro_tract_map_census.rds")
saveRDS(NYMetro_tract_map, file = "../census_shiny/data/NYMetro_tract_map_census.rds")
saveRDS(SFMetro_tract_map, file = "../census_shiny/data/SFMetro_tract_map_census.rds")
