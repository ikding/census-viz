# Import libraries ----
library(rgdal)
library(maptools)
library(stringr)
library(plyr)
source('census_munging/geo_functions.R')

censusDataFolder <- "data/census/census2010/"
acsDataFolder <-  "data/census/acs/"
shapeDataFolder <- "data/census/geojson/"
state_fips <- read.csv("data/fips.csv", stringsAsFactors = F)
county_fips <- read.csv("data/county_fips.csv", stringsAsFactors = F)

# Load ACS data ----

mergeAcsData <- function(state = "DC", geolevel = "tract", year = 2010:2014, table = c("B01001", "B01003", "B19001", "B19013")){

    # Merge several acs data tables across years

    acsData <- list()

    for (y in year){

        acsData[[as.character(y)]] <- read.table(file = paste0(acsDataFolder, "/", state, "_", geolevel, "_", as.character(y-4), "-", as.character(y), "_", table[1], ".txt"), header = T, sep = "\t", stringsAsFactors = F)

        for (i in 2:length(table)){
            acsData2 <- read.table(file = paste0(acsDataFolder, "/", state, "_", geolevel, "_", as.character(y-4), "-", as.character(y), "_", table[i], ".txt"), header = T, sep = "\t", stringsAsFactors = F)
            acsData[[as.character(y)]] <- merge(acsData[[as.character(y)]], acsData2)
        }

        acsData[[as.character(y)]]$endyear <- y
    }

    acsData <- rbind.fill(acsData)

    # Pad the string with starting zeros, if necessary
    if (geolevel == "state"){
        acsData$fips = sprintf("%02s", acsData$fips)
    } else if (geolevel == "county") {
        acsData$fips = sprintf("%05s", acsData$fips)
    } else if (geolevel == "tract") {
        acsData$fips = sprintf("%011s", acsData$fips)
    }

    return(acsData)

}

# This code block will merge data from all year ranges into the a list of SpatialPolygonDataFrames; each spDF is indexed by the as.character(year).
# Had to revert to the list of SpatialPolygonDataFrames because for CA and NY, the census tract changed between 2010 and 2013, and it is not possible to use a single shapefile anymore.

mergeSpDataFrameByState <- function(state = "DC", year = 2010, table = c("B01001", "B01003", "B19001", "B19013"), geolevel = "tract", county_subset = NULL) {

    # test code for CA/SF
#     state = "CA"
#     year = 2014
#     table = c("B01001", "B01003", "B19001", "B19013"); geolevel = "tract"
#     county_subset = c(1, 75, 81, 85)

    spDF_ <- readGeoJSON(state, year, geolevel, format = "sp")
    DF_ <- mergeAcsData(state, geolevel = "tract", year)
    county_fips <- county_fips

    if (is.numeric(county_subset)){
        # Subset the spDF_ and DF_ before merging to reduce chance of merge (join) conflict
        county_subset_string = sprintf("%03s", county_subset)
        DF_ <- subset(DF_, county %in% county_subset)
        spDF_ <- spDF_[str_sub(spDF_@data$GEOID, start = 3, end = 5) %in% county_subset_string,]
    } else if (is.null(county_subset)){

    } else {
        print("county_fips should be a list of integers that sets the counties you want to keep. Ex: c(510, 13, 59, 600, 610)")
        break
    }

    spDF_all <- mergeSpDataFrame(spDF = spDF_, DF = DF_)

    return(mergeSpDataFrame(spDF = spDF_, DF = DF_))
}

# Scale down the ambition, just deal with single year of ACS data (most recent is 2014)
# NY and SF
NYMetro_tract_map <- mergeSpDataFrameByState(state = "NY", year = 2014, county_subset = c(5, 47, 61, 81, 85)) # Bronx, Brooklyn (King County), Manhattan (New York County), Queens, Staten Island (Richmond County)
SFMetro_tract_map <- mergeSpDataFrameByState(state = "CA", year = 2014, county_subset = c(1, 75, 81, 85)) # Alameda, San Francisco, Santa Clara, San Mateo

# DC Metro
DC_tract_map <- mergeSpDataFrameByState(state = "DC", year = 2014)
MD_tract_map <- mergeSpDataFrameByState(state = "MD", year = 2014, county_subset = c(31, 33))
VA_tract_map <- mergeSpDataFrameByState(state = "VA", year = 2014, county_subset = c(510, 13, 59, 600, 610))
DCMetro_tract_map <- spRbind(MD_tract_map, VA_tract_map)
DCMetro_tract_map <- spRbind(DC_tract_map, DCMetro_tract_map)

# Output to RDS for shiny viz
saveRDS(DCMetro_tract_map, file = "./census_shiny/data/DCMetro_tract_map_acs.rds")
saveRDS(NYMetro_tract_map, file = "./census_shiny/data/NYMetro_tract_map_acs.rds")
saveRDS(SFMetro_tract_map, file = "./census_shiny/data/SFMetro_tract_map_acs.rds")
