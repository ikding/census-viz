# Import libraries ----
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

DC_tract <- mergeAcsData(state = "DC")
MD_tract <- mergeAcsData(state = "MD")
VA_tract <- mergeAcsData(state = "VA")
NY_tract <- mergeAcsData(state = "NY")
CA_tract <- mergeAcsData(state = "CA")


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
# This effort is ditched in favor of the single spDF, below, because the new approach renders faster in app.

# mergeSpaceTimeDF <- function(state = "DC", year = 2010, geolevel = "tract"){
#     spDF_ <- readGeoJSON(state, year, geolevel, format = "sp")
#     DF_ <- mergeAcsData(state, geolevel = "tract", year)
#     return(mergeSpDataFrame(spDF = spDF_, DF = DF_))
# }

# DC_tract_shape <- readGeoJSON(state = "DC", year = 2010, geolevel = "tract", format = "sp")
# MD_tract_shape <- readGeoJSON(state = "MD", year = 2010, geolevel = "tract", format = "sp")
# VA_tract_shape <- readGeoJSON(state = "VA", year = 2010, geolevel = "tract", format = "sp")
# 
# DC_tract_map <- mergeSpDataFrame(spDF = DC_tract_shape, DF = DC_tract)
# MD_tract_map <- mergeSpDataFrame(spDF = MD_tract_shape, DF = MD_tract)
# VA_tract_map <- mergeSpDataFrame(spDF = VA_tract_shape, DF = VA_tract)
# 
# DCMetroSpaceTimeDF <- list()
# 
# for (y in 2010:2013){
#     #     y = 2011
#     DC_tract_map <- mergeSpaceTimeDF(state = "DC", year = y)
#     MD_tract_map <- mergeSpaceTimeDF(state = "MD", year = y)
#     VA_tract_map <- mergeSpaceTimeDF(state = "VA", year = y)
#     
#     MD_tract_map <- MD_tract_map[as.integer(str_sub(rownames(as.data.frame(MD_tract_map)), start = 3, end = 5)) %in% c(31, 33),] # Montgomery and Prince George's Counties
#     VA_tract_map <- VA_tract_map[as.integer(str_sub(rownames(as.data.frame(VA_tract_map)), start = 3, end = 5)) %in% c(510, 13, 59, 600, 610),] # Alexandria, Arlington, Falls Church, Fairfax
#     
#     DCMetro_tract_map_sp <- spRbind(MD_tract_map, VA_tract_map)
#     DCMetro_tract_map_sp <- spRbind(DC_tract_map, DCMetro_tract_map_sp)
#     
#     DCMetroSpaceTimeDF[[as.character(y)]] <- DCMetro_tract_map_sp
#     
#     print(paste("Year", y, "... Done"))
#     rm(DC_tract_map, MD_tract_map, VA_tract_map, DCMetro_tract_map_sp)
# }
# saveRDS(DCMetroSpaceTimeDF, file = "../census_shiny/DCMetroSpaceTimeDF.rds")

# A better version of the function to merge data from all year ranges into a single SpatialPolygonDataFrame; rename the variables with year suffixes (eg: var.2010)
mergeAllYearsSpaceTimeDF <- function(state = "DC", shapeyear = 2010, year = c(2010:2013), geolevel = "tract"){
    
    spDF_ <- readGeoJSON(state, year = shapeyear, geolevel, format = "sp")
    for (y in year){
        DF_ <- mergeAcsData(state, geolevel = geolevel, year = y)
        names(DF_)[-(1:5)] <- paste0(names(DF_)[-(1:5)], ".", y)
        spDF_ <- mergeSpDataFrame(spDF_, DF_)
    }
    
    return(spDF_)
}

DC_tract_map <- mergeAllYearsSpaceTimeDF(state = "DC")
MD_tract_map <- mergeAllYearsSpaceTimeDF(state = "MD")
VA_tract_map <- mergeAllYearsSpaceTimeDF(state = "VA")
NY_tract_map <- mergeAllYearsSpaceTimeDF(state = "NY")
CA_tract_map <- mergeAllYearsSpaceTimeDF(state = "CA")

MD_tract_map <- MD_tract_map[as.integer(str_sub(rownames(as.data.frame(MD_tract_map)), start = 3, end = 5)) %in% c(31, 33),] # Montgomery and Prince George's Counties
VA_tract_map <- VA_tract_map[as.integer(str_sub(rownames(as.data.frame(VA_tract_map)), start = 3, end = 5)) %in% c(510, 13, 59, 600, 610),] # Alexandria, Arlington, Falls Church, Fairfax
NY_tract_map <- NY_tract_map[as.integer(str_sub(rownames(as.data.frame(NY_tract_map)), start = 3, end = 5)) %in% c(5, 47, 61, 81, 85),] # Bronx, Brooklyn (King County), Manhattan (New York County), Queens, Staten Island (Richmond County)
CA_tract_map <- CA_tract_map[as.integer(str_sub(rownames(as.data.frame(CA_tract_map)), start = 3, end = 5)) %in% c(1, 75, 81, 85),] # Alameda, San Francisco, Santa Clara, San Mateo


DCMetro_tract_map_sp_acs <- spRbind(MD_tract_map, VA_tract_map)
DCMetro_tract_map_sp_acs <- spRbind(DC_tract_map, DCMetro_tract_map_sp_acs)

str(DCMetro_tract_map_sp_acs@data)
names(DCMetro_tract_map_sp_acs@data)

saveRDS(DCMetro_tract_map_sp_acs, file = "../census_shiny/DCMetro_tract_map_sp_acs.rds")

# Leaflet visualization of single year ----

DCMetro_tract_map_sp <- DCMetroSpaceTimeDF[['2010']]
names(DCMetro_tract_map_sp@data)

qpal <- colorQuantile("YlOrRd", DCMetro_tract_map_sp@data$B19013_001, 5)

m <- leaflet(DCMetro_tract_map_sp) %>% 
    setView(lng = -77.1, lat = 38.9, zoom = 9) %>% 
    #     addTiles() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(stroke = T, weight = 3, color = "white", fillOpacity  = 0.5, fillColor = ~qpal(B19013_001), 
                popup = paste(sep = "<br/>", 
                              paste0("<strong>State: </strong>", DCMetro_tract_map_sp@data$state),
                              paste0("<strong>County: </strong>", DCMetro_tract_map_sp@data$county),
                              paste0("<strong>Name: </strong>", DCMetro_tract_map_sp@data$NAME),
                              paste0("<strong>FIPS: </strong>", DCMetro_tract_map_sp@data$fips),
                              paste0("<strong>Median Household Income: </strong>", DCMetro_tract_map_sp@data$B19013_001, " +/- ", round(DCMetro_tract_map_sp@data$B19013_001.err))
                )
    ) %>%
    addLegend(pal = qpal, values = ~B19013_001, opacity = 1)
m


# Convert age/sex variables to histograms ----

sumsqrt <- function(x){
    return(sqrt(sum(x^2)))
}

pop_table <- read.csv("acs_sexbyage_var_converted.csv", stringsAsFactors = F)
str(pop_table)
pop_table

names(DCMetro_tract_map_sp@data)
DCMetro_tract_pop <- DCMetro_tract_map_sp@data[,str_sub(names(DCMetro_tract_map_sp@data), start = 1, end = 6) == "B01001"]
DCMetro_tract_pop$fips <- rownames(DCMetro_tract_pop)
head(DCMetro_tract_pop)

DCMetro_tract_pop_t <- t(DCMetro_tract_pop)
DCMetro_tract_pop_t[,1]

nrow(DCMetro_tract_pop)



DCMetro_tract_pop_single <- DCMetro_tract_pop["11001002201",]
str(DCMetro_tract_pop_single)
single_fips <- data.frame(t(DCMetro_tract_pop_single))
names(single_fips) <- "population"
single_fips$population <- as.numeric(as.character(single_fips$population))
single_fips



# 15-17 + 18-19 -> 15-19
single_fips["B01001_007a",] <- single_fips["B01001_006",] + single_fips["B01001_007",]
single_fips["B01001_031a",] <- single_fips["B01001_030",] + single_fips["B01001_031",]
single_fips["B01001_007a.err",] <- sumsqrt(c(single_fips["B01001_006.err",], single_fips["B01001_007.err",]))
single_fips["B01001_031a.err",] <- sumsqrt(c(single_fips["B01001_030.err",], single_fips["B01001_031.err",]))

# 20 + 21 + 22-24 -> 20-24
single_fips["B01001_010a",] <- single_fips["B01001_008",] + single_fips["B01001_009",] + single_fips["B01001_010",]
single_fips["B01001_034a",] <- single_fips["B01001_032",] + single_fips["B01001_033",] + single_fips["B01001_034",]
single_fips["B01001_010a.err",] <- sumsqrt(c(single_fips["B01001_008.err",], single_fips["B01001_009.err",], single_fips["B01001_010.err",]))
single_fips["B01001_034a.err",] <- sumsqrt(c(single_fips["B01001_032.err",], single_fips["B01001_033.err",], single_fips["B01001_034.err",]))

# 60-61 + 62-64 -> 60-64
single_fips["B01001_019a",] <- single_fips["B01001_018",] + single_fips["B01001_019",]
single_fips["B01001_043a",] <- single_fips["B01001_042",] + single_fips["B01001_043",]
single_fips["B01001_019a.err",] <- sumsqrt(c(single_fips["B01001_018.err",], single_fips["B01001_019.err",]))
single_fips["B01001_043a.err",] <- sumsqrt(c(single_fips["B01001_042.err",], single_fips["B01001_043.err",]))

# 65-66 + 67-69 -> 65-69
single_fips["B01001_021a",] <- single_fips["B01001_020",] + single_fips["B01001_021",]
single_fips["B01001_045a",] <- single_fips["B01001_044",] + single_fips["B01001_045",]
single_fips["B01001_021a.err",] <- sumsqrt(c(single_fips["B01001_020.err",], single_fips["B01001_021.err",]))
single_fips["B01001_045a.err",] <- sumsqrt(c(single_fips["B01001_044.err",], single_fips["B01001_045.err",]))

single_fips$var <- row.names(single_fips)

single_fips <- merge(single_fips, pop_table, by.x = "var", by.y = "variable")
single_fips <- subset(single_fips, age != "Total")
single_fips$age <- factor(single_fips$age, levels = single_fips$age, labels = single_fips$age)
single_fips[single_fips$gender == "Male",]$population = -1 * single_fips[single_fips$gender == "Male",]$population

single_fips <- cbind(subset(single_fips, measure == "estimate", select = c("var", "population", "gender", "age")), subset(single_fips, measure == "error", select = c("population")))
names(single_fips) <- c("var", "population", "gender", "age", "error")
str(single_fips)

single_fips$age

p <- ggplot(single_fips, aes(x = age, y = population, fill = gender))
p <- p + geom_bar(subset = .(gender == "Female"), stat = "identity")
p <- p + geom_bar(subset = .(gender == "Male"), stat = "identity")
p <- p + geom_errorbar(aes(ymax = subset(single_fips)$population + subset(single_fips)$error, ymin = subset(single_fips)$population - subset(single_fips)$error))
p <- p + coord_flip()
p <- p + scale_fill_brewer(palette = "Set1")
p <- p + theme_bw()
p

getPopPyramidDF <- function(data, fips = "11001002201"){
    
    DF <- data@data[,str_sub(names(data@data), start = 1, end = 6) == "B01001"]
    DF$fips <- rownames(DF)
    
    single_fips <- DF[fips,]
    single_fips <- data.frame(t(single_fips))
    
    names(single_fips) <- "population"
    single_fips$population <- as.numeric(as.character(single_fips$population))
    
    # 15-17 + 18-19 -> 15-19
    single_fips["B01001_007a",] <- single_fips["B01001_006",] + single_fips["B01001_007",]
    single_fips["B01001_031a",] <- single_fips["B01001_030",] + single_fips["B01001_031",]
    single_fips["B01001_007a.err",] <- sumsqrt(c(single_fips["B01001_006.err",], single_fips["B01001_007.err",]))
    single_fips["B01001_031a.err",] <- sumsqrt(c(single_fips["B01001_030.err",], single_fips["B01001_031.err",]))
    
    # 20 + 21 + 22-24 -> 20-24
    single_fips["B01001_010a",] <- single_fips["B01001_008",] + single_fips["B01001_009",] + single_fips["B01001_010",]
    single_fips["B01001_034a",] <- single_fips["B01001_032",] + single_fips["B01001_033",] + single_fips["B01001_034",]
    single_fips["B01001_010a.err",] <- sumsqrt(c(single_fips["B01001_008.err",], single_fips["B01001_009.err",], single_fips["B01001_010.err",]))
    single_fips["B01001_034a.err",] <- sumsqrt(c(single_fips["B01001_032.err",], single_fips["B01001_033.err",], single_fips["B01001_034.err",]))
    
    # 60-61 + 62-64 -> 60-64
    single_fips["B01001_019a",] <- single_fips["B01001_018",] + single_fips["B01001_019",]
    single_fips["B01001_043a",] <- single_fips["B01001_042",] + single_fips["B01001_043",]
    single_fips["B01001_019a.err",] <- sumsqrt(c(single_fips["B01001_018.err",], single_fips["B01001_019.err",]))
    single_fips["B01001_043a.err",] <- sumsqrt(c(single_fips["B01001_042.err",], single_fips["B01001_043.err",]))
    
    # 65-66 + 67-69 -> 65-69
    single_fips["B01001_021a",] <- single_fips["B01001_020",] + single_fips["B01001_021",]
    single_fips["B01001_045a",] <- single_fips["B01001_044",] + single_fips["B01001_045",]
    single_fips["B01001_021a.err",] <- sumsqrt(c(single_fips["B01001_020.err",], single_fips["B01001_021.err",]))
    single_fips["B01001_045a.err",] <- sumsqrt(c(single_fips["B01001_044.err",], single_fips["B01001_045.err",]))
    
    single_fips$var <- row.names(single_fips)
    
    single_fips <- merge(single_fips, pop_table, by.x = "var", by.y = "variable")
    single_fips <- subset(single_fips, age != "Total")
    single_fips$age <- factor(single_fips$age, levels = single_fips$age, labels = single_fips$age)
    single_fips[single_fips$gender == "Male",]$population = -1 * single_fips[single_fips$gender == "Male",]$population
    
    single_fips <- cbind(subset(single_fips, measure == "estimate", select = c("var", "population", "gender", "age")), subset(single_fips, measure == "error", select = c("population")))
    names(single_fips) <- c("var", "population", "gender", "age", "error")
    
    return(single_fips)
}

# https://rpubs.com/walkerke/pyramids_ggplot2
plotPopPyramid <- function(input_DF){
    p <- ggplot(data = input_DF, aes(x = age, y = population, fill = gender))
    p <- p + geom_bar(subset = .(gender == "Female"), stat = "identity")
    p <- p + geom_bar(subset = .(gender == "Male"), stat = "identity")
    p <- p + geom_errorbar(aes(ymax = population + error, ymin = population - error))
    p <- p + coord_flip()
    p <- p + scale_fill_brewer(palette = "Set1")
    p <- p + theme_bw()
    p
}

getPopPyramidDF(data = DCMetro_tract_map_sp)
getPopPyramidDF(data = DCMetro_tract_map_sp, fips = "51510200107")

plotPopPyramid(getPopPyramidDF(data = DCMetro_tract_map_sp, fips = "51510200107"))

getPopPyramidDF(data = DCMetro_tract_map_sp, fips = "51510200107")

# Income data ----

income_table <- read.csv("acs_income_var_converted.csv", stringsAsFactors = F)
str(pop_table)
income_table

getIncomeDF <- function(data, fips = "11001002201"){
    
#     data = DCMetro_tract_map_sp; fips = "11001002201"
    DF <- data@data[,str_sub(names(data@data), start = 1, end = 6) == "B19001"]
    DF$fips <- rownames(DF)
    
    single_fips <- DF[fips,]
    single_fips <- data.frame(t(single_fips))
    
    names(single_fips) <- "population"
    single_fips$population <- as.numeric(as.character(single_fips$population))
    
    # 0-15k
    single_fips["B19001_003a",] <- single_fips["B19001_002",] + single_fips["B19001_003",]
    single_fips["B19001_003a.err",] <- sumsqrt(c(single_fips["B19001_002.err",], single_fips["B19001_003.err",]))
    
    # 15-30k
    single_fips["B19001_006a",] <- single_fips["B19001_004",] + single_fips["B19001_005",] + single_fips["B19001_006",]
    single_fips["B19001_006a.err",] <- sumsqrt(c(single_fips["B19001_004.err",], single_fips["B19001_005.err",], single_fips["B19001_006.err",]))
    
    # 30-45k
    single_fips["B19001_009a",] <- single_fips["B19001_007",] + single_fips["B19001_008",] + single_fips["B19001_009",]
    single_fips["B19001_009a.err",] <- sumsqrt(c(single_fips["B19001_007.err",], single_fips["B19001_008.err",], single_fips["B19001_009.err",]))
    
    # 45-60k
    single_fips["B19001_011a",] <- single_fips["B19001_010",] + single_fips["B19001_011",]
    single_fips["B19001_011a.err",] <- sumsqrt(c(single_fips["B19001_010.err",], single_fips["B19001_011.err",]))
    
    single_fips$var <- row.names(single_fips)
    single_fips <- merge(single_fips, income_table, by.x = "var", by.y = "variable")
    single_fips$income <- factor(single_fips$income, levels = unique(single_fips$income), labels = unique(single_fips$income))
    
    single_fips <- cbind(subset(single_fips, measure == "estimate", select = c("var", "population", "income")), subset(single_fips, measure == "error", select = c("population")))
    names(single_fips) <- c("var", "population", "income", "error")
    
    return(single_fips)
}

plotIncomeBin <- function(input_DF){
#     input_DF = single_fips
    p <- ggplot(data = input_DF, aes(x = income, y = population, fill = income))
    p <- p + geom_bar(stat = "identity")
    p <- p + geom_errorbar(aes(ymax = population + error, ymin = population - error), width = 0.5)
    p <- p + scale_fill_manual(values = c(brewer.pal(9, "Blues")[2:9], "#062553", "#041B3C"))
    p <- p + theme_bw() + theme(legend.position="none")
    p
}

getIncomeDF(data = DCMetro_tract_map_sp, fips = "51510200107")
plotIncomeBin(getIncomeDF(data = DCMetro_tract_map_sp, fips = "51510200107"))
