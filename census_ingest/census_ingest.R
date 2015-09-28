# Useful resources ----
# http://www.kevjohnson.org/making-maps-in-r/
# http://www.census.gov/prod/cen2010/doc/sf1.pdf
# https://www.braintreepayments.com/blog/vaulted-credit-card-maps-with-r/
# https://gist.github.com/braintreeps/5006126#file-map-r
# http://www.thisisthegreenroom.com/2009/choropleths-in-r/
# http://eglenn.scripts.mit.edu/citystate/wp-content/uploads/2013/06/wpid-working_with_acs_R3.pdf
# http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html

# Install & import libraries ----
# install.packages(c("ggplot2", "rgdal", "scales", "ggmap", "dplyr", "Cairo", "choroplethr")) # packages that are used to deal with census tract shapefiles # http://www.kevjohnson.org/making-maps-in-r/
# install.packages(c("acs", "UScensus2010", "UScensus2000")) # packages for Census and American Community Survey (ACS) data

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
library(acs)
library(gpclib)
library(maptools)
library(choroplethr)
library(choroplethrZip)
gpclibPermit()
library(UScensus2010)
library(UScensus2010tract)
library(UScensus2010blkgrp)
library(UScensus2010blk)

# US Census Data ----
# Official shape files from Census Bureau ----
# 2014 data
tract14 <- readOGR(dsn = "../../../../data/census/tl_2014_11_tract", layer = "tl_2014_11_tract") # DC 2014 tract shapefile
tract14 <- fortify(tract14, region = "GEOID")
block14 <- readOGR(dsn = "../../../../data/census/tl_2014_11_tabblock10", layer = "tl_2014_11_tabblock10") # DC 2014 block shapefile
block14 <- fortify(block14, region = "GEOID10")
blg14 <- readOGR(dsn = "../../../../data/census/tl_2014_11_bg", layer = "tl_2014_11_bg") # DC 2014 block group shapefile
blg14 <- fortify(blg14, region = "GEOID")

p <- ggplot()
p <- p + geom_path(data = block14, aes(x = long, y = lat, group = group), color = "grey50", fill = NA)
p <- p + geom_path(data = blg14, aes(x = long, y = lat, group = group), color = "red", fill = NA)
p <- p + geom_path(data = tract14, aes(x = long, y = lat, group = group), color = "black", fill = NA)
p <- p + labs(title = "Washington DC 2014 Census Tract (Black), Block Groups (Red), and Blocks (Grey)", x = "longitude", y = "latitude")
p <- p + coord_map() + theme_minimal()
p


# 2010 data
# p14 <- ggplot()
# p14 <- p14 + geom_polygon(data = tract, aes(x = long, y = lat, group = group), color = "black", fill = "white")
# p14 <- p14 + coord_map() + theme_nothing()

tract10 <- readOGR(dsn = "../../../../data/census/tl_2010_11001_tract10", layer = "tl_2010_11001_tract10") # DC 2010 tract shapefile
tract10 <- fortify(tract10, region= "GEOID10")
blg10 <- readOGR(dsn = "../../../../data/census/tl_2010_11001_bg10", layer = "tl_2010_11001_bg10")
blg10 <- fortify(blg10, region= "GEOID10")
block10 <- readOGR(dsn = "../../../../data/census/tl_2010_11001_tabblock10", layer = "tl_2010_11001_tabblock10")
block10 <- fortify(block10, region= "GEOID10")

p <- ggplot()
p <- p + geom_path(data = block10, aes(x = long, y = lat, group = group), color = "grey50", fill = NA)
p <- p + geom_path(data = blg10, aes(x = long, y = lat, group = group), color = "red", fill = NA)
p <- p + geom_path(data = tract10, aes(x = long, y = lat, group = group), color = "black", fill = NA)
p <- p + labs(title = "Washington DC 2010 Census Tract (Black), Block Groups (Red), and Blocks (Grey)", x = "longitude", y = "latitude")
p <- p + coord_map() + theme_minimal()
p


# 2000 tract data - more change from 2010.
tract00 <- readOGR(dsn = "../../../../data/census/tl_2010_11001_tract00", layer = "tl_2010_11001_tract00") # DC 2010 tract shapefile
tract00 <- fortify(tract00, region = "TRACTCE00")
blg00 <- readOGR(dsn = "../../../../data/census/tl_2010_11001_bg00", layer = "tl_2010_11001_bg00")
blg00 <- fortify(blg00, region= "BKGPIDFP00")
block00 <- readOGR(dsn = "../../../../data/census/tl_2010_11001_tabblock00", layer = "tl_2010_11001_tabblock00")
block00 <- fortify(block00, region= "BLKIDFP00")

p <- ggplot()
p <- p + geom_path(data = block00, aes(x = long, y = lat, group = group), color = "grey50", fill = NA)
p <- p + geom_path(data = blg00, aes(x = long, y = lat, group = group), color = "red", fill = NA)
p <- p + geom_path(data = tract00, aes(x = long, y = lat, group = group), color = "black", fill = NA)
p <- p + labs(title = "Washington DC 2000 Census Tract (Black), Block Groups (Red), and Blocks (Grey)", x = "longitude", y = "latitude")
p <- p + coord_map() + theme_minimal()
p

# Analyzing and Processing Census Data ----
# WARNING: the following commands will download census data with big file sizes!
# install.tract("osx") # data for census tract, ~ 180.0 MB
# install.blkgrp("osx") # data for census block groups, 341.2 MB
# install.blk("osx") # data for census blocks, ~4.2 GB
# install.county("osx") # data for county, 35.5 MB

# Load 2010 Census Data for tract, block groups, and blocks
data(district_of_columbia.tract10) # SpatialPolygonsDataFrame Object from UScensus2010tract library
data(district_of_columbia.blkgrp10)
data(district_of_columbia.blk10)

slotNames(district_of_columbia.tract10)
plot(district_of_columbia.tract10) # appears the same with the shapefiles downloaded separately from census bureau

names(district_of_columbia.tract10)
summary(district_of_columbia.tract10)
table(district_of_columbia.tract10$state)
table(district_of_columbia.tract10$county)
table(district_of_columbia.tract10$tract)
table(district_of_columbia.tract10$fips) # I need to match fips code in census data to shape file.

# Quick plots using choropleth function in UScensus2010 package.
choropleth(sp = district_of_columbia.tract10, dem = "P0010001")
choropleth(sp = district_of_columbia.blkgrp10, dem = "P0010001")
choropleth(sp = district_of_columbia.blk10, dem = "P0010001")
# type: plot
choropleth(district_of_columbia.tract10, dem = "P0010001", color = list(fun = "rainbow", attr = list(4)), main="2010 Census Tracts\n Washington DC", type="plot", border="transparent")
# type: spplot
choropleth(district_of_columbia.tract10, dem = "P0010001", main="2010 Census Tracts\n Washington DC", border="transparent", type="spplot")

# Merging census data with official shape files
# Confirming that the fips variable in district_of_columbia.tract10@data is identical as tract2010$id. This is important for plotting choropleth graphs later
names(district_of_columbia.tract10@data)
sort(unique(district_of_columbia.tract10@data$fips))
names(tract10)
sort(unique(tract10$id))
table(sort(unique(district_of_columbia.tract10@data$fips)) %in% sort(unique(tract10$id)))

names(district_of_columbia.blkgrp10@data)
names(blg10)
table(unique(district_of_columbia.blkgrp10@data$fips) %in% unique(blg10$id))

names(district_of_columbia.blk10@data)
names(block10)
table(unique(district_of_columbia.blk10@data$fips) %in% unique(block10$id))

# Merging census data with shape files
censusTractData <- merge(tract10, district_of_columbia.tract10@data, by.x = c("id"), by.y = c("fips"), all = TRUE, sort = FALSE)
censusBlgData <- merge(blg10, district_of_columbia.blkgrp10@data, by.x = c("id"), by.y = c("fips"), all = TRUE, sort = FALSE)
censusBlockData <- merge(block10, district_of_columbia.blk10@data, by.x = c("id"), by.y = c("fips"), all = TRUE, sort = FALSE)

censusTractData$color_scale <- cut_number(censusTractData$P0010001, 9)

# Start plotting. List of variables available can be found by the following command:
# ?district_of_columbia.tract10

chomap <- function(data, var = "P0010001", cut = "number", cut_n = 5){
    plotdata <- data
    if (cut == "number"){
        plotdata$color_scale <- cut_number(data[, var], cut_n)
    } else if (cut == "interval"){
        plotdata$color_scale <- cut_interval(data[, var], cut_n)
    } else {
        print("cut argument only takes two choices: number or interval")
        break
    }
    
    p <- ggplot()
    p <- p + geom_polygon(data = plotdata, aes(x = long, y = lat, group = id, fill = color_scale), color = "grey90")
    p <- p + scale_fill_brewer(palette = "YlOrRd", name = var)
    p <- p + coord_map() + theme_minimal()
    p <<- p
    return(p)
}

chomap(data = censusTractData)
p + ggtitle("Total Population by Census Tracts - Washington DC (2010 Census)")

chomap(data = censusBlgData)
p + ggtitle("Total Population by Block Groups - Washington DC (2010 Census)")

chomap(data = censusBlockData, cut = "interval", cut_n = 5)
chomap(data = subset(censusBlockData, P0010001 != 0), cut_n = 5)

plotdata <- subset(censusBlockData, P0010001 != 0)
plotdata$color_scale <- cut_number(plotdata$P0010001, 5)
p <- ggplot()
p <- p + geom_polygon(data = subset(censusBlockData, P0010001 == 0), aes(x = long, y = lat, group = id), fill = NA, color = "grey90")
p <- p + geom_polygon(data = plotdata, aes(x = long, y = lat, group = id, fill = color_scale), color = "grey90")
p <- p + scale_fill_brewer(palette = "YlOrRd", name = "Total Population")
p <- p + ggtitle("Total Population by Blocks - Washington DC (2010 Census)")
p <- p + coord_map() + theme_minimal()
p

# discrete color scales
p <- ggplot()
p <- p + geom_polygon(data = censusTractData, aes(x = long, y = lat, group = id, fill = color_scale), color = "white")
p <- p + scale_fill_brewer(palette = "PuRd")
p <- p + coord_map() + theme_minimal()
p

p <- ggplot()
p <- p + geom_polygon(data = subset(censusBlockData, P0010001 == 0), aes(x = long, y = lat, group = id), fill = "Red", color = "grey90")
p <- p + geom_polygon(data = subset(censusBlockData, P0010001 != 0), aes(x = long, y = lat, group = id), fill = "Blue", color = "grey90")
# p <- p + scale_fill_brewer(palette = "PuRd")
p <- p + coord_map() + theme_minimal()
p

# Gradient color scales
p <- ggplot()
p <- p + geom_polygon(data = censusTractData, aes(x = long, y = lat, group = id, fill = P0010001), color = "grey")
p <- p + scale_fill_gradient(low = "white", high = "red")
p <- p + coord_map() + theme_minimal()
p

# Analyzing ACS (American Community Survey) data ----
# http://eglenn.scripts.mit.edu/citystate/wp-content/uploads/2013/06/wpid-working_with_acs_R3.pdf
# http://eglenn.scripts.mit.edu/citystate/2013/07/acs-r-example-downloading-all-the-tracts-in-a-county-or-state/
# api.key.install(key = '') need to request your own key at Census API: http://api.census.gov/data/key_signup.html

# Tables of interest:
# B01001: Sex by Age
# B01003: Total Population

# Income:
# B19001: Household Income In The Past 12 Months
# B19013: Median Household Income In The Past 12 Months
# B19101: Family Income In The Past 12 Months
# B19113: Median Family Income In The Past 12 Months
# B19201: Nonfamily Household Income In The Past 12 Months
# B19202: Median Nonfamily Household Income In The Past 12 Months
# B19301: Per Capita Income In The Past 12 Months

# Housing units status (probably not necessary - REConnect team should have more granular data)
# B25002: Occupancy Status
# B25003: Tenure (owner vs renter occupied)

# Difference between household and family income: http://economistsoutlook.blogs.realtor.org/2014/04/08/median-income-family-vs-household/

acsSave <- function(endyear = 2011, span = 5, state, geo_level = c("county", "tract", "block group"), table.number = "B01003", output_path = file.path("..", "..", "..", "..", "data", "census", "acs")){    
    # Test variables, delete by the end
#     endyear = 2011; span = 5; state = "DC"; geo_level = "tract"; table.number = "B01003"
    
    library(acs)
    
    # Load state FIPS list
    state_fips <- read.csv("../../../../data/census/shapefiles/fips.csv", stringsAsFactors = F)
    state_code <- state_fips[state_fips$twoletter == state,]$code
#     state_code = sprintf("%02s", state_code) 
    
    if (geo_level == "county"){
        geo <- geo.make(state = state_code, county = "*", check = T)
    } else if (geo_level == "tract"){
        geo <- geo.make(state = state_code, county = "*", tract = "*", check = T)
    } else if (geo_level == "block group"){
        geo_interim <- acs.fetch(endyar = endyear, span = span, geography=geo.make(state = state_code, county="*"), table.number="B01003")
        geo <- geo.make(state = state_code, county = as.numeric(geography(geo_interim))[[3]], tract = "*", block.group = "*", check = T)
    } else {
        print('Accepted geo_level: "county", "tract", "block group"')
    }
    
    acsData <- acs.fetch(endyear = endyear, span = span, geography = geo, table.number = table.number)
    acsData <<- acsData

    acsDataEst <- data.frame(estimate(acsData))
    acsDataEst$NAME <- rownames(acsDataEst)
    acsDataEst <- merge(geography(acsData), acsDataEst)
    
    acsDataErr <- data.frame(standard.error(acsData))
    acsDataErr$NAME <- rownames(acsDataErr)
    acsDataErr <- merge(geography(acsData), acsDataErr)

    write.table(acsDataEst, file = file.path(output_path, paste0(state, "_", geo_level, "_", endyear-span+1, "-", endyear, "_", table.number, ".txt")), sep = "\t", row.names = F)
    write.table(acsDataErr, file = file.path(output_path, paste0(state, "_", geo_level, "_", endyear-span+1, "-", endyear, "_", table.number, "_error.txt")), sep = "\t", row.names = F)
    
    return(acsDataEst)
    
}

acsSave(endyear = 2012, state = "MD", geo_level = "county", table.number = "B01001")
acsSave(endyear = 2010, state = "MD", geo_level = "tract", table.number = "B01001")

str(acsSave(endyear = 2012, state = "MD", geo_level = "tract", table.number = "B01001"))

acsSave(state = "DC", geo_level = "tract")
acsSave(state = "DC", geo_level = "block group")

# DC tracts
DC = acs.fetch(geography=geo.make(state=11, county="*"), table.number="B01003")
DCTracts=geo.make(state="DC", county=as.numeric(geography(DC)[[3]]), tract="*", check=T)
acs.fetch(geography=DCTracts, table.number="B01003") 

# Figure out which census area to download the data
geo.lookup(state = "DC", county = "Columbia", county.subdivision = "*")
DC = geo.make(state = 11, county = 1)
acs.fetch(geography = DC, table.number = "B01003")
acs.fetch(endyear = 2013, span = 5, geography = DC, table.number = "B01003")

# Drill down to census tracts in DC
DC = acs.fetch(geography=geo.make(state=11, county="*"), table.number="B01003")
# geography(DC)
# geography(DC)[[3]]
DCTracts=geo.make(state="DC", county=as.numeric(geography(DC)[[3]]), tract="*", check=T)
acs.fetch(geography=DCTracts, table.number="B01003") 
acs.fetch(endyear = 2013, span = 1, geography=DCTracts, table.number="B01003") 
acs.fetch(endyear = 2011, span = 1, geography=DCTracts, table.number="B01003") 

# Drill to block groups in DC.
DC = acs.fetch(endyar = 2010, span = 5, geography=geo.make(state=11, county="*"), table.number="B01003")
DC_bg = geo.make(state="DC", county=as.numeric(geography(DC)[[3]]), tract="*", block.group = "*", check=T)
DC_bg_pop = acs.fetch(endyear = 2010, span = 5, geography = DC_bg, table.number = "B01003")
DC_bg_median_income = acs.fetch(endyear = 2010, span = 5, geography = DC_bg, table.number = "B19101")

# Creates data frame of the estimates from acs object
head(geography(DC_bg_pop))
DC_bg_pop_df = data.frame(estimate(DC_bg_pop))
DC_bg_pop_df$NAME = rownames(DC_bg_pop_df)
names(DC_bg_pop_df)

DC_bg_pop_df <- merge(DC_bg_pop_df, geography(DC_bg_pop))
head(DC_bg_pop_df)
tail(DC_bg_pop_df)

# Convert state, county, tract and block group into fips codes.
VA = acs.fetch(geography=geo.make(state=51, county="*"), table.number="B01003")

# Need to pad codes with zeros first
# state: 2 digits
# county: 3 digits
# tract: 6 digits
# blkgrp: 1 digit
head(district_of_columbia.blkgrp10@data[,c(1:6)])
table(DC_bg_pop_df$tract)
str(DC_bg_pop_df)
DC_bg_pop_df$fips <- paste0(sprintf("%02s", DC_bg_pop_df$state), 
                            sprintf("%03s", DC_bg_pop_df$county), 
                            sprintf("%06s", DC_bg_pop_df$tract), 
                            sprintf("%01s", DC_bg_pop_df$blockgroup))
head(DC_bg_pop_df)

# Confirming that all the fips in our custom dataframe matches with shape files in blg10.
table(sort(unique(DC_bg_pop_df$fips)) %in% sort(unique(blg10$id)))

# Merging acs data with shape files
acsBlgData <- merge(blg10, DC_bg_pop_df, by.x = c("id"), by.y = c("fips"), all = TRUE, sort = FALSE)

# Custom function to plot choropleth maps with cut_intervals or cut_number that neglect all the block groups with zero data.
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


chomap(data = acsBlgData, var = "B01003_001")
chomapNonZero(data = acsBlgData, var = "B01003_001", title = "Total Population by Census Block Groups - Washington DC (2010 ACS Survey)")
p + ggtitle("Total Population by Census Block Groups - Washington DC (2010 ACS Survey)")

# A new function to wrap three functionalities: 
# Fetching data from acs (acs.fetch), 
# Combining acs data with shape files
# Plot choropleth maps

# I used 2010 endyear by default since the shape files I have is from 2010. 
# May need different shape files if you need acs data from different end-years.
DC = acs.fetch(endyar = 2010, span = 5, geography=geo.make(state=11, county="*"), table.number = "B01003")
DC_bg = geo.make(state="DC", county=as.numeric(geography(DC)[[3]]), tract="*", block.group = "*", check=T)

acsChoropleth <- function(var = "B01003", cut = "number", cut_n = 5){
    
    "
    A custom function to fetch data from census ACS site and plot the choropleth map.
    
    Custom-defined variable:
    var: the table of interest. 'B01003' = total population. Full list of table can be found on https://www.socialexplorer.com/data/ACS2011_5yr/documentation/c88dd660-2fbc-4e67-96f2-660714209704. WARNING: this function has been tested for only a very small set of the variables.
    cut: can be either 'number' or 'interval'. This will produce the color scale from our numeric vectors. For more detail, type ?cut_number or ?cut_interval
    cut_n: number of intervals to create.
    "
    
    DC_bg_var = acs.fetch(endyear = 2010, span = 5, geography = DC_bg, table.number = var)    
    DC_bg_var_df = data.frame(estimate(DC_bg_var))
    DC_bg_var_df$NAME = rownames(DC_bg_var_df)    
    DC_bg_var_df <- merge(DC_bg_var_df, geography(DC_bg_var))
    
    # Convert state, county, tract and block group into fips codes.
    DC_bg_var_df$fips <- paste0(sprintf("%02s", DC_bg_var_df$state), 
                                sprintf("%03s", DC_bg_var_df$county), 
                                sprintf("%06s", DC_bg_var_df$tract), 
                                sprintf("%01s", DC_bg_var_df$blockgroup))
    
    # Merging census data with shape files
    data <- merge(blg10, DC_bg_var_df, by.x = c("id"), by.y = c("fips"), all = TRUE, sort = FALSE)
    
    var_long = paste0(var, "_001")
    plotdata <- data[data[,var_long] != 0,]
    zerodata <- data[data[,var_long] == 0,]
    
    if (cut == "number"){
        plotdata$color_scale <- cut_number(plotdata[, var_long], cut_n)
    } else if (cut == "interval"){
        plotdata$color_scale <- cut_interval(plotdata[, var_long], cut_n)
    } else {
        print("cut argument only takes two choices: number or interval")
        break
    }
    
    p <- ggplot()
    p <- p + geom_polygon(data = zerodata, aes(x = long, y = lat, group = id), fill = NA, color = "grey90")
    p <- p + geom_polygon(data = plotdata, aes(x = long, y = lat, group = id, fill = color_scale), color = "grey90")
    p <- p + scale_fill_brewer(palette = "YlOrRd", name = var_long)
    p <- p + coord_map() + theme_minimal()
    p <<- p
    return(p)
    
}

acsChoropleth(var)
p + ggtitle("Total Population by Census Block Groups - Washington DC (2010 ACS Survey)")

acsChoropleth(var = "B19113")
acsChoropleth(var = "B19113", cut = "interval")
p + ggtitle("Median Family Income by Census Block Groups - Washington DC (2010 ACS Survey)")
