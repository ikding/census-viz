# Import libraries
library(rgdal)
library(maptools)
library(stringr)
library(ggplot2)
library(tools)
library(leaflet)
library(RColorBrewer)

# Hard-coded file paths
censusDataFolder <- "../../../data/census/census2010/"
acsDataFolder <-  "../../../data/census/acs/"
shapeDataFolder <- "../../../data/census/shapefiles/geojson/"
state_fips <- read.csv("../../../data/census/shapefiles/fips.csv", stringsAsFactors = F)
county_fips <- read.csv("../../../data/census/shapefiles/county_fips.csv", stringsAsFactors = F)
hexagonDataFolder <- "../../../data/geojson_processed/"

# Read in the GeoJSON as SpatialPolygonDataFrame ----

readGeoJSON <- function(state = "DC", year = 2011, geolevel = c("tract", "bg"), format = c("sp", "ggplot2"), county_subset = NULL){
    # Function to read-in GeoJSON information.
    # format variable: sp = SpatialPolygonDataFrame (for leaflet)
    #                  ggplot2 = fortify(SpatialPolygonDataFrame), to be used in ggplot2
    
    statecode <- sprintf("%02s", state_fips[state_fips$twoletter == state,]$code)
    filesuffix <- if (year == 2010) '10.geojson' else '.geojson'
    polygonID = if (year == 2010) "GEOID10" else "GEOID"
    countyID = if (year == 2010) "COUNTYFP10" else "COUNTYFP"
    
    filename <- paste0(shapeDataFolder, "tl_", as.character(year), "_", statecode, "_", geolevel, filesuffix)
    spData <- readOGR(dsn = filename, layer = "OGRGeoJSON")
    
    if (is.character(county_subset)){
        spData <- spData[spData@data[, countyID] %in% county_subset,]
    }
    
    if (format == "ggplot2"){
        spData <- fortify(spData, region = polygonID)
    } else if (format == "sp"){
        spData@data$id <- as.character(spData@data[, polygonID])
        spData <- spChFIDs(spData, spData@data$id)
    }
    
    return(spData)
    
}

# Quick funtion to plot map
qplot_map <- function(spData){
    ggplot(data = fortify(spData)) + geom_polygon(aes(x = long, y = lat, group = id), fill = "white", color = "black") + coord_map()
}

# Merge census shape data ----

# DC Metro: census tracts inside the beltway ----
DC_tract <- readGeoJSON(state = "DC", year = 2010, geolevel = "tract", format = "sp")
# qplot_map(DC_tract)
MD_tract <- readGeoJSON(state = "MD", year = 2010, geolevel = "tract", format = "sp", county_subset = c("031", "033"))
VA_tract <- readGeoJSON(state = "VA", year = 2010, geolevel = "tract", format = "sp", county_subset = c("510", "013", "059", "600", "610"))
VA_tract2 <- readGeoJSON(state = "VA", year = 2010, geolevel = "tract", format = "sp", county_subset = c("510", "013", "610"))

qplot_map(VA_tract2)

# List of fips code of MD census tracts inside beltway - hand-curated
MD_insideBeltway = c("24031701601", "24031701602", "24031701701", "24031701702", "24031701703", "24031701704", "24031701800", "24031701900", "24031702000", "24031702101", "24031702200", "24031702301", "24031702302", "24031702401", "24031702402", "24031702500", "24031702900", "24031702601", "24031702602", "24031702700", "24031702800", "24031704403", "24031704404", "24031704502", "24031704503", "24031704504", "24031704600", "24031704700", "24031704803", "24031704804", "24031704805", "24031704806", "24031705000", "24031705100", "24031705200", "24031705300", "24031705400", "24031705501", "24031705502", "24031705601", "24031705602", "24031705701", "24031705702", "24031705800", "24031705901", "24031705902", "24031705903", "24033805201", "24033805000", "24033805500", "24033805602", "24033805906", "24033805907", "24033807305", "24033807301", "24033805904", "24033805700", "24033805601", "24033805202", "24033805909", "24033805801", "24033805802", "24033804802", "24033804900", "24033804801", "24033804700", "24033804600", "24033805101", "24033806000", "24033806100", "24033806200", "24033806300", "24033805908", "24033806400", "24033807200", "24033806900", "24033807000", "24033806800", "24033806714", "24033806713", "24033806706", "24033807102", "24033806501", "24033806601", "24033806602", "24033803900", "24033804002", "24033804001", "24033804400", "24033804300", "24033804200", "24033804101", "24033804102", "24033803801", "24033803803", "24033803605", "24033803610", "24033803613", "24033803612", "24033803700", "24033803602", "24033803509", "24033803508", "24033803200", "24033803300", "24033803100", "24033803001", "24033803401", "24033803402", "24033803525", "24033803524", "24033803519", "24033802805", "24033802901", "24033803002", "24033802700", "24033802600", "24033802803", "24033802804", "24033802204", "24033802404", "24033802501", "24033802502", "24033802408", "24033802407", "24033802406", "24033802405", "24033802002", "24033802301", "24033802203", "24033801805", "24033801807", "24033801808", "24033801804", "24033801802", "24033802103", "24033802106", "24033802104", "24033802107", "24033802001", "24033801908", "24033801907", "24033801905", "24033801706", "24033801801", "24033801500", "24033801600", "24033801704", "24033801707", "24033801708", "24033801906") 

# List of fips code of VA census tracts inside beltway - Fairfax county only
VA_insideBeltway <- as.character(VA_tract@data$GEOID10[str_sub(as.character(VA_tract@data$GEOID10), start = 6, end = 7) %in% c("45", "47")]) # Taking advantage of a rule specific to VA. The starting to characters of all the tracts in Fairfax County that is inside the beltway happens to have be "45" or "47"...
VA_insideBeltway <- c(VA_insideBeltway, "51059980300") # ... with one exception of the above rule

qplot_map(VA_tract[VA_tract@data$GEOID10 %in% VA_insideBeltway,])

# Subset the census tracts in each state
MD_tract <- MD_tract[MD_tract@data$GEOID10 %in% MD_insideBeltway,]
VA_tract <- VA_tract[VA_tract@data$GEOID10 %in% VA_insideBeltway,]
VA_tract <- spRbind(VA_tract, VA_tract2)

# Re-combine the census tracts
DCMetro_tract <- spRbind(MD_tract, VA_tract)
DCMetro_tract <- spRbind(DC_trfact, DCMetro_tract)

writeOGR(obj = DCMetro_tract, dsn = "DC_tract", driver = "GeoJSON", layer = "OGRGeoJSON")

# Make sure it looks right
qplot_map(DCMetro_tract)

DCMetro_tract@bbox # use for hexagon generation
#         min       max
# x -77.22214 -76.84387
# y  38.78522  39.02287

# NY Metro: census tracts within 5 boroughs ----
NY_tract <- readGeoJSON(state = "NY", year = 2010, geolevel = "tract", format = "sp", county_subset = c("005", "047", "061", "081", "085")) # Bronx, Brooklyn (King County), Manhattan (New York County), Queens, Staten Island (Richmond County)

qplot_map(NY_tract)

writeOGR(obj = NY_tract, dsn = "NY_tract", driver = "GeoJSON", layer = "OGRGeoJSON")

NY_tract@bbox
#         min       max
# x -74.25909 -73.70027
# y  40.47740  40.91758

writeSpatialShape()

# AUS Metro: census tracts within Austin metropolitan area
AUS_tract <- readGeoJSON(state = "TX", year = 2010, geolevel = "tract", format = "sp", county_subset = c("021", "055", "209", "453", "491")) #  Bastrop, Caldwell, Hays, Travis, and Williamson (Travis is not to be confused with Austin county that has nothing to do with the city of Austin)

qplot_map(AUS_tract)

writeOGR(obj = AUS_tract, dsn = "AUS_tract", driver = "GeoJSON", layer = "OGRGeoJSON")

AUS_tract@bbox
#         min       max
# x -98.29760 -97.02446
# y  29.63072  30.90619

DC_hexagon <- readOGR(dsn = file.path(hexagonDataFolder, "hexbin_DC_5e-03.geojson"), layer = "OGRGeoJSON")
NY_hexagon <- readOGR(dsn = file.path(hexagonDataFolder, "hexbin_NY_5e-03.geojson"), layer = "OGRGeoJSON")
AUS_hexagon <- readOGR(dsn = file.path(hexagonDataFolder, "hexbin_AUS_5e-03.geojson"), layer = "OGRGeoJSON")

ggplot() +
    geom_path(data = fortify(DC_hexagon), aes(x = long, y = lat, group = id), color = "blue") +
    geom_path(data = fortify(DCMetro_tract), aes(x = long, y = lat, group = id), color = "black", size = 1) +
    coord_map() + theme_minimal()

ggplot() +
    geom_path(data = fortify(NY_hexagon), aes(x = long, y = lat, group = id), color = "blue") +
    geom_path(data = fortify(NY_tract), aes(x = long, y = lat, group = id), color = "black", size = 1) +
    coord_map() + theme_minimal()

ggplot() +
  geom_path(data = fortify(AUS_hexagon), aes(x = long, y = lat, group = id), color = "blue") +
  geom_path(data = fortify(AUS_tract), aes(x = long, y = lat, group = id), color = "black", size = 1) +
  coord_map() + theme_minimal()

# For some reason, the writeOGR function will make duplicate IDs which cannot be read back into R.
# The get-around is to first use maptools::writeSpatialShape to write to shape file, then use ogr2ogr from command line to convert shape file to geojson.
# command line:
# $ ogr2ogr -s_srs crs:84 -t_srs crs:84 -f GeoJSON censustract_DC_2010.geojson DC_tract/DC_tract.shp
writeSpatialShape(x = DCMetro_tract, fn = "DC_tract")
writeSpatialShape(x = NY_tract, fn = "NY_tract")
writeSpatialShape(x = AUS_tract, fn = "AUS_tract")

readOGR(dsn = "DC.geojson", layer = "OGRGeoJSON")

# Save to RDS for shiny ----
saveRDS(DCMetro_tract, file = "shape_shiny/data/DC_tract.rds")
saveRDS(NY_tract, file = "shape_shiny/data/NY_tract.rds")
saveRDS(AUS_tract, file = "shape_shiny/data/AUS_tract.rds")

saveGeoJSONToRDS <- function(source_path, dest_path, file){
    spData <- readOGR(dsn = file.path(source_path, file), layer = "OGRGeoJSON")
    saveRDS(object = spData, file = file.path(dest_path, paste0(file_path_sans_ext(file), ".rds")))
}

list.files(hexagonDataFolder)

for (f in list.files(hexagonDataFolder)){
    saveGeoJSONToRDS(source_path = hexagonDataFolder, dest_path = file.path("shape_shiny", "data"), file = f)
}


# Filtered Hexagons, DC

list.files(hexagonDataFolder)

raw_hexagon_DC <- readOGR(dsn = file.path(hexagonDataFolder, "hexbin_DC_4e-03.geojson"), layer = "OGRGeoJSON")
filtered_hexagon_DC <- readOGR(dsn = file.path(hexagonDataFolder, "hexbin_DC_4e-03_filtered.geojson"), layer = "OGRGeoJSON")

qplot_map(raw_hexagon_DC)
qplot_map(filtered_hexagon_DC)

ggplot() +
    geom_path(data = fortify(filtered_hexagon_DC), aes(x = long, y = lat, group = id), color = "blue") +
    geom_path(data = fortify(DCMetro_tract), aes(x = long, y = lat, group = id), color = "black", size = 1) +
    coord_map() + theme_minimal()

leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -77.0, lat = 38.9, zoom = 11) %>%
    addPolygons(data = DCMetro_tract, group = "Census Tract", color = brewer.pal(name = "Set1", n = 3)[1], fill = F) %>%
    addPolygons(data = filtered_hexagon_DC, group = "Filtered Hexagons", color = brewer.pal(name = "Set1", n = 3)[2])

# Filtered Hexagons, NY

list.files(hexagonDataFolder)

raw_hexagon_NY <- readOGR(dsn = file.path(hexagonDataFolder, "hexbin_NY_3e-03.geojson"), layer = "OGRGeoJSON")
filtered_hexagon_NY <- readOGR(dsn = file.path(hexagonDataFolder, "hexbin_NY_3e-03_filtered.geojson"), layer = "OGRGeoJSON")

qplot_map(raw_hexagon_NY)
qplot_map(filtered_hexagon_NY)

ggplot() +
  geom_path(data = fortify(NY_tract), aes(x = long, y = lat, group = id), color = "black", size = 1) +
  geom_path(data = fortify(raw_hexagon_NY), aes(x = long, y = lat, group = id), color = "blue") +
  coord_map() + theme_minimal()

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = -74.0, lat = 40.7, zoom = 10) %>%
  addPolygons(data = NY_tract, group = "Census Tract", color = brewer.pal(name = "Set1", n = 3)[1], fill = F) %>%
  addPolygons(data = filtered_hexagon_NY, group = "Filtered Hexagons", color = brewer.pal(name = "Set1", n = 3)[2])

