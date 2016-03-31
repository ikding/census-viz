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

mergeSpDataFrame <- function(spDF, DF, matchvar = "fips"){
    # Merge the SpatialPolygonDataFrame and the dataframe based on row.names

    o <- match(rownames(as(spDF, "data.frame")), DF[,matchvar])
    DF <- DF[o,]
    row.names(DF) <- DF[,matchvar]
    return(spCbind(spDF, DF))
}
