library(rgdal)
library(ggplot2)
library(leaflet)
library(RColorBrewer)
library(maptools)
library(stringr)

# Official shape files from Census Bureau ----
# 2010 data
list.files("../../../data/census/shapefiles/geojson")
file.exists(file.path(census_path, "tl_2010_11_tract10.geojson"))

census_path <- file.path("..", "..", "..", "..", "data", "census", "shapefiles", "geojson")
zillow_path <- file.path("..", "..", "..", "..", "data", "zillow")

DC_tract <- readOGR(dsn = file.path(census_path, "tl_2010_11_tract10.geojson"), layer = "OGRGeoJSON")
DC_zillow <- readOGR(dsn = file.path(zillow_path, "ZillowNeighborhoods-DC"), layer = "ZillowNeighborhoods-DC")

VA_tract <- readOGR(dsn = file.path(census_path, "tl_2010_51_tract10.geojson"), layer = "OGRGeoJSON")
VA_zillow <- readOGR(dsn = file.path(zillow_path, "ZillowNeighborhoods-VA"), layer = "ZillowNeighborhoods-VA")

MD_tract <- readOGR(dsn = file.path(census_path, "tl_2010_24_tract10.geojson"), layer = "OGRGeoJSON")
MD_zillow <- readOGR(dsn = file.path(zillow_path, "ZillowNeighborhoods-MD"), layer = "ZillowNeighborhoods-MD")

NY_tract <- readOGR(dsn = file.path(census_path, "tl_2010_36_tract10.geojson"), layer = "OGRGeoJSON")
NY_zillow <- readOGR(dsn = file.path(zillow_path, "ZillowNeighborhoods-NY"), layer = "ZillowNeighborhoods-NY")

MD_tract <- MD_tract[MD_tract@data$COUNTYFP10 %in% c("031", "033"),] # Montgomery and Prince George's Counties
VA_tract <- VA_tract[VA_tract@data$COUNTYFP10 %in% c("510", "013", "059", "600", "610"),] # Alexandria, Arlington, Falls Church, Fairfax
NY_tract <- NY_tract[NY_tract@data$COUNTYFP10 %in% c("005", "047", "061", "081", "085"),] # Bronx, Brooklyn, Manhattan, Queens, Staten Island

VA_zillow <- VA_zillow[VA_zillow@data$CITY %in% c("Alexandria", "Arlington"),]
NY_zillow <- NY_zillow[str_sub(NY_zillow@data$CITY, 1, 13) %in% c("New York City"),]

saveRDS(DC_tract, "data/DC_tract.rds")
saveRDS(MD_tract, "data/MD_tract.rds")
saveRDS(VA_tract, "data/VA_tract.rds")
saveRDS(NY_tract, "data/NY_tract.rds")

saveRDS(DC_zillow, "data/DC_zillow.rds")
saveRDS(VA_zillow, "data/VA_zillow.rds")
saveRDS(NY_zillow, "data/NY_zillow.rds")


leaflet() %>% 
    setView(lng = -77.1, lat = 38.9, zoom = 9) %>% 
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = DC_tract, , fill = F, color = brewer.pal(3, "Set1")[2], group = "census tract") %>%
    addPolygons(data = VA_tract, , fill = F, color = brewer.pal(3, "Set1")[2], group = "census tract") %>%
    addPolygons(data = MD_tract, , fill = F, color = brewer.pal(3, "Set1")[2], group = "census tract") %>%
    addPolygons(data = NY_tract, , fill = F, color = brewer.pal(3, "Set1")[2], group = "census tract") %>%
    
    addPolygons(data = DC_zillow, , fill = F, color = brewer.pal(3, "Set1")[1], group = "zillow neighborhood") %>%
    addPolygons(data = VA_zillow, , fill = F, color = brewer.pal(3, "Set1")[1], group = "zillow neighborhood") %>%
    addPolygons(data = NY_zillow, , fill = F, color = brewer.pal(3, "Set1")[1], group = "zillow neighborhood") %>%
    
    addLayersControl(
        overlayGroups = c("census tract", "zillow neighborhood")
    )

    

leaflet() %>% 
    setView(lng = -77.1, lat = 38.9, zoom = 9) %>% 
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = DC_tract, layerId = DC_tract@data$GEOID10, popup = DC_tract@data$NAMELSAD10, fillColor = "white", fillOpacity = 0.01, color = brewer.pal(3, "Set1")[2]) %>%
    addPolygons(data = DC_zillow, layerId = DC_zillow@data$REGIONID, popup = DC_zillow@data$NAME, fillColor = "white", fillOpacity = 0.01, color = brewer.pal(3, "Set1")[1])

