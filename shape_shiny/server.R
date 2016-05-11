
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)
library(RColorBrewer)

DC_tract <- readRDS("data/DC_tract.rds")
NY_tract <- readRDS("data/NY_tract.rds")

DC_zillow <- readRDS("data/DC_zillow.rds")
VA_zillow <- readRDS("data/VA_zillow.rds")
NY_zillow <- readRDS("data/NY_zillow.rds")

DC_hex_002 <- readRDS("data/hexbin_DC_2e-03.rds")
DC_hex_003 <- readRDS("data/hexbin_DC_3e-03.rds")
DC_hex_004 <- readRDS("data/hexbin_DC_4e-03.rds")
DC_hex_005 <- readRDS("data/hexbin_DC_5e-03.rds")

NY_hex_002 <- readRDS("data/hexbin_NY_2e-03.rds")
NY_hex_003 <- readRDS("data/hexbin_NY_3e-03.rds")
NY_hex_004 <- readRDS("data/hexbin_NY_4e-03.rds")
NY_hex_005 <- readRDS("data/hexbin_NY_5e-03.rds")

shinyServer(function(input, output) {

    output$censusmap <- renderLeaflet({
        leaflet() %>% 
            addProviderTiles("CartoDB.Positron") %>%
            
            # addPolygons(data = DC_hex_002, fill = F, color = brewer.pal(8, "Set1")[3], group = "hexagon_2e-3", weight = 2) %>%
            # addPolygons(data = DC_hex_003, fill = F, color = brewer.pal(8, "Set1")[3], group = "hexagon_3e-3", weight = 2) %>%
            # addPolygons(data = DC_hex_004, fill = F, color = brewer.pal(8, "Set1")[3], group = "hexagon_4e-3", weight = 2) %>%
            # addPolygons(data = DC_hex_005, fill = F, color = brewer.pal(8, "Set1")[3], group = "hexagon_5e-3", weight = 2) %>%
            
            # addPolygons(data = NY_hex_002, fill = F, color = brewer.pal(8, "Set1")[3], group = "hexagon_2e-3", weight = 2) %>%
            # addPolygons(data = NY_hex_003, fill = F, color = brewer.pal(8, "Set1")[3], group = "hexagon_3e-3", weight = 2) %>%
            # addPolygons(data = NY_hex_004, fill = F, color = brewer.pal(8, "Set1")[3], group = "hexagon_4e-3", weight = 2) %>%
            # addPolygons(data = NY_hex_005, fill = F, color = brewer.pal(8, "Set1")[3], group = "hexagon_5e-3", weight = 2) %>%
            
            addPolygons(data = DC_tract, fill = F, color = brewer.pal(8, "Set1")[2], group = "census tract", weight = 2) %>%
            addPolygons(data = NY_tract, fill = F, color = brewer.pal(8, "Set1")[2], group = "census tract", weight = 2) %>%
            
            addPolygons(data = DC_zillow, fill = T, fillOpacity = 0.8, color = brewer.pal(8, "Set1")[1], group = "zillow neighborhood") %>%
            addPolygons(data = VA_zillow, fill = T, fillOpacity = 0.8, color = brewer.pal(8, "Set1")[1], group = "zillow neighborhood") %>%
            addPolygons(data = NY_zillow, fill = T, fillOpacity = 0.8, color = brewer.pal(8, "Set1")[1], group = "zillow neighborhood", layerId = NY_zillow@data$REGIONID, popup = paste( NY_zillow@data$REGIONID, NY_zillow@data$NAME)) %>%

            addLayersControl(
                # baseGroups = c("hexagon_5e-3", "hexagon_4e-3", "hexagon_3e-3", "hexagon_2e-3" ),
                # baseGroups = c("hexagon_4e-3", "hexagon_3e-3"),
                overlayGroups = c("census tract", "zillow neighborhood"),
                options = layersControlOptions(collapsed = FALSE)
            )
    })
    
    
        
    viewLng <- reactive({
        if (input$varMetro == "DC"){
            -77.1
        } else if (input$varMetro == "NY"){
            -74.0
        }
    })
    
    viewLat <- reactive({
        if (input$varMetro == "DC"){
            38.9
        } else if (input$varMetro == "NY"){
            40.7
        }
    })
    
    observe({
        leafletProxy("censusmap") %>%
            setView(lng = viewLng(), lat = viewLat(), zoom = 10)
    })
    
})
