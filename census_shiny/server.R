# Load libraries ----
library(shiny)
library(leaflet)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(plyr)
source('census_viz_functions.R')

# Load stationary datasets ----
censusDataDC <- readRDS("data/DCMetro_tract_map_census.rds") # Census data
censusDataNY <- readRDS("data/NYMetro_tract_map_census.rds") # Census data
censusDataSF <- readRDS("data/SFMetro_tract_map_census.rds") # Census data
acsDataDC <- readRDS("data/DCMetro_tract_map_acs.rds") # acs_data
acsDataNY <- readRDS("data/NYMetro_tract_map_acs.rds") # acs_data
acsDataSF <- readRDS("data/SFMetro_tract_map_acs.rds") # acs_data
county_fips <- read.csv("data/county_fips.csv", stringsAsFactors = F)

# Load dataframe for list of variables and attributes for population and income sub-plots
censusPopTable <- read.csv("data/census_sexbyage_var_converted.csv", stringsAsFactors = F)
acsPopTable <- read.csv("data/acs_sexbyage_var_converted.csv", stringsAsFactors = F)
acsIncomeTable <- read.csv("data/acs_income_var_converted.csv", stringsAsFactors = F)

shinyServer(function(input, output) {

    # Census plotting ----

    censusData <- reactive({
        if (input$varMetroCensus == "DC"){
            censusDataDC
        } else if (input$varMetroCensus == "NY"){
            censusDataNY
        } else if (input$varMetroCensus == "SF"){
            censusDataSF
        }
    })

    viewLngCensus <- reactive({
        if (input$varMetroCensus == "DC"){
            -77.10
        } else if (input$varMetroCensus == "NY"){
            -73.98
        } else if (input$varMetroCensus == "SF"){
            -122.20
        }
    })

    viewLatCensus <- reactive({
        if (input$varMetroCensus == "DC"){
            38.94
        } else if (input$varMetroCensus == "NY"){
            40.70
        } else if (input$varMetroCensus == "SF"){
            37.41
        }
    })

    # Color palette reactive to user input
    pal_census <- reactive({
        if (input$colorCensus == "colorNumeric"){
            colorNumeric("YlOrRd", censusData()@data[, input$varCensus])
        } else if (input$colorCensus == "colorBin"){
            colorBin("YlOrRd", censusData()@data[, input$varCensus], 6, pretty = T)
        } else if (input$colorCensus == "colorQuantile") {
            colorQuantile("YlOrRd", censusData()@data[, input$varCensus], 10)
        }

    })

    # Leaflet map elements that doesn't need to react to user input
    output$censusmap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles("CartoDB.Positron")
    })

    # Leaflet map elements that react to user input
    observe({
        pal <- pal_census()
        legendTitle <- "Total Population"

        leafletProxy("censusmap", data = censusData()) %>%
            clearShapes() %>%
            clearControls() %>%
            setView(lng = viewLngCensus(), lat = viewLatCensus(), zoom = 10) %>%
            addPolygons(layerId = row.names(censusData()@data), stroke = T, weight = 3,
                color = "white", fillOpacity  = 0.5,
                fillColor = ~pal(censusData()@data[,input$varCensus]),
                popup = paste(sep = "<br/>",
                    paste0("<strong>State: </strong>", censusData()@data$state),
                    paste0("<strong>County: </strong>", censusData()@data$countyname),
                    paste0("<strong>Tract: </strong>", censusData()@data$Name),
                    paste0("<strong>FIPS: </strong>", censusData()@data$fips),
                    paste0("<strong>Population: </strong>", censusData()@data$P0010001)
                )
            ) %>%
            addLegend(pal = pal, values = ~censusData()@data[,input$varCensus], opacity = 1, title = legendTitle)

    })

    tractDataCensus <- reactive({
        event <- input$censusmap_shape_click
        if (is.null(event))
            return(NULL)
        subset(censusData()@data, GEOID == as.character(event$id))
    })

    output$censusPopText <- reactive({
        if (is.null(tractDataCensus()))
            return("Total Population: NULL" )
        paste("Total Population: ", tractDataCensus()$P0010001)
    })

    censusPopData <- reactive({
        event <- input$censusmap_shape_click
        if (is.null(event))
            return()
        getCensusPopPyramidDF(data = censusData(), fips = event$id)
    })

    output$censusPopPyramidPlot <- renderPlot({
        if (is.null(censusPopData()))
            return()
        plotCensusPopPyramid(censusPopData())
    })

    # ACS plotting ----

    acsData <- reactive({
        if (input$varMetroACS == "DC"){
            acsDataDC
        } else if (input$varMetroACS == "NY"){
            acsDataNY
        } else if (input$varMetroACS == "SF"){
            acsDataSF
        }
    })

    viewLngACS <- reactive({
        if (input$varMetroACS == "DC"){
            -77.10
        } else if (input$varMetroACS == "NY"){
            -73.98
        } else if (input$varMetroACS == "SF"){
            -122.20
        }
    })

    viewLatACS <- reactive({
        if (input$varMetroACS == "DC"){
            38.94
        } else if (input$varMetroACS == "NY"){
            40.70
        } else if (input$varMetroACS == "SF"){
            37.41
        }
    })

    # color palette that is reactive to user input
    pal_acs <- reactive({
        if (input$colorACS == "colorNumeric"){
            colorNumeric("YlOrRd", acsData()@data[, input$varACS])
        } else if (input$colorACS == "colorBin"){
            colorBin("YlOrRd", acsData()@data[, input$varACS], 6, pretty = T)
        } else if (input$colorACS == "colorQuantile") {
            colorQuantile("YlOrRd", acsData()@data[, input$varACS], 10)
        }

    })

    # acsmap. Use the leaflet() to render map elements
    # that doesn't change with reactiveInput, such as zoom and tile
    output$acsmap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles("CartoDB.Positron")
    })

    # use leafletProxy() for map elements that needs to be updated dynamically.
    # Note: when we change user input such as end-year or choropleth variables,
    # the leaflet package will draw all the polygons from scratch (addPolygons()).
    # Redrawing the polygons introduce >1 sec delay.
    # Ideally we want to draw the polygon outine first, and only update the fill color
    # by user input. However, this function doesn't appear to be supported by the
    # current stable release of leaflet package in R.
    # This issue is worth re-visiting later.

    legendTitleACS <- reactive({
        if (input$varACS == "B01003_001"){
            "Total Population"
        } else if (input$varACS == "B19013_001"){
            "Median Household Income"
        }
    })

    observe({
        pal <- pal_acs()
        legendTitle <- legendTitleACS()

        leafletProxy("acsmap", data = acsData()) %>%
            clearShapes() %>%
            clearControls() %>%
            setView(lng = viewLngACS(), lat = viewLatACS(), zoom = 10) %>%
            addPolygons(layerId = row.names(acsData()@data), stroke = T, weight = 3,
                color = "white", fillOpacity  = 0.5,
                fillColor = ~pal(acsData()@data[,input$varACS]),
                popup = paste(sep = "<br/>",
                              paste0("<strong>Name: </strong>", acsData()@data$NAME),
                              paste0("<strong>FIPS: </strong>", acsData()@data$fips),
                              paste0("<strong>Population: </strong>",
                                     acsData()@data$B01003_001, " +/- ",
                                     round(acsData()@data$B01003_001.err)),
                              paste0("<strong>Median Household Income: </strong>",
                                     acsData()@data$B19013_001, " +/- ",
                                     round(acsData()@data$B19013_001.err))
                              )
                ) %>%
            addLegend(pal = pal, values = ~acsData()@data[,input$varACS], opacity = 1, title = legendTitle)

    })

    # Update text displayed in sidePanel based in which polygon was clicked

    tractDataACS <- reactive({
        event <- input$acsmap_shape_click
        if (is.null(event))
            return(NULL)
        subset(acsData()@data, fips == event$id)
    })

    output$acsPopText <- reactive({
        if (is.null(tractDataACS()))
            return("Total Population:" )
        paste0("Total Population: ", tractDataACS()$B01003_001, " +/- ", round(tractDataACS()$B01003_001.err))
    })

    output$acsIncomeText <- reactive({
        if (is.null(tractDataACS()))
            return("Median Household Income:" )
        paste0("Median Household Income: ", tractDataACS()$B19013_001, " +/- ", round(tractDataACS()$B19013_001.err))
    })

    # Update the population data and population pyramid plot based on which polygon was clicked
    acsPopData <- reactive({
        event <- input$acsmap_shape_click
        if (is.null(event))
            return()
        getACSPopPyramidDF(data = acsData(), fips = event$id)
    })

    output$acsPopPyramidPlot <- renderPlot({
        if (is.null(acsPopData()))
            return()
        plotACSPopPyramid(acsPopData())
    })

    # Update the income data and income binning bar plot based on which polygon was clicked
    acsIncomeData <- reactive({
        event <- input$acsmap_shape_click
        if (is.null(event))
            return()
        getACSIncomeDF(data = acsData(), fips = event$id)

    })

    output$acsIncomePlot <- renderPlot({
        if (is.null(acsIncomeData()))
            return()
        plotACSIncomeBar(acsIncomeData())
    })

})
