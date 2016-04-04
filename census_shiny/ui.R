# Load libraries ----
library(shiny)
library(leaflet)

shinyUI(navbarPage("Census Explorer",

    tabPanel("Census 2010",
        sidebarLayout(position = "right",
            sidebarPanel(
                selectInput("varMetroCensus", label = "Metro to Display",
                            choices = list("Washington DC" = "DC",
                                           "New York City" = "NY",
                                           "San Francisco" = "SF"
                                           ),
                            selected = "DC"
                ),
                selectInput("varCensus", label = "Choropleth variable",
                            choices = list("Total Population" = "P0010001"),
                            selected = "P0010001"
                ),
                selectInput("colorCensus", label = "Color scheme",
                            choices = list("Numeric (Continuous)" = "colorNumeric",
                                           "Numeric (Discrete)" = "colorBin",
                                           "Quantiles (Discrete)" = "colorQuantile"),
                            selected = "colorNumeric"),
                br(),
                textOutput("censusPopText"),
                br(),
                plotOutput("censusPopPyramidPlot", height = 400)
            ),

            mainPanel(
                leafletOutput("censusmap", width="100%", height = 600),
                helpText("Source: Census 2010 demographic data from U.S. Census Bureau.")
            )

        )
    ),
    tabPanel("ACS 5-Yr Summary (2010-2014)",
        sidebarLayout(position = "right",
            sidebarPanel(
                # sliderInput("endyear", "End Year:", min = 2014, max = 2014, value = 2014, step = 1, ticks = F, sep = ""),
                selectInput("varMetroACS", label = "Metro to Display",
                            choices = list("Washington DC" = "DC",
                                           "New York City" = "NY",
                                           "San Francisco" = "SF"
                            ),
                            selected = "DC"
                ),

                selectInput("varACS", label = "Choropleth variable",
                    choices = list("Total Population" = "B01003_001", "Median Household Income" = "B19013_001"),
                    selected = "B19013_001"
                ),
                selectInput("colorACS", label = "Color scheme",
                    choices = list("Numeric (Continuous)" = "colorNumeric",
                                   "Numeric (Discrete)" = "colorBin",
                                   "Quantiles (Discrete)" = "colorQuantile"),
                    selected = "colorNumeric"),
                br(),
                textOutput("acsIncomeText"),
                plotOutput("acsIncomePlot", height = 250),
                br(),
                textOutput("acsPopText"),
                plotOutput("acsPopPyramidPlot", height = 300)
           ),

           mainPanel(
               leafletOutput("acsmap", width="100%", height = 600),
               helpText("Source: American Community Survey from U.S. Census Bureau.
                        Note: Tract-level data is only available as 5-year summaries;
                        so end-year 2010 contains aggregated data from 2006-2010,
                        end year 2014 contains aggregated data from 2010-2014, etc")
           )
#
        )
    )
))
