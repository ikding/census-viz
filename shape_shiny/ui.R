
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)

shinyUI(navbarPage("REConnect-Shape Files",
                   
                   tabPanel("Census Tract vs. Zillow",
                            sidebarLayout(position = "right",
                                          sidebarPanel(
                                              selectInput("varMetro", label = "Metro to Display", 
                                                          choices = list("New York City" = "NY",
                                                                         "Washington DC" = "DC"),
                                                          selected = "DC"
                                              )
                                              
                                          ),          
                                          
                                          mainPanel(
                                              leafletOutput("censusmap", width="100%", height = 600),
                                              helpText("Source: Census 2010 shape files and Zillow shape files.")
                                          )
                                          
                            )
                   )
))
