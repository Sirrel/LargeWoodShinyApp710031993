###################################################
## Practical 6 for GEOM184 - Open Source GIS ##
## 14/11/2025 ##
## Creating a ShinyApp ##
## App.R ##
## Code by Ryan Reeve ##
###################################################

# Load packages ----
library(shiny)
library(leaflet)
library(sf)
library(raster)
library(ggplot2)
library(ggiraph)
library(RColorBrewer)
library(terra)
library(leafem)
library(dbscan)
library(dplyr)
library(plotly)

# Increase max allowed size of files loaded within app to 1000MB
options(shiny.maxRequestSize = 1000 * 1024^2)

# Run global script containing all relevant data ----
source("Global.R")

# Define UI for visualisation ----
source("UI.R")

ui <- navbarPage("Instream large wood on the River Isonzo", id = 'nav',
                 tabPanel("Map",
                          div(class="outer",
                              leafletOutput("map", height = "calc(100vh - 70px)")
                              ),
                          # Add floating panel for plotting
                          absolutePanel(
                            bottom = 80,
                            right = 20,
                            width = 450,
                            height = "auto",
                            draggable = TRUE,
                            style = "background: #FFFFFF; border: 1px solid #CCCCCC; padding: 10px; opacity: 0.9;",
                            plotlyOutput("plot_dynamic", height = "350px")
                          ),
                          # Add floating panel for instructions
                          absolutePanel(
                            bottom = 80,
                            left = 20,
                            width = 450,
                            height = "auto",
                            draggable = FALSE,
                            style = "background: #FFFFFF; border: 1px solid #CCCCCC; padding: 10px; opacity: 0.9;",
                            # Instructions Text
                            h3("Using the Map"),
                            p("This application visualises instream large wood on the River Isonzo"),
                            p("Please allow a moment or two for data to load and expand the window size for the best experience."),
                            tags$ul(
                              tags$li(strong("Explore:"), " Pan and zoom the map."),
                              tags$li(strong("Toggle Layers:"), " Use the layers control box to turn data on and off."),
                              tags$li(strong("Data Exploration:"), " Click on the large wood and bridge data for popup details. Hover to view slope and aspect data displayed in the top-right."),
                              tags$li(strong("Interactive Plot:"), " The plot in the bottom-right updates with your current map view."),
                              tags$li(strong("Measurements: "), "Use the measure tool in the top-left to measure distances and areas.")
                            )
                          )
                          ),
                 tabPanel("Large Wood Data Plot",
                          div(class="outer",
                              plotlyOutput("plot1"))
                          ),
                 tabPanel("Environmental Explorer",
                          plotlyOutput("slope_plot"),
                          br(),
                          br(),
                          plotlyOutput("aspect_plot"))
)

# Define the server that performs all necessary operations ----
server <- function(input, output, session){
  source("Server.R", local = TRUE)
}

# Run the application ----
shinyApp(ui, server)
