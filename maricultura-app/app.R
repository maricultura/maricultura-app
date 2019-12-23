#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(leaflet)
library(sf)
library(raster)

# Define UI for application
ui <- fluidPage(

    # Head element (contains metadata for app)
    tags$head(
        tags$style(HTML("@import url('https://fonts.googleapis.com/css?family=Source+Code+Pro&display=swap')"))),
    # Theme for the app
    theme = shinytheme("cosmo"),
    
    # Application title
    titlePanel(title = tags$img(src = "Bren-logo-only.jpg", width = 80,  "  Siting Tool for Mariculture in Brazil"),
               windowTitle = "Mariculture Tool"),
    
    # Navbar
    navbarPage("" ,
               # Do not need title for Navigation Bar
               # First tab
               tabPanel(div(icon("info-circle"),"About"),
                        p( "Visit our",
                           a( href = "https://maricultura.weebly.com", "website"),
                           "for more information.")
                        ),
               
               # Second tab
               tabPanel(div(icon("map-pin"),"Map"),
                        sidebarLayout(
                            sidebarPanel(
                                sliderInput("sst_slider", label = h4("Sea Surface Temperature"), min = 0, 
                                    max = 40, value = c(22,32)),
                                sliderInput("min_DO_slider", label = h4("Minimum Dissolved Oxygen"), min = 0, 
                                    max = 400, value = 200.5, step = 0.5),
                                sliderInput("depth_slider", label = h4("Depth"), min = -200, 
                                    max = 0, value = c(-100, -25)),
                                sliderInput("max_cv_slider", label = h4("Maximum Current Velocity"), min = 0, 
                                    max = 3, value = 1, step = 0.1),
                                sliderInput("dist_shore_slider", label = h4("Maximum Distance to Shore"), min = 0, 
                                    max = 200, value = 25, step = 0.5),
                                actionButton("run_button", label = "Run")),
                            mainPanel(
                                leafletOutput("suitableMap")
                            )
                            
                            ))),
    
    # Create footer
    br(),
    div( style = "background-color: #c5dbeb; padding: 15px; text-align: center;",
         tags$footer("Developed with",
                     tags$span(style = "font-family: 'Source Code Pro', monospace; font-size: 25px;" ,"Shiny"),
                     "from",
                     img(src = "https://rstudio.com/wp-content/uploads/2018/10/RStudio-Logo-Flat.png", height = "30px"),
                     ".",
                     br(),
                     "R version 3.6.1 (2019-07-05). Code on  ", tags$a(href ="https://github.com/annagaby/tree-monitoring", target="_blank", icon("github"),"GitHub."))
    )
)

# Define server logic 
server <- function(input, output) {

    # Render leaflet map
    output$suitableMap <- renderLeaflet({
        
        # Defining variables
        min_depth <- input$depth_slider[2]
        max_depth <- input$depth_slider[1]
        min_sst_value <- input$sst_slider[1]
        max_sst_value <- input$sst_slider[2]
        max_dist_shore <- input$dist_shore_slider*1852 # To convert from nautical miles to meters
        max_cv_value <- input$max_cv_slider
        min_DO_value <- input$min_DO_slider
        
        ### Depth
        # Read in file
        depth_mask <- raster("data/depth_mask.tif")
        
        # Reclassification matrix for depth layer
        rcl_mat_depth <- c(-Inf, max_depth, 0,
                           max_depth, min_depth, 1,
                           min_depth, Inf, 0)
        
        # Reclassify the depth layer
        depth_binary <- reclassify(depth_mask,rcl= rcl_mat_depth)
        
        ### Min SST
        # Read in file
        min_sst_mask <- raster("data/min_sst_mask.tif")
        
        # Reclassification matrix for min SST
        rcl_matrix_min <- c( -Inf, min_sst_value, 0,
                             min_sst_value, Inf, 1)
        
        # Reclassify min SST
        sst_binary_min <- reclassify(min_sst_mask, rcl = rcl_matrix_min)
        
        ### Max SST
        # Read in file
        max_sst_mask <- raster("data/max_sst_mask.tif")
        
        # Reclassify matrix for max SST layer
        rcl_matrix_max <- c( -Inf, max_sst_value, 1,
                             max_sst_value, Inf, 0)
        
        # Reclassify max SST layer
        sst_binary_max <- reclassify(max_sst_mask, rcl = rcl_matrix_max)
        
        ### Max Current Velocity
        # Read in file
        max_cv_mask <- raster("data/max_cv_mask.tif")
        
        # Reclassification matrix for current layer
        rcl_mat_current <- c(-Inf, max_cv_value, 1,
                             max_cv_value, Inf, 0)
        
        # Reclassify the depth layer
        current_binary <- reclassify(max_cv_mask ,rcl= rcl_mat_current)
        
        ### Distance to shore
        # Read in file
        dist_shore <- raster("data/dist_shore.tif")
        
        # Reclassify matrix for distance to shore layer
        rcl_matrix_dist_shore <- c( -Inf, 0, 0,
                                    0, max_dist_shore, 1,
                                    max_dist_shore, Inf, 0)
        
        # Reclassify distance to shore layer
        dist_shore_binary <- reclassify(dist_shore, rcl = rcl_matrix_dist_shore)
        
        ### DO
        # Read in file
        DO_min_mask <- raster("data/DO_min_mask.tif")
        
        # Reclassification matrix for DO
        rcl_matrix_DO <- c( -Inf, min_DO_value, 0,
                            min_DO_value, Inf, 1)
        
        # Reclassify DO min
        DO_min_binary <- reclassify(DO_min_mask, rcl = rcl_matrix_DO)
        
        ### Suitable areas (overlay of layers)
        suitable <- overlay(sst_binary_min, sst_binary_max, depth_binary, current_binary, dist_shore_binary, fun = function(a, b, c, d, e){a*b*c*d*e})
        
        # Color palette
        pal <- colorNumeric(c("#41B6C4", "#fc7303"), values(suitable), na.color = "transparent")
        
        # Leaflet map
        suitable_leaflet <- leaflet() %>% 
            addTiles() %>%
            addRasterImage(suitable, colors = pal)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
