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
                                    max = 400, value = 200.5),
                                sliderInput("depth_slider", label = h4("Depth"), min = -200, 
                                    max = 0, value = c(-100, -25)),
                                sliderInput("max_cv_slider", label = h4("Maximum Current Velocity"), min = 0, 
                                    max = 3, value = 1),
                                sliderInput("dist_shore_slider", label = h4("Maximum Distance to Shore"), min = 0, 
                                    max = 100000, value = 46300)),
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

    output$suitableMap <- renderLeaflet({
        suitable_leaflet
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
