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
library(htmlwidgets)
library(waiter)
library(mapview)

# Define UI for application
ui <- fluidPage(
    
    # Add waiter dependencies
    use_waiter(),
    
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
               selected = div(icon("map-pin"),"Map"),
               # Do not need title for Navigation Bar
               # First tab
               tabPanel(div(icon("info-circle"),"About"),
                        p( "MARICULTURA is a team of 5 bright graduate students studying at the Bren School of Environmental Science & Management at the University of California Santa Barbara.  A partnership with the World Wildlife Fund has enabled the team to shape marine aquaculture planning off the coast of Brazil. 
                           
                           
                           Visit our",
                           a( href = "https://maricultura.weebly.com", "website"),
                           "for more information."),
                        img(src ="schooloffish.jpg", height = 300, width = 800)
                        ),
               
               # Second tab
               tabPanel(div(icon("map-pin"),"Map"),
                        sidebarLayout(
                            sidebarPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel( "Variable",
                                                      sliderInput("sst_slider", label = h5("Sea Surface Temperature (°C)"), min = 0, 
                                                                  max = 40, value = c(22,32)),
                                                      sliderInput("depth_slider", label = h5("Depth (m)"), min = 0, 
                                                                  max = 200, value = c(25, 100)),
                                                      #sliderInput("min_DO_slider", label = h4("Minimum Dissolved Oxygen ()"), min = 0, 
                                                                  #max = 400, value = 200.5, step = 0.5),
                                                      #sliderInput("max_cv_slider", label = h4("Maximum Current Velocity (m/s)"), min = 0,                                                                     max = 3, value = 1, step = 0.1),
                                                      #sliderInput("dist_shore_slider", label = h4("Maximum Distance to Shore (NM)"), min                                                                     =0, max = 200, value = 25, step = 0.5)),
                                                      numericInput("min_DO_slider",
                                                                   label = HTML("<h5>Minimum Dissolved Oxygen (mol/m<sup>3</sup>)</h5>"),
                                                                   min = 0,   
                                                                   max = 400,
                                                                  step = 0.5, value = 200),
                                                      numericInput("max_cv_slider", label = h5("Maximum Current Velocity (m/s)"),
                                                                   min = 0,
                                                                   max = 3,
                                                                  step = 0.1,
                                                                  value = 1),
                                                      numericInput("max_swh_slider", label = h5("Maximum Significant Wave Height"),
                                                                   min = 0,
                                                                   max = 3,
                                                                   step = 0.1,
                                                                   value = 1),
                                                      numericInput("dist_shore_slider", label = h5("Maximum Distance to Shore (NM)"),
                                                                   min = 0,
                                                                   max = 200,
                                                                   step = 0.5,
                                                                   value = 25)),
                                            tabPanel( "Fixed",
                                                          checkboxGroupInput("checkGroup", label = h3("Select Barrier(s)"), 
                                                                         choices = list("MPAs" = 1,
                                                                                        "Reefs" = 2,
                                                                                        "Artificial Reefs" = 3,
                                                                                        "Oil Pipelines" = 4,
                                                                                        "Oil Production" = 5,
                                                                                        "Shipping lanes" = 6),
                                                                         selected = c(1, 2, 3, 4, 5, 6))
                                            )),
                                actionButton("run_button", label = "Run"),
                                downloadButton("download_button", label = "Download")
                            ),
                            mainPanel(
                                leafletOutput("suitableMap", height = "100vh")
                            )
                            
                            )
                        ),
               
               # Third Tab
               tabPanel(div(icon("book-open"),"User Guide" )),
            
                # Fourth Tab
                tabPanel(div(icon("table"),"Metadata"))
                ),
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
    
    ### Modal Dialogue
    # Create modal dialogue
    dataModal <- function(failed = FALSE) {
        modalDialog(
            title = tags$h1(
                style = "text-align: center;",
                "Welcome to Brazil's First Mariculture Siting Tool !"
            ),
            tags$div(
                style = "text-align: center;",
                tags$p("This tool identifies available sites for marine aquaculture development in Brazil"),
                tags$p(tags$b("1."), "Find sites by selecting and modifying environmental conditions and barriers"),
                tags$p(tags$b("2."), "Click the 'Run' button to generate a map of suitable sites"),
                tags$p(tags$b("3."), "Download your map!")
            ),
            footer = tagList(
                modalButton("Start")
            )
        )
    }
    
    # Show modal
        showModal(dataModal())
    
    ### Depth
    # Defining variables
    min_depth <- reactive(-input$depth_slider[1])
    max_depth <- reactive(-input$depth_slider[2])
    
    # Read in file
    depth_mask <- raster("data/depth_mask.tif")
    
    # Reclassification matrix for depth layer
    rcl_mat_depth <- reactive(c(-Inf, max_depth(), 0,
                       max_depth(), min_depth(), 1,
                       min_depth(), Inf, 0))
    
    # Reclassify the depth layer
    depth_binary <- reactive(reclassify(depth_mask,rcl= rcl_mat_depth()))
    
    ### Min SST
    # Defining variable
    min_sst_value <- reactive(input$sst_slider[1])
    
    # Read in file
    min_sst_mask <- raster("data/min_sst_mask.tif")
    
    # Reclassification matrix for min SST
    rcl_matrix_min <- reactive(c( -Inf, min_sst_value(), 0,
                         min_sst_value(), Inf, 1))
    
    # Reclassify min SST
    sst_binary_min <- reactive(reclassify(min_sst_mask, rcl = rcl_matrix_min()))
    
    
    ### Max SST
    # Defining variable
    max_sst_value <- reactive(input$sst_slider[2])
    
    # Read in file
    max_sst_mask <- raster("data/max_sst_mask.tif")
    
    # Reclassify matrix for max SST layer
    rcl_matrix_max <- reactive(c( -Inf, max_sst_value(), 1,
                         max_sst_value(), Inf, 0))
    
    # Reclassify max SST layer
    sst_binary_max <- reactive(reclassify(max_sst_mask, rcl = rcl_matrix_max()))
    
    ### Max Current Velocity
    # Defining variable
    max_cv_value <- reactive(input$max_cv_slider)
    
    # Read in file
    max_cv_mask <- raster("data/max_cv_mask.tif")
    
    # Reclassification matrix for current layer
    rcl_mat_current <- reactive(c(-Inf, max_cv_value(), 1,
                         max_cv_value(), Inf, 0))
    
    # Reclassify the depth layer
    current_binary <- reactive(reclassify(max_cv_mask ,rcl= rcl_mat_current()))
    
    ### Distance to shore
    # Defining variable
    max_dist_shore <- reactive(input$dist_shore_slider*1852) # Conversion from nautical miles to meters
    
    # Read in file
    dist_shore <- raster("data/dist_shore.tif")
    
    # Reclassify matrix for distance to shore layer
    rcl_matrix_dist_shore <- reactive(c( -Inf, 0, 0,
                                0, max_dist_shore(), 1,
                                max_dist_shore(), Inf, 0))
    
    # Reclassify distance to shore layer
    dist_shore_binary <- reactive(reclassify(dist_shore, rcl = rcl_matrix_dist_shore()))
    
    ### DO
    # Defining variable
    min_DO_value <- reactive(input$min_DO_slider)
    
    # Read in file
    DO_min_mask <- raster("data/DO_min_mask.tif")
    
    # Reclassification matrix for DO
    rcl_matrix_DO <- reactive(c( -Inf, min_DO_value(), 0,
                        min_DO_value(), Inf, 1))
    
    # Reclassify DO min
    DO_min_binary <- reactive(reclassify(DO_min_mask, rcl = rcl_matrix_DO()))
    
    ########## Fixed Variables ############
    ### MPAs
    # Read in file
    mpas_binary <- reactive(raster(ifelse(1 %in% input$checkGroup,"data/mpas_binary.tif","data/raster_ones.tif" )))
    
    ### Reefs
    # Read in file
    reefs_binary <- reactive(raster(ifelse(2 %in% input$checkGroup,"data/reefs_binary.tif", "data/raster_ones.tif")))
    
    ### Artificial Reefs
    # Read in file
    reefs_artificial_binary <- reactive(raster(ifelse(3 %in% input$checkGroup,"data/reefs_artificial_binary.tif", "data/raster_ones.tif")))
    
    ### Oil pipelines
    # Read in file
    og_pipeline_binary <- reactive(raster(ifelse(4 %in% input$checkGroup,"data/og_pipeline_binary.tif", "data/raster_ones.tif")))
    
    ### Oil production
    # Read in file
    og_production_binary <- reactive(raster(ifelse(5 %in% input$checkGroup,"data/og_production_binary.tif", "data/raster_ones.tif")))
    
    ### Suitable areas (overlay of layers)
    suitable <- eventReactive( input$run_button, {
        overlay(sst_binary_min(), sst_binary_max(), depth_binary(), current_binary(), dist_shore_binary(), mpas_binary(), reefs_binary(), reefs_artificial_binary(), og_pipeline_binary(), og_production_binary(), fun = function(a, b, c, d, e, f, g, h, i, j){a*b*c*d*e*f*g*h*i*j})
    })
   
    ### Waiter
    # Create waiter spinner
    waiting_screen <- tagList(
        spin_flower(),
        h4("Loading map...")
    ) 
    
    # Show waiter after clicking run button
    observeEvent(input$run_button, {
        waiter_show(
            html = waiting_screen,
            color = "#222222"
        )
    })
    
    ### Area Message
    # Show message after clicking run button
    observeEvent(input$run_button, {
        
        # Area calculation
        area <- round(freq(suitable(), value = 1)*184.64, digits = 0)
        
        # Notification
        showNotification("Total suitable area:",
                         HTML(paste(area, " km", tags$sup(2), sep = "")),
                         type = "message", duration = NULL)
    })
    
    
    ### Render leaflet map
    output$suitableMap <- renderLeaflet({
        
        # Color palettes
        pal <- colorNumeric(c("#FFFFFF40", "#8B0000FF"), values(suitable()), na.color = "transparent", alpha = TRUE)
        
        on.exit(waiter_hide())
        
        # Leaflet map
        leaflet(options = leafletOptions( zoomSnap = 0.2)) %>%
            addTiles() %>%
            addRasterImage(suitable(), colors = pal) %>% 
            addScaleBar(position = "bottomright") %>%  # adds scale bar
           # fitBounds(lng1 = 4937645, # sets initial view of map to fit coordinates
              # lng2 = 8111405,
              # lat1 = 6030062,
              # lat2 = 10778162) %>% 
            fitBounds(lng1 = -54.6903404, # sets initial view of map to fit coordinates
                      lng2 = -25.835314,
                      lat1 = 6.3071255,
                      lat2 = -35.8573806) %>% 
            addEasyButton(easyButton(
                icon="fa-globe", title="Reset View", # button to reset to initial view
                 onClick=JS("function(btn, map){
                           map.setView([-14.0182737, -39.8789667]);
                           map.setZoom(4.6);}"))) %>% 
            addLegend(colors = c("#8B0000FF", "#FFFFFF40"), # adds legend
                      labels = c("Suitable", "Non-suitable"),
                      title = "Legend") %>% 
            addMouseCoordinates()
         })  
    
    ###
         ### Download map
        output$download_button <- downloadHandler(
        
        filename = function() {
             "suitability_map.tif"
        },
        content = function(file) {
            writeRaster( suitable(), file)
        
           
            })
}



# Run the application 
shinyApp(ui = ui, server = server)



#output$out <- renderText({

#  if(is.null(input$hover_coordinates)) {
#        "Mouse outside of map"
#  } else {
#   paste0("Lat: ", input$hover_coordinates[1], 
#            "\nLng: ", input$hover_coordinates[2])
# }
# })


### Download map
# onRender(
#"function(el,x){
#   this.on('mousemove', function(e) {
#     var lat = e.latlng.lat;
#      var lng = e.latlng.lng;
#     var coord = [lat, lng];
#    Shiny.onInputChange('hover_coordinates', coord)
#   });
#   this.on('mouseout', function(e) {

#observeEvent(input$map_click, {
    
   # cat(file=stderr(), "\n")
   # cat(file=stderr(), paste("observed map_click"), "\n")    
    
   # click <- input$map_click
   # my$long <- click$lng
 #   my$lat <- click$lat
