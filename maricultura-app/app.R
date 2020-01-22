# Load required libraries

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
library(shinydashboard)
library(rgdal)
library(shinyBS)

# Source scripts
source("scripts/html.R")

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
               selected = div(icon("map-pin"),"Suitability"),
               # Do not need title for Navigation Bar
               # First tab
               tabPanel(div(icon("info-circle"),"About"),
                        h1("The Project",
                           class = "font-weight-light text-white",
                           class = "bg-primary text-center py-5 mb-4"),
                       test,
                        h1("Meet the Creators",
                           class = "font-weight-light text-white",
                           class = "bg-primary text-center py-5 mb-4"),
                        p(
                            wellPanel( style = "padding = 15",
                            img(src = "bartlett-kirby_orig.jpg", height = 300, width = 225), 
                            img(src = "bartlett-kirby_orig.jpg", height = 300, width   = 225),
                            img(src = "bartlett-kirby_orig.jpg", height = 300, width = 225), 
                            img(src = "bartlett-kirby_orig.jpg", height = 300, width   = 225),
                            img(src = "bartlett-kirby_orig.jpg", height = 300, width = 225)
                            
                            ),
                       
                       
                       
                       
                        "MARICULTURA is a team of 5 bright graduate students studying at the Bren School of Environmental Science & Management at the University of California Santa Barbara.  A partnership with the World Wildlife Fund has enabled the team to shape marine aquaculture planning off the coast of Brazil...... 
                           
                           
                           Visit our",
                           a( href = "https://maricultura.weebly.com", "website"),
                           "for more information."),
                        div(class = "container")
                        ),

               
               # Second tab
               tabPanel(div(icon("map-pin"),"Suitability"),
                        sidebarLayout(
                            sidebarPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel( "Variable",
                                                      sliderInput("sst_slider",
                                                                  label = h5("Sea Surface Temperature (°C)"),
                                                                  min = 0, 
                                                                  max = 40,
                                                                  value = c(22,32)),
                                                      bsTooltip(id = "sst_slider",
                                                                title = "Temperature range for fish survival",
                                                                placement = "right",
                                                                trigger = "hover",
                                                                options = NULL),
                                                      sliderInput("depth_slider",
                                                                  label = h5("Depth (m)"),
                                                                  min = 0, 
                                                                  max = 200,
                                                                  value = c(25, 100)),
                                                      bsTooltip(id = "depth_slider",
                                                                title = "Cage manufacturer depth specifications",
                                                                placement = "right",
                                                                trigger = "hover",
                                                                options = NULL),
                                                      numericInput("min_DO_slider",
                                                                   label = HTML("<h5>Minimum Dissolved Oxygen (mol/m<sup>3</sup>)</h5>"),
                                                                   min = 0,   
                                                                   max = 400,
                                                                   step = 0.5,
                                                                   value = 200),
                                                      numericInput("max_cv_slider", label = h5("Maximum Current Velocity (m/s)"),
                                                                   min = 0,
                                                                   max = 3,
                                                                  step = 0.1,
                                                                  value = 1),
                                                      numericInput("max_swh_slider", label = h5("Maximum Significant Wave Height (m)"),
                                                                   min = 0,
                                                                   max = 15,
                                                                   step = 0.1,
                                                                   value = 9),
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
               tabPanel(div(icon("chart-line"),"Growth/Econ"),
               sidebarLayout(
                   sidebarPanel(
                     tabsetPanel(type = "tabs",
                       tabPanel( "Species",
                                 fish_radiobuttons # Radio buttons sourced from scripts/html.R
                                ),
                       tabPanel("Economic Factors",
                                numericInput("sizetoharvest", label = h3("Size at Harvest (kg)"),
                                             min = 0,
                                             max = 5,
                                             step = 0.5,
                                             value = 3),
                                numericInput("stockingdensity", label = h3("Stocking Density (fish/m^3"),
                                             min = 1,
                                             max = 50,
                                             step = 1,
                                             value = 10), 
                                numericInput("fingerlingprice", label = h3("Fingerling Price ($USD/fish"),
                                             min = .10,
                                             max = 10.00,
                                             step = .10,
                                             value = 1.50),
                                numericInput("feedprice", label = h3("Feed Price ($USD/kg)"),
                                             min = 1.00,
                                             max = 10000.00,
                                             step = 1.00,
                                             value = 500.00),
                                numericInput("numberofcages", label = h3("Number of Cages (6400m^3 each)"),
                                             min = 1,
                                             max = 32,
                                             step = 1,
                                             value = 16)
                                             )),
                     actionButton("run_button_growth", label = "Run"),
                     downloadButton("download_button_growth", label = "Download")),
                   mainPanel(
                       leafletOutput("growthMap", height = "100vh")
                   )
                   )),
               
               # Fourth Tab
               
               tabPanel(div(icon("book-open"),"User Guide" )),
            
                # Fifth Tab
                tabPanel(div(icon("table"),"Metadata"),
                         tableOutput("metadataTable"))
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
    
    
    #### Shipping Lanes 
    # Read in File
    shipping_lanes_binary <- reactive(raster(ifelse(6 %in% input$checkGroup, "data/shipping_mask.tif", "data/raster_ones.tif")))
    
    ### Suitable areas (overlay of layers)
    suitable <- eventReactive( input$run_button, {
        overlay(sst_binary_min(), sst_binary_max(), depth_binary(), current_binary(), dist_shore_binary(), mpas_binary(), reefs_binary(), reefs_artificial_binary(), og_pipeline_binary(), og_production_binary(), shipping_lanes_binary(), fun = function(a, b, c, d, e, f, g, h, i, j, k){a*b*c*d*e*f*g*h*i*j*k})
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
        pal <- colorNumeric(c("#FFFFFF40", "blue"), values(suitable()), na.color = "transparent", alpha = TRUE)
        
        on.exit(waiter_hide())
        
        # Leaflet map
        leaflet(options = leafletOptions( zoomSnap = 0.2)) %>%
            addTiles(group = "Open Street Map") %>%
          addProviderTiles("Esri.WorldGrayCanvas", group = "Esri Gray Canvas (default)") %>%
            addRasterImage(suitable(),
                           colors = pal,
                           group = "Suitable Areas") %>% 
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
          addLayersControl(
            baseGroups = c("Esri Gray Canvas (default)", "Open Street Map"),
            overlayGroups = "Suitable Areas",
            options = layersControlOptions(collapsed = TRUE),
            position = "topleft") %>% 
            addLegend(colors = c("blue", "#FFFFFF40"), # adds legend
                      labels = c("Suitable Areas", "Exclusive Economic Zone"),
                      title = "Legend") %>% 
            addMouseCoordinates() 
         })  
    
    ###
         ### Download suitability map
        output$download_button <- downloadHandler(
        
        filename = function() {
             "suitability_map.tif"
        },
        content = function(file) {
            writeRaster( suitable(), file)
        
           
            })
        
        ### Metadata table
        output$metadataTable <- renderTable({
            
            # Read csv file
            metadata <- read_csv("data/metadata.csv")
            metadata
            
        }, width = "100vw")
        
        
        ### Growth Model
    # Read saved raster
    mean_sst_mask <- raster("data/mean_sst_mask.tif")
    
    
    # Overlay suitable raster and mean SST and reclassify
    suitable_sst_0 <- reactive(
        overlay(mean_sst_mask, suitable(), fun = function(x, y) {x * y})
    )
    
    # Convert zero values in raster into NAs, otherwise all background would be less than T0
    suitable_sst <- reactive(
        reclassify(suitable_sst_0(), cbind(0, NA))
    )
    

# Set reactive values
    
    
   # Data frame with coefficients for different species
    species <- c("Atlantic salmon", "Gilthead seabream", "cobia")
    a1 <- c(0.0264, 0.026, 0.0714)
    a2 <- c(-0.066, -0.0042, -0.1667)
    b1 <- c(-0.0396, -0.0308, -1.5714)
    b2 <- c(1.254, 0.1388, 5.3333)
    T0 <- c(14, 25, 29)
    
    species_df <- data.frame(species, a1, a2, b1, b2, T0)

    
    # Separete cells into cells above and below optimal SST
    cells_below_optimal <- reactive(
        suitable_sst() < values$T0
        )
    
    cells_above_optimal <-  reactive(
        suitable_sst() > values$T0
    )
    
    # Apply growth equations
    growth_below_optimal <- reactive(
        values$a1*cells_below_optimal()*suitable_sst() - values$b1*cells_below_optimal()
    )
    growth_above_optimal <- reactive(
        values$a2*cells_above_optimal()*suitable_sst() + values$b2*cells_above_optimal()
    )
    
    # Add both rasters
    growth_raster <- eventReactive( input$run_button_growth,
        growth_above_optimal() + growth_below_optimal()
    )
    
    
    # Render growth plot
    output$growthMap <- renderLeaflet({
        # Palette
        pal_growth <- colorNumeric(c("#DAF7A6", "#C70039", "#581845"), values(growth_raster()),
                                   na.color = "transparent")
            
        # Leaflet map
        leaflet(options = leafletOptions( zoomSnap = 0.2)) %>%
            addTiles() %>%
            addRasterImage(growth_raster(), colors = pal_growth) %>%
            fitBounds(lng1 = -54.6903404, # sets initial view of map to fit coordinates
                      lng2 = -25.835314,
                      lat1 = 6.3071255,
                      lat2 = -35.8573806) %>% 
            addEasyButton(easyButton(
                icon="fa-globe", title="Reset View", # button to reset to initial view
                onClick=JS("function(btn, map){
                           map.setView([-14.0182737, -39.8789667]);
                           map.setZoom(4.6);}"))) %>% 
            addLegend("topright",
                      pal = pal_growth,
                      values = values(growth_raster()),
                      title = "Somatic growth (kg/month)") %>% 
            addMouseCoordinates()
        
        }
    )
    
    ### Download suitability map
    output$download_button_growth <- downloadHandler(
        
        filename = function() {
            "growth_map.tif"
        },
        content = function(file) {
            writeRaster(growth_raster(), file)
            
            
        })

}


        

      


# Run the application 
shinyApp(ui = ui, server = server)