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
library(plotly)
library(shinyEventLogger) # Are we using this package?
library(leaflet.extras)
library(shinyjs)

# Source scripts
source("scripts/html.R")

#Species Dataframe 

# Data frame with coefficients for different species
species <- c("Atlantic salmon", "Gilthead seabream", "Rachycentron
canadum")
a1 <- c(0.0264, 0.026, 0.0714)
a2 <- c(-0.066, -0.0042, -0.1667)
b1 <- c(-0.0396, -0.0308, -1.5714)
b2 <- c(1.254, 0.1388, 5.3333)
T0 <- c(14, 25, 29)
Linf <- c(54.7, 140, 133.3)
time0 <- c(0, 0, -0.13)
a <- c(0, 0, 0.00479)
b <- c(0, 0, 3.11)

species_df <- data.frame(species, a1, a2, b1, b2, T0, Linf, time0, a, b)

# Set logging for history
set_logging()

# Define UI for application
ui <- fluidPage(
  
  # Add waiter dependencies
  use_waiter(),
  
  # Use previous and next buttons to navigate through the user guide
  useShinyjs(),
  
  # Set the number of pages for the user guide
  NUM_PAGES <- 5, 
  
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
             id = "navbar",
             selected = div(icon("map-pin"),"Site Suitability"),
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
             tabPanel(div(icon("map-pin"),"Site Suitability"),
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
             tabPanel( HTML('<div><i class="fa fa-chart-line"></i>Biomass</div>'),
                       sidebarLayout(
                         sidebarPanel(
                           tabsetPanel(type = "tabs",
                                       tabPanel( "Species",
                                                 radioButtons(inputId = "selectSpecies",
                                                              label = "Pick a species",
                                                              choiceNames = list(
                                                                HTML('<span>Atlantic salmon (<i>Salmo salar</i>)<br><img src="atlantic_salmon.png" alt=“image of salmon“ height="100px"/></span>'),
                                                                HTML('<span>gilthead seabream (<i>Sparus aurata</i>)<br><br><img src="seabream.png" alt=“image of salmon“ height="70px"/></span>'),
                                                                HTML('<span>cobia (<i>Rachycentron canadum</i>)<br><img src="cobia.png" alt=“image of salmon“  height="100px"/></span>')
                                                                ),
                                                              choiceValues = unique(species_df$species)) # Radio buttons sourced from scripts/html.R
                                       )),
                           actionButton("run_button_growth", label = "Run"),
                           downloadButton("download_button_growth", label = "Download")),
                         mainPanel(
                           leafletOutput("growthMap", height = "100vh")
                         )
                       )),
             #Fourth Tab
             tabPanel(HTML('<div><i class="fa fa-hand-holding-usd"></i>Economics</div>'),
                      sidebarLayout(
                        sidebarPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("Economic Factors",
                                               selectInput("sizetoharvest", label = h3("Size at Harvest (kg)"),
                                                           choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5),
                                                           selected = 1
                                               ),
                                               numericInput("stockingdensity", label = h3("Stocking Density (fish/m^3"),
                                                            min = 1,
                                                            max = 50,
                                                            step = 1,
                                                            value = 10), 
                                               bsTooltip(id = "stockingdensity",
                                                         title = "Desired density of adult fish at time of harvest (max 50)",
                                                         placement = "right",
                                                         trigger = "hover",
                                                         options = NULL),
                                               sliderInput("fingerlingprice", label = h3("Fingerling Price ($USD/fish)"),
                                                           min = .10,
                                                           max = 10.00,
                                                           step = .10,
                                                           value = 1.50),
                                               sliderInput("feedprice", label = h3("Feed Price ($USD/kg)"),
                                                           min = 100.00,
                                                           max = 5000.00,
                                                           step = 100.00,
                                                           value = 500.00),
                                               numericInput("numberofcages", label = h3("Number of Cages (6400m^3 each)"),
                                                            min = 1,
                                                            max = 32,
                                                            step = 1,
                                                            value = 16),
                                               bsTooltip(id = "numberofcages",
                                                         title = "Desired farm size (max 32 SeaStation cages)",
                                                         placement = "right",
                                                         trigger = "hover",
                                                         options = NULL),
                                      )),
                          actionButton("run_button_economics", label = "Run"),
                          downloadButton("download_button_economics", label = "Download")),
                        mainPanel(
                          leafletOutput("economics_map", height = "100vh")
                        )
                      )),
            
            
              
              # Fifth Tab
             tabPanel(HTML('<div><i class="fa fa-calculator"></i> Area Calculator</div>'),
                      fluidRow(
                        column(12,
                               plotlyOutput("barPlot"))
                      ),
                      fluidRow(
                        column(12,
                               tags$h3("Percentage of EEZ excluded"),
                               tabsetPanel(type = "tabs",
                                           id = "area_tabs",
                                           selected = "Run 1"
                                           
                               ))
                      )),
             
             # Sixth Tab
             tabPanel(div(icon("history"), "Run History"),
                      fluidRow(
                        column(12,
                               dataTableOutput("suitabilityTable"))
                      )),
             
             # Seventh Tab
             tabPanel(div(icon("book-open"),"User Guide"),
                      hidden(
                        lapply(seq(NUM_PAGES), function(ui) {
                          div(
                            class = "page",
                            id = paste0("step", ui),
                            "Step", ui
                          )
                        })
                      ),
                      br(),
                      actionButton("prevBtn", "< Previous"),
                      actionButton("nextBtn", "Next >")
             ),
             
             # Eight Tab
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

##########################################################################################
# Define server logic 
##########################################################################################
server <- function(input, output) {
  
  set_logging_session()
  
  ### Show/Hide tabs ###
  # Hide growth, economic, and area tabs when server starts
  hideTab(inputId = "navbar", target = HTML('<div><i class="fa fa-chart-line"></i>Biomass</div>'))
  hideTab(inputId = "navbar", target = HTML('<div><i class="fa fa-hand-holding-usd"></i>Economics</div>'))
  hideTab(inputId = "navbar", target = HTML('<div><i class="fa fa-calculator"></i> Area Calculator</div>'))
  
  # Show tabs after clicking the run button/ growth run button
  observeEvent(input$run_button, {
    showTab(inputId = "navbar", target = HTML('<div><i class="fa fa-chart-line"></i>Biomass</div>'))
  })
  observeEvent(input$run_button_growth, {
    showTab(inputId = "navbar", target = HTML('<div><i class="fa fa-hand-holding-usd"></i>Economics</div>'))
  })
  observeEvent(input$run_button, {
    showTab(inputId = "navbar", target = HTML('<div><i class="fa fa-calculator"></i> Area Calculator</div>'))
  })
  
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
  
  # Reclassification matrix for depth layer that makes unsuitable cells 1, suitable 2, and NAs 0
  rcl_mat_depth <- reactive(c(-Inf, max_depth(), 1,
                     max_depth(), min_depth(), 2,
                     min_depth(), 0, 1,
                     0, Inf, 0))
  
  # Reclassify the depth layer
  depth_binary_1 <- reactive(reclassify(depth_mask,rcl= rcl_mat_depth()))
  
  # Adding missing portion of EEZ using an raster cropped to the correct shape of the EEZ
  
  # Read in 1's raster cropped to EEZ
  eez_all_1 <- raster("data/eez_all_1.tif")
  
  # Overlay two layers
  depth_binary_2 <- reactive(overlay(depth_binary_1(), eez_all_1, fun = function(a, b) {a + b}))
  
  # Reclassification matrix to make depth layer binary
  rcl_mat_depth_2 <- c(-Inf, 2.1, 0,
                       2.9, 3.1, 1)
  
  # Reclassify the depth layer
  depth_binary <- reactive(reclassify(depth_binary_2(),rcl= rcl_mat_depth_2))
  
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
  
  # Reclassify the max current velocity layer
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
    overlay(sst_binary_min(),
            sst_binary_max(),
            depth_binary(),
            current_binary(),
            dist_shore_binary(),
            mpas_binary(),
            reefs_binary(),
            reefs_artificial_binary(),
            og_pipeline_binary(),
            og_production_binary(),
            shipping_lanes_binary(),
            fun = function(a, b, c, d, e, f, g, h, i, j, k){a*b*c*d*e*f*g*h*i*j*k})
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
  
  ### Area Calculator ###
  
  ### First Bar Graph ###
  
  # Calculate value for total area
  area <- reactive(
    round(freq(suitable(), value = 1)*123.424, digits = 0)
  )
  
  text_depth <- reactive(paste0(HTML("<b>Depth: </b>"), input$depth_slider[1], " - ", input$depth_slider[2], " m"))
  text_sst <-  reactive(paste0(HTML("<b>\nSST: </b>"), input$sst_slider[1], " - ", input$sst_slider[2], " °C"))
  text_do <- reactive(paste0(HTML("<b>\nDissolved Oxygen:  </b>"), input$min_DO_slider, HTML("mol/m<sup>3</sup>")))
  text_max_cv <- reactive(paste0(HTML("<b>\nMax Current Velocity:  </b>"), input$max_cv_slider, " m/s"))
  text_max_swh <- reactive(paste0(HTML("<b>\nMax. Sig. Wave Height:  </b>"), "m"))
  text_dist_shore <- reactive(paste0(HTML("<b>\nDistance to Shore:  </b>"), input$dist_shore_slider, " NM"))
  text_mpas <- reactive(paste0(HTML("<b>\nMPAS:  </b>"), input$checkGroup[1] == 1))
  text_reefs <- reactive(paste0(HTML("<b>\nReefs:  </b>"), input$checkGroup[2] == 2))
  text_a_reefs <- reactive(paste0(HTML("<b>\nArtificial Reefs:  </b>"), input$checkGroup[3] == 3))
  text_oil_pipe <- reactive(paste0(HTML("<b>\nOil Pipelines:  </b>"), input$checkGroup[4] == 4))
  text_oil_prod <- reactive(paste0(HTML("<b>\nOil Production:  </b>"), input$checkGroup[5] == 5))
  text_shipping <- reactive(paste0(HTML("<b>\nShipping Lanes:  </b>"), input$checkGroup[6] == 6))
  
  # Create text for inputs
  inputs_suitability <- reactive(
    paste0(text_depth(),
           text_sst(),
           text_do(),
           text_max_cv(),
           text_max_swh(),
           text_dist_shore(),
           text_mpas(),
           text_reefs(),
           text_a_reefs(),
           text_oil_pipe(),
           text_oil_prod(),
           text_shipping())
  )
  
  # Keep track of run number
  run_number <- reactive(
    paste0("Run ", input$run_button)
  )
  
  # Create empty reactive values
  values <- reactiveValues()
  values$x <- vector()
  values$y <- vector()
  values$text<- vector()
  
  # Add new run # and area value to x and y vectors every time the run button is clicked
  observeEvent(input$run_button, {
    values$x <- append(values$x, run_number() , input$run_button-1)
    values$y <- append(values$y, area(), input$run_button-1)
    values$text <- append(values$text, inputs_suitability(), input$run_button-1)
  }
  )
  
  # Render plotly bar chart
  output$barPlot <- renderPlotly(
    output$barPlot <- renderPlotly(
      p <- plot_ly(
        x = values$x,
        y = values$y,
        text = values$text,
        type = "bar")%>%
        layout(title = "Total Suitable Area by Run Number",
               yaxis = list(title = HTML("Area (km<sup>2</sup>)")))
    )
  )
  
  ### Second Bar Graph ### 
  
  # Create bar graph after clicking run button
  observeEvent(input$run_button, {
    if ( input$run_button == 1) {
      prependTab(
        inputId = "area_tabs",
        tabPanel( "Run 1", 
                  plotOutput("excludedPlot"))
      )
    } else {
      insertTab(inputId = "area_tabs",
                tabPanel(run_number(),
                         plotOutput("excludedPlot"),
                         target = paste0("Run ", input$run_button-1)
                ))
    }
  })
  
  # Create vector with input names
  input_names <- c("Min SST",
                   "Max SST",
                   "Depth",
                   "Current Velocity",
                   "Distance to Shore",
                   "Dissolved Oxygen",
                   "MPAs",
                   "Reefs",
                   "Artificial Reefs",
                   "Oil Pipelines",
                   "Oil Production",
                   "Shipping Lanes")
  
  
  # Create vector with number of 0 cells in each binary raster
  freq_0 <- reactive(c(
    freq(sst_binary_min(), value = 0),
    freq(sst_binary_max(), value = 0),
    freq(depth_binary(), value = 0),
    freq(current_binary(), value = 0),
    freq(dist_shore_binary(), value = 0),
    freq(DO_min_binary(), value = 0),
    freq(mpas_binary(), value = 0),
    freq(reefs_binary(), value = 0),
    freq(reefs_artificial_binary(), value = 0),
    freq(og_pipeline_binary(), value = 0),
    freq(og_production_binary(), value = 0),
    freq(shipping_lanes_binary(), value = 0)
  ))
  
  # Create df with layer names, frequency of 0 cells, and percentage of excluded area
  area_df <- reactive(
    data.frame(input_names, freq_0()) %>% 
      mutate(percent_excluded = round(freq_0()*100/30959, digits = 2)) %>% 
      arrange(percent_excluded) %>% 
      mutate( input_names = factor(input_names, levels = input_names))
  )
  
  # Render excluded areas plot
  output$excludedPlot <- renderPlot(
    ggplot(area_df(), aes(x = input_names, y = percent_excluded)) +
      geom_col(fill = "darkturquoise") +
      coord_flip() +
      ylab("Percentage of EEZ Excluded") +
      xlab("") +
      scale_y_continuous( expand = c(0,0)) +
      theme_classic(14) +
      geom_text(
        aes(label = paste0(percent_excluded,"%"), y = percent_excluded + 15), 
        color = "black", 
        size = 5,
        hjust = 1
      )
  )
  
  ### Render leaflet map
  output$suitableMap <- renderLeaflet({
    
    # Color palettes
    pal <- colorNumeric(c("#FFFFFF40", "#1D63A3"), values(suitable()), na.color = "transparent", alpha = TRUE)
    
    on.exit(waiter_hide())
    
    # Leaflet map
    leaflet(options = leafletOptions( zoomSnap = 0.2)) %>%
      addTiles(group = "Open Street Map") %>%
      addProviderTiles("Esri.WorldGrayCanvas", group = "Esri Gray Canvas (default)") %>%
      addRasterImage(suitable(),
                     method = "ngb",
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
      addFullscreenControl() %>% 
      addLayersControl(
        baseGroups = c("Esri Gray Canvas (default)", "Open Street Map"),
        overlayGroups = "Suitable Areas",
        options = layersControlOptions(collapsed = TRUE),
        position = "topleft") %>% 
      addLegend(colors = c("#1D63A3", "#FFFFFF40"), # adds legend
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
  fish_selection <-  reactive({
    species_df %>% 
      filter(species == input$selectSpecies)
  })  

  
  # Separete cells into cells above and below optimal SST
  cells_below_optimal <- reactive(
    suitable_sst() < fish_selection()$T0
  )
  
  cells_above_optimal <-  reactive(
    suitable_sst() >= fish_selection()$T0
  )
  
  # Apply growth equations
  growth_below_optimal <- reactive(
    fish_selection()$a1*cells_below_optimal()*suitable_sst() + fish_selection()$b1*cells_below_optimal()
  )
  growth_above_optimal <- reactive(
    fish_selection()$a2*cells_above_optimal()*suitable_sst() + fish_selection()$b2*cells_above_optimal()
  )
  
  # Add both rasters
  growth_raster <- eventReactive( input$run_button_growth,
                                  growth_above_optimal() + growth_below_optimal()
  )
  
  # Von Bertallanfy 
  von_raster <- reactive(fish_selection()$Linf*(1 - exp((-1*12*(growth_raster()))*(1-fish_selection()$time0))))
  
  #Allometric Ratio 
  weight_raster <- reactive((fish_selection()$a*von_raster()^fish_selection()$b)*0.001*261120)
  
  # Render growth plot
  output$growthMap <- renderLeaflet({
    # Palette
    pal_growth <- colorNumeric(c("#DAF7A6", "#C70039", "#581845"), values(weight_raster()),
                               na.color = "transparent")
    
    # Leaflet map
    leaflet(options = leafletOptions( zoomSnap = 0.2)) %>%
      addTiles(group = "Open Street Map") %>%
      addProviderTiles("Esri.WorldGrayCanvas", group = "Esri Gray Canvas (default)") %>%
      addRasterImage(weight_raster(),
                     colors = pal_growth,
                     group = "Growth Model") %>%
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
      addLegend("topright",
                pal = pal_growth,
                values = values(weight_raster()),
                title = "Fish Biomass (kg/cell)") %>% 
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
  
  header <- c("Input", "Value", "Unit")
  sst_table <- reactive(c("SST", paste0(input$sst_slider[1], "-" , input$sst_slider[2]), "°C"))
  suitability_df <- data.frame(header)
  
  output$suitabilityTable <- renderDataTable(
    suitability_df 
    
  )
  
  rv <- reactiveValues(page = 1)
  
  observe({
    toggleState(id = "prevBtn", condition = rv$page > 1)
    toggleState(id = "nextBtn", condition = rv$page < NUM_PAGES)
    hide(selector = ".page")
    show(paste0("step", rv$page))
  })
  
  navPage <- function(direction) {
    
    rv$page <- rv$page + direction
  }
  
  observeEvent(input$prevBtn, navPage(-1))
  observeEvent(input$nextBtn, navPage(1))
  
  
}







# Run the application 
shinyApp(ui = ui, server = server)
