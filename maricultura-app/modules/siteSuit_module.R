
######################################################################################
# Module UI function
######################################################################################
siteSuitUI <- function(id){
  ns = NS(id)
  tabPanel(div(icon("map-pin"),"Site Suitability"),
           sidebarLayout(
             sidebarPanel(
               tabsetPanel(type = "tabs",
                           tabPanel( "Variable",
                                     sliderInput(ns("sst_slider"),
                                                 label = h5("Sea Surface Temperature (°C)"),
                                                 min = 0, 
                                                 max = 40,
                                                 value = c(22,32)),
                                     bsTooltip(id = ns("sst_slider"),
                                               title = "Temperature range for fish survival",
                                               placement = "right",
                                               trigger = "hover",
                                               options = NULL),
                                     sliderInput(ns("depth_slider"),
                                                 label = h5("Depth (m)"),
                                                 min = 0, 
                                                 max = 200,
                                                 value = c(25, 100)),
                                     bsTooltip(id = ns("depth_slider"),
                                               title = "Cage manufacturer depth specifications",
                                               placement = "right",
                                               trigger = "hover",
                                               options = NULL),
                                     numericInput(ns("min_DO_slider"),
                                                  label = HTML("<h5>Minimum Dissolved Oxygen (mol/m<sup>3</sup>)</h5>"),
                                                  min = 0,   
                                                  max = 400,
                                                  step = 0.5,
                                                  value = 70.5),
                                     numericInput(ns("max_cv_slider"), label = h5("Maximum Current Velocity (m/s)"),
                                                  min = 0,
                                                  max = 3,
                                                  step = 0.1,
                                                  value = 1),
                                     numericInput(ns("max_swh_slider"), label = h5("Maximum Significant Wave Height (m)"),
                                                  min = 0,
                                                  max = 15,
                                                  step = 0.1,
                                                  value = 9),
                                     numericInput(ns("dist_shore_slider"), label = h5("Maximum Distance to Shore (NM)"),
                                                  min = 0,
                                                  max = 200,
                                                  step = 0.5,
                                                  value = 25)),
                           tabPanel( "Fixed",
                                     checkboxGroupInput(ns("checkGroup"), label = h3("Select Barrier(s)"), 
                                                        choices = list("MPAs" = 1,
                                                                       "Reefs" = 2,
                                                                       "Artificial Reefs" = 3,
                                                                       "Oil Pipelines" = 4,
                                                                       "Oil Production" = 5,
                                                                       "Shipping lanes" = 6),
                                                        selected = c(1, 2, 3, 4, 5, 6))
                           )),
               actionButton(ns("run_button"), label = "Run"),
               downloadButton(ns("download_button"), label = "Download")
             ),
             mainPanel(
               leafletOutput(ns("suitableMap"), height = "100vh")
             )
             
           )
  )
}


######################################################################################
# Module server function
######################################################################################

  siteSuit <- function(input, output, session, r) {
    
    r$mod1 <- reactiveValues()
    
    observeEvent(input$run_button, {
      r$mod1$run_num <- input$run_button})
    
    ### Depth
    # Defining variables
    min_depth <- reactive(-input$depth_slider[1])
    max_depth <- reactive(-input$depth_slider[2])
    
    # Read in file
    depth_mask <- raster("data/depth_mask.tif")
    
    # Reclassification matrix for depth layer that makes unsuitable cells 1, suitable 2, and NAs 0
    rcl_mat_depth <- reactive(c(
      -Inf,
      max_depth(),
      1,
      max_depth(),
      min_depth(),
      2,
      min_depth(),
      0,
      1,
      0,
      Inf,
      0
    ))
    
    # Reclassify the depth layer
    depth_binary_1 <-
      reactive(reclassify(depth_mask, rcl = rcl_mat_depth()))
    
    # Adding missing portion of EEZ using an raster cropped to the correct shape of the EEZ
    
    # Read in 1's raster cropped to EEZ
    eez_all_1 <- raster("data/eez_all_1.tif")
    
    # Overlay two layers
    depth_binary_2 <-
      reactive(overlay(
        depth_binary_1(),
        eez_all_1,
        fun = function(a, b) {
          a + b
        }
      ))
    
    # Reclassification matrix to make depth layer binary
    rcl_mat_depth_2 <- c(-Inf, 2.1, 0,
                         2.9, 3.1, 1)
    
    # Reclassify the depth layer
    depth_binary <-
      reactive(reclassify(depth_binary_2(), rcl = rcl_mat_depth_2))
    
    ### Min SST
    # Defining variable
    min_sst_value <- reactive(input$sst_slider[1])
    
    # Read in file
    min_sst_mask <- raster("data/min_sst_mask.tif")
    
    # Reclassification matrix for min SST
    rcl_matrix_min <- reactive(c(-Inf, min_sst_value(), 0,
                                 min_sst_value(), Inf, 1))
    
    # Reclassify min SST
    sst_binary_min <-
      reactive(reclassify(min_sst_mask, rcl = rcl_matrix_min()))
    
    
    ### Max SST
    # Defining variable
    max_sst_value <- reactive(input$sst_slider[2])
    
    # Read in file
    max_sst_mask <- raster("data/max_sst_mask.tif")
    
    # Reclassify matrix for max SST layer
    rcl_matrix_max <- reactive(c(-Inf, max_sst_value(), 1,
                                 max_sst_value(), Inf, 0))
    
    # Reclassify max SST layer
    sst_binary_max <-
      reactive(reclassify(max_sst_mask, rcl = rcl_matrix_max()))
    
    ### Max Current Velocity
    # Defining variable
    max_cv_value <- reactive(input$max_cv_slider)
    
    # Read in file
    max_cv_mask <- raster("data/max_cv_mask.tif")
    
    # Reclassification matrix for current layer
    rcl_mat_current <- reactive(c(-Inf, max_cv_value(), 1,
                                  max_cv_value(), Inf, 0))
    
    # Reclassify the max current velocity layer
    current_binary <-
      reactive(reclassify(max_cv_mask , rcl = rcl_mat_current()))
    
    ### Distance to shore
    # Defining variable
    max_dist_shore <-
      reactive(input$dist_shore_slider * 1852) # Conversion from nautical miles to meters
    
    # Read in file
    dist_shore <- raster("data/dist_shore.tif")
    
    # Reclassify matrix for distance to shore layer
    rcl_matrix_dist_shore <- reactive(c(-Inf, 0, 0,
                                        0, max_dist_shore(), 1,
                                        max_dist_shore(), Inf, 0))
    
    # Reclassify distance to shore layer
    dist_shore_binary <-
      reactive(reclassify(dist_shore, rcl = rcl_matrix_dist_shore()))
    
    ### DO
    # Defining variable
    min_DO_value <- reactive(input$min_DO_slider)
    
    # Read in file
    DO_min_mask <- raster("data/DO_min_mask.tif")
    
    # Reclassification matrix for DO
    rcl_matrix_DO <- reactive(c(-Inf, min_DO_value(), 0,
                                min_DO_value(), Inf, 1))
    
    # Reclassify DO min
    DO_min_binary <-
      reactive(reclassify(DO_min_mask, rcl = rcl_matrix_DO()))
    
    ########## Fixed Variables ############
    ### MPAs
    # Read in file
    mpas_binary <-
      reactive(raster(
        ifelse(
          1 %in% input$checkGroup,
          "data/mpas_binary.tif",
          "data/raster_ones.tif"
        )
      ))
    
    ### Reefs
    # Read in file
    reefs_binary <-
      reactive(raster(
        ifelse(
          2 %in% input$checkGroup,
          "data/reefs_binary.tif",
          "data/raster_ones.tif"
        )
      ))
    
    ### Artificial Reefs
    # Read in file
    reefs_artificial_binary <-
      reactive(raster(
        ifelse(
          3 %in% input$checkGroup,
          "data/reefs_artificial_binary.tif",
          "data/raster_ones.tif"
        )
      ))
    
    ### Oil pipelines
    # Read in file
    og_pipeline_binary <-
      reactive(raster(
        ifelse(
          4 %in% input$checkGroup,
          "data/og_pipeline_binary.tif",
          "data/raster_ones.tif"
        )
      ))
    
    ### Oil production
    # Read in file
    og_production_binary <-
      reactive(raster(
        ifelse(
          5 %in% input$checkGroup,
          "data/og_production_binary.tif",
          "data/raster_ones.tif"
        )
      ))
    
    
    #### Shipping Lanes
    # Read in File
    shipping_lanes_binary <-
      reactive(raster(
        ifelse(
          6 %in% input$checkGroup,
          "data/shipping_binary.tif",
          "data/raster_ones.tif"
        )
      ))
    
    ### Suitable areas (overlay of layers)
    suitable <- eventReactive(input$run_button, {
      overlay(
        sst_binary_min(),
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
        fun = function(a, b, c, d, e, f, g, h, i, j, k) {
          a * b * c * d * e * f * g * h * i * j * k
        }
      )
    })

    
    ### Waiter
    # Create waiter spinner
    waiting_screen <- tagList(spin_flower(),
                              h4("Loading map..."))
    
    # Show waiter after clicking run button
    observeEvent(input$run_button, {
      waiter_show(html = waiting_screen,
                  color = "#222222")
    })
    
    ### Render leaflet map
    output$suitableMap <- renderLeaflet({
      # Color palettes
      pal <-
        colorNumeric(
          c("#FFFFFF40", "#1D63A3"),
          values(suitable()),
          na.color = "transparent",
          alpha = TRUE
        )
      
      on.exit(waiter_hide())
      
      # Leaflet map
      leaflet(options = leafletOptions(zoomSnap = 0.2)) %>%
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
        fitBounds(
          lng1 = -54.6903404,
          # sets initial view of map to fit coordinates
          lng2 = -25.835314,
          lat1 = 6.3071255,
          lat2 = -35.8573806
        ) %>%
        addEasyButton(easyButton(
          icon = "fa-globe",
          title = "Reset View",
          # button to reset to initial view
          onClick = JS(
            "function(btn, map){
                   map.setView([-14.0182737, -39.8789667]);
                   map.setZoom(4.6);}"
          )
        )) %>%
        addFullscreenControl() %>%
        addLayersControl(
          baseGroups = c("Esri Gray Canvas (default)", "Open Street Map"),
          overlayGroups = "Suitable Areas",
          options = layersControlOptions(collapsed = TRUE),
          position = "topleft"
        ) %>%
        addLegend(
          colors = c("#1D63A3", "#FFFFFF40"),
          # adds legend
          labels = c("Suitable Areas", "Exclusive Economic Zone"),
          title = "Legend"
        ) %>%
        addMouseCoordinates()
    })
    
    ### Download suitability map  ###
    output$download_button <- downloadHandler(
      
      filename = function() {
        "suitability_map.tif"
      },
      content = function(file) {
        writeRaster( suitable(), file)
        
        
      })
    
    ### Steps needed for the area graph
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
  
    # Return list with parameters needed by other module functions   
   return(list(
        suit = reactive(suitable()),
        dist_shore = reactive(dist_shore),
        values_x = reactive(values$x),
        values_y = reactive(values$y),
        values_text = reactive(values$text)
      ))
    
}
  
  
 


