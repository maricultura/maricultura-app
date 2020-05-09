
######################################################################################
# Module UI function
######################################################################################
biomassProdUI <- function(id){
  
  # Create a namespace function using the provided id
  ns = NS(id)
  
  # Create biomass tab
  tabPanel( HTML('<div><i class="fa fa-chart-line"></i>Biomass</div>'),
            sidebarLayout(
              sidebarPanel(
                tabsetPanel(type = "tabs",
                            tabPanel( "Species",
                                      radioButtons(inputId = ns("selectSpecies"),
                                                   label = h4("Select a species:"),
                                                   choiceNames = list(
                                                     HTML('<span><h5>Atlantic salmon (<i>Salmo salar</i>)</h5><img src="atlantic_salmon.png" alt=“image of salmon“ height="100px"/></span>'),
                                                     HTML('<span><h5>gilthead seabream (<i>Sparus aurata</i>)</h5><img src="seabream.png" alt=“image of seabream“ height="70px"/></span>'),
                                                     HTML('<span><h5>cobia (<i>Rachycentron canadum</i>)</h5><img src="cobia.png" alt=“image of cobia“  height="100px"/></span>')
                                                   ),
                                                   choiceValues = unique(species_df$species),
                                                   selected =  HTML('<span>cobia (<i>Rachycentron canadum</i>)<br><img src="cobia.png" alt=“image of salmon“  height="100px"/></span>'))),
                            tabPanel("Farm",
                                      h4("Select value(s):"),
                                      numericInput(ns("stockingdensity"), label = HTML("<h5>Initial Stocking Density (fish/m<sup>3</sup>)</h5>"),
                                                   min = 1,
                                                   max = 50,
                                                   step = 1,
                                                   value = 3), 
                                      bsTooltip(id = ns("stockingdensity"),
                                                title = "Desired density of fingerlings to stock farm",
                                                placement = "right",
                                                trigger = "hover",
                                                options = NULL),
                                      numericInput(ns("numberofcages"),
                                                   label = HTML("<h5>Number of Cages (6400m<sup>3</sup> each)</h5>"),
                                                                              min = 1,
                                                                              max = 32,
                                                                              step = 1,
                                                                              value = 16),
                                      bsTooltip(id = ns("numberofcages"),
                                                title = "Desired farm size (max 32 SeaStation cages)",
                                                placement = "right",
                                                trigger = "hover",
                                                options = NULL)
                                      # Radio buttons sourced from scripts/html.R
                            )),
                actionButton(ns("run_button_growth"), label = "Run"),
                downloadButton(ns("download_button_growth"), label = "Download")),
              mainPanel(
                leafletOutput(ns("growthMap"), height = "100vh")
              )
            ))
}
  
  
######################################################################################
# Module server function
######################################################################################
biomassProd <- function(input, output, session, site_suitability, r) {
  
# Keep track of clicks on run button
  observeEvent(input$run_button_growth, {
    r$run_num_growth <- input$run_button_growth})
  
# Read saved raster
mean_sst_mask <- raster("data/mean_sst_mask.tif")

# Overlay suitable raster and mean SST and reclassify
suitable_sst_0 <- reactive(
  overlay(mean_sst_mask, site_suitability$suit(), fun = function(x, y) {x * y})
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

growth_rate <- reactive(
  fish_selection()$A_omega*suitable_sst() + fish_selection()$B_omega
)

#Stocking density and number of cages for biomass 
stockingdensity <- reactive(input$stockingdensity)
numberofcages <- reactive(input$numberofcages)  
cage_size <- 6400 #m^3
survival_rate <- 0.85 

# Growth raster 
growth_raster <- eventReactive(input$run_button_growth,
                               (growth_rate()/6.066)*6.066*(stockingdensity()*numberofcages()*cage_size)*survival_rate/1000) # dividing by 1000 to convert from kg to MT

### Waiter
# Create waiter spinner
waiting_screen <- tagList(spin_flower(),
                          h4("Loading map..."))

# Show waiter after clicking run button
observeEvent(input$run_button_growth, {
  waiter_show(html = waiting_screen,
              color = "#222222")
})

# Render growth plot
output$growthMap <- renderLeaflet({
  # Palette
  pal_growth <- colorNumeric(c("#3a1858", "#c700c4", "#f4f7a6"),
                             values(growth_raster()),
                             na.color = "transparent")
  # Reverse palette
  rev_pal_growth <- colorNumeric(c("#3a1858", "#c700c4", "#f4f7a6"),
                                               values(growth_raster()),
                                               na.color = "transparent",
                                 reverse = TRUE)
  
  # Hide waiter after map is rendered
  on.exit(waiter_hide())
  
  # Leaflet map
  leaflet(options = leafletOptions( zoomSnap = 0.2)) %>%
    addTiles(group = "Open Street Map") %>%
    addProviderTiles("Esri.WorldGrayCanvas", group = "Esri Gray Canvas (default)") %>%
    addRasterImage(growth_raster(),
                   colors = rev_pal_growth,
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
              values = values(growth_raster()),
              title = "Fish Biomass (MT/cell)",
              labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>% 
    addMouseCoordinates()
}
)

### Modal Dialogue ###
observeEvent(input$run_button_growth, 
             showModal(modal3()), once = TRUE)

### Download growth map
output$download_button_growth <- downloadHandler(
  
  filename = function() {
    "growth_map.tif"
  },
  content = function(file) {
    writeRaster(growth_raster(), file)
    
  })

# Return list with parameters needed by other module functions
return(list(
  growth_r = reactive(growth_raster()),
  cage_num = reactive(numberofcages()),
  stocking_d = reactive(stockingdensity()),
  cage_size = cage_size
))

}
