
# Module UI function
biomassProdUI <- function(id){
  ns = NS(id)
  
  tabPanel( HTML('<div><i class="fa fa-chart-line"></i>Biomass</div>'),
            sidebarLayout(
              sidebarPanel(
                tabsetPanel(type = "tabs",
                            tabPanel( "Species",
                                      radioButtons(inputId = ns("selectSpecies"),
                                                   label = "Pick a species",
                                                   choiceNames = list(
                                                     HTML('<span>Atlantic salmon (<i>Salmo salar</i>)<br><img src="atlantic_salmon.png" alt=“image of salmon“ height="100px"/></span>'),
                                                     HTML('<span>gilthead seabream (<i>Sparus aurata</i>)<br><br><img src="seabream.png" alt=“image of salmon“ height="70px"/></span>'),
                                                     HTML('<span>cobia (<i>Rachycentron canadum</i>)<br><img src="cobia.png" alt=“image of salmon“  height="100px"/></span>')
                                                   ),
                                                   choiceValues = unique(species_df$species)),
                                      numericInput(ns("stockingdensity"), label = HTML("<h3>Initial Stocking Density (fish/m<sup>3</sup>)</h3>"),
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
                                                   label = HTML("<h3>Number of Cages (6400m<sup>3</sup> each)</h3>"),
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
  
  
  r$mod2<- reactiveValues()
  
  observeEvent(input$run_button_growth, {
    r$mod2$run_num <- input$run_button_growth})

  
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

# Separete cells into cells above and below optimal SST
#cells_below_optimal <- reactive(
#suitable_sst() < fish_selection()$T0
#)

#cells_above_optimal <-  reactive(
#suitable_sst() >= fish_selection()$T0
# )

# Apply growth equations
# growth_below_optimal <- reactive(
#fish_selection()$a1*cells_below_optimal()*suitable_sst() + fish_selection()$b1*cells_below_optimal()
#)
# growth_above_optimal <- reactive(
#   fish_selection()$a2*cells_above_optimal()*suitable_sst() + fish_selection()$b2*cells_above_optimal()
# )

growth_rate <- reactive(
  fish_selection()$A_omega*suitable_sst() + fish_selection()$B_omega
)

# Add both rasters
#growth_raster <- eventReactive( input$run_button_growth,
#                               growth_above_optimal() + growth_below_optimal()
#)

#Stocking density and number of cages for biomass 
stockingdensity <- reactive(input$stockingdensity)
numberofcages <- reactive(input$numberofcages)  
cage_size <- 6400 #m^3
survival_rate <- 0.85 

# Growth raster 
growth_raster <- eventReactive(input$run_button_growth,
                               (growth_rate()/6.066)*6.066*(stockingdensity()*numberofcages()*cage_size)*survival_rate/1000) # dividing by 1000 to convert from kg to MT




# Render growth plot (now with the new growth raster)
output$growthMap <- renderLeaflet({
  # Palette
  pal_growth <- colorNumeric(c("#DAF7A6", "#C70039", "#581845"), values(growth_raster()),
                             na.color = "transparent")
  
  
  
  # Leaflet map
  leaflet(options = leafletOptions( zoomSnap = 0.2)) %>%
    addTiles(group = "Open Street Map") %>%
    addProviderTiles("Esri.WorldGrayCanvas", group = "Esri Gray Canvas (default)") %>%
    addRasterImage(growth_raster(),
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
              values = values(growth_raster()),
              title = "Fish Biomass (MT/cell)",
              labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>% 
    addMouseCoordinates()
  
}
)

### Download growth map
output$download_button_growth <- downloadHandler(
  
  filename = function() {
    "growth_map.tif"
  },
  content = function(file) {
    writeRaster(growth_raster(), file)
    
  })


return(list(
  growth_r = reactive(growth_raster()),
  cage_num = reactive(numberofcages()),
  stocking_d = reactive(stockingdensity()),
  cage_size = cage_size
))


}
