
# Module UI function
econUI <- function(id){
  ns = NS(id)

  tabPanel(HTML('<div><i class="fas fa-coins"></i>Economics</div>'),
           sidebarLayout(
             sidebarPanel(
               tabsetPanel(type = "tabs",
                           tabPanel("Economic Factors",
                                    numericInput(ns("fingerlingprice"), label = h3("Fingerling Price ($USD/fish)"),
                                                 min = .10,
                                                 max = 10.00,
                                                 step = .10,
                                                 value = 1.50),
                                    numericInput(ns("feedprice"), label = h3("Feed Price ($USD/kg)"),
                                                 min = 1.00,
                                                 max = 20.00,
                                                 step = .10,
                                                 value = 2.10),
                                    numericInput(ns("feedconversionratio"), label = h3("Feed Conversion Ratio"),
                                                 min = 1,
                                                 max = 10,
                                                 step = 1,
                                                 value = 3),
                                    numericInput(ns("priceoffish"), label = h3("Price of Fish at Market ($USD)"),
                                                 min = 1,
                                                 max = 20,
                                                 step = .10,
                                                 value = 8.6)
                           )),
               actionButton(ns("run_button_economics"), label = "Run"),
               downloadButton(ns("download_button_economics"), label = "Download")),
             mainPanel(
               leafletOutput(ns("economics_map"), height = "100vh")
             )
           ))

}

######################################################################################
# Module server function
######################################################################################

econ <- function(input, output, session, biomass_production) {
  

  
  
  dist_shore <- raster("data/dist_shore.tif")


num_farms <- 1 # number of farms per 9.2x9.2 km cell, most conservative estimate of 16 cages per/farm (per cell)
fuel_consumption <- 26.96 #L/hour
vessel_speed <- 15000 #average speed in m/hr
diesel_price <- 0.92 #USD/L using 2020 exchange rate 1 usd = 4 reais
distance_to_port <- 25 #depend on cell
num_of_boats <- 2

trips_annual <- 416 # roundtrips per farm per year, for 2 boats (1 boat @ 5 trips/week, 1 @ 8 trips per week, and 52 weeks a year)
one_way_trips_annual <- 2*trips_annual # (we have to double the roundtrips because we need to take into account that distance traveled happens TWICE for every round trip)

# Create raster for all fuel costs:
annual_fuel_cost_econ <- (dist_shore/vessel_speed)*fuel_consumption*diesel_price*one_way_trips_annual


full_time_workers <- 40
monthly_hours <- 160 #hours/month per fulltime employee
annual_hours <- (monthly_hours*12)
num_of_employees <-  ##/farm
  hourly_wage <- 4.50 #USD/hour average
work_days_per_month <- 20
workers_offshore <- 35
workers_onshore <- 5

# Determine Annual Fixed Wage Cost per Farm
fixed_labor_cost <- full_time_workers*hourly_wage*annual_hours

# Determine # of Annual Transit Hours
annual_transit_hours <- (dist_shore/vessel_speed)*one_way_trips_annual

# Determine Annual Wage Cost for Transit Hours Per Farm
transit_cost <- workers_offshore*annual_transit_hours*hourly_wage

# Create raster for total annual wage costs
total_annual_wage_costs <- transit_cost+fixed_labor_cost

# Farm Design
cage_cost <- 312000
time <- 12 #months, rotation period

# One-time costs
farm_installation <- 139555 # (Bezerra)
farm_lease <- 8668.74 # one-time lease (Bezerra)
signaling_system <- 28021.40 # one-time system installation (Bezerra)
project_development <- 53403.69 #project development (Bezerra)
miscellaneous <- 123685.54 # one time (Bezerra)
boats <- 420376.85 #for 3 boats, one time, 1 * 16m, 2* 7m (Bezerra)

# Annual fixed costs
electric_power <- 3661.32 # (Bezerra)
mooring_maintenance <- 53191.29 # (Bezerra)
diving_maintenance <- 8427.13 # (Bezerra)
office_rent <- 36626.43 # (Bezerra)
environmental_monitoring <- 45781.04 # (Bezerra)
boat_maintenance <- 30000 # for two boats (Costello)
dockage <- 20000 # for two boats (Costello)
insurance <- 50000 # (Costello)



feedprice <- reactive(input$feedprice)
feedconversionratio <- reactive(input$feedconversionratio)
fingerlingprice <- reactive(input$fingerlingprice)
priceoffish <- reactive(input$priceoffish)



# Create Feed Raster
feed_annual_rast <- reactive(biomass_production$growth_r()*1000*feedconversionratio()*feedprice()) # multiplied by 1000 to convert biomass from MT to kg

# Create Juvenile Cost 
juv_cost_annual <- reactive(biomass_production$stocking_d()*(biomass_production$cage_num()*biomass_production$cage_size)*fingerlingprice())

# Find Total Cage Cost
total_cage_cost <- reactive(cage_cost*biomass_production$cage_num())


# Non-Amortized Annual Fixed Costs
total_annual_fixed_costs <- reactive(electric_power + mooring_maintenance + diving_maintenance + office_rent + environmental_monitoring + boat_maintenance + dockage + insurance + feed_annual_rast() + juv_cost_annual())


# Amortized One-time Costs
one_time_fixed_costs_depreciated <- reactive(signaling_system + miscellaneous + boats + total_cage_cost() + farm_installation + farm_lease + project_development)


risk_rho <- 1.17647 # Discount rate = 15%
risk_discount <- (1-(1/risk_rho))


# Annuity Function
annuity <- (function(c, r = risk_discount, t = 10) {
  a <- c/ ((1-(1+r)^-t)/r)
  return(a)
})

# Find Amoritized Costs
amortized_costs <- reactive(annuity(one_time_fixed_costs_depreciated()))

# Find Total Costs
cost_total <- reactive(amortized_costs() + total_annual_fixed_costs() + annual_fuel_cost_econ + total_annual_wage_costs)

# Find Iost of Suitability Cells
#  cost_of_suitable <- reactive(mask(cost_total(), suitable()))

# Find Total Revenue
revenue_rast <- reactive(biomass_production$growth_r()*1000* priceoffish()) # multiplied by 1000 to convert biomass from MT to kg


# Find Profits
profit_raster <- reactive(revenue_rast()-cost_total())

# Find Net Present Value
npv <- eventReactive(input$run_button_economics, 
                     ((profit_raster()/(1-risk_discount)^1) +
                        (profit_raster()/(1-risk_discount)^2) + 
                        (profit_raster()/(1-risk_discount)^3) + 
                        (profit_raster()/(1-risk_discount)^4) + 
                        (profit_raster()/(1-risk_discount)^5) + 
                        (profit_raster()/(1-risk_discount)^6) + 
                        (profit_raster()/(1-risk_discount)^7) + 
                        (profit_raster()/(1-risk_discount)^8) + 
                        (profit_raster()/(1-risk_discount)^9) + 
                        (profit_raster()/(1-risk_discount)^10))/1000000) # divided by 1000000 to convert from USD to billion USD


# Create an Ouput Map

# Render economics plot
output$economics_map <- renderLeaflet({
  # Palette
  pal_econ <- colorNumeric(c("#DAF7A6", "#C70039", "#581845"),
                           values(npv()),
                           na.color = "transparent")
  
  # Leaflet map
  leaflet(options = leafletOptions( zoomSnap = 0.2)) %>%
    addTiles(group = "Open Street Map") %>%
    addProviderTiles("Esri.WorldGrayCanvas", group = "Esri Gray Canvas (default)") %>%
    addRasterImage(npv(),
                   colors = pal_econ,
                   group = "Economic Model") %>%
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
              pal = pal_econ,
              values = values(npv()),
              title = "NPV - 10 yrs<br>( Billion $USD/cell)",
              labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>% 
    addMouseCoordinates()
  
}
)



### Download Economics map
output$download_button_economics <- downloadHandler(
  
  filename = function() {
    "profitability_map.tif"
  },
  content = function(file) {
    writeRaster(npv(), file)
    
    
  })

}