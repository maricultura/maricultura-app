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
library(leaflet.extras)
library(shinyjs)

# Source helper R scripts
source("scripts/about_page.R")
source("scripts/species_df.R")
source("scripts/footer.R")
source("scripts/modal_dialogue.R")

# Source modules
source("modules/siteSuit_module.R")
source("modules/biomass_module.R")
source("modules/econ_module.R")
source("modules/metadata_module.R")
source("modules/area_module.R")

##########################################################################################
# Define UI for application
##########################################################################################
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
  navbarPage("" , # Empty Navbar title
             id = "navbar",
             selected = div(icon("map-pin"),"Site Suitability"),
             
             # First tab: About Page
             aboutPage,
             
             # Second tab: Site Suitability
             siteSuitUI("site_suitability"),
             
             # Third Tab: Biomass
             biomassProdUI("biomass_production"),
             
             #Fourth Tab: Economics
             econUI("profitability"),
             
             # Fifth Tab: Area Calculator
             areaCalculatorUI("area_graphs"),
             
             # Seventh Tab: User Guide
             tabPanel(HTML('<div><i class="fas fa-book"></i> User Guide</div>'),
                      fluidRow(
                        column(12,
                      HTML('<iframe height=400px  width=100% src= "user_guide.pdf"></iframe>')))
             ),
             
             # Eight Tab: Metadata
             metaUI("metadata_table")
  ),
  # Footer
  br(),
  footer
)
##########################################################################################
# Define server logic 
##########################################################################################
server <- function(input, output) {
  
  ### Show/Hide tabs ###
  # Hide growth, economic, and area tabs when server starts
   hideTab(inputId = "navbar", target = HTML('<div><i class="fa fa-chart-line"></i>Biomass</div>'))
   hideTab(inputId = "navbar", target = HTML('<div><i class="fas fa-coins"></i>Economics</div>'))
   hideTab(inputId = "navbar", target = HTML('<div><i class="fa fa-calculator"></i> Area Calculator</div>'))
  
   # Create reactive value to keep track of clicks on run buttons from different modules
   r <- reactiveValues()
   
  # Show tabs after clicking the run button/growth run button
  observeEvent(r$run_num_suit, {
    showTab(inputId = "navbar", target = HTML('<div><i class="fa fa-chart-line"></i>Biomass</div>'))
  })
  observeEvent(r$run_num_growth, {
    showTab(inputId = "navbar", target = HTML('<div><i class="fas fa-coins"></i>Economics</div>'))
  })
  observeEvent(r$run_num_suit, {
    showTab(inputId = "navbar", target = HTML('<div><i class="fa fa-calculator"></i> Area Calculator</div>'))
  })

  
  ### Modal Dialogue ###
  showModal(dataModal())
  
  ### Call Modules for Each Tab ###

  # Site Suitability
  site_suitability <- callModule(siteSuit,
                                 "site_suitability",
                                 r = r)
  
  # Growth Model
  biomass_production <- callModule(biomassProd,
                                   "biomass_production",
                                   site_suitability = site_suitability,
                                   r = r)
  
  # Economic Model
  profitability <- callModule(econ,
                              "profitability",
                              site_suitability = site_suitability,
                              biomass_production = biomass_production)
  
  # Area Calculator 
  area_graphs <- callModule(areaCalculator,
                            "area_graphs",
                            site_suitability = site_suitability)
  
  # Metadata Table
  metadata_table <- callModule(meta,
                               "metadata_table")
  
}
# Run the application 
shinyApp(ui = ui, server = server)