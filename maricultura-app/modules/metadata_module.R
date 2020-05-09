
######################################################################################
# Module UI function
######################################################################################
metaUI <- function(id){
  
  # Create a namespace function using the provided id
  ns = NS(id)
  
  # Create metadata tab
  tabPanel(div(icon("table"),"Metadata"),
           tableOutput(ns("metadataTable")))
}
 
######################################################################################
# Module server function
######################################################################################
meta <- function(input, output, session) {
  
  # Render metadata table
  output$metadataTable <- renderTable({
    
    # Read csv file
    metadata <- read_csv("data/metadata.csv")
    metadata
    
  }, width = "100vw")
  
}


