
######################################################################################
# Module UI function
######################################################################################
metaUI <- function(id){
  ns = NS(id)
  
  tabPanel(div(icon("table"),"Metadata"),
           tableOutput(ns("metadataTable")))
}
 
######################################################################################
# Module server function
######################################################################################

meta <- function(input, output, session) {
  
  output$metadataTable <- renderTable({
    
    # Read csv file
    metadata <- read_csv("data/metadata.csv")
    metadata
    
  }, width = "100vw")
  
}


