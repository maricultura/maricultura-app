
######################################################################################
# Module UI function
######################################################################################
areaCalculatorUI <- function(id){
  ns = NS(id)
  
  tabPanel(HTML('<div><i class="fa fa-calculator"></i> Area Calculator</div>'),
           fluidRow(
             column(12,
                    plotlyOutput(ns("barPlot")))
           ))
  
  
}

######################################################################################
# Module server function
######################################################################################

areaCalculator <- function(input, output, session, site_suitability) {

  ### First Bar Graph ###
  
  # Render plotly bar chart
  output$barPlot <- renderPlotly(
    output$barPlot <- renderPlotly(
      p <- plot_ly(
        x = site_suitability$values_x(),
        y = site_suitability$values_y(),
        text = site_suitability$values$text,
        marker = list(color = "#1D63A3"),
        type = "bar")%>%
        layout(title = "Total Suitable Area by Run Number",
               yaxis = list(title = HTML("Area (km<sup>2</sup>)")))
    )
  )
  
  
}