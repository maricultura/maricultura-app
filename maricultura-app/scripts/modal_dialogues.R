
# Create modal to be shown when the app is loaded
startModal <- function(failed = FALSE) {
  modalDialog(
    title = tags$h1(
      style = "text-align: center;",
      "Welcome to Brazil's First Mariculture Siting Tool !"
    ),
    tags$div(
      style = "text-align: center;",
      tags$p("This tool identifies available sites for marine aquaculture development in Brazil"),
      tags$p(tags$b("1."), "Find sites by selecting and modifying oceanographic conditions and spatial constraints"),
      tags$p(tags$b("2."), "Click the 'Run' button to generate a map of suitable sites"),
      tags$p(tags$b("3."), "Download your map!")
    ),
    footer = tagList(
      modalButton("Start")
    )
  )
}

# Create modal to be shown when after running site suitability tab
modal2 <- function(failed = FALSE) {
  modalDialog(
    title = tags$h3(
      style = "text-align: center;",
      "Success! Your site suitability map has been generated!"
    ),
    tags$div(
      style = "text-align: center;",
      tags$p("Now you can: "),
      HTML('<p><b>1.</b> Estimate yearly potential biomass production of suitable sites in the <span style="color:#2680e3;"><b><i class="fa fa-chart-line"></i>Biomass</b></span> tab.</p>'),
      HTML('<p><b>2.</b> Calculate total area of suitable sites in the <span style="color:#2680e3;"><b><i class="fa fa-calculator"></i> Area Calculator</b></span> tab.</p>')
    ),
    footer = tagList(
      modalButton("Continue")
    )
  )
}

# Create modal to be shown when after running biomass tab
modal3 <- function(failed = FALSE) {
  modalDialog(
    title = tags$h3(
      style = "text-align: center;",
      "Success! Your biomass production map has been generated!"
    ),
    tags$div(
      style = "text-align: center;",
      tags$p("Now you can: "),
      HTML('<p><b>1.</b> Estimate profitability of suitable sites in the <span style="color:#2680e3;"><b><i class="fas fa-coins"></i>Economics</b></span> tab.</p>'),
    ),
    footer = tagList(
      modalButton("Continue")
    )
  )
}
