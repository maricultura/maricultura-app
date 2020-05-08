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