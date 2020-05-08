footer <- div( style = "background-color: #c5dbeb; padding: 15px; text-align: center;",
               tags$footer("Developed with",
                           tags$span(style = "font-family: 'Source Code Pro', monospace; font-size: 25px;" ,"Shiny"),
                           "from",
                           img(src = "https://rstudio.com/wp-content/uploads/2018/10/RStudio-Logo-Flat.png", height = "30px"),
                           ".",
                           br(),
                           "R version 3.6.1 (2019-07-05). Code on  ", tags$a(href ="https://github.com/annagaby/tree-monitoring", target="_blank", icon("github"),"GitHub."))
)