library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  fluidRow(
    column(
    2, offset = 1, h3('List of elements:'),
    HTML(
      '<div class = "periodic-table">
          <div class = "element" style = "cursor: pointer;" id = "Hydrogen"> Hydrogen </div>
          <div class = "element" style = "cursor: pointer;" id = "Helium"> Helium </div>
          <div class = "element" style = "cursor: pointer;" id = "Lithium"> Lithium </div>
          ... <br> (115 more chemical elements)
       </div>'
    )),
    column(2, h3('Selected element:'), textOutput('SelectedElem'))),
  includeScript("sendID.js")
)

server <- function(input, output, session, devMode = TRUE) {
  # SelectedElem <- reactiveVal()
  # 
  #  onclick("Hydrogen", SelectedElem("Hydrogen"))
  #  onclick("Helium", SelectedElem("Helium"))
  # onclick("Lithium", SelectedElem("Lithium"))
  
  output$SelectedElem <- renderText(input$selected)
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")