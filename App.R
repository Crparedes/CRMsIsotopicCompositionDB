rm(list = ls())   # Delete all objects in workspace
gc()            # Garbage collector
# CRMsIsotopicCompositionDataBase

library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(ggplot2) #Grammar of graphics
library(ggfortify)
library(writexl)
library(rhandsontable)
library(shinycssloaders)
#library(htmlwidgets)
#library(htmltools)

modules <- with(list(pt = 'Modules/'), paste0(pt, list.files(path = pt)))
sapply(c(modules), source)


ui <- fluidPage(
  withMathJax(),
  useShinyjs(),
  # conditionalPanel(condition = "input.MainNavTabs == 'Home'",
  #                  div(class = "navbar2", 
  #                      navbarPage(windowTitle = 'masscor Graphical User Interface', title = Information, position = 'fixed-bottom', theme = shinytheme("flatly")))),
  navbarPage(
    title = title, windowTitle = 'MRCs Isotopic Composition DataBase', id = 'MainNavTabs',# selected = 'Home',
    theme = shinytheme("flatly"), position = 'fixed-top', collapsible = TRUE, lang = 'en',
    tabPanel(
      title = HTML('Explore<br>&nbsp;'), icon = icon('compass'), value = 'Home', 
      tags$hr(), tags$hr(),
      fluidRow(
        column(8, includeHTML('www/PeriodicTable.html')),
        column(
          4, tags$hr(), tags$hr(),
          h3('Element: ', tags$b(textOutput('SelectedElem', inline = TRUE))), 
          textOutput(outputId = 'WarningMonoIsot'), tags$hr(),
          actionLink(inputId = 'IsoComCRM', h4('CRMs certified for isotopic composition:')), uiOutput('ListIsoCompCRM'), tags$hr(),
          actionLink(inputId = 'CalSolCRM', h4('Calibration solution CRMs with Isotopic Composition data:')), uiOutput('ListCalibraCRM'), tags$hr(),
          actionLink(inputId = 'MatrixCRM', h4('Matrix CRMs with Isotopic Composition data:')), uiOutput('ListMatrixCRM'))), tags$hr(),
        actionButton(inputId = 'brwz1', label = tags$b('Browser()'))
    ),
    tabPanel(
      title = HTML('Upload<br>&nbsp;isotopic data'), icon = icon('compass')),
    
    tags$div(headTags1, headTags2, headTags3, style = 'display: none'),
    
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "PeriodicTable.css"))
  ),
  includeScript("www/sendID.js")
)

server <- function(input, output, session, devMode = TRUE) {
  observeEvent(input$brwz1, browser(), ignoreInit = TRUE)
  SelectedElem <- reactive(input$SelectedElement)
  
  output$SelectedElem <- renderText(SelectedElem())
  
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
