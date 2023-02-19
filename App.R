rm(list = ls())   # Delete all objects in workspace
gc()            # Garbage collector
# CRMsIsotopicCompositionDataBase

library(shiny)
library(shinyjs)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(CIAAWconsensus)
library(tidyr)
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
    theme = shinytheme("yeti"), position = 'fixed-top', collapsible = TRUE, lang = 'en',
    tabPanel(
      title = HTML('Explore<br>&nbsp;'), icon = icon('compass'), value = 'Home', 
      tags$hr(), tags$hr(),
      fluidRow(
        column(8, includeHTML('www/PeriodicTable.html')),
        column(
          4, tags$hr(), tags$hr(),
          h3('Element: ', tags$b(textOutput('SelectedElem', inline = TRUE))), 
          div(style = 'margin-left: 20px', uiOutput('IUPAC_Message'), tableOutput("IUPAC_Table")),
          tags$hr(),
          
          actionLink(inputId = 'IsoComCRM', h4('CRMs certified for isotopic composition:')),
          uiOutput('ListIsoCompCRM'), tags$hr(),
          actionLink(inputId = 'CalSolCRM', h4('Calibration solution CRMs with Isotopic Composition data:')), 
          uiOutput('ListCalibraCRM'), tags$hr(),
          actionLink(inputId = 'MatrixCRM', h4('Matrix CRMs with Isotopic Composition data:')), 
          uiOutput('ListMatrixCRM'))), tags$hr(),
        actionButton(inputId = 'brwz1', label = tags$b('Browser()'))
    ),
    tabPanel(
      title = HTML('Upload<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;new data'), icon = icon('upload'))
  ), 
  
  tags$div(headTags1, headTags2, headTags3, style = 'display: none'),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "PeriodicTable.css")),
  includeScript("www/sendID.js")
)

server <- function(input, output, session, devMode = TRUE) {
  observeEvent(input$brwz1, browser(), ignoreInit = TRUE)
  SelectedElem <- reactive(input$SelectedElement)
  
  isotopes <- eventReactive(
    SelectedElem(), ignoreInit = TRUE,
    #ciaaw.mass.2016[which(ciaaw.mass.2016$element == tolower(SelectedElem())), ]
    CIAAW_NatIsotAbunTable[which(CIAAW_NatIsotAbunTable$Element == tolower(SelectedElem())), ])
  
  
  IUPAC_Table <- reactive({if (nrow(isotopes()) > 1) return(isotopes())})
  observe({
    toggleElement(condition = nrow(isotopes()) > 1, id = 'IUPAC_Table', anim = TRUE, animType = 'fade', time = 1)
    toggleElement(condition = nrow(isotopes()) > 1, id = 'IsoComCRM', anim = TRUE, animType = 'fade', time = 1)
    toggleElement(condition = nrow(isotopes()) > 1, id = 'CalSolCRM', anim = TRUE, animType = 'fade', time = 1)
    toggleElement(condition = nrow(isotopes()) > 1, id = 'MatrixCRM', anim = TRUE, animType = 'fade', time = 1)
  })
  IUPAC_Message <- reactive({
    if (nrow(isotopes()) < 2) {
      return(tags$h5('Selected element is monoisotopic. No data on isotopic composition was found.', tags$br(),
                     tags$b('Please select another element')))
    } else {
      return(tags$b('IUPAC-CIAAW Isotopic Composition:'))
    }
  })
  
  
  output$SelectedElem  <- renderText(SelectedElem())
  output$IUPAC_Message <- renderUI(IUPAC_Message())
  output$IUPAC_Table   <- renderTable(IUPAC_Table())
  
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
