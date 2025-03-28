rm(list = ls())   # Delete all objects in workspace
gc()              # Garbage collector
# CRMsIsotopicCompositionDataBase

library(shiny)
library(shinyjs)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(CIAAWconsensus)
library(tidyr)
library(stringr)
# library(odbc)
# library(RMySQL)
# library(googlesheets4)
library(readxl)
library(writexl)
library(rhandsontable)
library(data.table)
library(DT)
library(shinycssloaders)
library(countrycode)
#library(htmlwidgets)
#library(htmltools)


modules <- with(list(pt = 'Modules/'), paste0(pt, list.files(path = pt)))
sapply(c(modules), source)

 #### Source following lines to construct initial tables to be used in 
 ### source('www/IsotopicData/02_INITIAL_DataTableConstruction.R')

source('www/Credentials.R')
source('www/IsotopicData/01_CIAAW_WebPageRvesting.R')
GenericPeriodicTable <- read.csv(file = 'www/IsotopicData/RAW_GenericPeriodicTable.csv')


ShyTheme <- shinytheme("yeti")
windowTitle <- 'CRMs Isotopic Composition DataBase'
CRMsICDBversion <- 'v.0.0.13.9999'

ui <- fluidPage(
  tags$head(tags$script('
                        var width = 1;
                        $(document).on("shiny:connected", function(e) {
                          width = window.innerWidth/1900;
                          document.body.style.zoom = Math.sqrt(width);
                        });
                        ')),
  withMathJax(),
  useShinyjs(),
  navbarPage(
    title = title, windowTitle = windowTitle, id = 'MainNavTabs',# selected = 'Home',
    theme = ShyTheme, position = 'fixed-top', collapsible = TRUE, lang = 'en',
    tabPanel(
      title = HTML('Explore<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'), 
      icon = icon('compass'), value = 'Home', tags$hr(), tags$hr(),
      fluidRow(
        column(
          width = 7,
          h4(style = 'margin-left: 50px;', ('Select an element from the periodic table below:'), tags$br()),
          includeHTML('www/PeriodicTable.html')),
        # uiOutput('ColumnPeriodTable'), 
        ShowDataUI('ShowData')
      )
    ),
    tabPanel(
      title = HTML('Add<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;new data'), icon = icon('upload'),
      UploadDataUI('UploadData')
    ),
    tabPanel(
      title = HTML('About the App,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Bibliography'), icon = icon('book'),
      BibliographyUI('Bibliography')
    ),
    footer = tags$div(
      style = 'margin-left: 50px; margin-top: 20px',
      tags$b('App version ', CRMsICDBversion), tags$hr(),
      materialSwitch(inputId = 'Desarrollador', status = 'primary', value = FALSE,
                     label = h6(style = "display: inline;", 'Developer tools')),
      uiOutput('brwz'))
  ),
  
  div(class = "navbar2", navbarPage(Disclaimer, position = 'fixed-bottom', theme = ShyTheme)), 
  
  tags$div(headTags1, headTags2, headTags3, style = 'display: none'),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "PeriodicTable.css")),
  includeScript("www/PeriodicTableSendID.js")
)

server <- function(input, output, session, devMode = TRUE) {
  devMode <- reactive(input$Desarrollador)
  showNotification(tags$b("This app is under development"), type = 'warning')
  
  output$brwz <- renderUI(
    if(devMode()) return(tags$span(
      tags$b('The', tags$em('Pause and inspect'), 'buttons will not work if running the app in a server: '), 
      actionButton(inputId = 'brwz', class = 'PauBtn', label = tags$b('Pause and inspect App')))))
  observeEvent(input$brwz, browser())
  
  CRMproducers <- loadFromDataBase('CRMproducers')
  MeasuReports <- loadFromDataBase('MeasuReports')
  MeasRepoAuth <- loadFromDataBase('MeasRepoAuth')
  
  SelectedElem <- reactive(tolower(input$SelectedElement))
  
  ShowDataServer('ShowData', devMode = devMode, SelectedElem = SelectedElem,
                 CRMproducers = CRMproducers, MeasuReports = MeasuReports, MeasRepoAuth = MeasRepoAuth)
  
  UploadDataServer('UploadData', devMode = devMode,
                   TableProducers = CRMproducers, TableStudies = MeasuReports)
  
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
