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
library(odbc)
library(RMySQL)
library(writexl)
library(rhandsontable)
library(data.table)
library(DT)
library(shinycssloaders)
library(countrycode)
#library(htmlwidgets)
#library(htmltools)

options(mysql = list("host" = "sql9.freesqldatabase.com", "user" = "sql9599488", "password" = "PHsEvvEBuY"))

modules <- with(list(pt = 'Modules/'), paste0(pt, list.files(path = pt)))
sapply(c(modules), source)

 #### Source following lines to construct initial tables to be used in 
 ### source('www/IsotopicData/01_CIAAW_WebPageRvesting.R')
 ### source('www/IsotopicData/02_INITIAL_DataTableConstruction.R')
GenericPeriodicTable <- read.csv(file = 'www/IsotopicData/RAW_GenericPeriodicTable.csv')


ShyTheme <- shinytheme("yeti")
windowTitle <- 'MRCs Isotopic Composition DataBase'

ui <- fluidPage(
  withMathJax(),
  useShinyjs(),
  navbarPage(
    title = title, windowTitle = 'MRCs Isotopic Composition DataBase', id = 'MainNavTabs',# selected = 'Home',
    theme = ShyTheme, position = 'fixed-top', collapsible = TRUE, lang = 'en',
    tabPanel(
      title = HTML('Explore<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'), 
      icon = icon('compass'), value = 'Home', tags$hr(), tags$hr(),
      fluidRow(
        column(
          width = 8,
          h4(style = 'margin-left: 50px;', ('Select an element from the periodic table below:')),
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
  output$brwz <- renderUI(
    if(devMode()) return(tags$span(
      tags$b('These buttons will not work if running in a server.'), 
      actionButton(inputId = 'brwz', label = tags$b('Pause App')))))
  observeEvent(input$brwz, browser())
  
  SelectedElem <- reactive(input$SelectedElement)
  
  
  # ColumnPeriodTable <- reactive({
  #   ColSize <- ifelse(is.null(SelectedElem()), 8, 5)
  #   ColumnPeriodTable <- 0
  #   return(ColumnPeriodTable)
  # })
  # output$ColumnPeriodTable <- renderUI(ColumnPeriodTable())
  
  ShowDataServer('ShowData', devMode = devMode, SelectedElem = SelectedElem)
  
  UploadDataServer('UploadData', devMode = devMode,
                   TableProducers = loadFromDataBase('CRMproducers'), TableStudies = loadFromDataBase('MeasuReports'))
  
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
