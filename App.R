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
library(stringr)
library(odbc)
library(RMySQL)
library(ggplot2) #Grammar of graphics
library(ggfortify)
library(writexl)
library(rhandsontable)
library(shinycssloaders)

#library(htmlwidgets)
#library(htmltools)

modules <- with(list(pt = 'Modules/'), paste0(pt, list.files(path = pt)))
sapply(c(modules), source)
source('www/IsotopicData/01_CIAAW_WebPageRvesting.R')
source('www/IsotopicData/02_INITIAL_DataTableConstruction.R')
GenericPeriodicTable <- read.csv(file = 'www/IsotopicData/RAW_GenericPeriodicTable.csv')

ShyTheme <- shinytheme("yeti")
windowTitle <- 'MRCs Isotopic Composition DataBase'

MRCsICDB <- RMySQL::dbConnect( ## Hosted freely by https://www.freesqldatabase.com/account/ (5Mb limit)
  RMySQL::MySQL(), user = 'sql9599488', password = 'PHsEvvEBuY', 
  dbname = 'sql9599488', host = 'sql9.freesqldatabase.com')

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
          8, 
          # selectizeInput(
          #   inputId = 'SelectedElement', label = NULL, choices = GenericPeriodicTable$Element,
          #   selected = NULL,
          #   options = list(placeholder = 'Write an element or choose it from the periodic table.',
          #                  onInitialize = I('function() { this.setValue(""); }'))),
          h4(style = 'margin-left: 50px;', ('Select an element from the periodic table below:')),
          includeHTML('www/PeriodicTable.html')), ShowDataUI('ShowData')
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
  includeScript("www/sendID.js")
)

server <- function(input, output, session, devMode = TRUE) {
  devMode <- reactive(input$Desarrollador)
  output$brwz <- renderUI(
    if(devMode()) return(tags$div(
      tags$b('These buttons will not work if running in a server.'), tags$br(),
      actionButton(inputId = 'brwz', label = tags$b('Pause App'))
      )))
  observeEvent(input$brwz, browser())
  
  SelectedElem <- reactive(input$SelectedElement)
  
  ShowDataServer('ShowData', devMode = devMode, SelectedElem = SelectedElem)
  
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
