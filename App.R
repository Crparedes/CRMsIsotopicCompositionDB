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
      title = HTML('Home<br>&nbsp;'), icon = icon('compass'), value = 'Home', 
      tags$hr(), tags$hr(),
      fluidRow(
        column(7, includeHTML('PeriodicTable.html'))),
      #homeMasscor.UI(id = 'homeMasscor'),
      # actionButton(inputId = 'brwz1', label = tags$b('Browser()')) #Eliminar esta linea
    ),
    # tabPanel(title = HTML('Create/upload <br>', spcs(7), 'NAWI DCC'), icon = icon('certificate'), value = 'CreateUploadDCC',
    #          tags$hr(), tags$hr(), manageDCC.UI(id = 'manageDCC')),
    # tabPanel(title = HTML('Conventional <br>', spcs(7), 'mass correction'), icon = icon('ethernet'), 
    #          tags$hr(), tags$hr(), conventionalMass.UI(id = 'conventionalMass')),
    # tabPanel(title = HTML('Air buoyancy <br>', spcs(6), 'correction factors'), icon = icon('leaf'), tags$hr(), tags$hr(), buoyancyCorrections.UI(id = 'MABC')),
    tags$div(headTags1, headTags2, headTags3, style = 'display: none'), # mainly css code
    
    # CSS files:
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "PeriodicTable.css")
    )
  )
)

server <- function(input, output, session, devMode = TRUE) {
  observeEvent(input$brwz1, browser(), ignoreInit = TRUE)
  
  
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
