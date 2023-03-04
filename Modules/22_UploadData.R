UploadDataUI <- function(id, label = "Counter", FlTy = 'Excel') {
  ns <- NS(id)
  fluidRow(
    column(
      8, offset = 1,  tags$hr(), tags$hr(),
      'Use this module to load new data into the dataframe.', tags$br(),
      tags$ol(
        tags$li('Select the type of information that you want to provide:'),
        tags$div(
          style = 'margin-left: 20px;', 
          radioGroupButtons(
            ns('CRM_type'), label = NULL, selected = NULL,
            choices = list('Data of a CRM with certified values for isotopic composition' = 'IsoCompCRM',
                           'Literature report of measured isotopic composition in a (non isotopically certified) CRM' = 'LitRepoCRM'))),
        conditionalPanel("input.SelectCol != null", ns = ns, {
          conditionalPanel(
            "input.CRM_type == 'LitRepoCRM'", ns = ns, 
            tags$li('Which kind of (non isotopically certified) CRM was measured?'),
            tags$div(style = 'margin-left: 20px;', 
                     radioButtons(
                       ns('CRM_NIC?type'), label = NULL, selected = NULL,
                       choices = list('Calibration solution of high purity solid CRM' = 'CalibraCRM',
                                      'Matrix CRM' = 'MatrixCRM'))),
          )
        })
        
      )
    ),
    column(
    4, tags$hr(), tags$hr(), uiOutput(ns('brwz'))
    )
  )
}

UploadDataServer <- function(id, devMode, SelectedElem) {
  moduleServer(
    id,
    function(input, output, session) {
      output$brwz <- renderUI(
        if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pause module'))))
      observeEvent(input$brwz, browser())
      
      
    }
  )
}