UploadDataUI <- function(id, label = "Counter", FlTy = 'Excel') {
  ns <- NS(id)
  fluidRow(
    column(
      8, offset = 1,  tags$hr(), tags$hr(),
      h4(tags$b('Use this module to load new data into the database')), tags$br(),
      tags$ol(
        radioButtons(
          ns('CRM_type'), label = tags$lib('Which kind of data will you provide:'), 
          selected = character(0), width = '100%',
          choices = list(
            '(UNDER CONSTRUCTION) Data of a CRM with certified values for isotopic composition' = 'IsoCompCRM',
            '(UNDER CONSTRUCTION) Report of measured isotopic composition in a (non isotopically certified) CRM' = 'LitRepoCRM',
            '(FEATURE TO IMPLEMENT IN THE FUTURE) Upload a Digital Reference Material Certificate with isotopic composition data' = 'DRMC')),
        conditionalPanel(
          "input.CRM_type != null", ns = ns,
          UploadProduStudyUI(ns('Producer'), key = 'Producer'),
          
          conditionalPanel(
            "input.CRM_type == 'LitRepoCRM", ns = ns,
            UploadProduStudyUI(ns('Study'), key = 'Study')),
          
          # Input CRM material info
          # Input CRM Certified/measured values
          
            
          conditionalPanel(
            "input.selectedProducer != ''", ns = ns, 
            conditionalPanel(
              "input.CRM_type == 'LitRepoCRM'", ns = ns, 
              tags$hr(), uiOutput(ns('selectStudy')),
              tags$hr(),
              radioButtons(
                ns('CRM_NIC_type'), label = tags$lib('Which kind of (non isotopically certified) CRM do you want to report?'),
                width = '100%',
                choices = list('Calibration solution or high purity solid' = 'CalibraCRM',
                               'Matrix certified reference material' = 'MatrixCRM'))
            )
          )
        )
      ), 
      uiOutput(ns('brwz'))
    )
  )
}

UploadDataServer <- function(id, devMode, TableProducers, TableStudies) {
  moduleServer(
    id,
    function(input, output, session) {
      output$brwz <- renderUI(if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pause module'))))
      observeEvent(input$brwz, browser())
      
      Producer <- UploadProduStudyServer(
          id = 'Producer', id2 = id, devMode = devMode, key = 'Producer', TableKreators = TableProducers)
      
      Study <- UploadProduStudyServer(
        id = 'Study', id2 = id, devMode = devMode, key = 'Study', TableKreators = TableStudies)
      
    }
  )
}