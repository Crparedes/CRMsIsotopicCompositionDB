ShowDataUI <- function(id, label = "Counter", FlTy = 'Excel') {
  ns <- NS(id)
  column(
    4, tags$hr(), tags$hr(), uiOutput(ns('brwz')),
    h3('Element: ', tags$b(textOutput(ns('SelectedElem'), inline = TRUE))), 
    div(style = 'margin-left: 20px', uiOutput(ns('IUPAC_Message')), tableOutput(ns('IUPAC_Table'))),
    tags$hr(),
           
    actionLink(inputId = ns('IsoComCRM'), h4('CRMs certified for isotopic composition:')),
    uiOutput('ListIsoCompCRM'), tags$hr(),
    actionLink(inputId = ns('CalSolCRM'), h4('Calibration solution CRMs with Isotopic Composition data:')), 
    uiOutput('ListCalibraCRM'), tags$hr(),
    actionLink(inputId = ns('MatrixCRM'), h4('Matrix CRMs with Isotopic Composition data:')), 
    uiOutput('ListMatrixCRM'))
}

ShowDataServer <- function(id, devMode, SelectedElem) {
  moduleServer(
    id,
    function(input, output, session) {
      output$brwz <- renderUI(
        if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pause module'))))
      observeEvent(input$brwz, browser())
      
      isotopes <- eventReactive(
        SelectedElem(), ignoreInit = TRUE,
        #ciaaw.mass.2016[which(ciaaw.mass.2016$element == tolower(SelectedElem())), ]
        CIAAW_NatIsotAbunTable[which(CIAAW_NatIsotAbunTable$Element == tolower(SelectedElem())), ])
      
      
      IUPAC_Table <- reactive({if (nrow(isotopes()) > 1) return(isotopes())})
      
      delay(0, {hide(selector = "IsoComCRM"); hide(selector = "CalSolCRM"); hide(selector = "MatrixCRM")})
      observe({
        # toggleElement(condition = nrow(isotopes()) > 1, id = 'IUPAC_Table', anim = TRUE, animType = 'fade', time = 1)
        toggleElement(condition = nrow(isotopes()) > 1, id = 'IsoComCRM', anim = TRUE, animType = 'fade', time = 1)
        toggleElement(condition = nrow(isotopes()) > 1, id = 'CalSolCRM', anim = TRUE, animType = 'fade', time = 1)
        toggleElement(condition = nrow(isotopes()) > 1, id = 'MatrixCRM', anim = TRUE, animType = 'fade', time = 1)
      })
      
      IUPAC_Message <- reactive({
        if (nrow(isotopes()) < 2) {
          return(tags$h5('Selected element is monoisotopic. No data on isotopic composition was found.', tags$br(),
                         tags$b('Please select another element')))
        } else {
          return(tags$b('CIAAW: Natural isotopic composition'))
        }
      })
      
      
      output$SelectedElem  <- renderText(SelectedElem())
      output$IUPAC_Message <- renderUI(IUPAC_Message())
      output$IUPAC_Table   <- renderTable(IUPAC_Table())
    }
  )
}