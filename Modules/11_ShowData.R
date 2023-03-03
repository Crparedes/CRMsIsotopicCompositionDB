ShowDataUI <- function(id, label = "Counter", FlTy = 'Excel') {
  ns <- NS(id)
  column(
    style = 'margin-left: -20px',
    4, tags$br(), uiOutput(ns('brwz')),
    h2('Element: ', tags$b(textOutput(ns('SelectedElem'), inline = TRUE))), 
    niceSeparator(),
    h4(style = 'margin-left: 10px', tags$b('CIAAW data on natural isotopic composition')),
    div(style = 'margin: 20px', uiOutput(ns('IUPAC_CIAAW'))),
    niceSeparator(),
    h4(style = 'margin-left: 10px', tags$b('Certified Reference Materials')),
    (div(
      style = 'margin: 20px', class = 'CRMsActionLinks',
      actionLink(inputId = ns('AcLnk_IsoCompCRM'), icon = icon('atom'), style = 'display: inline;',
                 tags$b(' With certified values for isotopic composition')),
      ShowAvailCRMsUI(ns("IsoCompCRM")),
      
      actionLink(inputId = ns('AcLnk_CalibraCRM'), icon = icon('flask'), style = 'display: inline;',
                 tags$b(' Calibration solutions and high purity solids')), 
      ShowAvailCRMsUI(ns("CalibraCRM")),
      
      actionLink(inputId = ns('AcLnk_MatrixCRM'), icon = icon('carrot'), style = 'display: inline;',
                 tags$b(' Matrix CRMs')), 
      ShowAvailCRMsUI(ns("MatrixCRM"))
    ))
  )
}

ShowDataServer <- function(id, devMode, SelectedElem) {
  moduleServer(
    id,
    function(input, output, session) {
      output$brwz <- renderUI(if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pause module'))))
      observeEvent(input$brwz, browser())
      
      observe(toggleElement(condition = nrow(isotopes()) > 1, selector = 'div.CRMsActionLinks',
                            anim = TRUE, animType = 'fade', time = 1))
      observeEvent(input$AcLnk_IsoCompCRM, toggle(selector = 'div.List_IsoCompCRM', anim = TRUE, animType = 'fade', time = 0.4))
      observeEvent(input$AcLnk_CalibraCRM, toggle(selector = 'div.List_CalibraCRM', anim = TRUE, animType = 'fade', time = 0.4))
      observeEvent(input$AcLnk_MatrixCRM, toggle(selector = 'div.List_MatrixCRM', anim = TRUE, animType = 'fade', time = 0.4))
      
      # CIAAW Information
      {
        isotopes <- eventReactive(SelectedElem(), ignoreInit = TRUE, {
          CIAAW_NatIsotAbunTable[
            which(CIAAW_NatIsotAbunTable$Element == tolower(SelectedElem())), 
            c("Isotope", "Relative.abundance", "Notes", "Interval")]})
  
        IUPAC_Table <- reactive({if (nrow(isotopes()) > 1) return(isotopes()[, 1:2])})
        IUPAC_CIAAW <- reactive({
          if (nrow(isotopes()) < 2) {
            return(NoIsotopesMessage)
          } else {
            Notes <- CIAAW_NatIsotAbunFtnts[CIAAW_NatIsotAbunFtnts$Note == strsplit(isotopes()$Notes[1], '')[[1]], 2]
            UncertStat <- ifelse(
              isotopes()$Interval[1], 
              'Intervals for the relative abundances are given for the elements whose isotopic composition vary significantly in nature.',
              'Uncertainties are given  in parentheses, following the last significant digit to which they are attributed.')
            Notes <- as.list(c(UncertStat, Notes))  
            return(tags$div(
              fluidRow(
                column(5, tableOutput(session$ns('IUPAC_Table'))),
                column(7, style = 'font-size: 11px;', tags$br(), tags$b('Notes:'), 
                       tags$ul(style = 'list-style-position: outside; padding-left: 0;',
                               HTML(paste0(lapply(Notes, FUN = function(x) return(as.character(tags$li(x)))), collapse = ''))))
              )))
          }
        })
      }
      
      # Isotopic Composition CRMs
      observe({
        req(SelectedElem())
        ShowAvailCRMsServer(
          id = 'IsoCompCRM', id2 = id, devMode = devMode, SelectedElem = SelectedElem, 
          gnrlClss = 'List_IsoCompCRM', key = 'Certified',
          CRMproducers = INITI_CRMproducers,
          CRMsInfoTable = INITI_IsoCompCRM_Info[grep(tolower(SelectedElem()), INITI_IsoCompCRM_Info$Elements, value = FALSE), ], 
          CRMsDataTable = INITI_IsoCompCRM_DataIR[INITI_IsoCompCRM_DataIR$Element == tolower(SelectedElem()), ])
      })
       
      # Calibration solution and high purity materials
      observe({
        req(SelectedElem())
        ShowAvailCRMsServer(
          id = 'CalibraCRM', id2 = id, devMode = devMode, SelectedElem = SelectedElem, 
          gnrlClss = 'List_CalibraCRM', key = 'Reported',
          CRMproducers = INITI_CRMproducers, MeasuReports = INITI_MeasuReports, 
          CRMsInfoTable = INITI_CalibraCRM_Info[grep(tolower(SelectedElem()), INITI_CalibraCRM_Info$Elements, value = FALSE), ], 
          CRMsDataTable = INITI_CalibraCRM_DataIR[INITI_CalibraCRM_DataIR$Element == tolower(SelectedElem()), ])
      })
      
      # Matrix CRMs
      observe({
        req(SelectedElem())
        ShowAvailCRMsServer(
          id = 'MatrixCRM', id2 = id, devMode = devMode, SelectedElem = SelectedElem, 
          gnrlClss = 'List_MatrixCRM', key = 'Reported',
          CRMproducers = INITI_CRMproducers, MeasuReports = INITI_MeasuReports,
          CRMsInfoTable = INITI_MatrixCRM_Info[grep(tolower(SelectedElem()), INITI_MatrixCRM_Info$Elements, value = FALSE), ], 
          CRMsDataTable = INITI_MatrixCRM_DataIR[INITI_MatrixCRM_DataIR$Element == tolower(SelectedElem()), ])
      })
      
      output$SelectedElem  <- renderText(SelectedElem())
      output$IUPAC_CIAAW   <- renderUI(IUPAC_CIAAW())
      output$IUPAC_Table   <- renderTable(IUPAC_Table())
    }
  )
}