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
      uiOutput(ns('UIIsoCompCRM')), tags$br(),
      
      actionLink(inputId = ns('AcLnk_CalibraCRM'), icon = icon('flask'), style = 'display: inline;',
                 tags$b(' Calibration solutions and high purity solids')), 
      tags$br(),
      actionLink(inputId = ns('AcLnk_MatrixCRM'), icon = icon('carrot'), style = 'display: inline;',
                 tags$b(' Matrix CRMs')), 
      ShowAvailCRMsUI(ns("MatrixCRM")) # First UI attempt to call a module from within a module
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
      
      NoInfo <- reactive(paste0('There are no entries yet for', tolower(SelectedElem()), 'in this category.'))
      # Isotopic Composition CRMs
      {
        IsoCompCRM <- eventReactive(SelectedElem(), ignoreInit = TRUE, {
          ShowTable <- INITI_IsoCompCRM_Info[grep(tolower(SelectedElem()), INITI_IsoCompCRM_Info$Elements, value = FALSE),
                                             c("Producer", "CRM.name", "Lot", "Description")]
          return(ShowTable)
        })
        
        UIIsoCompCRM <- reactive({
          if (nrow(IsoCompCRM()) >= 1) {
            ElmntToPrnt <- tableOutput(session$ns('Table_IsoCompCRM_List'))
          } else {ElmntToPrnt <- NoInfo()}
          return(hidden(tags$div(class = 'List_IsoCompCRM', style = 'margin-left: 8px;', ElmntToPrnt)))
        })
        
        Table_IsoCompCRM_List <- reactive({# Adapted from https://stackoverflow.com/a/70763580/7612904
          DT <- copy(IsoCompCRM())
          setDT(DT)
          DT[, inputId := CRM.name][
            , Details := as.character(
              actionLink(
                inputId = session$ns(inputId), label = 'Show details',
                onclick = sprintf(paste0("Shiny.setInputValue(id = '", id, "-SelectedCRM', value = '", inputId, "');")))),
            by = inputId][, inputId := NULL]
        })
        
        observeEvent(input$SelectedCRM, {
          Data <- INITI_IsoCompCRM_Info[INITI_IsoCompCRM_Info$CRM.name == input$SelectedCRM, ]
          Producer <- INITI_CRMproducers[INITI_CRMproducers$Producer == Data$Producer, ]
          showModal(modalDialog(
            title = HTML(paste0('Isotopic composition of ', tags$b(input$SelectedCRM))), easyClose = TRUE,
            crmSummary(Producer = Producer, Data = Data), tags$hr(), tableOutput(session$ns("Table_IsoCompCRM_IndDat"))))
        })
      
        output$Table_IsoCompCRM_IndDat <- renderTable({
          req(input$SelectedCRM)
          DT <- copy(INITI_IsoCompCRM_DataIR[
            INITI_IsoCompCRM_DataIR$CRM.name == input$SelectedCRM, 
            c('Isotopic.ratio', 'Value', 'Uncertainty', 'UncertType', 'k.factor')])
          DT$Value <- as.character(DT$Value)
          DT$Uncertainty <- as.character(DT$Uncertainty)
          return(DT)
        })
      }
       
      # Calibration solution and high purity materials
      
      # Matrix CRMs
      observe({
        req(SelectedElem())
        ShowAvailCRMsServer(
          id = 'MatrixCRM', devMode = devMode, SelectedElem = SelectedElem, gnrlClss = 'List_MatrixCRM',
          CRMsInfoTable = INITI_MatrixCRM_Info[grep(tolower(SelectedElem()), INITI_MatrixCRM_Info$Elements, value = FALSE), ], 
          CRMsDataTable = INITI_MatrixCRM_DataIR[INITI_MatrixCRM_DataIR$Element == tolower(SelectedElem()), ])
      })
      
      
      output$SelectedElem  <- renderText(SelectedElem())
      output$IUPAC_CIAAW   <- renderUI(IUPAC_CIAAW())
      output$IUPAC_Table   <- renderTable(IUPAC_Table())
      output$UIIsoCompCRM <- renderUI(UIIsoCompCRM())
      output$Table_IsoCompCRM_List <- renderTable(Table_IsoCompCRM_List(), sanitize.text.function = function(x) {x})
    }
  )
}