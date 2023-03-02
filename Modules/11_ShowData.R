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
      uiOutput(ns('UIIsoCompCRM')), #tableOutput(ns('TableIsoCompCRM')), 
      tags$br(),
      
      actionLink(inputId = ns('AcLnk_CalibraCRM'), icon = icon('flask'), style = 'display: inline;',
                 tags$b(' Calibration solutions and high purity solids')), 
      uiOutput(ns('ListCalibraCRM')), tags$br(),
      actionLink(inputId = ns('AcLnk_MatrixCRM'), icon = icon('carrot'), style = 'display: inline;',
                 tags$b(' Matrix CRMs')), 
      uiOutput(ns('ListMatrixCRM'))#, tags$hr()
    ))
  )
}

ShowDataServer <- function(id, devMode, SelectedElem) {
  moduleServer(
    id,
    function(input, output, session) {
      output$brwz <- renderUI(if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pause module'))))
      observeEvent(input$brwz, browser())
      
      # CIAAW Information
      {
        isotopes <- eventReactive(
          SelectedElem(), ignoreInit = TRUE, {
            CIAAW_NatIsotAbunTable[
              which(CIAAW_NatIsotAbunTable$Element == tolower(SelectedElem())), 
              c("Isotope", "Relative.abundance", "Notes", "Interval")]
        })
  
        observe(toggleElement(
          condition = nrow(isotopes()) > 1, selector = 'div.CRMsActionLinks', anim = TRUE, animType = 'fade', time = 1))
        observeEvent(input$AcLnk_IsoCompCRM, toggle(selector = 'div.List_IsoCompCRM', anim = TRUE, animType = 'fade', time = 0.4))
        observeEvent(input$AcLnk_CalibraCRM, toggle(selector = 'div.List_CalibraCRM', anim = TRUE, animType = 'fade', time = 0.4))
        observeEvent(input$AcLnk_MatrixCRM, toggle(selector = 'div.List_MatrixCRM', anim = TRUE, animType = 'fade', time = 0.4))

        IUPAC_Table <- reactive({if (nrow(isotopes()) > 1) return(isotopes()[, 1:2])})
        IUPAC_CIAAW <- reactive({
          if (nrow(isotopes()) < 2) {
            return(tags$h5('Selected element is monoisotopic or has no stable isotopes.
                           No data on isotopic composition was found.', tags$br(),
                           tags$b('Please select another element')))
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
      
      NoInfo <- reactive(hidden(tags$div(class = 'List_IsoCompCRM', style = 'margin-left: 20px;',
                                  'There are no entries yet for', tolower(SelectedElem()), 'in this category.')))
      # Isotopic Composition CRMs
      {
        IsoCompCRM <- eventReactive(SelectedElem(), ignoreInit = TRUE, {
          ShowTable <- INITI_IsoCompCRM_Info[grep(tolower(SelectedElem()), INITI_IsoCompCRM_Info$Elements, value = FALSE),
                                             c("Producer", "CRM.name", "Lot", "Description")]
          # ShowTable$CRM <- paste(ShowTable$CRM.name, ShowTable$Lot, sep = ', ')
          return(ShowTable)
        })
        
        UIIsoCompCRM <- reactive({
          if (nrow(IsoCompCRM()) >= 1) {
            return(hidden(tags$div(class = 'List_IsoCompCRM', tableOutput(session$ns('TableIsoCompCRM')))))
          } else {return(NoInfo())}
        })
      }
      
      
       TableIsoCompCRM <- reactive({
         # Nice solution adapted from ismirsehregal https://stackoverflow.com/a/70763580/7612904
        DT <- copy(IsoCompCRM())
         setDT(DT)
         
         DT[, inputId := CRM.name][#paste0("CRM.ID_input_", seq_len(.N))][
           , Details := as.character(
             actionLink(
               inputId = session$ns(inputId), label = 'Show details',
               onclick = sprintf(paste0("Shiny.setInputValue(id = '", id, "-SelectedCRM', value = '", inputId, "');"), 
                                 session$ns('SlctdCRM.name')))),
           by = inputId][, inputId := NULL]
       })

       observeEvent(input$SelectedCRM, {
         showModal(modalDialog(
           title = HTML(paste0('Isotopic composition of ', tags$b(input$SelectedCRM))), easyClose = TRUE,
           tableOutput(session$ns("filtered_table"))
         ))
       })
      
       output$filtered_table <- renderTable({
         req(input$SelectedCRM)
         DT <- copy(INITI_IsoCompCRM_DataIR[
           , c('CRM.name', 'Isotopic.ratio', 'Value', 'Uncertainty', 'UncertType', 'k.factor')])
         DT$Value <- as.character(DT$Value)
         DT$Uncertainty <- as.character(DT$Uncertainty)
         
         setDT(DT)
         DT[CRM.name == input$SelectedCRM, ]
       })
       
      # Calibration solution and high purity materials
      
      # Matrix CRMs
      
      output$SelectedElem  <- renderText(SelectedElem())
      output$IUPAC_CIAAW   <- renderUI(IUPAC_CIAAW())
      output$IUPAC_Table   <- renderTable(IUPAC_Table())
      output$UIIsoCompCRM <- renderUI(UIIsoCompCRM())
      output$TableIsoCompCRM <- renderTable(TableIsoCompCRM(), sanitize.text.function = function(x) {x})
    }
  )
}