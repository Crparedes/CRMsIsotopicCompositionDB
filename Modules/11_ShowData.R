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
      uiOutput(ns('ListIsoCompCRM')), tags$br(),
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
      
      NoInfo <- reactive(tags$div(style = 'margin-left: 20px;',
                                  'There are no entries yet for', SelectedElem(), 'in this category.'))
      # Isotopic Composition CRMs
      {
        IsoCompCRM <- eventReactive(SelectedElem(), ignoreInit = TRUE, {
          INITI_IsoCompCRM_Info[which(INITI_IsoCompCRM_Info$Elements == tolower(SelectedElem())),
                               c("CRM.name", "Lot", "Producer", "Description")]})
        
        ListIsoCompCRM <- reactive({
          if (nrow(IsoCompCRM()) >= 1) {
            IsoCompCRM <- IsoCompCRM()
            IsoCompCRMList <- vector(mode = 'list', length = nrow(IsoCompCRM))
            for (i in 1:nrow(IsoCompCRM)) {
              inputId <- sub(" ", "_", paste(IsoCompCRM$CRM.name[i], IsoCompCRM$Lot[i], sep = '_'))
              label <- paste0(IsoCompCRM$Producer[i], ', ', IsoCompCRM$CRM.name[i], '. Lot ', IsoCompCRM$Lot[i])
              IsoCompCRMList[[i]] <- actionLink(inputId = session$ns(inputId), label = label, class = "IsoCompCRM")
            }
            return(hidden(tags$div(
              class = 'List_IsoCompCRM', 
              tags$ul(HTML(paste0(lapply(IsoCompCRMList, FUN = function(x) return(as.character(tags$li(x)))), collapse = '')))
            )))
          } else {return(NoInfo())}
        })
      }
      
      # Calibration solution and high purity materials
      {
        CalibraCRM <- eventReactive(SelectedElem(), ignoreInit = TRUE, {
          INITI_CalibraCRM_Info[which(INITI_CalibraCRM_Info$Elements == tolower(SelectedElem())),
                                c("CRM.name", "Lot", "Producer", "Description")]})
        
        ListCalibraCRM <- reactive({
          if (nrow(CalibraCRM()) >= 1) {
            CalibraCRM <- IsoCompCRM()
            CalibraCRMList <- vector(mode = 'list', length = nrow(CalibraCRM))
            for (i in 1:nrow(CalibraCRM)) {
              inputId <- sub(" ", "_", paste(CalibraCRM$CRM.name[i], CalibraCRM$Lot[i], sep = '_'))
              label <- paste0(CalibraCRM$Producer[i], ', ', CalibraCRM$CRM.name[i], '. Lot ', CalibraCRM$Lot[i])
              CalibraCRMList[[i]] <- actionLink(inputId = session$ns(inputId), label = label, class = "CalibraCRM")
            }
            return(hidden(tags$div(
              class = 'List_CalibraCRM', 
              tags$ul(HTML(paste0(lapply(CalibraCRMList, FUN = function(x) return(as.character(tags$li(x)))), collapse = '')))
            )))
          } else {return(NoInfo())}
        })
      }
      
      # Matrix CRMs
      {
        MatrixCRM <- eventReactive(SelectedElem(), ignoreInit = TRUE, {
          INITI_MatrixCRM_Info[which(INITI_MatrixCRM_Info$Elements == tolower(SelectedElem())),
                               c("CRM.name", "Lot", "Producer", "Description")]})
        
        ListMatrixCRM <- reactive({
          if (nrow(MatrixCRM()) >= 1) {
            MatrixCRM <- IsoCompCRM()
            MatrixCRMList <- vector(mode = 'list', length = nrow(MatrixCRM))
            for (i in 1:nrow(MatrixCRM)) {
              inputId <- sub(" ", "_", paste(MatrixCRM$CRM.name[i], MatrixCRM$Lot[i], sep = '_'))
              label <- paste0(MatrixCRM$Producer[i], ', ', MatrixCRM$CRM.name[i], '. Lot ', MatrixCRM$Lot[i])
              MatrixCRMList[[i]] <- actionLink(inputId = session$ns(inputId), label = label, class = "MatrixCRM")
            }
            return(hidden(tags$div(
              class = 'List_MatrixCRM', 
              tags$ul(HTML(paste0(lapply(MatrixCRMList, FUN = function(x) return(as.character(tags$li(x)))), collapse = '')))
            )))
          } else {return(NoInfo())}
        })
      }
      
      output$SelectedElem  <- renderText(SelectedElem())
      output$IUPAC_CIAAW   <- renderUI(IUPAC_CIAAW())
      output$IUPAC_Table   <- renderTable(IUPAC_Table())
      output$ListIsoCompCRM <- renderUI(ListIsoCompCRM())
      output$ListCalibraCRM <- renderUI(ListCalibraCRM())
      output$ListMatrixCRM  <- renderUI(ListMatrixCRM())
    }
  )
}