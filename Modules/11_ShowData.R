ShowDataUI <- function(id, label = "Counter", FlTy = 'Excel') {
  ns <- NS(id)
  column(
    style = 'margin-left: -20px',
    4, tags$br(), uiOutput(ns('brwz')),
    h2('Element: ', tags$b(textOutput(ns('SelectedElem'), inline = TRUE))), 
    niceSeparator(),
    h4(style = 'margin-left: 10px', tags$b('Certified reference materials in the database:')),
    (div(
      style = 'margin: 20px', class = 'CRMsActionLinks',
      uiOutput(ns('NoElement')),
      actionLink(inputId = ns('AcLnk_IsoCompCRM'), icon = icon('atom'), style = 'display: inline;',
                 tags$b(' With certified values for isotopic composition')),
      ShowAvailCRMsUI(ns("IsoCompCRM")),
      
      actionLink(inputId = ns('AcLnk_CalibraCRM'), icon = icon('flask'), style = 'display: inline;',
                 tags$b(' Calibration solutions and high purity solids')), 
      ShowAvailCRMsUI(ns("CalibraCRM")),
      
      actionLink(inputId = ns('AcLnk_MatrixCRM'), icon = icon('carrot'), style = 'display: inline;',
                 tags$b(' Matrix CRMs')), 
      ShowAvailCRMsUI(ns("MatrixCRM"))
    )),
    niceSeparator(),
    div(style = 'margin: 20px', uiOutput(ns('IUPAC_CIAAW')))
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
  
        IUPAC_Table <- reactive({if (nrow(isotopes()) > 1) {
          IUPAC_Abun <- isotopes()[, 1:2]
          IUPAC_Mass <- apply(ciaaw.mass.2016[ciaaw.mass.2016$isotope %in% IUPAC_Abun$Isotope, 3:4], 2, as.character)
          IUPAC_Table <- cbind(IUPAC_Abun, IUPAC_Mass)
          colnames(IUPAC_Table)[2:4] <- c('Relative abundance (amount fraction)', 'Atomic mass (Da)', 'Uncertainty (Da)')
          return(IUPAC_Table[c(1, 3:4, 2)])
        }})
        
        IUPAC_CIAAW <- reactive({
          if (nrow(isotopes()) < 2) {
            return(NoIsotopesMessage)
          } else {
            Notes <- CIAAW_NatIsotAbunFtnts[CIAAW_NatIsotAbunFtnts$Note == strsplit(isotopes()$Notes[1], '')[[1]], 2]
            UncertStat <- ifelse(
              isotopes()$Interval[1], 
              'For isotopic relative abundances, intervals are given for the elements whose isotopic composition vary significantly in nature.',
              'For isotopic relative abundances the uncertainties are given  in parentheses, following the last significant digit to which they are attributed.')
            Notes <- as.list(c(UncertStat, Notes))  
            return(tags$div(
              h4(style = 'margin-left: -10px', 
                 tags$a(tags$b('CIAAW data on natural isotopic composition'), target = '_blank',
                        href = paste0('https://ciaaw.org/', tolower(SelectedElem()), '.htm'))),
              tags$hr(), dataTableOutput(session$ns('IUPAC_Table')), tags$hr(),
              tags$div(#style = 'font-size: 11px;',
                tags$b('Notes:'), 
                tags$ul(style = 'list-style-position: outside; font-size:0.9em',
                        HTML(paste0(lapply(Notes, FUN = function(x) return(as.character(tags$li(x)))), collapse = ''))))
              ))
          }
        })
      }
      
      # Isotopic Composition CRMs
      observe({
        req(SelectedElem())
        ShowAvailCRMsServer(
          id = 'IsoCompCRM', id2 = id, devMode = devMode, SelectedElem = SelectedElem, 
          gnrlClss = 'List_IsoCompCRM', key = 'Certified', Actionate = reactive(input$AcLnk_IsoCompCRM),
          CRMproducers = loadFromDataBase('CRMproducers'), 
          CRMsInfoTable = loadFromDataBase('IsoCompCRM_Info', SelectedElem()), 
          CRMsDataTable = loadFromDataBase('IsoCompCRM_DataIR', SelectedElem()))
      })
       
      # Calibration solution and high purity materials
      observe({
        req(SelectedElem())
        ShowAvailCRMsServer(
          id = 'CalibraCRM', id2 = id, devMode = devMode, SelectedElem = SelectedElem,
          gnrlClss = 'List_CalibraCRM', key = 'Reported', Actionate = reactive(input$AcLnk_CalibraCRM),
          CRMproducers = loadFromDataBase('CRMproducers'),
          MeasuReports = loadFromDataBase('MeasuReports'), MeasRepoAuth = loadFromDataBase('MeasRepoAuth'),
          CRMsInfoTable = loadFromDataBase('CalibraCRM_Info', SelectedElem()),
          CRMsDataTable = loadFromDataBase('CalibraCRM_DataIR', SelectedElem()))
      })
      
      # Matrix CRMs
      observe({
        req(SelectedElem())
        ShowAvailCRMsServer(
          id = 'MatrixCRM', id2 = id, devMode = devMode, SelectedElem = SelectedElem, 
          gnrlClss = 'List_MatrixCRM', key = 'Reported', Actionate = reactive(input$AcLnk_MatrixCRM),
          CRMproducers = loadFromDataBase('CRMproducers'),
          MeasuReports = loadFromDataBase('MeasuReports'), MeasRepoAuth = loadFromDataBase('MeasRepoAuth'),
          CRMsInfoTable = loadFromDataBase('MatrixCRM_Info', SelectedElem()), 
          CRMsDataTable = loadFromDataBase('MatrixCRM_DataIR', SelectedElem()))
      })
      
      NoElement <- eventReactive(
        c(input$AcLnk_MatrixCRM, input$AcLnk_CalibraCRM, input$AcLnk_IsoCompCRM, SelectedElem()), ignoreInit = TRUE, {
        if (is.null(SelectedElem())) {
          return(tags$b(style = 'color: red;', "Please select an element from the periodic table.", tags$hr()))
        } else {return()}})
      
      output$NoElement     <- renderUI(NoElement())
      output$SelectedElem  <- renderText(SelectedElem())
      output$IUPAC_CIAAW   <- renderUI(IUPAC_CIAAW())
      output$IUPAC_Table   <- renderDataTable(IUPAC_Table(), options = list(dom = 't'), rownames = FALSE, selection = "single")
    }
  )
}