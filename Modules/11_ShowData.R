ShowDataUI <- function(id, label = "Counter", FlTy = 'Excel') {
  ns <- NS(id)
  column(
    style = 'margin-left: -20px',
    5, tags$br(), uiOutput(ns('brwz')),
    h2('Element ', tags$b(textOutput(ns('SelectedElem'), inline = TRUE))), 
    niceSeparator(),
    h4(style = 'margin-left: 10px', tags$b('Certified reference materials in the database')), 
    withSpinner(uiOutput(ns('NoElement')), type = 6, color = '#808080', proxy.height = '100px'),
    (div(
      style = 'margin: 20px', class = 'CRMsActionLinks',
      ShowAvailCRMsUI(ns("IsoCompCRM")),
      ShowAvailCRMsUI(ns("CalibraCRM")),
      ShowAvailCRMsUI(ns("MatrixCRM"))
    )),
    niceSeparator(),
    h4(style = 'margin-left: 10px', tags$b('CIAAW data on natural isotopic composition')), 
    withSpinner(uiOutput(ns('NoElement2')), type = 6, color = '#808080', proxy.height = '100px'),
    div(style = 'margin: 20px', uiOutput(ns('IUPAC_CIAAW')))
  )
}

ShowDataServer <- function(id, devMode, SelectedElem, CRMproducers, MeasuReports, MeasRepoAuth) {
  moduleServer(
    id,
    function(input, output, session) {
      output$brwz <- renderUI(if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pause module'))))
      observeEvent(input$brwz, browser())
      
      observe(toggleElement(condition = nrow(isotopes()) > 1, selector = 'div.CRMsActionLinks',
                            anim = TRUE, animType = 'fade', time = 1))
      
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
              tags$a('See full information on the IUPAC CIAAW webpage.', target = '_blank',
                     href = paste0('https://ciaaw.org/', tolower(SelectedElem()), '.htm')),
              tags$hr(), dataTableOutput(session$ns('IUPAC_Table')), tags$hr(),
              tags$div(style = 'font-size: 0.7vw;',
                tags$b('Notes:'), 
                tags$ul(style = 'list-style-position: outside; font-size:0.9em',
                        HTML(paste0(lapply(Notes, FUN = function(x) return(as.character(tags$li(x)))), collapse = ''))))
              ))
          }
        })
      }
      
      
      observe({
        # Isotopic Composition CRMs
        ShowAvailCRMsServer(
          id = 'IsoCompCRM', id2 = id, devMode = devMode, SelectedElem = SelectedElem,
          icono = icon('atom'), description = ' With certified values for isotopic composition', 
          gnrlClss = 'List_IsoCompCRM', key = 'Certified',
          CRMproducers = CRMproducers,
          Info_TableName = 'IsoCompCRM_Info', CRMsData_TableName = 'IsoCompCRM_DataIR')
       
        # Calibration solution and high purity materials
        ShowAvailCRMsServer(
          id = 'CalibraCRM', id2 = id, devMode = devMode, SelectedElem = SelectedElem,
          icono = icon('flask'), description = ' Calibration solutions and high purity solids',
          gnrlClss = 'List_CalibraCRM', key = 'Reported',
          CRMproducers = CRMproducers, MeasuReports = MeasuReports, MeasRepoAuth = MeasRepoAuth,
          Info_TableName = 'CalibraCRM_Info', CRMsData_TableName = 'CalibraCRM_DataIR')
        
        # Matrix CRMs
        ShowAvailCRMsServer(
          id = 'MatrixCRM', id2 = id, devMode = devMode, SelectedElem = SelectedElem,
          icono = icon('carrot'), description = ' Matrix CRMs',
          gnrlClss = 'List_MatrixCRM', key = 'Reported',
          CRMproducers = CRMproducers, MeasuReports = MeasuReports, MeasRepoAuth = MeasRepoAuth,
          Info_TableName = 'MatrixCRM_Info', CRMsData_TableName = 'MatrixCRM_DataIR')
      })
      
      NoElement <- reactive({
        if (length(SelectedElem()) == 0) {
          return(tags$b(style = 'color: gray; margin-left: 30px', "Please select an element from the periodic table."))
        } else {return()}})
      
      output$NoElement     <- output$NoElement2 <- renderUI(NoElement())
      output$SelectedElem  <- renderText(SelectedElem())
      output$IUPAC_CIAAW   <- renderUI(IUPAC_CIAAW())
      output$IUPAC_Table   <- renderDataTable(IUPAC_Table(), options = list(dom = 't'), rownames = FALSE, selection = "single")
    }
  )
}