ShowDataUI <- function(id, label = "Counter", FlTy = 'Excel') {
  ns <- NS(id)
  column(
    4, tags$hr(), tags$hr(), uiOutput(ns('brwz')),
    h3('Element: ', tags$b(textOutput(ns('SelectedElem'), inline = TRUE))), 
    div(style = 'margin-left: 20px', uiOutput(ns('IUPAC_CIAAW'))),
    tags$hr(),
           
    actionLink(inputId = ns('AcLnk_IsoComCRM'), h4('CRMs certified for isotopic composition.')),
    uiOutput(ns('InfoIsoCompCRM')), tags$hr(),
    actionLink(inputId = ns('AcLnk_CalSolCRM'), h4('Calibration solution CRMs with Isotopic Composition data.')), 
    uiOutput(ns('InfoCalibraCRM')), tags$hr(),
    actionLink(inputId = ns('AcLnk_MatrixCRM'), h4('Matrix CRMs with Isotopic Composition data.')), 
    uiOutput(ns('InfoMatrixCRM')))
}

ShowDataServer <- function(id, devMode, SelectedElem) {
  moduleServer(
    id,
    function(input, output, session) {
      output$brwz <- renderUI(
        if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pause module'))))
      observeEvent(input$brwz, browser())
      
      isotopes <- eventReactive(
        SelectedElem(), ignoreInit = TRUE, {
          CIAAW_NatIsotAbunTable[
            which(CIAAW_NatIsotAbunTable$Element == tolower(SelectedElem())), 
            c("Isotope", "Relative.abundance", "Notes", "Interval")]
        }
        #ciaaw.mass.2016[which(ciaaw.mass.2016$element == tolower(SelectedElem())), ]
        )
      
      
      
      delay(0, {hide(selector = "IsoComCRM"); hide(selector = "CalSolCRM"); hide(selector = "MatrixCRM")})
      observe({
        # toggleElement(condition = nrow(isotopes()) > 1, id = 'IUPAC_Table', anim = TRUE, animType = 'fade', time = 1)
        toggleElement(condition = nrow(isotopes()) > 1, id = 'IsoComCRM', anim = TRUE, animType = 'fade', time = 1)
        toggleElement(condition = nrow(isotopes()) > 1, id = 'CalSolCRM', anim = TRUE, animType = 'fade', time = 1)
        toggleElement(condition = nrow(isotopes()) > 1, id = 'MatrixCRM', anim = TRUE, animType = 'fade', time = 1)
      })
            
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
            tags$b('CIAAW data on natural isotopic composition'),
            tableOutput(session$ns('IUPAC_Table')),
            h6(tags$b('Notes:'), 
               tags$ul(HTML(paste0(lapply(Notes, FUN = function(x) return(as.character(tags$li(x)))), collapse = ''))))
            ))
        }
      })
      
      IsoCompCRM <- eventReactive(
        SelectedElem(), ignoreInit = TRUE, {
          INITI_IsoComCRM_Info[
            which(INITI_IsoComCRM_Info$Element == SelectedElem()), 
            c("CRM.name", "Lot", "Producer", "Description")]
        })
      
      InfoIsoCompCRM <- reactive({
        if (nrow(IsoCompCRM()) >= 1) {
          IsoCompCRM <- IsoCompCRM()
          IsoCompCRMList <- vector(mode = 'list', length = nrow(IsoCompCRM))
          for (i in 1:nrow(IsoCompCRM)) {
            inputId <- sub(" ", "_", paste(IsoCompCRM$CRM.name[i], IsoCompCRM$Lot[i], sep = '_'))
            label <- paste0(IsoCompCRM$Producer[i], ', ', IsoCompCRM$CRM.name[i], '. Lot ', IsoCompCRM$Lot[i])
            IsoCompCRMList[[i]] <- actionLink(inputId = session$ns(inputId), label = label, class = "IsoCompCRM")
          }
          return(tags$div(IsoCompCRMList))
        } else {
          return(h6(tags$b('There is no entries for', SelectedElem(), 'isotopic composition CRMs in the database yet.')))
        }
      })
      
      output$SelectedElem  <- renderText(SelectedElem())
      output$IUPAC_CIAAW <- renderUI(IUPAC_CIAAW())
      output$IUPAC_Table   <- renderTable(IUPAC_Table())
      output$InfoIsoCompCRM <- renderUI(InfoIsoCompCRM())
    }
  )
}