ShowAvailCRMsUI <- function(id) {
  ns <- NS(id)
  tags$div(
    uiOutput(ns('AcLnk')), #actionLink(inputId = ns('AcLnk2'), icon = icon, style = 'display: inline;', tags$b(description)),
    uiOutput(ns('brwz')),
    uiOutput(ns('UI_CRM_List')),# %>% withSpinner(type = 6, color = '#808080', proxy.height = '50px'),
    tags$br())
}

ShowAvailCRMsServer <- function(id, id2, devMode, SelectedElem, icono, description,
                                CRMproducers, MeasuReports = NULL, MeasRepoAuth = NULL,
                                Info_TableName, CRMsData_TableName, gnrlClss, key) {
  moduleServer(
    id,
    function(input, output, session) {
      output$brwz <- renderUI(if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pause submodule'))))
      observeEvent(input$brwz, browser())
      
      AcLnk <- eventReactive(SelectedElem(), ignoreInit = TRUE, {
        actionLink(inputId = session$ns('AcLnk'), icon = icono, style = 'display: inline;', tags$b(description))
      })
      output$AcLnk <- renderUI(AcLnk())
      
      observeEvent(input$AcLnk, {
        showElement(selector = paste0('div.', gnrlClss), anim = TRUE, animType = 'fade', time = 0.4)
      })
      
      
      
      
      CRMsInfoTable <- eventReactive(input$AcLnk, loadFromDataBase(Info_TableName, element = SelectedElem()))
      
      
      NoInfo <- reactive(paste0('There are no entries yet for ', tolower(SelectedElem()), ' in this category.'))
      
      UI_CRM_List <- reactive({
        # req(input$AcLnk)
        req(SelectedElem)
        if (nrow(CRMsInfoTable()) >= 1) {
          ElmntToPrnt <- div(tags$hr(), DT::dataTableOutput(session$ns('Table_CRM_List')), tags$hr())
        } else {
          ElmntToPrnt <- NoInfo()
        }
        return(tags$div(class = gnrlClss, style = 'margin-left: 10px;', ElmntToPrnt))
      })
      
      Table_CRM_List <- reactive({# From https://stackoverflow.com/a/70763580/7612904
        # req(input$AcLnk)
        DT <- copy(CRMsInfoTable()[, c("Producer", "CRM_name", "Lot", "Description")])
        setDT(DT)
        inputName <- paste0(id2, '-', id, '-SelectedCRM')
        DT[, inputId := CRM_name][
          , Details := as.character(
            actionLink(
              inputId = session$ns(inputId), label = 'Show details', class = 'CRMOption',
              onclick = sprintf(paste0(#"Shiny.setInputValue(\"", session$ns("SelectedCRM"), "\", null, {priority: \"event\"});",
                                       "Shiny.setInputValue(id = '", inputName, "', value = '", inputId, "', {priority: 'event'});")))),
          by = inputId][, inputId := NULL]
      })
      
      observeEvent(input$SelectedCRM, ignoreInit = TRUE, {
        IndivData <- CRMsInfoTable()[CRMsInfoTable()$CRM_name == input$SelectedCRM, ]
        Producer  <- CRMproducers[CRMproducers$Producer == IndivData$Producer, ]
        
        if (TRUE) {
          showModal(modalDialog(
            title = HTML(paste0(key, ' isotopic composition of ', tags$b(input$SelectedCRM))), easyClose = TRUE,
            crmSummary(Producer = Producer, Data = IndivData, key = key),
            tags$hr(),
            withSpinner(
              tableOutput(session$ns("Table_CRM_IndivData")), 
              type = 6, color = '#808080', proxy.height = '50px'),
            tags$hr(),
            if (key == 'Reported') StudySummary(MeasuReports[MeasuReports$Report.DOI == IndivData$Report.DOI, ])))
        }
      })
      
      Table_CRM_IndivData <- eventReactive(input$SelectedCRM, {
        # CRMsDataTable <- loadFromDataBase(CRMsData_TableName, SelectedElem())
        CRMsDataTable <- loadFromDataBase(CRMsData_TableName, CRM = input$SelectedCRM)
        if (key == 'Certified') {
          columns <- c('IsotopeAmountRatio', 'Type', 'Unit', 'Value', 'Uncertainty', 'UncertType', 'k_factor')
        } else {
          columns <- c('IsotopeAmountRatio', 'Unit', 'Value', 'Uncertainty', 'UncertType', 'k_factor', 'Calibration_standard')
        }
        DT <- copy(CRMsDataTable[, columns])
        DT$Value <- as.character(DT$Value)
        DT$Uncertainty <- as.character(DT$Uncertainty)
        return(DT)
      })
      output$Table_CRM_IndivData <- renderTable(Table_CRM_IndivData())
      
      output$UI_CRM_List <- renderUI(UI_CRM_List())
      output$Table_CRM_List <- DT::renderDataTable(Table_CRM_List(), escape = FALSE, selection = "single", server = TRUE)
    }
  )
}

crmSummary <- function(Producer, Data, key) {
  Info <- HTML(paste0(
    '<table class="tg" style="margin-left: 100px;"><tbody>
      <tr><td class="tg-field">Producer:</td>
      <td class="tg-value"><b>', Data$Producer, '</b></td></tr>
      <tr"><td class="tg-field"> </td>
      <td class="tg-value"><a href="', Producer$URL, '" target=_blank">', Producer$ProducerFullName, '</a></td></tr>
      <tr style="height: 10px !important;"><td colspan="2"></td></tr>
      <tr><th class="tg-field">CRM name:</th>
      <th class="tg-value">', Data$CRM_name, '</th></tr>
      <tr><th class="tg-field">Description:</th>
      <th class="tg-value">', Data$Description, '</th></tr>',
    ifelse(key == 'Certified', 
           paste0('<tr><th class="tg-field">Presentation:</th><th class="tg-value">', Data$Presentation, '</th></tr>'), ''),
    '<tr><td class="tg-field">Lot:</td>
      <td class="tg-value">', Data$Lot, '</td></tr>',
    ifelse(key == 'Certified', 
           paste0('<tr><td class="tg-field">URL:</td>
                  <td class="tg-0lax"> <a href="', Data$URL, '" target=_blank">', Data$URL, '</a></td></tr>'), ''), 
    '</tbody></table>'))
  return(Info)
}

StudySummary <- function(Study) {
  Info <- HTML(paste0(
    '<table class="tg"><tbody>
      <tr><td class="tg-field">Report:</td>
      <td class="tg-value">', Study$Title, '</td></tr>
      <tr style="height: 10px !important;"><td colspan="2"></td></tr>
      <tr><th class="tg-field">Authors:</th>
      <th class="tg-value">', Study$Authors, '</th></tr>
      <tr><th class="tg-field">Journal:</th>
      <th class="tg-value">', Study$Journal, '<b>', Study$Year, '</b>. Issue ', Study$Issue, ', pp. ', Study$Pages, '.</th></tr>
      <tr><td class="tg-field">Technique, instrument:</td>
      <td class="tg-value">', Study$Technique, ', ', Study$Instrument, '</td></tr>
      <tr><td class="tg-field">DOI/URL:</td>
      <td class="tg-0lax"> <a href="', Study$Report.DOI, '" target=_blank">', Study$Report.DOI, '</a></td></tr>
    </tbody></table>'))
  return(Info)
}