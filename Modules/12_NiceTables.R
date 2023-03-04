ShowAvailCRMsUI <- function(id) {
  ns <- NS(id)
  tags$div(uiOutput(ns('brwz')), uiOutput(ns('UI_CRM_List')), tags$br())
}

ShowAvailCRMsServer <- function(id, id2, devMode, SelectedElem, CRMproducers, MeasuReports = NULL, Actionate,
                                CRMsInfoTable, CRMsDataTable, gnrlClss, key) {
  moduleServer(
    id,
    function(input, output, session) {
      output$brwz <- renderUI(if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pause submodule'))))
      observeEvent(input$brwz, browser())
      
      NoInfo <- reactive(paste0('There are no entries yet for ', tolower(SelectedElem()), ' in this category.'))
      
      UI_CRM_List <- reactive({
        req(SelectedElem)
        if (nrow(CRMsInfoTable) >= 1) {
          ElmntToPrnt <- div(tags$hr(), DT::dataTableOutput(session$ns('Table_CRM_List')), tags$hr())
        } else {
          ElmntToPrnt <- NoInfo()
        }
        return(hidden(tags$div(class = gnrlClss, style = 'margin-left: 10px;', ElmntToPrnt)))
      })
      
      Table_CRM_List <- reactive({# From https://stackoverflow.com/a/70763580/7612904
        DT <- copy(CRMsInfoTable[, c("Producer", "CRM.name", "Lot", "Description")])
        setDT(DT)
        inputName <- paste0(id2, '-', id, '-SelectedCRM')
        DT[, inputId := CRM.name][
          , Details := as.character(
            actionLink(
              inputId = session$ns(inputId), label = 'Show details', class = 'CRMOption',
              onclick = sprintf(paste0("Shiny.setInputValue(id = '", inputName, "', value = '", inputId, "');")))),
          by = inputId][, inputId := NULL]
      })
      
      observeEvent(input$SelectedCRM, ignoreInit = TRUE, {
        IndivData <- CRMsInfoTable[CRMsInfoTable$CRM.name == input$SelectedCRM, ]
        Producer  <- CRMproducers[CRMproducers$Producer == IndivData$Producer, ]
        
        if (TRUE) {
          showModal(modalDialog(
            title = HTML(paste0(key, ' isotopic composition of ', tags$b(input$SelectedCRM))), easyClose = TRUE,
            crmSummary(Producer = Producer, Data = IndivData, key = key),
            tags$hr(),
            tableOutput(session$ns("Table_CRM_IndivData")),
            tags$hr(),
            if (key == 'Reported') StudySummary(MeasuReports[MeasuReports$Report.DOI == IndivData$Report.DOI, ])))
        }
      })
      
      output$Table_CRM_IndivData <- renderTable({
        req(input$SelectedCRM)
        if (key == 'Certified') {
          DT <- copy(CRMsDataTable[
            CRMsDataTable$CRM.name == input$SelectedCRM, 
            c('Isotopic.ratio', 'Type', 'Value', 'Uncertainty', 'UncertType', 'k.factor')])
        } else {
          DT <- copy(CRMsDataTable[
            CRMsDataTable$CRM.name == input$SelectedCRM, 
            c('Isotopic.ratio', 'Value', 'Uncertainty', 'UncertType', 'k.factor', 'Calibration.standard')])
        }
        
        DT$Value <- as.character(DT$Value)
        DT$Uncertainty <- as.character(DT$Uncertainty)
        return(DT)
      })
      
      output$UI_CRM_List <- renderUI(UI_CRM_List())
      output$Table_CRM_List <- DT::renderDataTable(Table_CRM_List(), escape = FALSE)#sanitize.text.function = function(x) {x})
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
      <th class="tg-value">', Data$CRM.name, '</th></tr>
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