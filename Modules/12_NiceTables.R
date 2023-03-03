crmSummary <- function(Producer, Data, Study = NULL) {
  Info <- HTML(paste0(
    '<table class="tg""><tbody>
      <tr><td class="tg-field">Producer:</td>
      <td class="tg-value"><b>', Data$Producer, '</b></td></tr>
      <tr"><td class="tg-field"> </td>
      <td class="tg-value"><a href="', Producer$URL, '" target=_blank">', Producer$ProducerFullName, '</a></td></tr>
      <tr style="height: 10px !important;"><td colspan="2"></td></tr>
      <tr><th class="tg-field">CRM name:</th>
      <th class="tg-value">', Data$CRM.name, '</th></tr>
      <tr><th class="tg-field">Description:</th>
      <th class="tg-value">', Data$Description, '</th></tr>
      <tr><th class="tg-field">Presentation:</th>
      <th class="tg-value">', Data$Presentation, '</th></tr>
      <tr><td class="tg-field">Lot:</td>
      <td class="tg-value">', Data$Lot, '</td></tr>
      <tr><td class="tg-field">URL:</td>
      <td class="tg-0lax"> <a href="', Data$URL, '" target=_blank">', Data$URL, '</a></td></tr>
      </tbody></table>'))
  return(Info)
}

ShowAvailCRMsUI <- function(id) {
  ns <- NS(id)
  tags$div(uiOutput(ns('brwz')), uiOutput(ns('UI_CRM_List')), tags$br())
}

ShowAvailCRMsServer <- function(id, devMode, SelectedElem, CRMsInfoTable, CRMsDataTable, gnrlClss) {
  moduleServer(
    id,
    function(input, output, session) {
      output$brwz <- renderUI(if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pause submodule'))))
      observeEvent(input$brwz, browser())
      
      {
        IsoCompCRM <- eventReactive(SelectedElem(), ignoreInit = TRUE, {
        # IsoCompCRM <- eventReactive(SelectedElem(), ignoreInit = TRUE, {
          ShowTable <- INITI_IsoCompCRM_Info[grep(tolower(SelectedElem()), INITI_IsoCompCRM_Info$Elements, value = FALSE),
                                             c("Producer", "CRM.name", "Lot", "Description")]
          return(ShowTable)
        })
        
        UI_CRM_List <- reactive({
          req(SelectedElem)
          if (nrow(CRMsInfoTable) >= 1) {
            ElmntToPrnt <- tableOutput(session$ns('Table_CRM_List'))
          } else {ElmntToPrnt <- NoInfo()}
          return(hidden(tags$div(class = gnrlClss, style = 'margin-left: 8px;', ElmntToPrnt)))
        })
        
        Table_CRM_List <- reactive({# Adapted from https://stackoverflow.com/a/70763580/7612904
          DT <- copy(CRMsInfoTable)
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
      
      output$UI_CRM_List <- renderUI(UI_CRM_List())
      output$Table_CRM_List <- renderTable(Table_CRM_List(), sanitize.text.function = function(x) {x})
    }
  )
}