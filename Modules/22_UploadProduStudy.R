UploadProduStudyUI <- function(id, key) {
  ns <- NS(id)
  tags$div(
    uiOutput(ns('brwz')),
    uiOutput(ns('selectProducer')),
    conditionalPanel("input.SelectedProducer == 'Other'", ns = ns, uiOutput(ns('NewProdInfo'))),
    tags$br())
}

UploadProduStudyServer <- function(id, id2, devMode, key, TableProducers) {
  moduleServer(
    id,
    function(input, output, session) {
      output$brwz <- renderUI(if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pause submodule'))))
      observeEvent(input$brwz, browser())
      
       {
         restartProdList <- reactiveVal(0)
         selectProducer <- eventReactive(restartProdList(), {
           selectizeInput(
             inputId = session$ns('SelectedProducer'), label = tags$lib('Select the CRM producer:'), width = '50%',
             choices = as.list(
               setNames(c(TableProducers$Producer, 'Other'),
                        c(paste0(TableProducers$Producer, ', ', TableProducers$ProducerFullName), 'Other...'))),
             options = list(placeholder = 'Write or select an option below', onInitialize = I('function() { this.setValue(""); }'))
           )
         })
         output$selectProducer <- renderUI(selectProducer())
         
        fieldsCrmProd <- colnames(TableProducers)
        observe({
          req(input$SelectedProducer)
          if (input$SelectedProducer == 'Other') {
            countries <- countrycode::codelist$country.name.en
            # countrycode('Colombia','country.name', 'iso2c')
            showModal(modalDialog(
              title = tags$b('Provide the following information about the CRM producer:'),
              footer = NULL, fade = TRUE, easyClose = FALSE,
              tags$div(
                id = 'inlineTOP', style = 'margin-left: 10px;', tags$br(),
                textInput(session$ns(fieldsCrmProd[1]), label = ReqField('Short name:'), placeholder = 'Acronym', width = '180px'),
                textInput(session$ns(fieldsCrmProd[2]), label = ReqField('Full name:'), width = '180px'),
                textInput(session$ns(fieldsCrmProd[3]), label = NonReqField('Alternative name:'),
                          placeholder = '(Optional)', width = '180px'),
                selectizeInput(session$ns(fieldsCrmProd[4]), label = ReqField('Country'), choices = countries, width = '180px',
                               options = list(placeholder = 'Write or select an option below:',
                                              onInitialize = I('function() { this.setValue(""); }'))),
                textInput(session$ns(fieldsCrmProd[6]), label = ReqField('Website:'), placeholder = 'A valid URL', width = '180px'),
                tags$br(), uiOutput(session$ns('BadNewProducer'))
              ),
              splitLayout(
                actionButton(session$ns('createNewProducer'), label = tags$b('Record new CRM producer')),
                actionButton(session$ns('cancelNewProducer'), label = tags$b('Cancel')))
            ))
          }
        })

        observeEvent(input$createNewProducer, {
          if (are.null.empty(c(input$Producer, input$ProducerFullName, input$Country, input$URL))) {
            output$BadNewProducer <- renderUI(tags$b(style = 'color: red;', 'Please fill in all required fields.'))
          } else {
            removeModal()
            output$NewProdInfo <- renderUI(tags$div(
              tags$b('New CRM producer: '), input$Producer, ', ',
              tags$a(input$ProducerFullName, href = input$URL, target = '_blank')))
          }
        })


        observeEvent(input$cancelNewProducer, {
          removeModal()
          restartProdList(restartProdList() + 1)# runjs("Shiny.setInputValue('UploadData-SelectedProducer', '');")
        })
      }

      return(reactive(input$SelectedProducer))
      
      
      
      # NoInfo <- reactive(paste0('There are no entries yet for ', tolower(SelectedElem()), ' in this category.'))
      # 
      # UI_CRM_List <- reactive({
      #   req(SelectedElem)
      #   if (nrow(CRMsInfoTable) >= 1) {
      #     ElmntToPrnt <- div(tags$hr(), DT::dataTableOutput(session$ns('Table_CRM_List')), tags$hr())
      #   } else {
      #     ElmntToPrnt <- NoInfo()
      #   }
      #   return(hidden(tags$div(class = gnrlClss, style = 'margin-left: 10px;', ElmntToPrnt)))
      # })
      # 
      # Table_CRM_List <- reactive({# From https://stackoverflow.com/a/70763580/7612904
      #   DT <- copy(CRMsInfoTable[, c("Producer", "CRM.name", "Lot", "Description")])
      #   setDT(DT)
      #   inputName <- paste0(id2, '-', id, '-SelectedCRM')
      #   DT[, inputId := CRM.name][
      #     , Details := as.character(
      #       actionLink(
      #         inputId = session$ns(inputId), label = 'Show details', class = 'CRMOption',
      #         onclick = sprintf(paste0("Shiny.setInputValue(\"", session$ns("SelectedCRM"), "\", null, {priority: \"event\"});",
      #                                  "Shiny.setInputValue(id = '", inputName, "', value = '", inputId, "');")))),
      #     by = inputId][, inputId := NULL]
      # })
      # 
      # observeEvent(input$SelectedCRM, ignoreInit = TRUE, {
      #   IndivData <- CRMsInfoTable[CRMsInfoTable$CRM.name == input$SelectedCRM, ]
      #   Producer  <- CRMproducers[CRMproducers$Producer == IndivData$Producer, ]
      #   
      #   if (TRUE) {
      #     showModal(modalDialog(
      #       title = HTML(paste0(key, ' isotopic composition of ', tags$b(input$SelectedCRM))), easyClose = TRUE,
      #       crmSummary(Producer = Producer, Data = IndivData, key = key),
      #       tags$hr(),
      #       tableOutput(session$ns("Table_CRM_IndivData")),
      #       tags$hr(),
      #       if (key == 'Reported') StudySummary(MeasuReports[MeasuReports$Report.DOI == IndivData$Report.DOI, ])))
      #   }
      # })
      # 
      # output$Table_CRM_IndivData <- renderTable({
      #   req(input$SelectedCRM)
      #   if (key == 'Certified') {
      #     DT <- copy(CRMsDataTable[
      #       CRMsDataTable$CRM.name == input$SelectedCRM, 
      #       c('Isotopic.ratio', 'Type', 'Value', 'Uncertainty', 'UncertType', 'k.factor')])
      #   } else {
      #     DT <- copy(CRMsDataTable[
      #       CRMsDataTable$CRM.name == input$SelectedCRM, 
      #       c('Isotopic.ratio', 'Value', 'Uncertainty', 'UncertType', 'k.factor', 'Calibration.standard')])
      #   }
      #   
      #   DT$Value <- as.character(DT$Value)
      #   DT$Uncertainty <- as.character(DT$Uncertainty)
      #   return(DT)
      # })
      # 
      # output$UI_CRM_List <- renderUI(UI_CRM_List())
      # output$Table_CRM_List <- DT::renderDataTable(Table_CRM_List(), escape = FALSE)#sanitize.text.function = function(x) {x})
    }
  )
}