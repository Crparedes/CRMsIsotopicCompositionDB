UploadProducerUI <- function(id) {
  ns <- NS(id)
  tags$div(
    tags$hr(), uiOutput(ns('brwz')),
    uiOutput(ns('selectKreator')),
    conditionalPanel("input.selectedKreator == 'Other'", ns = ns, uiOutput(ns('NewProdInfo'))),
    tags$br())
}

UploadProducerServer <- function(id, id2, devMode, TableKreators, DataEntryClerk) {
  moduleServer(
    id,
    function(input, output, session) {
      output$brwz <- renderUI(if(devMode()) return(actionButton(session$ns('brwz'), class = 'PauBtn', 
                                                                label = tags$b('Pause and inspect submodule'))))
      observeEvent(input$brwz, browser())
      
      output$Subbrwz <- renderUI(if(devMode()) return(actionButton(session$ns('Subbrwz'), class = 'PauBtn', 
                                                                label = tags$b('Pause and inspect modal Box'))))
      observeEvent(input$Subbrwz, browser())
      
       {
         restartKreaList <- reactiveVal(0)
         selectKreator <- eventReactive(restartKreaList(), {
           selectizeInput(
             inputId = session$ns('selectedKreator'), label = tags$lib('Select the producer of the CRM:'), width = '85%',
             choices = as.list(setNames(c(TableKreators$Producer, 'Other'), 
                                        c(paste0(TableKreators$Producer, ', ', TableKreators$ProducerFullName), 'Other...'))),
             options = list(placeholder = 'Write or select an option below', onInitialize = I('function() { this.setValue(""); }'))
           )
         })
         output$selectKreator <- renderUI(selectKreator())
         
         fieldsKreator <- colnames(TableKreators)
         
        observe({
          req(input$selectedKreator)
          if (input$selectedKreator == 'Other') {
            countries <- countrycode::codelist$country.name.en
            c(ReqField('Authors:'), ReqField('Year:'), NonReqField('Journal:'), ReqField('Issue:'), ReqField('Pages:'),
              ReqField('Title:'), ReqField('Technique:'), ReqField('Instrument used:'))
            
            
            NewEntryInputs <- list(
              textInput(session$ns(fieldsKreator[1]), label = ReqField('Short name:'), placeholder = 'Acronym', width = '180px'),
              textInput(session$ns(fieldsKreator[2]), label = ReqField('Full name:'), width = '180px'),
              textInput(session$ns(fieldsKreator[3]), label = NonReqField('Alternative name:'),
                        placeholder = '(Optional)', width = '180px'),
              textInput(session$ns(fieldsKreator[6]), label = ReqField('Website:'), value = 'https://', width = '180px'),
              selectizeInput(session$ns(fieldsKreator[4]), label = ReqField('Country'), choices = countries, width = '180px',
                             options = list(placeholder = 'Write or select an option below:',
                                            onInitialize = I('function() { this.setValue(""); }'))))

            showModal(modalDialog(
              title = tags$b('Provide the following information about the CRM producer:'),
              footer = NULL, fade = TRUE, easyClose = FALSE,
              tags$div(id = 'inlineTOP', style = 'margin-left: 10px;', tags$br(), NewEntryInputs),
              tags$br(), uiOutput(session$ns('BadNewKreator')),
              uiOutput(session$ns('Subbrwz')),
              splitLayout(
                actionButton(session$ns('createNewKreator'), label = tags$b('Record new CRM producer')),
                actionButton(session$ns('cancelNewKreator'), label = tags$b('Cancel writing to database')))
            ))
          }
        })

        observeEvent(input$createNewKreator, {
          Incompleto <- are.null.empty(c(input$Producer, input$ProducerFullName, input$Country, input$URL))
          
          if (Incompleto) {
            output$BadNewKreator <- renderUI(tags$b(style = 'color: red;', 'Please fill in all required fields.'))
          } else {
            if (input$Producer %in% TableKreators$Producer) {
              output$BadNewKreator <- renderUI(
                tags$b(style = 'color: red;', 'ERROR: It seems that', isolate(input$Producer), 'is already recorded in the data base.'))
            } else {
              removeModal()
              dataNew <- sapply(fieldsKreator, function(x) return(input[[x]]))
              dataNew$CountryAbreviation <- countrycode(dataNew$Country, 'country.name', 'iso2c')
              dataNew$DataEntryClerk <- DataEntryClerk
              saveData(tableName = 'CRMproducers', data = dataNew)
              
              output$NewProdInfo <- renderUI(tags$div(
                tags$b('New CRM producer in the database: '),
                tags$a(input$Producer, ', ', paste0(input$ProducerFullName), href = input$URL, target = '_blank')))
            }
          }
        })


        observeEvent(input$cancelNewKreator, {
          removeModal()
          restartKreaList(restartKreaList() + 1)# runjs("Shiny.setInputValue('UploadData-selectedKreator', '');")
        })
      }

      return(reactive(input$selectedKreator))
    }
  )
}