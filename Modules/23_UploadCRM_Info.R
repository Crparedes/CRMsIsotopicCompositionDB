UploadMaterialInfoUI <- function(id, key) {
  ns <- NS(id)
  tags$div(
    tags$hr(), uiOutput(ns('brwz')),
    uiOutput(ns('selectKreator')),
    conditionalPanel("input.selectedKreator == 'Other'", ns = ns, uiOutput(ns('NewProdInfo'))),
    tags$br())
}

UploadMaterialInfoServer <- function(id, id2, devMode, key, Producer, TableInfos, DataEntryClerk) {
  moduleServer(
    id,
    function(input, output, session) {
      output$brwz <- renderUI(if(devMode()) return(actionButton(session$ns('brwz'), class = 'PauBtn', 
                                                                label = tags$b('Pause and inspect submodule'))))
      observeEvent(input$brwz, browser())
      
      # item <- reactive(ifelse(
      #   key == 'Producer', 'producer of the CRM', 'measurement report'))
      # choiceValues <- reactive(ifelse(
      #   key == 'Producer', return(c(TableKreators$Producer, 'Other')), return(c('Other', TableKreators$Report.DOI))))
      # choiceNames <- reactive(ifelse(
      #   key == 'Producer', 
      #   return(c(paste0(TableKreators$Producer, ', ', TableKreators$ProducerFullName), 'Other...')),
      #   return(c('New report...', 
      #            paste0(TableKreators$Authors, ', ', TableKreators$Year, ', ', 
      #                   TableKreators$Journal, ' No, ', TableKreators$Issue, '. pp ', TableKreators$Pages)))))
      # 
      #  {
      #    restartKreaList <- reactiveVal(0)
      #    selectKreator <- eventReactive(restartKreaList(), {
      #      selectizeInput(
      #        inputId = session$ns('selectedKreator'), label = tags$lib(paste0('Select the ', item(), ':')), width = '85%',
      #        choices = as.list(setNames(choiceValues(), choiceNames())),
      #        options = list(placeholder = 'Write or select an option below', onInitialize = I('function() { this.setValue(""); }'))
      #      )
      #    })
      #    output$selectKreator <- renderUI(selectKreator())
      #    
      #   observe({
      #     req(input$selectedKreator)
      #     if (input$selectedKreator == 'Other') {
      #       countries <- countrycode::codelist$country.name.en
      #       
      #         c(ReqField('Authors:'), ReqField('Year:'), NonReqField('Journal:'), ReqField('Issue:'), ReqField('Pages:'),
      #           ReqField('Title:'), ReqField('Technique:'), ReqField('Instrument used:'))
      #       fieldsKreator <- colnames(TableKreators)
      #       if (key == 'Producer') {
      #         NewEntryInputs <- list(
      #           textInput(session$ns(fieldsKreator[1]), label = ReqField('Short name:'), placeholder = 'Acronym', width = '180px'),
      #           textInput(session$ns(fieldsKreator[2]), label = ReqField('Full name:'), width = '180px'),
      #           textInput(session$ns(fieldsKreator[3]), label = NonReqField('Alternative name:'),
      #                     placeholder = '(Optional)', width = '180px'),
      #           textInput(session$ns(fieldsKreator[6]), label = ReqField('Website:'), value = 'https://', width = '180px'),
      #           selectizeInput(session$ns(fieldsKreator[4]), label = ReqField('Country'), choices = countries, width = '180px',
      #                          options = list(placeholder = 'Write or select an option below:',
      #                                         onInitialize = I('function() { this.setValue(""); }'))))
      #       } else {
      #         NewEntryInputs <- list(
      #           textInput(session$ns(fieldsKreator[1]), label = ReqField('Authors:'), placeholder = 'Separated by commas', width = '180px'),
      #           textInput(session$ns(fieldsKreator[6]), label = ReqField('Report title:'), width = '180px'),
      #           numericInput(session$ns(fieldsKreator[2]), label = ReqField('Year:'), width = '180px',
      #                        min = 1900, max = 2025, value = NULL),
      #           textInput(session$ns(fieldsKreator[3]), label = ReqField('Journal:'), width = '180px'),
      #           textInput(session$ns(fieldsKreator[6]), label = ReqField('Issue:'), width = '180px'),
      #           textInput(session$ns(fieldsKreator[3]), label = ReqField('Pages:'), placeholder = 'pp - pp', width = '180px'),
      #           textInput(session$ns(fieldsKreator[3]), label = ReqField('Technique:'), width = '180px'),
      #           textInput(session$ns(fieldsKreator[6]), label = ReqField('Instrument used:'), width = '180px'),
      #           textInput(session$ns(fieldsKreator[6]), label = ReqField('DOI or URL:'), value = 'https://doi.org/', width = '180px'))
      #       } 
      #       
      #       # countrycode('Colombia','country.name', 'iso2c')
      #       showModal(modalDialog(
      #         title = tags$b(paste0('Provide the following information about the ', item(), ':')),
      #         footer = NULL, fade = TRUE, easyClose = FALSE,
      #         tags$div(id = 'inlineTOP', style = 'margin-left: 10px;', 
      #                  tags$b(style = 'color: red;', 'This inputs do not work well yet'),
      #                  tags$br(), NewEntryInputs),
      #         tags$br(), uiOutput(session$ns('BadNewKreator')),
      #         splitLayout(
      #           actionButton(session$ns('createNewKreator'), label = tags$b('Record new ', item())),
      #           actionButton(session$ns('cancelNewKreator'), label = tags$b('Cancel writing to database')))
      #       ))
      #     }
      #   })
      # 
      #   observeEvent(input$createNewKreator, {
      #     fields2Check <- ifelse(
      #       key == 'Producer',
      #       c(input$Producer, input$ProducerFullName, input$Country, input$URL),
      #       c()
      #     )
      #     
      #     if (are.null.empty(fields2Check)) {
      #       output$BadNewKreator <- renderUI(tags$b(style = 'color: red;', 'Please fill in all required fields.'))
      #     } else {
      #       # removeModal()
      #       output$NewProdInfo <- renderUI(tags$div(
      #         tags$b('New ', item(), ' in the database: '), input$Producer, ', ',
      #         tags$a(input$ProducerFullName, href = input$URL, target = '_blank')))
      #     }
      #   })
      # 
      # 
      #   observeEvent(input$cancelNewKreator, {
      #     removeModal()
      #     restartKreaList(restartKreaList() + 1)# runjs("Shiny.setInputValue('UploadData-selectedKreator', '');")
      #   })
      # }
      # 
      # return(reactive(input$selectedKreator))
      
    }
  )
}