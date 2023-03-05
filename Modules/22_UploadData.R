UploadDataUI <- function(id, label = "Counter", FlTy = 'Excel') {
  ns <- NS(id)
  fluidRow(
    column(
      8, offset = 1,  tags$hr(), tags$hr(),
      h4(tags$b('Use this module to load new data into the database')), tags$br(),
      tags$ol(
        radioButtons(
          ns('CRM_type'), label = tags$lib('Select the type of information that you want to provide:'), 
          selected = character(0), width = '100%',
          choices = list('Data of a CRM with certified values for isotopic composition' = 'IsoCompCRM',
                         'Literature report of measured isotopic composition in a (non isotopically certified) CRM' = 'LitRepoCRM')),
        conditionalPanel("input.CRM_type != null", ns = ns, 
          conditionalPanel(
            "input.CRM_type == 'LitRepoCRM'", ns = ns, tags$hr(),
            radioButtons(
              ns('CRM_NIC_type'), label = tags$lib('Which kind of (non isotopically certified) CRM do you want to report?'),
              width = '100%',
              choices = list('Calibration solution or high purity solid' = 'CalibraCRM',
                             'Matrix certified reference material' = 'MatrixCRM'))
          ),
          tags$hr(), uiOutput(ns('selectProducer')),
          conditionalPanel("input.SelectedProducer == 'Other'", ns = ns, tags$b('New CRM producer: '), uiOutput(ns('NewProdInfo'))),
          
          uiOutput('StudyInfoEntry')
          
          
        )
      ), 
      uiOutput(ns('brwz'))
    )
  )
}

UploadDataServer <- function(id, devMode, TableProducers) {
  moduleServer(
    id,
    function(input, output, session) {
      output$brwz <- renderUI(
        if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pause module'))))
      observeEvent(input$brwz, browser())
      
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
              textInput(session$ns(fieldsCrmProd[1]), label = ReqField('CRM producer:'), placeholder = 'Short name or acronym', width = '80%'),
              textInput(session$ns(fieldsCrmProd[2]), label = ReqField('Full name:'), placeholder = '(In english)', width = '80%'),
              textInput(session$ns(fieldsCrmProd[3]), label = NonReqField('Full name in original languaje:'),
                        placeholder = '(Optional)', width = '80%'),
              selectizeInput(session$ns(fieldsCrmProd[4]), label = ReqField('Country'), choices = countries, width = '80%', 
                             options = list(placeholder = 'Write or select an option below:', 
                                            onInitialize = I('function() { this.setValue(""); }'))),
              textInput(session$ns(fieldsCrmProd[6]), label = ReqField('Producer website:'), placeholder = 'A valid URL', width = '80%'),
              tags$br(), uiOutput(session$ns('BadNewProducer'))
            ),
            splitLayout(
              actionButton(session$ns('createNewProducer'), label = tags$b('Define new CRM producer')),
              actionButton(session$ns('cancelNewProducer'), label = tags$b('Cancel')))
          ))
        }
      })
      
      observeEvent(input$createNewProducer, {
        if (are.null.empty(c(input$Producer, input$ProducerFullName, input$Country, input$URL))) {
          output$BadNewProducer <- renderUI(tags$b(style = 'color: red;', 'Please fill in all required fields.'))
        } else {
          removeModal()
          output$NewProdInfo <- renderUI(tags$div(tags$b(input$Producer), ', ',
                                                  tags$a(input$ProducerFullName, href = input$URL, target = '_blank')))
        }
      })
      NewProdInfo <- reactive(0)
      
      observeEvent(input$cancelNewProducer, {
        removeModal()
        restartProdList(restartProdList() + 1)
        # runjs("Shiny.setInputValue('UploadData-SelectedProducer', '');")
      })
      
      
      StudyInfoUI <- reactive({
        if (input$Producer != '' && input$Producer != 'Other' ) {
          StudyInfo <- tags$div(
            tags$hr(),
            tags$lib('Provide the following information about the CRM producer:'), tags$br()
          )
          return(StudyInfo)
        }
      })
      output$StudyInfoUI <- renderUI(StudyInfoUI())
      
    }
  )
}