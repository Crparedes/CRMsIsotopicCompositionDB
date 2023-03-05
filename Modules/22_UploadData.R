UploadDataUI <- function(id, label = "Counter", FlTy = 'Excel') {
  ns <- NS(id)
  fluidRow(
    column(
      8, offset = 1,  tags$hr(), tags$hr(),
      h4(tags$b('Use this module to load new data into the database')), tags$br(),
      tags$ol(
        radioButtons(
          ns('CRM_type'), label = tags$lib('Which kind of data will you provide:'), 
          selected = character(0), width = '100%',
          choices = list('Data of a CRM with certified values for isotopic composition' = 'IsoCompCRM',
                         'Report of measured isotopic composition in a (non isotopically certified) CRM' = 'LitRepoCRM')),
        conditionalPanel(
          "input.CRM_type != null", ns = ns,
          tags$hr(), uiOutput(ns('selectProducer')), 
          conditionalPanel("input.SelectedProducer == 'Other'", ns = ns, uiOutput(ns('NewProdInfo'))),
          
          conditionalPanel(
            "input.selectProducer != ''", ns = ns, 
            conditionalPanel(
              "input.CRM_type == 'LitRepoCRM'", ns = ns, 
              tags$hr(), uiOutput(ns('selectStudy')),
              tags$hr(),
              radioButtons(
                ns('CRM_NIC_type'), label = tags$lib('Which kind of (non isotopically certified) CRM do you want to report?'),
                width = '100%',
                choices = list('Calibration solution or high purity solid' = 'CalibraCRM',
                               'Matrix certified reference material' = 'MatrixCRM'))
            )
          ),
          
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
      
      restartProdList <- reactiveVal(0)
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