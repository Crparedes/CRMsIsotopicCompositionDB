UploadDataUI <- function(id, label = "Counter", FlTy = 'Excel') {
  ns <- NS(id)
  fluidRow(
    column(
      8, offset = 1,  tags$hr(), tags$hr(),
      h4(tags$b('Use this module to load new data into the database')), tags$br(),
      tags$div(
        class = 'DivLogIn',
        h5('You need to log in with your credentials to use the functions in this module.', tags$br(),
           'Please contact', tags$a(href = "mailto:caparedes@inm.gov.co", "caparedes@inm.gov.co"),
           'if you require access to help constructing the database.'),
        tags$div(id = 'inline', style = 'font-size:14px; margin-left:60px;',
                 textInput(ns('username'), label = ReqField('User name'), width = '400px',
                           placeholder = 'user@organization.com', value = 'caparedes@inm.gov.co'),
                 passwordInput(ns('password'), label = ReqField('Password'), width = '400px',
                               placeholder = '********', value = 'IsotoAdmin0.')),
        actionButton(ns('login'), label = tags$b('Log In'), width = '100px', 
                     style = 'margin-left:100px; margin-top:15px; padding:4px; font-size:80%')),
      
      tags$ol(
       hidden(tags$div(
         class = 'AllInputs', uiOutput(ns('loggedin'), inline = FALSE),
         radioButtons(
            ns('CRM_type'), label = tags$lib('Which kind of data will you provide:'), 
            selected = character(0), width = '100%',
            choices = list(
              'Data of a CRM with certified values for isotopic composition' = 'IsoCompCRM',
              'Report of measured isotopic composition in a (non isotopically certified) CRM' = 'LitRepoCRM',
              'For the future... Upload a Digital Certificate of a Reference Material with isotopic composition data' = 'DRMC')),
          
         conditionalPanel(
            "input.CRM_type != null", ns = ns, tags$hr(),
            
            conditionalPanel(
              "input.CRM_type == 'IsoCompCRM'", ns = ns,
              HTML("<b style='color:blue !important;'>
                    The functions of this module are under implementation.</b>"),
              UploadProducerUI(ns('Producer')),
              UploadMaterialInfoUI(ns('IsoCompCrm'), key = 'IsoCompCrm')
            ),
            
            conditionalPanel(
              "input.CRM_type == 'LitRepoCRM'", ns = ns, tags$hr(),
              HTML("<b style='color:blue !important;'>
                    The functions to upload independent measurement reports will be implemented soon.</b>"), 
              tags$hr(),
              #UploadProduStudyUI(ns('Study'), key = 'Study')
              radioButtons(
                inputId = ns('CRM_NIC_type'), width = '100%',
                label = tags$lib('Which kind of (non isotopically certified) RM do you want to report?'),
                choices = list('Calibration solution or high purity solid' = 'CalibraCRM',
                               'Matrix certified reference material' = 'MatrixCRM'))
            ),
            
            conditionalPanel(
              "input.CRM_type == 'DRMC'", ns = ns, tags$hr(),
              HTML("<b style='color:blue !important;'>
                    The Digital Certificates of Reference Materials are still a topic under discussion
                    and therefore, these functions are not available yet.</b>"), 
              tags$hr(),
              fileInput(ns('DRMC'), label = tags$lib('Upload your file'), multiple = FALSE, 
                        placeholder = 'FUNCTION DISABLED')
            )
          )
        ))
      ), 
      uiOutput(ns('brwz'))
    )
  )
}

UploadDataServer <- function(id, devMode, TableProducers, TableStudies, 
                             TableIsoCompCRMsInfo) {
  moduleServer(
    id,
    function(input, output, session) {
      output$brwz <- renderUI(if(devMode()) return(actionButton(session$ns('brwz'), class = 'PauBtn',
                                                                label = tags$b('Pause and inspect module'))))
      observeEvent(input$brwz, browser())
      
      logInf <- eventReactive(input$login, ignoreInit = TRUE, {
        if (is.null(Credentials[[input$username]])) {
          showNotification('Username not found.', duration = 3, type = 'error')
          return(FALSE)
        } else {
          if (Credentials[[input$username]] != input$password) {
            showNotification('Password is not correct.', duration = 3, type = 'error')
            return(FALSE)
          } else {return(TRUE)}
        }
      })
      
      observeEvent(logInf(), {
        if (logInf()) {
          shinyjs::hide(selector = 'div.DivLogIn', anim = TRUE, animType = 'fade', time = 0.5)
          shinyjs::show(selector = 'div.AllInputs', anim = TRUE, animType = 'fade', time = 1.5)
        }
      })
      
      loggedin <- reactive(tags$div(
        'Logged in as ', tags$b(input$username), '. Close or refresh this page to log out.', tags$hr()))
      output$loggedin <- renderUI(loggedin())
      
      Producer <- UploadProducerServer(
        id = 'Producer', id2 = id, devMode = devMode, TableKreators = TableProducers, DataEntryClerk = input$username)
      
      IsCoCrmInfo <- UploadMaterialInfoServer(
        id = 'IsoCompCrm', id2 = id, devMode = devMode, key = 'IsoCompCrm', 
        Producer = Producer, TableInfos = TableIsoCompCRMsInfo, DataEntryClerk = input$username)
      
      
      
      Study <- UploadProduStudyServer(
        id = 'Study', id2 = id, devMode = devMode, key = 'Study', TableKreators = TableStudies,
        DataEntryClerk = input$username)
      
    }
  )
}