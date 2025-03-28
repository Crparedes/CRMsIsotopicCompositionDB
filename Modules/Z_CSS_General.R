headTags1 <- tags$li(
  tags$script(HTML("$('body').addClass('fixed');")),
  tags$head(
    tags$style(type = "text/css", "body {padding-top: 60px; padding-bottom: 60px; background-color: #efefef;}"),
    tags$link(rel = "stylesheet", type = "text/css", href = "infoBox.css"),
    tags$style(HTML("
      .radio-inline, .checkbox-inline {
        padding-left: 40px;
      }")),
    
    # Dialogo modal
    tags$style(HTML('.modal-dialog { width: fit-content !important; }')),
    
    # Botones de pausa
    tags$style(HTML('.PauBtn {font-size: 0.8vw;     line-height: 0.5;}')),

    # labels
    tags$style(HTML('label, .control-label, .help-block, .checkbox, .radio {
        font-size: 1em;
        font-weight: normal;
    }')),
    # Switchinput
    tags$style(HTML('.material-switch>.label-primary {
      background-color: #000000;
    }')),
    
    # Tablas
    tags$style(HTML('.tg{margin-left: 30px;}
                     .tg .tg-field{font-weight:bold; text-align:right; vertical-align:top; padding-right: 10px}
                     .tg .tg-value{font-weight:normal; text-align:left;vertical-align:top}')),
    
    # Links
    tags$style(HTML('
      a {
        color: #337ab7;
        /*color: #2c3e50;      CSS comments  */
        text-decoration: none;
      }
      a:hover, a:focus {
        color: #000000;
        text-decoration: underline;
      }
    ')),
    # numeric inputs
    tags$style(HTML("
      input[type=\"number\"] {
        height: 15px;
        width: 80px;
        font-size: 12px;
      input[type=number]::-webkit-outer-spin-button,
        input[type=number]::-webkit-inner-spin-button {
              -webkit-appearance: none;
              margin: 4px;
        }
      ")),
    # other inputs
    tags$style(HTML("
      .form-control {
        height: 28px;
        padding: 10px 1px 10px 10px;
        font-size: 15px;
        line-height: 1.42857143;
        color: #2c3e50;
        background-color: #ffffff;
      }
      .btn-file {
        height: 22px;
        padding: 3px 3px;
        font-size: 15px;
        background-color:black; 
      }
    ")),
    # # fluidrows
    # tags$style(HTML('
    # .tab-content>.active {
    #   display: block;
    #   background-color: #efefef;
    # }
    # ')),
    # 
    tags$style(HTML(
      '.shiny-notification {position:fixed; top: calc(50% - 150px); left: calc(50% - 150px); 
      height: auto !important; opacity:0.98; margin-right:500px}
      .btn-box-tool {color: #001848; font-size: 15px}')),
    
    # Cosas en una sola linea
    tags$style(
      type = "text/css", 
      "#inline label{ display: table-cell; text-align: right; vertical-align: middle; padding-top:6px} 
      #inline .form-group {display: table-row; width:auto !important;}"),
    tags$style(
      type = "text/css", 
      "#inlineTOP label{ display: table-cell; text-align: right; vertical-align: top;  padding-top:8px} 
      #inlineTOP .form-group {display: table-row;}"),# width:auto !important;}"),
    tags$style(
      type = "text/css", 
      "#inlineBOT label{ display: table-cell; text-align: right; vertical-align: bottom;  padding-top:6px} 
      #inlineBOT .form-group {display: table-row; width:auto !important;}"),
    
    )
  )