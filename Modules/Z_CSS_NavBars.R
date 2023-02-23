headTags2 <- tags$li(
  tags$head(
    # Navigation Bar:
    tags$style(HTML(
      '.navbar {
        height: 100px;
        min-height:100px !important;
      }
      .navbar-nav > li > a, .navbar-brand {
        padding-top:1px !important; 
        padding-bottom:1px !important;
        height: 100px;
      }
      .navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:hover, .navbar-default .navbar-nav>.active>a:focus {
        color: #dddddd;
        background-color: #´;
        font-size:15px;
        font-weight: bold;
        vertical-align: bottom;
        display: table-cell;
      }
      .navbar-default .navbar-nav>li>a {
        color: #9f9f9f;
        font-size:15px;
        font-weight: bold;
        vertical-align: bottom;
        display: table-cell;
      }
      .navbar-default .navbar-nav>li>a:hover, .navbar-default .navbar-nav>li>a:focus {
        color: #ffffff;
        background-color: transparent;
      }
      .navbar-brand {
        float: left;
        padding: 19.5px 55px 10px 10px;
        font-size: 19px;
        line-height: 21px;
        height: 60px;
      }')),
    # Navigation Bar:
    tags$style(HTML('
    .btn-default {
        color: #ffffff;
        background-color: #222222;
        border-color: #222222;
    }')),
    #Second navigation bar
     tags$style(
       type = "text/css", 
       ".navbar2
       .navbar {
         height: 40px;
         min-height: 40px !important;
       }"
     )
    )
  )