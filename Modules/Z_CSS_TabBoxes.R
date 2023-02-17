headTags3 <- tags$li(
  tags$head(
    # TabBoxes
    tags$style(HTML(
      '.nav-tabs {
        border-bottom: 2px solid #ffffff;
      }
      .tab-content>.active {
        display: block;
        background-color: #ffffff;
      }
      .nav-tabs>li.active>a, .nav-tabs>li.active>a:hover, .nav-tabs>li.active>a:focus {
        color: #1a242f;
        background-color: #ffffff;
        border: 2px solid #ffffff;
        border-bottom-color: transparent;
        cursor: default;
        font-weight: bold;
      }
      .nav-tabs>li>a {
        background-color: #e1e1e1;
        margin-right: 2px;
        line-height: 1.42857143;
        border-radius: 4px 4px 0 0;
      }
    '))
    )
  )