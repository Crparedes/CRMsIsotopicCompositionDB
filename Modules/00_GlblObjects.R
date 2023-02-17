MRCsICDBversion <- '0.0.1.9000'
DummyNumber <- c(0, 0.123456789)

title <- tags$div(HTML(
  '<table text-align=left cellspacing=-10 cellPadding=30>
  <tr><th rowspan = 2>', spcs(5),
  '<a id = "logo" href = "http://www.inm.gov.co" target = ”_blank” title = "MRCs Isotopic Composition DataBase" data-height="50">
  <img src = "IsotopeLogo.png" height = "90" alt = "LOGO" style = "margin-top: 5px">
  </a>', spcs(5),
  '</th>
  <th><h1 style="LINE-HEIGHT:25px; color: #dddddd; margin-bottom: 5px;">
    <b>Isotopic Composition Database</b>
  </h1></th></tr>
  <tr><th><h3 style="LINE-HEIGHT:0px; color: #dddddd; margin-top: 20px;">
  for Certified Reference Materials
  </h3></th></tr>
  </table>'))

Information <- h5(
  "This interactive web application was developed in the framework of a project financially supported by the Ministerio de Ciencia, 
  Tecnología e Innovación de Colombia", tags$a(href = "https://minciencias.gov.co/", "(MinCiencias)", target = "_blank"), "under project number 9932100271370.", tags$br(),
  "The valuable knowledge transfer from the expert Coaches of the SIM-PTB-IDB joint project", 
  tags$a(href = "https://sim-metrologia.org/2021/09/13/workshop-for-launching-the-implementation-of-new-nmi-services-related-to-digital-transformation-idb-project/",
         "CABUREK-SIM-M4DT", target = "_blank"), 
  "notably inspired this implementation. Their help is greatly acknowledged.", tags$br(),
  "The work was presented at the ", tags$a(href = "https://URL.sin.asignar/", "XXX XXXX Anual Conference on XXXXX XXXX (20XX)", target = "_blank"),
  "by chemist", tags$a(href = "https://www.researchgate.net/profile/Cristhian-Paredes-2", "Cristhian Paredes.", target = "_blank"))
