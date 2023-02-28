MRCsICDBversion <- '0.0.1.9000'
DummyNumber <- c(0, 0.123456789)

title <- tags$div(HTML(
  '<table text-align=left cellspacing=-10 cellPadding=30>
  <tr><th rowspan = 2>', spcs(5),
  '<a id = "logo" href = "http://www.GenericPage.gov.co" target = ”_blank” 
  title = "MRCs Isotopic Composition DataBase" data-height = "50"">
  <img src = "logo.png" height = "90" alt = "LOGO" style = "margin-top:5px">
  </a>', spcs(5),
  '</th>
  <th><h1 style="LINE-HEIGHT:25px; color: #dddddd; margin-bottom: 5px;">
    <b>Isotopic Composition Database</b>
    <b style="LINE-HEIGHT:5px; color: #dddddd; font-size:14px;">v.0.0.1.9000</b>
  </h1></th></tr>
  <tr><th><h3 style="LINE-HEIGHT:0px; color: #dddddd; margin-top: 20px;">
  for Certified Reference Materials
  </h3></th></tr>
  </table>'))

Disclaimer <- h6(
  tags$b('Disclaimer:'),
  tags$b('THIS WEB APP IS UNDER DEVELOPMENT.'), 
  'This tool is made available freely in the hope that it will be useful, but is provided WITHOUT ANY WARRANTY, 
  to the extent permitted by law; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.')
