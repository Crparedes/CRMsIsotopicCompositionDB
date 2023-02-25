BibliographyUI <- function(id, label = "Counter", FlTy = 'Excel') {
  ns <- NS(id)
  fluidRow(
    tags$hr(), tags$hr(),
    column(
      width = 10, offset = 1,
      h4(tags$b('About MRCs Isotopic Composition DataBase')),
      'This App is under development.',
      tags$br(), tags$br(), tags$hr(),
      
      h4(tags$b('Bibliography, resources')),
      tags$ul(
        tags$li(
          'IUPAC-CIAAW (2021). Isotopic compositions of the elements. Available online at',
          tags$a('www.ciaaw.org.', href = 'https://ciaaw.org/isotopic-abundances.htm', target = '_blank')),
        tags$li(
          'Meija J, Possolo A (2018). CIAAWconsensus: Isotope Ratio Meta-Analysis. R package version 1.3.
          Available online at',
          tags$a('https://CRAN.R-project.org/package=CIAAWconsensus.', 
                 href = 'https://CRAN.R-project.org/package=CIAAWconsensus', target = '_blank')),
        tags$br(), tags$br(),
        tags$li(
          style = 'margin-top:10px;',
          'The periodic table widget was adapted from an Open Source', 
          tags$a('CodePen Project,', href = 'https://codepen.io/mikegolus/pen/OwrPgB', target = '_blank'),
          'by', tags$a('Mike Golus.', href = 'https://www.mikegolus.com/', target = '_blank'))
      ),
      tags$hr(), tags$br(), tags$br(), tags$br(), tags$br(),
    )
  )        
}