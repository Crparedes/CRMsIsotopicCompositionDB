if (F) {
  # install.packages('rvest')
  library(rvest)
  # CIAAW. Isotopic compositions of the elements 2021. Available online at www.ciaaw.org.
  CIAAW_WebPage <- read_html('https://ciaaw.org/isotopic-abundances.htm')
  (CIAAW_NatIsotAbunTable <- CIAAW_WebPage %>% html_elements('table') %>% html_table())
  CIAAW_NatIsotAbunTable[[1]][['Element']] # It gets pretty messy after titanium. Why????
}

CIAAW_NatIsotAbunTable <- read.csv(file = 'www/CIAAW_Data/NatIsotAbun.csv')
CIAAW_NatIsotAbunTable['E'][CIAAW_NatIsotAbunTable['E'] == ''] <- NA
CIAAW_NatIsotAbunTable['Element'][CIAAW_NatIsotAbunTable['Element'] == ''] <- NA
CIAAW_NatIsotAbunTable <- zoo::na.locf(CIAAW_NatIsotAbunTable)

CIAAW_NatIsotAbunFtnts <- read.csv(file = 'www/CIAAW_Data/NatIsotAbunNotes.csv')

IntervalAbundance <- grepl(pattern = '[', CIAAW_NatIsotAbunTable$Representative.isotopic.composition, fixed = TRUE)
CIAAW_NatIsotAbunTable[Interval, ]
