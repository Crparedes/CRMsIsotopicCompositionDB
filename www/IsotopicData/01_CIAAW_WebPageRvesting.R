if (F) {
  # install.packages('rvest')
  library(rvest)
  # CIAAW. Isotopic compositions of the elements 2021. Available online at www.ciaaw.org.
  CIAAW_WebPage <- read_html('https://ciaaw.org/isotopic-abundances.htm')
  (CIAAW_NatIsotAbunTable <- CIAAW_WebPage %>% html_elements('table') %>% html_table())
  CIAAW_NatIsotAbunTable[[1]][['Element']] # It gets pretty messy after titanium. Why????
}

CIAAW_NatIsotAbunTable <- read.csv(file = 'www/IsotopicData/RAW_CIAAW_NatIsotAbun.csv')
CIAAW_NatIsotAbunTable['E'][CIAAW_NatIsotAbunTable['E'] == ''] <- NA
CIAAW_NatIsotAbunTable['Element'][CIAAW_NatIsotAbunTable['Element'] == ''] <- NA
CIAAW_NatIsotAbunTable <- zoo::na.locf(CIAAW_NatIsotAbunTable)
colnames(CIAAW_NatIsotAbunTable)[5] <- 'Relative.abundance'

Interval <- grepl(pattern = '[', CIAAW_NatIsotAbunTable$Relative.abundance, fixed = TRUE)
CIAAW_NatIsotAbunTable$Interval <- Interval
CIAAW_NatIsotAbunTable$Isotope <- paste0(CIAAW_NatIsotAbunTable$A, CIAAW_NatIsotAbunTable$E)



CIAAW_NatIsotAbunFtnts <- read.csv(file = 'www/IsotopicData/RAW_CIAAW_NatIsotAbunNotes.csv')


