# install.packages('odbc')
# install.packages('RMySQL')
# install.packages('config') # If we want to store the credentials to connect to the database in a .yml file.

MRCsICDB <- RMySQL::dbConnect(
  RMySQL::MySQL(), user = 'sql9599488', password = 'PHsEvvEBuY', dbname = 'sql9599488', host = 'sql9.freesqldatabase.com'
)

summary(MRCsICDB)
dbListTables(MRCsICDB)

source('www/IsotopicData/01_CIAAW_WebPageRvesting.R')
dbWriteTable(conn = MRCsICDB, name = 'CIAAW_NatIsotAbunTable', value = CIAAW_NatIsotAbunTable)
dbWriteTable(conn = MRCsICDB, name = 'CIAAW_NatIsotAbunFtnts', value = CIAAW_NatIsotAbunFtnts)
dbWriteTable(conn = MRCsICDB, name = 'CIAAW_IsotopesMass2016', value = ciaaw.mass.2016)
dbListTables(MRCsICDB)



# dbDisconnect(MRCsICDB)

