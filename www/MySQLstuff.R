# install.packages('odbc')
# install.packages('RMySQL')
# install.packages('config') # If we want to store the credentials to connect to the database in a .yml file.

MRCsICDB <- RMySQL::dbConnect( ## Hosted freely by https://www.freesqldatabase.com/account/ (5Mb limit)
  RMySQL::MySQL(), user = 'sql9599488', password = 'PHsEvvEBuY', 
  dbname = 'sql9599488', host = 'sql9.freesqldatabase.com'
)
# Following line will delete all tables
####for (i in dbListTables(MRCsICDB)) RMySQL::dbSendQuery(MRCsICDB, paste0("DROP TABLE ", i))
summary(MRCsICDB)
dbListTables(MRCsICDB)

source('www/IsotopicData/01_CIAAW_WebPageRvesting.R')
source('www/IsotopicData/02_INITIAL_DataTableConstruction.R')

dbWriteTable(conn = MRCsICDB, name = 'CIAAW_NatIsotAbunTable', value = CIAAW_NatIsotAbunTable)
dbWriteTable(conn = MRCsICDB, name = 'CIAAW_NatIsotAbunFtnts', value = CIAAW_NatIsotAbunFtnts)
dbWriteTable(conn = MRCsICDB, name = 'CIAAW_IsotopesMass2016', value = CIAAWconsensus::ciaaw.mass.2016)
dbWriteTable(conn = MRCsICDB, name = 'CRMproducers', value = INITI_CRMproducers)
dbWriteTable(conn = MRCsICDB, name = 'IsoComCRM_Info', value = INITI_IsoComCRM_Info)
dbWriteTable(conn = MRCsICDB, name = 'IsoComCRM_DataIR', value = INITI_IsoComCRM_DataIR)
dbWriteTable(conn = MRCsICDB, name = 'IsoComCRM_DataAbundances', value = INITI_IsoComCRM_DataAbundances)

dbListTables(MRCsICDB)



# dbDisconnect(MRCsICDB)

