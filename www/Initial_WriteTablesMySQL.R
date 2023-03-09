#### install.packages('odbc')
#### install.packages('RMySQL')
#### install.packages('config') # If we want to store the credentials to connect to the database in a .yml file.


### PROCEED CAUTIOSLY
# MRCsICDB <- RMySQL::dbConnect( ## Hosted freely by https://www.freesqldatabase.com/account/ (5Mb limit)
#   RMySQL::MySQL(), #user = 'sql9599488', #password = 'PHsEvvEBuY', 
#   dbname = 'sql9599488', #host = 'sql9.freesqldatabase.com'
# )

# Following line will delete all tables
####for (i in dbListTables(MRCsICDB)) RMySQL::dbSendQuery(MRCsICDB, paste0("DROP TABLE ", i))
summary(MRCsICDB)
dbListTables(MRCsICDB)

source('www/IsotopicData/01_CIAAW_WebPageRvesting.R')
source('www/IsotopicData/02_INITIAL_DataTableConstruction.R')
# overwrite <- FALSE

dbWriteTable(overwrite = overwrite, conn = MRCsICDB, name = 'CIAAW_NatIsotAbunTable', value = CIAAW_NatIsotAbunTable)
dbWriteTable(overwrite = overwrite, conn = MRCsICDB, name = 'CIAAW_NatIsotAbunFtnts', value = CIAAW_NatIsotAbunFtnts)
dbWriteTable(overwrite = overwrite, conn = MRCsICDB, name = 'CIAAW_IsotopesMass2016', value = CIAAWconsensus::ciaaw.mass.2016)

dbWriteTable(overwrite = overwrite, conn = MRCsICDB, name = 'CRMproducers', value = INITI_CRMproducers)
dbWriteTable(overwrite = overwrite, conn = MRCsICDB, name = 'MeasuReports', value = INITI_MeasuReports)
dbWriteTable(overwrite = overwrite, conn = MRCsICDB, name = 'MeasRepoAuth', value = INITI_MeasRepoAuth)

dbWriteTable(overwrite = overwrite, conn = MRCsICDB, name = 'IsoCompCRM_Info', value = INITI_IsoCompCRM_Info)
dbWriteTable(overwrite = overwrite, conn = MRCsICDB, name = 'IsoCompCRM_DataIR', value = INITI_IsoCompCRM_DataIR)
dbWriteTable(overwrite = overwrite, conn = MRCsICDB, name = 'IsoCompCRM_DataAbundances', value = INITI_IsoCompCRM_DataAb)

dbWriteTable(overwrite = overwrite, conn = MRCsICDB, name = 'CalibraCRM_Info', value = INITI_CalibraCRM_Info)
dbWriteTable(overwrite = overwrite, conn = MRCsICDB, name = 'CalibraCRM_DataIR', value = INITI_CalibraCRM_DataIR)

dbWriteTable(overwrite = overwrite, conn = MRCsICDB, name = 'MatrixCRM_Info', value = INITI_MatrixCRM_Info)
dbWriteTable(overwrite = overwrite, conn = MRCsICDB, name = 'MatrixCRM_DataIR', value = INITI_MatrixCRM_DataIR)

dbListTables(MRCsICDB)

dbDisconnect(MRCsICDB)

