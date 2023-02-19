# install.packages('odbc')
# install.packages('RMySQL')
# install.packages('config') # If we want to store the credentials to connect to the database in a .yml file.

library(odbc)
library(RMySQL)

MRCsICDB <- RMySQL::dbConnect(
  RMySQL::MySQL(), user = 'sql9599488', password = 'PHsEvvEBuY', dbname = 'sql9599488', host = 'sql9.freesqldatabase.com'
)

summary(MRCsICDB)
dbListTables(MRCsICDB)



dbDisconnect(MRCsICDB)

