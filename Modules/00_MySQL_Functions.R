# ## See secttion 5 of https://shiny.rstudio.com/articles/persistent-data-storage.html#mysql
# 
# options(mysql = list("host" = "sql9.freesqldatabase.com", "user" = "sql9599488", "password" = "PHsEvvEBuY"))
# 
# saveData <- function(data) {
#   # Connect to the database
#   MRCsICDB <- dbConnect(RMySQL::MySQL(), user = "sql9599488", password = "PHsEvvEBuY",
#                         dbname = "sql9599488", host = "sql9.freesqldatabase.com")
#   
#   # Construct the update query by looping over the data fields
#   query <- sprintf(
#     "INSERT INTO %s (%s) VALUES ('%s')",
#     table, 
#     paste(names(data), collapse = ", "),
#     paste(data, collapse = "', '")
#   )
#   
#   # Submit the update query and disconnect
#   dbGetQuery(MRCsICDB, query)
#   dbDisconnect(MRCsICDB)
# }
# 
# loadFromDataBase <- function(tableName, element = NULL, CRM = NULL) {
#   # Connect to the database
#   MRCsICDB <- dbConnect(RMySQL::MySQL(), user = "sql9599488", password = "PHsEvvEBuY",
#                         dbname = "sql9599488", host = "sql9.freesqldatabase.com")
#   
#   # Construct the fetching query
#   if (missing(element) && missing(CRM)) {
#     query <- sprintf("SELECT * FROM %s", tableName)
#   } else {
#     if (missing(CRM)) {
#       query <- sprintf("SELECT * FROM %s WHERE Elements LIKE '%%%s%%'", tableName, element)
#     } else {
#       query <- sprintf("SELECT * FROM %s WHERE CRM_name LIKE '%%%s%%'", tableName, CRM)
#     }
#     
#   }
#   # Submit the fetch query and disconnect
#   data <- dbGetQuery(MRCsICDB, query)
#   dbDisconnect(MRCsICDB)
#   return(data)
# }
# 
# # loadFromDataBase(tableName = 'MatrixCRM_DataIR')
# # loadFromDataBase(tableName = 'MatrixCRM_DataIR', element = 'lead')
# # library(microbenchmark)
# # library(ggthemes)
# # library(ggplot2) 
# # 
# # tm <- microbenchmark(loadFromDataBase('IsoCompCRM_Info', element = 'lead'), 
# #                      TrimTableElement(loadFromDataBase('IsoCompCRM_Info'), element = 'lead'), 
# #                      times = 100)  
# # print(tm)
# # autoplot(object = tm)
