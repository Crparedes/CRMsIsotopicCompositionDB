## See secttion 5 of https://shiny.rstudio.com/articles/persistent-data-storage.html#mysql

databaseName <- "myshinydatabase"
table <- "responses"

saveData <- function(data) {
  # Connect to the database
  MRCsICDB <- dbConnect(drv = MySQL(), user = options()$mysql$user, password = options()$mysql$password,
                        dbname = options()$mysql$user, host = options()$mysql$host)
  
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    table, 
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  
  # Submit the update query and disconnect
  dbGetQuery(MRCsICDB, query)
  dbDisconnect(MRCsICDB)
}

loadFromDataBase <- function(tableName, element = NULL) {
  # Connect to the database
  MRCsICDB <- dbConnect(drv = MySQL(), user = options()$mysql$user, password = options()$mysql$password,
                        dbname = options()$mysql$user, host = options()$mysql$host)
  
  # Construct the fetching query
  query <- ifelse(missing(element),
                  sprintf("SELECT * FROM %s", tableName),
                  sprintf("SELECT * FROM %s WHERE Elements LIKE '%%%s%%'", tableName, element))
  
  # Submit the fetch query and disconnect
  data <- dbGetQuery(MRCsICDB, query)
  dbDisconnect(MRCsICDB)
  return(data)
}

# loadFromDataBase(tableName = 'MatrixCRM_DataIR')
# loadFromDataBase(tableName = 'MatrixCRM_DataIR', element = 'lead')
