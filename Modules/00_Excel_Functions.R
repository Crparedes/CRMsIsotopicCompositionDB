saveData <- function(tableName, data) {
  # The data must be a dataframe rather than a named vector
  data <- data %>% as.list() %>% data.frame()
  # Add the data as a new row
  sheet_append(ss = SHEET_ID, data = data, sheet = tableName)
}

loadFromDataBase <- function(tableName, element = NULL, CRM = NULL) {
  table <- data.frame(read_excel(path = 'www/CRMsIsotopicCompositionDB.xlsx', sheet = tableName))
  if (missing(element) && missing(CRM)) {
    return(table)
  } else {
    if (missing(CRM)) {
      return(table[grep(tolower(element), table$Elements, value = FALSE), ])
    } else {
      return(table[table$CRM_name == CRM, ])
    }
  }
}

# loadFromDataBase(tableName = 'IsoCompCRM_Info')
# loadFromDataBase(tableName = 'IsoCompCRM_Info', element = 'uranium')
# loadFromDataBase(tableName = 'IsoCompCRM_Info', CRM = 'UCLO-1')

