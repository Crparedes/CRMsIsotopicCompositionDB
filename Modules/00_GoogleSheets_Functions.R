## See https://stackoverflow.com/a/70215575/7612904
if (FALSE) { # Run this only once
  options(gargle_oauth_cache = ".secrets")
  gs4_auth()
  list.files(".secrets/")
  gs4_deauth()
}

options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)

gs4_auth(cache = ".secrets", email = TRUE)

## Previous lines do not work when deployed on server. 
## See https://gargle.r-lib.org/articles/non-interactive-auth.html 
## and https://gargle.r-lib.org/articles/get-api-credentials.html

## Following line was working :c
# gs4_auth_configure(api_key = "AIzaSyAZK7lbllXJ76f_kBfGxwfQvHwm4RKOKx8")
# gs4_deauth()

# gs4_auth(cache = ".secrets", email = "craparedesca@unal.edu.co")


## See secttion 6 of https://shiny.rstudio.com/articles/persistent-data-storage.html#mysql
SHEET_ID <- 'https://docs.google.com/spreadsheets/d/1vpIlGMA3rLVD3ScpQyXVphLEeCsxW_xjcZeo75crM34/edit#gid=979538683'

saveData <- function(tableName, data) {
  # The data must be a dataframe rather than a named vector
  data <- data %>% as.list() %>% data.frame()
  # Add the data as a new row
  sheet_append(ss = SHEET_ID, data = data, sheet = tableName)
}

loadFromGoogleSheetsDataBase <- function(tableName, element = NULL, CRM = NULL) {
  table <- data.frame(read_sheet(ss = SHEET_ID, sheet = tableName))
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

