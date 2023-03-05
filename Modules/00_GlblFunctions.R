spcs <- function(n) {return(paste0(rep('&nbsp;', n), collapse = ''))}
ReqField <- function(x) {return(HTML(paste0(x, '<font color=\"#FF0000\">*</font>', spcs(3))))}
NonReqField <- function(x) {return(HTML(paste0(x, spcs(3))))}
tags$lib <- function(x) return(tags$b(tags$li(x)))

is.null.empty <- function(x) {
  if (is.null(x)) {
    return(TRUE)
  } else {
    if (length(x) == 0 || x == '' || any(is.na(unlist(x)))) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

are.null.empty <- function(x) {
  for (i in 1:length(x)) {
    if (is.null.empty(x[i])) {return(TRUE)}}
  return(FALSE)
}


niceSeparator <- function(){return(tags$hr(style = "border-top: 3px solid #2c3e50;"))}

EnsureMinValue <- function(x, min) {return(max(na.omit(c(x, min))))}

# Taken from https://www.rdocumentation.org/packages/berryFunctions/versions/1.21.14
is.error <- function (expr, tell = FALSE, force = FALSE) { 
  expr_name <- deparse(substitute(expr))
  test <- try(expr, silent = TRUE)
  iserror <- inherits(test, "try-error")
  if (tell) 
    if (iserror) 
      message("Note in is.error: ", test)
  if (force) 
    if (!iserror) 
      stop(expr_name, " is not returning an error.", call. = FALSE)
  iserror
}