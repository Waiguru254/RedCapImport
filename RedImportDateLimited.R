#@days Indicated the number of days you want to download the data from RedCap. For example, 5 will mean records that have been submitted or editted in the last 5 days. 
RedImportDateLimited <- function(token , url, days = NULL, columns = NULL) {
  ### Include null to capture all the data in the database. 
  if (is.null(days)) {
    date_begin <- NULL
  } else {
    date_begin <- Sys.Date() - lubridate::days(days)
  }

  ### Aliging the columns
  fields <- c()
  for (i in seq_along(columns)) {
    if (i == 1) {
      fields[paste0("fields[0]", sep = '')] <- columns[i]
    } else {
      fields[paste0("fields[", i - 1, "]")] <- columns[i]
    }
  }
  ### Download the data 
  formData <- c(as.list(fields),list("token"= token,
                                     content='record',
                                     action='export',
                                     format='json',
                                     type='flat',
                                     csvDelimiter='',
                                     rawOrLabel='raw',
                                     rawOrLabelHeaders='raw',
                                     exportCheckboxLabel='false',
                                     exportSurveyFields='false',
                                     exportDataAccessGroups='false',
                                     returnFormat='json',
                                     dateRangeBegin= date_begin,
                                     dateRangeEnd=Sys.Date()
  ))
  response <- httr::POST(url, body = formData, encode = "form")
  result <- httr::content(response, 'text')
  data <- jsonlite::fromJSON(result) 
  return(data)
}
