pad_date <- function(date_string) {
  if (grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", date_string)){
    parts <- unlist(strsplit(date_string, "/"))
    return(paste(sprintf("%02d", as.integer(parts[1])), 
                 sprintf("%02d", as.integer(parts[2])), 
                 parts[3], sep = "/"))
  }
  return(date_string)
}

convert_dates <- function(date_strings) {
  sapply(date_strings, function(date_string) {
    padded_date <- pad_date(date_string)
    if (grepl("^\\d{2}/\\d{2}/\\d{4}$", padded_date)) {
      return(dmy(padded_date))
    } else if (grepl("^\\d+$", date_string)) {
      return(as.Date(as.integer(date_string), origin = "1899-12-30"))
    } else {
      return(NA)
    }
  })
}