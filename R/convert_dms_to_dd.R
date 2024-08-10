convert_dms_to_dd <- function(dms) {

  cleaned_dms <- str_replace_all(dms, "[^0-9.-]", " ")
  parts <- as.numeric(unlist(strsplit(cleaned_dms, "\\s+")))
  
  degrees <- parts[1]
  minutes <- parts[2] / 60
  seconds <- parts[3] / 3600
  
  decimal_degrees <- degrees + minutes + seconds
  
  return(decimal_degrees)
}
