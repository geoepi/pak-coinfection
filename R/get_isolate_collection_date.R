get_isolate_collection_date <- function(iso_names) {
  
  require(rentrez)
  
  results <- list()
  
  for (isolate_name in iso_names) {
    
    search_term <- paste0("\"", isolate_name, "\"[Isolate]")
    
    search_results <- entrez_search(db = "nuccore", term = search_term, retmax = 10)
    
    if(length(search_results$ids) > 0) {
      record_id <- search_results$ids[1]
      
      record_details <- tryCatch({
        entrez_fetch(db = "nuccore", id = record_id, rettype = "gb", retmode = "text")
      }, error = function(e) {
        cat("Error fetching details:", e$message, "\n")
        NULL
      })
      
      if (!is.null(record_details)) {
        
        collection_date_pattern <- "/collection_date=\"(\\d{4})\""
        collection_date_match <- regmatches(record_details, regexec(collection_date_pattern, record_details))
        
        if(length(collection_date_match[[1]]) > 1) {
          collection_date <- collection_date_match[[1]][2]
        } else {
          collection_date <- NA
        }
      } else {
        collection_date <- NA
      }
    } else {
      collection_date <- NA
    }
    
    results[[isolate_name]] <- list(CollectionDate = collection_date)
  }
  
  isolate_names <- c()
  collection_dates <- c()
  
  for (name in names(results)) {
    isolate_names <- c(isolate_names, name)
    collection_dates <- c(collection_dates, results[[name]]$CollectionDate)
  }
  
  isolate_df <- data.frame(IsolateName = isolate_names, CollectionDate = collection_dates)
  
  return(isolate_df)
}
