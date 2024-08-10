get_accession_date_meta <- function(acc_numbers) {
  
  require(rentrez)
  
  results <- list()
  
  for (accession_number in acc_numbers) {
    
    search_term <- paste0(accession_number, "[Accession]")

    search_results <- entrez_search(db = "nuccore", term = search_term, retmax = 10)
    
    if(length(search_results$ids) > 0) {
      record_id <- search_results$ids[1]
      
      record_details <- tryCatch({
        entrez_fetch(db = "nuccore", id = record_id, rettype = "gb", retmode = "text")
      }, error = function(e) {
        cat("Error fetching details:", e$message, "\n")
        NULL
      })
      
      collection_date <- NA
      isolate_name <- NA
      strain_name <- NA
      
      if (!is.null(record_details)) {

        collection_date_pattern <- '/collection_date="([^"]*)"'
        collection_date_match <- regmatches(record_details, regexec(collection_date_pattern, record_details))
        
        if(length(collection_date_match[[1]]) > 1) {
          collection_date <- collection_date_match[[1]][2]
        }
        
        isolate_pattern <- '/isolate="([^"]*)"'
        isolate_match <- regmatches(record_details, regexec(isolate_pattern, record_details))
        
        if(length(isolate_match[[1]]) > 1) {
          isolate_name <- isolate_match[[1]][2]
        }
        
        strain_pattern <- '/strain="([^"]*)"'
        strain_match <- regmatches(record_details, regexec(strain_pattern, record_details))
        
        if(length(strain_match[[1]]) > 1) {
          strain_name <- strain_match[[1]][2]
        }
      }
      
    } else {
      collection_date <- NA
      isolate_name <- NA
      strain_name <- NA
    }
    
    results[[accession_number]] <- list(CollectionDate = collection_date, IsolateName = isolate_name, StrainName = strain_name)
  }
  
  df_results <- do.call(rbind, lapply(names(results), function(acc) {
    c(AccessionNumber = acc, results[[acc]])
  }))
  
  df_results <- as.data.frame(df_results, stringsAsFactors = FALSE)
  colnames(df_results) <- c("AccessionNumber", "CollectionDate", "IsolateName", "StrainName")
  
  return(df_results)
}
