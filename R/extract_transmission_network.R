extract_transmission_network <- function(mcmc_Tree) {
  if (length(mcmc_Tree) == 0) stop("mcmc_Tree is empty.")
  
  lastIteration <- mcmc_Tree[[length(mcmc_Tree)]]
  comb <- lastIteration$ctree
  
  lent <- extractTTree(comb)
  trans_Tree <- lent$ttree
  names <- lent$nam
  
  na_vect <- rep(NA, length(trans_Tree[,1]) - length(names))
  id_vect <- c(names, na_vect)
  
  tmp <- as.data.frame(trans_Tree, stringsAsFactors = FALSE)
  tmp[,1:2] <- lapply(tmp[,1:2], as.numeric)
  
  dates <- tmp[, 1]
  First_date <- min(dates, na.rm = TRUE)
  First <- format(date_decimal(First_date), "%Y-%m-%d")
  
  report_out <- data.frame(matrix(NA, nrow = nrow(tmp), ncol = 5))
  for (r in seq_len(nrow(tmp))) {
    report_out[r, 1] <- r
    report_out[r, 2] <- ifelse(is.na(id_vect[r]), paste0("host_", r), id_vect[r])
    report_out[r, 3] <- format(date_decimal(tmp[r, 1]), "%Y-%m-%d", tz = "UTC")
    report_out[r, 4] <- format(date_decimal(tmp[r, 2]), "%Y-%m-%d", tz = "UTC")
    
    infector <- if (!is.na(tmp[r, 3]) && tmp[r, 3] > 0 && tmp[r, 3] <= length(names)) {
      names[tmp[r, 3]]
    } else {
      tmp[r, 3]
    }
    
    report_out[r, 5] <- ifelse(nchar(infector) < 3, paste0("host_", infector), infector)
  }
  
  colnames(report_out) <- c("host_no", "host_id", "infection_date", "sample_date", "infector")
  
  return(report_out)
}
