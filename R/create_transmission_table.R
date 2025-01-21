create_transmission_table <- function(mcmc_Tree) {
  # last iteration from mcmc_Tree
  last_iteration <- mcmc_Tree[[length(mcmc_Tree)]]
  combined_tree <- last_iteration$ctree
  
  # transmission tree and names
  extracted_data <- extractTTree(combined_tree)
  transmission_tree <- extracted_data$ttree
  tip_names <- extracted_data$nam
  
  # missing names
  na_vector <- rep("NA", nrow(transmission_tree) - length(tip_names))
  id_vector <- c(tip_names, na_vector)
  
  transmission_df <- as.data.frame(transmission_tree)
  transmission_df <- data.frame(lapply(transmission_df, function(col) as.numeric(as.character(col))))
  
  # data frame
  trans_table <- data.frame(matrix(NA, nrow = nrow(transmission_df), ncol = ncol(transmission_df) + 2))
  
  # populate data frame
  for (row in seq_len(nrow(transmission_df))) {
    trans_table[row, 1] <- row
    trans_table[row, 2] <- id_vector[row]
    trans_table[row, 3] <- format(date_decimal(transmission_df[row, 1]), "%Y-%m-%d")
    trans_table[row, 4] <- format(date_decimal(transmission_df[row, 2]), "%Y-%m-%d")
    
    infector_id <- transmission_df[row, 3]
    if (0 < infector_id && infector_id <= length(tip_names)) {
      trans_table[row, 5] <- tip_names[infector_id]
    } else {
      trans_table[row, 5] <- infector_id
    }
  }
  
  colnames(trans_table) <- c("id", "infected_tip", "infected_date", "sample_date", "infector_tip")
  
  return(trans_table)
}
