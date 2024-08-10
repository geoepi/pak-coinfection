nexus_label_swap <- function(input_file) {

  lines <- readLines(input_file)
  
  lines <- str_replace_all(lines, "'", "")
  lines <- str_replace_all(lines, ">", "")
  
  pattern <- "([^\\[]+)\\[&Description=\"([^\"]+)\"\\]"
  
  label_map <- list()
  
  for (i in seq_along(lines)) {
    matches <- str_match_all(lines[i], pattern)[[1]]
    if (nrow(matches) > 0) {
      for (j in 1:nrow(matches)) {
        original_label <- str_trim(matches[j, 2])
        description <- str_trim(matches[j, 3])
        label_map[[original_label]] <- description
        # swap LABEL and DESCRIPTION in pattern
        lines[i] <- sub(paste0(original_label, "\\[&Description=\"", description, "\"\\]"),
                        paste0(description, "[&Description=\"", original_label, "\"]"),
                        lines[i])
      }
    }
  }
  
  for (original_label in names(label_map)) {
    description <- label_map[[original_label]]

    lines <- str_replace_all(lines, 
                             pattern = paste0("(?<!&Description=\")\\b", original_label, "\\b"),
                             replacement = description)
  }
  
  output_file <- sub("\\.nex$", "_rev.nex", input_file)
  
  writeLines(lines, output_file)
  
  return(output_file)
}