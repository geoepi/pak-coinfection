extract_subtree <- function(tree_in) {

  tree_labs <- as.data.frame(tree_in$tip.label)
  names(tree_labs) <- "tip"
  
  tree_labs$subclinical <- grepl("_pro$", tree_labs$tip)

  subclinical_tips <- tree_labs %>%
    filter(subclinical == TRUE) %>%
    pull(tip)

  if (length(subclinical_tips) == 0) {
    stop("No subclinical tips found in the tree.")
  }
  
  subtree_result <- tryCatch({
    castor::get_subtree_with_tips(tree_in, only_tips = subclinical_tips)$subtree
  }, error = function(e) {
    stop("Error extracting the subtree: ", e$message)
  })
  
  return(subtree_result)
}
