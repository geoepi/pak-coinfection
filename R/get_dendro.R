get_dendro <- function(subtree) {

  if (is.null(subtree) || !inherits(subtree, "phylo")) {
    stop("Invalid subtree: The input must be a valid 'phylo' object.")
  }

  dend <- tryCatch({
    chronopl(subtree, lambda = 0, age.min = 1)
  }, error = function(e) {
    stop("Error in chronopl transformation: ", e$message)
  })
  
  dend <- tryCatch({
    dend <- as.hclust(reorder(multi2di(dend)))
    dend <- as.dendrogram(dend)
    dend
  }, error = function(e) {
    stop("Error converting to dendrogram: ", e$message)
  })
  
  return(dend)
}
