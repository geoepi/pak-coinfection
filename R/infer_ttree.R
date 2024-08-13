infer_ttree <- function(tree, stats, mcmcIterations=10000, w.shape, w.scale, dateT=2017) {
  
  root_age <- stats %>%
    filter(Parameter == "age.root.") %>%
    select(Median) %>%
    pull()
  
  tree_mrsd <- stats %>%
    filter(Parameter == "treeModel.rootHeight") %>%
    select(Median) %>%
    pull() + root_age
  
  if(min(tree$edge.length) < 0.0001){
    tree$edge.length <- pmax(tree$edge.length,1/365)
  }
  
  tree_p <- ptreeFromPhylo(tree, dateLastSample = tree_mrsd)
  
  tt_out <- inferTTree(tree_p,
                       mcmcIterations = mcmcIterations,
                       w.shape = w.shape,
                       w.scale = w.scale,
                       dateT = dateT)
  
  return(tt_out)
}