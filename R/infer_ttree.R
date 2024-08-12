infer_ttree <- function(tree, stats, mcmcIterations=10000, w.shape, w.scale, dateT=2017) {
  
  root_age <- stats %>%
    filter(Parameter == "age.root.") %>%
    select(Median) %>%
    pull()
  
  tree_mrsd <- stats %>%
    filter(Parameter == "treeModel.rootHeight") %>%
    select(Median) %>%
    pull() + root_age
  
  tree_p <- ptreeFromPhylo(tree, dateLastSample = tree_mrsd)
  
  tt_out <- inferTTree(tree_p,
                       mcmcIterations = mcmcIterations,
                       w.shape = w.shape,
                       w.scale = w.scale,
                       dateT = dateT)
  
  return(tt_out)
}