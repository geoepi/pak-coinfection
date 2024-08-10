plot_tree_serotype <- function(tree) {
  
  tree_df <- as.data.frame(
    tree$tip.label
  )
  
  names(tree_df) <- "label"
  
  tree_df$serotype <- sub("/.*", "", tree_df$label)
  
  tree_df <- tree_df %>%
    mutate(subclinical = ifelse(grepl("_pro$", label), "Subclinical", "Clinical"))
  
  tmp_tree <- full_join(tree, tree_df, by = 'label')
  
  p <- ggtree(tmp_tree, size=0.5, col = "gray30", 
              ladderize=TRUE) + 
    geom_tiplab(col="gray40", size=2, align=FALSE, offset = 0.009, hjust = .5) +
    geom_tippoint(aes(colour=serotype, shape=serotype)) +
    theme(plot.margin = unit(c(2,8,5,8), "mm"),
          axis.title.x = element_text(size=24, face="bold"),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks=element_blank(),
          axis.line=element_blank(), 
          legend.direction = "vertical",
          legend.position= "inside",
          legend.position.inside = c(0.2, 0.5),
          strip.text = element_blank(), 
          strip.background = element_blank(),
          legend.key.size = unit(2,"line"),
          legend.key.width = unit(3,"line"),
          legend.text = element_text(size=16, face="bold"),
          legend.title = element_text(size=16, face="bold"),
          plot.title = element_text(size=28, face="bold")) +
    geom_treescale(width=0.02) +
    labs(colour = "Serotype", shape = "Serotype") +
    guides(colour = guide_legend(override.aes = list(size=4))) 
  
  return(p)
}
