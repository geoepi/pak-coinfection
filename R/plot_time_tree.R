plot_time_tree <- function(tree, stats, legend_pos = c(0.3, 0.7)) {
  
  root_age <- stats %>%
    filter(Parameter == "age.root.") %>%
    select(Median) %>%
    pull()
  
  tree_mrsd <- stats %>%
    filter(Parameter == "treeModel.rootHeight") %>%
    select(Median) %>%
    pull() + root_age
  
  tree$root.time <- root_age 
  
  
  seq_min <- floor(root_age)
  seq_max <- ceiling(tree_mrsd) + 1
  
  tips_df <- as.data.frame(
    cbind(
      label = tree$tip.label
    )
  )
  
  tips_df <- tips_df %>%
    mutate(Status = ifelse(grepl("_pro$", label), "Subclinical", "Clinical"))
  
  tree <- full_join(tree, tips_df, by = 'label')
  
  p <- ggtree(tree, mrsd = convert_decimal_date(tree_mrsd), 
       as.Date = FALSE, size = 0.5, col = "gray30") + 
  ggnewscale::new_scale_colour()  + 
  geom_tiplab(col = "gray40", size = 4, align = FALSE, offset = 0.5, linesize =0.5, hjust = 0.03) +
  geom_tippoint(aes(colour = Status, shape = Status), size = 5,
                position=position_nudge(x = 0.2)) +  # Make sure aes inside geom_tippoint
  scale_color_manual(values = c("Clinical" = "red", "Subclinical" = "blue")) +  # Customize accordingly
  scale_shape_manual(values = c("Clinical" = 16, "Subclinical" = 17)) +  # Customize accordingly
  scale_x_continuous(breaks = seq(seq_min, seq_max, 2), 
                     labels = seq(seq_min, seq_max, 2), limits = c(seq_min, seq_max + 2)) +
  theme_tree2(axis.title.x = element_text(size = 24, face = "bold"),
              axis.title.y = element_blank(),
              axis.text.x = element_text(face = "bold", size = 15, vjust = 1, 
                                         hjust = 1, angle = 45),
              axis.text.y = element_blank(),
              plot.title = element_text(size = 28, face = "bold")) + 
  theme(legend.position.inside = legend_pos,
        legend.position = "inside",
        legend.key.size = unit(2,"line"),
        legend.key.width = unit(3,"line"),
        legend.text = element_text(size=20, face="bold"),
        legend.title = element_text(size=25, face="bold"), 
        plot.margin = unit(c(1,8,1,0.1), "mm"))
  
  return(p)
}