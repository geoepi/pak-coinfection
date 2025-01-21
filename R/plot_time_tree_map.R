plot_time_tree_map <- function(tree, stats, metadata, title = "Time-Calibrated Tree", 
                               legend_title = "Farm Code") {
  
  # root age and mrsd
  options(ignore.negative.edge = TRUE)
  
  root_age <- stats %>%
    filter(Parameter == "age.root.") %>%
    pull(Median)
  
  tree_mrsd <- stats %>%
    filter(Parameter == "treeModel.rootHeight") %>%
    pull(Median) + root_age
  
  tree$root.time <- root_age  
  
  seq_min <- floor(root_age)
  seq_max <- ceiling(tree_mrsd) + 1
  
  # metadata
  if (!"label" %in% colnames(metadata)) {
    stop("Metadata must include a 'label' column that matches tree tip labels.")
  }
  
  # tree plot
  p <- ggtree(tree, mrsd = convert_decimal_date(tree_mrsd), as.Date = FALSE) %<+% metadata +
    geom_tippoint(aes(color = farm_code), size = 3) +
    scale_color_manual(values = farm_palette) +
    geom_tiplab(aes(label = label), size = 2, hjust = -0.3) +
    theme_tree2() +  
    scale_x_continuous(breaks = seq(seq_min, seq_max, 2), 
                       labels = seq(seq_min, seq_max, 2), 
                       limits = c(seq_min, seq_max + 2)) +
    labs(title = title, color = legend_title, x = "Time (Years)") +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      legend.key = element_rect(fill = "white")
    )
  
  return(p)
}
