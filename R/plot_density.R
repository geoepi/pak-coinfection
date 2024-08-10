plot_density <- function(values, max_x = 20, y_text = 0.1) {
 
  median_value <- median(values)
  
  p <- ggplot(data.frame(values), aes(x = values)) +
    geom_density(fill="lightblue", alpha=0.5) +  # Density plot
    geom_vline(aes(xintercept = median_value), linetype="dashed", color = "gray20", linewidth=1) + 
    annotate("text", x = median_value, y = 0.1, label = paste("Median =", round(median_value, 2)), 
             vjust = -0.5, color = "gray20", angle = 90, size = 4) +  
    scale_x_continuous(breaks = seq(0, max_x, by = 2), limits = c(0, max_x)) +
    theme_minimal() +
    labs(title = " ", x = "Days", y = "Density") +
    theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"),
          panel.grid.major = element_line(linewidth = 0.15),
          panel.grid.minor = element_line(linewidth = 0.05),
          legend.direction = "horizontal",
          legend.position="none",
          strip.text = element_text(size=16, face="bold"),
          strip.background = element_blank(),
          legend.key.size = unit(2,"line"),
          legend.key.width = unit(3,"line"),
          legend.text = element_text(size=16, face="bold"),
          legend.title = element_text(size=18, face="bold"),
          axis.title.x = element_text(size=18, face="bold"),
          axis.title.y = element_text(size=22, face="bold"),
          axis.text.x = element_text(face="bold", size=15, vjust=0.5, 
                                     hjust=0.5, angle=0),
          axis.text.y = element_text(size=10, face="bold"),
          plot.title = element_text(size=10, face="bold"),
          legend.spacing = unit(4, "cm"), 
          legend.margin = margin(t = 2, b = 1))
  
  return(p)
}