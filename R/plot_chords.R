plot_chords <- function(gene, links_df, serotype_palette, farm_palette, animal_palette) {
  
  gene_links <- links_df[links_df$gene == gene, ]
  gene_links <- gene_links[order(gene_links$animal, gene_links$serotype, gene_links$label), ]
  
  sector_order <- c(unique(gene_links$label), unique(gene_links$farm_code))
  
  sample_colors <- setNames(serotype_palette[gene_links$serotype], gene_links$label)
  animal_colors <- setNames(animal_palette[gene_links$animal], gene_links$label)
  all_colors <- c(sample_colors, farm_palette)
  
  circos.clear()
  chordDiagram(
    x = gene_links[, c("label", "farm_code")], 
    grid.col = all_colors,  
    order = sector_order,
    annotationTrack = "grid",
    transparency = 0.5,
    preAllocateTracks = 2 
  )
  # animal indicator
  circos.trackPlotRegion(
    track.index = 1,
    bg.border = NA,
    panel.fun = function(x, y) {
      sector.name <- get.cell.meta.data("sector.index")
      if (sector.name %in% gene_links$label) {
        rect_color <- animal_colors[sector.name]

        xlim <- get.cell.meta.data("xlim", sector.index = sector.name)
        ylim <- get.cell.meta.data("ylim", sector.index = sector.name)
        ybottom <- ylim[1] + -1.1 
        ytop <- ylim[2] + -2 
        
        circos.rect(
          xleft = xlim[1],
          xright = xlim[2],
          ybottom = ybottom,
          ytop = ytop,
          col = rect_color, 
          border = rect_color
        )
      }
    }
  )
  
  # labels
  circos.trackPlotRegion(
    track.index = 1,
    bg.border = NA,
    panel.fun = function(x, y) {
      sector.name <- get.cell.meta.data("sector.index")
      if (sector.name %in% gene_links$label) { 
        circos.text(
          x = mean(get.cell.meta.data("xlim")), 
          y = -0.9,  
          labels = sector.name, 
          facing = "clockwise", 
          adj = c(0, 0.5), 
          cex = 0.4 
        )
      }
    }
  )
  
}
