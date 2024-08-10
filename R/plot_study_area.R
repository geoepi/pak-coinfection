plot_study_area <- function(background_map, farms_locs) {
  
  p <- ggmap(background_map) +
    geom_point(data = farms_locs, 
               aes(x = coord_y, y = coord_x), 
               color = "red", size = 5, alpha = 0.5) +
    xlim(bbox$min_lon, bbox$max_lon) +
    ylim(bbox$min_lat, bbox$max_lat) +
    ggrepel::geom_text_repel(data = farms_locs, aes(x = coord_y, y = coord_x, 
                                                    label = farm_code), 
                             size = 3.5, color = "blue", max.overlaps = 50) +
    theme_classic() +
    theme(plot.margin = unit(c(2,8,5,8), "mm"),
          axis.title.x = element_text(size=24, face="bold"),
          axis.title.y = element_text(size=24, face="bold"),
          axis.text.x = element_text(size=12, face="bold"),
          axis.text.y = element_text(size=12, face="bold"),
          legend.direction = "vertical",
          legend.position= "none",
          strip.text = element_blank(), 
          strip.background = element_blank(),
          legend.key.size = unit(2,"line"),
          legend.key.width = unit(3,"line"),
          legend.text = element_text(size=16, face="bold"),
          legend.title = element_text(size=16, face="bold"),
          plot.title = element_text(size=28, face="bold")) +
    labs(title = " ", x = "Longitude", y = "Latitude") 
  
    p <- p + annotation_scale(location = "tl", 
                                            width_hint = 0.4) + 
    coord_sf(crs = 4326)
  
  return(p)
}