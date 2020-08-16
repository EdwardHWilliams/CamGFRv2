ggplot_theme <- function(..., facet = F) {
  library(grid)
  library(ggthemes)
  #  (theme_foundation(base_size=base_size, base_family=base_family)
  t = theme_foundation() + 
    theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1.2)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(size=rel(1.2)), 
            axis.line = element_line(),
            axis.ticks = element_line(),
            axis.ticks.length = unit(0.25, "cm"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            #legend.key = element_rect(colour = NA),
            #legend.position = "right",
            #legend.direction = "vertical",
            #legend.key.size= unit(0.2, "cm"),
            #legend.margin = unit(0, "cm"),
            #legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,10,10,10),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold"), 
            ...
    )
  if(facet == T){
    t = t + 
      theme(axis.title.x = element_blank(), 
            strip.background = element_blank(), 
            strip.placement = "outside",
            text = element_text(size = 12), 
            panel.spacing.x = unit(8, "mm"),
            plot.margin=unit(c(2,8,2,2),"mm"))
  }
  t
}

