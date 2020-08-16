# Some helper ggplot function

################################################################################

# Colours
tol1qualitative=c("#4477AA")
tol2qualitative=c("#4477AA", "#CC6677")
tol3qualitative=c("#4477AA", "#DDCC77", "#CC6677")
tol4qualitative=c("#4477AA", "#117733", "#DDCC77", "#CC6677")
tol5qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677")
tol6qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677","#AA4499")
tol7qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677","#AA4499")
tol8qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499")
tol9qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499")
tol10qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")
tol11qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")
tol12qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499")


################################################################################

# function takes a list of plots and plots them vertically allinigng the y axis
grid.arrange_aligned <- function(lplots){
  
  # required packages
  require(ggplot2)
  require(gridExtra)
  require(grid)
  
  # convert plots to gtables
  lplots <- lapply(lplots, function(p) ggplot_gtable(ggplot_build(p)))
  
  # find max width
  maxWidth <- do.call("unit.pmax", lapply(lplots, function(p) p$widths))
  
  # set max width for all plots
  for(i in 1:length(lplots)){
    lplots[[i]]$widths <- maxWidth
  }
  
  # plot the column of plots
  do.call("grid.arrange", c(lplots, ncol =1))
}


################################################################################
# ggplot theme

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



################################################################################
# dplyr_summary

dplyr_summary <- function(df, full_name = F){
  output <- df %>%
    summarise_all(funs(mean = mean(., na.rm =T), 
                       sd = sd(., na.rm =T),
                       min = min(., na.rm =T), 
                       q25 = quantile(., 0.25, na.rm =T), 
                       median = median(., na.rm =T), 
                       q75 = quantile(., 0.75, na.rm =T), 
                       max = max(., na.rm =T))) %>% 
    gather(stat, val) %>%
    mutate(val = signif(val, digits = 4)) %>%
    separate(stat, into = c("var", "stat"), sep = "_") %>%
    spread(stat, val) %>%
    select(var, mean, sd, min, q25, median, q75, max) %>%
    as.data.frame() 
  if(full_name == T){
    output <- output %>%
      rename("standard deviation" = sd, "minimum" = min, "lower quartile" = q25, 
             "upper quartile" = q75, maximum = max)
  }
  output %>%
    column_to_rownames("var") %>%
    t()
}


grid.arrange_aligned_2 <- function(lplots){
  
  # required packages
  require(ggplot2)
  require(gridExtra)
  require(grid)
  
  # convert plots to gtables
  lplots <- lapply(lplots, function(p) ggplot_gtable(ggplot_build(p)))
  
  # find max width
  maxWidth <- do.call("unit.pmax", lapply(lplots, function(p) p$heights))
  
  # set max width for all plots
  for(i in 1:length(lplots)){
    lplots[[i]]$heights <- maxWidth
  }
  
  # plot the column of plots
  do.call("grid.arrange", c(lplots, nrow =1))
}

################################################################################
library(ggplot2)
library(gridExtra)
library(grid)


grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
}


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


################################################################################