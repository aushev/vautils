# reset graphic state
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}


reqq(ggplot2)

gg_nogrids <- theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())

legJ.BL <- legJ.DL <- theme(legend.justification=c(0,0)); # bottom-left
legJ.TL <- legJ.UL <- theme(legend.justification=c(0,1)); # top-left
legJ.BR <- legJ.DR <- theme(legend.justification=c(1,0)); # bottom-right
legJ.TR <- legJ.UR <- theme(legend.justification=c(1,1)); # upper-right


legP.MM <- theme(legend.position=c(0.5,0.5)); # middle-middle
legP.BL <- legP.DL <- theme(legend.position=c(0,0)) + legJ.BL; # bottom-left
legP.BR <- legP.DR <- theme(legend.position=c(1,0)) + legJ.BR; # bottom-right
legP.TL <- legP.UL <- theme(legend.position=c(0,1)) + legJ.TL; # upper-left
legP.TR <- legP.UR <- theme(legend.position=c(1,1)) + legJ.TR; # upper-right

legP <- function(x,y){return(theme(legend.position=c(x, y)))}

ggX.notitle <- theme(axis.title.x = element_blank())
leg.no <- theme(legend.position="none")
ggH <- geom_hline(yintercept = 0)
ggV <- geom_vline(xintercept = 0)
ggD <- geom_abline(slope = 1)


# Multiple plot function ######################################
# from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL, byrow=T) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols), byrow=byrow)
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# example usage:
# aes.point=aes(fill=Tissue, color=Run.by, shape=Run.by, label=Sample.ID)
# ggplot(data = inpdata, aes_add(aes(x=tmp.id, y=sum.pos),aes.point)) + ...
aes_add <- function(aes1,aes2){
  aesAll <- aes2
  only1 <- setdiff(names(aes1),names(aes2))
  aes1 <- aes1[only1]
  aesAll[only1] <- aes1[only1]
  return(aesAll)
}




gghist_quick <- function(values, breaks=50) {
  hist_base <- hist(values, plot=FALSE, breaks=breaks)

  dat <- data.frame(xmin=head(hist_base$breaks, -1L),
                    xmax=tail(hist_base$breaks, -1L),
                    ymin=0.0,
                    ymax=hist_base$counts)

  ggplot(dat, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) +
    geom_rect(size=0.5, colour="grey30", fill="grey80")
}

annotation_compass <- function(label,position='N',
                               padding=grid::unit(c(0.5,0.5),"line"), ...){
  #position <- match.arg(position)
  x <- switch (position,
               W=0,NW=0,SW=0,
               N=0.5, S=0.5,
               E=1,NE=1,SE=1)
  y <- switch (position,
               S=0, SE=0,SW=0,
               E=0.5,W=0.5,
               N=1,NE=1,NW=1)
  hjust <- x
  vjust <- y
  f1 <- switch (position,
                E=-1,NE=-1,SE=-1,
                N=0,S=0,
                W=1,NW=1,SW=1 )
  f2 <- switch (position,
                N=-1,NE=-1,NW=-1,
                W=0,E=0,
                S=1, SE=1,SW=1 )
  annotation_custom(grid::textGrob(label,
                                   x=grid::unit(x,"npc") + f1*padding[1] ,
                                   y=grid::unit(y,"npc") + f2*padding[2],
                                   hjust=hjust,vjust=vjust, ...))
}



ggsaveopen <- function(fn, inpPlot=last_plot(), ...){
  if (exists('OUT') & OUT==F) return(FALSE);
  ext <- tools::file_ext(fn)
  if ('list' %in% class(inpPlot)) {
    ggpubr::ggexport(filename=fn,plot=inpPlot, device=ext, ...)
  } else {
    ggsave(fn, inpPlot, ...)
  }
  system(command = paste0('cmd /C ', fn));
}
