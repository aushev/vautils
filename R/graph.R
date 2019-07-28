# reset graphic state
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}


req(ggplot2)

gg_nogrids <- theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())

legJ.BL <- theme(legend.justification=c(0,0)); # bottom-left
legJ.TL <- theme(legend.justification=c(0,1)); # top-left
legJ.BR <- theme(legend.justification=c(1,0)); # bottom-right
legJ.UR <- theme(legend.justification=c(1,1)); # upper-right

legP.MM <- theme(legend.position=c(0.5,0.5)); # middle-middle
legP.BR <- theme(legend.position=c(1,0)) + legJ.BR; # bottom-right
legP.UR <- theme(legend.position=c(1,1)) + legJ.UR; # upper-right

legP <- function(x,y){return(theme(legend.position=c(x, y)))}

ggX.notitle <- theme(axis.title.x = element_blank())
leg.no <- theme(legend.position="none")
ggH <- geom_hline(yintercept = 0)
ggV <- geom_vline(xintercept = 0)



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
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
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
                     ncol = cols, nrow = ceiling(numPlots/cols))
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
