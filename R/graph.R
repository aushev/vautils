# reset graphic state
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}


library(ggplot2)
legJ.BL <- theme(legend.justification=c(0,0)); # bottom-left
legJ.TL <- theme(legend.justification=c(0,1)); # top-left
legJ.BR <- theme(legend.justification=c(1,0)); # bottom-right
legP.BR <- theme(legend.position=c(1,0)); # bottom-right
legP.MM <- theme(legend.position=c(0.5,0.5)); # bottom-right
legP <- function(x,y){return(theme(legend.position=c(x, y)))}
ggX.notitle <- theme(axis.title.x = element_blank())
leg.no <- theme(legend.position="none")
