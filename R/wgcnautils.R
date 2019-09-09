
net_ini <- function(inp_net, sample_names=NULL){
reqq('WGCNA')
  # adds to net object:
  # N - number of modules not including grey
  # colList - list of colors including grey, length = N+1
  # colorsNamed - assignment of the genes to the colors
  # MEsNamed - matrix equal to net$MEs, but with

  inp_net$N <- ncol(inp_net$MEs) - 1L; # 17
  inp_net$colList <- labels2colors(0:inp_net$N); # grey turquoise blue ...
  inp_net$colorsNamed  <- labels2colors(inp_net$colors); # moduleColorsAutomatic
  inp_net$colorsNamed %<>% factor(levels = inp_net$colList)

  inp_net$MEsNamed <- inp_net$MEs # ME0-ME17 MEsColors <- MEsAutomatic <-
  # 13 1 6 5 10

  # order colors as in the $MEs:
  inp_net$colList1 <- inp_net$colList[as.numeric(gsub('ME','',names(inp_net$MEsNamed)))+1]; # ME13 ME1 ... -> salmon turquoise ...
  names(inp_net$MEsNamed) <- 'ME_' %+% inp_net$colList1

  if (!is.null(sample_names)) row.names(inp_net$MEsNamed) <- sample_names;

  invisible(inp_net);
}

scaleFreePlot.makeVA <- function (connectivity, nBreaks = 10, truncated = FALSE, removeFirst = FALSE, main = "", ...)
{
reqq('WGCNA')
  k <- connectivity
  discretized.k <- cut(k, nBreaks)
  dk <- tapply(k, discretized.k, mean)
  p.dk <- as.vector(tapply(k, discretized.k, length)/length(k))

  breaks1 <- seq(from = min(k), to = max(k), length = nBreaks + 1)
  hist1 <- suppressWarnings(hist(k, breaks = breaks1, equidist = FALSE, plot = FALSE, right = TRUE, ...))

  dk2 <- hist1$mids
  dk <- ifelse(is.na(dk), dk2, dk)
  dk <- ifelse(dk == 0, dk2, dk)
  p.dk <- ifelse(is.na(p.dk), 0, p.dk)
  log.dk <- as.vector(log10(dk))
  if (removeFirst) {
    p.dk <- p.dk[-1]
    log.dk <- log.dk[-1]
  }
  log.p.dk <- as.numeric(log10(p.dk + 1e-09))
  lm1 <- lm(log.p.dk ~ log.dk)


  OUTPUT <- data.frame(
    scaleFreeRsquared = round(summary(lm1)$adj.r.squared,2),
    slope = round(lm1$coefficients[[2]],2)
  )

  title.rsq <- as.character(round(summary(lm1)$adj.r.squared,2))
  title.slp <- round(lm1$coefficients[[2]],2)
  title.trunc <- ''

  if (truncated == TRUE) {
    lm2 <- lm(log.p.dk ~ log.dk + I(10^log.dk))
    OUTPUT$TruncatedRsquared <- round(summary(lm2)$adj.r.squared,2)
    title.trunc <- paste(", trunc.R^2=", as.character(round(summary(lm2)$adj.r.squared,2)) )
    title.rsq <- paste(" scale free R^2=", title.rsq)
    #printFlush("the red line corresponds to the truncated exponential fit")
  } # e. if truncated
  else {
    title.rsq <- paste(" scale R^2=", title.rsq)
  }

  title <- paste(main, title.rsq, ", slope=", title.slp, title.trunc)

  df.plot <- data.frame(log.dk=log.dk, log.p.dk=log.p.dk)

  #  suppressWarnings(plot(log.dk, log.p.dk, xlab = "log10(k)", ylab = "log10(p(k))", main = title, ...))

  p1 <-
    ggplot(df.plot, aes(x=log.dk, y=log.p.dk)) + xlab('log10(k)') + ylab('log10(p(k))') + ggtitle(title)

  p1 <- p1 + geom_point()
  p1 <- p1 + geom_line(aes(x=log.dk, y=predict(lm1)))

  if (truncated)
    p1 <- p1 + geom_line(aes(x=log.dk, y=predict(lm2), color=2))

  return(list(plot=p1, tab=OUTPUT))

}
