
req('WGCNA')
net_ini <- function(inp_net, sample_names=NULL){
  # adds to net object:
  # N - number of modules not including grey
  # colList - list of colors including grey, length = N+1
  # colorsNamed - assignment of the genes to the colors
  # MEsNamed - matrix equal to net$MEs, but with

  inp_net$N <- ncol(inp_net$MEs) - 1L; # 17
  inp_net$colList <- labels2colors(0:inp_net$N); # grey turquoise blue ...
  inp_net$colorsNamed  <- labels2colors(inp_net$colors); # moduleColorsAutomatic

  inp_net$MEsNamed <- inp_net$MEs # ME0-ME17 MEsColors <- MEsAutomatic <-
  # 13 1 6 5 10

  # order colors as in the $MEs:
  inp_net$colList1 <- inp_net$colList[as.numeric(gsub('ME','',names(inp_net$MEsNamed)))+1]; # ME13 ME1 ... -> salmon turquoise ...
  names(inp_net$MEsNamed) <- 'ME_' %+% inp_net$colList1

  if (!is.null(sample_names)) row.names(inp_net$MEsNamed) <- sample_names;

  invisible(inp_net);
}

