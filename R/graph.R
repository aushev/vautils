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



ggsaveopen <- function(fn, inpPlot=last_plot(), OUT=2, ...){
  if (exists('OUT') & OUT==0) {message("Skipping. "); return(FALSE);}
  fn <- trimws(fn)
  fn_dir  <- fs::path_dir(fn)
  fn_name <- fs::path_sanitize(fs::path_file(fn))

  fn <- ifelse(fn_dir=='.',fn_name,fs::path(fn_dir,fn_name))
  ext <- tools::file_ext(fn_name)
  if (exists(fn)) {warning('File already exists! Will try to save under different name. '); fn <- gsub(fn_name,nicedate() %+% fn_name,fn, fixed = T)}
  if ('list' %in% class(inpPlot)) {
    ggpubr::ggexport(filename=fn,plot=inpPlot, device=ext, ...)
  } else {
    ggsave(fn, inpPlot, ...)
  }
  if (exists('OUT') & OUT==1) {message("Saved but won't be open. "); return(FALSE);}
  system(command = paste0('cmd /C "', fn, '"'), wait = F);
}


# https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

gg_vec2colors <- function(inpVec, levs=sort(unique(inpVec)), shift=0L   ){
  retval <- NULL
  inpF <- factor(inpVec, levels=levs)
  N <- length(levels(inpF))
  if (is.numeric(shift) & length(shift)==1) retval <- gg_color_hue(N+shift)[as.numeric(inpF)+shift]
  if (is.character(shift) | length(shift)>1) retval <- shift[as.numeric(inpF)]

#  retval <- gg_color_hue(N+shift)[as.numeric(inpF)+shift]
  return(retval)
}


gg_replace_geomlabel <- function(inpPlot){
  outPlot <- copy(inpPlot)
  for (this_layer in inpPlot$layers){
    if ('GeomLabel' %in% class(this_layer$geom)){
      # message('GeomLabel!')
      this_map.fill.str <- as_label(this_layer$computed_mapping$fill)
      if (this_map.fill.str=="NULL") this_map.fill.str <- as_label(this_layer$mapping$fill)
      this_data <- this_layer$data
      new_aes <- this_layer$mapping
      new_aes$fill <- NULL
      new_aes <- aes_add(new_aes, aes_string(color=this_map.fill.str))

      for (this_param in names(this_layer$aes_params)){
        this_param_val <- this_layer$aes_params[[this_param]]
        if (this_param_val=='bold') next; # temporary clutch !!!
        tmp <- aes_string(this_param=this_param_val)
        names(tmp) <- this_param
        new_aes <- aes_add(new_aes, tmp)
      }

      outPlot <- outPlot + geom_text(data=this_data, mapping = new_aes)
    } # e. if
  } # e. for

  for (i in rev(seq_along(outPlot$layers))){
    this_layer <- outPlot$layers[[i]]
    if ('GeomLabel' %in% class(this_layer$geom)){
      outPlot$layers[[i]] <- NULL
    } # e. if
  } # e. for
  outPlot
}



pmod1 <- function(inp, coefs, lab.x=NA, lab.y=0.4){
  lab.s <- sprintf('HR = %.1f [%.1f - %.1f]', coefs$`exp(coef)`, coefs$CIl, coefs$CIh) # coefs[,2] === coefs$`exp(coef)`
  lab.s <- lab.s %+% ifelse(coefs$p<1e-3, sprintf('\np = %.2e',coefs$p), sprintf('\np = %.3f',coefs$p))

  n.risk <- inp$plot$data[inp$plot$data$time==0,]$n.risk
  lab.n <- 'N = ' %+% paste(n.risk, collapse='+') %+% ' = ' %+% sum(n.risk)
  #lab.s <- lab.s %+% '\n' %+% lab.n

  if (is.na(lab.x)){
    #message('Redefining lab.x')
    lab.x <- max(inp$data.survplot$time)*0.4
    #message('909: ',lab.x)
    if (!is.null(inp$data.survtable)) lab.x <- max(inp$data.survtable$time)*0.4
  }
  #message(lab.x)
  inp$plot <- inp$plot + annotate('text', x=lab.x, y=lab.y, label=lab.s, hjust=0, size=5)
  return(inp)
}



gg_labN <- function(inpPlot, yPos=NULL, ...){
  grpX <- as_label(inpPlot$mapping$x)
  if (is.null(yPos)) yPos <- 1.1 * suppressWarnings(ggplot_build(inpPlot)$layout$panel_params[[1]]$y.range[2])
  data1 <- inpPlot$data[, .(lblN='N='%+%.N), by=c(grpX)]
  #setnames(data1,'get',grpX)
  inpPlot + geom_text(aes(label=lblN,y=yPos, fill='black'), data = data1, ...)
}

gg_pie <- function(inp, colTitle='Var', colNum='Freq'){
  if (! 'data.frame' %in% class(inp)){
    inp <- tab(inp, inpName=colTitle)
    setnames(inp,'Freq',colNum)
#    DT::datatable(inp)
    # inp[[colTitle]] %<>% factor(levels = inp[[colTitle]])
  }
  setnames(inp, c(colNum,colTitle), cs('tmp.Freq tmp.Fill'))
  print(inp)
  ggplot(inp, aes(x="", y=tmp.Freq, fill=tmp.Fill)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme_void()
}

# gg_pie(dt1, colTitle = 'Study.ID')
# gg_pie(dt.qlik.summary$Study.ID) + leg.no
