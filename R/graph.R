# reset graphic state
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}


reqq(ggplot2)

# annotate <- ggplot2::annotate


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




gghist_quick <- function(values, breaks=50, to.numeric=T) {
#  browser()
  if (to.numeric==T) values <- as.numeric(values)
  hist_base <- hist(values, plot=FALSE, breaks=breaks)

  dat <- data.frame(xmin=head(hist_base$breaks, -1L),
                    xmax=tail(hist_base$breaks, -1L),
                    ymin=0.0,
                    ymax=hist_base$counts)

  ggplot(dat, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) +
    geom_rect(size=0.5, colour="grey30", fill="grey80")
}



gghist_slow <- function(values, breaks=50, to.numeric=T) {
#  browser()
  if (to.numeric==T) values <- as.numeric(values)
  dat <- data.frame(values=values)

  ggplot(dat, aes(x=values)) +
    geom_histogram(size=0.5, colour="grey30", fill="grey80")
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



ggsaveopen <- function(fn, inpPlot=last_plot(), OUT=2, device=NULL, ...){
  if (exists('OUT') & OUT==0) {message("Skipping. "); return(FALSE);}
  fn <- trimws(fn)
  fn_dir  <- fs::path_dir(fn)
  fn_name <- fs::path_sanitize(fs::path_file(fn))

  fn <- ifelse(fn_dir=='.',fn_name,fs::path(fn_dir,fn_name))
  ext <- tools::file_ext(fn_name)
  #if (!exists('device') || is.na(device)) device <- ext;
  if (exists(fn)) {warning('File already exists! Will try to save under different name. '); fn <- gsub(fn_name,nicedate() %+% fn_name,fn, fixed = T)}
  message('Saving as ' %+% bold(fn) )
#  browser()
  if ('list' %in% class(inpPlot)) {
    ggpubr::ggexport(filename=fn,plot=inpPlot, device=device, ...)
  } else {
    ggsave(fn, inpPlot, device=device, ...)
  }
  if (exists('OUT') & OUT==1) {message("Saved but won't be open. "); return(FALSE);}
  cmd_str <- ifelse(Sys.info()['sysname'] == 'Windows', 'cmd /C "', 'open "')
  ret <- system(command = paste0(cmd_str, fn, '"'), wait = F);
#  browser()
}


# https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

gg_vec2colors <- function(inpVec, levs=sort(unique(inpVec)), shift=0L, valNA=NA){
  retval <- NULL
  inpF <- factors(as.character(inpVec), newlevels=as.character(levs))
#   N0 <- length(start) # number of colors already provided
  N  <- length(levels(inpF)) # total number of levels/colors
  if (is.numeric(shift) & length(shift)==1) retval <- gg_color_hue(N+shift)[as.numeric(inpF)+shift]
  if (is.character(shift) | length(shift)>1) retval <- shift[as.numeric(inpF)]
  if (not.na(valNA)) retval[is.na(retval)] <- valNA

#  retval <- gg_color_hue(N+shift)[as.numeric(inpF)+shift]
  return(retval)
}


gg_replace_geomlabel <- function(inpPlot){
  outPlot <- copy(inpPlot)
  for (this_layer in inpPlot$layers){
    cat('\n', paste0(bold(class(this_layer$geom)),collapse = ', '))
    if ('GeomLabel' %in% class(this_layer$geom)){
      # message('GeomLabel!')
      this_map.fill.str <- as_label(this_layer$computed_mapping$fill)
      if (this_map.fill.str=="NULL") this_map.fill.str <- as_label(this_layer$mapping$fill)
      this_data <- this_layer$data
      new_aes <- this_layer$mapping
      new_aes$fill <- NULL
      new_aes <- aes_add(new_aes, aes_string(color=this_map.fill.str))

#      browser()

      for (this_param in names(this_layer$aes_params)){
        this_param_val <- this_layer$aes_params[[this_param]]
        message(bold(this_param),'=',bold(this_param_val));
        if (this_param_val=='bold') next; # temporary clutch !!!
        if (this_param=='colour') next; # WTF?!?!?! temporary clutch !!!
        tmp <- aes_string(this_param=this_param_val)
        names(tmp) <- this_param
        new_aes <- aes_add(new_aes, tmp)
      }

      outPlot <- outPlot + geom_text(data=this_data, mapping = new_aes)
    } # e. if
  } # e. for
# browser()
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
  inp$plot <- inp$plot + ggplot2::annotate('text', x=lab.x, y=lab.y, label=lab.s, hjust=0, size=5)
  return(inp)
}



gg_labN <- function(inpPlot, yPos=Inf, vjust=1, prefix='N=', ...){
  grpX <- as_label(inpPlot$mapping$x)
  grpY <- as_label(inpPlot$mapping$y)
  if (is.null(yPos)) yPos <- 1.1 * suppressWarnings(ggplot_build(inpPlot)$layout$panel_params[[1]]$y.range[2])
  data1 <- inpPlot$data[, .(lblN=prefix%+%.N), by=c(grpX)]
  #setnames(data1,'get',grpX)
  inpPlot + geom_text(aes(label=lblN,y=yPos, fill='blue', color='blue',vjust=vjust), data = data1, ...)
#  inpPlot + geom_text(aes(label=lblN,y=yPos), data = data1, ...)
}

gg_labNb <- function(inpPlot, yPos=Inf, vjust=0, ...){
  grpX <- as_label(inpPlot$mapping$x)
  grpY <- as_label(inpPlot$mapping$y)
  if (is.null(yPos)) yPos <- 1.1 * suppressWarnings(ggplot_build(inpPlot)$layout$panel_params[[1]]$y.range[2])
  data1 <- inpPlot$data[, .(N=sumI(get(grpY))), by=c(grpX)]
  data1[,lblN := 'N='%+% N]
  #setnames(data1,'get',grpX)
#  inpPlot + geom_text(aes(label=lblN,y=yPos, fill='blue', color='blue',vjust=vjust), data = data1, ...)
  inpPlot + annotate('text',label=data1$lblN, x=data1$yearS, y=data1$N, vjust=vjust)
#  inpPlot + geom_text(aes(label=lblN,y=yPos), data = data1, ...)
}

gg_pie <- function(inp, colTitle='Var', colNum='Count'){
  if (! 'data.frame' %in% class(inp)){
    inp <- tab(inp, inpName=colTitle)
    setnames(inp,'Count',colNum)
#    DT::datatable(inp)
    # inp[[colTitle]] %<>% factor(levels = inp[[colTitle]])
  } else inp <- copy(inp)
#  setnames(inp, c(colNum,colTitle), cs('tmp.Freq tmp.Fill'))
  print(inp)
#  ggplot(inp, aes(x="", y=tmp.Freq, fill=tmp.Fill)) +
  ggplot(inp, aes_string(x="1", y=colNum, fill=colTitle)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme_void()
}

# gg_pie(dt1, colTitle = 'Study.ID')
# gg_pie(dt.qlik.summary$Study.ID) + leg.no


stat_build_barplot <- function(inp, threshold=NA, inp.label=NA, refactor=T, showN=F, show.leg=F, labY=NA){
#  browser()
  dt.stat <- tab(inp)
  dt.stat[, inpS:=inp]
  if (refactor==T) dt.stat[, inpS:=as.character(inp)]
  if (not.na(threshold)) {
    dt.stat[as.numeric(inpS)>=threshold, inpS:=threshold %+% '+']
    dt.stat[,Count:=sum(Count),by=inpS]
    dt.stat$inp <- NULL
    dt.stat$FreqP <- NULL
    dt.stat <- unique(dt.stat)
  }
  if (refactor==T) dt.stat$inpS %<>% factor(ordered=T)

  if (F){
    dt.stat %<>% setnames('inpS', inp.label)
  }

  pRet <-
    dt.stat %>%
    ggplot(aes(x=inpS, y=Count, fill=inpS)) +
    geom_bar(stat = 'identity')

  if (not.na(inp.label)){
    pRet <- pRet + xlab(inp.label) + guides(fill=guide_legend(title=inp.label))
  }

  if (showN==T){
    pRet <- pRet + geom_text(aes(label=Count), vjust=-0.2)
  }

  if (show.leg==F) pRet <- pRet + leg.no
  if (not.na(labY)) pRet <- pRet + ylab(labY)

  pRet
}


layer_if <- function(inpDT, geom,...){
  # browser()
  catch_ret <- tryCatch(
    inpDT,
    error = function(cond) {
      warning(cond);
      return(NULL);
    }
  )

  if (is.null(catch_ret)) return(NULL);
  if (nrow(inpDT)==0) return(NULL);
  return(geom(data=inpDT,...))
}

geom_box_custom <- function(inpDT, byX, colY, q=0.25, barwidth=0.5, ...){
  dt.stat <- inpDT[,.(
    medVal=median(get(colY)),
    avgVal=median(get(colY)),
    qLow=quantile(get(colY), q),
    qHigh=quantile(get(colY), 1-q)
  ),by=c(byX)]
  dt.stat[,IQR:=qHigh-qLow]
  geom_tile(aes_string(x=byX, y='medVal', height='IQR'), data=dt.stat, width=barwidth,...)
}


gghist <- function(inpDT,val.col,col.mean='red',col.med='darkgreen',lg10=NA,col.line='grey30',col.fill='grey80', xlab=val.col,...){
  values <- as.numeric(inpDT[[val.col]])
  val.median <- median(values,na.rm=T)
  val.mean <- mean(values,na.rm=T)
  val.05 <- quantile(values, 0.05, na.rm=T) # 6.14 all,  2.74 circ
  val.95 <- quantile(values, 0.95, na.rm=T) # 8.76 all, 17.87 circ

  pHist <-
    inpDT %>%
    ggplot(aes(x=values)) +
    geom_histogram(colour=col.line,fill=col.fill,...) +
    geom_vline(xintercept = c(val.05,val.95), linetype='dashed', col='darkgrey')+
    annotate('text', x=val.05, y=Inf, label=round(val.05,2), col='darkgrey', angle=90, vjust=-0.5, hjust=1.2)+
    annotate('text', x=val.95, y=Inf, label=round(val.95,2), col='darkgrey', angle=90, vjust=-0.5, hjust=1.2)+

    geom_vline(xintercept = val.median, linetype='dashed', col=col.med)+
    annotate('text', x=val.median, y=0, label=round(val.median,2), col=col.med, angle=90, vjust=-0.5, hjust=-0.2)+
    geom_vline(xintercept =   val.mean, linetype='dashed', col=col.mean)+
    xlab(xlab) + ylab('Counts')+
    annotate('text', x=val.mean, y=0, label=round(val.mean,2), col=col.mean, angle=90, vjust=-0.5,hjust=-0.2)

  if (lg10 %~~% 'x' | lg10 %==% T) pHist <- pHist+scale_x_log10()
  if (lg10 %~~% 'y') pHist <- pHist+scale_y_log10()

  pHist
}

gg_va_rescale <- function(inp, base=3, basemin=1,basemax=6){
  # browser()
  if (!is.numeric(inp)) inp <- as.numeric(inp)
  inp[inp<=0] <- NA
  inp <- log10(inp)
  inp.min <- minI(inp)
  inp.max <- maxI(inp)
  inp.delta <- (inp-inp.min)
  inp.scale <- (inp.max-inp.min)
  base.scale <- basemax-basemin
  ret <- basemin + inp.delta*base.scale/inp.scale
  ret[is.na(ret)] <- base
  return(ret)
}
