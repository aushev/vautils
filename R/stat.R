
se <- function(input, na.rm=T) {
  if(na.rm==T) {input <- na.omit(input);}
  return(sd(input)/sqrt(length(input)));
}

perc.ranks <- function(x) trunc(rank(x))/length(x)

NA_get <- function(obj){
  #if (class(obj)=="numeric") return(NA_real_);
  #if (class(obj)=="logical") return(NA_);
}

percent <- function(num1, ndig=2){
  paste0(format(round(100*num1, digits=ndig), nsmall=ndig),"%")
}

tab <- function(input, useNA='ifany', na.rm=F, do.sort=T, inpName=NA, ...){
  if (useNA==F | na.rm==T) useNA <- 'no';
  if (useNA==T | na.rm==F) useNA <- 'ifany';

  name1 <- deparse(substitute(input));
  df <- data.frame(table(input, useNA = useNA, ...));
  df <- df[df$Freq!=0,]
  sum1 <- sum(df$Freq);
  df$FreqP <- percent(df$Freq/sum1);
  print(names(df));
  df <- data.table(df);
  setnames(df, 'input', name1); # names(df)[names(df)=='input'] <- name1;
  print(order(-df$Freq));
  df <- df[order(-df$Freq)];
  print(names(df));
  return(df);
}

tabDF <- function(input, useNA='ifany', na.rm=F, do.sort=T, keepN=T, keepP=T, inpName=NA, ...){
  if (useNA==F | na.rm==T) useNA <- 'no';
  if (useNA==T | na.rm==F) useNA <- 'ifany';

  name1 <- deparse(substitute(input));
  df1 <- data.frame(table(input, useNA = useNA, ...));
  df1 <- df1[df1$Freq!=0,]
  sum1 <- sum(df1$Freq);
  if (keepP) {df1$FreqP <- percent(df1$Freq/sum1);}
#  print(names(df));
  if (is.na(inpName)) {
    names(df1)[names(df1)=='input'] <- name1
  } else names(df1)[names(df1)=='input'] <- inpName;

#  print(order(-df$Freq));
  if (do.sort) df1 <- df1[order(-df1$Freq),];
#  print(names(df));
  if (!keepN) {row.names(df1) <- NULL;}
  return(df1);
}

tabDT <- function(input, useNA='ifany', do.sort=T, ...){
  if (useNA==F) useNA <- 'no';
  name1 <- deparse(substitute(input));
  dt1 <- data.table(table(input, useNA = useNA, ...));
  setnames(dt1, 'N', 'Freq')
  print(names(dt1));
  print(is.data.table(dt1))
  dt1 <- dt1[Freq!=0,]
  sum1 <- sum(dt1$Freq);
  dt1[, FreqP:=percent(Freq/sum1)];
  setnames(dt1, 'input', name1);
#  print(order(-df$Freq));
#  print(names(dt));
  if (do.sort) dt1 <- dt1[order(Freq),];
#  print(names(dt1));
  return(dt1);
}

tab <- tabDF;

dt4mosaic <- function(inpDT, byX, byY){
  inpDT <- copy(inpDT)
  inpDT <- inpDT[,c(byX,byY),with=F]
  dt.stat <- as.data.table(tab(inpDT))
  dt.stat[,rel:=Freq/sum(Freq),by=get(byX)]
  dt.stat[,grpSize:=sum(Freq),by=get(byX)]
  return(dt.stat)
}

plot4mosaic <- function(inpDTmosaic, byX=NULL, byY=NULL, del=10, colors=NULL, colFreq='Freq'){
  if (is.null(byX)) byX <- names(inpDTmosaic)[1]
  if (is.null(byY)) byY <- names(inpDTmosaic)[2]
  inpDTmosaic[,grpSize:=grpSize/del]

  inpDTmosaic[,grpN:=sum(get(colFreq)),by=get(byX)]
  inpDTmosaic[, xN:=as.character(get(byX))]
  inpDTmosaic[, xN:=sprintf('%s\nn=%s',xN,grpN), by=.(xN,grpN)]

#  browser()

  p <-
    ggplot(inpDTmosaic,
           aes_string(x='xN',y='rel',fill=byY,width='grpSize')) +
    geom_bar(stat='identity') +
    scale_x_discrete(expand = c(0, 0)) +
    facet_grid(as.formula('~ ' %+% byX), scales = "free", space = "free")
  if (!is.null(colors)) p <- p + scale_fill_manual(values = colors)
  p + xlab(byX) + ylab(NULL)
}

# test1 <- function(input){
#   name1 <- deparse(substitute(input));
#   dt1 <- data.table(table(input, useNA = "ifany"));
#   setnames(dt1, 'N', 'Freq')
#   print(names(dt1));
#   print(is.data.table(dt1))
#   dt1 <- dt1[Freq!=0,]
# }
#
# test2 <- function(inpdt){
#   dt1 <- copy(inpdt)
#   setnames(dt1, 'N', 'Freq')
#   print(names(dt1));
#   print(is.data.table(dt1))
#   dt1 <- dt1[Freq!=0,]
# }
#
# test2a <- function(inpdt){
#   dt1 <- copy(inpdt)
#   print(names(dt1));
#   print(is.data.table(dt1))
#   dt1 <- dt1[N!=0,]
# }
tabv <- function(...){View(tab(...))}

mean2sd <- function(x) mean(x)+2*sd(x);

outlier.tukey <- function(data, x, k=1.5){
  lowerq = unname(quantile(data)[2]); # Q1
  upperq = unname(quantile(data)[4]); # Q3
  iqr = upperq - lowerq # IQR(data)
  upper = iqr*k + upperq
  lower = lowerq - iqr*k
  if (!missing(x)) {return(x>upper | x<lower);}
  else {return(sapply(data, outlier, data=data));}
}


closest <- function(data,x){
  data[which.min(abs(x-data))]
}

closest.dist.excl <- function(data,x){
  if (! x %in% data) stop("x not found in data!")
  .pos <- min(which(x==data)); # find (first) position of x in data
  data.rest <- data[c(-.pos)]; # exclude x from data
  return(min(abs(data.rest-x), na.rm = T));
}

closest.dists.excl <- function(data1){
  sapply(data1, closest.dist.excl, data=data1)
}

outlier.Dixon.Q <- function(data,x){
  if (! x %in% data) stop("x not found in data!")
  .gap <- closest.dist.excl(data,x);
  .range <- max(data, na.rm = T) - min(data, na.rm = T)
  return(.gap/.range);
}

outlier.Dixon <- function(data,x,conf=0.95){
  Q.table <- data.table(
    perc = c(rep(0.9,8), rep(0.95,8), rep(0.99,8)),
    num  = rep(3:10,3),
    Q    = c(0.941,0.765,0.642,0.56,0.507,0.468,0.437,0.412,0.97,0.829,0.71,0.625,0.568,0.526,0.493,0.466,0.994,0.926,0.821,0.74,0.68,0.634,0.598,0.568)
  )
  if (! length(data) %in% Q.table$num) stop('No value for length of data', length(data))
  if (! conf %in% Q.table$perc) stop('No value for confidence level ', conf)
  ref.val <- Q.table[perc==conf & num==length(data), Q];
  Q.val <- outlier.Dixon.Q(data,x);
  cat(ref.val, Q.val, '\n')
  return(Q.val>ref.val);
}
#vec1 <- c(189,167,187,183,186,182,181,184,181,177)/1000
#outlier.Dixon(vec1,.167,0.95)


# revPCA: transposes input data and returns PCA ####
revPCA <- function(inpdata, colexcl, sample, ...){
  if (class(colexcl)=="character") {colexcl <- match(colexcl, names(inpdata));}
  colexcl <- colexcl[!is.na(colexcl)]; # remove NA
  if (class(sample) =="numeric"  ) {sample  <- names(inpdata)[sample];}
  this_df <- data.frame(inpdata)[-(colexcl)];
  row.names(this_df) <- this_df[[sample]];
  this_df[[sample]] <- NULL;
  this_df <- t(this_df);
  return(prcomp(this_df, ...));
}


# avg(): like mean() but takes all arguments (not just the first one) ####
avg <- function(...){
  mean(c(...));
}

cut_my <- function(values,n){
  probs1 <- seq(0,1,1/n);
  q_norm <- quantile(values,probs=probs1,na.rm = T)

  if (anyDuplicated(q_norm)>0){
    breaks <- c(0,quantile(values[values!=0],probs=probs1,na.rm = T))
    labs <- 0:(length(breaks)-2)
  } else {
    breaks <- q_norm;
    labs <- 1:n
  }
  if (anyDuplicated(breaks)) {
    warning('Breaks are not unique! ');
    labs <- labs[!duplicated(breaks)[-1]]
    breaks <- breaks[!duplicated(breaks)];
    #return(NULL)
    }
  q <- cut(values, breaks = breaks, include.lowest = TRUE, right=FALSE, labels = labs)
  return(q);
}

cut_va0 <- function(values,n)
{
  values[values==0] <- NA
  cuts <- cut(values, quantile(values,probs=seq(0,1,1/n),na.rm = T), labels = F, include.lowest = T)
  cuts[is.na(cuts)] <- 0
  return(cuts)
}

add_q <- function(inpDT, inpCols, q=10L, verbose=F){
  inpDT <- copy(inpDT);
  for (this.col in inpCols){
    if (verbose==T) cat(this.col,'\n')
    if (this.col %!in% names(inpDT)){warning(this.col, ' not found!\n'); next;}
    vals <- inpDT[[this.col]]
    name_q <- this.col %+% '_q' %+% as.character(q);
    qvals <- as.numeric(cut_my(vals, q));
    inpDT[, (name_q):=qvals]
  }
  return(inpDT)
}


contingency <- function(inpDT, colTest, colReal, valNeg, valPos, percDigits=1){
  inpDT <- copy(inpDT)
  valNegTest <- valNeg[[1]]
  valNegReal <- valNeg[[2]]
  valPosTest <- valPos[[1]]
  valPosReal <- valPos[[2]]
  valOther <-'<OTHER>'


  inpDT[, .tmp.Test:=get(colTest)]
  inpDT[, .tmp.Real:=get(colReal)]

  inpDT[.tmp.Test==valNegTest & .tmp.Real==valNegReal, rez:='TrueNeg'];
  inpDT[.tmp.Test==valPosTest & .tmp.Real==valPosReal, rez:='TruePos'];
  inpDT[.tmp.Test==valNegTest & .tmp.Real==valPosReal, rez:='FalsNeg'];
  inpDT[.tmp.Test==valPosTest & .tmp.Real==valNegReal, rez:='FalsPos'];

  trueNeg <- sum(inpDT$rez=='TrueNeg', na.rm = T)
  truePos <- sum(inpDT$rez=='TruePos', na.rm = T)
  falsNeg <- sum(inpDT$rez=='FalsNeg', na.rm = T)
  falsPos <- sum(inpDT$rez=='FalsPos', na.rm = T)
  Sens <- truePos/(truePos+falsNeg)
  Spec <- trueNeg/(trueNeg+falsPos)
  PPV  <- truePos/(truePos+falsPos)
  NPV  <- trueNeg/(trueNeg+falsNeg)


  inpDT[.tmp.Test %!in% c(valNegTest, valPosTest), .tmp.Test:=valOther]
  inpDT[.tmp.Real %!in% c(valNegReal, valPosReal), .tmp.Real:=valOther]

  tab1 <- tab(inpDT[,.(Test=.tmp.Test,Real=.tmp.Real,class=rez)])
  tab1$Test %<>% factor(levels = c(valNegTest, valPosTest, valOther))
  tab1$Real %<>% factor(levels = c(valNegReal, valPosReal, valOther))
  tab2 <- dcast(as.data.table(tab1),Test~Real,value.var = 'Freq', fill=0)

  tab3 <- rbind(
    data.table(Metrics='Sensitivity', Calc=sprintf('%s / (%s + %s)', truePos, truePos, falsNeg), Value=percent(Sens,percDigits)),
    data.table(Metrics='Specificity', Calc=sprintf('%s / (%s + %s)', trueNeg, trueNeg, falsPos), Value=percent(Spec,percDigits)),
    data.table(Metrics='PPV', Calc=sprintf('%s / (%s + %s)', truePos, truePos, falsPos), Value=percent(PPV,percDigits)),
    data.table(Metrics='NPV', Calc=sprintf('%s / (%s + %s)', trueNeg, trueNeg, falsNeg), Value=percent(NPV,percDigits))
  )

  tab3a <- rbind(
    data.table(Metrics='Sensitivity', Calc=sprintf('%s / (%s + %s) = %s', truePos, truePos, falsNeg, percent(Sens,percDigits))),
    data.table(Metrics='Specificity', Calc=sprintf('%s / (%s + %s) = %s', trueNeg, trueNeg, falsPos, percent(Spec,percDigits))),
    data.table(Metrics='PPV',         Calc=sprintf('%s / (%s + %s) = %s', truePos, truePos, falsPos, percent(PPV,percDigits))),
    data.table(Metrics='NPV',         Calc=sprintf('%s / (%s + %s) = %s', trueNeg, trueNeg, falsNeg, percent(NPV,percDigits)))
  )

  cat('\n')
  print(tab1)
  cat('\n')
  print(tab2)
  cat('\n')
  print(tab3a)
  cat('\n')
  print(fisher.test(as.matrix(tab2,rownames = 'Test')))
  invisible(list(tab1=tab1,tab2=tab2,tab3=tab3))
}


scaleInt <- function(x){
  if (length(x)==1) return(0)
  scaled <- scale(x)[,1]
  ceiling(abs(scaled))*sign(scaled) # <- can this be done simpler?
}

unqN <- function(x) length(unique(x))
empty <- function(x) length(x)==0;
topN <- function(x,thr){sort(unique(x), decreasing = T)[1:thr]}
btmN <- function(x,thr){sort(unique(x), decreasing = F)[1:thr]}
