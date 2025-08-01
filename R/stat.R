
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
  setnames(df,'Freq','Count')
  if (nrow(df)==0) {warning(' Empty!'); return(NULL);}
  df <- df[df$Count!=0,]
  sum1 <- sum(df$Count);
  df$FreqP <- percent(df$Count/sum1);
  print(names(df));
  df <- data.table(df);
  setnames(df, 'input', name1); # names(df)[names(df)=='input'] <- name1;
  print(order(-df$Count));
  df <- df[order(-df$Count)];
  print(names(df));
  return(df);
}

tabDF <- function(input, useNA='ifany', na.rm=F, do.sort=T, keepN=T, keepP=T, inpName=NA, thrRank=NA, thrNum=NA, thrLabel='Other',...){
  if (useNA==F | na.rm==T) useNA <- 'no';
  if (useNA==T | na.rm==F) useNA <- 'ifany';

  name1 <- deparse(substitute(input));
  if (is.data.table(input)){
    vec.by <- names(input)
    dt.ret <- input[,.(Count=.N),by=vec.by]
    if (do.sort) dt.ret <- dt.ret[order(-Count),];
    colVal <- first(names(dt.ret))
    colFreq <- last(names(dt.ret))

  } else {
    df1 <- data.frame(table(input, useNA = useNA, ...));
    setnames(df1,'Freq','Count')
    if (nrow(df1)==0) return(NULL);

    df1 <- df1[df1$Count!=0,]

    if (do.sort) df1 <- df1[order(-df1$Count),];
    if (!keepN) {row.names(df1) <- NULL;}

    dt.ret <- data.table(df1);
    colVal <- first(names(dt.ret))
    colFreq <- last(names(dt.ret))
  }


#      browser()

#  stopifnot(colVal=='input')
  stopifnot(colFreq=='Count')
  if (colVal!='input' & (not.na(thrRank) | not.na(thrNum))) stop('Not implemented yet!')


  if (not.na(thrRank)){
    dt.ret[, rankFreq:=rank(-Count)]
    dt.ret[rankFreq>thrRank & not.na(get(colVal)), `:=`(tmp_cat_Other=T,Count=sumI(Count) )]
    dt.ret[tmp_cat_Other==T, c(colVal):=thrLabel]
    dt.ret[,tmp_cat_Other:=NULL]
    dt.ret[, rankFreq:=NULL]
    dt.ret <- unique(dt.ret)
  }

  if (not.na(thrNum)){
#    browser()
    dt.ret[as.numeric(as.character(get(colFreq)))<thrNum, `:=`(tmp_cat_Other=T,Count=sumI(Count) )]
    dt.ret[tmp_cat_Other==T, c(colVal):=thrLabel]
    dt.ret <- unique(dt.ret)
    dt.ret[,tmp_cat_Other:=NULL]
  }

  if (keepP) dt.ret[,FreqP := percent(Count/sum(Count))];

  if (is.na(inpName) & !isTRUE(dim(input)[2]>0)) {
    names(dt.ret)[1] <- name1
  } else names(dt.ret)[1] <- inpName;

  return(dt.ret);
}

tabDT <- function(input, useNA='ifany', do.sort=T, ...){
  if (useNA==F) useNA <- 'no';
  name1 <- deparse(substitute(input));
  dt1 <- data.table(table(input, useNA = useNA, ...));
  setnames(dt1, 'N', 'Count')
  print(names(dt1));
  print(is.data.table(dt1))
  dt1 <- dt1[Count!=0,]
  sum1 <- sum(dt1$Count);
  dt1[, FreqP:=percent(Count/sum1)];
  setnames(dt1, 'input', name1);
#  print(order(-df$Count));
#  print(names(dt));
  if (do.sort) dt1 <- dt1[order(Count),];
#  print(names(dt1));
  return(dt1);
}

tab <- tabDF;

dt4mosaic <- function(inpDT, byX, byY){
  inpDT <- copy(inpDT)
  inpDT <- inpDT[,c(byX,byY),with=F]
  dt.stat <- as.data.table(tab(inpDT))
  # dt.stat[,rel:=Count/sum(Count),by=get(byX)]
  # dt.stat[,grpSize:=sum(Count), by=get(byX)]
  # dt.stat[,grpN:=sum(Count),    by=get(byX)]
  dt.stat[,rel := Count/sum(Count),by=c(byX)]
  dt.stat[,grpSize:=sum(Count), by=c(byX)]
  dt.stat[,grpN:=sum(Count),    by=c(byX)]
  dt.stat[,nP:=Count %+% '/' %+% grpN]
  dt.stat[,relP:=percent(rel)]
  dt.stat %<>% setorderv(c(byX,byY),na.last=T)

  if (!is.null(names(byX))) dt.stat %<>% setnames(byX,names(byX))
  if (!is.null(names(byY))) dt.stat %<>% setnames(byY,names(byY))

  return(dt.stat)
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


#contingency <- function(inpDT, colTest, colReal, valNeg=c(F,F), valPos=c(T,T), percDigits=1){
contingency <- function(inpDT, colTest, colReal, valsNeg=NULL, valsPos=NULL, percDigits=1){
  inpDT <- copy(inpDT)

  levels.test <- inpDT[[colTest]] %>% unique() %>% na.omit()
  levels.real <- inpDT[[colReal]] %>% unique() %>% na.omit()

  if (is.factor(levels.test)) levels.test %<>% as.character()
  if (is.factor(levels.real)) levels.real %<>% as.character()

  if (is.null(valsNeg)) valsNeg <- c(levels.test[1],levels.real[1])
  if (is.null(valsPos)) valsPos <- c(levels.test[2],levels.real[2])

  valNegTest <- valsNeg[[1]]
  valNegReal <- valsNeg[[2]]
  valPosTest <- valsPos[[1]]
  valPosReal <- valsPos[[2]]
  valOther <-'<OTHER>'

  if (valNegTest %in% cs('FALSE TRUE')) valNegTest %<>% as.logical()
  if (valNegReal %in% cs('FALSE TRUE')) valNegReal %<>% as.logical()
  if (valPosTest %in% cs('FALSE TRUE')) valPosTest %<>% as.logical()
  if (valPosReal %in% cs('FALSE TRUE')) valPosReal %<>% as.logical()

  message('Test column: ' %+% bold(colTest) %+% '; negative value: ' %+% bold(valNegTest)%+% '; positive value: ' %+% bold(valPosTest));
  message('Real column: ' %+% bold(colReal) %+% '; negative value: ' %+% bold(valNegReal)%+% '; positive value: ' %+% bold(valPosReal));

#  browser()


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

  # browser()

  tab1 <- data.table(tab(inpDT[,.(Test=.tmp.Test,Real=.tmp.Real,class=rez)]))
  tab1$Test %<>% factor(levels = c(valNegTest, valPosTest, valOther))
  tab1$Real %<>% factor(levels = c(valNegReal, valPosReal, valOther))
  tab2 <- dcast(as.data.table(tab1),Test~Real,value.var = 'Count', fill=0)

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

  tab0 <- inpDT[,c(key(inpDT), colTest, colReal, '.tmp.Test', '.tmp.Real', 'rez'), with=F]


  # inpDT %>% duView(c(colTest,colReal,'.tmp.Test','.tmp.Real','rez'))

  cat('\n')
  print(tab1)
  cat('\n')
  print(tab2)
  cat('\n')
  print(tab3a)
  cat('\n')
  mtx4fisher <- as.matrix(tab2[Test!=valOther,],rownames = 'Test')
# browser()
  print(fisher.test(mtx4fisher))
  invisible(list(tab0=tab0,tab1=tab1,tab2=tab2,tab3=tab3))
}


scaleInt <- function(x){
  if (length(x)==1) return(0)
  scaled <- scale(x)[,1]
  ceiling(abs(scaled))*sign(scaled) # <- can this be done simpler?
}

unqN <- function(x) length(unique(x))
empty <- function(x) length(x)==0;
topN <- function(x,thr=3){sort(unique(x), decreasing = T)[1:thr]}
btmN <- function(x,thr=3){sort(unique(x), decreasing = F)[1:thr]}



topNf <- function(x, thr=3) {
  dt1 <- data.table(x)
  dt1[, .N, by = x][order(-N, x)][1:thr, x]
}

topNf1 <- function(x, pos=1) {
  dt1 <- data.table(x)
  dt1[, .N, by = x][order(-N, x)][pos, x]
}

# `%bw%`   <- function(x,rng){return(x>rng[1]  & x<rng[2])}
# `%bbw%`  <- function(x,rng){return(x>=rng[1] & x<rng[2])}
# `%bww%`  <- function(x,rng){return(x>rng[1]  & x<=rng[2])}
# `%bbww%` <- function(x,rng){return(x>=rng[1] & x<=rng[2])}

# between() is forked from data.table::between
between <- function (x, lower, upper, incbounds = TRUE, NAbounds = TRUE,
          check = FALSE)
{
  if (is.logical(x)) stop("between has been passed an argument x of type logical")
  if (is.logical(lower)) lower = as.integer(lower)
  if (is.logical(upper)) upper = as.integer(upper)
  is.px = function(x) inherits(x, "POSIXct")
  is.i64 = function(x) inherits(x, "integer64")
  if (is.px(x) && (is.character(lower) || is.character(upper))) {
    tz = attr(x, "tzone", exact = TRUE)
    if (is.null(tz)) tz = ""
    if (is.character(lower)) lower = tryCatch(as.POSIXct(lower, tz = tz), error = function(e) stop("'between' function the 'x' argument is a POSIX class while '%s' was not, coercion to POSIX failed with: %s", "lower", e$message))
    if (is.character(upper)) upper = tryCatch(as.POSIXct(upper, tz = tz), error = function(e) stopf("'between' function the 'x' argument is a POSIX class while '%s' was not, coercion to POSIX failed with: %s", "upper", e$message))
    stopifnot(is.px(x), is.px(lower), is.px(upper))
  }

  if (is.px(x) && is.px(lower) && is.px(upper)) {
    tzs = sapply(list(x, lower, upper), function(x) {
      tt = attr(x, "tzone", exact = TRUE)
      if (is.null(tt))
        ""
      else tt
    })
    if (tzs[2L] != tzs[3L]) {
      stop("'between' lower= and upper= are both POSIXct but have different tzone attributes: %s. Please align their time zones.", data.table:::brackify(tzs[2:3], quote = TRUE))
    }
    if (tzs[1L] != tzs[2L]) {
      message("'between' arguments are all POSIXct but have mismatched tzone attributes: %s. The UTC times will be compared.", data.table:::brackify(tzs, quote = TRUE))
    }
  }
  if (is.i64(x)) {
    if (!requireNamespace("bit64", quietly = TRUE)) stop("trying to use integer64 class when 'bit64' package is not installed")
    if (!is.i64(lower) && is.numeric(lower)) lower = bit64::as.integer64(lower)
    if (!is.i64(upper) && is.numeric(upper)) upper = bit64::as.integer64(upper)
  }
  is.supported = function(x) is.numeric(x) || is.character(x) || is.px(x)
  if (is.supported(x) && is.supported(lower) && is.supported(upper) && length(incbounds)==1L) {
    .Call(data.table:::Cbetween, x, lower, upper, incbounds, NAbounds, check)
  }
  else {
    if (isTRUE(getOption("datatable.verbose")))
      cat("optimised between not available for this data type, fallback to slow R routine\n")
    if (isTRUE(NAbounds) && (anyNA(lower) || anyNA(upper)))
      stop("Not yet implemented NAbounds=TRUE for this non-numeric and non-character type")
    if (check && any(lower > upper, na.rm = TRUE))
      stop("Some lower>upper for this non-numeric and non-character type")
    if (incbounds %===% T) {
      x >= lower & x <= upper
    } else if (incbounds %===% F){
      x > lower & x < upper
    } else if (incbounds %===% c(T,F)){
      x >= lower & x < upper
    } else if (incbounds %===% c(F,T)){
      x > lower & x <= upper
    } else stop('Unexpected combination! ')
  }
}



# Checks if `x` falls in any of the intervals provided in `rng`
# vectorized for `x`
mybetween <- function(x, rng, incbounds=F, NAbounds=NA){
  if (length(rng)== 0) {
    return(rep(F,length(x)))
    # browser()
    # stop('provided range is empty!')
  }
  # if (length(rng) != 2 & length(rng) != 2*length(x)) {
  #   message('Length of rng: ' %+% length(rng))
  #   message('Length of x: ' %+% length(x))
  #   # browser()
  #   # stop('range must be length 2 or 2x .')
  # }
  n<-length(x);
#  browser()
  lower <- ifelse1(length(rng)>2, rng[1:n],         rng[1]) # WTF!!! ifelse doesn't work!
  upper <- ifelse1(length(rng)>2, rng[(n+1):(2*n)], rng[2]) # WTF!!! ifelse doesn't work!
  between(x,lower,upper,incbounds=incbounds, NAbounds = NAbounds)
}

`%bw%`   <- function(x,rng){mybetween(x,rng, incbounds=F);}
`%bbw%`  <- function(x,rng){mybetween(x,rng, incbounds=c(T,F));}
`%bww%`  <- function(x,rng){mybetween(x,rng, incbounds=c(F,T));}
`%bbww%` <- function(x,rng){mybetween(x,rng, incbounds=T);}





# usual duplicated() returns all but first, this one returns all:
#    duplicated(c(1,2,3,2,2,4)) == F F F T T F
# allduplicated(c(1,2,3,2,2,4)) == F T F T T F
allDuplicated <- function(x, values=F, ...) {
  ret <- duplicated(x,...) | duplicated(x, fromLast=T,...)
  if (values==T) ret <- x[ret];
  return(ret);
}



# prop.test.str('46/395,237/2100')
prop.test.str <- function(inpStr,...){
#  browser()
  inpStr  <- cs(inpStr)
  inpStr2 <- cs(inpStr, sep = '/')
  if (length(inpStr)<2) stop("Couldn't split the string. ")
  if (length(inpStr2)==2*length(inpStr)){      # with '/'
    inpNum <- as.numeric(inpStr2)
    vec.succ <- inpNum[c(T,F)]
    vec.tot  <- inpNum[c(F,T)]
    vec.fail <- vec.tot-vec.succ
  } else if (length(inpStr2)==length(inpStr)){ # no '/'
    inpNum <- as.numeric(inpStr)
    vec.succ <- inpNum[c(T,F)]
    vec.fail <- inpNum[c(F,T)]
  } else stop("Couldn't split the string. ")

  inpMtx <- matrix(c(vec.succ,vec.fail), ncol=2)

  print(inpMtx)
  message('Values: ',paste(percent(inpMtx[,1]/(inpMtx[,1]+inpMtx[,2])),collapse = ', ' ))
  prop.test(inpMtx, ...)
}


getmode <- function(x, all=F, na.rm=T, noNULL=T) {
  if (na.rm==T) x <- na.omit(x)
  freq_table <- table(x)
  max_freq <- maxI(freq_table)
  mode_values <- names(freq_table[freq_table==max_freq])
  if (all==F & length(mode_values)>1){
    retval <- sort(mode_values)[floor(length(mode_values)/2)+1]
  } else retval <- mode_values
  if (noNULL==T & is.null(retval)) retval <- NA
  return(retval)
}


phi_coefficient <- function(tab1) {
  if (!all(dim(tab1) == c(2, 2))) return(NA)
  n11 <- as.numeric(tab1[2, 2])  # TRUE, TRUE
  n10 <- as.numeric(tab1[2, 1])  # TRUE, FALSE
  n01 <- as.numeric(tab1[1, 2])  # FALSE, TRUE
  n00 <- as.numeric(tab1[1, 1])  # FALSE, FALSE

  # Compute numerator and denominator safely
  numerator <- (n11 * n00) - (n10 * n01)
  denominator <- sqrt((n11 + n10) * (n01 + n00) * (n11 + n01) * (n10 + n00))

  # Avoid division by zero or undefined cases
  if (is.nan(denominator) || denominator == 0) return(NA_real_)

  return(numerator / denominator)
}



corr_pair <- function(pair, dtIn){
  var1 <- pair[1]
  var2 <- pair[2]

  # Build contingency table
  tab1 <- table(dtIn[[var1]], dtIn[[var2]])

  # Ensure 2x2 table
  if (!all(dim(tab1) == c(2, 2)) ) {
    # Pad missing TRUE/FALSE combinations if necessary
    all_levels <- list(c(FALSE, TRUE), c(FALSE, TRUE))
    tab1 <- table(factor(dtIn[[var1]], levels = all_levels[[1]]),
                  factor(dtIn[[var2]], levels = all_levels[[2]]))
  }

  stopifnot(dimnames(tab1) %===% structure(list(c("FALSE", "TRUE"), c("FALSE", "TRUE")), names = c("","")))

#  browser()

  n00 <- tab1[1,1]
  n01 <- tab1[1,2]
  n10 <- tab1[2,1]
  n11 <- tab1[2,2]

  whenTrue  <- n11/(n11+n10)
  whenFalse <- n01/(n01+n00)

  # Fisher's exact test (one-sided)
  pval.excl <- fisher.test(tab1, alternative = "less")$p.value    # for mutual exclusivity
  pval.cooc <- fisher.test(tab1, alternative = "greater")$p.value # for co-occurrence

  # Phi coefficient (binary correlation)
  phi <- phi_coefficient(tab1)
  if (is.null(phi)) phi <- NA

  # Return result row
  data.table(
    var1 = var1,
    var2 = var2,
    whenTrue=whenTrue,
    whenFalse=whenFalse,
    pval.excl = pval.excl,
    pval.cooc = pval.cooc,
    phi = phi
  )

}
# Usage example1:
# corr_pair(c('mut_TP53','mut_APC'), dt.pat)

# Usage example2:
# vec.genes2test <- cs('APC TP53 KRAS PIK3CA BRAF')
# gene_pairs <- combn('mut_' %+% vec.genes2test, 2, simplify=FALSE)
# dt.corr <- rbindlist(lapply(gene_pairs, FUN=corr_pair, dtIn=dt.pat))




# cat.N <- c(Gender='Gender',
#            Cancer.Stage='Overall pathologic stage',
#            hadNeoB='Neoadjuvant therapy given',
#            Resectability='Resectable status'
#            # stageT='Pathologic T stage',
#            #Location='Cancer location'
# )

build_stat_table_N <- function(dtIn, categories, do.sort = F, thrRank=15, ...){
  dt.N <- NULL
  #  browser()
  for (this.cat in names(categories)){
    this.label <- categories[[this.cat]]
    this.vals  <- dtIn[[this.cat]]
    if (is.null(this.vals)) {message(' Not found: ', this.cat, ' - ', this.label); next;}
    this.stat  <- tab(this.vals, thrRank = thrRank, do.sort = do.sort, ...)
    this.title <- data.table(Category=this.label)
    this.tab   <- rbind(this.title, data.table(Value=this.stat$this.vals, N=this.stat$Count, `%`=this.stat$FreqP), fill=T)
    dt.N %<>% rbind(this.tab, fill=T)
  }

  dt.N[is.na(Value) & is.na(Category),Value:='N/A']
  invisible(dt.N)
}

build_stat_table_med <- function(dtIn,categories){
  dt.med <- NULL
  for (this.cat in names(categories)){
    cat('\n',this.cat,'\t')
    this.label <- categories[[this.cat]]
    this.vals  <- dtIn[[this.cat]]
    if (is.null(this.vals)) {message(' Not found: ', this.cat, ' - ', this.label); next;}
    this.vals %<>% as.numeric()
    if (length(this.vals)==0) warning('')

    this.med <- median(this.vals, na.rm=T)
    this.sd  <- sd(    this.vals, na.rm=T)
    this.rng <- range( this.vals, na.rm=T)

    #    this.title <- data.table(Category=this.label)
    #    this.tab   <- rbind(this.title, data.table(Value=this.stat$this.vals, N=this.stat$Freq, `%`=this.stat$FreqP), fill=T)
    dt.med %<>% rbind(data.table(Category=this.label, Median=round(this.med, 2), SD=round(this.sd,2), range=paste(round(this.rng,2),collapse = ' .. ')), fill=T)
  }

  invisible(dt.med)
}

build_stat_table_both <- function(dtIn, categories.N, categories.med, ...){
  dt.demo.N    <- build_stat_table_N(dtIn, categories.N, ...)
  dt.demo.cont <- build_stat_table_med(dtIn, categories.med)
  dt.demo.cont1 <- dt.demo.cont[,.(Category, Value='(median±SD, range)', N=NA_integer_, `%`=(Median %+% '±' %+% SD %+% ', ' %+% range))]
  dt.demo.both <- rbindV(dt.demo.cont1, dt.demo.N)
  return(dt.demo.both)
}

stat_cbind <- function(inpList){
  dt.wide <- NULL
  for (i in names(inpList)){
    cat('\n\n', i)
    #browser()
    this_dt <- inpList[[i]]
    this_dt[, Category1 := zoo::na.locf(Category)]
    this_dt %<>% dt_del_columns('Category', do_copy = T)

    this_dt %<>% dt_setnames(cs('N %'), cs('N %') %+% '_' %+% i, verbose = F)

    if (is.null(dt.wide)) {
      this_dt[, orderCategory := .GRP, by=Category1]
      this_dt[, orderValue := .GRP, by=.(Category1,Value)]
      dt.wide <- this_dt

    } else {dt.wide %<>% merge(this_dt, by=cs('Category1,Value'), all=T)}
  } # e. for (i)

  dt.wide[is.na(Value), Category := Category1]
  dt.wide %<>%
    setorderv(cs('orderCategory,orderValue')) %>%
    setcolorderV(cs('Category')) %>%
    dt_del_columns(cs('orderCategory,orderValue,Category1'))
  invisible(dt.wide)
}
