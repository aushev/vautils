
# extends phenoData of given eSet by adding a data.table
attachpData <- function(es, dat, by.dat=NULL, by.es=NULL, reqUniqESkeys=T){

  # if no fields indicated, then add complete [dat] to pData
  if (is.null(by.es) & is.null(by.dat)) {
     pData(es) <-  cbind(pData(es), dat);
     return(invisible(es));
  } # return & exit

  if (!is.data.table(dat)) dat <- data.table(dat);

  #
  if (!is.null(by.dat)){
    keyvals <- dat[[by.dat]];
    if (anyDuplicated(keyvals)) {
      print(keyvals[duplicated(keyvals)]);
      stop('Duplicated key values found in dat!');
    }

    dat <- data.table(dat);
    setkeyv(dat, by.dat);
  }

  # if key.col is defined only for dat, we use sampleNames(es), otherwise defined by.es col
  es.keys <- ifelse1(is.null(by.es), sampleNames(es), pData(es)[[by.es]])
  if (anyDuplicated(es.keys) & reqUniqESkeys) {
    print(es.keys[duplicated(es.keys)])
    warning('Duplicated es.keys found!');
  }

  dat.use <- dat[es.keys,];
  dat.use <- dat.use[,-c(by.dat),with=F]; # #dat.use <- dat.use[,(names(dat.use) %-% by.dat),with=F]; #

  names.dup <- colnames(dat.use) %&% varLabels(es); # colnames() instead of ... in case of matrix
  if (length(names.dup)>0) {
    pData(es) <- pData(es)[,(varLabels(es) %-% names.dup)]
    warning('Replaced columns in phenoData: ', paste(names.dup, collapse = ', '))
  }

  pData(es) <-  cbind(pData(es), dat.use);
  invisible(es);
}













attachpData0 <- function(es, dat, keycol, es.keycol=NULL){
  #if (!is.data.table(dat)) dat <- data.table(dat);

  if (missing(keycol)){
    keyvals0 <- row.names(dat);
  } else {
    keyvals0 <- dat[[keycol]]
    setkeyv(dat,keycol);
  }

  es.keys <- ifelse1(is.null(es.keycol), sampleNames(es), pData(es)[[es.keycol]])
  if (anyDuplicated(es.keys)) {
    print(es.keys[duplicated(es.keys)])
    stop('Duplicated es.keys found!');
  }

  names.nof <- es.keys %-% keyvals0;
  if (length(names.nof)>0)
    warning('phenoData IDs not found in key: ', paste(names.nof, collapse = ', '))

  dat.use <- dat[es.keys,];

  if (missing(keycol)){
    keyvals <- row.names(dat.use);
  } else {
    keyvals <- row.names(dat.use) <-  dat.use[[keycol]];
    dat.use <- dat.use[,-c(keycol),with=F];
  }

  if (anyDuplicated(keyvals)) stop('Duplicated key values found!');

  names.dup <- colnames(dat.use) %&% varLabels(es); # colnames() instead of colnames() in case of matrix
  if (length(names.dup)>0) stop('Columns already in phenoData: ', paste(names.dup, collapse = ', '))

  pData(es) <-  cbind(pData(es), dat.use);
  invisible(es);
}


exprext <- function(es.input, cols2add=NULL){
  mtx.expr <- t(exprs(es.input)); # 608
  dat2add <- ifelse1(is.null(cols2add), pData(es.input), pData(es.input)[cols2add])
  return(data.table(cbind(dat2add,mtx.expr)))
}


es.rename.f <- function(es, kcol){ # rename features
  featureNames(es) <- fData(es)[[kcol]]
  invisible(es)
}

es.rename.s <- function(es, kcol){ # rename samples
  sampleNames(es)  <- pData(es)[[kcol]]
  invisible(es)
}


debugES <- function(es){
  tmpX <- exprs(es)
  tmpF <- fData(es)
  tmpP <- pData(es)

  assign('tmpF',tmpF,pos = 1L)
  assign('tmpP',tmpP,pos = 1L)
  assign('tmpX',tmpX,pos = 1L)

  View(tmpX)
  View(tmpF)
  View(tmpP)

}

eSetFromTable <- function(tabInput,samples,featureNamesCol=NULL,featuresCols=character(0),orientation=T){
  # by default, we expect features as rownames, samples as column names
  # samples: sample names as character vector, we'll check those within column names
  # featureNamesCol: column containing feature ID. If NULL, row.names(tabInput) will be used
  # featuresCols: columns with feature data - will go to fData(es). If TRUE, all remaining columns

  if (orientation==F){
    stop('Not implemented yet');
  }

  if(is.data.table(tabInput)) tabInput <- as.data.frame(tabInput);

  samplesNotFound <- (samples %-% names(tabInput))
  if (length(samplesNotFound)>0) {stop('Samples not found: ', samplesNotFound)}

  expr <- as.matrix(tabInput[,(samples)]);
  tabRest <- tabInput[,(names(tabInput) %-% samples)]

  if (!is.null(featureNamesCol)){
    if (!featureNamesCol %in% names(tabInput)){
      stop('Column with gene names not found! ',featureNamesCol);
    }
    row.names(expr) <- as.character(tabInput[[featureNamesCol]]);
    tabRest <- tabRest[,(names(tabRest) %-% featureNamesCol)]
  }

  es <- ExpressionSet(assayData = expr);

  if (length(featuresCols)>0){
    if (isTRUE(featuresCols)){ # include all remaining columns
      fData(es) <- cbind(fData(es),tabRest)
    } else {
      featurecolsNotFound <- (featuresCols %-% names(tabRest))
      if (length(featurecolsNotFound)>0) {stop('featurecols not found: ', featurecolsNotFound)}
      fData(es) <- cbind(fData(es),tabRest[featuresCols])
    }
  } # e. if featuresCols not empty

  return(es);
}

summaryG <- function(es){
  n.samples <- dim(es)[2]
  n.genes   <- dim(es)[1]
  tmpX <- exprs(es)

  dt.SummaryG         <- as.data.frame(fData(es))
  dt.SummaryG$oriID   <- seq_len(n.genes)
  dt.SummaryG$oriName <- featureNames(es)
  dt.SummaryG %<>% as.data.table

  dt.SummaryG$not0   <- apply(tmpX,1,function(X){sum(X!=0)})
  dt.SummaryG[,not0f:=not0/n.samples]
  dt.SummaryG$avgSig <- apply(tmpX,1,mean)
  dt.SummaryG$medSig <- apply(tmpX,1,median)
  dt.SummaryG$rank1 <- frank(dt.SummaryG[,.(-not0,-avgSig)], ties.method = 'first')
  dt.SummaryG$rank2 <- frank(dt.SummaryG[,.(-avgSig,-not0)], ties.method = 'first')

  dt.SummaryG$IQR <- apply(tmpX,1,IQR)

  fData(es) <- dt.SummaryG
  featureNames(es) <- dt.SummaryG$oriName

  return(es);
}


summaryS <- function(es){
  n.samples <- dim(es)[2]
  n.genes   <- dim(es)[1]
  tmpX <- exprs(es)

  dt.SummaryS        <- as.data.frame(pData(es))
  dt.SummaryS$oriID  <- seq_len(n.samples)
  dt.SummaryS$oriName  <- sampleNames(es)
  dt.SummaryS %<>% as.data.table; # this should be after assigning oriID!

  dt.SummaryS$not0   <- apply(tmpX,2,function(X){sum(X!=0)})
  dt.SummaryS[,not0f:=not0/n.genes]
  dt.SummaryS$avgSig <- apply(tmpX,2,mean)
  dt.SummaryS$medSig <- apply(tmpX,2,median)
  dt.SummaryS$rank1 <- frank(dt.SummaryS[,.(-not0,-avgSig)], ties.method = 'first')
  dt.SummaryS$rank2 <- frank(dt.SummaryS[,.(-medSig,-not0)], ties.method = 'first')

  pData(es) <- dt.SummaryS
  sampleNames(es) <- dt.SummaryS$oriName

  return(es);
}

