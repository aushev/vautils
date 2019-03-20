
# extends phenoData of given eSet by adding a data.table
attachpData <- function(es, dat, key.dat=NULL, key.es=NULL, reqUniqESkeys=T){

  if (!is.data.table(dat)) dat <- data.table(dat);
  dat.use <- dat;
  if (is.null(key.dat) & !is.null(key.es)) warning('Key of dat is not defined => Key of es will be ignored!');

  if (!is.null(key.dat)){
    keyvals <- dat[[key.dat]];
    if (anyDuplicated(keyvals)) {
      print(keyvals[duplicated(keyvals)]);
      stop('Duplicated key values found in dat!');
    }
    setkeyv(dat, key.dat);

    # if key.es is undefined, we use sampleNames(es), otherwise defined key.es col
    es.keys <- ifelse1(is.null(key.es), sampleNames(es), pData(es)[[key.es]])
    if (anyDuplicated(es.keys) & reqUniqESkeys) {
      print(es.keys[duplicated(es.keys)])
      warning('Duplicated es.keys found! Some of the dat records will be duplicated.');
    }
    dat.use <- dat[es.keys,]; # only selected records from dat
    dat.use <- dat.use[,-c(key.dat),with=F]; # #dat.use <- dat.use[,(names(dat.use) %-% key.dat),with=F]; #

  }

  names.dup <- colnames(dat.use) %&% varLabels(es); # colnames() instead of ... in case of matrix
  if (length(names.dup)>0) {
    pData(es) <- pData(es)[,(varLabels(es) %-% names.dup)]
    warning('Replaced columns in phenoData: ', paste(names.dup, collapse = ', '))
  }

  pData(es) <-  cbind(pData(es), dat.use);
  invisible(es);
}

# extends featureData of given eSet by adding a data.table
attachfData <- function(es, dat, key.dat=NULL, key.es=NULL, reqUniqESkeys=T){

  if (!is.data.table(dat)) dat <- data.table(dat);
  dat.use <- dat;
  if (is.null(key.dat) & !is.null(key.es)) warning('Key of dat is not defined => Key of es will be ignored!');

  if (!is.null(key.dat)){
    keyvals <- dat[[key.dat]];
    if (anyDuplicated(keyvals)) {
      print(keyvals[duplicated(keyvals)]);
      stop('Duplicated key values found in dat!');
    }
    setkeyv(dat, key.dat);

    # if key.es is undefined, we use featureNames(es), otherwise defined key.es col
    es.keys <- ifelse1(is.null(key.es), featureNames(es), fData(es)[[key.es]])
    if (anyDuplicated(es.keys) & reqUniqESkeys) {
      print(es.keys[duplicated(es.keys)])
      warning('Duplicated es.keys found! Some of the dat records will be duplicated.');
    }
    dat.use <- dat[es.keys,]; # only selected records from dat
    dat.use <- dat.use[,-c(key.dat),with=F]; # #dat.use <- dat.use[,(names(dat.use) %-% key.dat),with=F]; #

  }

  names.dup <- colnames(dat.use) %&% fvarLabels(es); # colnames() instead of ... in case of matrix
  if (length(names.dup)>0) {
    fData(es) <- fData(es)[,(fvarLabels(es) %-% names.dup)]
    warning('Replaced columns in phenoData: ', paste(names.dup, collapse = ', '))
  }

  fData(es) <-  cbind(fData(es), dat.use);
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


debugES <- function(es, prefix='tmp'){
  #if ()
  tmpX <- exprs(es)
  tmpF <- fData(es)
  tmpP <- pData(es)

  assign(paste0(prefix,'F'),tmpF,pos = 1L)
  assign(paste0(prefix,'P'),tmpP,pos = 1L)
  assign(paste0(prefix,'X'),tmpX,pos = 1L)

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

