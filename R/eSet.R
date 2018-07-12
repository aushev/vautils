
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
