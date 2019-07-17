
# extends phenoData of given eSet by adding a data.table
attachpData <- function(es, dat, key.dat=NULL, key.es=NULL, reqUniqESkeys=T){
  if(!identical(sampleNames(es), row.names(pData(es)))) stop('Input eSet is broken! sampleNames(es) must be equal to row.names(pData(es))!');

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
    warning('Replaced columns in pData: ', paste(names.dup, collapse = ', '))
  }

  pData(es) <-  cbind(pData(es), dat.use);
  invisible(es);
}

# attachfData(): extends featureData of given eSet by adding a data.table
# key.dat='' - use row.names(dat) as key
attachfData <- function(es, dat, key.dat=NULL, key.es=NULL, reqUniqESkeys=T){
  if(!identical(featureNames(es), row.names(fData(es)))) stop('Input eSet is broken! featureNames(es) must be equal to row.names(fData(es))!');
  if (ncol(dat)==0 | nrow(dat)==0) {
    warning('Data is empty. Returning unchanged eSet.')
    invisible(es);
  }


  if (!is.data.table(dat)) {
    if ('rn' %in% names(dat)) stop('Name "rn" is reserved. Please rename that column.')
    dat <- data.table(dat, keep.rownames = T);
    if (key.dat=='') key.dat <- 'rn';
  }

  dat.use <- dat;
#  if (is.null(key.dat) & !is.null(key.es)) warning('Key of dat is not defined => Key of eSet will be ignored!');

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
    warning('Replaced columns in fData: ', paste(names.dup, collapse = ', '))
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

eSetFromTable <- function(tabInput,samples=NULL,featureNamesCol=NULL,featuresCols=T,orientation=T){
  # by default, we expect features as rownames, samples as column names
  # samples: sample names as character vector, we'll check those within column names
  #          If NULL, all the columns minus featureNamesCol and featuresCols
  # featureNamesCol: column containing feature ID. If NULL, row.names(tabInput) will be used
  # featuresCols: columns with feature data - will go to fData(es). If TRUE, all remaining columns

  if (orientation==F){
    stop('Not implemented yet');
  }

  if(is.data.table(tabInput)) tabInput <- as.data.frame(tabInput);

  if (is.null(samples)){
    samples <- names(tabInput) %-% unique(c(featureNamesCol, featuresCols))
  }

  if (is.null(samples)){
    stop('Samples not defined!')
  }


  samplesNotFound <- (samples %-% names(tabInput))
  if (length(samplesNotFound)>0) {stop('Samples not found: ', samplesNotFound)}

  expr <- as.matrix(tabInput[,(samples)]);
  tabRest <- tabInput[,(names(tabInput) %-% samples)]

  if (!is.null(featureNamesCol)){
    if (!featureNamesCol %in% names(tabInput)){
      stop('Column with gene names not found! ',featureNamesCol);
    }
    row.names(expr) <- as.character(tabInput[[featureNamesCol]]);
    tabRest.names <- (names(tabRest) %-% featureNamesCol)
    if (is.null(tabRest.names)) {tabRest <- NULL;} else tabRest <- tabRest[,(tabRest.names)];
  }

  es <- ExpressionSet(assayData = expr);
  fData(es)$oriID <- featureNames(es);

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


eSetFromLong <- function(tabInput,featureNamesCol='geneID',sampleNamesCol='ffn',valueCol='counts'){
  dtInput <- as.data.table(tabInput)

  frm <- as.formula(paste(featureNamesCol,'~',sampleNamesCol))

  dtInput <- dcast(dtInput, frm, value.var = valueCol)
  setDF(dtInput, rownames = dtInput[[featureNamesCol]])
  dtInput[[featureNamesCol]] <- NULL
  #dat.counts <- as.matrix(dtInput)
  es <- ExpressionSet(as.matrix(dtInput))
  invisible(es)
}



summaryG <- function(es, not0.thr=0){
  n.samples <- dim(es)[2]
  n.genes   <- dim(es)[1]
  tmpX <- exprs(es)

  dt.SummaryG         <- as.data.frame(fData(es))
  dt.SummaryG$oriID   <- seq_len(n.genes)
  dt.SummaryG$oriName <- featureNames(es)
  dt.SummaryG %<>% as.data.table

  dt.SummaryG$not0   <- apply(tmpX,1,function(X){sum(X>not0.thr)})
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


summaryS <- function(es, not0.thr=0){
  n.samples <- dim(es)[2]
  n.genes   <- dim(es)[1]
  tmpX <- exprs(es)

  dt.SummaryS        <- as.data.frame(pData(es))
  dt.SummaryS$oriID  <- seq_len(n.samples)
  dt.SummaryS$oriName  <- sampleNames(es)
  dt.SummaryS %<>% as.data.table; # this should be after assigning oriID!

  dt.SummaryS$not0   <- apply(tmpX,2,function(X){sum(X>not0.thr)})
  dt.SummaryS[,not0f:=not0/n.genes]
  dt.SummaryS$avgSig <- apply(tmpX,2,mean)
  dt.SummaryS$medSig <- apply(tmpX,2,median)
  dt.SummaryS$rank1 <- frank(dt.SummaryS[,.(-not0,-avgSig)], ties.method = 'first')
  dt.SummaryS$rank2 <- frank(dt.SummaryS[,.(-medSig,-not0)], ties.method = 'first')

  pData(es) <- dt.SummaryS
  sampleNames(es) <- dt.SummaryS$oriName

  return(es);
}

eSet.From.fCounts <- function(fn.fCounts,maskRemove='_Aligned.sortedByCoord.out.bam'){
  dt.fcounts <- flexread(fn.fCounts)
  names(dt.fcounts) %<>% gsub(maskRemove,'',.)
  es <- eSetFromTable(dt.fcounts, featureNamesCol='Geneid', featuresCols=cs('Chr Start End Strand Length'))
  fData(es)$Chr <- sapply(strsplit(fData(es)$Chr,';'),function(X){paste0(unique(X),collapse = ';')})
  fData(es)$Strand <- sapply(strsplit(fData(es)$Strand,';'),function(X){paste0(unique(X),collapse = ';')})
  invisible(es)
}



eSet.From.starquant <- function(fnInput, mask='*_ReadsPerGene.out.tab', stranded=0){
  # fnInput - directory or list of files
  #

  dt.all <- mergefiletabs(fnInput, fn.mask = mask, full.names = F, recursive = F, colnames = cs('geneID countsU counts1 counts2'), mask.remove = mask)

  if (nrow(dt.all)==0L) stop('Read failed! Check that the path exists.');

  cols.meta <- cs('N_unmapped N_multimapping N_noFeature N_ambiguous')
  dt.counts <- dt.all[geneID %!in% cols.meta,]
  dat.meta <- dt.all[geneID %in% cols.meta,]
  rm(dt.all)

  dat.meta <- dat.meta[,
                       .(
                         unmapped    =.SD[geneID=='N_unmapped',    countsU],
                         multimapping=.SD[geneID=='N_multimapping',countsU],
                         noFeature0  =.SD[geneID=='N_noFeature',   countsU],
                         noFeature1  =.SD[geneID=='N_noFeature',   counts1],
                         noFeature2  =.SD[geneID=='N_noFeature',   counts2],
                         ambiguous0  =.SD[geneID=='N_ambiguous',  countsU],
                         ambiguous1  =.SD[geneID=='N_ambiguous',  counts1],
                         ambiguous2  =.SD[geneID=='N_ambiguous',  counts2]
                       ),
                       by=ffn]
  setDF(dat.meta, rownames = dat.meta$ffn)
  dat.meta$ffn <- NULL

  if (stranded==0) dt.counts.sel <- dt.counts[,.(geneID,counts=countsU,ffn)]
  else if (stranded!=0) stop('Not implemented yet!')

  dat.counts <- dcast(dt.counts.sel, geneID ~ ffn, value.var = 'counts')
  setDF(dat.counts, rownames = dat.counts$geneID)
  dat.counts$geneID <- NULL
  dat.counts <- as.matrix(dat.counts)

  es <- ExpressionSet(dat.counts)
  #sampleNames(es)
  pData(es) <- cbind(pData(es), dat.meta)


  invisible(es)
}



# compare_esets(): merges melted exprs() for both eSets
compare_esets <- function(es1,es2, add_feat_cols=NULL){
  if (is.character(es1)) {
    es1.name <- loadv(es1)
    es1 <- get(es1.name)
    if (es1.name!='es1') rm(list=es1.name)
  }

  if (is.character(es2)) {
    es2.name <- loadv(es2)
    es2 <- get(es2.name)
    if (es2.name!='es2') rm(list=es2.name)
  }

  dtX1 <- es1 %>% exprs %>% as.data.table(keep.rownames = T) %>% setnames('rn','featureID')
  dtX2 <- es2 %>% exprs %>% as.data.table(keep.rownames = T) %>% setnames('rn','featureID')

  if (!is.null(add_feat_cols)){
    dtX1 <- cbind(dtX1, fData(es1)[,add_feat_cols,drop=FALSE])
    dtX2 <- cbind(dtX2, fData(es1)[,add_feat_cols,drop=FALSE])
  }

  dtX1 <- melt(dtX1, id.vars=c('featureID', add_feat_cols))
  dtX2 <- melt(dtX2, id.vars=c('featureID', add_feat_cols))

  setnames(dtX1, cs('value variable'), cs('value1 sampleID'))
  setnames(dtX2, cs('value variable'), cs('value2 sampleID'))

  dtX <- merge(dtX1,dtX2,by=c('featureID', add_feat_cols, 'sampleID'), all=T)
  rm(dtX1, dtX2)

  invisible(dtX)

} # e. compare_esets()


# returns expression valued transformed by a chosen method
# method=c('','log2','vst'), form=c('matrix','df','dt')
exprsV <- function(es, method=NULL, trans=FALSE, form='matrix'){
  values <- exprs(es);

  values <- switch(method,
         log2= log2(values+1),
         vst = vst(values),
               values )

  if (trans==TRUE) values <- t(values)

  values <- switch(form,
         matrix= as.matrix(values),
         df    = as.data.frame(values),
         dt    = as.data.table(values),
                 values )

  return(values)
  }
