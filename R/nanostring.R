
nses.ini1 <- function(es){

  fData(es)$CodeClass[fData(es)$CodeClass=='Endogenous1'] <- 'Endogenous'

  # should be done AFTER applying PCF!
  sum.pos <- colSums(exprs(es[fData(es)$CodeClass == 'Positive',]))
  sum.neg <- colSums(exprs(es[fData(es)$CodeClass == 'Negative',]))
  sum.hkg <- colSums(exprs(es[fData(es)$CodeClass == 'Housekeeping',]))
  sum.gen <- colSums(exprs(es[fData(es)$CodeClass %in% cs('Housekeeping Endogenous Endogenous1'),]))
  sum.end <- colSums(exprs(es[fData(es)$CodeClass %in% cs('Endogenous Endogenous1'),]))
  dat.add <- cbind(sum.pos,sum.neg,sum.hkg,sum.gen,sum.end)
  dat.add %<>% as.data.frame
  es %<>% attachpData(dat.add)
}


nses.spike <- function(es){
    # very dirty
  if (!is.null(fData(es)$SpikeInInput)){
    warning('SpikeInInput already present. Returning unchanged.');
    invisible(es);
  } else {
  this.fData <- fData(es)
  this.fData$SpikeInInput <- 0;
  this.fData[this.fData$GeneName=='POS_A',]$SpikeInInput <- 128
  this.fData[this.fData$GeneName=='POS_B',]$SpikeInInput <- 32
  this.fData[this.fData$GeneName=='POS_C',]$SpikeInInput <- 8
  this.fData[this.fData$GeneName=='POS_D',]$SpikeInInput <- 2
  this.fData[this.fData$GeneName=='POS_E',]$SpikeInInput <- 0.5
  this.fData[this.fData$GeneName=='POS_F',]$SpikeInInput <- 0.125
  fData(es) <- this.fData;
 } #e. if no spike info
  invisible(es);
}

extract.PCF <- function(es, label.GeneName='GeneName', label.PCF='PCF'){
  # extract & remove PCF from GeneName column of fData, save it to new 'PCF' column of fData
  # actual expression values will not be changed yet; use apply.PCF()
  # debug: es=es.raw; label.GeneName='GeneName'; label.PCF='PCF'
  # end debug: rm(es,label.GeneName,label.PCF,geneNames,have.PCF,PCFs)
  geneNames <- fData(es)[[label.GeneName]]; # ex: 'hsa-miR-302d-3p|0.006', or 'LIG_POS_B'
  have.PCF <- grepl('\\|\\d',geneNames)
  if (sum(have.PCF)==0) {
    warning('PCF not found in gene names. Returning unchanged.');
    invisible(es);
  }
  geneNames[!have.PCF] <- paste0(geneNames[!have.PCF], '|0');
  PCFs <- as.numeric(            gsub(   '.*\\|(\\d+)','\\1',geneNames))
  fData(es)[[label.GeneName]] <- gsub('^(.+)\\|(.*)'  ,'\\1',geneNames)
  fData(es)[[label.PCF]] <- PCFs
  invisible(es);
}


apply.PCF <- function(es, extract=TRUE){
  PCF.applied <- pData(es)$PCF.applied
  if (sum(PCF.applied,na.rm = T)>0) {stop('PCF seems to be already applied!');}
  PCFs <- fData(es)$PCF;
  if (is.null(PCFs) && extract!=TRUE) {stop('Need to extract PCF first!');}
  if (is.null(PCFs)) {es %<>% extract.PCF; PCFs <- fData(es)$PCF;}
  posA <- exprs(es)[fData(es)$GeneName=='POS_A']
  backgrounds <- sapply(posA, FUN = '*',PCFs)
  X <- exprs(es) - backgrounds
  X[X<0] <- 0
  exprs(es) <- X
  pData(es)$PCF.applied <- TRUE;
  invisible(es)
}
