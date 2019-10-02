
nses.load.RCCs <- function(fn.RCCs){
# load RCC files using NanoStringNorm::read.markup.RCC()
# CAUTION! Stupid read.markup.RCC() can't treat RCCs with different PCFs!
  if (length(fn.RCCs)==1 && dir.exists(fn.RCCs)){
    cat(' Reading directory:',fn.RCCs)
    fn.RCCs <- list.files(fn.RCCs, pattern = '*.RCC|*.rcc')
  }

  fn.found <- file.exists(fn.RCCs)

  if (sum(!fn.found)>0){
    warning(sum(!fn.found),' files not found! ')
  }

  fn.RCCs <- fn.RCCs[fn.found]

  es.all <- NULL

  for (this.dir in unique(dirname(fn.RCCs))){
    # stupid read.markup.RCC() can't just take list of files, it will always scan the whole directory,
    # so we have to manually define files to exclude
    this.NSN <- NanoStringNorm::read.markup.RCC(rcc.path = this.dir, exclude = list.files(this.dir) %-% basename(fn.RCCs))

    this.es <- eSetFromTable(this.NSN$x, featureNamesCol='Name',featuresCols = cs('CodeClass Accession'))
    this.es %<>% attachpData(t(this.NSN$header))

    this.es %<>% extract.PCF()
    this.es %<>% apply.PCF()

    fData(this.es)$PCF <- NULL
    fData(this.es)$oriID <- NULL

    es.all <- ifelse1(is.null(es.all), this.es, Biobase::combine(es.all, this.es))

  } # e. for (this.dir)
invisible(es.all)
} # e. nses.load.RCCs()



nses.ini1 <- function(es){

  fData(es)$CodeClass[fData(es)$CodeClass=='Endogenous1'] <- 'Endogenous'
  if (! 'GeneName' %in% fvarLabels(es)) fData(es)$GeneName <- featureNames(es)

  # should be done AFTER applying PCF!
  sum.pos <- colSums(exprs(es[fData(es)$CodeClass == 'Positive',]))
  sum.neg <- colSums(exprs(es[fData(es)$CodeClass == 'Negative',]))
  sum.lig <- colSums(exprs(es[fData(es)$CodeClass == 'Ligation',]))
  sum.ligP<- colSums(exprs(es[grepl('LIG_POS',featureNames(es)),])) # was fData(es)$GeneName instead of featureNames()
  sum.ligN<- colSums(exprs(es[grepl('LIG_NEG',featureNames(es)),])) # was fData(es)$GeneName instead of featureNames()
  sum.hkg <- colSums(exprs(es[fData(es)$CodeClass == 'Housekeeping',]))
  sum.gen <- colSums(exprs(es[fData(es)$CodeClass %in% cs('Housekeeping Endogenous Endogenous1'),]))
  sum.end <- colSums(exprs(es[fData(es)$CodeClass %in% cs('Endogenous Endogenous1'),]))
  dat.add <- cbind(sum.pos,sum.neg,sum.lig,sum.ligP,sum.ligN,sum.hkg,sum.gen,sum.end)
  dat.add %<>% as.data.frame
  es %<>% attachpData(dat.add)
}


nses.conc <- function(es, label.conc='SpikeInInput', re.conc='.*\\((.*)\\)'){
  if (label.conc %in% fvarLabels(es)){
    warning(label.conc, ' already present. Returning unchanged.');
    invisible(es);
  } else {
    tmp.geneNames <- featureNames(es)
    tmp.conc <- gsub(re.conc,'\\1',tmp.geneNames)
    tmp.conc[ !grepl(re.conc,      tmp.geneNames)] <- NA
    fData(es)[[label.conc]] <- as.numeric(tmp.conc)
  }
  invisible(es)
}

nses.spike <- function(es){
    # very dirty
  if ('SpikeInInput' %in% fvarLabels(es)){
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

extract.PCF <- function(es, label.GeneName=NULL, label.PCF='PCF'){
  # extract & remove PCF from GeneName column of fData, save it to new 'PCF' column of fData
  # actual expression values will not be changed yet; use apply.PCF()
  # debug: es=es.raw; label.GeneName='GeneName'; label.PCF='PCF'
  # end debug: rm(es,label.GeneName,label.PCF,geneNames,have.PCF,PCFs)

  # extract gene names to work with,
  # something like 'hsa-miR-302d-3p|0.006', or 'LIG_POS_B'
  geneNames <- ifelse1(is.null(label.GeneName),
                      featureNames(es),
                      fData(es)[[label.GeneName]])

  have.PCF <- grepl('\\|\\d',geneNames)
  if (sum(have.PCF)==0) {
    warning('PCF not found in gene names. Returning unchanged.');
    invisible(es);
  }

  geneNames[!have.PCF] <- paste0(geneNames[!have.PCF], '|0');
  PCFs <- as.numeric(            gsub(   '.*\\|(\\d+)','\\1',geneNames))
  # tmp.PCF <- gsub('^.+\\|', '', tmp.fNames)
  # tmp.PCF[!grepl(  '^.+\\|.+',tmp.fNames)] <- '0'

  featureNames(es) <- gsub('^(.+)\\|.+$', '\\1', geneNames)
  fData(es)[[label.PCF]] <- PCFs
  invisible(es);
}


apply.PCF <- function(es, extract=TRUE, label.pos='POS_A(128)'){
  PCF.applied <- pData(es)$PCF.applied
  if (sum(PCF.applied,na.rm = T)>0) {stop('PCF seems to be already applied!');}
  PCFs <- fData(es)$PCF;
  if (is.null(PCFs) && extract!=TRUE) {stop('Need to extract PCF first!');}
  if (is.null(PCFs)) {es %<>% extract.PCF; PCFs <- fData(es)$PCF;}

  posA <- exprs(es[label.pos,])
  backgrounds <- sapply(X = posA, FUN = '*', PCFs)

  X <- exprs(es) - backgrounds
  X[X<1] <- 1
  exprs(es) <- round(X)
  pData(es)$PCF.applied <- TRUE;
  invisible(es)
}
