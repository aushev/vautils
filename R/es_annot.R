annotateENS <- function(es,
                        es.ID='ENSG',
                        mart_filter="ensembl_gene_id",
                        mart_attr=cs("external_gene_name gene_biotype")){
  mart <- useDataset("hsapiens_gene_ensembl", useMart("ENSEMBL_MART_ENSEMBL"))

  annotLookup <- biomaRt::getBM(
    attributes = c(mart_filter,mart_attr), # cs("ensembl_gene_id gene_biotype external_gene_name")
    filters    = mart_filter, # "ensembl_gene_id"
    values     = Biobase::fData(es)[[es.ID]], # $ENSG,
    mart       = mart,
    uniqueRows = TRUE)

  dt.annotlookup <- data.table(annotLookup)
  dt.annotlookup <- unique(dt.annotlookup)

  setkeyv(dt.annotlookup,mart_filter); # ensembl_gene_id

  es %<>% attachfData(dt.annotlookup, key.dat=mart_filter, key.es=es.ID)

  invisible(es)

}

.orphan <- function(es.input){
  fData(es.input)$oriGeneID <- featureNames(es.input)
  fData(es.input)$ENSG <- gsub('\\..*','',featureNames(es.input))
  fData(es.input)$par <- gsub('ENSG\\d+\\.\\d+','',featureNames(es.input))

}


gene.info.from.ENSG <- function(input, mart=NULL, fnLocal=NA, forceupdate=F, dataset='hsapiens_gene_ensembl', mirror="uswest"){ # former gene.info.from.ENSG()
  if ('ExpressionSet' %in% class(input)){
    #stop('Function changed. Input must be a character vector (list of ENSG).');
    dt.annotlookup <- gene.info.from.ENSG(featureNames(input), mart, fnLocal, forceupdate, dataset, mirror)
    input %<>% attachfData(dat=dt.annotlookup, key.dat = 'oriID')
    return(input);
  }

  if (! 'character' %in% class(input)){
    stop('Please provide eSet or character vector as input.')
  }

  if (is.na(fnLocal) || !file.exists(fnLocal) || forceupdate){

    if (is.null(mart)){
      require("biomaRt")
      message('Fetching mart dataset ', dataset, ' from the mirror ', mirror);
      mart <- useEnsembl("ensembl", dataset=dataset, mirror=mirror, verbose=T)
    }

    annotLookup.full <- getBM(
      mart=mart,
      # cs("ensembl_transcript_id"),
      attributes=cs("ensembl_gene_id gene_biotype external_gene_name chromosome_name"),
      filter="ensembl_gene_id",
      values="",
      uniqueRows=TRUE)

    message('Full annotation table retrieved: ', dim(annotLookup.full)[1], ' x ', dim(annotLookup.full)[2], ' (',paste0(names(annotLookup.full), collapse = ','),')')

    dt.annot <- data.table(annotLookup.full); # 58611
    dt.annot <- unique(dt.annot)

    #....object <- list(tx2gene=tx2gene,metadata=TxDb.metadata)
    #if (!is.na(fnLocal)) save(tx2gene.object, file = fnLocal);
    if (!is.na(fnLocal)) save(dt.annot, file = fnLocal);
  } else {
    message('Trying to load local file ',fnLocal)
    dt.annot <- loadv1(fnLocal)
  }

  dt.lookup <- data.table(oriID=input)
  dt.lookup[, ENSG:=gsub('\\..*','',oriID)]; # remove version and par
  ensLookup <- unique(dt.lookup$ENSG);

  setkey(dt.annot,ensembl_gene_id)
  setkey(dt.lookup,ENSG)

  dt.annotlookup <- dt.annot[dt.lookup]

  return(dt.annotlookup)
}


tx2gene.from.salmon <- function(fn_salmon, colsNameSplit=cs('ENST ENSG OTTHUMG OTTHUMT NameT NameG x1 type'), removeVersions=F){
  dt.features.g <- fread(fn_salmon[1]); # 205,870 x 5
  stopifnot(sapply(strsplit(dt.features.g$Name, split = '|', fixed = T), length)==length(colsNameSplit))
  dt.features.g[,(colsNameSplit):=tstrsplit(Name,split = '|', fixed = T)]
  dt.features.g[, Name:=NULL]
  dt.features.g[, hasProt:=('protein_coding' %in% type), by=ENSG]
  dt.features.g[, txTypes:=paste0(unique(type),collapse = ';'), by=ENSG]
  if (removeVersions){
    dt.features.g[, ENST:=gsub('(ENST\\d+).*','\\1',ENST)]
    dt.features.g[, ENSG:=gsub('(ENSG\\d+).*','\\1',ENSG)]
  }
  return(dt.features.g)
}

gene.info.from.salmon <- function(es.input,
                                  fn_salmon,
                                  colsNameSplit=cs('ENST ENSG OTTHUMG OTTHUMT NameT NameG x1 type')){
  dt.features.g <- tx2gene.from.salmon(fn_salmon, colsNameSplit);
  dt.features.g <- unique(dt.features.g[,.(ENSG,OTTHUMG,NameG,txTypes,hasProt)])
  es.input %<>% attachfData(dt.features.g, key.dat = 'ENSG');
}


# tx2geneFromEnsembl.cached():
# workaround to "cache" results of makeTxDbFromEnsembl() call
tx2geneFromEnsembl.cached <- function(fnLocal=NA,forceupdate=F,server='useastdb.ensembl.org'){
  if (is.na(fnLocal) || !file.exists(fnLocal) || forceupdate){
    message('Fetching database from the server: ', server);
    TxDb  <- makeTxDbFromEnsembl(server=server)
    print(TxDb)
    k <- keys(TxDb, keytype = "TXNAME");  # for makeTxDbFromEnsembl(): 250,641 txs like ENST00000636745 etc
    message('Loaded ', length(k), ' keys.')
    tx2gene <- select(TxDb, k, columns = cs('GENEID TXCHROM TXTYPE'), "TXNAME");  # 250641 x 4
    message('Resulting tx2gene: ', dim(tx2gene)[1], ' x ', dim(tx2gene)[2])
    TxDb.metadata <- metadata(TxDb);
    tx2gene.object <- list(tx2gene=tx2gene,metadata=TxDb.metadata)
    if (!is.na(fnLocal)) save(tx2gene.object, file = fnLocal);
    return(tx2gene);
  }
  message('Trying to load local file ',fnLocal)
  tx2gene.object <- loadv1(fnLocal)
  print(tx2gene.object$metadata)
  return(tx2gene.object$tx2gene)
}

