annotateENS <- function(es,
                        es.ID='ENSG',
                        mart_filter="ensembl_gene_id",
                        mart_attr=cs("external_gene_name gene_biotype")){
  mart <- useDataset("hsapiens_gene_ensembl", useMart("ENSEMBL_MART_ENSEMBL"))

  annotLookup <- getBM(
    attributes=c(mart_filter,mart_attr), # cs("ensembl_gene_id gene_biotype external_gene_name")
    filters=mart_filter, # "ensembl_gene_id"
    values=fData(es.sebra10)[[es.ID]], # $ENSG,
    mart=mart,
    uniqueRows=TRUE)

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


gene.info.from.ENSG <- function(input, mart=NULL){ # former gene.info.from.ENSG()
  if ('ExpressionSet' %in% class(input)){
    stop('Function changed. Input must be a character vector (list of ENSG).');
  }

  list.ENSG <- gsub('\\..*','',input); # remove version and par

  if (is.null(mart)){
    require("biomaRt")
    #mart <- useMart("ENSEMBL_MART_ENSEMBL", dataset = "hsapiens_gene_ensembl")
    mart <- useEnsembl("ensembl", dataset = "hsapiens_gene_ensembl", mirror = "useast")
  }

  #genes.test <- cs('ENSG00000223972 ENSG00000228589 ENSG00000228943 ENSG00000214812 ENSG00000253005 ENSG00000253005')
  #ensLookup <- genes.test
  ensLookup <- unique(list.ENSG)

  annotLookup <- getBM(
    mart=mart,
    # cs("ensembl_transcript_id"),
    attributes=cs("ensembl_gene_id gene_biotype external_gene_name chromosome_name"),
    filter="ensembl_gene_id",
    values=ensLookup,
    uniqueRows=TRUE)

  dt.annotlookup <- data.table(annotLookup); # 58611
  dt.annotlookup <- unique(dt.annotlookup)

  setkey(dt.annotlookup,ensembl_gene_id)

  dt.annotlookup <- dt.annotlookup[list.ENSG,]

#  es.input %<>% attachfData(dt.annotlookup)
  #tmpF1 <- tmpF[is.na(dt.annotlookup$gene_biotype),]

#  fData(es.input)$gene_biotype[is.na(fData(es.input)$gene_biotype)] <- '?'
  return(dt.annotlookup)
}


gene.info.from.salmon <- function(es.input,
                                  fn_salmon,
                                  colsNameSplit=cs('ENST ENSG OTTHUMG OTTHUMT NameT NameG x1 type')){
  dt.features.g <- fread(fn_salmon[1]); # 205,870 x 5
  stopifnot(sapply(strsplit(dt.features.g$Name, split = '|', fixed = T), length)==length(colsNameSplit))
  dt.features.g[,(colsNameSplit):=tstrsplit(Name,split = '|', fixed = T)]
  dt.features.g[, Name:=NULL]
  dt.features.g[, hasProt:=('protein_coding' %in% type), by=ENSG]
  dt.features.g[, txTypes:=paste0(unique(type),collapse = ';'), by=ENSG]
  dt.features.g <- unique(dt.features.g[,.(ENSG,OTTHUMG,NameG,txTypes,hasProt)])

  es.input %<>% attachfData(dt.features.g, key.dat = 'ENSG')


}
