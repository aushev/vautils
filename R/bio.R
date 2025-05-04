update_gene_symbols <- function(inpGenes, db=org.Hs.eg.db, from="ENSEMBL", to='SYMBOL'){
  stopifnot(duplicated(inpGenes)==0L)

  conv.genesymbols <- select(db, keys = inpGenes, columns=to, keytype=from, multiVals=T)
  #mapIds(org.Hs.eg.db,keys=allGenes,column="SYMBOL",keytype="ENSEMBL",multiVals="first")

  conv.genesymbols <- as.data.table(conv.genesymbols)
  conv.genesymbols[,from:=get(from)]
  conv.genesymbols[,  to:=get(to)]
  stopifnot(length(conv.genesymbols[,from] %-% inpGenes)==0L)
  conv.genesymbols[, dup:=.N,by=from]
  #tab(conv.genesymbols$dup)

  conv.genesymbols[,          to2:=to]; #
  conv.genesymbols[is.na(to), to2:=from]; # when nothing found, we keep original

  conv.genesymbols[dup>1, hasSame:=(from %in% .SD$to2),by=from]
  conv.genesymbols[, isSame:=(from==to2)]
  #tab(conv.genesymbols[dup==1, isSame])

  conv.genesymbols[dup==1,     OK:=TRUE]
  conv.genesymbols[hasSame==T, OK:=isSame]

  conv.genesymbols[dup>1 & hasSame==F, OK:=(to2==.SD[1,to2]), by=from]; # when multiple hits, we keep the first one
  #tab(conv.genesymbols$OK)

  conv.genesymbols <- conv.genesymbols[OK==T,]
  stopifnot(duplicated(conv.genesymbols$from)==0)
  stopifnot(inpGenes %in% conv.genesymbols$from)

  setkey(conv.genesymbols, from)
  conv.genesymbols <- conv.genesymbols[inpGenes]

  conv.genesymbols[, dup2:=.N, by=to2]
  conv.genesymbols[dup2>1, isFirst:=(from==.SD[1,from]), by=to2]; # when duplicated hits, we keep the first one
  conv.genesymbols[isFirst==F,to2:=from]

  conv.genesymbols[, dup2:=.N, by=to2]
  conv.genesymbols[dup2>1, to2:=from] # OMG
  conv.genesymbols <- conv.genesymbols[OK==T,]

  stopifnot(duplicated(conv.genesymbols$to2)==0)
  stopifnot(conv.genesymbols$from==inpGenes)

  return(conv.genesymbols)
}







