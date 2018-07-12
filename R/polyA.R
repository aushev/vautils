mergaxtmaps <- function(dir) {
  #dir <- "d:/OutDocs/BigData/Genome/UCSC/hs vs pantro/";
  if (substr(dir, nchar(dir), nchar(dir))=="/") dir <- substr(dir, 1, nchar(dir)-1); # chop trailing '/'
  if (!file.exists(dir)) stop("Directory not found!");
  mask <- "*net.axt.map$";
  flist <- list.files(path=dir, pattern=mask, full.names=TRUE);
  if (length(flist)==0) stop("no files found!");
  dtaxtmap <- fread(flist[1], sep="\t");
  dtaxtmapFull <- dtaxtmap[0,]; # just to initialize columns

  # merge all *.axt.map files to single table dtaxtmapFull
  before<-proc.time()[3];
  for (fnInpAXTmap in flist) {
    if (file.exists(fnInpAXTmap)) {
      dtaxtmap <- fread(fnInpAXTmap, sep="\t");
      dtaxtmapFull <- rbind(dtaxtmapFull, dtaxtmap);
    } # end if file exists
  } # end for
  rm(fnInpAXTmap); rm(dtaxtmap);
  after<-proc.time()[3]; (after-before); rm(before); rm(after);
  return(dtaxtmapFull);
}


goverlap <- function (dtLeft,         dtRight,
                      chrL="chr",     chrR="chr",
                      startL="start", endL="end",
                      startLsh=0,     endLsh=0,
                      startR="hex5p", endR="hex5p",
                      startRsh=0,     endRsh=5) {
  dtLeft [[chrL]] <- factor(dtLeft [[chrL]]) # factorize 'chromosome' field
  dtRight[[chrR]] <- factor(dtRight[[chrR]]) # factorize 'chromosome' field
  mylevels <- unique(c(levels(dtLeft[[chrL]]), levels(dtRight[[chrR]]))) #
  # dtLeft$tmp.rowid  <- seq_len(nrow(dtLeft));  setkey(dtLeft,  'tmp.rowid')
  dtRight$tmp.rowid <- seq_len(nrow(dtRight)); setkey(dtRight, 'tmp.rowid')
  if (anyNA(dtLeft [[startL]])) stop('NA in dtLeft[[', startL, ']]!');
  if (anyNA(dtLeft [[endL  ]])) stop('NA in dtLeft[[',   endL, ']]!');
  if (anyNA(dtRight[[startR]])) stop('NA in dtRight[[',startR, ']]!');
  if (anyNA(dtRight[[endR  ]])) stop('NA in dtRight[[',  endR, ']]!');
  irLeft  <- IRanges(dtLeft [[startL]]+startLsh, dtLeft [[endL]]+endLsh)
  irRight <- IRanges(dtRight[[startR]]+startRsh, dtRight[[endR]]+endRsh)
  rleLeft <-  Rle(factor(dtLeft [[chrL]], levels=mylevels))
  rleRight <- Rle(factor(dtRight[[chrR]], levels=mylevels))
  grL <- GRanges(rleLeft,  irLeft)
  grR <- GRanges(rleRight, irRight)

  ovlaps = findOverlaps(grR, grL) # 75687

  dtLeftHits <- dtLeft[subjectHits(ovlaps)]; # 75687 # don't re-set key!
  dtLeftHits[, rightHit := queryHits(ovlaps)]    #
  setkey(dtLeftHits, rightHit)

  dtRightHits <- dtLeftHits[dtRight, nomatch=NA, allow.cartesian = TRUE] # 90190 o_O # that's where we need to have key tmp.rowid!
  #print(names(dtRightHits));
  if (chrL==chrR) {deldupflds(dtRightHits, chrL);} else {deldupflds(dtRightHits, chrL, chrR);}
  setkey(dtRightHits, rightHit)
  #print(names(dtRightHits));
  return(dtRightHits);
} ### goverlap







# names_old <- cs('  ugenellid   symbol   gnum   dist   hexSeq')
# names_new <- cs('g.ugenellid g.symbol g.gnum h.dist h.hexSeq')

#names_old <- c(cs('  llid   siteid   sitenum   s2PDU   s3PDU   cleavage   site1   unigene.id  unigene2'), 'supporting EST', names_old)
#names_new <- c(cs('s.llid s.siteid s.sitenum s.s2PDU s.s3PDU s.cleavage s.site1 s.unigene   s.unigene2'), 's.ESTs',         names_new)

#names_old <- c(cs('adjFreq hexseqOK posNewClv adjHexSeqRef adjHexSeqAlt'), names_old)
#names_new <- c(cs('    MAF hexSeqOK posClvNew hexSeqMajor  hexSeqMinor '), names_new)

#names_old <- c(cs(' adjHexSeqRef adjHexSeqAlt '), names_old)
#names_new <- c(cs(' hexSeqMajor  hexSeqMinor  '), names_new)

