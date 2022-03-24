
load_DT <- function(dtVar, fnVar, fnDef, refresh=T, colsIncl, colsExcl=NULL, ...) {
  varnameDT <- deparse(substitute(dtVar));
  varnameFN <- deparse(substitute(fnVar));
  newfn <- "";
  if (exists(varnameDT) & refresh!=T) {
    cat("DT (", varnameDT, ") already exists!\n");
    dtOut <- copy(dtVar);
  } else {
    if (refresh==T) {cat("Existing DT (", varnameDT, ") ignored! ");} else {cat("DT (", varnameDT, ") does not exist! ");}
    if (exists(varnameFN)) {
      cat("filename variable (", varnameFN,") is defined! \n");
      newfn <- fnVar;
    } else {
      cat("filename (", varnameFN,") was not defined before! \n");
      if (!missing(fnDef)) {newfn <- fnDef;} else {cat("Default filename not set!\n")};
    }
  }
  dtOut <- loadDT(fnInput = newfn, colsExcl=colsExcl, ...);
  return(dtOut);
} # e. load_DT

loadDT <- function(fnInput, sep="\t", header=T, refresh=T, colsIncl, colsExcl=NULL, force_chr=NULL, force_num=NULL, ...) {
  while (!file.exists(fnInput)) {
    ans <- ask("File not found: ", fnInput, ".\n Enter another file name, or press Enter to try again, or ESC to exit: ");
    if (ans!="") {fnInput <- ans;}
  } # end while

  #  if (!is.null(force_chr))

  cat(fnInput, "exists. Loading... ");
  dtOut <- fread(input = fnInput, sep=sep, header=header, ...);
  cat(nrow(dtOut), " records loaded.\n");

  if (!missing(colsIncl)) {
    dtOut <- dtOut[,colsIncl, with=FALSE];
  }

  if (!missing(colsExcl)) {
    outnames <- setdiff(names(dtOut), colsExcl);
    cat('outnames: ', outnames, '\n')
    dtOut <- dtOut[,outnames, with=FALSE];
  }

  invisible(dtOut);
}


save_DT <- function(dtIn, fnSaveTo=NULL, quote=F, sep="\t", header=T, row.names=F, commentString=NULL, ...) {
  if (!is.data.frame(dtIn)) {dtIn <- as.data.frame(dtIn);}
  if (is.null(fnSaveTo)) {
    fnSaveTo <- deparse(substitute(dtIn));
    if (sep=='\t') fnSaveTo <- paste0(fnSaveTo, '.tsv');
    if (sep==',')  fnSaveTo <- paste0(fnSaveTo, '.csv');
  }
  cat('Saving', nrow(dtIn),'records to:', fnSaveTo, "... ");
  con <- file(fnSaveTo, open="wt")
  if (!is.null(commentString)) writeLines(paste0('# ',commentString), con)
  write.table(dtIn, file=con, quote=quote, sep=sep, row.names=row.names, col.names=header, ...);
  close(con)
  cat("done.");
}

inexcel <- function(dtIn, row.names=F, na='', ...){
  name_dt <- deparse(substitute(dtIn))
  name_date <- format(Sys.time(), '%Y%m%d_%Hh%Mm%Ss')
  fn_save <- tempfile(pattern = name_date %+% '_' %+% name_dt %+% '_', fileext = '.xls');
  if (row.names==T) dtIn <- cbind(rn=row.names(dtIn),dtIn)
  save_DT(dtIn, fnSaveTo = fn_save, na=na, ...)
  system(command = paste0('cmd /C ', fn_save));
  return(fn_save);
}


inexcel2 <- function(dtIn, row.names=F, na='', ...){

  name_dt <- deparse(substitute(dtIn))
  name_date <- format(Sys.time(), '%Y%m%d_%Hh%Mm%Ss')
  name_fn <- name_date %+% '_' %+% name_dt %+% '_'
  fn_save <- tempfile(pattern = name_fn, fileext = '.xlsx');
  if (row.names==T) dtIn <- cbind(rn=row.names(dtIn),dtIn)
  # save_DT(dtIn, fnSaveTo = fn_save, na=na, ...)
  # system(command = paste0('cmd /C ', fn_save));
  # return(fn_save);

  wb <- createWorkbook()
  #  style.bold <- createStyle(textDecoration = 'bold')

  addWorksheet(wb, name_fn)
  writeData(wb,name_fn,dtIn)


  saveWorkbook(wb, file = fn_save,overwrite = T)
  system('cmd /C ' %+% fn_save)

  return(fn_save)

}






# tdt() - transpose a data.table ####
tdt <- function(inpdt, newcolname=names(inpdt)[1]){
  transposed <- t(inpdt[,-1,with=F]);
  colnames(transposed) <- inpdt[[1]];
  transposed <- data.table(transposed, keep.rownames=T);
  setnames(transposed, 1, newcolname);
  return(transposed);
}

# returns a vector of values where TRUE means this column is empty ####
emptycolumns <- function(input.table){
  return(
    sapply(1:ncol(input.table),
           function(X){
             all(is.na(input.table[, X]))
           }
    )
  );
}

dterr <- function (dtIn, condition, errtext) {
  eval.parent(substitute({ # header of magic
    if (!is.element('errors', names(dtIn))) {dtIn[,errors:=""];}
    #dtIn[eval(substitute(condition)), errors:={paste0(errors, errtext)}]; # without magic we needed this
    dtIn[condition, errors:={paste0(errors, errtext)}];
    invisible(dtIn);
  })); # footer of magic
}




# dtIn      - data.table containing our records
# condition - expression defines which records will be flagged
# flagtxt   - text of error message
adderrb <- function(dtIn, condition, flagtxt, inpArrName="flagsList"){
  newval <- addBinaryFlag(flagtxt = flagtxt, inpArrName = inpArrName);
  # adding flags to the 'errors' column of our table
  eval.parent(substitute({ # header of magic
    if (!is.element('errors', names(dtIn))) {dtIn[,errors:=0L];} # if this column did not exist, create it, initialize with 0L
    dtIn[condition, errors:=bitwOr(errors, 2^(newval-1))]; # for records which meet condition, we add new flad to existing mask
    invisible(dtIn);
  })); # footer of magic

}

flexread <- function(fnRead, sheetIndex=1, sheetName=NULL,
                     silent=T, keyby = NA, char=NULL, num=NULL, filetype=NULL,
                     clean.names = F, trimspaces=F, deluseless=F,
                     rename.from=NULL,rename.to=NULL,
                     ...){
  message(' Opening ' %+% fnRead);

  dots <- substitute(list(...));

  if (!file.exists(fnRead)){stop('... File not found!\n');return(NULL);}

  if (is.null(filetype)){
    #cat('Trying to guess filetype... ');
    if (substrRight(fnRead,4) %in% cs('xlsx xlsm .xls')) {
      filetype <- 'xls';
    }
    else if (substrRight(fnRead,8) == 'sas7bdat') {
      filetype <- 'sas7bdat';
    }
    else if (substrRight(fnRead,3) == 'sav') {
      filetype <- 'spss';
    }
    else if (substrRight(fnRead,3) == 'zip') {
      filetype <- 'zip';
    }
    else {filetype <- 'auto';}
    cat(' as ', filetype);
  }

  if (filetype=='xls'){
    cat(' with openxlsx/read.xlsx... ');
    sheet <- sheetIndex;
    if (!is.null(sheetName)) sheet <- sheetName;
    reqq('openxlsx', verbose = F);
    rez <- openxlsx::read.xlsx(fnRead, sheet, ...);
    rez <- data.table(rez);
  }
  else if (filetype == 'sas7bdat') {
    cat(' with read.sas7bdat... ');
    reqq('sas7bdat', verbose = F);
    rez <- read.sas7bdat(fnRead);
    rez <- data.table(rez);
  }
  else if (filetype == 'spss') {
    cat(' with read.spss... ');
    reqq('foreign', verbose = F);
    rez <- read.spss(fnRead);
    rez <- as.data.table(rez);
  }
  else {
    cat(' with fread... ');
    rez <- fread(fnRead, ...);
  }

  if (!is.null(dots$skip) & filetype!='auto'){
    skip <- as.numeric(dots$skip);
    if (!isTRUE(skip>0)) error('*skip* argument must be a positive integer');
    names(rez) <- sapply(rez[skip,], as.character); #as.character(unlist(rez[skip,]));
    rez <- rez[-(1:skip),]
  }

  if (length(rez)>0) cat('Success:', paste(dim(rez),collapse = ' x '));
  if (!is.na(keyby)) setkeyv(rez, keyby)

  if (!is.null(num)){
    for (.col in num){
      if (.col %in% names(rez) & !class(rez[[.col]])=='character'){rez[[.col]] <- as.numeric(as.character(rez[[.col]]));}
    } # e. for
  } # e. if

  if (!is.null(char)){
    for (.col in char){
      if (! .col %in% names(rez)) {warning(.col,' asked to be converted to character but this column not found!'); next;}
      if (!class(rez[[.col]])=='character'){
        #rez[[.col]] <- as.character(rez[[.col]])
        set(rez, j = .col, value = as.character(rez[[.col]]))
        }
    } # e. for
  } # e. if !is.null(char)

  if (clean.names == T) names(rez) <- cleannames(names(rez));
  if (deluseless == T) rez <- deluselesscol(rez);

  if (trimspaces==T){
    for (i in seq_len(ncol(rez))){
      if (is.character(rez[[i]]))
        rez[[i]] <- trimws(rez[[i]])
    }
  } # e. trimspaces

  if (!is.null(rename.from) & !is.null(rename.to)){
    stopifnot(length(rename.from)==length(rename.to));
    cat('\n')
    rez %<>% setnamessp(rename.from,rename.to, verbose=!silent);
  }



  cat('\n')
  return(rez);
}



# usage:
# Diagnosis=get_data_long2wide(.SD, 'evValue', list(evType2=  'Overall'    ), multi='last', dbgI=pID),
# Subtype  =get_data_long2wide(.SD, 'evValue', list(evType2=c('HR subtype')), multi='paste', dbgI=pID),
# DOB      =get_data_long2wide(.SD, 'evDate',  list(evType='DOB'), dbgI=pID),
# DoDiagn  =get_data_long2wide(.SD, 'evDate',  list(evType2='Overall'),    multi='first', dbgI=pID),
get_data_long2wide <- function(inpDat, field, filters=NULL, multi='error', na.rm=T, dbgI=NULL){ # error, first, last

  if (length(field)!=1) stop('field argument must have a length 1')

  inp.filtered <- inpDat
  if (!is.null(filters)) {
    for (this.filter in names(filters)){
      this.filter.value <- filters[[this.filter]]
      inp.filtered <- inp.filtered[get(this.filter) %in% this.filter.value,]
    }# e. foreach filter
  } # e. if has filters

  output <- inp.filtered[[field]]
  if ('paste' %in% multi | 'pasteunique' %in% multi) {output <- as.character(output);}
  if (na.rm) output <- na.omit(output);
  if (length(output)>1){
    if ('error' %in% multi) stop('Error! Non-unique output!\n', dbgI, '\n', (filters), '\n', paste(output, collapse = '\n'))
    if ('paste' %in% multi)       output <- paste(output, collapse='; ')
    if ('pasteunique' %in% multi) output <- paste(unique(output), collapse='; ')
    if ('first' %in% multi)       output <- output[1]
    if ('last' %in% multi)        output <- output[length(output)]
  }

  if (length(output)==0) output <- output[NA];
  if (length(output)>1) warning('Something wrong... ', field)

  output
}

extract_event_data <- function(inpDat, lookupCol='evType', lookupVal, col2get, unq='paste', sep=' !!! ', skip=NULL){
  vals <- inpDat[[col2get]];
  filt1 <- inpDat[[lookupCol]]==lookupVal;
  filt2 <- TRUE
  if (!is.null(skip)){
    if (length(skip)>1) stop('Not implemented yet')
    field1 <- names(skip)[[1]]
    if (field1 %!in% names(inpDat)) stop('Column [',field1,'] not found!');
    filt2.vals <- inpDat[[field1]]
    filt2 <- is.na(filt2.vals) | filt2.vals!=skip[[1]]
  }
  vals <- vals[filt1 & filt2]
  rez <- na.omit(vals)
  # if (length(rez)>1)
  #   browser()
  if (length(rez)==0) return(rez)
  rez <- switch (unq,
                 paste = paste(rez, collapse =sep),
                 first = rez[[1]],
                 `1` = rez[[1]],
                 last = rez[[length(rez)]]
  )

  return(rez)
  #  return(length(rez))
}


















apprec <- function(dtInput, dtRecord){
  oldkeys <- key(dtInput);
  #print(names(dtRecord));
  if (ncol(dtInput)>ncol(dtRecord)) {
    cat("Main table has more columns (", ncol(dtInput), ") than table being added (", ncol(dtRecord), "). Adding missing colums: \n");
    for (eachcol in names(dtInput)) {
      if (is.element(eachcol, names(dtRecord))) next;
      thisclass <- class(dtInput[[eachcol]]);
      cat(eachcol, ": ", thisclass, "\n");
      dtRecord[,eachcol:=as(NA, thisclass), with=F];
    } # end for
    #print(names(dtRecord));
  } # end if >
  dtInput <- rbind(dtInput, dtRecord, use.names=T);
  setkeyv(dtInput, oldkeys);
  invisible(dtInput);
}

duplicated3 <- function(dtIn, bykey) { #
  if (nrow(dtIn)==0) return(integer(0));
  dtOut <- data.table(dtIn); # instead of copy(). Maybe not needed...
  if (!missing(bykey)) { # if key is passed, we re-key the table
    dtOut[, tmp.id.original:=seq_len(nrow(dtOut))] # to save original order
    setkeyv(dtOut, bykey);
  }
  dups1 <- duplicated(dtOut);
  if (is.null(key(dtOut))){ # if table is not keyed
    dups2 <- duplicated(dtOut, fromLast = T);
    dups <- dups1 | rev(dups2);
  } else { # if table is keyed
    dups2 <- c(dups1[-1L], FALSE);
    dups <- dups1 | dups2;
    if (!missing(bykey)) {dups <- dups[order(dtOut$tmp.id.original)]}
  }
  return(dups);
}

# returns all column names except those set as keys
notkeys <- function(dtIn, stringnames=T){
  if (! 'data.table' %in% class(dtIn)) stop('Not a data.table!');
  return(setdiff(names(dtIn), key(dtIn)));
}

dtdup <- function(dtIn, bykey) { # makes new data.table with only duplicated records from input table
  if (nrow(dtIn)==0) return(dtIn);
  dtOut <- data.table(dtIn); # instead of copy()
  if (!missing(bykey)) {setkeyv(dtOut, bykey);}
  if (is.null(key(dtOut))){
    dups1 <- duplicated(dtOut);
    dups2 <- duplicated(dtOut[nrow(dtOut):1]);
    dups <- dups1 | rev(dups2);
  } else {
    dups1 <- duplicated(dtOut);
    dups2 <- c(dups1[-1L], FALSE);
    dups <- dups1 | dups2;
  }
  dtOut <- dtOut[dups==TRUE];
  return(dtOut);
}





# compares 2 columns, returns vector (F means equal or both NA, T means different, NA means *one* is NA)
cmp2fldsbynum <- function(dtIn, f1n, f2n, verbose=F){
  if (length(f1n)!=1) stop('cmp2dupfldsbynum: one argument expected in f1n, ', length(f1n), ' received: ', f1n);
  if (length(f2n)!=1) stop('cmp2dupfldsbynum: one argument expected in f2n, ', length(f2n), ' received: ', f2n);
  if (!is.numeric(f1n) | !is.numeric(f2n))  stop('Column indices must be numeric!');
  if (f1n > ncol(dtIn) | f2n > ncol(dtIn)) {warning('Table has less columns than requested number!'); return(F);}
  if (f1n==f2n)                            {warning('Identical indices, nothing to compare!'); return(F);}

  ftitle <- paste0(f1n, " (", names(dtIn)[f1n], ") vs ", f2n, " (", names(dtIn)[f2n], ")");
  if (verbose) cat(ftitle);
  values1 <- dtIn[[f1n]];
  values2 <- dtIn[[f2n]];
  diff <- F;

  if (is.factor(values1)) {values1 <- as.character.factor(values1);}
  if (is.factor(values2)) {values2 <- as.character.factor(values2);}

  if (class(values1)=='Date') {values1 <- as.character(values1);}
  if (class(values2)=='Date') {values2 <- as.character(values2);}

  diff <- (values1 != values2);

  #diff[is.na(diff)] <- T
  diff[is.na(values1) & is.na(values2)] <- F

  invisible(diff);
}


cmp2flds <- function(dtIn, f1, f2, verbose=F){  # compares 2 fields
  if (length(f1)!=1) stop('cmp2dupflds: one value expected in f1, ', length(f1), ' received: ', f1);
  if (length(f2)!=1) stop('cmp2dupflds: one value expected in f2, ', length(f2), ' received: ', f2);

  if (is.numeric(f1) & is.numeric(f2)) {return(cmp2dupfldsbynum(dtIn, f1, f2));}

  ftitle <- f1 %+% " vs " %+% f2;
  if (! f1 %in% names(dtIn)) {warning("No column named ", f1); return(F);}
  if (! f2 %in% names(dtIn)) {warning("No column named ", f2); return(F);}

  if (f1==f2 & sum(names(dtIn)==f1)<2){
    warning('Identical column names (', f1, ') indicated, but there is only 1 column with this name!')
    return(invisible(F));
  }

  f1n = which(names(dtIn)==f1)
  f2n = which(names(dtIn)==f2)

  if (f1==f2){f1n = f1n[1]; f2n = f2n[2];} #

  if (length(f1n)>1){warning('Multiple fields named ', f1);}
  if (length(f2n)>1){warning('Multiple fields named ', f2);}

  invisible(cmp2fldsbynum(dtIn, f1n[1], f2n[1], verbose=verbose));
} # e. cmp2flds()

# strictly compares 2 fields
cmp2dupflds.strict <- function(dtIn, f1, f2){
  if (length(f1)!=1) stop('cmp2dupflds: one value expected in f1, ', length(f1), ' received: ', f1);
  if (length(f2)!=1) stop('cmp2dupflds: one value expected in f2, ', length(f2), ' received: ', f2);
  if (identical(dtIn[[f1]],dtIn[[f2]])) return(TRUE);
  return(FALSE);
}


del2dupflds <- function(dtIn, f1, f2){
  if (is.numeric(f1) & is.numeric(f2)) diff <- cmp2fldsbynum(dtIn, f1, f2)
  else diff <- cmp2flds(dtIn, f1, f2);
  if (all(!is.na(diff) & diff==F)){
    cat('\nEqual, deleting: ', f2)
    dtIn[,c(f2):=NULL]
  } else cat('Not equal, skipping.');

  invisible(dtIn)

}


deldupflds <- function(dtIn, f1=names(dtIn), f2=NA, tolNA=FALSE) { # delete one of 2 fields if they are "identical"
  #dtOut <- dtIn;
  lf1 <- length(f1);
  lf2 <- length(f2);
  diff <- rep(F, nrow(dtIn));
  if      (lf1==1 & lf2==1) {diff <- diff | del2dupflds(dtIn, f1, f2, tolNA);}
  else if (lf1>0 & is.na(f2[1])) {
    for (i in f1) {diff <- diff | del2dupflds(dtIn, i, NA, tolNA);}
  }
  else if (lf1>1 & lf2>1 & lf1==lf2) {
    for (i in 1:lf1) {
      diff <- diff | del2dupflds(dtIn, f1[i], f2[i], tolNA);
    }
  }
  else stop('Unexpected number of arguments!');


  invisible(diff);
}



cleanXY <- function(dtIn, cols2check, rename=T, tryNA=F, verbose=F, suf.x='.x', suf.y='.y'){
  for (this.col in cols2check){
    f1x <- paste0(this.col,suf.x);
    f1y <- paste0(this.col,suf.y);
    if (f1x %!in% names(dtIn) | f1y %!in% names(dtIn)){
      if (verbose==T) {cat('Column not found! ',this.col,'\n');}
      next;
    } # e.if col not found

    if (cmp2dupflds.strict(dtIn, f1x, f1y)){
      if (verbose==T) {cat('Identical: ',this.col,'\n');}
      dtIn[, (f1y):=NULL];
      if (rename==T) setnames(dtIn,f1x,this.col)
    } else {
      if (verbose==T) {cat(this.col,' strict comparison: not equal. \n');}
      if (tryNA==T){
        notNAx <- !is.na(dtIn[[f1x]]);
        notNAy <- !is.na(dtIn[[f1y]]);
        notNAboth <- notNAx & notNAy;
        if (!identical(dtIn[[f1x]][notNAboth],dtIn[[f1y]][notNAboth])) {
          if (verbose==T) {cat('Different in non-NA.\n');}
          notNAx.val <- dtIn[[f1x]][notNAboth];
          notNAy.val <- dtIn[[f1y]][notNAboth];
          # !!!
          next;
        }

        if (sum(notNAx)>sum(notNAy)){
          if (sum(!notNAx & notNAy)==0){
            if (verbose==T) {cat(f1x, ' is more complete. \n');}
            dtIn[, (f1y):=NULL];
            if (rename==T) setnames(dtIn,f1x,this.col)
          }
        } # e. NAx > NAy

        if (sum(notNAx)<sum(notNAy)){
          if (sum(notNAx & !notNAy)==0){
            if (verbose==T) {cat(f1y, ' is more complete. \n');}
            dtIn[, (f1x):=NULL];
            if (rename==T) setnames(dtIn,f1y,this.col)
          }
        } # e. NAx < NAy

      } # e. tryNA
    }
  } # e. for
  invisible(dtIn)
}


clean2flds.bak <- function(dtIn, f1, f2=NA, rezfname=f1, guess=F, verbose=T){
  #cat(f1, 'vs', f2,'... ');
  if (length(f1)!=1) stop('clean2flds.bak: one value expected in f1, ', length(f1), ' received: ', f1);
  if (length(f2)!=1) stop('clean2flds.bak: one value expected in f2, ', length(f2), ' received: ', f2);
  f1.old <- f1;
  rezfname <- rezfname; # we need to evaluate it here! Otherwise it will evaluate too late
  if (is.na(f2)) {f1 <- paste0(f1,'.x'); f2 <- paste0(f1.old,'.y'); }

  diff <- rep(F, nrow(dtIn));
  if (! f1 %in% names(dtIn)){
    if (verbose) cat(f1, 'not found!\n');
    if (guess) {
      if (paste0(f1,'.x') %in% names(dtIn)) f1 <- paste0(f1,'.x');
    } else return(NULL);
  }

  if (! f2 %in% names(dtIn)){
    if (verbose) cat(f2, 'not found! ');
    if (guess) {
      if (paste0(f2,'.y') %in% names(dtIn)) f2 <- paste0(f2,'.y');
    } else return(NULL);
  }

  diff <- cmp2flds(dtIn, f1, f2, verbose=verbose);

  if (sum(diff)==0){
    cat(": equal, deleting.\n");
    dtIn <- dtIn[, (f2):=NULL]
    if (rezfname!=f1) {
#       if (verbose) cat(' Renaming! ', f1, rezfname)
       setnames(dtIn,f1,rezfname);
      }
  }

}


cleandupflds <- function(dtIn, f1=names(dtIn), f2=NA, scanall=T, guess2=T, verbose=T) { # delete one of 2 fields if they are "identical"
  if (all(is.na(f1))) {f1 <- names(dtIn);} else {f1 <- intersect(f1, names(dtIn));}
  lf1 <- length(f1);
  lf2 <- length(f2);

  if (lf2==1 & is.na(f2)){
    if (scanall==T) {f2 <- (names(dtIn) %-% i);}
  }

  for (i in f1) { # for each column in f1
    #if (verbose)
    cat('\n',i,'... ');
    values1 <- dtIn[[i]];
    if (is.factor(values1))     {values1 <- as.character.factor(values1);}
    if ('Date' %in% class(values1)) {values1 <- as.character(values1);}

    f2.add <- NULL;
    if (guess2==T){
      if (grepl('.x$',i)) {f2.add <- gsub('.x$','.y',i);}
    }
    f2.all <- c(f2, f2.add);
    for (j in (f2.all %-% i)){
      if (! j %in% names(dtIn)) next;
      if (verbose) cat(' vs ', j,'... ')
      values2 <- dtIn[[j]];
      if (is.factor(values2)) {values2 <- as.character.factor(values2);}
      if ('Date' %in% class(values2)) {values2 <- as.character(values2);}
      if (identical(values1, values2)){
        cat(i,'equals',j,'. Deleting',j,'\n');
        #if (j %in% f2.add) {setnames(dtIn, i, )}
        dtIn[, (j):=NULL]
        f2.all <- (f2.all %-% j);
      }
    } # e. for j
  } # e. for i

  #  if      (lf1==1 & lf2==1) {diff <- diff | clean2dupflds(dtIn, f1, f2, guess2=guess2, tolNA=tolNA);}
  invisible(diff);
}


dt_combinecomplete <- function(inpDT, colsFrom, colTo, delFrom=F){
  colsNotFound <- colsFrom %-% names(inpDT)
  if (length(colsNotFound)>0) cat('Columns not found: ', colsNotFound);
  colsFrom <- colsFrom %&% names(inpDT)
  if (length(colsFrom)==0) {return(invisible(inpDT));}
  result <- inpDT[[colsFrom[1]]]
  for (colFrom in colsFrom){
    this.vals <- inpDT[[colFrom]]
    notNAboth <- !is.na(result) & !is.na(this.vals)
    if (!identical(result[notNAboth], this.vals[notNAboth])) stop('Discrepancy found!')
    compl <- is.na(result) & !is.na(this.vals)
    result[compl] <- this.vals[compl]
  }
  inpDT[[colTo]] <- result
  if (delFrom) inpDT[,(colsFrom):=NULL]
  invisible(inpDT)
}



reordcols <- function(dtIn, first=NULL, last=NULL) {
  nfirst <- nlast <- NULL;
  if (sum(duplicated(names(dtIn)))>0) warning('Duplicated names!');
  for (.col in first){
    nfirst <- c(nfirst, which(names(dtIn)==.col))
  }

  for (.col in last){
    nlast <- c(nlast, which(names(dtIn)==.col))
  }

  nrest <- which(!( names(dtIn) %in% unique(c(first,last))  ))
  rez <- c(nfirst,nrest,nlast);
  dtIn <- dtIn[, rez, with=F]
  invisible(dtIn)
}



# deluselesscol: deletes column(s) if they contain only one value (i.e. no diff between records) ####
deluselesscol0 <- function (dtIn, icolnames=names(dtIn), ignNA=F, silent = F, padON=F, padW=NULL, padSide='right', verbose=F) {
  catV <- ifelse(verbose,cat,function(...){})
  if (!is.data.table(dtIn)){
    catV('Input is not data.table! ')
    if (is.data.frame(dtIn)){
      catV('Converting from data.frame... ');
      dtIn <- as.data.table(dtIn);
    } else {stop('Required data.table or data.frame!')}
  }
  cols2del <- NULL;
  if (padON==T & is.null(padW)) padW <- max(nchar(icolnames));
  for (colname in icolnames) {
    catV('\n',colname,'... ')
    values <- dtIn[[colname]];
    # (!any(is.na(values)) && all(values==values[1]))
    if (is.list(values)) {catV('list!!! '); next;}
    if ( all(is.na(values)) || isTRUE(all(values==values[1], na.rm = ignNA))) {
      cols2del <- c(cols2del, colname);
      if (silent==F){
        col_print <- paste0("[", colname, "]");
        padded <- ifelse1(padON==F, col_print, strpad(col_print,padW+2L))
        cat(padded,"is all equal to: ", dtIn[[colname]][1],'\n');
      }# e. not silent
    } # e. identical
  } # e. for
  # catV('\nClass:',class(dtIn))
  catV('\n\nFor deletion:\n', paste0(cols2del,collapse = ' '))
  if (!is.null(cols2del)) dtIn[, (cols2del):=NULL];
  invisible(dtIn);
} # e. deluselesscol()

deluselesscol <- function (dtIn, icolnames=names(dtIn), ignoreColumns=NULL, ignNA=F, silent = F, padON=F, padW=NULL, padSide='right', verbose=F) {
  catV <- ifelse(verbose,cat,function(...){})
  if (nrow(dtIn)==0) {
    if (!silent) warning("Table is empty (0 rows).");
    return(dtIn)
    }
  icolnames <- setdiff(icolnames, ignoreColumns);
  if (!is.data.table(dtIn)){
    catV('Input is not data.table! ')
    if (is.data.frame(dtIn)){
      catV('Converting from data.frame... ');
      dtIn <- as.data.table(dtIn);
    } else {stop('Required data.table or data.frame!')}
  }
  cols2del <- NULL;
  colNs2del <- NULL;
  if (padON==T & is.null(padW)) padW <- max(nchar(icolnames));

  for (colN in seq_len(length(names(dtIn)))) {
    colname <- names(dtIn)[colN];
    if (! colname %in% icolnames) next;
    catV('\n',colname,'... ')
    #values <- dtIn[[colname]];
    values <- dtIn[[colN]];
    refVal <- values[1];
    # (!any(is.na(values)) && all(values==values[1]))
    if (is.list(values)) {catV('list!!! '); next;}
    if ( all(is.na(values)) || isTRUE(all(values==refVal, na.rm = ignNA))) {
      cols2del <- c(cols2del, colname);
      colNs2del <- c(colNs2del, colN);
      if (silent==F){
        col_print <- paste0("[", colname, "]");
        padded <- ifelse1(padON==F, col_print, strpad(col_print,padW+2L))
        cat('\n',padded,"is all equal to: ", refVal);
      }# e. not silent
    } # e. identical
  } # e. for
  # catV('\nClass:',class(dtIn))
  catV('\n\nFor deletion:\n', paste0(cols2del,collapse = ' '))
  #if (!is.null(cols2del)) dtIn[, (cols2del):=NULL];
  if (!is.null(colNs2del)) dtIn[, c(colNs2del):=NULL];
  invisible(dtIn);
} # e. deluselesscol()


mrgcols <- function(dtInput, f_scan, csep="; ", f_ndx="id", delold=T, noNA=T, noE=T) {
  #dtInput <- copy(dtInput);
  if (missing(f_scan)) {f_scan <- setdiff(names(dtInput),f_ndx);} # if fields-to-scan are not defined, we scan all except index
  newfield <- paste0(f_scan[1], ".tmp");
  dtInput[, (newfield):={
    rez <- NULL;
    for (i in 1:ncol(.SD)) {
      val <- .SD[[i]][1];
      if (!is.na(val) & val!="") {rez <- c(rez, cs(.SD[[i]][1], csep));} else
        if ( is.na(val) & noNA==F) {rez <- c(rez, "NA");} else
          if (val=="" & noE==F) {rez <- c(rez, "A");}
    }
    rez <- unique(rez);
    rez <- paste0(rez, collapse=csep);
    rez;
  }, by=f_ndx, .SDcols=f_scan]

  if (delold==TRUE) {
    dtInput[,(f_scan):=NULL];  # delete old fields
    setnames(dtInput, newfield, f_scan[1]);# put old names
  }
  invisible(dtInput);
} # f_end mrgcols3

mergerows <- function(dtInput, f_ndx, f_scan, csep=";", delold=TRUE) {
  dtInput <- data.table(dtInput); # copy to new table
  if (missing(f_scan)) {f_scan <- setdiff(names(dtInput),f_ndx);} # if fields-to-scan are not defined, we scan all except index
  newfields <- paste0(f_scan, "_S");
  cat('Index by:', f_ndx, '; merging fields:\n', f_scan, '\n');
  mrg.cnt <- 0L;
  dtInput[, (newfields):={
    #print(paste0("N=", .N, "; ncol=", ncol(.SD)));
    rez2 <- list(); # each member of the list will be a column
    mrg.cnt <- rep(0L, ncol(.SD)); # counter for merges made
    for (i in 1:ncol(.SD)) {
      #print(paste0("  i: ", i));
      allvals <- ifelse(is.factor(.SD[[i]]), as.character.factor(.SD[[i]]), as.character(.SD[[i]]));
      first <- allvals[1];
      #print(paste0("  first: ", first));
      if (all(is.na(allvals))) {
        #print("   allNA");
        pasted <- NA_character_;
      } else if (!any(is.na(allvals)) & all(allvals==first)){
        pasted <- first;
        #print("all equal to first");
      } else {
        #print("merging!");
        #print(.SD[[i]]);
        pasted <- paste0(allvals, collapse=csep);
        mrg.cnt[i] <- mrg.cnt[i] + 1L;
        #print(pasted);
      }
      #cat("pasted:", pasted);
      rez2 <- c(rez2, list(rep(pasted,.N)));
    } # end for i 1..ncol
    rez2;
  }, by=f_ndx, .SDcols=f_scan]
  if (delold==TRUE) {
    #cat("old names:", names(dtInput), '\n');
    dtInput[,(f_scan):=NULL];  # delete old fields
    setnames(dtInput, newfields, f_scan);# put old names
    dtInput <- dtInput[!duplicated(dtInput)];
    #cat("new names:", names(dtInput), '\n');
  }
  print(mrg.cnt);
  invisible(dtInput);
} # f_end mergerows


mergerows <- function(dtInput, f_ndx, f_scan, csep=";", delold=TRUE) {
  dtInput <- data.table(dtInput); # copy to new table
  if (missing(f_scan)) {f_scan <- setdiff(names(dtInput),f_ndx);} # if fields-to-scan are not defined, we scan all except index
  newfields <- paste0(f_scan, "_S");
  #  cat('Index by:', f_ndx, '; merging fields:\n', f_scan, '\n');
  #mrg.cnt <- 0L;
  dtInput[, (newfields):={
    #print(paste0("N=", .N, "; ncol=", ncol(.SD)));
    rez2 <- list(); # each member of the list will be a column
    #mrg.cnt <- rep(0L, ncol(.SD)); # counter for merges made
    #    cat("N:", .GRP, '; keyval:', .BY[[1]], '.N =', .N, '\n');
    for (i in 1:ncol(.SD)) {
      #      cat("  i:", i, '=',names(.SD)[i]);
      if (.N<2 | all.same(.SD[[i]])) {
        pasted <- .SD[[i]][1];
        pasted <- as.character(pasted);
        #        cat('  equal\n');
      } else {
        #        cat('  diff! Len:', length(.SD[[i]]),'...');
        allvals <- as.character(.SD[[i]]);
        pasted <- paste0(allvals, collapse=csep);
        #        mrg.cnt[i] <- mrg.cnt[i] + 1L;
        #        cat(length(allvals), pasted,'\n');
      } # end of else
      #cat("pasted:", pasted);
      rez2 <- c(rez2, list(rep(pasted,.N)));
    } # end for i 1..ncol
    rez2;
  }, by=f_ndx, .SDcols=f_scan]
  if (delold==TRUE) {
    #cat("old names:", names(dtInput), '\n');
    dtInput[,(f_scan):=NULL];  # delete old fields
    setnames(dtInput, newfields, f_scan);# put old names
    dtInput <- dtInput[!duplicated(dtInput)];
    #cat("new names:", names(dtInput), '\n');
  }
  #  print(mrg.cnt);
  invisible(dtInput);
} # f_end mergerows


paste_or_NA <- function(inp,csep=';'){
  ret <-
  ifelse1(
    all(is.na(inp)),
    inp[1],
    paste(unique(na.omit(inp)), collapse = csep)
  )
  ret
}


mergerows <- function(dtInput, f_ndx, f_scan, csep=";") {
  dtInput <- data.table(dtInput); # copy to new table
  if (missing(f_scan)) {f_scan <- setdiff(names(dtInput),f_ndx);} # if fields-to-scan are not defined, we scan all except index
  newfields <- paste0(f_scan, "_S");
  cat('Index by:', f_ndx, '; merging fields:\n', f_scan, '\n');
  #mrg.cnt <- 0L;
  for (this_col in f_scan){
    dtInput[,(this_col):=paste_or_NA(get(this_col),csep=csep),by=f_ndx]
  }
  invisible(dtInput);
} # f_end mergerows

mergebyrownames <- function(x,y){
  df <- merge(x, y, by='row.names', all=T);
  row.names(df) <- df$Row.names;
  df$Row.names <- NULL;
  df;
}


compcols <- function(inptab1, inptab2){
  commoncols <- intersect(colnames(inptab1), colnames(inptab2));
  diff1 <- setdiff(colnames(inptab1), colnames(inptab2));
  diff2 <- setdiff(colnames(inptab2), colnames(inptab1));
  if (length(diff1)>0) {cat("only in table1:", diff1, "\n");}
  if (length(diff2)>0) {cat("only in table2:", diff2, "\n");}
  for (thiscol in commoncols){
    cat(thiscol, ": ", sep="");
    cat(identical(inptab1[, thiscol], inptab2[, thiscol]), "\n");
  }
}

cleannames <- function(inputnames,
               forbidden = '[^[:alnum:]_]+', replaceTo='.',
               leading='', trailing='', multi=T){
  if (!is.na( forbidden))  inputnames <- gsub(forbidden,     replaceTo, inputnames);
  if (!is.na(leading))  inputnames <- gsub('^\\' %+% replaceTo %+% '+',  leading, inputnames); # just remove leading "."
  if (!is.na(trailing)) inputnames <- gsub( '\\' %+% replaceTo %+% '+$',trailing, inputnames); # just remove trailing "."
  if (multi==T)         inputnames <- gsub( '\\' %+% replaceTo %+% '+', replaceTo,inputnames); # change ".." to "."
  inputnames <- make.names(inputnames);

  inputnames;
}




dtcleannames <- function(dtIn, worknames=names(dtIn),...){
  setnames(dtIn, worknames, cleannames(worknames,...));
  invisible(dtIn);
}

dtgsubnames <- function(dtIn, from, to, ...){
  setnames(dtIn, gsub(from,to,names(dtIn),...));
  invisible(dtIn);
  #print(names(dtIn))
}

# group records by fields defined in 'bys',
# then in each group leaves only records
# having minimal total amount of NAs in fields defined in 'countnames' fields
leave.most.info <- function(dtIn, bys, countnames=NULL, keep=F){
  if (is.null(countnames)) {.countnames <- !(names(dtIn) %in% bys);}
  else .countnames <- (names(dtIn) %in% countnames);
  not.na.count <- !is.na(dtIn[, .countnames, with=F]);
  dtIn[, .tmp_info_score:=apply(not.na.count, 1, sum)]
  dtIn[, '.tmp_delit':={
    delit <- F;
    if (.N>1){
      delit <- (get('.tmp_info_score') < max(get('.tmp_info_score')));
    }
    delit;
  }, by=bys, with=F]
  cat('Deletion: ', sum(dtIn$.tmp_delit==T),'\n');
  #dtIn[, ':='(my.N=.N, my.grp=.GRP), by=bys, with=F]
  if (keep) return(dtIn);
  dtIn <- dtIn[.tmp_delit==F,][,c('.tmp_delit', '.tmp_info_score'):=NULL]
  return(dtIn)
}

# DON'T USE IT!!! Use cSplit() instead!!!
# dt1 <- fread("V1 V2 V3 V4
# a b;cc;d e f
#              c d;e h j
#              x d;e uu kk")
#dtIn=dt1; col2split='V2'; sep=';';
#dtIn=copy(dt.fullprot); col2split='Gene names  (primary )'; sep=';';
# DON'T USE IT!!! Use cSplit() from splitstackshape instead!!!
splitcol2rows <- function(dtIn, col2split, sep, req=T){
  warning("DON'T USE IT!!! Use cSplit() from splitstackshape instead!!!");
  dtOut <- dtIn;
  orinames <- c(names(dtOut)); # must make a copy of names() to avoid its update!
  orikeys  <- key(dtOut);
  if(length(col2split)>1){
    warning('Multiple columns to split!\n');
    for (eachcol in col2split){
      dtOut <- splitcol2rows(dtOut, eachcol, sep, req)
    }# end for
    return(dtOut);
  } # end if
  if (!is.character(col2split)) error('Column name required!');
  if (!(col2split %in% names(dtOut))) error('Column not found: ', col2split);
  dt.split <- dtOut[,
                    .(tmp.add.col=rep(unlist(strsplit(get(col2split),sep,T)), .N)),
                    #('new1'=.N),
                    by=col2split]
  dt.split <- unique(dt.split, by=NULL);
  setkeyv(dt.split, col2split)
  setkeyv(dtOut, col2split)
  dtOut <- dt.split[dtOut,]; # not dtOut[dt.split,] !!!
  dtOut[, c(col2split):=NULL];
  setnames(dtOut, 'tmp.add.col', col2split);
  setcolorder(dtOut, orinames);
  setkeyv(dtOut, orikeys);
  return(dtOut);
}


# read all files and merge into one data.table
mergefiles <- function(fn.list, fill=T, fn.col=NULL, FUN=fread, ...){
  final.dt <- NULL;
  for (fn.this in fn.list){
    this.dt <- FUN(fn.this, ...); # read file with chosen function: fread() by default
    if (is.null(this.dt)) next;
    if (!is.null(fn.col)) this.dt[,(fn.col):=fn.this]; # add column with file name
    if (!is.null(final.dt)) {
      final.dt <- rbindlist(l = list(final.dt, this.dt), use.names = T, fill=fill);
    } else final.dt <- this.dt;
  }
  return(final.dt)
}




mergefiletabs <- function(
  fn.inpdir,      # input directory
  fn.mask='.*',   # files mask
  recursive = T,  # recursive
  fn.list=NULL,   # list of files => fn.inpdir, recursive and fn.mask will be ignored
  full.names = T, # which form of filenames to put in the table
  rn=NULL,        # if not NULL, add rn column with id
  colnames = NULL, # rbind mode: if not NULL, each table names will be set to this
                   # cbind mode: if not NULL, these columns are considered 'constant'
  fn.mask.remove='NULL',
  fn.mask.replace='',
  mode = 'r', # default: rbind (long table), alternative: cbind (wide table)
  ...
  ){

  if (is.null(fn.list))
    fn.list <- list.files(fn.inpdir, fn.mask, include.dirs = FALSE, full.names = TRUE, recursive=recursive, ignore.case = TRUE)

  dt.all <- NULL;
  N <- length(fn.list)
  i <- 0L
  for (fn.this in fn.list){
    i <- i+1L
    cat(' ', i, '/', N,' ', sep = '')
    dt.this <- flexread(fn.this, ...)
    fn.this.show <- ifelse(full.names==T, fn.this, basename(fn.this))
    if (!is.null(fn.mask.remove)) fn.this.show <- gsub(fn.mask.remove,fn.mask.replace,fn.this.show);

    if (mode=='r'){
      if (!is.null(colnames)) {setnames(dt.this, colnames);}
      dt.this[, ffn:=fn.this.show]
      if (!is.null(rn)) {dt.this[, (rn):=seq_len(nrow(dt.this))]}
      dt.all <- rbind(dt.all, dt.this, fill=T)
    } else { # mode == 'c'
      dt.left <- dt.this[,(colnames), with=F]
      dt.right <- dt.this[,-(colnames), with=F]
      if (is.null(dt.all)){
        dt.const <- dt.left
        dt.all <- dt.this
      } else {
        stopifnot(dt.left==dt.const)
        dt.all <- cbind(dt.all, dt.right)
      }

    }
  }
  if (is.null(dt.all)) warning('No data to return!')
  invisible(dt.all);
} # e. mergefiletabs()


flexreadA <- function(fnRead,full.names=T,rn=NULL,colnames=NULL,fn.mask.remove='NULL',fn.mask.replace='', ...){
# flexread() with modifications for mergefiletabs2()
  dt.rez <- flexread(fnRead,...)
  fn.this.show <- ifelse(full.names==T, fnRead, basename(fnRead))
  if (!is.null(fn.mask.remove)) fn.this.show <- gsub(fn.mask.remove,fn.mask.replace,fn.this.show);
  if (!is.null(colnames)) {setnames(dt.rez, colnames);}
  dt.rez[, ffn:=fn.this.show]
  if (!is.null(rn)) {dt.rez[, (rn):=seq_len(nrow(dt.rez))]}
  invisible(dt.rez)
}


mergefiletabs2 <- function(
  fn.inpdir,      # input directory
  fn.mask='.*',   # files mask
  recursive = T,  # recursive
  fn.list=NULL,   # list of files => fn.inpdir, recursive and fn.mask will be ignored
  full.names = T, # which form of filenames to put in the table
  rn=NULL,        # if not NULL, add rn column with id
  colnames = NULL, # rbind mode: if not NULL, each table names will be set to this
                   # cbind mode: if not NULL, these columns are considered 'constant'
  fn.mask.remove='NULL',
  fn.mask.replace='',
  mode = 'r', # default: rbind (long table), alternative: cbind (wide table)
  ...
  ){

  if (is.null(fn.list))
    fn.list <- list.files(fn.inpdir, fn.mask, include.dirs = FALSE, full.names = TRUE, recursive=recursive)

  dt.all <- NULL;
  dt.all <- rbindlist(lapply(fn.list, flexreadA,
                             full.names=full.names,rn=rn,colnames=colnames,fn.mask.remove=fn.mask.remove,fn.mask.replace=fn.mask.replace,...))

  invisible(dt.all);
} # e. mergefiletabs()

# when last column(s) were not defined in the file,
# fread() shifts column names adding V1 at the beginning
# for example, file had header CHROM-POS-REF-ALT,
# and real content was         CHROM-POS-REF-ALT-comment,
# result of fread() will be    V1-CHROM-POS-REF-ALT,
# result of mergefiletabs():   V1-CHROM-POS-REF-ALT-ffn,
# so we run fixLastCol(dt1,colName='comment',addcols=0)
# or        fixLastCol(dtMerged,colName='comment',addcols=1)
fixLastCol <- function(inpDT, colName=NULL, addcols=0){
  if (is.null(colName)) colName <- names(inpDT)[1]
  lastcolnames <- tail(names(inpDT),addcols)
  names(inpDT) <- c(
    names(inpDT)[2:(length(names(inpDT))-addcols)],
    colName,
    lastcolnames
  )
  invisible(inpDT)
}




mergefiletabs.partial <- function(fnInput, mask='*.*', colsHeader=NULL, colValue=-1, separate=F){
  # merge files where part of the columns ("header" columns) are supposed to be the same across all files

  # fnInput - directory or list of files
  # colsHeader - columns describing features (must be the same across all files)
  #              can be column numbers or names
  # colValue - column containing expression value to keep. Can be column number (-1 means last column), or name
  if (length(colValue)!=1) stop('colValue must be of length 1!')
  if (dir.exists(fnInput)) {
    fnInput <- dir(fnInput, mask, full.names = T)
  }

  if (sum(file.exists(fnInput))==0){
    message('No input files found!');
    return(NULL);
  }


  par.dir <- '';
  dt.all <- NULL;
  base.header <- NULL;
  cat('\n ')
  for (this.fn in fnInput){
    this.dir <- dirname(this.fn);
    if (this.dir != par.dir){
      cat(this.dir,'\n '); par.dir <- this.dir;
    }
    this.fn.base <- basename(this.fn)
    cat(this.fn.base)
    if (!file.exists(this.fn)) {cat(' ERROR!\n ');next;}
    this.dt <- fread(this.fn);

    colValue.eff <- colValue;
    if (is.character(colValue)){
      colValue.eff <- which(names(this.dt)==colValue)
      if (length(colValue.eff)>1) {stop('Ambiguous column names!')}
      if (length(colValue.eff)==0) {stop('colValue',colValue, 'not found!')}
    } else if (colValue<0){ # last column
      colValue.eff <- ncol(this.dt)+colValue+1;
    }

    this.values <- this.dt[,(colValue.eff),with=F]
    names(this.values) <- this.fn.base;

    colsHeader.eff <- colsHeader;
    if (is.null(colsHeader)){
      colsHeader.eff <- seq_len(ncol(this.dt)) %-% colValue.eff
    } else if (is.character(colsHeader)){
      colsHeader.eff <- match(colsHeader,names(this.dt))
      if (length(colsHeader.eff)==0) {stop('colsHeader: columns not defined!')}
      if (any(is.na(colsHeader.eff))) {stop('colsHeader: some columns not found!')}
      if (sum(duplicated(colsHeader.eff))) {stop('colsHeader: ambiguous column names!')}
    } else if (any(colsHeader<0)){
      stop('Negative colsHeader not implemented')
    }

    this.header <- this.dt[,(colsHeader.eff),with=F]
    if (is.null(base.header)){
      base.header <- this.header;
    } else {
      if (! base.header %===% this.header) stop('Header columns are not the same!')
    }

    dt.all %<>% cbind(this.values)

    cat('\n ')
  } # e. for


  if (separate==F){
    return(cbind(base.header,dt.all))
  } else return(list(header=base.header, data=dt.all));

} # e. mergefiletabs.partial()




# loadOrBuild - if file exists, then loads the table from it, otherwise re-buils the table ####
# inDT inpuit parameter is a function call, but in fact the function is only evaluated if the file doesn't exist => saves time
loadOrBuild <- function (fnDT, inDT, saveResult=TRUE, ...){
  nm.load <- c(names(formals(fread)), names(formals(loadDT)));
  nm.save <- c(names(formals(write.table)), names(formals(save_DT)));
  dots <- list(...);
  if (file.exists(fnDT)){
    cat('File found:', fnDT, '; will load it instead of building a new table.\n');
    return(
      do.call('loadDT',
              c(
                list(fnInput = fnDT),
                dots[names(dots) %in% nm.load]
              )
      ) # instead of  loadDT(fnDT, ...)
    );
  } else {
    cat('File not found:', fnDT, '; we\'ll build new table.\n');
    inDT <- eval(inDT);
    if (saveResult){
      do.call('save_DT',
              c(list(dtIn=inDT, fnSaveTo = fnDT),
                dots[names(dots) %in% nm.save])
      ) # instead of  save_DT(inDT, fnDT, ...);

    }
    return(inDT);
  }
}

# loadDTlazy: loads dt from file only if it is not defined yet ####
# previously known as load_DT, should be replaced everywhere
loadDTlazy <- function(dtVar, fnVar=NULL, fnDef, refresh=T, colsExcl=NULL, ...) {
  varnameDT <- deparse(substitute(dtVar));
  varnameFN <- deparse(substitute(fnVar));
  newfn <- "";
  if ((exists(varnameDT) | varnameDT=='.') & refresh!=T) {
    cat("DT (", varnameDT, ") already exists!\n");
    dtOut <- dtVar;# copy(dtVar) doesn't work :/
  } else {
    if (refresh==T) {cat("Existing DT (", varnameDT, ") ignored! ");} else {cat("DT (", varnameDT, ") does not exist! ");}
    if (exists(varnameFN)) {
      cat("filename variable (", varnameFN,") is defined! \n");
      newfn <- fnVar;
    } else {
      cat("filename (", varnameFN,") was not defined before! \n");
      if (!missing(fnDef)) {newfn <- fnDef;} else {cat("Default filename not set!\n")};
    }
    dtOut <- loadDT(fnInput = newfn, colsExcl=colsExcl, ...);
  }
  return(dtOut);
}


# setnamessp ####################################
# usual setnames() requires that all old names are present in the table,
# setnamessp() tolerates missing names
setnamessp <- function(dtIn, old, new, verbose=T){
  foundOld <- old %in% names(dtIn);
  newDups <- new[foundOld] %in% names(dtIn); # if old_col is supposed to be renamed to new_col while new_col already exists in dtIn
  if (sum(newDups)>0) warning('Warning! those column already existed: ', new[newDups]);
  if (sum(foundOld)>0){
    setnames(dtIn, old[foundOld], new[foundOld]);
    if (verbose) {cat(sum(foundOld), ' names changed:\nFrom:', old[foundOld], '\n  to:', new[foundOld], '\n');}
  }
  invisible(dtIn);
}



# combines all values from selected columns and returns set of unique
getlevels <- function(dtIn, cols){
  ret.levels <- NULL;
  for (this.col in cols){
    ret.levels <- c(ret.levels,
                    ifelse1(is.factor(dtIn[[this.col]]),
                            levels(dtIn[[this.col]]),
                            unique(dtIn[[this.col]]))
    )

  } # end for
  return(unique(ret.levels));
}

cnames <- function(dtIn) return(c(names(dtIn))); # copy names() to avoid updates


setfirstcol <- function(dtIn, firstcols){
  missingcols <- firstcols %-% names(dtIn)
  if (length(missingcols)>0){
    warning('These columns are missing and will be ignored: ',missingcols);
  }
  firstcols %<>% intersect(names(dtIn));
  setcolorder(dtIn, c(firstcols, (names(dtIn) %-% firstcols))  )
}

setlastcol <- function(dtIn, lastcols){
  missingcols <- firstcols %-% names(dtIn)
  if (length(missingcols)>0){
    warning('These columns are missing and will be ignored: ',missingcols);
  }
  lastcols %<>% intersect(names(dtIn));
  setcolorder(dtIn, c((names(dtIn) %-% firstcols), lastcols))
}

setcolorderV <- function(dtIn, newcols, ellipsis='...'){
  ell.pos <- match(ellipsis, newcols)
  if (!is.na(ell.pos)){
    firstcols <- newcols[1:(ell.pos-1)]
    lastcols  <- newcols[(ell.pos+1):length(newcols)]
    newcols <- c(firstcols,lastcols)
    missingcols <- newcols %-% names(dtIn)
    firstcols %<>% intersect(names(dtIn));
    lastcols  %<>% intersect(names(dtIn));
    newcols   %<>% intersect(names(dtIn));
  } else {
    missingcols <- newcols %-% names(dtIn)
    firstcols  <- intersect(newcols, names(dtIn))
    lastcols <- NULL
  }

  if (length(missingcols)>0){
    warning('These columns are missing and will be ignored: ',paste(missingcols, collapse = ' '));
  }

  setcolorder(dtIn, c(firstcols, (names(dtIn) %-% newcols), lastcols))
}


names.comm <- function(dt1,dt2) {return(intersect(names(dt1),names(dt2)))}
names.diff <- function(dt1,dt2) {return(setdiff(names(dt1),names(dt2)))}

findnamesrange <- function(inpDT,name1,name2, values=F){
  col1 <- which(names(inpDT)==name1)
  col2 <- which(names(inpDT)==name2)
  if (values==T) {return(names(inpDT)[col1:col2]);}
  else return(col1:col2);
}



cast.fun <- function(inp.dt, cols2cast=names(inp.dt), FUN, ...){
  cols2castY <- intersect(cols2cast,names(inp.dt))
  if (!identical(cols2castY,cols2cast)) warning('Columns not found: ', paste0(setdiff(cols2cast,names(inp.dt)), collapse = ' '),'\n')
  inp.dt[, (cols2castY) := lapply(.SD, FUN, ...), .SDcols = cols2castY]
  return(inp.dt);
}

cast.char <- function(inp.dt, cols2cast=names(inp.dt)){
  return(cast.fun(inp.dt,cols2cast,as.character));
}

cast.num <- function(inp.dt, cols2cast=names(inp.dt)){
  return(cast.fun(inp.dt,cols2cast,all2num));
}

cast.factor <- function(inp.dt, cols2cast=names(inp.dt), ...){
  return(cast.fun(inp.dt,cols2cast,as.factor, ...));
}

cast.date <- function(inp.dt, cols2cast=names(inp.dt), ...){
  return(cast.fun(inp.dt,cols2cast,as.Date, ...));
}


each.row <- function(inp.dt) seq_len(nrow(inp.dt));


compare.df <- function(df1,df2) {
  stopifnot(dim(df1)==dim(df2))
  for (i in seq_len(dim(df1)[2])){
    if (! identical(df1[,i], df2[,i])) {
      not.eq <- sum(df1[,i] != df2[,i], na.rm = T)
      cat(i, colnames(df1)[i],not.eq, '\n')
    }
  }
}


split_vers <- function(inpDT, col_format, col_content, sep=':'){
  for (this.format in unique(inpDT[[col_format]])){
    inpDT[get(col_format)==this.format, unlist(strsplit(this.format,sep)):=tstrsplit(get(col_content),sep)]
  }
  invisible(inpDT)
}

split_vers2 <- function(inpDT, col_format, col_content, sep=':', prefix=''){
  allnewnames <- c()
  for (this.format in unique(inpDT[[col_format]])){
    newnames <- unlist(strsplit(this.format,sep))
    allnewnames <- unique(c(allnewnames, newnames))
    inpDT[get(col_format)==this.format, (newnames):=tstrsplit(get(col_content),sep)]
  }
  setnames(inpDT, allnewnames, paste0(prefix, allnewnames))
  invisible(inpDT)
}



shrink.col <- function(inpDT, cols, sep=';'){
  # 'chrX;chrX;chrX;chrX' => 'chrX'
  for (this.col in cols){ # this.col='Chr'
    inpDT[, c(this.col):=paste0( unique(unlist(strsplit(get(this.col),sep,fixed = T))), collapse = sep) , by=c(this.col)]
  }
  invisible(inpDT)
}


shrink_cols <- function(inpDT, col_by, cols=setdiff(names(inpDT),col_by), sep=';', ...) {
  for (this.col in cols){ # this.col='Chr'
    if (this.col %!in% names(inpDT)) {warning(' Column ',this.col, ' not found within names of input table. '); next;}
    inpDT[, c(this.col):=shrink_values(get(this.col), ...), by=c(col_by)]
  }
  invisible(inpDT)
}

dt_addcols <- function(inpDT, cols, defval=NA){
# adding column if it is not in the table yet
# dt_addcols(dt, cols = cs('colA colB colC')) - adds columns as NA
# dt_addcols(dt, cols = c(colA=NA_integer, colB=NA_real, 'colC') )
  if (is.null(names(cols))){ # cols = cs('colA colB colC')
    colnamesA <- cols
    colvalsA  <- ifelse(length(defval)==1, rep(defval,length(colnamesA)), defval)
  } else {                   # cols = c(colA=NA_integer, colB=NA_real, 'colC')
    colnamesA <- names(cols)
    colvalsA  <- cols
    empty <- (names(cols)=='')
    colnamesA[empty] <- cols[empty]
    colvalsA[empty]  <- defval
  }

  for (i in seq_along(colnamesA)){
    this.colname <- colnamesA[i]
    this.colval  <- colvalsA[i]
    if (this.colname %in% names(inpDT)) next;
    inpDT[[this.colname]] <- this.colval
  }
  invisible(inpDT)
}

# getfldFrom <- 'aaa; level 32; transcript_support_level "4";'

extract.fld <- function(inpDT,fldFrom,fldTo,regex1,regex2='\\1',regex3='',pos=1L,remove=T){
  str.wide <- paste0('.*',regex1,'.*')
  inpDT[grepl(regex1,get(fldFrom)), (fldTo):=gsub(str.wide, regex2, get(fldFrom))]
  if (remove==T){
    inpDT[grepl(regex1,get(fldFrom)), (fldFrom):=gsub(regex1, regex3,get(fldFrom))]
  }

  invisible(inpDT)
}


setDF_my <- function(inpDT, col2rownames){
  setDF(inpDT)
  row.names(inpDT) <- inpDT[[col2rownames]]
  invisible(inpDT)
}


# merge more than 2 data.tables:
mergemulti <- function(dlist,key,...){
  if (length(dlist)<2) return(dlist);
  dt.rez <- dlist[[1]];
  for (i in 2:length(dlist)){
    dt.next <- dlist[[i]]
    dt.rez <- merge.data.table(dt.rez,dt.next,by = key,...)
  }
  return(dt.rez)
}




get_top_via_ranks <- function(inpDT,colVal,inpConditions,rankNum=5L,side=c('top','btm')) {
  for (this.condition in inpConditions){
    if ('top' %in% side){
      inpDT[eval(parse(text = this.condition)),newRankCol:=frank(-get(colVal), ties.method =  'average')]
      #eff.thr <- ifelse(rankNum>min(inpDT$newRankCol,na.rm=T),rankNum,)
      inpDT[newRankCol<=rankNum,selected:=TRUE]
    }
    if ('btm' %in% side){
      inpDT[eval(parse(text = this.condition)),newRankCol:=frank(get(colVal), ties.method =  'average')]
      inpDT[newRankCol<=max(rankNum,min(newRankCol)),selected:=TRUE]
    }
  }
  inpDT[,newRankCol:=NULL]
  return(inpDT)
}

get_top_via_headtail <- function(inpDT,colVal,inpConditions,rankNum=5L,side='both') {
  ht <- function(x, n=5L) unique(c(head(x, n), tail(x, n)))
  inpDT[, rn := .I]
  for (this.condition in inpConditions){
    if (side=='top') {
      inpDT[rn %in% inpDT[order(get(colVal)),head(rn[eval(parse(text = this.condition))], rankNum)],selected:=TRUE]
    } else if (side=='btm'){
      inpDT[rn %in% inpDT[order(get(colVal)),tail(rn[eval(parse(text = this.condition))], rankNum)],selected:=TRUE]
    } else inpDT[rn %in% inpDT[order(get(colVal)),ht(rn[eval(parse(text = this.condition))], rankNum)],selected:=TRUE]
  }
  return(inpDT)
}



# search for pair of columns with identical names and delete one if equal
del.dupflds.dupnames <- function(inpDT, verbose=T){
  for (this.f in names(inpDT)){
    this.is <- which(this.f == names(inpDT)); # indices of occurrences
    if (length(this.is)>1) {
      if (verbose) cat('\n', length(this.is), this.f);
      if (all(inpDT[,(this.is[1]), with=F] == inpDT[,(this.is[2]), with=F])){
        if (verbose) cat('!!!');
        set(inpDT, , this.is[2], NULL)
        if (verbose) cat('+');
      }
    }# e. if >1
  } # e. for
 invisible(inpDT)
}



# given a field with non-unique IDs,
# selects fields which are unique multiple within same ID, i.e. can't be "reduced"
# and fields that can be "reduced"
# usage:
# g(dt.patients, dt.plasmas) %<<% dt_normalize(dt1a.a, 'pID')
dt_normalize <- function(inDT, key, verbose=F, nCol=NULL){ #inDT=dt.PMCC; key='Patient_ID';
  cols.gen <- c()
  cols.unq <- c()

  for (this.f in names(inDT) %-% key){ # this.f='Primary_Institute_Patient_ID'
    this.subdt <- inDT[,.(xN=.N, xU=nrow(unique(.SD))), by=key, .SDcols=this.f]

    if (all(this.subdt[,xU==1])){
      cols.gen <- c(cols.gen, this.f)
    } else {
      cols.unq <- c(cols.unq, this.f)
    }
  }

  cat('\nGen: \n', paste(cols.gen, collapse = '\n '))
  cat('\n\nUnq: \n', paste(cols.unq, collapse = '\n '))

  dt.master <- inDT[,c(key,cols.gen), with=F];
  if (!is.null(nCol)){
    dt.master[,myNewNumField:=.N,by=key]
    setnames(dt.master,'myNewNumField',nCol)
  }
  dt.master <- unique(dt.master);

  dt.detail <- inDT[,c(key,cols.unq), with=F]

  invisible(list(dt.master=dt.master, dt.detail=dt.detail))

}

# renames duplicated column names
# MyColumn MyColumn -> MyColumn.1 MyColumn.2
dedup.colnames <- function(dtIn, sep='.'){
  inpnames <- names(dtIn)
  if (sum(duplicated(inpnames))==0) {message('No duplicate names;'); invisible(dtIn);}
  dupnames <- unique(inpnames[duplicated(inpnames)]);
  for (dupname in dupnames){
    positions <- which(inpnames==dupname);
    newnames <- paste0(dupname,sep,seqlen(positions))
    names(dtIn)[positions] <- newnames
  }
  invisible(dtIn);
}



dt_del_columns <- function(inpDT, names=NULL, re=NULL){
  if (!is.null(names)){
    inpDT[, c(names):=NULL]
  }

    if (!is.null(re)){
    inpDT[, grep(re,names(inpDT),value = T):=NULL]
  }


  invisible(inpDT)

}


# used in 3=Cervantes.Rmd
dt_datify <- function(dtInp, cols, tryFs=NA){
  for (this.col in cols){
    dtInp[, (this.col):=as.character(as.Date(get(this.col), tryFormats=tryFs)), by=get(this.col)]
    dtInp[, (this.col):=as.Date(get(this.col))]
  }
  invisible(dtInp)
}



build_cum_table <- function(inp_dt_sum,colDate='earliestBlood',colType='Type'){
  dt.work <- copy(inp_dt_sum)
  dt.work %<>% setnames(c(colDate,colType), c('internal__Date','internal__Type'))
  dt.work %<>% setorder(internal__Date,internal__Type)
  dt.work[,n1:=seqlen(.N),by=internal__Type] # by=Type
  dt.work[,n2:=.I,        by=internal__Type] # by=Type


  dt1 <- dt.work[,.(internal__Date,internal__Type,n1)]

  dt2 <- dcast(dt1, internal__Date ~ internal__Type, value.var='n1', fun.aggregate = maxI)

  dt3 <- melt(dt2, id.vars = 'internal__Date', variable.name = 'internal__Type', value.name = 'xCases')
  dt3[,Cases:=nafill(xCases,'locf'),by=internal__Type]
  dt3[is.na(Cases),Cases:=0]

  dt3 %<>% setnames(c('internal__Date','internal__Type'),c(colDate,colType))

  invisible(dt3)
}

  # cat.N <- c(Gender='Gender',
  #            Cancer.Stage='Overall pathologic stage',
  #            hadNeoB='Neoadjuvant therapy given',
  #            Resectability='Resectable status'
  #            # stageT='Pathologic T stage',
  #            #Location='Cancer location'
  # )

build_stat_table_N <- function(inpDT,categories){
  dt.N <- NULL
  for (this.cat in names(categories)){
    this.label <- categories[[this.cat]]
    this.vals  <- inpDT[[this.cat]]
    if (is.null(this.vals)) {message(' Not found: ', this.cat, ' - ', this.label); next;}
    this.stat  <- tab(this.vals, do.sort = F)
    this.title <- data.table(Category=this.label)
    this.tab   <- rbind(this.title, data.table(Value=this.stat$this.vals, N=this.stat$Freq, `%`=this.stat$FreqP), fill=T)
    dt.N %<>% rbind(this.tab, fill=T)
  }

  dt.N[is.na(Value) & is.na(Category),Value:='N/A']
  invisible(dt.N)
}

build_stat_table_med <- function(inpDT,categories){
  dt.med <- NULL
  for (this.cat in names(categories)){
    this.label <- categories[[this.cat]]
    this.vals  <- inpDT[[this.cat]]
    if (is.null(this.vals)) {message(' Not found: ', this.cat, ' - ', this.label); next;}
    this.med <- median(this.vals, na.rm=T)
    this.sd  <- sd(this.vals, na.rm = T)
    this.rng <- range(this.vals, na.rm = T)

#    this.title <- data.table(Category=this.label)
#    this.tab   <- rbind(this.title, data.table(Value=this.stat$this.vals, N=this.stat$Freq, `%`=this.stat$FreqP), fill=T)
    dt.med %<>% rbind(data.table(Category=this.label, Median=round(this.med, 2), SD=round(this.sd,2), range=paste(round(this.rng,2),collapse = ' .. ')), fill=T)
  }

  invisible(dt.med)
}

# usage:
# relabel(dt.mosaic,
# list(cs('postMRD,postMRD_ctDNA_positivity,Negative,Positive'),
#                cs('Relapsed,Recurred,Not recurred,Recurred') ) )
relabel <- function(inpDT,inpList){
  for (item in inpList){
    colFrom <- item[1]
    colTo   <- item[2]
    newLevels <- item[-(1:2)]
    message('Renaming ',colFrom,' to ',colTo,' with levels: ',newLevels)
    #inpDT[, newCol:=get(colFrom)]
    inpDT$newCol <- inpDT[[colFrom]]
    setnames(inpDT,colFrom, colTo)
    setnames(inpDT,'newCol',colFrom)
    if (length(newLevels)>0) levels(inpDT[[colTo]]) <- newLevels
  }
  return(inpDT)
}



merge_version_tables <- function(dt1, dt2, key.x, key.y=key.x, all=T){
  dups <- names(dt1) %&% names(dt2)

  dt.bad.wide <- data.table(keycol='')[0]  %>% setnames('keycol',key.x)
  dt.bad.long <- data.table(keycol='')[0]  %>% setnames('keycol',key.x)

  dt.merged <- merge(dt1, dt2, by.x=key.x, by.y=key.y, all=all)

  for (.dup in (dups %-% c(key.x,key.y))){
    .dupX <- .dup %+% '.x'
    .dupY <- .dup %+% '.y'
    cat('\n',.dup)
    this.dt <- dt.merged[,c(key.x,.dupX,.dupY),with=F]
    this.dt[, isNA.x:=is.na(get(.dupX))]
    this.dt[, isNA.y:=is.na(get(.dupY))]
    this.dt[, equal:=(get(.dupX)==get(.dupY))]
    this.tab <- tab(this.dt[,.(isNA.x,isNA.y,equal)])
    this.bad <- this.dt[equal==F,]
    this.bad[, cs('isNA.x isNA.y equal'):=NULL]
    if (nrow(this.bad)>0) {
      dt.bad.wide %<>% merge(this.bad, by=key.x, all=T)

      this.bad.long <- copy(this.bad)
      this.bad.long[,field:=.dup]
      names(this.bad.long) %<>% gsub(.dup,'',.)
      this.bad.long %<>% cast.char(cs('.x .y'))
      dt.bad.long %<>% rbind(this.bad.long, fill=T)

      cat('\t', nrow(this.bad))

    } # e. if (nrow(this.bad)>0)

    dt.merged[,c(.dup):=get(.dupY)]
    dt.merged[is.na(get(.dupY)),c(.dup):=get(.dupX)]
    dt.merged[,c(.dupX,.dupY):=NULL]

    names(this.tab) %<>% gsub('isNA',.dup,.)
    #  print(this.tab[])
  } # e. for


  # print(dim(dt.bad.wide))
  # print(dim(dt.bad.long))

  dt.bad.wide <<- dt.bad.wide
  dt.bad.long <<- dt.bad.long

  invisible(dt.merged)
}


mergeR <- function(dt1, dt2, ...){
  warning('Function not tested thoroughly!')
  warning('resulting table is re-keyed!')
  argsList <- list(...)
  names.ovl <- names(dt1) %&% names(dt2) %-% c(argsList$byX, argsList$by)
  if (length(names.ovl)>0){
    message('Columns to delete and replace: ', paste(names.ovl, collapse = ', '))
    dt1[,c(names.ovl):=NULL]
  }
  return(merge(dt1,dt2,...))
}
