tryRdat1 <- function(fnRdat, lazyobj, objname='result', saveResult=TRUE){
# This function tries to load data from the indicated Rdat file
# if it doesn't exist, it will call indicated function, and save the results to the file
  cat('Checking pre-existing Rdat file:', fnRdat, '... ');
  if (file.exists(fnRdat)) {
    cat(' found. ');
    result <- loadv1(fnRdat);
  } else {
      cat(' not found. Will be generated.\n');
      result <- lazyobj;
      assign(value = result, x=objname);
      if (saveResult==T) save(list = objname, file = fnRdat);
    }

  invisible(result);
} # e. try_rdat
