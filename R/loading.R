testX <- function(){
  print('Running! 2');
}



# from list of locations, tries to load source
loadAny <- function(...){
  arglist <- unlist(list(...));
  cat(' ', length(arglist), 'locations provided.\n')
  ndx <- 0L;
  for (arg in arglist){
    ndx <- ndx + 1L;
    ndx_s <- paste0(' [', ndx, ']');
    cat(ndx_s, 'Trying to load ', arg, '...\n');

    rez.src <- '';
    tryCatch(
      expr    = {rez.src <<- source(arg);},
      warning = function(w){rez.src <<- (paste0('Warrning: ', w$message, '\n'));},
      error   = function(e){rez.src <<- (paste0('Errrrror: ',   e$message, '\n'));},
      finally = {cat(rez.src);}
    );

    if (rez.src=='') {cat('Sourced successfully! (',arg,')\n'); return(T);} else
      if (grepl('Warrning: package .* was built under R version .*', rez.src)) {cat('Sourced with minor warning. (',arg,')\n'); return(T);} else
        cat('Sourced unsuccessfully! (',arg,')\n');
  } # e. for

  stop('All source() calls failed!')

} # e. loadAny()


req <- function(packagename, verbose=T){
  if (length(packagename)>1) { # recursively process vector/list
    if (verbose) cat('Requested', length(packagename), 'packages:', paste0(packagename, collapse = ','),'\n');
    ndx <- 0L;
    for (eachpackagename in packagename) {
      ndx <- ndx + 1L;
      ndx_s <- paste0('(', ndx, ')');
      if (verbose) cat(ndx_s);
      req(eachpackagename);
    }
    if (verbose) cat("Finished loading", length(packagename), "packages.\n");
    return(T);
  }

  packagename <- unlist(strsplit(packagename, " ", fixed=T));
  if (length(packagename)>1) {
    if (verbose) cat('Splitting package name:', length(packagename), "names.\n");
    req(packagename);
    return(T);
  }

  if (verbose) cat(" Checking package ", packagename, "... ");
  if(packagename %in% rownames(installed.packages()) == TRUE) {
    if (verbose) cat('already installed... ')
  } else {
    if (verbose) cat('Not installed! Trying to install... \n');
    install.packages(packagename);
    if (verbose) cat("installation finished, checking... ");
    if(packagename %in% rownames(installed.packages()) == FALSE) {warning('Still failed!\n'); return(FALSE);}
  }

  if (paste0('package:',packagename) %in% search()) {
    if (verbose) cat('already loaded!\n'); return(TRUE);
  }

  reqrez <- TRUE;
  tryrez <- tryCatch(
    expr    = {reqrez <<- require(packagename, character.only = T);},
    warning = function(w){
      cat('Warning: ', w$message, '\n');
      if (grepl('there is no package called', w$message)) {
        reqrez <<- FALSE;
      } else {reqrez <<- require(packagename, character.only = T);} # very ugly...
    },
    error   = function(e){
      cat('Error: ',   e$message, '\n');
      reqrez <<- FALSE;
    },
    finally = cat("finished with", packagename, ".\n")
  );
  if (verbose) cat('\ntryrez: ', tryrez, 'reqrez: ', reqrez, '\n');
  #if (is.null(reqrez)) reqrez <- FALSE;

  if (reqrez) {if (verbose) cat("Success!\n"); return(T);}
}

reload <- function(pkgName){
  pkgNameP <- paste0('package:',pkgName);
  print(pkgNameP);
  if(pkgNameP %in% search())
  {
    #pkgNameP <- "package:ggplot2";
    print('package found! will be detached. v5');
    #detach(name = pkgNameP, unload=TRUE);
    unloadNamespace(pkgName);
    }
  req(pkgName);
}



# trylocs:
# for given list of files, try (one-by-one) if they exist;
# returns either first existing (with all=FALSE, default)
# or all existing (with all=TRUE)
# if none found, shows warning or error (with req=TRUE)
trylocs <- function(..., req=F, all=F){
  arglist <- unlist(list(...));
  rez <- c()
  for (elem in arglist){
    if (file.exists(elem)) {
      if (all==F) return(elem) else {rez <- c(rez, elem);}
    }
  }
  if (length(rez)>0) return(rez);
  warning('None of the locations found!')
  if (req==T) error('None of the locations found!')
  return(NULL);
}


getvloc <- function(){
  locs <- list();
  locs['~/../AppData/Roaming/locconfig/dell'] <- 'dell';
  locs['~/../AppData/Roaming/locconfig/T560'] <- 'T560';
  locs['~/../AppData/Roaming/locconfig/helix'] <- 'helix';
  locs['~/../AppData/Roaming/locconfig/atran'] <- 'atran';

  for (loc in names(locs)){
    if (file.exists(loc)) {return(locs[[loc]]);}
  }
  return("");
}


req('data.table magrittr');

vai <- function(){
  req('data.table magrittr');
}

