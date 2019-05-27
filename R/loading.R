testX <- function(){
  print('Running! 2');
}

catV <- function(..., verbose=T){
  if (verbose) return;
  cat(...);
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


reqS <- function(packagename, verbose=T, tryBioconductor=T){
  catV <- ifelse(verbose,cat,function(...){})
  stopifnot(length(packagename)==1L);
  stopifnot(class(packagename)=='character');

  catV('Loading',packagename,'...')

  # first, check if the package is installed at all:
  if(packagename %in% rownames(installed.packages())) {
    catV('already installed... ')
  } else {
    catV('Not installed! Trying to install... \n');
    install.packages(packagename);
    catV("installation finished, checking... ");
    if(packagename %in% rownames(installed.packages()) == FALSE) {
      if (tryBioconductor==T) {
        catV(' Trying from Bioconductor... ')
        if (!exists('biocLite')) source("https://bioconductor.org/biocLite.R");
        biocLite(packagename);
       } # e.tryBioconductor
    }

    if(packagename %in% rownames(installed.packages()) == FALSE){
      warning('Still failed!\n'); return(FALSE);
    }
  } # e. else

  # now check if it is already loaded:
  if (paste0('package:',packagename) %in% search()) {
    catV('already loaded!\n'); return(TRUE);
  }

  # if it is installed but not loaded:
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
  ); # e. tryCatch()

  catV('\ntryrez: ', tryrez, 'reqrez: ', reqrez, '\n');
  #if (is.null(reqrez)) reqrez <- FALSE;

  if (tryrez) {catV("Success!\n"); return(T);}
  if (tryrez==F) {cat("Failed!\n"); return(F);}

}


req <- function(packagename, verbose=T, tryBioconductor=T){
  catV <- ifelse(verbose,cat,function(...){})
  catV('\n=======================================================\n');
  pkname.subs <- substitute(packagename);
  catV('Loading [',class(pkname.subs),']',sep='')
  if (class(pkname.subs)=='name') {
    catV(" named variable ");
    if (exists(deparse(pkname.subs))){
      cat("exists");
      packagename <- eval.parent(pkname.subs);

      if (!is.character(packagename)){
        catV(' but is not character. ')
        packagename <- deparse(pkname.subs);
      }

    } else {
      cat(" doesn't exist");
      packagename <- deparse(pkname.subs);
    }
    #,);
  }
  #if (class(pkname.subs)!='character') packagename <- deparse(pkname.subs);
  catV(' [',packagename,']\n',sep=' ')

  if (length(packagename)>1) { # recursively process vector/list
    catV('Requested', length(packagename), 'packages:', paste0(packagename, collapse = ','),'\n');
    ndx <- 0L;
    for (eachpackagename in packagename) {
      ndx <- ndx + 1L;
      ndx_s <- paste0('(', ndx, ')');
      catV(ndx_s);
      reqS(eachpackagename);
    }
    catV("Finished loading", length(packagename), "packages.\n");
    return(T);
  }

  packagename <- unlist(strsplit(packagename, " ", fixed=T));
  if (length(packagename)>1) {
    catV('Splitting package name:', length(packagename), "names.\n");
    req(packagename);
    return(T);
  }

  reqS(packagename,verbose = verbose,tryBioconductor=tryBioconductor);
} # e. req()

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
  reqS(pkgName);
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

loadv <- function(file=NULL, ...){
  if (is.null(file)) {file <- askfilename();}
  load(file, verbose=T, envir = parent.frame(n=1L), ...)
}


loadv1 <- function(fnRdat, verbose=T){
  obj.names <- load(fnRdat, verbose=verbose)
  if (length(obj.names)>1) warning('Multiple objects loaded! Only first one will be returned.')
  obj.name <- obj.names[1]
  obj.ret <- get(obj.name)
  invisible(obj.ret)
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

