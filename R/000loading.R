requireNamespace("magrittr")


catV <- function(..., verbose=T){
  if (!verbose) return;
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


reqS <- function(packagename, verbose=T, tryBioconductor=T, reload=F){
  catV <- ifelse(verbose,cat,function(...){})
  stopifnot(length(packagename)==1L);
  stopifnot(class(packagename)=='character');

  catV('Loading',packagename,'...')

  # first, check if the package is installed at all:
  if(packagename %in% rownames(utils::installed.packages())) {
    catV('already installed... ')
  } else {
    catV('Not installed! Trying to install... \n');
    install.packages(packagename);
    catV("installation finished, checking... ");
    if(packagename %in% rownames(installed.packages()) == FALSE) {
      if (tryBioconductor==T) {
        catV(' Trying from Bioconductor... ')
        #if (!exists('biocLite')) source("https://bioconductor.org/biocLite.R");
        #biocLite(packagename);
        if (!requireNamespace("BiocManager", quietly = TRUE))
          install.packages("BiocManager")
        BiocManager::install(packagename);
       } # e.tryBioconductor
    }

    if(packagename %in% rownames(installed.packages()) == FALSE){
      warning('Still failed!\n'); return(FALSE);
    }
  } # e. else

  # now check if it is already loaded:
  if (paste0('package:',packagename) %in% search()) {
    catV('already loaded.\n'); return(TRUE);
  }

  # if it is installed but not loaded:
  tryrez <- tryCatch(
    expr    = {require(packagename, character.only = T);},
    warning = function(w){
      cat('Warning raised: ', w$message, '\n');
      if (grepl('there is no package called', w$message)) {
      } else {require(packagename, character.only = T);} # very ugly...
    },
    error   = function(e){
      cat('Error occurred: ',   e$message, '\n');

      if (grepl('please re-install it', e$message)) {
        install.packages(packagename);
        require(packagename, character.only = T);
      }

    },
    finally = {cat(" Finished with", packagename, ".\n");}
  ); # e. tryCatch()

  if (tryrez) {catV("Success!\n"); return(T);}
  if (tryrez==F) {cat("Failed!\n"); return(F);}

  if (reload==T){
    message('\nReloading vautils...\n')
    unloadNamespace('vautils');
    require(vautils);
  }

}

#' Install (if needed) and attach one or more packages
#'
#' Convenience wrapper that accepts a package name as a symbol (e.g. `dplyr`),
#' a single character string (e.g. `"dplyr"`), a space-separated string
#' (e.g. `"dplyr ggplot2"`), or a character vector (e.g. `c("dplyr","ggplot2")`),
#' and ensures each package is installed and attached. If a package is not
#' available from CRAN and `tryBioconductor = TRUE`, a Bioconductor install is
#' attempted via **BiocManager**.
#'
#' When multiple packages are supplied and `reload = TRUE`, the function
#' unloads and re-attaches **vautils** at the end (to refresh its search path
#' after attachments).
#'
#' @section Important:
#' This helper is intended for **interactive scripts**. Do **not** use it inside
#' package code; packages should declare dependencies in `DESCRIPTION` and use
#' `Imports`/`NAMESPACE` (e.g., `pkg::fun` or `@importFrom`), not attach packages
#' at runtime.
#'
#' @param packagename A package identifier provided as a symbol, a character
#'   string, a space-separated string of names, or a character vector of names.
#'   Internally, symbols are normalized to character names. Space-separated
#'   strings are split and processed element-wise.
#' @param verbose `logical(1)`. If `TRUE`, prints progress messages.
#' @param tryBioconductor `logical(1)`. If `TRUE`, try installing from
#'   Bioconductor (via **BiocManager**) when CRAN install fails.
#' @param reload `logical(1)`. If `TRUE` and more than one package was processed,
#'   `vautils` is reloaded (`unloadNamespace("vautils")` then `require(vautils)`).
#'
#' @return `logical(1)`. For a single package, returns the success status from
#'   the underlying loader (`TRUE` if attached, `FALSE` otherwise). For multiple
#'   packages, returns `TRUE` after processing (individual failures are reported
#'   during the run).
#'
#' @seealso [BiocManager::install()], [library()], [require()]
#'
#' @examples
#' \dontshow{old_opt <- options(warn = 1); on.exit(options(old_opt), add = TRUE)}
#' # Interactive usage (do not run in package examples to avoid installs):
#' \dontrun{
#'   # Symbol, single name
#'   reqq(dplyr)
#'
#'   # Character string
#'   reqq("dplyr")
#'
#'   # Space-separated string
#'   reqq("dplyr ggplot2", verbose = TRUE)
#'
#'   # Character vector; skip vautils reload at the end
#'   reqq(c("dplyr", "ggplot2"), reload = FALSE)
#'
#'   # Try Bioconductor if not on CRAN
#'   reqq("BiocGenerics", tryBioconductor = TRUE)
#' }
#'
#' @export
reqq <- function(packagename, verbose=F, tryBioconductor=T, reload=T){
  catV <- ifelse(verbose,cat,function(...){})
  catV('\n=======================================================\n');
  pkname.subs <- substitute(packagename);
  catV('Loading [',class(pkname.subs),']',sep='')
  if (class(pkname.subs)=='name') {
    catV(" named variable ");
    if (exists(deparse(pkname.subs))){
      catV("exists");
      packagename <- eval.parent(pkname.subs);

      if (!is.character(packagename)){
        catV(' but is not character. ')
        packagename <- deparse(pkname.subs);
      }

    } else {
      catV(" doesn't exist");
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
      reqS(eachpackagename, reload=F);
    }
    catV("Finished loading", length(packagename), "packages.\n");

    if (reload==T){
      unloadNamespace('vautils');
      require(vautils);
    }

    return(T);
  } # e. if (length>1)

  packagename <- unlist(strsplit(packagename, " ", fixed=T));
  if (length(packagename)>1) {
    catV('Splitting package name:', length(packagename), "names.\n");
    reqq(packagename,verbose = verbose,tryBioconductor=tryBioconductor, reload=reload);
    return(T);
  }

  reqS(packagename, verbose=verbose, tryBioconductor=tryBioconductor, reload=reload);
} # e. reqq()

#' Detach (if attached) and re-attach a package by name
#'
#' Convenience helper that checks whether a package is currently **attached**
#' on the search path and, if so, detaches it, then (re)loads it via
#' [reqS()]. This is handy during interactive development when you want to
#' pick up changes to a dependency without restarting R.
#'
#' @section Important:
#' This function performs side-effects on the session search path and namespace
#' table. Avoid using it inside package code or scripts where reproducibility
#' matters. Prefer declaring dependencies in `DESCRIPTION` and using
#' `Imports`/`NAMESPACE` rather than attaching/detaching at runtime.
#'
#' @param pkgName `character(1)`. The package name (e.g., `"dplyr"`).
#'
#' @return A logical value indicating whether the package
#'   was successfully attached (`TRUE`) or not (`FALSE`). Messages are printed
#'   for user feedback.
#'
#' @details
#' - Attachment is detected via [`search()`]; if `package:PKG` is present, the
#'   function prints a message and calls [`unloadNamespace()`] on `PKG` before
#'   reloading it with [reqS()].
#' - If the package namespace is loaded but not attached, it may still be
#'   unloaded by `unloadNamespace()`. Detach failures can occur if other loaded
#'   packages depend on `pkgName`.
#'
#' @seealso [search()], [detach()], [unloadNamespace()]
#'
#' @examples
#' \dontrun{
#'   # Reload ggplot2 interactively
#'   reload("ggplot2")
#'
#'   # Use within a development session after editing a dependency
#'   reload("data.table")
#' }
#'
#' @export
reload <- function(pkgName){
  pkgNameP <- paste0('package:',pkgName);
  print(pkgNameP);
  if(pkgNameP %in% search())
  {
    print('package found! will be detached.');
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

loadv <- function(file=NULL, envir = parent.frame(n=1L), verbose=T){
  if (is.null(file)) {file <- askfilename();}
  #load(file, verbose=T, envir = parent.frame(n=1L), ...)

  if (is_char_drive_id(file) && !file.exists(file)){
    message(' Seems to be a Google Drive file.');
    file <- googledrive::as_id(file)
  }

  if (file %inherits% 'drive_id'){
    message(' Opening as Google Drive file.');
    # browser()
    drDownloaded <- googledrive::drive_download(file, overwrite = T)
    file <- drDownloaded$local_path
  }

  message('Loading file: ' %+% bold(file))
  file_info <- fs::file_info(file)
  file_size <- fs::file_size(file)

  cat('\t',yellow(bold(file_info$modification_time)))
  cat('\t',blue(bold(file_size)),'\t')

  returned.objects <- base::load(file, envir = envir, verbose=verbose)
  if ('run_on_load_dat' %in% returned.objects) {
    cat('\n Running', italic('run_on_load_dat()'))
    run_on_load_dat()
  }
  invisible(returned.objects)
}

loadvc <- function()loadv(fromClip(), envir = parent.frame(n=1L))

loadv1 <- function(fnRdat, index=1, verbose=T){
  message('Loading file: ' %+% bold(fnRdat))
  obj.names <- base::load(fnRdat, verbose=verbose)
  if (length(obj.names)>1 & verbose) warning('Multiple objects loaded! Only first one will be returned.')
  obj.name <- obj.names[index]
  obj.return <- get(obj.name)
  return(obj.return)
}

load <- loadv


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


require('data.table');
require('magrittr');
require('crayon');

filter <- dplyr::filter

checkPlus <- function(){
  if (is.null('a'%+%'b')) reload('vautils')
}

# vai <- function(){
#   reqq('data.table magrittr');
# }


computer_user_names <- function() {
  shrink_values(
    c(
      Sys.info()[["nodename"]],
      Sys.info()[["login"]],
      Sys.info()[["user"]],
      Sys.info()[["effective_user"]],
      Sys.getenv("COMPUTERNAME"),
      Sys.getenv("HOSTNAME")
    ), exclude = '', do.sort = T
  )
}
