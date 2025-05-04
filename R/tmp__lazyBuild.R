lazyBuild <- function(objNames, fnRdat=NULL, object=NULL, verbose=F, unlist=F){
#   browser()
  obj.return <- NULL

  objName1 <- objNames[[1]]

  obj_find <- sapply(objNames,exists)

  if (all(obj_find)) {
    message('Objects ', paste(bold(objNames), collapse=', '),' already exist in the current environment.');
    if (length(objNames)==1) {invisible(get(objName1));} else {invisible(NULL)}
  } else {
    message('Objects ', paste(bold(objNames[obj_find==F]), collapse=', '),' not found in the current environment.');
    if (!is.null(fnRdat)){
      message(' Trying to load from Rdat file ', bold(fnRdat),'. ');
      if (!file.exists(fnRdat)) {
        message(' Rdat file not found! Will try to build.');
      } else { # Rdat file exists
        obj.names <- load(fnRdat, verbose=verbose, envir = parent.frame(n=1L))
        if (length(obj.names)==0) {
          message(' No objects loaded! ');
        } else {
          message(length(obj.names) %+% ' objects loaded! ' %+% paste(obj.names, collapse = ', '));
          obj.return <- get(obj.names[1])
          if (length(obj.names)>1) {
            message('Multiple objects loaded into environment, hence returning NULL.')
            invisible(NULL)
          }
        }
      }
    } # e. if (!is.null(fnRdat))

    if (is.null(obj.return)){
      message(' Building object... ');
      obj.return <- object
      if (is.null(object)) stop('No object definition provided, or NULL provided.')
      if (!is.null(fnRdat)) {
        if (unlist==T){
          message('\n Saving ', bold(cs1(names(obj.return))), ' to ',bold(fnRdat),'...');
          save(list = names(obj.return), file = fnRdat, envir = list2env(obj.return))
        } else {
          message('\n Saving ', bold(cs1(objName1)), ' to ',bold(fnRdat),'...');
          saveas(obj.return, names2save = objName1, file = fnRdat)
        }
      }
    }

    if (unlist==T) {
      message(' Unfolding list of ' %+% bold(length(obj.return)) %+% ' objects: ' %+% paste(bold(blue(names(obj.return))),collapse = ', ') %+% '.\n')
      list2env(obj.return, envir = parent.frame(n=1L))
      invisible(obj.return[[1]]);
    } else invisible(obj.return);
  }

} # e. lazyBuild()
