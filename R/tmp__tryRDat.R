tryRdat <- function(fnRdat, FUN, nEnv=1L, resnames=NA,...){
# This function tries to load data from the indicated Rdat file
# if it doesn't exist, it will call indicated function, and save the results to the file
# Function must return named objects in a list
  cat('Checking pre-existing Rdat file:', fnRdat, '... ');
  if (file.exists(fnRdat)) {
    cat(' found. ');
    load(fnRdat, verbose = T, envir = parent.frame(n=nEnv));
  } else {
    cat(' not found. Will be generated and saved.\n');
    result <- do.call(FUN, args = list(...))

    names_to_save <- c()
    for(this_name in names(result)){
      new_name <- resnames[[this_name]];
      if (is.na(new_name)) new_name <- this_name;
      assign(x=new_name, value=result[[this_name]]) # , envir = parent.frame()
      names_to_save <- c(names_to_save,new_name)
    }

    save(list = names_to_save, file = fnRdat);
    load(fnRdat, verbose = T, envir = parent.frame(n=2));
  } # e. else

} # e. try_rdat