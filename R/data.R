
getEnvByName <- function(inpEnv=.GlobalEnv, lookFor){
  e <- inpEnv;
  while (environmentName(e) != 'R_EmptyEnv' & environmentName(e)!=lookFor) e <- parent.env(e);
  if (environmentName(e) != lookFor) return(NULL);
  return(e);
}

show.envs <- function(inpEnv=NULL){
  e <- inpEnv;
  if (is.null(e)) e <- environment(show.envs);
  #  e <- .GlobalEnv;
  i <- 0;
  while (environmentName(e) != 'R_EmptyEnv') {
    cat(i,': ', environmentName(e),'\n')
    #    cat(str(e, give.attr=F))
    #    print(exists('%+%', envir = e))
    e <- parent.env(e);
    i <- i+1;
  }
}

show.envs.plus <- function(){
  e <- environment(show.envs);
  #  e <- .GlobalEnv;
  i <- 0;
  while (environmentName(e) != 'R_EmptyEnv') {
    cat(i,': ', environmentName(e),'. ')
    #    cat(str(e, give.attr=F))
    cat(exists('%+%', envir = e))
    cat('\n')
    e <- parent.env(e);
    i <- i+1;
  }
}


# %+c%: c()
"%+c%" <- function(arg1, arg2){return(c(arg1, arg2));}
"%+C%" <- function(arg1, arg2){return(c(arg1, arg2));}
'%,%' <- function(...){  return(c(...)) }

# %+l%: list()
"%+l%" <- function(arg1, arg2){return(list(arg1, arg2));}
"%+L%" <- function(arg1, arg2){return(list(arg1, arg2));}
'%;%' <- function(...){  return(list(...)) }



"%!in%" <- function(arg1, arg2){
  return(!(arg1 %in% arg2));
}

# set difference
"%-%" <- function(arg1, arg2){
  return(setdiff(arg1, arg2));
}

"%--%" <- function(arg1, arg2){
  return(setdiff(arg2, arg1));
}

# intersect
"%&%" <- function(arg1, arg2){return(intersect(arg1, arg2));}

"%!=%" <- function(arg1,arg2){
  eq <- (arg1!=arg2);
  eq[is.na(eq)] <- T;
  return(eq)
}


"%~%" <- function(arg1, arg2){
  if (identical(sort(arg1, na.last=T), sort(arg2, na.last=T))) {return(TRUE);}
  else {print(sort(arg1, na.last=T)); print(sort(arg2, na.last=T));}
  return(FALSE);
}

# %===%: alias for `identical()` ####
"%===%" <- function(x,y) {
  return(identical(x,y));
}

# %==%: compares with (NA==NA) = TRUE; factors as character ####
"%==%" <- function(vec1, vec2){
  dims <- NULL;
  if (is.data.frame(vec1) | is.data.frame(vec2)){
    if (!is.data.frame(vec1)){stop("Second variable is a data.frame while first one is not. ")}
    if (!is.data.frame(vec2)){stop("First variable is a data.frame while second one is not. ")}
    if (ncol(vec1)!=ncol(vec2)){stop("Can't compare tables with different number of columns.")}
    if (nrow(vec1)!=nrow(vec2)){stop("Can't (yet) compare tables with different number of rows.")}
    dims <- c(nrow(vec1),ncol(vec1))
  }

  if (!is.vector(vec1)) vec1 <- unlist(vec1, use.names = F);
  if (!is.vector(vec2)) vec2 <- unlist(vec2, use.names = F);
  if (is.factor(vec1)) vec1 <- as.character(vec1);
  if (is.factor(vec2)) vec2 <- as.character(vec2);

  rez <- (vec1==vec2);
  rez[is.na(vec1) & is.na(vec2)] <- TRUE;
  rez[is.na(rez)] <- FALSE;
  if (!is.null(dims)){
    rez <- matrix(rez, nrow = dims[[1]])
    rez <- apply(rez, 1, function(x){sum(!x)==0})
  }
  rez;
} # e. %==%:



# %inw% - returns positions of any of needles in hay ####
# ex.:  c(2,2,3) %inw% c(1,1,2,2,3,3,4,4)
# will return:               3 4 5 6
"%inw%" <- function(needles, hay) {
  return(which(hay %in% needles));
}


# multiple return ####
# taken from https://stackoverflow.com/questions/7519790/assign-multiple-new-variables-on-lhs-in-a-single-line

# Generic form
'%<<%' = function(leftHS, rightHS, ...) UseMethod('%<<%')
'%>>%' = function(leftHS, rightHS, ...) UseMethod('%>>%')

# Binary Operator
'%<<%.leftbunch' = function(leftHS, rightHS, ...) {
  #browser()
  Envir = as.environment(-1)

  if (length(rightHS) > length(leftHS))
    warning("RHS has more args than LHS. Only first", length(leftHS), "used.")

  if (length(leftHS) > length(rightHS))  {
    warning("LHS has more args than RHS. RHS will be repeated.")
    r <- extendToMatch(rightHS, leftHS)
  }

  for (ndx in 1:length(leftHS)) {
    do.call('<-', list(leftHS[[ndx]], rightHS[[ndx]]), envir=Envir)
  }
}

'%<<%.bunch' = function(leftHS, rightHS, ...) {
  #browser()
  Envir = as.environment(-1)

  if (length(rightHS) > length(leftHS))
    warning("RHS has more args than LHS. Only first", length(leftHS), "used.")

  if (length(leftHS) > length(rightHS))  {
    warning("LHS has more args than RHS. RHS will be repeated.")
    r <- extendToMatch(rightHS, leftHS)
  }

  for (ndx in 1:length(leftHS)) {
    do.call('<-', list(leftHS[[ndx]], rightHS[[ndx]]), envir=Envir)
  }
}

# Used if LHS is larger than RHS
extendToMatch <- function(source, destin) {
  len_source <- length(source)
  len_dest <- length(destin)

  # Assume that destin is a length when it is a single number and source is not
  if(len_dest==1 && len_source>1 && !is.null(as.numeric(destin)))
    len_dest <- destin

  dif <- len_dest - len_source
  if (dif > 0) {
    source <- rep(source, ceiling(len_dest/len_source))[1:len_dest]
  }
  return (source)
}

# Grouping the left hand side
g <- function(...) {
  List = as.list(substitute(list(...)))[-1L]
  class(List) = 'leftbunch'
  return(List)
}






# geometric mean
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

sortnames <- function(input) sort(names(input))

toBase <- function (inpN, base, alphabet="0123456789abcdefghijklmnopqrstuvwxyz", pad=NA) {
  if (length(inpN)==1 && is.na(inpN)) return(NA_character_);
  strrep <- function(x, times){return(paste0(rep(x,times),collapse=""))}
  if (length(alphabet)==1) alphabet <- unlist(strsplit(alphabet,split = '',fixed = T));
  if (missing(base)) {base <- length(alphabet);}
  if (length(alphabet)<base) stop('Alphabet (',length(alphabet),') is shorter than base!');
  if (is.list(inpN)) return(lapply(inpN, toBase, base=base, alphabet=alphabet, pad=pad));
  if (is.vector(inpN) && length(inpN)>1) return(sapply(inpN, toBase, base=base, alphabet=alphabet, pad=pad));
  rezStr <- "";
  Q <- floor(abs(inpN));
  R <- NULL;
  while(TRUE) {
    R <- Q %% base;
    rezStr <- alphabet[R+1] %+% rezStr;
    Q <- (Q-R)/base;
    if (Q==0) break;
  }
  if (!is.na(pad) && nchar(rezStr)<pad){
    rezStr <- strrep(alphabet[1],pad-nchar(rezStr)) %+% rezStr;
  }
  return(ifelse(inpN<0,"-" %+% rezStr, rezStr));
}

fromBase <- function(inpS, alphabet="0123456789abcdefghijklmnopqrstuvwxyz"){
  if (length(inpS)==1 && is.na(inpS)) return(NA);
  alphabetSS <- unlist(strsplit(alphabet,split = '',fixed = T));
  base <- nchar(alphabet);
  rezN <- 0;

  if (is.list(inpS)) return(lapply(inpS, fromBase, alphabet=alphabet));
  if (is.vector(inpS) && length(inpS)>1) return(sapply(inpS, fromBase, alphabet=alphabet));

  for (i in seq_len(nchar(inpS))){
    .char <- substr(inpS,i,i)
    #.this <- grep(.char, alphabet); # this doesn't work!!! WTF!!!
    .this <- match(.char, alphabetSS) - 1;
    pos <- nchar(inpS) - i + 1;
    rezN <- rezN + base^(pos-1)*.this;
    #cat(i, .char, pos, .this, rezN, '\n');
  }
  return(rezN);
}

int2bitsV <- function(x) { which(as.logical(intToBits(x)), T);}


addBinaryFlag <- function(flagtxt, inpArrName="flagsList"){
  wEnv <- globalenv(); # environment needed to modify variable with flags list

  if (exists(inpArrName, envir=wEnv )){ # check if variable with flags list is defined
    inpArr <- get(inpArrName, envir = wEnv); # then use it
  } else {inpArr <- array(dim=0);} # otherwise create it as an empty array

  newval <- match(flagtxt, inpArr); # trying to find requested flag text in current list, to avoid duplication

  if (length(newval)==0 || is.na(newval)) { # if this flag text isn't in list
    newval <- length(inpArr)+1; # we will add it as new member
    inpArr[newval] <- flagtxt;
    assign(inpArrName, inpArr, envir=wEnv); # modify variable in parent (global) variable
  }
  return(newval);
}

time1 <- function() {
  assign("time1before", proc.time()[3], envir = .GlobalEnv)
  timestamp();
}

time2 <- function() {
  time1after<-proc.time()[3];
  #  time1before <-

  #rm(time1before);
  cat('\n',timestamp(), sep = '');
  return(time1after-time1before)
}


# Total incomprehensible magic ####
list <- structure(NA,class="result")
"[<-.result" <- function(x,...,value) {
  args <- as.list(match.call())
  args <- args[-c(1:2,length(args))]
  length(value) <- length(args)
  for(i in seq(along=args)) {
    a <- args[[i]]
    if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
  }
  x
}


#  convert factors to numeric ####
fac2num <- function(f) {
  if (!is.factor(f)) {
    return(f);
  } else return(as.numeric(levels(f))[f])
}

all2num <- function(f) {
  if (!is.factor(f)) {
    return(as.numeric(f));
  } else return(as.numeric(levels(f))[f])
}


# roundC(): rounds all negative to 0, all positive to integer ####
# ex.: roundC(c(-1, 0.2, 1.5, -3.8))
# will return    0  0    2     0
roundC <- function(x){
  ifelse(x<0, 0, round(x));
}

# checks if all elements are equal to one another
all.same <- function(input, na.rm=FALSE){
  if (length(input)<2)   {return(TRUE);}
  if (all(is.na(input))) {return(TRUE);}
  return(isTRUE(all(input==input[1], na.rm = na.rm)));
}

# non-vectorized ifelse ####
ifelse1 <- function(test, yes, no){
  if (all(test)) return(yes) else return(no);
}

# smart seq_len ####
seqlen <- function(obj){
  if (length(obj)==1 & is.numeric(obj)) {return(seq_len(obj));}
  if (is.data.frame(obj)) {return(seq_len(nrow(obj)));}
  return(seq_len(length(obj)));
}

replace.mult <- function(inpvec, from, to){
  stopifnot(length(from)>0 & length(from)==length(to));
  for (i in seqlen(from)){
    inpvec <- replace(inpvec, inpvec==from[i], to[i])
  }
  return(inpvec)
}

orderby <- function(x,y){
  if (is.character(y) && length(y)==1 && (y %in% names(x))) {
    ret.ord <- x[[y]];
  } else ret.ord <- y;
  if ('data.table' %in% class(x)) {
    return(x[order(ret.ord),])
   } else return(x[order(ret.ord)])

}


melt.distance <- function(inpDist){
  L <- length(inpDist)
  N <- length(labels(inpDist))
  df.melted <- data.frame(i=1:L,X=NA,Y=NA,dist=NA)
  distLabels <- labels(inpDist)
  i <- 1L
  for (x in 1:(N-1)){
    for (y in (x+1):N){
      #cat(i,x,y,distLabels[x],distLabels[y], '\n')
      nameX <- distLabels[x]
      nameY <- distLabels[y]
      df.melted[i, 'X'] <- nameX
      df.melted[i, 'Y'] <- nameY
      df.melted[i, 'dist'] <- inpDist[i]
      i <- i+1L
    } # e. for y
  } # e. for x
  return(df.melted)
} # e. melt.distance()

mtx.distance <- function(inpDist){
  L <- length(inpDist)
  N <- length(labels(inpDist))
  distLabels <- labels(inpDist)

  distMtx <- matrix(,nrow = N, ncol = N)
  colnames(distMtx) <- distLabels
  rownames(distMtx) <- distLabels

  i <- 1L
  for (x in 1:(N-1)){
    for (y in (x+1):N){
      nameX <- distLabels[x]
      nameY <- distLabels[y]
      distMtx[nameX, nameY] <- inpDist[i]
      distMtx[nameY, nameX] <- inpDist[i]
      i <- i+1L
    } # e. for y
  } # e. for x
  return(distMtx)
} # e. mtx.distance()



na.allow <- function(input) return(is.na(input) | input);


tryRdat <- function(fnRdat, FUN, nEnv=1L, ...){
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
      assign(value = result[[this_name]], x=this_name) # , envir = parent.frame()
      names_to_save <- c(names_to_save,this_name)
    }

    save(list = names_to_save, file = fnRdat);
    load(fnRdat, verbose = T, envir = parent.frame(n=2));
  } # e. else

} # e. try_rdat


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


compare.lists <- function(list1,list2) {
  names1 <- names(list1);
  names2 <- names(list2);
  if ('' %in% names(list1)){
    cat('List1 contains nameless components!\n')
  }

  if ('' %in% names(list2)){
    cat('List2 contains nameless components!\n')
  }

  if ( !identical(sort(names1),sort(names2)) ){
    cat('List1 and List2 have different names of components!\n')
    only1 <- names1 %-% names2;
    only2 <- names2 %-% names1;
    if (length(only1)>0) cat('Only in List1: ', paste0(only1,collapse = ', '),'.\n')
    if (length(only2)>0) cat('Only in List2: ', paste0(only2,collapse = ', '),'.\n')
  } # e. if names different

  common <- intersect(names1, names2);
  cat(length(common),'common names found.\n')

  for(i in common){
    if (!identical(list1[i],list2[i])){
      cat(i,':\t Different\n')
    }

  }

} # e. compare.lists()


mtx2int <- function(input){
  input <- round(input)
  storage.mode(input) <- 'integer'
  input;
}




guessyear <- function(inpdates, datesformat='%a %b %e %H:%M:%S', years){
  rez <- strptime(inpdates, datesformat)
  for (year in years){
    fulldate <- paste0(year, ' ', inpdates)
    fullformat <- paste0('%Y', ' ', datesformat)
    convdate <- strptime(fulldate, fullformat)
    teststr <- strftime(convdate, datesformat)
    correct <- (teststr == inpdates)
    rez[correct] <- convdate[correct]
  }
  return(rez)
}

# tmp <- guessyear(cs('Sun Dec 30 02:34:21;Wed Jan  2 17:06:35;Fri Sep 21 16:23:33',sep=';'),
#                  years=cs('2018 2019'))


minDate <- function(inpX, na.rm=T) {
  # browser()
  if (length(inpX)==0) return(as.Date(NA_real_)) else  min(inpX,na.rm=na.rm);
}

# maxI() and minI(): modified max() and min();
# when input is NA only, return NA instead of Inf
maxI <- function(inp){
  inp <- na.omit(inp)
  if (length(inp)==0) {
     return(ifelse('integer' %in% class(inp), NA_integer_, NA_real_))
    }
  return(max(inp))
}

minI <- function(inp){
  inp <- na.omit(inp)
  if (length(inp)==0) {
    return(ifelse('integer' %in% class(inp), NA_integer_, NA_real_))
  }
  return(min(inp))
}
