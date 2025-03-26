testab1 <- function(){
  print ('a' %+% 'b')
}

testab2 <- function(inpA, inpB){
  print (inpA %+% inpB)
}



cs <- function(inputstr, sep=",", fixed=T, nonewlines=T, allowempty=F){
  if (length(inputstr)==0) return(inputstr);
  if (missing(sep)){
    sep <- ' '
    if (any(inputstr %~~% ','))  {sep=",";}
    if (any(inputstr %~~% '\t')) {sep="\t";}
  }
  if (nonewlines) inputstr <- gsub("[\n\r]+", sep, inputstr);
  rez <- unlist(strsplit(inputstr, sep, fixed=fixed));
  if (allowempty==F) rez <- rez[rez!=""];
  return(rez);
}

printcs <- cs1 <- function(input, collapse=','){paste0(cs(input), collapse = collapse)}



catpastelist <- function(inp){
  if (is.data.frame(inp)) inp <- names(inp);
  cat(paste(inp,collapse = '\n'))
}


rightstr <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
substrRight <- rightstr

substr1 <- function(inpStr) {substring(inpStr,1,1)}

strpad <- function(inpstr,padWidth, padSym=' ',padSide='right'){
  add_length <- padWidth - nchar(inpstr)
  if (any(add_length<0)) warning('Some input strings are already longer than required string length! ')
  add_length[add_length<0] <- 0
  pads <- strrep(padSym,add_length);
  switch(padSide, right = paste0(inpstr,pads), left = paste0(pads, inpstr))
}


print_list <- function(inp) {
  for (i in seqlen(inp)){
    .member <- inp[i];
    cat('\n',i,
        stringr::str_pad(names(.member), max(nchar(names(inp)))),
        unlist(unname(.member)),
        #str_pad(unlist(unname(fn[i])), max(nchar(unlist(unname(fn))))),
        '')
  } # e. for
} # e. fun


# sub_str <- function(inputstr, start=NA, stop=NA, len=NA){
#   tmp = NA
#   if (tmp>0) print(">!")
# }

# detach("package:vautils", unload=TRUE)
# library(vautils)
# f2()
# detach("package:vautils", unload=TRUE)
# library(vautils)
# f2()
#

# "+" = function(x,y) {
#   if(is.character(x) || is.character(y)) {
#     return(paste(x , y, sep=""))
#   } else {
#     .Primitive("+")(x,y)
#   }
# }



paste0ignNA <- function(arg1, arg2){
  if (length(arg1)==1 && length(arg2)==1 && not.na(arg1) && not.na(arg2)) return(paste0(arg1,arg2));
  if (length(arg1)==0) return(arg2);
  if (length(arg2)==0) return(arg1);

#   browser()
  if (length(arg1)>1 & length(arg2)>1)  return(mapply(paste0ignNA, arg1, arg2, USE.NAMES=F))
  if (length(arg1)>1 & length(arg2)==1) return(sapply(arg1, paste0ignNA, arg2=arg2, USE.NAMES=F))
  if (length(arg2)>1 & length(arg1)==1) return(sapply(arg2, paste0ignNA, arg1=arg1, USE.NAMES=F))

  stopifnot(length(arg1)==1 & length(arg2)==1)

  if (is.na(arg1)) return(arg2);
  if (is.na(arg2)) return(arg1);

  # if (any(length(arg1)==0, is.na(arg1)) & any(length(arg2)==0, is.na(arg2)) )  return('');
  # if (any(length(arg1)==0, is.na(arg1)) ) return(arg2);
  # if (any(length(arg2)==0, is.na(arg2)) ) return(arg1);
  stop('Unexpected error!');
}

`%+%` <- function(...) UseMethod("%+%")
`%+%.character` <- paste0ignNA
`%+%.numeric`   <- paste0ignNA
`%+%.logical`   <- paste0ignNA
`%+%.NULL`      <- paste0ignNA
`%+%.default` <- function (arg1, arg2){
  if (is.character(arg2)) {return(paste0ignNA(arg1,arg2));}
  message('\n Running %+%.default! \n ');
  e <- parent.env(getEnvByName(.GlobalEnv,'package:vautils'));
  if (exists('%+%', envir = e)) get('%+%',envir = e)(arg1,arg2);
}

# Usage:
# 'a' %+% 'b': 'ab'
# 'a' %+% NA : 'a'
# NA %+% NA : NA
# cs('A B') %+% cs('1 2') : 'A1', 'B2'
# cs('A B') %+% c('1', NA): 'A1', 'B'


paste0notNA <- function(arg1, arg2){
  if (length(arg1)==1 && length(arg2)==1 && not.na(arg1) && not.na(arg2)) return(paste0(arg1,arg2));
  if (length(arg1)==0) return(arg2);
  if (length(arg2)==0) return(arg1);

  #   browser()
  if (length(arg1)>1 & length(arg2)>1)  return(mapply(paste0notNA, arg1, arg2, USE.NAMES=F))
  if (length(arg1)>1 & length(arg2)==1) return(sapply(arg1, paste0notNA, arg2=arg2, USE.NAMES=F))
  if (length(arg2)>1 & length(arg1)==1) return(sapply(arg2, paste0notNA, arg1=arg1, USE.NAMES=F))

  stopifnot(length(arg1)==1 & length(arg2)==1)

  if (is.na(arg1)) return(NA_character_);
  if (is.na(arg2)) return(NA_character_);

  # if (any(length(arg1)==0, is.na(arg1)) & any(length(arg2)==0, is.na(arg2)) )  return('');
  # if (any(length(arg1)==0, is.na(arg1)) ) return(arg2);
  # if (any(length(arg2)==0, is.na(arg2)) ) return(arg1);
  stop('Unexpected error!');
}


`%++%` <- function(...) UseMethod("%++%")
`%++%.character` <- paste0notNA
`%++%.numeric`   <- paste0notNA
`%++%.logical`   <- paste0notNA
`%++%.NULL`      <- paste0notNA
`%++%.default` <- function (arg1, arg2){
#  browser()
  if (all(is.na(arg1)) & is.character(arg2)) return(paste0notNA(arg1,arg2));
#  if (all(is.na(arg1)) & all(is.na(arg2)) ) return(NA);
  e <- parent.env(getEnvByName(.GlobalEnv,'package:vautils'));
  if (exists('%++%', envir = e)) get('%++%',envir = e)(arg1,arg2);
}

# Usage:
# 'a' %++% 'b': 'ab'
# 'a' %++% NA : NA
# NA %++% NA : NA
# cs('A B') %++% cs('1 2') : 'A1', 'B2'
# cs('A B') %++% c('1', NA): 'A1', NA

pasteNotNA <- function(...,collapse=', '){
  arglist <- list(...);
  arglens <- sapply(arglist, length)
#  browser()
  if (all(arglens<2)) return(paste(na.omit(unlist(arglist)),collapse=collapse))
  maxlen <- max(arglens)

  ans <- do.call("mapply", c(pasteNotNA, arglist, collapse=collapse, USE.NAMES = FALSE))
  ans
}

# %+%: strings concatenation
# "%+%" <- function(arg1, arg2){
#   if (is.character(arg1) & is.character(arg2)) {
#     paste0(arg1, arg2);
#   } else {
#     e <- parent.env(parent.env(.GlobalEnv));
# #    cat(".G:",environmentName(e),'\n')
#     while (environmentName(e) != 'R_EmptyEnv' & !exists('%+%', envir = e)) {
# #      print(environmentName(e))
#       e <- parent.env(e);
#     } # e. while
#     if (environmentName(e) != 'R_EmptyEnv'){
#       old.func <- get('%+%',envir = e)
#       old.func(arg1,arg2);
#     }
#   } # e. else
# }

prn <- function(...){ # just print(..., collapse=""); - with CRLF at the end
  arglist <- list(...);
  cat(paste0(unlist(arglist), collapse=""),'\n');
  invisible(NULL);
}

prnc <- function(...){ # just print(..., collapse=""); - no CRLF at the end
  arglist <- list(...);
  cat(paste0(unlist(arglist), collapse=""));
  invisible(NULL);
}


ask <- function(...){
  readline(prompt=paste0(unlist(list(...)), collapse=""));
}



deperc <- function(str_perc){
  return(as.numeric(gsub('([\\d\\.\\+\\-]*)%','\\1',str_perc))/100);
}


# removes trailing slash (for dir path) ####
chops <- function(inputstr){
  return(gsub("(.*)/", "\\1", inputstr));
}

# chop character
chopLeft <- function(inpstr,n=1L){
  return(substring(inpstr,n+1L));
}

chopRight <- function(inpstr,n=1L){
  return(substring(inpstr,1,nchar(inpstr)-n));
}


xlsxlsdate <- function(inp, optional=T, limits=c(as.Date('1906-01-01'),as.Date('2100-01-01'))) {
  output <- as.Date(as.numeric(inp), origin="1899-12-30", optional=T)
  output[output<limits[1]] <- NA
  output[output>limits[2]] <- NA
  return(output)
}

#xls_date(c("6/30/22"," 1/2021"))

xls_date <- function(input, strict=F, quiet=T, split=F, formats2try=cs('%m/%d/%Y,%d/%m/%Y,%Y/%m/%d'), tryPOSIX=T){
  messageA <- warning;
  if (quiet==T) messageA <- function(x) invisible(x);

  tryformats <- formats2try
  if (is.someDate(input)) return(input);
  if (length(input)==0)      {if (!quiet) warning(' Input of zero length in xls_date(). '); return(input)}

  if ('character' %in% class(input)){
    input %<>% trimws()
    input[nchar(input)==0] <- NA_character_
  }

  if (sum(!is.na(input))==0) {if (!quiet) warning(' Input of NA only in xls_date(). ');     return(as.Date(NA))}

  inputNotNA <- na.omit(input)
  inputNum <- suppressWarnings(as.numeric(input));
  inputNumOnly <- na.omit(inputNum)
  inputNumOnlyInt <- as.integer(inputNumOnly)
  notNums <- is.na(inputNum)

  # browser()

  if (split==T) {
    ret <- sapply(input, xls_date, strict=strict, quiet=quiet, split=F, formats2try=formats2try, tryPOSIX=tryPOSIX, USE.NAMES = F)
    class(ret) <- 'Date'
    return(ret)
  }

  if (length(inputNumOnly)>0){
    messageA(" Numeric!")
    if (all(inputNumOnly == inputNumOnlyInt)){
      messageA(" Integer!")
      output <- xlsxlsdate(inputNum)
      return(output)
    } else {
      messageA(" Not integer!")
      messageA(inputNum)
      output.posix <- as.POSIXct(inputNum*(60*60*24), origin="1899-12-30", optional=T);
      messageA(as.character(output.posix))
      return(output.posix)
    }
  } else {
    messageA(" Not numeric!")
    input.cleaned <- input %>% trimws() %>% gsub('[\\\ /-]+','/',.)
    if (all(nchar(inputNotNA)==10 | nchar(inputNotNA)==9)){
      messageA("Text date, 10!");
      if (all(inputNotNA %~~% '^\\d{4}'))  tryformats <- cs('%Y/%m/%d')
      if (all(inputNotNA %~~%  '\\d{4}$')) tryformats <- formats2try
      return(as.Date(input.cleaned, tryFormats=tryformats, optional=T))
    } else if (all(nchar(inputNotNA) %in% c(6,7,8))){
      messageA("Text date, 8!");
      tryformats <- cs('%m/%d/%y,%d/%m/%y')
      #browser()
      if (any(inputNotNA %~~% '^\\d{4}'))  tryformats <- cs('%Y/%m/%d') # ??? !!! should be all() instead of any()!!!
      if (any(inputNotNA %~~%  '\\d{4}$')) tryformats <- formats2try
      return(as.Date(input.cleaned, tryFormats=tryformats, optional=T))
    } else {
      messageA(" Maybe date and time?");
      if (tryPOSIX==T) {
        messageA(" Trying as.POSIXct()");
        return(as.Date(as.POSIXct(input, optional=T)))
      } else return(as.Date(NA))
    }
  }

  messageA("Something else!")

  output <- as.Date(inputNum, origin="1899-12-30", optional=T);
  # output <- as.Date(output.posix)
  vec4date <- notNums & (nchar(input)>7)
  vec4date[is.na(vec4date)] <- FALSE


  # dt_debug <-  data.table(inp=input, inpN=inputNum, notn=notNums, outp=output)
  # sapply(input[vec4date], function(X){cat(X,'. '); a=as.Date(X);})

  output[vec4date] <- as.Date(input[vec4date], optional=T)

  return(output)
}


va_date <- function(input, strict=F, quiet=T, split=F, formats2try=cs('%m/%d/%Y,%d/%m/%Y,%Y/%m/%d'), tryPOSIX=T, limits=c(as.Date('1906-01-01'),as.Date('2100-01-01'))){
  messageA <- warning;
  if (quiet==T) messageA <- function(x) invisible(x);

  tryformats <- formats2try
  if (is.someDate(input)) return(input);
  if (length(input)==0)      {if (!quiet) warning(' Input of zero length. '); return(input)}

  if ('character' %in% class(input)){
    input %<>% trimws()
    input[nchar(input)==0] <- NA_character_
  }

  if (sum(!is.na(input))==0) {if (!quiet) warning(' Input of NA only in xls_date(). ');     return(as.Date(NA))}

  inputNotNA <- na.omit(input)
  inputNum <- suppressWarnings(as.numeric(input));
  inputNumOnly <- na.omit(inputNum)
  inputNumOnlyInt <- as.integer(inputNumOnly)
  notNums <- is.na(inputNum)

  # browser()

  if (split==T) {
    ret <- sapply(input, va_date, strict=strict, quiet=quiet, split=F, formats2try=formats2try, tryPOSIX=tryPOSIX, USE.NAMES = F)
    class(ret) <- 'Date'
    return(ret)
  }

  output <- rep(as.Date(NA), length(input))

  mask.xls <- input %~~% '\\d+\\.0'
  output[mask.xls] <- xlsxlsdate(input[mask.xls])

  re.ok <- '\\d{4}[-/\\.]\\d{1,2}[-/\\.]\\d{1,2}'
  re.ok.ex <- '.*(' %+% re.ok %+% ').*'
  match.ok <- input %~~% re.ok
  output[match.ok] <-
    input[match.ok] %>%
    gsub(re.ok.ex,'\\1',.) %>%
    gsub('[-\\.]','/',.) %>%
    as.Date(optional=T)

 # browser()


  re.ok <- '\\d{1,2}[-/\\.]\\d{1,2}[-/\\.]\\d{4}'
  re.ok.ex <- '[^0-9]*(' %+% re.ok %+% ').*'
  match.ok <- input %~~% re.ok
  output[match.ok] <-
    input[match.ok] %>%
    gsub(re.ok.ex,'\\1',.) %>%
    gsub('[-\\.]','/',.) %>%
    as.Date(tryFormats=formats2try, optional=T)

  match.fail <- is.na(output)
  output[match.fail] <- lubridate::as_date(input[match.fail])

  match.fail <- is.na(output)
  output[match.fail] <- xls_date(input[match.fail])

  output[output<limits[1]] <- NA
  output[output>limits[2]] <- NA

  return(output)


}

va_date_char <- function(inp, ...){
#  browser()
  ret <- as.character(va_date(inp, ...))
  ret[is.na(ret)] <- inp[is.na(ret)]
  ret
}


xlsxlsdate_char <- function(inp, ...){
#  browser()
  ret <- as.character(xlsxlsdate(inp, ...))
  ret[is.na(ret)] <- inp[is.na(ret)]
  ret
}

xls_date_char <- function(inp, ...){
#  browser()
  ret <- as.character(xls_date(inp, ...))
  ret[is.na(ret)] <- inp[is.na(ret)]
  ret
}





# charnumchar() deals with the bug of excel import,
# when "1234567" becomes "1.2345E7":
# "1.2345E7" -> 1234567 -> "1234567"
charnumchar <- function(input){
  #if ('Date' %in% class(input)) return(input);
  inputNum <- suppressWarnings(as.numeric(input));
  notNums <- is.na(inputNum)

  output <- as.character(inputNum);
  output[notNums] <- input[notNums]

  return(output)
}


# 'chrX;chrX;chrX;chrX' => 'chrX'
str_shrink <- function(inp_str, sep=';'){
  # opposite of rep()
  # 'chrX;chrX;chrX;chrX' => 'chrX'
  if (length(inp_str)==0) stop('Wrong input!')
  if (length(inp_str)==1){
    paste0(unique(unlist(strsplit(inp_str,sep,fixed = T))), collapse = sep);
  } else {
    str_shrink_1 <- function(inp,sep=';'){paste0(unique(inp),collapse=sep)}
    sapply(strsplit(inp_str,sep), str_shrink_1, sep=sep);
  }
}

# shrink_values():
# c(3,2,3,NA,4) => '3;2;4'
shrink_values <- function(values, collapse=';', all=F, dropNA=T, exclude=NULL, fillempty=NULL, force.char=T, do.sort=NA){
  # browser()
  if (force.char==T){
    if (!is.character(values)) values %<>% as.character()
    if (!is.null(fillempty) & !is.character(fillempty)) fillempty %<>% as.character()
  }
  values2 <- values;

  if (all==F) values2 <- unique(values);
  if (dropNA==T) values2 <- na.omitva(values2);
  if (length(exclude)>0) values2 <- values2[values2 %!in% exclude];
  if (not.na(do.sort)) values2 %<>% sort()

  if (length(values2)==1) return(values2);
  #if (length(values2)==0) return(ifelse(is.null(fill),values[1],fill));
  if (length(values2)==0){
    if (!is.null(fillempty)) values2 <- fillempty;
    #if (is.na(fillempty)) values2 <- fillempty;
    return(values2);
  }

  paste(values2, collapse = collapse)
}

shrink_values1 <- shrink_values

shrink_values_any <- function(values, fun_char=shrink_values, fun_numeric=meanI, fun_other=shrink_values, ...){
  dots <- list(...)
  f_args_num  <- names(formals(fun_numeric))
  f_args_char <- names(formals(fun_char))
  f_args_othr <- names(formals(fun_other))
  args_num  <- dots[names(dots) %in% f_args_num]
  args_char <- dots[names(dots) %in% f_args_char]
  args_othr <- dots[names(dots) %in% f_args_othr]


  if (is.character(values)) {return(do.call(fun_char,    c(list(values), args_char)))} # cat('\t Running fun_numeric with ' %+% cs1(args_num));
  if (is.numeric(values))   {return(do.call(fun_numeric, c(list(values), args_num)))}

  return(do.call(fun_other, c(list(values), args_othr)))
}


# cs('+ - - - + +') => '+-+'
# cs('+ - - - ') => '+-'
str_shrink_rle <- function(x, sep='', dropNA=T){
  if (dropNA==T) x <- x[!is.na(x)]; # can't use na.omit() here because rle() requires a vector of an atomic type
  paste(rle(x)$values, collapse = sep)
}


askfilename <- function(fnInput=NULL, allowEmpty=F, prompt=NULL){
  if (!is.null(prompt)) cat(prompt);

  while (is.null(fnInput) || !file.exists(fnInput)) {
    cat("File not found:", fnInput);

    if (allowEmpty==T) {
      ans <- ask("File not found: ", fnInput, ".\n Enter another file name, or space (\" \") to leave it empty, or press Enter to try again, or ESC to exit: ");
      if (ans!="") {fnInput <- ans;}
      if (ans==" ") {return("");}
    } else {
      ans <- ask("File not found: ", fnInput, ".\n Enter another file name, or press Enter to try again, or ESC to exit: ");
      if (ans!="") {fnInput <- ans;}
    } # end else
  } # end while
  fnInput <- gsub('\\\\','/',fnInput);
  return(fnInput);
}


asc <- function(x) { strtoi(charToRaw(x),16L) }
chr <- function(n) { rawToChar(as.raw(n)) }


lettersX <- c(letters,
              sapply(letters, function(X){X %+% letters})
              )

LETTERSX <- toupper(lettersX)

stage.roman <- function(inp.decStr){
  part.num <- gsub('(\\d).*','\\1',inp.decStr)
  part.let <- gsub('(\\d)(.*)','\\2',inp.decStr)
  cs('I II III IV')[as.numeric(part.num)] %+% part.let
}


signChar <- function(x) unlist(sapply(x, function(x) switch (as.character(sign(x)),`-1` = '-',`1` = '+',`0` = '0')))


# converts KB, MB, GB, etc to bytes
size_bytes <- function(inpTxt){
  sizes <- c(bytes=1, KB=1024, MB=1024^2, GB=1024^3, TB=1024^4)
  num <- gsub('([-+0-9\\.]+).*','\\1',inpTxt)
  suffix <- gsub('.*?([a-zA-Z]+)','\\1',inpTxt)
  mult <- sizes[suffix]
  mult[inpTxt==''] <- 0
  result <- as.numeric(num)*mult
  #tmp <- data.table(inp=inpTxt,num=num,suffix=suffix,mult=mult, result=result)
  return(result)
}


# compl_year('12/12/24') => '12/12/2024'
# compl_year('12/12/26') => '12/12/1926'
compl_year <- function(inpStr, regex='(.*)/(\\d+)', thr=25){
  # more strict: '([01]?\\d/[0123]?\\d)/(\\d+)'
  inpStr.1 <- gsub(regex,'\\1',inpStr)
  inpStr.2 <- gsub(regex,'\\2',inpStr)

  yr <- as.numeric(inpStr.2)
  inpStr.2a <- ifelse(yr<=thr,'20','19') %+% inpStr.2

  rez <- inpStr.1 %+% '/' %+% inpStr.2a

  already.compl <- (nchar(inpStr.2)>2);
  already.compl[is.na(already.compl)] <- TRUE;

  rez[already.compl] <- inpStr[already.compl];

  return(rez)

}

greplic <- function(...) grepl(...,ignore.case = T)

#`%like%` <- function(x, pattern){grepl(pattern,x)}
 `%~~%`  <- function(x, pattern){ grepl(pattern,x)}
 `%~~i%` <- function(x, pattern){ greplic(pattern,x)}
`%!~~%`  <- function(x, pattern){!grepl(pattern,x)}
`%!~~i%` <- function(x, pattern){!greplic(pattern,x)}


grepl_mult <- function(y, patterns){
  if (length(y)==1) {return(any(sapply(patterns, grepl, x=y)))}
  apply(X=sapply(X=patterns, FUN=grepl, x=y),MARGIN=1,FUN=any)
}

grepl_mult_ic <- function(y, patterns){
  if (length(y)==1) {return(any(sapply(patterns, grepl, x=y, ignore.case=T)))}
  apply(X=sapply(X=patterns, FUN=grepl, x=y, ignore.case=T),MARGIN=1,FUN=any)
}


 `%~~~%`  <- grepl_mult
`%!~~~%`  <- function(x, patterns) !grepl_mult(x,patterns)
 `%~~~i%` <- grepl_mult_ic
`%!~~~i%` <- function(x, patterns) !grepl_mult_ic(x,patterns)

#  `%~~~%`  <- function(x, patterns){ apply(sapply(patterns, function(pattern) grepl(pattern,x), USE.NAMES=F), 1,any) }
# `%!~~~%`  <- function(x, patterns){!apply(sapply(patterns, function(pattern) grepl(pattern,x), USE.NAMES=F), 1,any) }
#  `%~~~i%` <- function(x, patterns){ apply(sapply(patterns, function(pattern) greplic(pattern,x), USE.NAMES=F), 1,any) }
# `%!~~~i%` <- function(x, patterns){!apply(sapply(patterns, function(pattern) greplic(pattern,x), USE.NAMES=F), 1,any) }


`%=u=%` <- function(x,y) toupper(x)==toupper(y)

nicedate <- function(inpDate=Sys.time()) format(inpDate, '%Y%m%d_%Hh%Mm%Ss_')


wrap_add <- function(inpStr, width=100){
  outStr <-
    inpStr %>%
    strsplit(split = '[\r\n]') %>%
    unlist() %>%
    stringi::stri_wrap(width = width) %>%
    paste(collapse = '\n')
}


strsplitS <- function(input,split=';',...){
  if (input %===% NA) return(NA)
  unlist(strsplit(input,split=split,...))
}
strsplitMin <- function(x,split=';',...) sapply(strsplit(x,split=split,...), min)
strsplitMax <- function(x,split=';',...) sapply(strsplit(x,split=split,...), max)
strsplitUnq <- function(x,split=';',...) sapply(strsplit(x,split=split,...), function(x) paste0(unique(x), collapse = split))


if (Sys.info()['sysname'] != 'Windows'){
  readClipboard <- clipr::read_clip
  writeClipboard<- clipr::write_clip
}

toClip <- function(content){writeClipboard(replace.mult(as.character(content),NA,''))}
fromClip <- function(...){readClipboard()}
tromClip <- function(...){fromClip() %>% paste(collapse = '\n') %>% fread(...)}


# this doesn't work in data.table, see https://stackoverflow.com/questions/72926127/
paste_clean <- function(...){
  arglistS <- as.list(substitute(list(...)))
  ret <- ''

  for (arg in arglistS){
    argVal <- tryCatch(eval(arg), error = function(cond) {warning(cond); return(NULL);})
    if (isTRUE(attr(argVal, 'class')=='result') & class(arg)=='name') next; # R v 4.1
    if (identical(argVal, .Primitive('list'))) next; # R v 4.2+
    if (is.null(argVal)) return('')
    argVal[is.na(argVal)] <- '';
    ret <- paste0(ret,argVal);
    ret[is.na(argVal)] <- '';
  }

  return(ret)
}


paste_clean <- function(a1,a2='',a3='',a4=''){
  a1 <- tryCatch(a1, error = function(cond) {warning(cond); return(NULL);})
  a2 <- tryCatch(a2, error = function(cond) {warning(cond); return(NULL);})
  a3 <- tryCatch(a3, error = function(cond) {warning(cond); return(NULL);})
  a4 <- tryCatch(a4, error = function(cond) {warning(cond); return(NULL);})
  if (is.null(a1) | is.null(a2) | is.null(a3) | is.null(a4)) return('');
  a1[is.na(a1)] <- '';
  a2[is.na(a2)] <- '';
  a3[is.na(a3)] <- '';
  a4[is.na(a4)] <- '';
  ret <- paste0(a1,a2,a3,a4);
  ret[is.na(a1) | is.na(a2) | is.na(a3) | is.na(a4)] <- ''
  return(ret)
}



lastname <- function(fullname, split1=' ') {
  # browser()
  if (length(fullname)>1) {
    ret <- sapply(fullname, lastname)
  } else {
    ret <- last(strsplitS(fullname, split = split1))
  }
  unname(ret)
}

lastnames <- function(fullnames){
  paste(lastname(strsplitS(fullnames, split = ';')), collapse = ';')
}


va_txt_reduce <- function(inpTxt,fun.case=toupper,repl=T){
  inpTxt %<>% fun.case()
  if (!repl==F) inpTxt %<>% gsub('_','.',.)
  inpTxt
}


# Removes more "general" text if more "specific" presents, for example:
# cs('CRC/Colon,CRC,Lung,Lung/NSCLC,Lung/SCLC') => cs('CRC/Colon,Lung/NSCLC,Lung/SCLC') # ('CRC' and 'Lung' are removed)
va_txt_remove_parents <- function(inpVec){
  vec.work <- sort(unique(inpVec))
  if (length(vec.work)<2) return(inpVec)

  vec.remove <- c()
  for (i in 2:length(vec.work)){
    el.1 <- vec.work[i-1]
    el.2 <- vec.work[i]
    if (el.1==substr(el.2,1,nchar(el.1))) vec.remove <- c(vec.remove,el.1);
  }
  return(inpVec[!inpVec %in% vec.remove])
}


# myfun <- function(x){
#   browser()
# }

# c('a;b','b;a') => c('a;b','a;b')
va_txt_splitsort <- function(x, split=','){
  x %>% strsplit(split=split) %>% lapply(sort) %>% sapply(paste, collapse=split)
}

# c('a;b','b;a','c;b;c') => c('a;b','a;b', 'b;c')
va_txt_splitsortunique <- function(x, split=','){
  x %>% strsplit(split=split) %>% lapply(sort) %>% lapply(unique) %>% sapply(paste, collapse=split)
}


trim0 <- function(input) gsub('\\.0$','',input)


va_txt_dominant_case <- function(inpVec){

  dt.inp <- data.table(inpStr=inpVec)
  dt.inp[, upperCase:=toupper(inpStr)]

  dt.stat <- dt.inp[,.N,by=.(inpStr,upperCase)]

  dt.stat[, keep:=(N==max(N)),by=upperCase]
  dt.stat <- dt.stat[keep==T,]

  dt.stat[, dupN:=.N, by=upperCase]
  #  if (all(dt.stat$dupN==1))
  dt.stat[dupN>1, startsCap:=(substr1(inpStr)==substr1(upperCase))]
  dt.stat[dupN>1, keep := (startsCap==T | all(startsCap==F)), by=upperCase]
  dt.stat <- dt.stat[keep==T,]

  dt.stat[, dupN:=.N, by=upperCase]
  dt.stat %<>% setorder(upperCase,-inpStr)
  dt.stat[, xN:=seq_len(.N), by=upperCase]
  dt.stat <- dt.stat[xN==1,]
  stopifnotunique(dt.stat$upperCase)

  setkey(dt.stat, upperCase)

  stopifnot(all(dt.inp$upperCase %in% dt.stat$upperCase))


  return(dt.stat[dt.inp$upperCase,inpStr])

}

re.class <- function(x) {class(x) <- c(class(x), 'regex'); return(x)}
re.is    <- function(x) {'regex' %in% class(x);}





dates_test <- function(inpDT, colsKey='Case.ID', colsCheck=names(inpDT) %-% colsKey, colsIgnore=NULL, minDate=as.Date('1910-01-01'), maxDate=as.Date('2100-01-01')){
  colsCheck <- colsCheck %-% colsIgnore;
  if (is.character(colsCheck)) colsCheck <- (colsCheck %inw% names(inpDT))
  outDT <- outDT.char <-
    inpDT[,c(colsKey), with=F]
  for (i in colsCheck){
    this.col <- names(inpDT)[[i]]
    this.vals <- inpDT[[i]]
    this.vals.char <- as.character(this.vals)
    cat('\n',i,this.col)
    if (sum(!is.na(this.vals))==0){
      cat('   Empty or NA only!');
      next;
    }
    if (is.someDate(this.vals)) {
      cat('               It\'s a date already!')
      this.vals.dates <- this.vals
    } else {
      this.vals.dates <- xls_date(this.vals)
      if (!is.someDate(this.vals.dates)) {
        cat('               Could not convert to a date!');
        next;
      } else{
        looks_ok <- F
        if (      'Date' %in% class(this.vals.dates)  ) {minDateComp <- as.Date(   minDate); maxDateComp <- as.Date(   maxDate);}
        if (any(grepl('POSIX',class(this.vals.dates)))) {minDateComp <- as.POSIXct(minDate); maxDateComp <- as.POSIXct(maxDate);}
        looks_ok <- this.vals.dates>minDateComp & this.vals.dates<maxDateComp
      }

      if (sum(looks_ok,na.rm = T)>0) {
        cat('               Looks like a date!')
      } else {cat(' Not a date...'); next;}
    }
    cat(' Ok, working on it');
    outDT.char[[this.col]] <- this.vals.char
    outDT[[this.col]] <- as.character(this.vals.dates)
  }
  # outDT.long <- melt(outDT, id.vars = colsKey, variable.name = 'Column', value.name = 'Date')
  outDT.long <- melt(outDT, id.vars = colsKey, variable.name = 'Column', value.name = 'Date')
  outDT.char.long <- melt(outDT.char, id.vars = colsKey, variable.name = 'Column', value.name = 'Char')
  stopifnot(outDT.long[,c(colsKey,'Column'),with=F] %===% outDT.char.long[,c(colsKey,'Column'),with=F])

  outDT.full <- cbind(outDT.long,outDT.char.long[,.(Char)])
  outDT.full %<>% setcolorderV(c(colsKey,'Column','Char'))
  outDT.full %<>% setorderv(c(colsKey, 'Date'))
  return(outDT.full)
}





va_txt_initials <- function(inpTxt, collapse=''){
  lapply(strsplit(inpTxt, ' '), FUN = function(x){paste0(substr1(x),collapse=collapse)}) %>% unlist
}


# dedup_vals(cs('abc def abc xyz')) -> abc.1 def abc.2 xyz
dedup_vals <- function(inpvec, sep='.'){
  if (sum(duplicated(inpvec))==0) {
  #  message('No duplicate values;');
    invisible(inpvec);
  }
  dupvals <- unique(inpvec[duplicated(inpvec)]);

  for (val in dupvals){
    positions <- which(inpvec==val);
    newvals <- paste0(val,sep,seqlen(positions))
    inpvec[positions] <- newvals
  }
  inpvec
} # e. dedup_vals()



# nbspace <- rawToChar(as.raw(0xA0))
nbspace <- "\u00A0"

