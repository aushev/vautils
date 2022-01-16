testab1 <- function(){
  print ('a' %+% 'b')
}

testab2 <- function(inpA, inpB){
  print (inpA %+% inpB)
}



cs <- function(inputstr, sep=",", fix=T, nonewlines=T){
  if (missing(sep) & grepl(',', inputstr[[1]])==F & grepl(' ', inputstr[[1]])==T) {sep=" ";}
  if (nonewlines) inputstr <- gsub("[\n\r]+", sep, inputstr);
  rez <- unlist(strsplit(inputstr, sep, fixed=fix));
  return(rez[rez!=""]);
}

printcs <- cs1 <- function(input, collapse=' '){paste0(cs(input), collapse = collapse)}



catpastelist <- function(inp){
  if (is.data.frame(inp)) inp <- names(inp);
  cat(paste(inp,collapse = '\n'))
}


rightstr <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
substrRight <- rightstr

substr1 <- function(inpStr) {substring(inpStr,1,1)}

strpad <- function(inpstr,padW,padSide='right'){
  pads <- strrep(' ',padW - nchar(inpstr));
  switch(padSide, right = paste0(inpstr,pads), left = paste0(pads, inpstr))
}


print_list <- function(inp) {
  for (i in seqlen(inp)){
    .member <- inp[i];
    cat(i,
        stringr::str_pad(names(.member), max(nchar(names(inp)))),
        unlist(unname(.member)),
        #str_pad(unlist(unname(fn[i])), max(nchar(unlist(unname(fn))))),
        '\n')
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


`%+%` <- function(...) UseMethod("%+%")
`%+%.character` <- paste0
`%+%.numeric` <- paste0
`%+%.default` <- function (arg1, arg2){
  e <- parent.env(getEnvByName(.GlobalEnv,'package:vautils'));
  if (exists('%+%', envir = e)) get('%+%',envir = e)(arg1,arg2);
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


xls_date <- function(input, strict=F){
  if ('Date' %in% class(input)) return(input);
  inputNum <- suppressWarnings(as.numeric(input));
  notNums <- is.na(inputNum)

  output <- as.Date(inputNum, origin="1899-12-30", optional=T);

  vec4date <- notNums & (nchar(input)>7)
  vec4date[is.na(vec4date)] <- FALSE


  # dt_debug <-  data.table(inp=input, inpN=inputNum, notn=notNums, outp=output)
  # sapply(input[vec4date], function(X){cat(X,'. '); a=as.Date(X);})

  output[vec4date] <- as.Date(input[vec4date], optional=T)

  return(output)
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
shrink_values <- function(values, collapse=';', all=F, dropNA=T, exclude=NULL, fill=NULL){
  values2 <- values;

  if (all==F) values2 <- unique(values);
  if (dropNA==T) values2 <- na.omit(values2);
  if (length(exclude)>0) values2 <- values2[values2 %!in% exclude];

  if (length(values2)==1) return(values2);
  if (length(values2)==0) return(ifelse(is.null(fill),values[1],fill));


  paste(values2, collapse = collapse)
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

#`%like%` <- function(hay, needle){grepl(needle,hay)}
`%~~%` <- function(hay, needle){grepl(needle,hay)}
`%~~i%` <- function(hay, needle){grepl(needle,hay,ignore.case = T)}
`%!~~%` <- function(hay, needle){!grepl(needle,hay)}
`%!~~i%` <- function(hay, needle){!grepl(needle,hay,ignore.case = T)}


nicedate <- function(inpDate=Sys.time()) format(inpDate, '%Y%m%d_%Hh%Mm%Ss')
