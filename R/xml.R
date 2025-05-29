va_xpathApply <- function(inpNode, inpPath, delim='; '){ # former my.xpathApply
  rez.list <- sapply(inpNode, FUN=xpathApply, path=inpPath, fun=xmlValue)
  empty <- sapply(sapply(rez.list, unlist), is.null)
  rez.list[empty] <- NA
  
  rez.list <- sapply(rez.list, unlist)
  
  nested <- (sapply(rez.list, length) > 1)
  
  rez.list[nested] <- sapply(rez.list[nested], paste, collapse=delim)
  
  return(unlist(rez.list))
}


