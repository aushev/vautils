mergesymbols <- function(symbols, ...){

req(quantmod)

  symbols.in <- na.omit(unique(symbols))
  symbols.out <- getSymbols(symbols.in, ...)
  all.data <- NULL

  syms.dats <- lapply(symbols.out, FUN = get, envir=sys.frame(sys.parent(0)))

  for (this.sym in syms.dats){
    all.data %<>% cbind(this.sym)
  }

  return(all.data)
}

mergesymbolsdt <- function(symbols, ...){

req(quantmod)

  symdata <- mergesymbols(symbols, ...)
  dtWide <- data.table(symdata, keep.rownames = T)
  names(dtWide) %<>% gsub('(.*)\\.(.*)','\\2.\\1',.)
  dtLong <- reshape(data = dtWide,
                  varying = names(dtWide[ , -1]),
                  sep = ".",
                  direction = "long")

  setnames(dtLong, cs('index time'), cs('Date Ticker'))

  invisible(dtLong)
}

stocks.convert.continuous <- function(inpDT, nmTicker='Ticker', nmDate='Date', nmOpen='Open', nmClose='Close'){
req(quantmod)

  setnames(inpDT, c(nmTicker, nmDate, nmOpen, nmClose), cs('colTicker colDate colOpen colClose'))
  dt.Open  <- inpDT[,.(Time1=as.POSIXct(paste0(colDate,' 09:30:00')), colTicker, colPrice=colOpen)]
  dt.close <- inpDT[,.(Time1=as.POSIXct(paste0(colDate,' 16:00:00')), colTicker, colPrice=colClose)]
  dt.Time <- rbind(dt.Open, dt.close)
  setnames(dt.Time, cs('colTicker colPrice'), cs('Ticker Price'));
  setnames(inpDT, cs('colTicker colDate colOpen colClose'), c(nmTicker, nmDate, nmOpen, nmClose)); # just reverting to old names in the original table
  return(dt.Time)
}
