sel_gettext <- function(el){el$getElementText()}

sel_gettexts <- function(els){sapply(els, sel_gettext)}

sel_get_el_by_text <- function(inpRemDr,inpXPath, remove=''){
  el <- inpRemDr$findElement(using = 'xpath', value = inpXPath)
  if (is.null(el)) return(NA);
  ret.val <- unlist(el$getElementText())
  if(remove!='') {ret.val <- gsub(remove,'',ret.val)}
  return(ret.val)
}
