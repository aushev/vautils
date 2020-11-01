sel_gettext <- function(el){el$getElementText()}

sel_gettexts <- function(els){sapply(els, sel_gettext)}

sel_get_el_by_text <- function(inpRemDr,inpXPath, remove=''){
  el <- inpRemDr$findElement(using = 'xpath', value = inpXPath)
  if (is.null(el)) return(NA);
  ret.val <- unlist(el$getElementText())
  if(remove!='') {ret.val <- gsub(remove,'',ret.val)}
  return(ret.val)
}


rsel_texts <- function(els){
  sapply(els, function(X) unlist(X$getElementText()))
}

rsel_attrs <- function(els, attr){
  sapply(els, function(X) unlist(X$getElementAttribute(attr)))
}

findChildElement_safe <- function(input, using, value){
  result = tryCatch(
    expr = {res <- input$findChildElement(using=using,value=value)},
    warning = function(w) {cat('Warning: ',   w$message, '\n');},
    error = function(e) {cat('Error: ',   e$message, '\n');},
    finally = {
      #cat('Finished! ');
    }
  )
}

findElement_safe <- function(input, using, value){
  result = tryCatch(
    expr = {res <- input$findElement(using=using,value=value)},
    warning = function(w) {cat('Warning: ',   w$message, '\n');},
    error = function(e) {cat('Error: ',   e$message, '\n');},
    finally = {
      #cat('Finished! ');
    }
  )
}

navigate_safe <- function(input, URL){
  result = TRUE;
  result = tryCatch(
    expr = {result <- input$navigate(URL);},
    warning = function(w) {cat('Warning: ',   w$message, '\n');},
    error = function(e) {cat('Error: ',   e$message, '\n'); return(FALSE);},
    finally = {
      #cat('Finished! ');
    }
  )


}


mygettext <- function(el){el$getElementText()}
mygettexts <- function(els){sapply(els, function(el){el$getElementText()})}


get_el_by_text <- function(inpRemDr,inpXPath, remove=''){
  el <- inpRemDr$findElement(using = 'xpath', value = inpXPath)
  if (is.null(el)) return(NA);
  ret.val <- unlist(el$getElementText())
  if(remove!='') {ret.val <- gsub(remove,'',ret.val)}
  return(ret.val)
}

rsel_get_el_text <- function(inpRemDr, inpUsing, using='xpath', remove=''){
  #el <- inpRemDr$findElement(using = using, value = inpUsing)
  el <- findElement_safe(inpRemDr,using,inpUsing)
  if (is.null(el)) return(NA);
  ret.val <- unlist(el$getElementText())
  if(remove!='') {ret.val <- gsub(remove,'',ret.val)}
  return(ret.val)
}
