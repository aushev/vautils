#' Explore selected object:
#'  for table, calls RStudio's View() function
#'  for function, calls debugonce()
#' @return dashes inside RStudio
selectionView <- function(){
  context <- rstudioapi::getActiveDocumentContext()
  sel_text <- context$selection[[1]]$text

#  message('\nselectionView called. ')

  if (exists(sel_text)){
    sel_obj <- get(sel_text)
  } else {
    sel_obj <- eval(parse(text = sel_text))
  }

  # print(sel_text)

  if ('ExpressionSet' %in% class(sel_obj) | 'RccSet' %in% class(sel_obj) ){
    tmpX <- exprs(sel_obj)
    tmpF <- fData(sel_obj)
    tmpP <- pData(sel_obj)

    View(tmpX, title=paste0(sel_text,'$X'))
    View(tmpF, title=paste0(sel_text,'$F'))
    View(tmpP, title=paste0(sel_text,'$P'))
  } else if (is.function(sel_obj)) {
    message('Function will be debugged once. ')
    debugonce(sel_obj)
  } else {
#    message('Table will be Viewed. ')
    View(sel_obj, title=sel_text)
  }
}

