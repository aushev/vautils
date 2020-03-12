#' Calls RStudio's View() function
#' for the selected text
#' @return dashes inside RStudio
selectionView <- function(){
  context <- rstudioapi::getActiveDocumentContext()
  sel_text <- context$selection[[1]]$text
  sel_obj <- get(sel_text)
  # print(sel_text)

  if ('ExpressionSet' %in% class(sel_obj)){
    tmpX <- exprs(sel_obj)
    tmpF <- fData(sel_obj)
    tmpP <- pData(sel_obj)

    View(tmpX, title=paste0(sel_text,'$X'))
    View(tmpF, title=paste0(sel_text,'$F'))
    View(tmpP, title=paste0(sel_text,'$P'))
  } else {
    View(sel_obj, title=sel_text)
  }
}

