#' Calls RStudio's View() function
#' for the selected text
#' @return dashes inside RStudio
selectionView <- function(){
  context <- rstudioapi::getActiveDocumentContext()
  sel_text <- context$selection[[1]]$text
  # print(sel_text)
  View(get(sel_text), title=sel_text)
}

