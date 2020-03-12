#' Insert dashes from courser position to up to 80 characters
#'
#' @return dashes inside RStudio
selectionView <- function(){
  # set limit to which position dashes should be included
  nchars <- 81

  # grab current document information
  context <- rstudioapi::getActiveDocumentContext()
  # extract horizontal courser position in document
  context_col <- context$selection[[1]]$range$end["column"]

  tmp <- context$selection[[1]]$text

  print(tmp)
  View(tmp)
}

# selectionView()


