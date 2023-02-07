

txt2obj <- function(txt){
  if (txt=='') {message(' Empty input! '); return(NULL);}
  if (exists(txt)){
    sel_obj <- get(txt)
  } else {
    sel_obj <- eval(parse(text = txt))
  }
  return(sel_obj);
}

getrseltxt <- function(){
  adc <- rstudioapi::getActiveDocumentContext();
  sel <- adc$selection[[1]];
  sel_text <- sel$text;

  if (sel_text=='') {
    msgStr <- 'Empty string. '
    sel_text <- trimws(adc$contents[sel$range$start[[1]]]);
    if (sel_text!='') {
      msgStr <- msgStr %+% ' New string: ' %+% bold(sel_text);
    } else msgStr <- msgStr %+% ' No string found! ';

    message(msgStr)
  }

  return(sel_text);
}


#' Explore selected object:
#'  for ExpressionSet, calls exprs(), fData(), and pData()
#'  for table,         calls RStudio's View() function
#'  for function,      calls debugonce()
#' @return dashes inside RStudio
selectionView <- function(){
  sel_text <- getrseltxt();
  if (sel_text=='') {message(' Nothing selected! '); return(NULL);}
  sel_obj <- txt2obj(sel_text);

  # print(sel_text)

  if ('ExpressionSet' %in% class(sel_obj) | 'RccSet' %in% class(sel_obj) ){
    tmpX <- exprs(sel_obj)
    tmpF <- fData(sel_obj)
    tmpP <- pData(sel_obj)

    View(tmpX, title=paste0(sel_text,'$X'))
    View(tmpF, title=paste0(sel_text,'$F'))
    View(tmpP, title=paste0(sel_text,'$P'))
  } else if (is.function(sel_obj)) {
    message('Function ', sel_text, '() will be debugged once. ')
    debugonce(sel_obj)
  } else {
#    message('Table will be Viewed. ')
    View(sel_obj, title=sel_text)
  }
} # e. selectionView

#' Calls tab() for selected object
selectionTab <- function(){
  sel_text <- getrseltxt();
  sel_obj <- txt2obj(sel_text);

#  message('\nselectionTab v2 called. ')
  #if ('data.frame' %in% class(sel_obj)) {
  if (grepl('tab\\(',sel_text)) {
    this_tab <- sel_obj
  } else this_tab <- tab(sel_obj)

  View(this_tab, title=sel_text)

} # e. selectionView



expFun <- function(){
  message('Debugging!!!')
  tmp.ADC <<- rstudioapi::getActiveDocumentContext()
  tmp.prj <<- rstudioapi::getActiveProject()
  tmp.SEC <<- rstudioapi::getSourceEditorContext()
  tmp.CEC <<- rstudioapi::getConsoleEditorContext()
  message(tmp.ADC$id)
}

clipboard2selected <- function(){
  execute <- TRUE;
  sel_text <- getrseltxt();
  if (sel_text=='') {sel_text <- 'list1'; if (exists('list1')) execute <- FALSE;}
  rstudioapi::sendToConsole(sel_text %+% ' <- readClipboard()', execute = execute)
}

