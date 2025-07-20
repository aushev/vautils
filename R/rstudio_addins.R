

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
  if (sel_text=='') {sel_text <- 'vecB'; if (exists('vecB')) execute <- FALSE; message('Variable already exists, check before executing.')}
  rstudioapi::sendToConsole(sel_text %+% ' <- fromClip()', execute = execute)
}

selected2clipboard <- function(){
  execute <- TRUE;
  sel_text <- getrseltxt();
#  if (sel_text=='') {sel_text <- 'list1'; if (exists('list1')) execute <- FALSE;}
  rstudioapi::sendToConsole('(' %+% sel_text %+% ') %>% toClip()', execute = execute)
}

flexread_clip <- function(fnOri=fromClip(), obj_open='dt1', write_code=T){
  checkPlus()
  if (file.exists(fnOri)){
    message('Existing file found! \nExtension: ' %+% tools::file_ext(fnOri))
    txt2inp <- sprintf("fn1 <- '%s';\n", gsub('\\\\','/',fnOri))
    txt2inp <- txt2inp %+% 'dt1 <- flexread(fn1, deluseless = T)\n'
    if (tools::file_ext(fnOri) %~~i% 'Rdat'){
      message('RDat file')
      txt2inp <- sprintf("loadv('%s')\n", gsub('\\\\','/',fnOri))
      obj_open <- NA
    }

  } else {
    print('Not a file!')
    fnOri <- gsub('.*/([_a-zA-Z0-9\\-]{30,})/.*','\\1',fnOri)
    txt2inp <- 'fn1 <- ' %+% "as_id('" %+% fnOri %+% "')\n"
    txt2inp <- txt2inp %+% 'dt1 <- flexread(fn1, deluseless = T)\n'

  }
  if (write_code==T) rstudioapi::insertText(NULL, txt2inp)
  # print(str(txt2inp))
  # message('YYY')
  rstudioapi::sendToConsole(txt2inp)
  if (not.na(obj_open)) rstudioapi::sendToConsole('duView(dt1)')
}



duView <- function(x, columns=NULL,ignoreColumns=columns, title=NULL, n=0) {
  dt.tmp <- x;
  if (nrow(dt.tmp)==0) {warning('No records in the input table!'); return(NULL);}

  dt.tmp <- dt_deluselesscols(setcolorderV(dt.tmp,columns), ignoreColumns = ignoreColumns)
  if (nrow(dt.tmp)==0) {warning('No records left in the table!'); return(NULL);}
  if (is.null(title)) title <- deparse(substitute(x));
  View(dt.tmp, title = title)
}

duView <- function(dtIn, ..., ignoreColumns=NULL, title = NULL, n=0) {
  dots_raw <- substitute(list(...))[-1]  # capture unevaluated ...
  # browser()

  dt.duView <<- copy(dtIn)
  if (nrow(dt.duView) == 0) {
    warning("No records in the input table!")
    return(NULL)
  }

  if (n>0) dt.duView %<>% head(n)
  if (n<0) dt.duView %<>% tail(-n)


  # CASE 1: No columns specified
  if (length(dots_raw) == 0) {
    col_names <- NULL

    # CASE 2: Single character vector like c("a", "b")
  } else {
    # Evaluate the first argument to see if it's a character vector
    first_eval <- try(eval(dots_raw[[1]], envir = parent.frame()), silent = TRUE)
    if (length(dots_raw) == 1 && !inherits(first_eval, "try-error") && is.character(first_eval)) {
      # Single character vector: quoted column names
      col_names <- first_eval

    } else if (all(sapply(dots_raw, is.name))) {
      # Unquoted column names
      col_names <- sapply(dots_raw, deparse)

    } else stop("Invalid column specification: use unquoted names or a single character vector.")
  }

  col_names %<>% unique()
  cols_notfound <- col_names %-% names(dt.duView)

  col_names <- col_names %&% names(dt.duView)

  if (!is.null(col_names)) {
    setcolorder(dt.duView, neworder = col_names)
  }

  ignoreColumns %<>% c(col_names)
  dt.duView %<>% dt_deluselesscols(ignoreColumns = ignoreColumns)

  if (is.null(title)) {
    title <- deparse(substitute(dtIn))
  }

  View(dt.duView, title = title)
} # e. duView()

tView <- function(x) {
  dt.tmp <<-  as.data.table(t(x), keep.rownames=T);
  title <- deparse(substitute(x));
  if (title=='.') title <- 'dt.tmp'
  View(dt.tmp, title = title)
}

