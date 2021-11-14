file_renameR <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.rename(from = from,  to = to)
}

`%path%` <- file.path


file_findRecent <- function(pattern, dirs, orderstrict=T, recursive=F, include.dirs=F, ...){
  found <- FALSE
  time.latest <- NA
  fn.latest <- NA
  for (this.dir in dirs){
    this.files <- list.files(path = this.dir, pattern = pattern, full.names = T, recursive=recursive, include.dirs = include.dirs, ...)
    if (length(this.files) > 0) {found <- TRUE} else next;
    this.times <- sapply(this.files, file.mtime)
    this.islatest <- (this.times==max(c(this.times,time.latest), na.rm = T))
    if (sum(this.islatest)==0) next;
    fn.latest <- this.files[this.islatest][[1]]
    time.latest <- file.mtime(fn.latest)
    if (found==T & orderstrict==T) break;
  }

  if (found==F) warning('File not found!')

  message(fn.latest)
  return(fn.latest)
}



dir.createS <- function(paths, showWarnings=F, recursive=T, ...){
  if (length(paths)==0) return;
  cat(' Creating dirs: asked', length(paths))
  paths <- unique(paths);
  cat('; unique ',length(paths))
  paths <- paths[!dir.exists(paths)]
  cat('; to create ',length(paths))
  rez <- sapply(paths, dir.create, showWarnings=showWarnings, recursive=recursive, ...)
  cat('; success ',sum(unlist(rez)),'. ')

}

dt_file_list <- function(list.fndirs, recursive=T, full.names=T, ...){
  fns <- list.files(list.fndirs, full.names=full.names, recursive=recursive, no.. = T, ...)
  dt.rez <- data.table(fn=fns)

  dt.rez[, bfn:=basename(fn)]
  dt.rez[, dir:=dirname(fn)]
  dt.rez[, bdir:=basename(dirname(fn))]

  return(dt.rez)
}
