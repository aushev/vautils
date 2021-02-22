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

  return(fn.latest)
}



