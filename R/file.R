file_renameR <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.rename(from = from,  to = to)
}


`%path%` <- file.path


file_findRecent <- function(
    pattern,
    dirs,
    orderstrict = TRUE,
    recursive   = FALSE,
    include.dirs= FALSE,
    ignore.case = TRUE,
    exclude.pattern='^~\\$',
    ...){
  found <- FALSE
  time.latest <- NA
  fn.latest <- NA
#  browser()
  for (this.dir in dirs){
    this.files <- list.files(path = this.dir, pattern = pattern, full.names = T, recursive=recursive, include.dirs = include.dirs, ignore.case=ignore.case, ...)
    this.files <- this.files[grep(exclude.pattern,basename(this.files),invert=T)]
    if (length(this.files) > 0) {found <- TRUE} else next;
    this.times <- sapply(this.files, file.mtime)
    this.islatest <- (this.times==max(c(this.times,time.latest), na.rm = T))
    if (sum(this.islatest)==0) next;
    fn.latest <- this.files[this.islatest][[1]]
    time.latest <- file.mtime(fn.latest)
    if (found==T & orderstrict==T) break;
  }

  if (found==F) warning('File not found: ' %+% bold(pattern) %+% ' in ' %+% bold(dirs))

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



#' Build a safe output file path
#'
#' Combines a directory and a file name into a path that is safe to write to:
#' the file name is sanitized, the directory part is kept intact, and an
#' existing file is not silently overwritten.
#'
#' `fs::path_sanitize()` strips slashes, so applying it to a full path such as
#' `"out/x.pdf"` yields the file name `"outx.pdf"`. `file_safeSavePath()` splits the
#' path first and sanitizes only the file name.
#'
#' @param fn Character scalar. File name or path, e.g. `"plot.pdf"` or
#'   `"results/plot.pdf"`. Surrounding whitespace is removed.
#' @param dir Character scalar or `NULL`. Output directory. If `NULL`
#'   (default), the directory part of `fn` is used (`"."` if there is none).
#'   If given, it takes precedence over the directory part of `fn`.
#' @param on_exists What to do if the file already exists:
#'   * `"timestamp"` (default): warn and prefix the file name with
#'     [nicedate()], e.g. `20261009_14h05m33s_plot.pdf`.
#'   * `"overwrite"`: return the path unchanged.
#'   * `"error"`: stop.
#' @param create_dir Logical. Create `dir` (recursively) if it does not exist?
#'   Default `FALSE`.
#'
#' @return Character scalar: the normalized path (no leading `./`). The file
#'   itself is not created.
#'
#' @seealso [file_launch()], [file_renameR()], [nicedate()]
#'
#' @examples
#' \dontrun{
#' file_safeSavePath("  report: v2?.pdf ")          # "report v2.pdf"
#' file_safeSavePath("out/plot.pdf")                # "out/plot.pdf"
#' file_safeSavePath("plot.pdf", dir = "figs")      # "figs/plot.pdf"
#' file_safeSavePath("plot.pdf", on_exists = "overwrite")
#' }
#' @export
file_safeSavePath <- function(
    fn,
    dir = NULL,
    on_exists = c("timestamp", "overwrite", "error"),
    create_dir = FALSE) {

  on_exists <- match.arg(on_exists)

  fn   <- trimws(fn)
  name <- fs::path_sanitize(fs::path_file(fn))
  dir  <- if (is.null(dir)) fs::path_dir(fn) else dir
  path <- fs::path_norm(fs::path(dir, name))

  if (create_dir && !dir.exists(dir)) dir.create(dir, recursive = TRUE)

  if (file.exists(path)) {
    switch(
      on_exists,
      error     = stop("File already exists: ", path, call. = FALSE),
      overwrite = NULL,
      timestamp = {
        warning("File already exists! Will try to save under different name.")
        path <- fs::path_norm(fs::path(dir, paste0(nicedate(), name)))
      }
    )
  }
  as.character(path)
}


#' Open a file with the system default application
#'
#' Cross-platform replacement for the `system('cmd /C <file>')` pattern used in
#' [inexcel()], [inexcel2()] and [ggsaveopen()]. Uses `shell.exec()` on
#' Windows (handles spaces and special characters such as `&`), `open` on
#' macOS and `xdg-open` on Linux. Does not wait for the application to exit.
#'
#' @param path Character scalar. Existing file.
#'
#' @return `path` (normalized), invisibly.
#' @seealso [file_safeSavePath()]
#' @export
file_launch <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)
  if (.Platform$OS.type == "windows") {
    shell.exec(path)
  } else {
    opener <- if (Sys.info()[["sysname"]] == "Darwin") "open" else "xdg-open"
    system2(opener, shQuote(path), wait = FALSE)
  }
  invisible(path)
}
