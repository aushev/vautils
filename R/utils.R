detachAllPackages <- function() {lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE);}

pkgparent <- function (fun) {
  nsenv <- topenv(environment(fun))
  environmentName(nsenv)
}


# from https://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                        decreasing=FALSE, head=FALSE, n=5) {
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    objnames <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(objnames, function(x) as.character(class(x))[1])
    obj.mode <- napply(objnames, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.size <- napply(objnames, object.size)
    obj.dim <- t(napply(objnames, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(objnames, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.dim)
    objnames(out) <- c("Type", "Size", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
        out <- head(out, n)
    out
}


# shorthand
lsos <- function(..., n=10) {
    .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}


stopifnotunique <- function(x, allowNULL=F) {
  if (is.null(x) & allowNULL==F) stop('input is NULL!')
  if (anyDuplicated(x)==0) return(invisible());
  dt.tab <- tab(x)
  dt.show <- dt.tab[dt.tab$Freq>1,]
  print(dt.show)
  stop('\nNot unique!')
}
