va_xpathApply <- function(inpNode, inpPath, delim='; '){ # former my.xpathApply
  rez.list <- sapply(inpNode, FUN=XML::xpathApply, path=inpPath, fun=XML::xmlValue)
  empty <- sapply(sapply(rez.list, unlist), is.null)
  rez.list[empty] <- NA

  rez.list <- sapply(rez.list, unlist)

  nested <- (sapply(rez.list, length) > 1)

  rez.list[nested] <- sapply(rez.list[nested], paste, collapse=delim)

  return(unlist(rez.list))
}


xml_text_with_linebreaks <- function(node) {
  out <- character()

  for (child in xml_contents(node)) {
    type <- xml_type(child)

    if (type == "text") {
      out <- c(out, xml_text(child))
    } else if (type == "element" && xml_name(child) == "br") {
      out <- c(out, "\n")
    } else {
      # Recursively process nested elements if needed
      out <- c(out, xml_text_with_linebreaks(child))
    }
  }

  paste(out, collapse = "")
}


# Get readable XML path as concatenated tags
get_tag_path <- function(node, attributes=TRUE) {
  tags <- list()

  while (!is.null(node) && !xml2::xml_name(node) %in% c(NA, "")) {
    tag_name <- xml2::xml_name(node)
    attrs <- xml2::xml_attrs(node)

    # Format attributes if any
    if (attributes==TRUE & length(attrs) > 0) {
      attr_str <- paste(
        paste0(names(attrs), '="', attrs, '"'),
        collapse = " "
      )
      tag <- paste0("<", tag_name, " ", attr_str, ">")
    } else {
      tag <- paste0("<", tag_name, ">")
    }

    tags <- c(tag, tags)
    node <- xml2::xml_parent(node)
  }

  paste(tags, collapse = " ")
}

xml_find_nodes_with_text <- function(doc, search_string, ignore_case = TRUE) {
  all_nodes <- xml_find_all(doc, ".//*")

  match_func <- if (ignore_case==T) function(...) grepl(..., ignore.case = TRUE) else function(...) grepl(..., ignore.case = FALSE, fixed = TRUE)

  filter_func <- function(node) {
    children <- xml_contents(node)
    text_nodes <- children[xml2::xml_type(children) == "text"]
    any(match_func(search_string, as.character(text_nodes)))
  }

  # Keep only nodes that have a direct text node child matching the string
  matched <- Filter(filter_func, all_nodes)

}

xml_attr2dt <- function(node){
  attrs <- xml_attrs(node)
  dt.attrs <- as.data.table(as.list(attrs))
  return(dt.attrs)
}

xml_find_parent <- function(node, tagname){
  proceed <- TRUE
  nodeP <- xml_parent(node)
  while (proceed) {
    tagnameP <- xml_name(nodeP)
    # cat('\n', bold(tagnameP))
    if (tagnameP=='') break
    if (tagnameP==tagname) break
    # browser()
    nodeP <- xml_parent(nodeP)
  }

  if (tagnameP=='') return(NULL)
  return(nodeP)
}
