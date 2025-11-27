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


xml_find_first_with_attr <- function(node, attr, return_attr=FALSE, direct_only=TRUE) {
  if (!direct_only) stop('not implemented yet!')
  # Get all direct children
  children <- xml_children(node)

  node_found <- NULL
  attr_found <- NA_character_
  # Iterate and find the first with the attribute requested
  for (this_child in children) {
    this_attr <- xml_attr(this_child, attr)
    if (!is.na(this_attr)) {
      node_found <- this_child
      attr_found <- this_attr
      break
    }
  }

  if (return_attr) return(attr_found) else return(node_found)
}



xml_find_all_with_attr <- function(node, attr, return_attr=FALSE, direct_only=TRUE) {
  if (!direct_only) stop('not implemented yet!')
  # Get all direct children
  children <- xml_children(node)

  node_found <- NULL
  attr_found <- NA_character_
  # Iterate and find the first with the attribute requested
  for (this_child in children) {
    this_attr <- xml_attr(this_child, attr)
    if (!is.na(this_attr)) {
      node_found <- this_child
      attr_found <- this_attr
      break
    }
  }

  if (return_attr) return(attr_found) else return(node_found)
}


read_xml_safe <- function(file_path,...) {
  if (file.size(file_path)==0) return(NULL);
  tryCatch(
    {
      read_xml(file_path, ...)
    },
    error = function(e) {
      message("Failed to read XML ",file_path, '\n', conditionMessage(e))
      return(NULL)
    }
  )
}

xml_ns_custom <- function(doc, uri){
  ns_uri <- unname(uri)
  # If you want to be defensive, you can try to read whatever the root declares:
  root    <- xml_root(doc)
  root_ns <- xml_ns(root)
  # If a default namespace exists, prefer it
  if ("d1" %in% names(root_ns)) {
    ns_uri <- unname(root_ns[["d1"]])
  }
  ret_obj <- c(ns1=ns_uri)
  if (!is.null(names(uri)))
    names(ret_obj) <- names(uri)
  ret_obj
}





xml_get_xpath <- function(node, reverse=F) {
  stopifnot(inherits(node, "xml_node"))

  parts <- character()
  cur <- node

#  while (!xml2::xml_is_document(cur)) {
  while (!inherits(cur, "xml_document")) {
    tagname <- xml2::xml_name(cur)
    if (tagname=='') break;
#    cat('\n', red(tagname))
#browser()
    # index among siblings of the same name
#    siblings <- xml2::xml_find_all(xml2::xml_parent(cur), tagname)
#    siblings <- xml2::xml_siblings(cur)
    siblings <- xml2::xml_children(xml_parent(cur))
    siblings_tagnames <- sapply(siblings, xml_name,simplify = T, USE.NAMES = F)
    siblings <- siblings[siblings_tagnames==tagname]
    if (length(siblings) > 1) {
#      cat('\t', blue(length(siblings)))
      # browser()
      # find position
      pos <- which(vapply(siblings, identical, logical(1), cur))
      parts <- c(sprintf("%s[%d]", tagname, pos), parts)
    } else {
      parts <- c(tagname, parts)
    }

    cur <- xml2::xml_parent(cur)
  }
  if (reverse==T) parts %<>% rev()
  paste0("/", paste(parts, collapse = "/"))
}





