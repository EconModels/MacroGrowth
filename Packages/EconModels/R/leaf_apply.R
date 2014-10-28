#' Applies a function to leaves of a list.
#' 
#' This function applies a function to leaves of a nested list (depth-first) if and only if the 
#' class of the leaf matches one of the names in \code{class}.
#' 
#' @param l the list to be traversed.
#' @param f the function to be applied to the leaves of the list. 
#' @param class a list of classes to be matched.
#' @param name a prefix for the names in the resulting list
#' @param sep the separator to be used when creating the names of the leaves from the list nesting.
#' @return a flat, named list of the results of applying \code{f} to the leaves of \code{l}.
#' @export
leaf_apply <- function ( l, f, class=NULL, name="", sep="." ) {
  res <- list()
  list_names <- names(l)
  if (is.null(list_names)) {
    list_names <- as.character(1L: length(l)) 
  }
  idx <- 0L
  while(idx < length(l)) {
    idx <- idx + 1
    item <- l[[idx]]
    if (nchar(name) > 0) {
      item_name <- paste(name, list_names[idx], sep=sep)
    } else {
      item_name <- list_names[idx]
    }
    
    if (is.null(class)) {
      if (is.list(item)) {
        res <- c(res, leaf_apply(item, f, class=NULL, name=item_name))
      } else {
        res <- c(res, list(f(item, item_name)))
        names(res)[length(res)] <- item_name
      }
    } else {
      if ( length(intersect(class(item) , class)) > 0L ) {
        res <- c(res, list(f(item, item_name)))
        names(res)[length(res)] <- item_name
      } else {
        if (is.list(item)) { res <- c(res, leaf_apply(item, f, class=class, name=item_name)) }
      }
    }
  }
  res
}