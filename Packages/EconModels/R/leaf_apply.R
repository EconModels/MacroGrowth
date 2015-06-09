#' Applies a function to leaves of a list.
#' 
#' This function applies a function to leaves of a nested list (depth-first) if and only if the 
#' class of the leaf matches one of the names in \code{class}.
#' 
#' @param l the list to be traversed.
#' @param f the function to be applied to the leaves of the list. 
#' @param class a vector of class names.  All and only objects inheriting from any of 
#' these will be considered a leaf, even if they are also lists.   
#' Any objects that are neither lists nor inherit from \code{class} will be ignored.
#' @param strict.lists a logical.  If \code{TRUE}, an item will only be considered a list
#' if \code{"list"} is the first among its classes.  Setting this to \code{TRUE} may be 
#' simpler than enumeratin all of the possible leaf classes using \code{class}.
#' @param is.parent a function of an item and item name that returns a logical
#' indicating whether a node is a leaf.  See the default vaule for examples.
#' @param name a prefix for the names in the resulting list
#' @param sep the separator to be used when creating the names of the leaves from the list nesting.
#' @return a flat, named list of the results of applying \code{f} to the leaves of \code{l}.
#' @details \code{f} must be of the form \code{f(item, item_name)} where 
#' \code{item} is a member of \code{l} and 
#' \code{item_name} is the concatenated name of \code{item} created 
#' by applying \code{sep} between node names in the list.
#' @export
leaf_apply <- 
  function ( 
    l, f, class=NULL, name="", sep=".", 
    strict.lists = FALSE, 
    is.parent = if (strict.lists) 
      function(x, n, ...) { class(x)[1] == "list" }
    else 
      function(x, n, ...) { is.list(x) && (is.null(class) || !inherits(x, as.character(class))) }
  ){                      
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
    
    if (is.parent(item, item_name)) {
      # descend
      res <- c(res, leaf_apply(item, f, class=class, name=item_name))
    } else {
      if (is.null(class) || inherits(item, as.character(class))) {
        # process leaf
        res <- c(res, list(f(item, item_name)))
        names(res)[length(res)] <- item_name
      }
    }
  }
  res
}