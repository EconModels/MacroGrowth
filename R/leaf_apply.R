#' Apply a function to leaves of a list.
#'
#' This function applies a function to leaves of a nested list (depth-first) if and only if the
#' class of the leaf matches one of the names in `class`.
#'
#' @param l the list to be traversed.
#' @param f the function to be applied to the leaves of the list.
#' @param class a vector of class names (as quoted strings).
#' All and only objects inheriting from any of
#' these will be considered a leaf, even if they are also lists.
#' Any objects that are neither lists nor inherit from `class` will be ignored.
#' @param strict.lists a logical.  If `TRUE`, an item will only be considered a list
#' if `"list"` is the first among its classes.  Setting this to `TRUE` may be
#' simpler than enumerating all of the possible leaf classes using `class`.
#' @param is.parent a function of an item and item name that returns a logical
#' indicating whether a node is a leaf.  See the default vaule for examples.
#' @param name a prefix for the names in the resulting list
#' @param sep the separator to be used when creating the names of the leaves from the list nesting.
#' @return a flat, named list of the results of applying `f` to the leaves of `l`.
#' @details `f` must be of the form `f(item, item_name)` where
#' `item` is a member of `l` and
#' `item_name` is the concatenated name of `item` created
#' by applying `sep` between node names in the list.
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
      res <- c(res, leaf_apply(item, f, name=item_name, is.parent=is.parent, class=class))
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