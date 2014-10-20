
leaf_apply <- function ( l, f, class=NULL, name="", sep="." ) {
  res <- list()
  list_names <- names(l)
  for (idx in 1:length(l)) {
    item <- l[[idx]]
    item_name <- paste0(name, list_names[idx], sep=sep)
    if (is.null(class)) {
      if (is.list(item)) {
        res <- c(res, leaf_apply(item, f, class=NULL, name=item_name))
      } else {
        res <- c(res, list(f(item)))
        names(res)[length(res)] <- item_name
      }
    } else {
      if ( length(intersect(class(item) , class)) > 0L ) {
        res <- c(res, list(f(item)))
        names(res)[length(res)] <- item_name
      } else {
        if (is.list(item)) { res <- c(res, leaf_apply(item, f, class=class, name=item_name)) }
      }
    }
  }
  res
}