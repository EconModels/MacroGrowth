
leaf_apply <- function ( l, f, class=NULL, name="", sep="." ) {
  res <- list()
  list_names <- names(l)
  if (is.null(list_names)) {
    list_names <- as.character(1L: length(l)) 
  }
  for (idx in 1:length(l)) {
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

extract_resid <- function(x, name) {
  data.frame(resid = resid(x), name=name)
}