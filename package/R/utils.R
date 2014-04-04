


#' @export
myqdata <- function( p, vals, data = parent.frame(), ...)  {
  quantile( eval(substitute(vals), data, parent.frame() ), p, ... )
}

#' @export
safeMatchArg <- function(arg, choices, several.ok=FALSE) {
  return( tryCatch( match.arg( arg, choices, several.ok=several.ok), 
                    error=function(e) { choices[1] } )
  )
}

#' @export
safeDF <- function(object, nrow=1, ncol=0){
  if (is.null(object)){
    return (data.frame(matrix(nrow=nrow, ncol=ncol)))    
  }
  emptyDataFrame <- FALSE
  if (inherits(object,"data.frame") ) {
    if (nrow(object) + ncol(object) > 0){
      # We're already a data.frame.
      return (object)
    }
    emptyDataFrame <- TRUE
  }
  if ( emptyDataFrame ) {
    return (data.frame(matrix(nrow=nrow, ncol=ncol)))
  }
  return(as.data.frame(object))
}
