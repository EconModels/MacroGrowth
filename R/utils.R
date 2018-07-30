#' Quantiles from Data
#'
#' A thin wrapper around \code{\link{quantile}}



# Provides access to data melting. See http://cran.r-project.org/web/packages/reshape2/reshape2.pdf
# tikz allows use of LaTeX formatting and font in graphs. Allows for a consistent look across the paper.
# See http://r-forge.r-project.org/R/?group_id=440 for instructions on installing tikzDevice.
# require(tikzDevice) 
# source("Graphics.R")



#' @param p a vector of probabilities
#' @param vals a numeric vector or a name of a variable in \code{data}
#' @param data a data frame
#' @param ... addtional arguments passed to \code{\link{quantile}}
#' @return a vector of quantiles
#' @export
#' @examples
#' myqdata( .5, eruptions, data=faithful)

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

as_name_or_null <- function(x) {
  tryCatch( as.name(x), error = function(e) NULL )
}
