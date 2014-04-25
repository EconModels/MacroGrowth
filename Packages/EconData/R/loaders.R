is.in <- function( el, set ) {
  if (is.null(set)) return( rep(TRUE, length(el) ) )
  is.element(el, set)
}

loadResampledData <- function( path, country=NULL, model=NULL, factors=NULL ) {
  files <- dir(path)
  pieces <- strsplit( files, c("_\\."))
  keep <- sapply( pieces, 
                  function(x) { is.in(x[1],country) && is.in(x[2],model) &&  is.in(x[3],factors) } )
  files <- files[ keep ]
  # the next line could be augmented to add additional information to the 
  # data frame.
  dflist <- lapply(files, function(x) load ( file.path(path,file) ) )
  do.call( rbind.fill, dflist )
}