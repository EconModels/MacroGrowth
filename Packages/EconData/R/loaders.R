is.in <- function( el, set ) {
  if (is.null(set)) return( rep(TRUE, length(el) ) )
  is.element(el, set)
}

loadResampledData <- function( path, country=NULL, model=NULL, factors=NULL, sep="_" ) {
  files <- dir(path)
  # Remove any files that begin with "models_". These are model files, not coefficient files.
  toRemove <- grepl(pattern="models_", x=files, fixed=TRUE)
  files <- files[!toRemove]
  # Remove the .Rdata suffix from the files.
  names <- gsub(pattern=".Rdata", replacement="", x=files, fixed=TRUE)
  pieces <- strsplit( x=names, split=sep )
  keep <- sapply( pieces, 
                  function(x) { is.in(x[1],country) && is.in(x[2],model) &&  is.in(x[3],factors) } )
  files <- files[ keep ]
  # the next line could be augmented to add additional information to the 
  # data frame.
  dflist <- lapply(files, function(x) { 
                              readRDS ( file.path(path, x) ) 
                          }
                   )
  do.call( rbind.fill, dflist )
}