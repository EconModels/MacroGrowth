is.in <- function( el, set ) {
  if (is.null(set)) return( rep(TRUE, length(el) ) )
  is.element(el, set)
}

#' Loads resampled coefficients from files
#' 
#' @param path the directory containing the resampled coefficient files
#' @param country a country or vector of countries.
#' @param model a model or vector of models.
#' @param factors strings of factors used in fitting the models.
#' @param sep the separator in the file names between country, model, and factors
#' @return a string representing the energyType that was used for the fitting. NA if the sfModel was used.
#' @export
loadResampledData <- function( path, country=NULL, model=NULL, factors=NULL, sep="_" ) {
  files <- dir(path)
  # Remove any files that contains the string "models_". These are model files, not coefficient files.
  toRemove <- grepl(pattern="models_", x=files, fixed=TRUE)
  files <- files[!toRemove]
  # Remove the .Rdata suffix from the file names.
  names <- gsub(pattern=".Rdata", replacement="", x=files, fixed=TRUE)
  pieces <- strsplit( x=names, split=sep )
  keep <- sapply( pieces, 
                  function(x) { is.in(x[1],country) && is.in(x[2],model) &&  is.in(x[3],factors) } )
  files <- files[ keep ]
  # the next line could be augmented to add additional information to the 
  # data frame.
  dflist <- lapply(files, function(x) { readRDS ( file.path(path, x) ) } )
  do.call( rbind.fill, dflist )
}