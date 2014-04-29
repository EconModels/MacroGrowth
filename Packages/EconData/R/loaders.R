is.in <- function( el, set ) {
  if (is.null(set)) return( rep(TRUE, length(el) ) )
  is.element(el, set)
}

#' Loads resampled coefficients from files
#' 
#' @param path the directory containing the resampled coefficient files. Do not add a trailing file separator.
#' @param archive a zip archive containing resampled coefficient files
#' @param country a country or vector of countries.
#' @param model a model or vector of models.
#' @param factors strings of factors used in fitting the models.
#' @param sep the separator in the file names between country, model, and factors
#' @return a string representing the energyType that was used for the fitting. NA if the sfModel was used.
#' @export
loadResampledData <- function( path, archive=NULL, country=NULL, model=NULL, factors=NULL, sep="_" ) {
  if (is.null(archive)){
    files <- dir(path)
  } else {
    filesDF <- unzip(zipfile=archive, list=TRUE)
    files <- filesDF[["Name"]]
    # Keep only those files with the ".Rdata" extension
    files <- files[grepl(pattern=".Rdata", x=files, fixed=TRUE)]
  }
  # Remove any files that contains the string "models_". These are model files, not coefficient files.
  toRemove <- grepl(pattern="models_", x=files, fixed=TRUE)
  files <- files[!toRemove]
  # Remove the .Rdata suffix from the file names.
  names <- gsub(pattern=".Rdata", replacement="", x=files, fixed=TRUE)
  # Remove the path prefix from the file names, if present
  names <- gsub(pattern=path, replacement="", x=names, fixed=TRUE)
  # Remove any file separators, if present.
  names <- gsub(pattern=.Platform$file.sep, replacement="", x=names, fixed=TRUE)
  pieces <- strsplit( x=names, split=sep )
  keep <- sapply( pieces, 
                  function(x) { is.in(x[1],country) && is.in(x[2],model) &&  is.in(x[3],factors) } )
  files <- files[ keep ]
  # the next line could be augmented to add additional information to the 
  # data frame.
  if (is.null(archive)){
    # Read files from the directory on disk
    dflist <- lapply(files, function(x) { readRDS ( file.path(path, x) ) } )
  } else {
    # Read files from the archive
    dflist <- lapply(files, function(x) { readRDS(gzcon(unz(archive, x))) } )
  }
  do.call( rbind.fill, dflist )
}