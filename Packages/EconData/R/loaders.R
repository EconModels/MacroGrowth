is.in <- function( el, set ) {
  if (is.null(set)) { return( rep(TRUE, length(el) ) ) }
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
#' @param prefix a character string naming prefix for file name.
#' @return a string representing the energyType that was used for the fitting. NA if the sfModel was used.
#' 
#' @export
loadResampledData <- function( path, archive=NULL, country=NULL, model=NULL, 
                               factors=NULL, sep="_", prefix="" ) {
  if (is.null(archive)){
    files <- dir(path)
  } else {
    filesDF <- unzip(zipfile=archive, list=TRUE)
    files <- filesDF[["Name"]]
  }
  # Keep only those files with the ".Rdata" extension
  files <- files[grepl(pattern=".Rdata", x=files, fixed=TRUE)]
  # Keep only files with desired prefix
  files <- files[grepl(pattern=prefix, x=files, fixed=TRUE)]
  # Remove the .Rdata suffix from the file names.
  names <- gsub(pattern=".Rdata", replacement="", x=files, fixed=TRUE)
  # Remove the path prefix from the file names, if present
  names <- gsub(pattern=path, replacement="", x=names, fixed=TRUE)
  names <- gsub(pattern=prefix, replacement="", x=names, fixed=TRUE)
  # Remove any file separators, if present.
  names <- gsub(pattern=.Platform$file.sep, replacement="", x=names, fixed=TRUE)
  pieces <- strsplit( x=names, split=sep )
  keep <- sapply( pieces, 
                  function(x) { is.in(x[1],country) && is.in(x[2],model) &&  is.in(x[3],factors) } )
  files <- files[ keep ]
  pieces <- pieces[keep]
  dflist <- list()
  nFiles <- length(files)
  
  if (length(pieces) != nFiles){
    stop("Unequal length for files and names in loadResampledData.")
  }
  
  if (prefix  %in% c("", "coef_")) {
    for (i in 1:nFiles){
      if (is.null(archive)){
        df <- readRDS ( file.path(path, files[i]) ) # Read files from the directory on disk
      } else {
        df <- readRDS ( gzcon(unz(archive, files[i])) ) # Read files from the archive
      }
      if (! inherits(df, "data.frame") ) next
      
      # Add relevant information to the data frame
      if ("sigma" %in% names(df) ){
        sigmaTrans <- ifelse(df$sigma < 2, df$sigma, 1.5 - df$rho )
        df$sigmaTrans <- sigmaTrans
      }  
      if ("sigma_1" %in% names(df) ){
        sigmaTrans_1 <- ifelse(df$sigma_1 < 2, df$sigma_1, 1.5 - df$rho_1 )
        df$sigmaTrans_1 <- sigmaTrans_1
      }
      # Add several relevant columns to the data frame.
      countryAbbrev <- pieces[[i]][1]
      modelType <- pieces[[i]][2]
      nestStr <- pieces[[i]][3]
      parsedNestStr <- parseFactorString(factorString=nestStr)
      # Add the relevant information to the data frame.
      df$country <- countryAbbrev
      df$modelType <- modelType
      df$nestStr <- nestStr
      df$energyType <- parsedNestStr[["energyType"]]
      df$factor <- parsedNestStr[["factor"]]
      dflist[[i]] <- df
    }
    return( do.call( rbind.fill, dflist ) )
  } else {
    modelsList <- list()
    for (i in 1:nFiles){
      if (is.null(archive)){
        dat <- readRDS ( file.path(path, files[i]) ) # Read files from the directory on disk
      } else {
        dat <- readRDS ( gzcon(unz(archive, files[i])) ) # Read files from the archive
      }
      modelsList <- c(modelsList, dat)
    }
    return(modelsList)
  }
}