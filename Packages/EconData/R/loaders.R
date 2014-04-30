
#' @importFrom plyr rbind.fill

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
#' @param sep the separator within the file names between country, model, and factors
#' @param prefix a character string naming prefix for file name.
#' @return a string representing the energyType that was used for the fitting. NA if the sfModel was used.
#' @details Supply either \code{path} or \code{archive} arguments. 
#' Defaults are correct when only one is supplied, so long as archives are constructed with 
#' all resample files at the root level of the zip archive.
#' @export
loadResampledData <- function( path="", archive=NULL, country=NULL, model=NULL, 
                               factors=NULL, sep="_", prefix=c("coeffs","models")
                               ){
  prefix <- match.arg(prefix)
  if (is.null(archive)){
    files <- dir(path)
  } else {
    filesDF <- unzip(zipfile=archive, list=TRUE)
    files <- filesDF[["Name"]]
  }
  # Keep only those files with the ".Rdata" extension
  files <- files[grepl(pattern="\\>.Rdata", x=files)]
  # Keep only files with desired prefix
  files <- files[grepl(pattern=prefix, x=files)]
  # Remove the path from the file names, if present
  names <- sub(pattern=paste0("\\<", path), replacement="", x=files)
  # Remove any file separators at the beginning of the names, if present.
  names <- sub(pattern=paste0("\\<", .Platform$file.sep), replacement="", x=names)
  # Remove the prefix from the file names, if present
  names <- sub(pattern=paste0("\\<", prefix, sep), replacement="", x=names)
  # Remove the .Rdata suffix from the file names.
  names <- sub(pattern="\\>.Rdata", replacement="", x=names)
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
  
  if (prefix %in% c("", "coeffs")) {
    # Load the resample coefficient files
    for (i in 1:nFiles){
      if (is.null(archive)){
        df <- readRDS ( file.path(path, files[i]) ) # Read files from the directory on disk
      } else {
        connection <- gzcon(unz(archive, files[i])) 
        df <- readRDS ( connection )  # Read files from the archive
        close(connection)
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
      df$model <- modelType
      df$nestStr <- nestStr
      df$energy <- parsedNestStr[["energyType"]]
      df$factor <- parsedNestStr[["factor"]]
      dflist[[i]] <- df
    }
    res <- do.call( rbind.fill, dflist ) 
    return(res)
  } else {
    # Load the resample model files
    modelsList <- list()
    for (i in 1:nFiles){
      if (is.null(archive)){
        dat <- readRDS ( file.path(path, files[i]) ) # Read files from the directory on disk
      } else {
        connection <- gzcon(unz(archive, files[i])) 
        dat <- readRDS ( connection ) # Read files from the archive
        close(connection)
      }
      modelsList <- c(modelsList, dat)
    }
    return(modelsList)
  }
}