#' @export
haveDataSF <- function(countryAbbrev, factor){
  ###############################
  # Tells whether we have data for this combination of country and energy type for a single-factor curve fit.
  #
  # returns logical value (TRUE or FALSE)
  ##
  # The design here is to go through several cases where data is unavailable and
  # return FALSE. If we make it through all of those cases, we'll return TRUE.
  if (!(countryAbbrev %in% countryAbbrevs)){
    # If we don't know the country, we don't have data
    return(FALSE)
  }
  if (missing(factor) || is.na(factor)){
    # If we don't have a factor, we don't have data.
    return(FALSE)
  }
  if (!(factor %in% factors)){
    # If we don't know the factor, we don't have data
    return(FALSE)
  }
  if (factor == "U" && (!(countryAbbrev %in% countryAbbrevsU))){
    # We have U data for only a few countries.
    return(FALSE)
  }
  return(TRUE)
}

#' @export
haveDataCD <- function(countryAbbrev, energyType="none"){
  ###############################
  # Tells whether we have data for this combination of country and energy type for a Cobb-Douglas curve fit.
  # a missing energyType or energyType == NA means that we want to develop a model without energy, if possible.
  #
  # returns logical value (TRUE or FALSE)
  ##
  # The design here is to go through several cases where data is unavailable and
  # return FALSE. If we make it through all of those cases, we'll return TRUE.
  if (!(countryAbbrev %in% countryAbbrevs)){
    # If we don't know the country, we don't have data
    return(FALSE)
  }
  if (energyType == "none"){
    # If we don't want energy included, we have data for all known countries. We need to check this case
    # first, BEFORE doing various checks on the energyType.
    return(TRUE)
  }
  if (!(energyType %in% energyTypes)){
    # If we want to include energy, we know only a few types. So, return false if we don't know the energy type.
    return(FALSE)
  }
  if (energyType == "U" && (!(countryAbbrev %in% countryAbbrevsU))){
    # We have U data for only a few countries.
    return(FALSE)
  }
  return(TRUE)
}

#' @export
haveDataCES <- function(countryAbbrev, energyType="none"){
  ###############################
  # Tells whether we have data for this combination of country and energy type for a CES curve fit.
  # a missing energyType or energyType == NA means that we want to develop a model without energy, if possible.
  # This function re-routes to haveDataCD, because the logic is identical.
  #
  # returns logical value (TRUE or FALSE)
  ##
  return(haveDataCD(countryAbbrev, energyType))
}

#' @export
haveDataLINEX <- function(countryAbbrev, energyType="none"){
  ###############################
  # Tells whether we have data for this combination of country and energy type for a LINEX curve fit.
  # a missing energyType or energyType == NA means that we want to develop a model without energy, if possible.
  # This function re-routes to haveDataCD, because the logic is identical.
  #
  # returns logical value (TRUE or FALSE)
  ##
  return(haveDataCD(countryAbbrev, energyType))
}

