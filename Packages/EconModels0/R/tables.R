#' @export
covarianceTable <- function(countryAbbrev, baseHistorical){
  ##############################
  # Returns an xtable of covariance information for the specified country.
  ##
  data <- loadData(countryAbbrev=countryAbbrev, baseHistorical=baseHistorical)
  if (countryAbbrev %in% countryAbbrevsU){
    # Calculate covariances among the typical variables.
    dataForCovar <- cbind(data$iY, data$iK, data$iL, data$iQ, data$iX, data$iU)
    # Calculates the covariances among y, k. l, q, x, and u using all available data, namely,
    # 1980-2011 for covariances among y, k, l, q, and x for developed economies
    # 1980-2000 for covariances involving u for developed economies
    # 1991-2011 for covariances among y, k, l, q, and x for developing economies
    # The key here is the use="pairwise.complete.obs" argument.
    covarResults <- data.frame(cor(x=dataForCovar, use="pairwise.complete.obs"))
    names <- c("$y$", "$k$", "$l$", "$q$", "$x$", "$u$")
  } else {
    dataForCovar <- cbind(data$iY, data$iK, data$iL, data$iQ, data$iX)
    covarResults <- cor(dataForCovar)
    names <- c("$y$", "$k$", "$l$", "$q$", "$x$")
  }
  colnames(covarResults) <- names
  rownames(covarResults) <- names
  return(covarResults)
}

#' @export
singleFactorParamsTable <- function(factor){
  ############################
  # Aggregates the single-factor results into a big data table for the given factor.
  ##
  dataSF <- singleFactorParamsDF(factor)
  if (factor == "K"){
    factorString <- "$\\alpha$"
  } else if (factor == "L"){
    factorString <- "$\\beta$"
  } else {
    factorString <- "$\\gamma$"
  }
  colnames(dataSF) <- c(" ", "$\\lambda$", " ", 
                        " ", factorString,  " ")
  rownames(dataSF) <- countryAbbrevs
  tableSF <- xtable(dataSF, 
                    caption=paste("Single-factor model (with $", tolower(factor), 
                                  "$) parameters for 1980--2011 (US, UK, JP) and 1991--2011 (CN, ZA, SA, IR, TZ, and ZM). (Parameter estimates beneath symbol. 95\\% confidence interval bounds to left and right.)", 
                                  sep=""), 
                    label=paste("tab:SF_Parameters_With_", factor, sep=""),
                    digits = c(1, 4,4,4, 2,2,2),
                    align = "r|ccc|ccc") #Sets alignment of the numbers in the columns
  return(tableSF)
}

#' @export
printSFParamsTable <- function(factor){
  ###############################
  # Prints a covariance table for the given factor of production
  ##
  print(singleFactorParamsTable(factor),
        caption.placement="top", 
        sanitize.colnames.function = identity, 
        size="\\tiny", 
        table.placement="H")
}

#' @export
cobbDouglasParamsTableNoEnergyDF <- function(baseResample){
  ######################
  # Makes a data.frame with the parameters for the Cobb-Douglas model without energy.
  ##
  #Do rbind on the results of creating a row in the table for every country abbreviation that we know.
  dataCD <- do.call("rbind", lapply(countryAbbrevs, cobbDouglasCountryRow, energyType="none", baseResample=baseResample))
  rownames(dataCD) <- countryAbbrevs
  colnames(dataCD) <- c("lowerCI_lambda", "lambda", "upperCI_lambda", 
                        "lowerCI_alpha", "alpha", "upperCI_alpha",
                        "lowerCI_beta", "beta", "upperCI_beta")
  dataCD <- data.frame(dataCD)
  return(dataCD)
}

#' @export
cobbDouglasParamsTableNoEnergy <- function(baseResample){
  ############################
  # Aggregates the Cobb-Douglas results into a big data table. No energy.
  ##
  dataCD <- cobbDouglasParamsTableNoEnergyDF(baseResample=baseResample)
  colnames(dataCD) <- c(" ", "$\\lambda$", " ", 
                        " ", "$\\alpha$",  " ",
                        " ", "$\\beta$",   " ")
  rownames(dataCD) <- countryAbbrevs
  tableCD <- xtable(dataCD, 
                    caption="Cobb-Douglas model (without energy) parameters for 1980-2011 (US, UK, JP) and 1991--2011 (CN, ZA, SA, IR, TZ, and ZM). (Parameter estimates beneath symbol. 95\\% confidence interval bounds to left and right.)",
                    label="tab:CD_Parameters_No_Energy",
                    digits = c(1, 4,4,4, 2,2,2, 2,2,2),
                    align = "r|ccc|ccc|ccc") #Sets alignment of the numbers in the columns
  return(tableCD)
}

#' @export
cobbDouglasParamsTableWithEnergyDF <- function(energyType, baseResample){
  ######################
  # Makes a data.frame with the parameters for the Cobb-Douglas model with energy.
  ##
  #Do rbind on the results of creating a row in the table for every country abbreviation that we know.
  dataCD <- do.call("rbind", lapply(countryAbbrevs, cobbDouglasCountryRow, energyType=energyType, baseResample=baseResample))
  rownames(dataCD) <- countryAbbrevs
  colnames(dataCD) <- c("lowerCI_lambda", "lambda", "upperCI_lambda", 
                        "lowerCI_alpha", "alpha", "upperCI_alpha",
                        "lowerCI_beta", "beta", "upperCI_beta",
                        "lowerCI_gamma", "gamma", "upperCI_gamma")
  dataCD <- data.frame(dataCD)
  return(dataCD)
}

#' @export
cobbDouglasParamsTableWithEnergy <- function(energyType, baseResample){
  ############################
  # Aggregates the Cobb-Douglas results into a big data table for the given energyType.
  ##
  #Do rbind on the results of creating a row in the table for every country abbreviation that we know.
  dataCD <- cobbDouglasParamsTableWithEnergyDF(energyType=energyType, baseResample=baseResample)
  colnames(dataCD) <- c(" ", "$\\lambda$", " ", 
                        " ", "$\\alpha$",  " ",
                        " ", "$\\beta$",   " ",
                        " ", "$\\gamma$",  " ")
  rownames(dataCD) <- countryAbbrevs
  tableCD <- xtable(dataCD, 
                    caption=paste("Cobb-Douglas model (with $", 
                                  tolower(energyType), 
                                  "$) parameters for 1980-2011 (US, UK, JP) and 1991--2011 (CN, ZA, SA, IR, TZ, and ZM). (Parameter estimates beneath symbol. 95\\% confidence interval bounds to left and right.)", 
                                  sep=""), 
                    label=paste("tab:CD_Parameters_With_", energyType, sep=""),
                    digits = c(1, 4,4,4, 2,2,2, 2,2,2, 2,2,2),
                    align = "r|ccc|ccc|ccc|ccc") #Sets alignment of the numbers in the columns
  return(tableCD)
}

#' @export
printCDParamsTable <- function(energyType="none", baseResample){
  ############################
  # Prints a table with parameters from a Cobb-Douglas model for the given energyType. 
  # Set energyType="none" to print a table for Cobb-Douglas without energy.
  ##
  if (energyType == "none"){
    print(cobbDouglasParamsTableNoEnergy(baseResample=baseResample), 
          caption.placement="top", 
          sanitize.colnames.function = identity, 
          size="\\tiny", 
          table.placement="H")
  } else {
    print(cobbDouglasParamsTableWithEnergy(energyType=energyType, baseResample=baseResample), 
          caption.placement="top", 
          sanitize.colnames.function = identity, 
          size="\\tiny",
          table.placement="H")
  }
}

#' @export
cesResampleCoeffProps <- function(cesResampleFits, ...){
  #######
  # This function creates a table of confidence intervals for the ces and cese models
  # from the data supplied
  ##
  # Grab the original curve fit
  baseFitCoeffs <- cesResampleFits[cesResampleFits[["method"]]=="orig", ]
  # Grab the resample curve fits
  resampleFitCoeffs <- cesResampleFits[cesResampleFits[["method"]] != "orig", ]
  gammaCI <- myqdata(p=ciVals, vals=gamma, data=resampleFitCoeffs)
  lambdaCI <- myqdata(p=ciVals, vals=lambda, data=resampleFitCoeffs)
  delta_1CI <- myqdata(p=ciVals, vals=delta_1, data=resampleFitCoeffs)  
  rho_1CI <- myqdata(p=ciVals, vals=rho_1, data=resampleFitCoeffs)  
  sigma_1CI <- myqdata(p=ciVals, vals=sigma_1, data=resampleFitCoeffs)  
  deltaCI <- myqdata(p=ciVals, vals=delta, data=resampleFitCoeffs)  
  rhoCI <- myqdata(p=ciVals, vals=rho, data=resampleFitCoeffs)  
  sigmaCI <- myqdata(p=ciVals, vals=sigma, data=resampleFitCoeffs)  
  # Now make a data.frame that contains the information.
  lower <- data.frame(gamma=gammaCI["2.5%"],
                      lambda=lambdaCI["2.5%"],
                      delta_1=delta_1CI["2.5%"],
                      rho_1=rho_1CI["2.5%"],
                      sigma_1=sigma_1CI["2.5%"],
                      delta=deltaCI["2.5%"],
                      rho=rhoCI["2.5%"],
                      sigma=sigmaCI["2.5%"])
  row.names(lower) <- "-95% CI"
  mid <- data.frame(gamma=baseFitCoeffs["gamma"],
                    lambda=baseFitCoeffs["lambda"],
                    delta_1=baseFitCoeffs["delta_1"],
                    rho_1=baseFitCoeffs["rho_1"],
                    sigma_1=baseFitCoeffs["sigma_1"],
                    delta=baseFitCoeffs["delta"],
                    rho=baseFitCoeffs["rho"],
                    sigma=baseFitCoeffs["sigma"])
  row.names(mid) <- "CESe"
  upper <- data.frame(gamma=gammaCI["97.5%"],
                      lambda=lambdaCI["97.5%"],
                      delta_1=delta_1CI["97.5%"],
                      rho_1=rho_1CI["97.5%"],
                      sigma_1=sigma_1CI["97.5%"],
                      delta=deltaCI["97.5%"],
                      rho=rhoCI["97.5%"],
                      sigma=sigmaCI["97.5%"])
  row.names(upper) <- "+95% CI"
  dataCD <- rbind(upper, mid, lower)
  return(dataCD)
}



#' @export
cesData <- function(countryAbbrev, energyType="none", nest="(kl)e", archive=NULL, baseResample){
  #################################################
  # Calculates parameter estimates and confidence intervals
  # for the CES production function given a country and an energyType.
  #
  # countryAbbrev is a string containing the 2-letter abbreviation for the country, e.g. "US" or "CN"
  # energyType is a string, one of "Q", "X", "U", or "none".
  # energyType="none" means we're interested in a CES fit without energy.
  #
  # returns a data.frame of data for the CES model.
  # First row is the +95% CI on all parameters
  # Second row contains the parameter estimates
  # Third row is the -95% CI on all parameters
  # Each column has names: gamma, lambda, delta_1, delta, rho_1, rho
  ##
  #First, check to see if we want useful work (U) AND one of the countries for which we don't have data.
  if (!haveDataCES(countryAbbrev, energyType)){
    #Return a column of NAs if we don't have data available for this combination of country and energy type.
    nRows <- 3 # +95% CI, CDe, and -95% CI.
    nCols <- 8 # gamma, lambda, delta_1, delta, sigma_1, and sigma
    df <- as.data.frame(matrix(NA, ncol = nCols, nrow = nRows))
    colnames(df) <- c("gamma", "lambda", "delta_1", "rho_1", "sigma_1", "delta", "rho", "sigma")
    rownames(df) <- c("+95% CI", "CES", "-95% CI")
    return(df)
  } else if (energyType == "none"){
    # We want CES without energy
    resampleData <- loadResampleData(modelType="ces", countryAbbrev=countryAbbrev, energyType="none",
                                     archive=archive, baseResample=baseResample)
  } else {
    # We want CES with energy  -- might want all three for this later.
    modelType <- paste("cese-", nest, sep="")
    resampleData <- loadResampleData(modelType=modelType, countryAbbrev=countryAbbrev, 
                                     energyType=energyType, archive=archive, baseResample=baseResample)
  }
  statisticalProperties <- cesResampleCoeffProps(resampleData)
  # Set the correct label in the row that shows the base values.
  if (energyType == "none"){
    rownames(statisticalProperties) <- c("+95% CI", "CES", "-95% CI")
  } else {
    rownames(statisticalProperties) <- c("+95% CI", "CESe", "-95% CI")
  }
  return(statisticalProperties)
}

#' @export
cesCountryRow <- function(countryAbbrev, energyType="none", nest="(kl)e", baseResample){
  ############
  # Creates a row for the CES parameters table for the given country (2-letter code),
  # energyType (Q, X, U, or NA), and nest.
  ##
  dataCES <- cesData(countryAbbrev=countryAbbrev, energyType=energyType, nest=nest, baseResample=baseResample)
  out <- cbind(dataCES["-95% CI", "gamma"], dataCES["CES", "gamma"], dataCES["+95% CI", "gamma"],
               dataCES["-95% CI", "lambda"], dataCES["CES", "lambda"], dataCES["+95% CI", "lambda"],
               dataCES["-95% CI", "delta_1"], dataCES["CES", "delta_1"], dataCES["+95% CI", "delta_1"],
               dataCES["-95% CI", "delta"], dataCES["CES", "delta"], dataCES["+95% CI", "delta"],
               dataCES["-95% CI", "sigma_1"], dataCES["CES", "sigma_1"], dataCES["+95% CI", "sigma_1"],
               dataCES["-95% CI", "sigma"], dataCES["CES", "sigma"], dataCES["+95% CI", "sigma"]
  )
  return(out)
}



#' @export
cesParamsTableDF <- function(energyType, baseResample){
  ######################
  # Creates a data.frame for CES parameters
  ##
  dataCES <- do.call("rbind", lapply(countryAbbrevs, cesCountryRow, energyType=energyType, baseResample=baseResample))
  colnames(dataCES) <- c("lowerCI_gamma", "gamma", "upperCI_gamma",
                         "lowerCI_lambda", "lambda", "upperCI_lambda",
                         "lowerCI_delta_1", "delta_1", "upperCI_delta_1",
                         "lowerCI_delta", "delta", "upperCI_delta",
                         "lowerCI_sigma_1", "sigma_1", "upperCI_sigma_1",
                         "lowerCI_sigma", "sigma", "upperCI_sigma")
  rownames(dataCES) <- countryAbbrevs
  return(dataCES)
}

#' @export
cesParamsTableA <- function(energyType="none", nest="(kl)e", baseResample){
  ############################
  # Aggregates the CES results for lambda, delta, and sigma into a table for the given energyType.
  ##
  dataCES <- do.call("rbind", lapply(countryAbbrevs, cesCountryRow, energyType=energyType, nest=nest, baseResample))
  colnames(dataCES) <- c(" ", "$\\gamma$",    " ", 
                         " ", "$\\lambda$",   " ",
                         " ", "$\\delta_1$",  " ",
                         " ", "$\\delta$",    " ",
                         " ", "$\\sigma_1$",  " ",
                         " ", "$\\sigma$",    " ")
  rownames(dataCES) <- countryAbbrevs
  if (energyType == "none"){
    energyStringCaption <- "(without energy, ($kl$) nesting)"
    energyStringLabel <- ""
  } else {
    if (nest == "(kl)e"){
      energyStringCaption <- "(with energy, ($kl$)$e$ nesting)"
    } else if (nest == "(le)k"){
      energyStringCaption <- "(with energy, ($le$)$k$ nesting)"
    } else if (nest == "(ek)l"){
      energyStringCaption <- "(with energy, ($ek$)$l$ nesting)"
    } else {
      stop(paste("Unknown nesting", nest, "in cesParamsTableA."))
    }
    energyStringLabel <- paste("_With_", energyType, "_", nest, sep="")
  }
  tableCESa <- xtable(dataCES[,c(4,5,6, 10,11,12, 16,17,18)], #Picks up lambda, delta, sigma
                      caption=paste("CES model parameters ", 
                                    energyStringCaption, 
                                    ". $\\lambda$, $\\delta$, and $\\sigma$ parameters for 1980-2011 (US, UK, JP) and 1991--2011 (CN, ZA, SA, IR, TZ, and ZM). (Parameter estimates beneath symbol. 95\\% confidence interval bounds to left and right.)", 
                                    sep=""), 
                      label=paste("tab:CES_Parameters_A", energyStringLabel, sep=""),
                      digits = c(1, 4,4,4, 2,2,2, 2,2,2),
                      align = "r|ccc|ccc|ccc" #Sets alignment of the numbers in the columns
  ) 
  return(tableCESa)
}

#' @export
cesParamsTableB <- function(energyType="none", nest="(kl)e", baseResample){
  ############################
  # Aggregates the CES results for gamma, delta_1, and sigma_1 into a table for the given energyType.
  ##
  dataCES <- do.call("rbind", lapply(countryAbbrevs, cesCountryRow, energyType=energyType, nest=nest, baseResample=baseResample))
  colnames(dataCES) <- c(" ", "$\\gamma$",    " ", 
                         " ", "$\\lambda$",   " ",
                         " ", "$\\delta_1$",  " ",
                         " ", "$\\delta$",    " ",
                         " ", "$\\sigma_1$",  " ",
                         " ", "$\\sigma$",    " ")
  rownames(dataCES) <- countryAbbrevs
  if (energyType == "none"){
    energyStringCaption <- "(without energy, ($kl$) nesting)"
    energyStringLabel <- ""
  } else {
    if (nest == "(kl)e"){
      energyStringCaption <- "(with energy, ($kl$)$e$ nesting)"
    } else if (nest == "(le)k"){
      energyStringCaption <- "(with energy, ($le$)$k$ nesting)"
    } else if (nest == "(ek)l"){
      energyStringCaption <- "(with energy, ($ek$)$l$ nesting)"
    } else {
      stop(paste("Unknown nesting", nest, "in cesParamsTableA."))
    }
    energyStringLabel <- paste("_With_", energyType, "_", nest, sep="")
  }
  tableCESb <- xtable(dataCES[,c(1,2,3, 7,8,9, 13,14,15)], #Picks up gamma, delta_1, and sigma_1
                      caption=paste("CES model parameters ", energyStringCaption, ". $\\gamma$, $\\delta_1$, and $\\sigma_1$ parameters for 1980--2011 (US, UK, JP) and 1991--2011 (CN, ZA, SA, IR, TZ, and ZM). (Parameter estimates beneath symbol. 95\\% confidence interval bounds to left and right.)", sep=""), 
                      label=paste("tab:CES_Parameters_B", energyStringLabel, sep=""),
                      digits = c(1, 2,2,2, 2,2,2, 2,2,2),
                      align = "r|ccc|ccc|ccc"  #Sets alignment of the numbers in the columns
  )
  return(tableCESb)
}


#' @export
linexResampleCoeffProps <- function(linexResampleFits, ...){
  ####### 
  # This function creates a table of confidence intervals for the LINEX models
  # from the data supplied
  ##
  # Grab the original curve fit
  baseFitCoeffs <- linexResampleFits[linexResampleFits[["method"]]=="orig", ]
  # Grab the resample curve fits
  resampleFitCoeffs <- linexResampleFits[linexResampleFits[["method"]] != "orig", ]
  a_0CI <- myqdata(p=ciVals, vals=a_0, data=resampleFitCoeffs)
  c_tCI <- myqdata(p=ciVals, vals=c_t, data=resampleFitCoeffs)
  # Now make a data.frame that contains the information.
  lower <- data.frame(a_0=a_0CI["2.5%"],
                      c_t=c_tCI["2.5%"])
  row.names(lower) <- "-95% CI"
  mid <- data.frame(a_0=baseFitCoeffs["a_0"],
                    c_t=baseFitCoeffs["c_t"])
  row.names(mid) <- "LINEX"
  upper <- data.frame(a_0=a_0CI["97.5%"],
                      c_t=c_tCI["97.5%"])
  row.names(upper) <- "+95% CI"
  dataCD <- rbind(upper, mid, lower)
  return(dataCD)
}

#' @export
linexData <- function(countryAbbrev, energyType, archive=NULL, baseResample){
  #################################################
  # Calculates parameter estimates and confidence intervals
  # for the LINEX production function given a country and an energyType.
  #
  # countryAbbrev is a string containing the 2-letter abbreviation for the country, e.g. "US" or "CN"
  # energyType is a string, one of "Q", "X", or "U"
  #
  # returns a data.frame of data for the LINEX model.
  # First row is the +95% CI on all parameters
  # Second row contains the parameter estimates
  # Third row is the -95% CI on all parameters
  # Each column has names: a_0 and c_t, corresponding to the parameters in the model.
  ##
  #First, check to see if we want useful work (U) AND one of the countries for which we don't have data.
  if (!haveDataCD(countryAbbrev, energyType)){
    #Return a column of NAs if the above conditions have been met.
    nRows <- 3 # +95% CI, CDe, and -95% CI.
    nCols <- 2 # a_0 and c_t
    df <- as.data.frame(matrix(NA, ncol = nCols, nrow = nRows))
    colnames(df) <- c("a_0", "c_t")
    rownames(df) <- c("+95% CI", "LINEX", "-95% CI")
    return(df)
  }
  resampledData <- loadResampleData(modelType="linex", countryAbbrev=countryAbbrev, energyType=energyType,
                                    archive=archive, baseResample=baseResample)
  statisticalProperties <- linexResampleCoeffProps(resampledData)
  return(statisticalProperties)
}

#' @export
linexCountryRow <- function(countryAbbrev, energyType, baseResample){
  ############
  # Creates a row for the LINEX parameters table for the given country (2-letter code) and energyType (Q, X, or U)
  ##
  dataLINEX <- linexData(countryAbbrev=countryAbbrev, energyType=energyType, baseResample=baseResample)
  out <- cbind(dataLINEX["-95% CI", "a_0"], dataLINEX["LINEX", "a_0"], dataLINEX["+95% CI", "a_0"],
               dataLINEX["-95% CI", "c_t"], dataLINEX["LINEX", "c_t"], dataLINEX["+95% CI", "c_t"])
  return(out)
}



#' @export
linexParamsTableDF <- function(energyType, baseResample){
  #####################
  # Creates a data.frame containing all parameters and their confidence intervals 
  ##
  #Do rbind on the results of creating a row in the table for every country abbreviation that we know.
  dataLINEX <- do.call("rbind", lapply(countryAbbrevs, linexCountryRow, energyType=energyType, baseResample=baseResample))
  colnames(dataLINEX) <- c("lowerCI_a_0", "a_0", "upperCI_a_0",
                           "lowerCI_c_t", "c_t", "upperCI_c_t")
  rownames(dataLINEX) <- countryAbbrevs
  return(dataLINEX)
}

#' @export
linexParamsTable <- function(energyType, baseResample){
  ############################
  # Aggregates the LINEX results into a big data table for the given energyType.
  ##
  dataLINEX <- linexParamsTableDF(energyType, baseResample)
  colnames(dataLINEX) <- c(" ", "$a_0$", " ", " ", "$c_t$",  " ")
  rownames(dataLINEX) <- countryAbbrevs
  tableLINEX <- xtable(dataLINEX, 
                       caption=paste("LINEX model (with $", tolower(energyType), "$) parameters for 1980--2011 (US, UK, JP) and 1991--2011 (CN, ZA, SA, IR, TZ, and ZM). (Parameter estimates beneath symbol. 95\\% confidence interval bounds to left and right.)", sep=""), 
                       label=paste("tab:LINEX_Parameters_With_", energyType, sep=""),
                       digits = c(1, 2,2,2, 2,2,2),
                       align = "r|ccc|ccc") #Sets alignment of the numbers in the columns
  return(tableLINEX)
}

#' @export
createAICTable <- function(baseHistorical){
  ###############################
  # Creates an xtable object that holds the AIC values for each parameter estimation that we include.
  ##
  ######################
  # Single-factor models
  ######################
  # Single-factor with K
  # At present, this function re-fits all models with lines like the following.
  sfKModels <- lapply(countryAbbrevs, singleFactorModel, factor="K", respectRangeConstraints=TRUE, baseHistorical=baseHistorical)
  # The above approach will be painfully slow for the CES models, because they take soooo long to run.
  # If we want to load from disk instead of re-running the model, we should use code that looks like the line below
  # But, at present, the next line of code is slooooow, because loadResampleModelsBaseModelOnly 
  # reads all models and only returns the first one (the base model), throwing away all resample models.
  # And, there could be LOTS of resample models to load (typically, 1000).
  # sfKModels <- lapply(countryAbbrevs, loadResampleModelsBaseModelOnly, modelType="sf", factor="K", baseResample="data_resample")
  aicSFk <- data.frame(lapply(sfKModels, AIC))
  rownames(aicSFk) <- "SF$k$"
  # Single-factor with L
  sfLModels <- lapply(countryAbbrevs, singleFactorModel, factor="L", respectRangeConstraints=TRUE, baseHistorical=baseHistorical)
  aicSFl <- data.frame(lapply(sfLModels, AIC))
  rownames(aicSFl) <- "SF$l$"
  # Single-factor with Q
  sfQModels <- lapply(countryAbbrevs, singleFactorModel, factor="Q", respectRangeConstraints=TRUE, baseHistorical=baseHistorical)
  aicSFq <- data.frame(lapply(sfQModels, AIC))
  rownames(aicSFq) <- "SF$q$"
  # Single-factor with X
  sfXModels <- lapply(countryAbbrevs, singleFactorModel, factor="X", respectRangeConstraints=TRUE, baseHistorical=baseHistorical)
  aicSFx <- data.frame(lapply(sfXModels, AIC))
  rownames(aicSFx) <- "SF$x$"
  # Single-factor with U
  aicSFu <- cbind(US=AIC(singleFactorModel(countryAbbrev="US", factor="U", respectRangeConstraints=TRUE, baseHistorical=baseHistorical)), 
                  UK=AIC(singleFactorModel(countryAbbrev="UK", factor="U", respectRangeConstraints=TRUE, baseHistorical=baseHistorical)), 
                  JP=AIC(singleFactorModel(countryAbbrev="JP", factor="U", respectRangeConstraints=TRUE, baseHistorical=baseHistorical)),
                  CN=NA, ZA=NA, SA=NA, IR=NA, TZ=NA, ZM=NA) #No U data for these countries.
  rownames(aicSFu) <- "SF$u$"
  ######################
  # Cobb-Douglas models
  ######################
  # Cobb-Douglas without energy
  cdModels <- lapply(countryAbbrevs, cobbDouglasModel, energyType="none", respectRangeConstraints=TRUE, baseHistorical=baseHistorical)
  aicCD <- data.frame(lapply(cdModels, AIC))
  rownames(aicCD) <- "CD"
  # Cobb-Douglas with Q
  cdQModels <- lapply(countryAbbrevs, cobbDouglasModel, energyType="Q", respectRangeConstraints=TRUE, baseHistorical=baseHistorical)
  aicCDq <- data.frame(lapply(cdQModels, AIC))
  rownames(aicCDq) <- "CD$q$"
  # Cobb-Douglas with X
  cdXModels <- lapply(countryAbbrevs, cobbDouglasModel, energyType="X", respectRangeConstraints=TRUE, baseHistorical=baseHistorical)
  aicCDx <- data.frame(lapply(cdXModels, AIC))
  rownames(aicCDx) <- "CD$x$"
  # Cobb-Douglas with U
  aicCDu <- cbind(US=AIC(cobbDouglasModel(countryAbbrev="US", energyType="U", respectRangeConstraints=TRUE, baseHistorical=baseHistorical)), 
                  UK=AIC(cobbDouglasModel(countryAbbrev="UK", energyType="U", respectRangeConstraints=TRUE, baseHistorical=baseHistorical)), 
                  JP=AIC(cobbDouglasModel(countryAbbrev="JP", energyType="U", respectRangeConstraints=TRUE, baseHistorical=baseHistorical)),
                  CN=NA, ZA=NA, SA=NA, IR=NA, TZ=NA, ZM=NA) #No U data for these countries.
  rownames(aicCDu) <- "CD$u$"
  ######################
  # CES models
  ######################
  # At present, this AIC for CES code is not working. Perhaps because the CES model from the cesEst function
  # in the micEcon package does not provide its data in the correct format for the AIC function?
  # --Matthew Kuperus Heun, 10 April 2013.
  # 
  # If we ever resurrect this code, we'll want to make sure that we're not running the cesModel
  # repeatedly. We'll want to load data from an archive using loadResampleModelsBaseModelOnly
  # 
  #   # CES with Q
  #   cesQModels <- lapply(countryAbbrevs, cesModel2, energyType="Q")
  #   aicCESq <- data.frame(lapply(cesQModels, AIC))
  #   rownames(aicCESq) <- "CES$q$"
  #   # CES with X
  #   cesXModels <- lapply(countryAbbrevs, cesModel2, energyType="X")
  #   aicDEXx <- data.frame(lapply(cesXModels, AIC))
  #   rownames(aicCESx) <- "CES$x$"
  #   # CES with U
  #   aicCESu <- cbind(US=AIC(cesModel2("US", "U")), 
  #                   UK=AIC(cesModel2("UK", "U")), 
  #                   JP=AIC(cesModel2("JP", "U")),
  #                   CN=NA, ZA=NA, SA=NA, IR=NA, TZ=NA, ZM=NA) #No U data for these countries.
  #   rownames(aicCESu) <- "CES$u$"
  aicCES  <- data.frame(US=NA, UK=NA, JP=NA, CN=NA, ZA=NA, SA=NA, IR=NA, TZ=NA, ZM=NA)
  rownames(aicCES) <- "CES"
  aicCESq <- data.frame(US=NA, UK=NA, JP=NA, CN=NA, ZA=NA, SA=NA, IR=NA, TZ=NA, ZM=NA)
  rownames(aicCESq) <- "CES$q$"
  aicCESx <- data.frame(US=NA, UK=NA, JP=NA, CN=NA, ZA=NA, SA=NA, IR=NA, TZ=NA, ZM=NA)
  rownames(aicCESx) <- "CES$x$"
  aicCESu <- data.frame(US=NA, UK=NA, JP=NA, CN=NA, ZA=NA, SA=NA, IR=NA, TZ=NA, ZM=NA)
  rownames(aicCESu) <- "CES$u$"
  ######################
  # LINEX models
  ######################
  # LINEX with Q
  linexQModels <- lapply(countryAbbrevs, linexModel, energyType="Q", baseHistorical=baseHistorical)
  aicLINEXq <- data.frame(lapply(linexQModels, AIC))
  rownames(aicLINEXq) <- "LINEX$q$"
  # LINEX with X
  linexXModels <- lapply(countryAbbrevs, linexModel, energyType="X", baseHistorical=baseHistorical)
  aicLINEXx <- data.frame(lapply(linexXModels, AIC))
  rownames(aicLINEXx) <- "LINEX$x$"  
  # LINEX with U
  aicLINEXu <- cbind(US=AIC(linexModel(countryAbbrev="US", energyType="U", baseHistorical=baseHistorical)), 
                     UK=AIC(linexModel(countryAbbrev="UK", energyType="U", baseHistorical=baseHistorical)), 
                     JP=AIC(linexModel(countryAbbrev="JP", energyType="U", baseHistorical=baseHistorical)),
                     CN=NA, ZA=NA, SA=NA, IR=NA, TZ=NA, ZM=NA) #No U data for these countries.
  rownames(aicLINEXu) <- "LINEX$u$"
  
  # Create the data.frame table
  out <- rbind(aicSFk, aicSFl, aicSFq, aicSFx, aicSFu, 
               aicCD, aicCDq, aicCDx, aicCDu, 
               aicCES, aicCESq, aicCESx, aicCESu, 
               aicLINEXq, aicLINEXx, aicLINEXu) 
  # Create the xtable with caption and label
  out <- xtable(out, caption="AIC values for all models.", label="tab:AICTable", digits=1)
  return(out)
}

## <<CIvsParam_Graph, eval=TRUE>>=
#' @export
CIvsParamDF <- function(model, param, energyType="none", factor=NA, baseResample){
  ############################
  # Creates a data.frame that contains the following information:
  # row name: 2-letter country abbreviation
  # 1st col: parameter value. The name of this column is the name of the parameter, "sigma", "delta", etc. 
  #          as appropriate for the model generating the data
  # 2nd col: width of the confidence interval. The name of this column is "CI"
  # 3rd col: an identifier for parameter, model, and (factor or energy type). 
  #          For example, if you want the sigma parameter for the CES model with Q, you would get "sigmaCESeq".
  #          If you want the m parameter for the single-factor mdoel with k, you would get "mSFk".
  #          If you want the alpha parameter for the Cobb-Douglas model without energy, you would get "alphaCD".
  #          The name of the column is "factor"
  ##
  # Get the data from the requested model
  if (model == "SF"){data <- singleFactorParamsDF(factor)} 
  else if (model == "CD"){data <- cobbDouglasParamsTableNoEnergyDF(baseResample=baseResample)}
  else if (model == "CDe"){data <- cobbDouglasParamsTableWithEnergyDF(energyType=energyType, baseResample=baseResample)}
  else if (model == "CES"){data <- cesParamsTableDF(energyType="none")}
  else if (model == "CESe"){data <- cesParamsTableDF(energyType=energyType)}
  else if (model == "LINEX"){data <- linexParamsTableDF(energyType)}
  # Now get the data for the parameter requested
  # Also, set limits on the x and y axes if needed
  if (param == "lambda"){
    x <- subset(data, select=lambda) # The column of lambdas
    y <- subset(data, select=upperCI_lambda) - subset(data, select=lowerCI_lambda) # A column of CI values
  } else if (param == "m"){
    x <- subset(data, select=m) # The column of m's
    colnames(x) <- "m"
    y <- subset(data, select=upperCI_m) - subset(data, select=lowerCI_m) # A column of CI values
  } else if (param == "alpha"){
    x <- subset(data, select=alpha) # The column of alphas
    y <- subset(data, select=upperCI_alpha) - subset(data, select=lowerCI_alpha) # A column of CI values
  } else if (param == "beta"){
    x <- subset(data, select=beta) # The column of betas
    y <- subset(data, select=upperCI_beta) - subset(data, select=lowerCI_beta) # A column of CI values
  } else if (param == "gamma"){
    x <- subset(data, select=gamma) # The column of gammas
    y <- subset(data, select=upperCI_gamma) - subset(data, select=lowerCI_gamma) # A column of CI values
  } else if (param == "delta_1"){
    x <- subset(data, select=delta_1) # The column of delta_1's
    y <- subset(data, select=upperCI_delta_1) - subset(data, select=lowerCI_delta_1) # A column of CI values
  } else if (param == "sigma_1"){
    x <- subset(data, select=sigma_1) # The column of sigma_1's
    y <- subset(data, select=upperCI_sigma_1) - subset(data, select=lowerCI_sigma_1) # A column of CI values
  } else if (param == "delta"){
    x <- subset(data, select=delta) # The column of deltas
    y <- subset(data, select=upperCI_delta) - subset(data, select=lowerCI_delta) # A column of CI values
  } else if (param == "sigma"){
    x <- subset(data, select=sigma) # The column of sigmas
    y <- subset(data, select=upperCI_sigma) - subset(data, select=lowerCI_sigma) # A column of CI values
  } else if (param == "a_0"){
    x <- subset(data, select=a_0) # The column of a_0's
    y <- subset(data, select=upperCI_a_0) - subset(data, select=lowerCI_a_0) # A column of CI values
  } else if (param == "c_t"){
    x <- subset(data, select=c_t) # The column of c_t's
    y <- subset(data, select=upperCI_c_t) - subset(data, select=lowerCI_c_t) # A column of CI values
  }
  colnames(y) <- "CI"
  # Create the factor string. We are guaranteed to have a value for the param and model arguments.
  # However, we might have NA for either energyType or factor. But, we will not have values for both 
  # energyType and factor at the same time! So, we can simply paste everything together here and 
  # obtain the desired result.
  if (is.na(factor)){
    factorString <- paste(param, model, energyType, sep="")
  } else if (energyType == "none"){
    factorString <- paste(param, model, factor, sep="")
  } else {
    print("Neither energyType nor factor were NA in CIvsParamDF. But one should be!")
    return(NULL)
  }
  nRows <- nrow(x)
  factor <- as.data.frame(matrix(factorString, ncol=1, nrow=nRows))
  colnames(factor) <- "factor"
  # Now make a column containing the country abbreviations.
  Country <- data.frame(countryAbbrevs)
  data <- cbind(x, y, factor, Country)
  return(data)
}

