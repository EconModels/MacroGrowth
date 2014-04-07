
require(EconModels2)
require(plyr)  # for rbind.fill()

All <- read.table("data/AllData.txt", header=TRUE)
Countries <- unique(All$Country)
Energies <- c(quote(iQ), quote(iX), quote(iU))

oModels <- list()
rModels <- list()
coefs <- coefs <- list()

for (country in Countries) {
  cdata <- subset(All, Country==country)
  for (energy in Energies) {
    formula <- substitute( iGDP ~ iCapStk + iLabor + e + iYear, list(e = energy))
    # tryCatch to skip over country/energy combos that don't exist.
    tryCatch({
      oModel <- cdeModel( formula, cdata )
      oModels[[length(oModels) + 1]] <- oModel
      rFits <- resampledFits( oModel, "wild", n=5, id=paste(country,energy, sep=":") )
      rModels[[length(rModels) + 1]] <- rFits[["models"]]
      coefs[[length(coefs) + 1]] <- rFits[["coeffs"]]
    }, error=function(e) paste0("Skipping ", energy, " for ", country) 
    )
  }
}

coefs2 <- do.call(rbind.fill, coefs)

                  
