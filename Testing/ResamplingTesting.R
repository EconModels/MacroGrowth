
require(EconModels2)
require(plyr)  # for rbind.fill()

All <- read.table("data/AllData.txt", header=TRUE)
Countries <- unique(All$Country)
Energies <- c(quote(iQ), quote(iX), quote(iU))
Energies <- c("iQ", "iX", "iU")

ModelInfos <- list(
  list( formulaStr = "iGDP ~ iCapStk + iLabor + energy + iYear",
        fun = "cdeModel",
        n=100,
        dots = list()),
  list( formulaStr = "iGDP ~ iCapStk + iYear",
        fun = "singleFactorModel",
        n=100,
        dots = list()),
  list( formulaStr = "iGDP ~ iLabor + iYear",
        fun = "singleFactorModel",
        n=100,
        dots = list()),
  list( formulaStr = "iGDP ~ iLabor + energy",
        fun = "singleFactorModel",
        n=100,
        dots = list()),
  list( formulaStr = "iGDP ~ iCapStk + iLabor + energy + iYear",
        fun = "linexModel",
        n=100,
        dots = list()),
  list( formulaStr = "iGDP ~ iCapStk + iLabor + energy + iYear",
        fun = "cesModel",
        n=2,
        dots = list(nest=1:3)),
  list( formulaStr = "iGDP ~ iCapStk + iLabor + energy + iYear",
        fun = "cesModel",
        n=2,
        dots = list(nest=c(1,3,2))),
  list( formulaStr = "iGDP ~ iCapStk + iLabor + energy + iYear",
        fun = "cesModel",
        n=2,
        dots = list(nest=c(2,3,1)))
)

ModelInfos <- head(ModelInfos, -3)  # skip ces models

oModels <- list()
rModels <- list()
coefs <- list()

for (country in Countries) {
  cdata <- subset(All, Country==country)
  for (m in ModelInfos) {
    for (energy in if (grepl("energy", m$formulaStr))  Energies else 'xxx') {
      formulaStr <- sub( "energy", energy, m$formulaStr ) 
      formula <- eval( parse( text= formulaStr ) )
      # formula <- substitute( iGDP ~ iCapStk + iLabor + e + iYear, list(e = energy))
      # tryCatch to skip over country/energy combos that don't exist.
      cat ( paste(country, formulaStr, m$fun, m$n, sep=" : ") )
      cat ("\n")
      
      tryCatch({
        oModel <- do.call( m$fun, c( list( formula, data=cdata ), m$dots) )
        oModels[[length(oModels) + 1]] <- oModel
        rFits <- resampledFits( oModel, "wild", n=m$n, id=paste(country,energy, sep=":") )
        rModels[[length(rModels) + 1]] <- rFits[["models"]]
        coefs[[length(coefs) + 1]] <- rFits[["coeffs"]]
      }, 
      error=function(e) {
        cat(paste0("  *** Skipping ", energy, " for ", country, "\n"))
      }
      )
    }
  }
}

coefs2 <- do.call(rbind.fill, coefs)


