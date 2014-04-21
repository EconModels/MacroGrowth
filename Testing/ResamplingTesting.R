
require(EconModels2)
require(plyr)  # for rbind.fill()

All <- read.table("data/AllData.txt", header=TRUE)
Countries <- unique(All$Country)
# Countries <- "ZA"
Energies <- c(quote(iQ), quote(iX), quote(iU))
Energies <- c("iQ", "iX", "iU")

lots <- 4
few <- 1

ModelInfos <- list(
  list( formulaStr = c("iGDP ~ iCapStk + iYear", 
                       "iGDP ~ iLabor + iYear",
                       "iGDP ~ energy + iYear"),
        fun = "singleFactorModel",
        n=lots,
        dots = list()),
  list( formulaStr = c("iGDP ~ iCapStk + iLabor + iYear",
                       "iGDP ~ iCapStk + iLabor + energy + iYear"),
        fun = "cobbDouglasModel",
        n=lots,
        dots = list()),
  list( formulaStr = "iGDP ~ iCapStk + iLabor + energy + iYear",
        fun = "linexModel",
        n=lots,
        dots = list()),
  list( formulaStr = "iGDP ~ iCapStk + iLabor + iYear",
        fun = "cesModel",
        n=few,
        dots = list(nest=1:2)),
  list( formulaStr = "iGDP ~ iCapStk + iLabor + energy + iYear",
        fun = "cesModel",
        n=few,
        dots = list(nest=1:3)),
  list( formulaStr = "iGDP ~ iCapStk + iLabor + energy + iYear",
        fun = "cesModel",
        n=few,
        dots = list(nest=c(2,3,1))),
  list( formulaStr = "iGDP ~ iCapStk + iLabor + energy + iYear",
        fun = "cesModel",
        n=few,
        dots = list(nest=c(3,1,2)))
)

# ModelInfos <- head(ModelInfos, -3)  # skip ces models with energy
# ModelInfos <- head(ModelInfos, -4)  # skip all ces models
ModelInfos <- tail( ModelInfos, 4)  # Do ONLY ces models

oModels <- list()
rModels <- list()
coefs <- list()

for (country in Countries) {
  cdata <- subset(All, Country==country)
  for (m in ModelInfos) {
    for (f in m$formulaStr) {
      for (energy in if (grepl("energy", f))  Energies else 'noEnergy') {
        formulaStr <- sub( "energy", energy, f ) 
        formula <- eval( parse( text= formulaStr ) )
        # formula <- substitute( iGDP ~ iCapStk + iLabor + e + iYear, list(e = energy))
        # tryCatch to skip over country/energy combos that don't exist.
        cat ( paste(country, formulaStr, m$fun, m$dots, m$n, sep=" : ") )
        cat ("\n")
        
        tryCatch({
          oModel <- do.call( m$fun, c( list( formula, data=cdata ), m$dots) )
          oModels[[length(oModels) + 1]] <- oModel
          if (m$fun == "cesModel") {
            # Want to set prevModel to oModel in the call to cesModel. It will be passed in the ... argument.
            rFits <- resampledFits( oModel, "wild", n=m$n, id=paste(country,energy,m$fun, sep=":"), prevModel=oModel )
          } else {
            # No need for a prevModel argument, because none of the model functions (except cesModel) use it.
            rFits <- resampledFits( oModel, "wild", n=m$n, id=paste(country,energy,m$fun, sep=":") )
          }
          rFits <- resampledFits( oModel, "wild", n=m$n, id=paste(country,energy,m$fun, sep=":") )
          rModels[[length(rModels) + 1]] <- rFits[["models"]]
          coefs[[length(coefs) + 1]] <- rFits[["coeffs"]]
        }, 
        error=function(e) {
          cat(paste0("  *** Skipping ", energy, " for ", country, "\n"))
          print(e)
        }
        )
      }
    }
  }
}

coefs2 <- do.call(rbind.fill, coefs)


