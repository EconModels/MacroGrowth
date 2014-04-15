
require(EconModels2)
require(plyr)  # for rbind.fill()
nestStr <- function(nest) paste(nest, collapse="")

All <- read.table("data/AllData.txt", header=TRUE)
Countries <- unique(All$Country)
Energies <- c("iQ", "iX", "iU")

ModelInfos <- list(
  list( formulaStr = c("iGDP ~ iCapStk + iYear", 
                       "iGDP ~ iLabor + iYear",
                       "iGDP ~ energy + iYear"),
        fun = "singleFactorModel",
        dots = list()),
  list( formulaStr = c("iGDP ~ iCapStk + iLabor + iYear",
                       "iGDP ~ iCapStk + iLabor + energy + iYear"),
        fun = "cobbDouglasModel",
        dots = list()),
  list( formulaStr = "iGDP ~ iCapStk + iLabor + energy + iYear",
        fun = "linexModel",
        dots = list()),
  list( formulaStr = "iGDP ~ iCapStk + iLabor + iYear",
        fun = "cesModel",
        dots = list(nest=1:2)),
  list( formulaStr = "iGDP ~ iCapStk + iLabor + energy + iYear",
        fun = "cesModel",
        dots = list(nest=1:3)),
  list( formulaStr = "iGDP ~ iCapStk + iLabor + energy + iYear",
        fun = "cesModel",
        dots = list(nest=c(2,3,1))),
  list( formulaStr = "iGDP ~ iCapStk + iLabor + energy + iYear",
        fun = "cesModel",
        dots = list(nest=c(1,3,2)))
)

# ModelInfos <- head(ModelInfos, -3)  # skip ces models with energy
# ModelInfos <- head(ModelInfos, -4)  # skip all ces models
# ModelInfos <- tail( ModelInfos,2)

oModels <- list()
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
          if (is.null(m$dots$nest)) {
            oModels[[country]][[m$fun]][[formulaStr]] <- oModel
          } else {
            oModels[[country]][[m$fun]][[formulaStr]][[nestStr(m$dots$nest)]] <- oModel
          }
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

saveRDS(oModels, file="data_orig/oModels.Rdata")

