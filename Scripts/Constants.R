# List of countries in our data set.
countryAbbrevs <- c(US="US", UK="UK", JP="JP", CN="CN", ZA="ZA", SA="SA", IR="IR", TZ="TZ", ZM="ZM")
countryAbbrevsAlph <- sort(countryAbbrevs)
countryAbbrevsU <- c(US="US", UK="UK", JP="JP") # Only these countries have useful work data
countryAbbrevsAlphU <- sort(countryAbbrevsU)
countryNamesAlph <- c(CN="China", IR="Iran", JP="Japan", 
                      SA="Saudi Arabia", TZ="Tanzania", UK="United Kingdom", 
                      US="USA", ZA="South Africa", ZM="Zambia") 
countryNamesAlphU <- c(JP="Japan", UK="United Kingdom", US="USA") 
# Models and nests
modelTypes <- c('sf', 'cd', 'cde', 'ces', 'cese-(kl)e', 'cese-(le)k', 'cese-(ek)l', 'linex')
cesNests <- c(kl="(kl)", kle="(kl)e", lek="(le)k", ekl="(ek)l")
# Resampling information
resampleMethods <- c("resample", "residual", "wild", "debug")
# logical names for variables. These are the names of the columns in our data set
Timevar <- "iYear"
GDPvar <- "iY"
Kvar <- "iK"
Lvar <- "iL"
Qvar <- "iQ"
Xvar <- "iX"
Uvar <- "iU"
# Energy types and factors
energyTypes <- c(Q=Qvar, X=Xvar, U=Uvar) 
factors <- c(K=Kvar, L=Lvar, energyTypes)