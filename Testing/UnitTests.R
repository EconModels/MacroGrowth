require(EconModels)
require(EconData)

# Concocting data and testing.

# Use the US factors of production from the Calvin source
cData <- subset(Calvin, Country=="US") # cData stands for "concocted" data

# Cobb-Douglas without energy
alpha <- -0.1
beta <- 1.1
lambda <- 0.02
# Replace real GDP data by the concocted cata
cData$iGDP <- exp(lambda * cData$iYear) * cData$iK^alpha * cData$iL^beta

# Fit
modelFree <- cdModel(iGDP ~ iK + iL + iYear, data = cData, constrained = FALSE, save.data = TRUE)
# Randy: can you convert these into unit tests?
# We expect the following
# assert: modelFree$coefficients[["logscale"]] == 0.0 # within machine precision
# assert: modelFree$coefficients[["lambda"]] == lambda # within machine precision
# assert: modelFree$coefficients[["alpha"]] == alpha # within machine precision

modelConstrained <- cdModel(iGDP ~ iK + iL + iYear, data = cData, constrained = TRUE, save.data = TRUE)
# assert: modelConstrained$coefficients[["alpha"]] == 0.0 # within machine precision
# Do we want any more tests here?