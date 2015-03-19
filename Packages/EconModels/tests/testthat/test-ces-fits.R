
context('Testing CES Model Fits')

library(dplyr)
library(magrittr)

test_that("cesModel() fits without energy give same results with either nesting.", {
  testData <- subset(EconData::Calvin, Country=="US")
  
  # These models should all give the same results 
  model12 <- cesModel(iGDP ~ iK + iL + iYear, data = testData, nest = c(1, 2), digits=30)
  model21 <- cesModel(iGDP ~ iL + iK + iYear, data = testData, nest = c(2, 1), digits=30)
  
  expect_equivalent( coef(model12), coef(model21) )
})


# test_that("cesModel() fits without energy are correct", {
#   
#   # Use the US factors of production from the Calvin source
#   testData <- subset(EconData::Calvin, Country=="US")
#   
#   # Concoct some data and add it to testData
#   scale <- 1.0 # cesEst calls this "gamma"
#   lambda <- 0.02
#   delta <- 0.3
#   rho <- 0.4
#   nu <- 1.0
#   fitGDP <- cesCalc(xNames = c("iK", "iL"), data = testData, 
#                     coef = c(gamma=scale, lambda=lambda, delta=delta, rho=rho, nu=nu),
#                     tName = "iYear")
#   testData <- cbind(testData, fitGDP)
#   
#   # Try a manual fit using cesEst.
#   model_manual <- cesEst(yName = "fitGDP", xNames = c("iK", "iL"), data = testData, 
#                          tName = "iYear", method = "PORT", multErr = TRUE)
#   expect_equivalent(coef(model_manual)[c("gamma", "lambda", "delta", "rho")], 
#                     list(scale, lambda, delta, rho))
#   
#   # Fit using cesModel()
#   modelces <- cesModel(fitGDP ~ iK + iL + iYear, data = testData, nest = c(1, 2), digits=30)
#   expect_equivalent(coef(modelces)[c("gamma", "lambda", "delta", "rho"), drop=TRUE], 
#                     list(scale, lambda, delta, rho))
#   
#   # Try different nesting
#   modelces2 <- cesModel(fitGDP ~ iK + iL + iYear, data = testData, nest = c(2, 1), digits=30)
#   expect_equivalent(coef(modelces2)[c("gamma", "lambda", "delta", "rho"), drop=TRUE], 
#                     list(scale, lambda, 1-delta, rho))
#   
#   # Try data near a boundary, delta = 0.95
#   # When exploring a bug, I noted that cesEst (and cesModel) have 
#   # difficulty very near the boundary (delta = 0.99). 
#   # I suppose that is not unexpected. 
#   # For example, with delta = 0.99, cesEst fails the tests below,
#   # i.e., it does not not land on the variables (scale, lambda, delta, rho) that were used
#   # to create the fitGDP3 time series in the first place. 
#   # But, at delta = 0.95, both cesEst and cesModel work fine. 
#   # Thus, I left delta = 0.95 in the test.
#   scale <- 1.0 # cesEst calls this "gamma"
#   lambda <- 0.02
#   delta <- 0.95
#   rho <- 0.4
#   nu <- 1.0  
#   
#   fitGDP3 <- cesCalc(xNames = c("iK", "iL"), data = testData, 
#                      coef = c(gamma=scale, lambda=lambda, delta=delta, rho=rho, nu=nu),
#                      tName = "iYear")
#   testData <- cbind(testData, fitGDP3)
#   
#   # Try a manual fit using cesEst.
#   model_manual_3 <- cesEst(yName = "fitGDP3", xNames = c("iK", "iL"), data = testData, 
#                            tName = "iYear", method = "PORT", multErr = TRUE)
#   expect_equivalent(coef(model_manual_3)[c("gamma", "lambda", "delta", "rho")], 
#                     list(scale, lambda, delta, rho))
# 
#   modelces3 <- cesModel(fitGDP3 ~ iK + iL + iYear, data = testData, nest = c(1, 2), digits=30)
#   expect_equivalent(coef(modelces3)[c("gamma", "lambda", "delta", "rho"), drop=TRUE], 
#                     list(scale, lambda, delta, rho))
# })

test_that("cesModel() fits with energy are the same with any nesting", {
  
  testData <- subset(EconData::Calvin, Country=="US")
  
  # These models should all give the same results 
  model123 <- cesModel(iGDP ~ iK + iL + iQp + iYear, data = testData, nest = c(1, 2, 3), digits=30)
  model132 <- cesModel(iGDP ~ iK + iQp + iL + iYear, data = testData, nest = c(1, 3, 2), digits=30)
  model213 <- cesModel(iGDP ~ iL + iK + iQp + iYear, data = testData, nest = c(2, 1, 3), digits=30)
  model231 <- cesModel(iGDP ~ iQp + iK + iL + iYear, data = testData, nest = c(2, 3, 1), digits=30)
  model312 <- cesModel(iGDP ~ iL + iQp + iK + iYear, data = testData, nest = c(3, 1, 2), digits=30)
  model321 <- cesModel(iGDP ~ iQp + iL + iK + iYear, data = testData, nest = c(3, 2, 1), digits=30)
  
  expect_equivalent( coef(model123), coef(model132) )
  expect_equivalent( coef(model123), coef(model213) )
  expect_equivalent( coef(model123), coef(model231) )
  expect_equivalent( coef(model123), coef(model312) )
  expect_equivalent( coef(model123), coef(model321) )
  
})

# test_that("cesModel() fits with energy are correct", {
#   
#   # Use the US factors of production from the Calvin source
#   testData <- subset(EconData::Calvin, Country=="US")
#   
#   # Concoct some data and add it to testData
#   scale <- 1.0 # cesEst calls this "gamma"
#   lambda <- 0.02
#   delta_1 <- 0.3
#   delta <- 0.4
#   rho_1 <- 0.35
#   rho <- 0.4
#   nu <- 1.0
#   
#   # Concoct some data and add it to testData
#   fitGDPXp <- cesCalc(xNames = c("iK", "iL", "iXp"), data = testData, 
#                       coef = c(gamma=scale, lambda=lambda, delta_1=delta_1, delta=delta, 
#                                rho_1=rho_1, rho=rho, nu=nu),
#                       nested = TRUE,
#                       tName = "iYear")
#   testData <- cbind(testData, fitGDPXp)
#   
#   # Try a manual fit using cesEst
#   model_manualXP <- cesEst(yName = "fitGDPXp", xNames = c("iK", "iL", "iXp"), tName = "iYear", 
#                            data = testData, method = "PORT", multErr = TRUE,
#                            start = c(gamma=scale, lambda=lambda,
#                                      delta_1=delta_1, delta=delta),
#                            rho = rho,
#                            rho1 = rho_1,
#                            control=chooseCESControl("PORT"))
#   expect_equivalent(coef(model_manualXP)[c("gamma", "lambda", "delta_1", "delta", "rho_1", "rho")], 
#                     list(scale, lambda, delta_1, delta, rho_1, rho))
# 
#   # Fit using cesModel and compare to concocted data
#   modelcesXp <- cesModel(fitGDPXp ~ iK + iL + iXp + iYear, data = testData, nest = c(1,2,3), digits=30)
#   expect_equivalent(coef(modelcesXp)[c("gamma", "lambda", "delta_1", "delta", "rho_1", "rho"), drop=TRUE], 
#                     list(scale, lambda, delta_1, delta, rho_1, rho))
#   
#   # Fit using different nest
#   fitGDPXp2 <- cesCalc(xNames = c("iL", "iK", "iXp"), data = testData, 
#                        coef = c(gamma=scale, lambda=lambda, delta_1=delta_1, delta=delta, 
#                                 rho_1=rho_1, rho=rho, nu=nu),
#                        nested = TRUE,
#                        tName = "iYear")
#   testData <- cbind(testData, fitGDPXp2)
#   
#   # Fit using cesModel and compare to concocted data
#   modelcesXp2 <- cesModel(fitGDPXp2 ~ iK + iL + iXp + iYear, data = testData, nest = c(2,1,3), digits=30)
#   expect_equivalent(coef(modelcesXp2)[c("gamma", "lambda", "delta_1", "delta", "rho_1", "rho"), drop=TRUE], 
#                     list(scale, lambda, delta_1, delta, rho_1, rho))
# 
#   # Try with mixed up variables
#   modelcesXp3 <- cesModel(fitGDPXp ~ iK + iL + iXp + iYear, data = testData, nest = c(2,1,3), digits=30)
#   expect_equivalent(coef(modelcesXp3)[c("gamma", "lambda", "delta_1", "delta", "rho_1", "rho"), drop=TRUE], 
#                     list(scale, lambda, 1 - delta_1, delta, rho_1, rho))
#   
#   # Try to fit using a different energy
#   fitGDPU <- cesCalc(xNames = c("iK", "iL", "iU"), data = testData, 
#                       coef = c(gamma=scale, lambda=lambda, delta_1=delta_1, delta=delta, 
#                                rho_1=rho_1, rho=rho, nu=nu),
#                       nested = TRUE,
#                       tName = "iYear")
#   testData <- cbind(testData, fitGDPU)
# 
#   # Fit using cesModel and compare to concocted data
#   modelcesU <- cesModel(fitGDPU ~ iK + iL + iU + iYear, data = testData, nest = c(1,2,3), digits=30)
#   expect_equivalent(coef(modelcesU)[c("gamma", "lambda", "delta_1", "delta", "rho_1", "rho"), drop=TRUE], 
#                     list(scale, lambda, delta_1, delta, rho_1, rho))
#   
#   # Try to fit using kle nest
#   fitGDPUlek <- cesCalc(xNames = c("iL", "iU", "iK"), data = testData, 
#                         coef = c(gamma=scale, lambda=lambda, delta_1=delta_1, delta=delta, 
#                                  rho_1=rho_1, rho=rho, nu=nu),
#                         nested = TRUE,
#                         tName = "iYear")
#   testData <- cbind(testData, fitGDPUlek)
#   
#   # Fit with cesModel and compare to concocted data
#   modelcesUlek <- cesModel(fitGDPUlek ~ iK + iL + iU + iYear, data = testData, nest = c(2,3,1), digits=30)
#   expect_equivalent(coef(modelcesUlek)[c("gamma", "lambda", "delta_1", "delta", "rho_1", "rho"), drop=TRUE], 
#                     list(scale, lambda, delta_1, delta, rho_1, rho))
# 
#   # Try to fit using ekl nest
#   fitGDPUekl <- cesCalc(xNames = c("iU", "iK", "iL"), data = testData, 
#                         coef = c(gamma=scale, lambda=lambda, delta_1=delta_1, delta=delta, 
#                                  rho_1=rho_1, rho=rho, nu=nu),
#                         nested = TRUE,
#                         tName = "iYear")
#   testData <- cbind(testData, fitGDPUekl)
# 
#   # Fit with cesModel and compare to concocted data
#   modelcesUekl <- cesModel(fitGDPUekl ~ iK + iL + iU + iYear, data = testData, nest = c(3,1,2), digits=30)
#   expect_equivalent(coef(modelcesUekl)[c("gamma", "lambda", "delta_1", "delta", "rho_1", "rho"), drop=TRUE], 
#                     list(scale, lambda, delta_1, delta, rho_1, rho))
# })
