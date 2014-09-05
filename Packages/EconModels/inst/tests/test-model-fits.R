
context('Testing Model Fits')

test_that("cdModel() without energy fits are correct", {

  # Use the US factors of production from the Calvin source
  testData <- subset(EconData::Calvin, Country=="US")  
  
  # Cobb-Douglas without energy
  alpha <- -0.1
  beta <- 1.1
  gamma <- 0
  lambda <- 0.02
  testData$fitGDPnoe <- exp(lambda * testData$iYear) * testData$iK^alpha * testData$iL^beta
  
  # Fit without constraints
  modelFree <- cdModel(fitGDPnoe ~ iK + iL + iYear, data = testData, constrained = FALSE, 
                       save.data = TRUE)
  # We should obtain scale = 1, because every data point should be fit exactly.
  # We should obtain gamma = 0, because there is no energy.
  expect_equivalent(naturalCoef(modelFree)[, c("scale", "alpha", "beta", "gamma", "lambda"), drop=TRUE], 
                    list(1, alpha, beta, gamma, lambda) )
  # Switch K and L in the formula to see if the results change appropriately
  modelFree2 <- cdModel(fitGDPnoe ~ iL + iK + iYear, data = testData, constrained = FALSE, 
                       save.data = TRUE)
  expect_equivalent(naturalCoef(modelFree2)[, c("scale", "alpha", "beta", "gamma", "lambda"), drop=TRUE], 
                    list(1, beta, alpha, gamma, lambda) )

  # Hit the constraint of alpha < 0, beta > 1.
  modelConstrained <- cdModel(fitGDPnoe ~ iK + iL + iYear, data = testData, 
                              constrained = TRUE, save.data = TRUE)
  # scale is not expected to be exactly 1.0, because we will not exactly fit the data. 
  # lambda is not expected to be exactly the value we used to create testData, 
  # because we will not exactly fit the data when constrained.
  # alpha will be 0 instead of -0.1.
  expect_equivalent(naturalCoef(modelConstrained)[, c("alpha", "beta", "gamma"), drop=TRUE], 
                    list(0, 1, gamma))
  
  # Hit the constraint of beta < 0, alpha > 1.
  alpha <- 1.1
  beta <- -0.1
  testData$fitGDPnoebeta <- exp(lambda * testData$iYear) * testData$iK^alpha * testData$iL^beta
  modelConstrainedbeta <- cdModel(fitGDPnoebeta ~ iK + iL + iYear, data = testData, 
                                  constrained = TRUE, save.data = TRUE)
  expect_equivalent(naturalCoef(modelConstrainedbeta)[, c("alpha", "beta", "gamma"), drop=TRUE], 
                    list(1, 0, gamma))
})

test_that("cdModel() with energy fits are correct", {
  
  # Use the US factors of production from the Calvin source
  testData <- subset(EconData::Calvin, Country=="US")  
  
  # Cobb-Douglas with energy
  alpha <- -0.1
  beta <- 0.8
  gamma <- 0.3
  lambda <- 0.02
  testData$fitGDPe <- exp(lambda * testData$iYear) * testData$iK^alpha * testData$iL^beta * testData$iU^gamma
  
  # Fit without constraints
  modelFree <- cdModel(fitGDPe ~ iK + iL + iU + iYear, data = testData, constrained = FALSE, 
                       save.data = TRUE)
  # We should obtain scale = 1, because every data point should be fit exactly.
  expect_equivalent(naturalCoef(modelFree)[, c("scale","alpha", "beta", "gamma", "lambda"), drop=TRUE], 
                    list(1, alpha, beta, gamma, lambda) )
  # Permute K, L, and E in the formula to see if the results change appropriately
  modelFree2 <- cdModel(fitGDPe ~ iU + iK + iL + iYear, data = testData, 
                        constrained = FALSE, save.data = TRUE)
  expect_equivalent(naturalCoef(modelFree2)[, c("scale", "alpha", "beta", "gamma", "lambda"), drop=TRUE], 
                    list(1, gamma, alpha, beta, lambda) )
  modelFree3 <- cdModel(fitGDPe ~ iL + iU + iK + iYear, data = testData, 
                        constrained = FALSE, save.data = TRUE)
  expect_equivalent(naturalCoef(modelFree3)[, c("scale", "alpha", "beta", "gamma", "lambda"), drop=TRUE], 
                    list(1, beta, gamma, alpha, lambda) )
  
  # Hit the constraint where alpha < 0.
  alpha <- -0.1
  beta <- 0.2
  gamma <- 0.9
  testData$fitGDPalpha0 <- exp(lambda * testData$iYear) * testData$iK^alpha * testData$iL^beta * testData$iU^gamma
  modelAlpha0 <- cdModel(fitGDPalpha0 ~ iK + iL + iU + iYear, data = testData, 
                              constrained = TRUE, save.data = TRUE)
  # scale is not expected to be exactly 1.0, because we will not exactly fit the data. 
  # lambda is not expected to be exactly the value we used to create testData, 
  # because we will not exactly fit the data when constrained.
  # alpha will be 0 instead of -0.1.
  expect_equal(naturalCoef(modelAlpha0)[, "alpha", drop=TRUE], 0)
  # beta and gamma should sum to 1
  expect_equal(sum(naturalCoef(modelAlpha0)[, c("beta", "gamma")]), 1)
  
  # Hit the constraint where beta < 0.
  alpha <- 0.3
  beta <- -0.2
  gamma <- 0.9
  testData$fitGDPbeta0 <- exp(lambda * testData$iYear) * testData$iK^alpha * testData$iL^beta * testData$iU^gamma
  modelBeta0 <- cdModel(fitGDPbeta0 ~ iK + iL + iU + iYear, data = testData, 
                         constrained = TRUE, save.data = TRUE)
  # scale is not expected to be exactly 1.0, because we will not exactly fit the data. 
  # lambda is not expected to be exactly the value we used to create testData, 
  # because we will not exactly fit the data when constrained.
  # beta will be 0 instead of -0.2.
  expect_equal(naturalCoef(modelBeta0)[, "beta", drop=TRUE], 0)
  # alpha and gamma should sum to 1
  expect_equal(sum(naturalCoef(modelBeta0)[, c("alpha", "gamma")]), 1)
  
  # Hit the constraint where gamma < 0.
  alpha <- 0.3
  beta <- 0.9
  gamma <- -0.2
  testData$fitGDPgamma0 <- exp(lambda * testData$iYear) * testData$iK^alpha * testData$iL^beta * testData$iU^gamma
  modelGamma0 <- cdModel(fitGDPgamma0 ~ iK + iL + iU + iYear, data = testData, 
                         constrained = TRUE, save.data = TRUE)
  # scale is not expected to be exactly 1.0, because we will not exactly fit the data. 
  # lambda is not expected to be exactly the value we used to create testData, 
  # because we will not exactly fit the data when constrained.
  # beta will be 0 instead of -0.2.
  expect_equal(naturalCoef(modelGamma0)[, "gamma", drop=TRUE], 0)
  # alpha and gamma should sum to 1
  expect_equal(sum(naturalCoef(modelGamma0)[, c("alpha", "beta")]), 1)
  
  # Hit the constraint where alpha = 1
  
  # Hit the constraint where beta = 1
  
  # Hit the constraint where gamma = 1
  
})

test_that("linexModel() fits are correct", {
  
  # add tests here.
  expect_equal(1,1)
  
  
})
