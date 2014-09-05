
context('Testing Model Fits')

test_that("cdModel() fits are correct", {

  # Use the US factors of production from the Calvin source
  testData <- subset(EconData::Calvin, Country=="US")  
  
  # Cobb-Douglas without energy
  alpha <- -0.1
  beta <- 1.1
  lambda <- 0.02
  testData$fitGDP <- exp(lambda * testData$iYear) * testData$iK^alpha * testData$iL^beta
  
  # Fit
  modelFree <- cdModel(fitGDP ~ iK + iL + iYear, data = testData, constrained = FALSE, 
                       save.data = TRUE)
  
  expect_equivalent(coef(modelFree), c(0.0, alpha, lambda) )
  
  modelConstrained <- cdModel(fitGDP ~ iK + iL + iYear, data = testData, 
                              constrained = TRUE, save.data = TRUE)
  
  expect_equal(coef(modelConstrained)["alpha"], 0.0)
})