
context('Testing Model Fits')

test_that("cdModel() fits are correct", {

  # Use the US factors of production from the Calvin source
  testData <- subset(Calvin, Country=="US")  
  
  # Cobb-Douglas without energy
  alpha <- -0.1
  beta <- 1.1
  lambda <- 0.02
  testData$fitGDP <- exp(lambda * cData$iYear) * cData$iK^alpha * cData$iL^beta
  
  # Fit
  modelFree <- cdModel(fitGDP ~ iK + iL + iYear, data = cData, constrained = FALSE, 
                       save.data = TRUE)
  
  expect_equal(modelFree$coefficients[["logscale"]], 0.0 )
  expect_equal(modelFree$coefficients[["lambda"]], lambda)
  expect_equal(modelFree$coefficients[["alpha"]], alpha )
  
  modelConstrained <- cdModel(fitGDP ~ iK + iL + iYear, data = cData, 
                              constrained = TRUE, save.data = TRUE)
  
  expect_equal(modelContrained$coefficients[["alpha"]], 0.0)
})