
context('Testing CES Model Fits')

library(dplyr, quietly = TRUE)
# library(micEconCES)

test_that("cesModel() fits without energy give same results with either nesting.", {
  testData <- EconUK

  # These models should all give the same results
  model12 <-
    cesModel(iGDP ~ iK + iL + iYear, data = testData, nest = c(1, 2), digits = 30)
  model21 <-
    cesModel(iGDP ~ iL + iK + iYear, data = testData, nest = c(2, 1), digits = 30)
  # Because both the order of the factors of production and the nest change,
  # the coef values from the model should be identical
  expect_equivalent(
    coef(model12),
    coef(model21)
  )

  # Because the models are the same except for the order of the factors, the roles of
  # alpha_1 and alpha_2 are reversed.
  expect_equivalent(
    naturalCoef(model12)["alpha_1", "alpha_2"],
    naturalCoef(model21)["alpha_2", "alpha_1"])
  expect_equivalent(
    sort(naturalCoef(model12)),
    sort(naturalCoef(model21)))

  # In this case, we have maintained the order of the factors of production,
  # but we have a new nest.  However, naturalCoef should associate alpha_1, alpha_2, and alpha_3
  # with the variables in the order they appear in the production function.
  # Thus, we expect the following test to pass.
  modelkl21 <-
    cesModel(iGDP ~ iK + iL + iYear, data = testData, nest = c(2, 1), digits = 30)
  expect_equivalent(
    naturalCoef(model12)[c("alpha_1", "alpha_2", "alpha_3")],
    naturalCoef(modelkl21)[c("alpha_1", "alpha_2", "alpha_3")])
})


test_that("cesModel() fits without energy are correct", {


  # Concoct some data and add it to testData
  scale <- 1.0 # cesEst calls this "gamma"
  lambda <- 0.02
  delta <- 0.3
  rho <- 0.4
  nu <- 1.0
  testData <- EconUK
  testData <-
    EconUK %>%
    mutate(fitGDP = cesCalc(xNames = c("iK", "iL"), data = testData,
                            coef = c(gamma=scale, lambda=lambda, delta=delta, rho=rho, nu=nu),
                            tName = "iYear")
    )

  # Try a manual fit using cesEst.
  model_manual <-
    cesEst(yName = "fitGDP", xNames = c("iK", "iL"), data = testData,
           tName = "iYear", method = "PORT", multErr = TRUE)
  expect_equivalent(tolerance = 1E-6,
    coef(model_manual)[c("gamma", "lambda", "delta", "rho")],
    c(scale, lambda, delta, rho))

  # Fit using cesModel()
  modelces <-
    cesModel(fitGDP ~ iK + iL + iYear, data = testData, nest = c(1, 2), digits = 30)
  expect_equivalent(tolerance = 1e-6,
    coef(modelces)[c("gamma", "lambda", "delta", "rho"), drop=TRUE],
    c(scale, lambda, delta, rho))

  # Check the values of alpha_1, alpha_2, and alpha_3 calculated by naturalCoef.
  expect_equivalent(
    naturalCoef(modelces)[c("alpha_1", "alpha_2", "alpha_3")],
    c(delta, 1 - delta, 0))


  # Try data near a boundary, delta = 0.95
  # When exploring a bug, I noted that cesEst (and cesModel) have
  # difficulty very near the boundary (delta = 0.99).
  # I suppose that is not unexpected.
  # For example, with delta = 0.99, cesEst fails the tests below,
  # i.e., it does not not land on the variables (scale, lambda, delta, rho) that were used
  # to create the fitGDP3 time series in the first place.
  # But, at delta = 0.95, both cesEst and cesModel work fine.
  # Thus, I left delta = 0.95 in the test.
  scale <- 1.0 # cesEst calls this "gamma"
  lambda <- 0.02
  delta <- 0.95
  rho <- 0.4
  nu <- 1.0

  testData <-
    EconUK %>%
    mutate(
      fitGDP3 =
        cesCalc(xNames = c("iK", "iL"), data = testData,
                coef = c(gamma=scale, lambda = lambda, delta = delta, rho = rho, nu = nu),
                tName = "iYear")
    )

  # Try a manual fit using cesEst.
  model_manual_3 <-
    cesEst(yName = "fitGDP3", xNames = c("iK", "iL"), data = testData,
           tName = "iYear", method = "PORT", multErr = TRUE)
  expect_equivalent(
    coef(model_manual_3)[c("gamma", "lambda", "delta", "rho")],
    c(scale, lambda, delta, rho))
  # Use cesModel
  modelces3 <-
    cesModel(fitGDP3 ~ iK + iL + iYear, data = testData, nest = c(1, 2), digits = 30)
  expect_equivalent(
    coef(modelces3)[c("gamma", "lambda", "delta", "rho"), drop=TRUE],
    c(scale, lambda, delta, rho))
})

test_that("cesModel() fits with energy are the same with any nesting", {

  testData <- EconUK

  # These models should all give the same results
  model123 <-
    cesModel(iGDP ~ iK + iL + iXp + iYear, data = testData, nest = c(1, 2, 3), digits = 30)
  model132 <-
    cesModel(iGDP ~ iK + iXp + iL + iYear, data = testData, nest = c(1, 3, 2), digits = 30)
  model213 <-
    cesModel(iGDP ~ iL + iK + iXp + iYear, data = testData, nest = c(2, 1, 3), digits = 30)
  model231 <-
    cesModel(iGDP ~ iXp + iK + iL + iYear, data = testData, nest = c(2, 3, 1), digits = 30)
  model312 <-
    cesModel(iGDP ~ iL + iXp + iK + iYear, data = testData, nest = c(3, 1, 2), digits = 30)
  model321 <-
    cesModel(iGDP ~ iXp + iL + iK + iYear, data = testData, nest = c(3, 2, 1), digits = 30)

  expect_equivalent(
    coef(model123),
    coef(model132))
  expect_equivalent(
    coef(model123),
    coef(model213))
  expect_equivalent(
    coef(model123),
    coef(model231))
  expect_equivalent(
    coef(model123),
    coef(model312))
  expect_equivalent(
    coef(model123),
    coef(model321))
})

test_that("cesModel() fits with energy are correct", {


  # Concoct some data and add it to testData
  scale <- 1.0 # cesEst calls this "gamma"
  lambda <- 0.02
  delta_1 <- 0.3
  delta <- 0.4
  rho_1 <- 0.35
  rho <- 0.4
  nu <- 1.0

  # Concoct some data and add it to testData
  testData <- EconUK
  testData <-
    EconUK %>%
    mutate(
      fitGDPXp =
        cesCalc(xNames = c("iK", "iL", "iXp"), data = testData,
                coef = c(gamma=scale, lambda=lambda, delta_1=delta_1, delta=delta,
                         rho_1=rho_1, rho=rho, nu=nu),
                nested = TRUE,
                tName = "iYear")
    )

  # Try a manual fit using cesEst
  model_manualXP <-
    cesEst(yName = "fitGDPXp", xNames = c("iK", "iL", "iXp"), tName = "iYear",
           data = testData, method = "PORT", multErr = TRUE,
           start = c(gamma=scale, lambda=lambda,
                     delta_1=delta_1, delta=delta),
           rho = rho,
           rho1 = rho_1,
           control=list(iter.max=2000, eval.max=2000))
  expect_equivalent(
    coef(model_manualXP)[c("gamma", "lambda", "delta_1", "delta", "rho_1", "rho")],
    c(scale, lambda, delta_1, delta, rho_1, rho))

  # Fit using cesModel and compare to concocted data
  modelcesXp <- cesModel(fitGDPXp ~ iK + iL + iXp + iYear, data = testData, nest = c(1,2,3), digits = 30)
  expect_equivalent(
    coef(modelcesXp)[c("gamma", "lambda", "delta_1", "delta", "rho_1", "rho"), drop=TRUE],
    c(scale, lambda, delta_1, delta, rho_1, rho))

  # Try to fit using a different energy
  testData <-
    testData %>%
    mutate(
      fitGDPU =
        cesCalc(xNames = c("iK", "iL", "iXu"), data = testData,
                coef = c(gamma=scale, lambda=lambda, delta_1=delta_1, delta=delta,
                         rho_1=rho_1, rho=rho, nu=nu),
                nested = TRUE,
                tName = "iYear")
    )

  # Fit using cesModel and compare to concocted data
  modelcesU <-
    cesModel(fitGDPU ~ iK + iL + iXu + iYear, data = testData, nest = c(1,2,3), digits = 30)
  expect_equivalent(
    coef(modelcesU)[c("gamma", "lambda", "delta_1", "delta", "rho_1", "rho"), drop=TRUE],
    c(scale, lambda, delta_1, delta, rho_1, rho))
})