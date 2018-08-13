
context('Testing Model Fits')

library(dplyr, quietly = TRUE)
set.seed(12345)
fuzz <- rlnorm(nrow(EconUK), 0, 1E-5)

test_that("cdModel() without energy fits are correct", {

  # Cobb-Douglas without energy
  alpha <- -0.1
  beta <- 1.1
  gamma <- 0
  lambda <- 0.02
  testData <-
    EconUK %>%
    mutate(perfect = exp(lambda * iYear) * iK^alpha * iL^beta,
           # small amount of noise to avoid singularity
           fitGDPnoe = perfect * fuzz
           )

  # Fit without constraints
  modelFree <-
    cdModel(fitGDPnoe ~ iK + iL + iYear, data = testData,
            constrained = FALSE, save.data = TRUE)
  # We should obtain scale = 1, because every data point should fit nearly exactly.
  # We should obtain gamma = 0, because there is no energy.
  # Loosen tolerance since things were perturbed a bit to avoid singularity
  expect_equivalent(tolerance = 5E-3,
    naturalCoef(modelFree)[, c("scale", "alpha_1", "alpha_2", "alpha_3", "lambda"), drop=TRUE],
    c(1, alpha, beta, gamma, lambda)
  )
  # Switch K and L in the formula to see if the results change appropriately
  modelFree2 <-
    cdModel(fitGDPnoe ~ iL + iK + iYear, data = testData,
            constrained = FALSE, save.data = TRUE)
  expect_equivalent(
    naturalCoef(modelFree) [, c("scale", "alpha_1", "alpha_2", "lambda"), drop=TRUE],
    naturalCoef(modelFree2)[, c("scale", "alpha_2", "alpha_1", "lambda"), drop=TRUE]
  )
  expect_equivalent(naturalCoef(modelFree2)[, "alpha_3"], 0)

  # Hit the constraint of alpha < 0, beta > 1.
  modelConstrained <- cdModel(fitGDPnoe ~ iK + iL + iYear, data = testData,
                              constrained = TRUE, save.data = TRUE)
  # scale is not expected to be exactly 1.0, because we will not exactly fit the data.
  # lambda is not expected to be exactly the value we used to create testData,
  # because we will not exactly fit the data when constrained.
  # alpha_1 will be 0 instead of -0.1.
  expect_equivalent(tolerance = 5E-3,
    naturalCoef(modelConstrained)[, c("alpha_1", "alpha_2", "alpha_3"), drop=TRUE],
    c(0, 1, gamma))

  # Hit the constraint of beta < 0, alpha > 1.
  alpha <- 1.1
  beta <- -0.1
  set.seed(12345)
  testData <-
    testData %>%
    mutate(
      perfect = exp(lambda * iYear) * iK^alpha * iL^beta,
      fitGDPnoebeta = perfect * fuzz
    )

  modelConstrainedbeta <-
    cdModel(fitGDPnoebeta ~ iK + iL + iYear, data = testData,
            constrained = TRUE, save.data = TRUE)

  expect_equivalent(tolerance = 5E-3,
    naturalCoef(modelConstrainedbeta)[, c("alpha_1", "alpha_2", "alpha_3"), drop=TRUE],
    c(1, 0, gamma)
  )
})

test_that("cdModel() without energy is correct at edges", {

  # Use the US factors of production from the Calvin source
  testData <- EconUK

  # Cobb-Douglas without energy, alpha=1, beta=0.
  alpha <- 1.0
  beta <- 0.0
  gamma <- 0.0
  lambda <- 0.1
  testData <-
    testData %>%
    mutate(
      fitGDPnoe_alpha1 = exp(lambda * iYear) * iK^alpha * iL^beta * fuzz
      )

  # Fit without constraints
  modelFree_alpha1 <-
    cdModel(fitGDPnoe_alpha1 ~ iK + iL + iYear, data = testData,
            constrained = FALSE, save.data = TRUE)
  # We should obtain scale = 1, because every data point should be fit exactly.
  # We should obtain gamma = 0, because there is no energy.
  expect_equivalent(tolerance = 1E-3,
    naturalCoef(modelFree_alpha1)[, c("scale", "alpha_1", "alpha_2", "alpha_3", "lambda"), drop=TRUE],
    c(1, alpha, beta, gamma, lambda)
  )

  # Now switch alpha and beta.
  # Cobb-Douglas without energy, alpha=1, beta=0.
  alpha <- 0.0
  beta <- 1.0
  gamma <- 0.0
  lambda <- 0.1
  testData <-
    testData %>%
    mutate(fitGDPnoe_beta1 = exp(lambda * iYear) * iK^alpha * iL^beta * fuzz)

  # Fit without constraints
  modelFree_beta1 <-
    cdModel(fitGDPnoe_beta1 ~ iK + iL + iYear, data = testData,
            constrained = FALSE, save.data = TRUE)

  # We should obtain scale = 1, because every data point should be fit exactly.
  # We should obtain gamma = 0, because there is no energy.
  expect_equivalent(tolerance = 1E-3,
    naturalCoef(modelFree_beta1)[, c("scale", "alpha_1", "alpha_2", "alpha_3", "lambda"), drop=TRUE],
    c(1, alpha, beta, gamma, lambda) )
})

test_that("cdModel() with energy fits are correct", {

  # Cobb-Douglas with energy
  alpha <- -0.1
  beta <- 0.8
  gamma <- 0.3
  lambda <- 0.02

  testData <-
    EconUK %>%
    mutate(
      fitGDPe = exp(lambda * iYear) * iK^alpha * iL^beta * iXu^gamma * fuzz
    )

  # Fit without constraints with 3 permutations o inputs
  modelFree1 <-
    cdModel(fitGDPe ~ iK + iL + iXu + iYear, data = testData,
            constrained = FALSE, save.data = TRUE)

  modelFree2 <-
    cdModel(fitGDPe ~ iXu + iK + iL + iYear, data = testData,
            constrained = FALSE, save.data = TRUE)

  modelFree3 <-
    cdModel(fitGDPe ~ iL + iXu + iK + iYear, data = testData,
            constrained = FALSE, save.data = TRUE)

  # We should obtain scale = 1, because every data point should be fit exactly.
  expect_equivalent(tolerance = 1E-3,
    naturalCoef(modelFree1)[, c("scale","alpha_1", "alpha_2", "alpha_3", "lambda"), drop=TRUE],
    c(1, alpha, beta, gamma, lambda) )
  expect_equivalent(tolerance = 1E-3,
    naturalCoef(modelFree2)[, c("scale", "alpha_1", "alpha_2", "alpha_3", "lambda"), drop=TRUE],
    c(1, gamma, alpha, beta, lambda) )
  expect_equivalent(tolerance = 1E-3,
    naturalCoef(modelFree3)[, c("scale", "alpha_1", "alpha_2", "alpha_3", "lambda"), drop=TRUE],
    c(1, beta, gamma, alpha, lambda) )

  # alpha < 0
  alpha <- -0.1
  beta <- 0.2
  gamma <- 0.9
  testData <-
    testData %>%
    mutate(fitGDPalpha0 = exp(lambda * iYear) * iK^alpha * iL^beta * iXu^gamma * fuzz)

  modelAlpha0 <-
    cdModel(fitGDPalpha0 ~ iK + iL + iXu + iYear, data = testData,
            constrained = TRUE, save.data = TRUE)
  # scale is not expected to be exactly 1.0, because we will not exactly fit the data.
  # lambda is not expected to be exactly the value we used to create testData,
  # because we will not exactly fit the data when constrained.
  # alpha will be 0 instead of -0.1.
  expect_equivalent(naturalCoef(modelAlpha0)[, "alpha_1", drop=TRUE], 0)
  # beta and gamma should sum to 1
  expect_equivalent(sum(naturalCoef(modelAlpha0)[, c("alpha_2", "alpha_3")]), 1)

  # beta < 0
  alpha <- 0.3
  beta <- -0.2
  gamma <- 0.9
  testData <-
    testData %>%
    mutate(fitGDPbeta0 = exp(lambda * iYear) * iK^alpha * iL^beta * iXu^gamma * fuzz)

  modelBeta0 <-
    cdModel(fitGDPbeta0 ~ iK + iL + iXu + iYear, data = testData,
            constrained = TRUE, save.data = TRUE)
  # scale is not expected to be exactly 1.0, because we will not exactly fit the data.
  # lambda is not expected to be exactly the value we used to create testData,
  # because we will not exactly fit the data when constrained.
  # beta will be 0 instead of -0.2.
  expect_equivalent(
    naturalCoef(modelBeta0)[, "alpha_2", drop=TRUE],
    0)
  # alpha and gamma should sum to 1
  expect_equivalent(
    sum(naturalCoef(modelBeta0)[, c("alpha_1", "alpha_3")]),
    1)

  # gamma < 0
  alpha <- 0.3
  beta <- 0.9
  gamma <- -0.2
  testData <-
    testData %>%
    mutate(fitGDPgamma0 = exp(lambda * iYear) * iK^alpha * iL^beta * iXu^gamma * fuzz)

  modelGamma0 <-
    cdModel(fitGDPgamma0 ~ iK + iL + iXu + iYear, data = testData,
            constrained = TRUE, save.data = TRUE)
  # scale is not expected to be exactly 1.0, because we will not exactly fit the data.
  # lambda is not expected to be exactly the value we used to create testData,
  # because we will not exactly fit the data when constrained.
  # beta will be 0 instead of -0.2.
  expect_equivalent(
    naturalCoef(modelGamma0)[, "alpha_3", drop=TRUE],
    0)
  # alpha and gamma should sum to 1
  expect_equivalent(
    sum(naturalCoef(modelGamma0)[, c("alpha_1", "alpha_2")]),
    1)

  # alpha = 1
  alpha <- 1.2
  beta <- -0.1
  gamma <- -0.1
  testData <-
    testData %>%
    mutate(fitGDPalpha1 = exp(lambda * iYear) * iK^alpha * iL^beta * iXu^gamma * fuzz)

  modelAlpha1 <-
    cdModel(fitGDPalpha1 ~ iK + iL + iXu + iYear, data = testData,
            constrained = TRUE, save.data = TRUE)

  # scale is not expected to be exactly 1.0, because we will not exactly fit the data.
  # lambda is not expected to be exactly the value we used to create testData,
  # because we will not exactly fit the data when constrained.
  # alpha will be 1 instead of 1.1.
  expect_equivalent(
    naturalCoef(modelAlpha1)[, c("alpha_1", "alpha_2", "alpha_3"), drop=TRUE],
    c(1, 0, 0) )

  # beta = 1
  alpha <- -0.1
  beta <- 1.2
  gamma <- -0.1
  testData <- testData %>%
    mutate(fitGDPbeta1 = exp(lambda * iYear) * iK^alpha * iL^beta * iXu^gamma * fuzz)

  modelBeta1 <-
    cdModel(fitGDPbeta1 ~ iK + iL + iXu + iYear, data = testData,
            constrained = TRUE, save.data = TRUE)
  # scale is not expected to be exactly 1.0, because we will not exactly fit the data.
  # lambda is not expected to be exactly the value we used to create testData,
  # because we will not exactly fit the data when constrained.
  # alpha will be 1 instead of 1.1.
  expect_equivalent(
    naturalCoef(modelBeta1)[, c("alpha_1", "alpha_2", "alpha_3"), drop=TRUE],
    c(0, 1, 0) )

  # gamma = 1
  alpha <- -0.1
  beta <- -0.1
  gamma <- 1.3
  testData <-
    testData %>%
    mutate(fitGDPgamma1 = exp(lambda * iYear) * iK^alpha * iL^beta * iXu^gamma * fuzz)

  modelGamma1 <-
    cdModel(fitGDPgamma1 ~ iK + iL + iXu + iYear, data = testData,
            constrained = TRUE, save.data = TRUE)
  # scale is not expected to be exactly 1.0, because we will not exactly fit the data.
  # lambda is not expected to be exactly the value we used to create testData,
  # because we will not exactly fit the data when constrained.
  # alpha will be 1 instead of 1.1.
  expect_equivalent(
    naturalCoef(modelGamma1)[, c("alpha_1", "alpha_2", "alpha_3"), drop=TRUE],
    c(0, 0, 1) )
})

test_that("linexModel() fits are correct", {

  # Concoct some data and add it to testData.
  a_0 <- 0.2
  c_t <- 0.5
  a_1 <- a_0 * c_t
  testData <-
    EconUK %>%
    mutate(rho_l = iL / iXu,
           rho_k = iK / (0.5 * (iL + iXu)),
           fitGDP = iXu * exp(a_0 * (2 * (1 - 1 / rho_k) + c_t * (rho_l - 1))) * fuzz
    )
  # Use testData to perform a fit.
  modelLinex <-
    linexModel(fitGDP ~ iK + iL + iXu + iYear, data = testData, save.data = TRUE)
  # We expect scale to be 1.0, because we fit exactly
  # We expect a_0 and c_t to be the same as we used to create fitGDP.
  expect_equivalent(tolerance = 1E-3,
    naturalCoef(modelLinex)[, c("scale", "a_0", "c_t", "a_1"), drop = TRUE],
    c(1, a_0, c_t, a_1))
})

test_that("sfModel() fits are correct", {
  # Concoct some data and add it to testData.
  m <- 1
  lambda <- 0.02
  testData <-
    EconUK %>%
    mutate( fitGDP =  exp(lambda * iYear) * iK^m * fuzz,
            fitGDPU = exp(lambda * iYear) * iXu^m * fuzz,
            fitGDP9 = exp(0.9 * iYear) * iL^m * fuzz,
            fitGDP0 = exp(0.0 * iYear) * iL^m * fuzz
            )

  model_manual <-
    lm(log(fitGDP) - log(iK) ~ iYear, data = testData)
  expect_equivalent(tolerance = 1E-3,
    coef(model_manual)[c("(Intercept)", "iYear")],
    c(0, lambda))

  # We expect scale to be 1.0, because we should fit exactly.
  # We expect m and lambda to be the same as we used to create fitGDP.
  modelsf <-
    sfModel(fitGDP ~ iK + iYear, data = testData,
            constrained = TRUE, save.data = TRUE)

  expect_equivalent(tolerance = 1E-3,
    naturalCoef(modelsf)[, c("scale", "lambda", "m"), drop=TRUE],
    c(1, lambda, m))

  modelsfU <- sfModel(fitGDPU ~ iXu + iYear, data = testData, constrained = TRUE, save.data = TRUE)
  expect_equivalent(tolerance = 1E-3,
    naturalCoef(modelsfU)[, c("scale", "lambda", "m"), drop=TRUE],
    c(1, lambda, m))

  # Try with lambda = 0
  modelsfsmalll <- sfModel(fitGDP0 ~ iL + iYear, data = testData, constrained = TRUE, save.data = TRUE)
  expect_equivalent(tolerance = 1E-3,
    naturalCoef(modelsfsmalll)[, c("scale", "lambda", "m"), drop=TRUE],
    c(1, lambda=0, m))

  # Try with very large lambda
  modelsflargel <- sfModel(fitGDP9 ~ iL + iYear, data = testData, constrained = TRUE, save.data = TRUE)
  expect_equivalent(tolerance = 1E-3,
    naturalCoef(modelsflargel)[, c("scale", "lambda", "m"), drop=TRUE],
    c(1, lambda=0.9, m))
})

