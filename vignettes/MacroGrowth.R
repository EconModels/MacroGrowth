## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(dplyr)
library(ggplot2)
library(magrittr)
library(rlang)
library(tidyr)
library(MacroGrowth)

## ------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(magrittr)
library(rlang)
library(tidyr)
library(MacroGrowth)
head(EconUK)

## ------------------------------------------------------------------------
sffit <- sfModel(formula = iGDP ~ iK + iYear, data = EconUK)
sffit

## ------------------------------------------------------------------------
naturalCoef(sffit)

## ------------------------------------------------------------------------
cdfit <- cdModel(formula = iGDP ~ iK + iL + iYear, data = EconUK)
cdfit

## ------------------------------------------------------------------------
naturalCoef(cdfit)

## ------------------------------------------------------------------------
cesfit <- cesModel(formula = iGDP ~ iK + iL + iYear, data = EconUK)
naturalCoef(cesfit)

## ------------------------------------------------------------------------
linexfit <- linexModel(formula = iGDP ~ iK + iL + iXp + iYear, data = EconUK)
naturalCoef(linexfit)

## ------------------------------------------------------------------------
head(fortify(linexfit))

## ------------------------------------------------------------------------
head(getData(cdfit))

## ------------------------------------------------------------------------
yhat(cdfit)

## ------------------------------------------------------------------------
resid(cdfit)

## ------------------------------------------------------------------------
sum(resid(cdfit)^2)

## ------------------------------------------------------------------------
sfModel(formula = iGDP ~ iK + iYear, data = EconUK) %>% naturalCoef()
sfModel(formula = iGDP ~ iK + iYear, data = EconUK, constrained = TRUE) %>% naturalCoef()

## ------------------------------------------------------------------------
cdModel(formula = iGDP ~ iK + iL + iXu + iYear, 
        data = EconUK %>% filter(Year < 1970), 
        constrained = FALSE) %>% 
  naturalCoef()

## ------------------------------------------------------------------------
cdModel(formula = iGDP ~ iK + iL + iXu + iYear, data = EconUK %>% filter(Year < 1970)) %>% naturalCoef()

## ------------------------------------------------------------------------
cesModel(formula = iGDP ~ iK + iXu + iYear, 
         data = EconUK %>% filter(Year >= 1980 & Year < 1990), 
         constrained = FALSE) %>% 
  naturalCoef()

## ------------------------------------------------------------------------
cesModel(formula = iGDP ~ iK + iXu + iYear, 
         data = EconUK %>% filter(Year >= 1980 & Year < 1990)) %>% 
  naturalCoef()

## ------------------------------------------------------------------------
attr(cesfit, "model.attempts")[[1]] %>% naturalCoef()

## ------------------------------------------------------------------------
cdfits_rs <- resampledFits(model = cdfit, method = "wild", n = 5, seed = 123)

## ------------------------------------------------------------------------
cdfits_rs$coeffs

## ------------------------------------------------------------------------
# Original fit
cdfits_rs$models[[1]] %>% naturalCoef()
# First resampled model
cdfits_rs$models[[2]] %>% naturalCoef()
# Last resampled model
cdfits_rs$models[[6]] %>% naturalCoef()

## ------------------------------------------------------------------------
resampledFits(model = sffit, method = "wild", n = 5)[["coeffs"]]
resampledFits(model = cesfit, method = "wild", n = 5)[["coeffs"]]
resampledFits(model = linexfit, method = "wild", n = 5)[["coeffs"]]

## ---- plotting1, fig.width = 6, fig.align = "center"---------------------
bind_cols(EconUK, yhat(cdfit) %>% as.data.frame() %>% set_names("yhat")) %>% 
  ggplot() + 
  # Add historical data as points
  geom_point(mapping = aes(x = Year, y = iGDP), shape = 1) +
  # Add the fitted model as a line
  geom_line(mapping = aes(x = Year, y = yhat))

## ---- fig.width = 3, fig.align = "center"--------------------------------
triData <- cdModel(formula = iGDP ~ iK + iL + iXu + iYear, data = EconUK) %>%
  resampledFits(method = "wild", n = 20) %>%
  extract2("coeffs")

triPlot(data = triData %>% filter(method == "wild"),
        mapping = aes(x = alpha_1, y = alpha_2, z = alpha_3)) +
  geom_point(data = triData %>% filter(method == "orig"),
             mapping = aes(x = alpha_1, y = alpha_2, z = alpha_3),
             color = "red", alpha = 1, size = 4, shape = 10, stat = "triangle")

