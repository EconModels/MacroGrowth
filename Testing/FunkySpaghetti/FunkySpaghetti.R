require(EconData)
require(EconModels)
require(plyr)
require(ggplot2)

# setwd() to the location of this file.
ppData <- loadPostProcessedData(Source = "REXS1960", kind = "fitted", dir="../../data_postprocessed/")
ppData <- subset(ppData, subset=model=="cd" & energy=="none" & Country=="US")

# Find any fits where iGDP in 2009 is below 4. Resample 777 is the problem!
ppData2009 <- subset(ppData, subset=Year==2009 & iGDP.hat<4)
ppData2009_777 <- subset(ppData, subset=resampleNumber==777)

# Make the graph without resample number 777. It looks clean!
spaghettiPlot(data=subset(ppData, subset=resampleNumber != 777 | !resampled), facet_formula=Country~energy, level=1.0) + 
  labs(y="Indexed GDP (1960=1)")

# Now make the plot with resample number 777. Funky Spaghetti!
plot <- spaghettiPlot(data=ppData, facet_formula=Country~energy, level=1.0) + labs(y="Indexed GDP (1960=1)")
show(plot)

# Get the original resamples from the "models_" file.
resampleModels <- readRDS("models_US_cd_iK+iL.Rdata")
model777 <- resampleModels[[778]] # the first model is the original fit to historical data
data777 <- attr(model777, "data")
yhat777 <- yhat(model777)
data777 <- cbind(data777, yhat777)

# Verify that we have the right model.
cat(paste("Do we have the right model?", all.equal(ppData2009_777$iGDP.hat, data777$yhat777)))

# Draw the plot using a red line for resampleNumber==777
plot <- plot + geom_line(data=ppData2009_777, mapping=aes(x=Year, y=iGDP.hat, color=as.character(resampleNumber)))
show(plot)

# Draw dots for the resample data
plot <- plot + geom_point(data=data777, mapping=aes(x=Year, y=iGDP))
show(plot)

# Fit data777 using our functions
newModel777 <- cdModel(formula = iGDP ~ iK + iL + iYear, data = data777, constrained = TRUE, save.data = TRUE)
# Compare coefficients of cdModel777 to coefficients of model777
cat(paste("Are all coefficients of model777 and newModel777 same?", all.equal(model777$coefficients, newModel777$coefficients)))
cat(paste("Are all residuals of model777 and newModel777 same?", all.equal(model777$residuals, newModel777$residuals)))
# So, using our own cdModel function on the same data is consistent, as expected.

# What about using manual lm on the data?
# Fit without constraints
lmModel <- lm(log(iGDP) - log(iL) ~ I(log(iK) - log(iL)) + iYear, data = data777)
cat(paste("alpha =", lmModel$coefficients[[2]]), "so we need to fit with constraints of alpha = 0.")
# Get the fitted response from lmModel. Need to take exp(), because fit was accomplished 
# in log-transform space.
data_manual_fit <- as.data.frame(cbind(data777$Year, exp(fitted(lmModel) + log(data777$iL))))
# Rename columns
names(data_manual_fit)[1]<-"Year"
names(data_manual_fit)[2]<-"yhat"
data_manual_fit$Source <- "exp_fitted_lmModel"
# Add to plot
plot <- plot + geom_line(data=data_manual_fit, mapping=aes(x=Year, y=yhat, color = as.character(Source)))
show(plot)

# Fit with constraint of alpha = 0, because the unconstrainted fit gave alpha < 0.
lmModelConstrained <- lm(log(iGDP) - log(iL) ~ iYear, data = data777)
# Get the fitted response
data_manual_fit_constrained <- as.data.frame(cbind(data777$Year, exp(fitted(lmModelConstrained) + log(data777$iL))))
# Rename columns
names(data_manual_fit_constrained)[1]<-"Year"
names(data_manual_fit_constrained)[2]<-"yhat"
data_manual_fit_constrained$Source <- "exp_fitted_lmModel_constrained"
# Add to plot
plot <- plot + geom_line(data=data_manual_fit_constrained, mapping=aes(x=Year, y=yhat, 
                                                                       color = as.character(Source)))
show(plot)
cat(paste("Are yhats for manual lm constrained and unconstrained same?", 
          all.equal(data_manual_fit$yhat, data_manual_fit_constrained$yhat))) # Pretty close!

# Now compare to model777
cat(paste("Is logscale same?", all.equal(model777$coefficients[[1]], lmModelConstrained$coefficients[[1]])))
# Well, that didn't work. What about lambda?
cat(paste("Is lambda same?", all.equal(model777$coefficients[[2]], lmModelConstrained$coefficients[[2]])))

# The above analysis shows that the manual fit using lm is better than cdModel. A bug in cdModel?
