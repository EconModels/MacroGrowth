require(EconData)
require(EconModels)
require(plyr)

# setwd() to the location of this file.
ppData <- loadPostProcessedData(Source = "REXS1960", kind = "fitted", dir="../../data_postprocessed/")
ppData <- subset(ppData, subset=model=="cd" & energy=="none" & Country=="US")
plot <- spaghettiPlot(data=ppData, facet_formula=Country~energy, level=1.0) + labs(y="Indexed GDP (1960=1)")
show(plot)

# Find any fits where iGDP in 2009 is below 4. Resample 777 is the problem!
ppData2009 <- subset(ppData, subset=Year==2009 & iGDP.hat<4)
ppData2009_777 <- subset(ppData, subset=resampleNumber==777)

# Get the original resamples from the "models_" file.
resampleModels <- readRDS("models_US_cd_iK+iL.Rdata")
model777 <- resampleModels[[778]] # the first model is the original fit to historical data
data777 <- attr(model777, "data")
yhat777 <- yhat(model777)
data777 <- cbind(data777, yhat777)

# Verify that we have the right model.
cat(paste("Do we have the right model?", all.equal(ppData2009_777$iGDP.hat, data777$yhat777)))

# Draw the plot using a red line for resampleNumber==777
plot <- plot + geom_line(data=subset(ppData, resampleNumber==777), mapping=aes(x=Year, y=iGDP.hat, color="red"))
show(plot)

# Draw dots for the resample data
plot <- plot + geom_point(data=data777, mapping=aes(x=Year, y=iGDP))
show(plot)

# Make the graph without resample number 777. It looks clean!
spaghettiPlot(data=subset(ppData, subset=resampleNumber != 777 | !resampled), facet_formula=Country~energy, level=1.0) + 
  labs(y="Indexed GDP (1960=1)")