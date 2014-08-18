require(EconData)
require(EconModels)

# setwd() to the location of this file.
ppData <- loadPostProcessedData(Source = "REXS1960", kind = "fitted", dir="../data_postprocessed/")
ppData <- subset(ppData, subset=model=="cd" & energy=="none" & Country=="US")
plot <- spaghettiPlot(data=ppData, facet_formula=Country~energy, level=1.0) + labs(y="Indexed GDP (1960=1)")

# Find any fits where iGDP in 2009 is below 4.
ppData2009 <- subset(ppData, subset=Year==2009 & iGDP.hat<4)

# Draw the plot using a red line for resampleNumber==777
plot + geom_line(data=subset(ppData, resampleNumber==777), mapping=aes(x=Year, y=iGDP.hat, color="red"))

# Make the graph without resample number 777. It looks clean!
spaghettiPlot(data=subset(ppData, subset=resampleNumber != 777 | !resampled), facet_formula=Country~energy, level=1.0) + labs(y="Indexed GDP (1960=1)")

# Try to generate the resamples manually here.
historicalData <- subset(REXS1960, Country=="US")
orig <- cdModel(formula = iGDP ~ iK + iL + iYear, 
                data = historicalData, 
                constrained = TRUE, 
                save.data = TRUE)
resampled <- resampledFits(model = orig, 
                           method = "wild", 
                           n = 1000, 
                           save.data = TRUE, 
                           id = fittingID(Source = "REXS1960", 
                                          fitfun = "cdModel", 
                                          countryAbbrev = "US", 
                                          formula = "iGDP ~ iK + iL + iYear", 
                                          n = 1000),
                            seed=123
)
models <- resampled[[2]]
model777 <- models[[778]] # first model is the original
yhat777 <- yhat(model777)
yhat777 <- as.data.frame(yhat777)
historicalAndResampledData <- cbind(historicalData, yhat777)