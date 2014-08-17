require(EconData)
require(EconModels)

# setwd() to the location of this file.
data <- loadPostProcessedData(Source = "REXS1960", kind = "fitted", dir="../data_postprocessed/")
data <- subset(data, subset=model=="cd" & energy=="none" & Country=="US")
spaghettiPlot(data = data, facet_formula=Country ~ energy, level=1.0 ) + labs(y="Indexed GDP (1960=1)")
