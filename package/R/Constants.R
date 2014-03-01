
# Statistical significance levels. We'll work with 95% CIs
#' @export
ciLevel <- 0.95
#' @export
ciHalfLevel <- ciLevel + (1.0-ciLevel)/2.0
#' @export
ciVals <- c(lower=1-ciHalfLevel, upper=ciHalfLevel)

# List of countries
#' @export
countryAbbrevs <- c(US="US", UK="UK", JP="JP", CN="CN", ZA="ZA", 
					SA="SA", IR="IR", TZ="TZ", ZM="ZM")
#' @export
countryAbbrevsFor3x3Graph <- c(US="US", UK="UK", JP="JP", CN="CN", 
							   ZA="ZA", TZ="TZ", SA="SA", IR="IR", ZM="ZM")
#' @export
countryAbbrevsU <- c(US="US", UK="UK", JP="JP") # Only these countries have useful work data
#' @export
countryAbbrevsAlphU <- sort(countryAbbrevsU)
#' @export
countryAbbrevsForGraphU <- c(US="US", UK="UK", JP="JP")

#' @export
modelTypes <- c('sf', 'cd', 'cde', 'ces', 'cese-(kl)e', 
				'cese-(le)k', 'cese-(ek)l', 'linex')

#' @export
cesNests <- c(kl="(kl)", kle="(kl)e", lek="(le)k", ekl="(ek)l")

#' @export
resampleMethods <- c("resample", "residual", "wild", "debug")

# energy types
#' @export
energyTypes <- c(Q="Q", X="X", U="U") 

# factors of production
#' @export
factors <- c(K="K", L="L", Q="Q", X="X", U="U") 


# Some things in alphabetical order
#' @export
countryAbbrevsAlph <- sort(countryAbbrevs)
#' @export
countryNamesAlph <- c(CN="China", IR="Iran", JP="Japan", 
					  SA="Saudi Arabia", TZ="Tanzania", UK="United Kingdom", 
					  US="USA", ZA="South Africa", ZM="Zambia") 

#' @export
countryNamesAlphU <- c(JP="Japan", UK="United Kingdom", US="USA") 

#' @export
yLimitsForGDPGraphs <- list(c(1,10), c(1,4), c(1,4), c(1,4), c(1,4), c(1,4), c(1,4), c(1,4), c(1,4))



########### Several global parameters for graphs. Set here and use below to ensure consistent appearance of graphs.
# Set the order for presenting countries in 3x3 lattice graphs. Default is alphabetical. 
# "1" means first alphabetically.
#' @export
countryOrderForGraphs <- c(7,6,3,1,8,5,4,2,9) # Sets the order as US, UK, JP, CN, ZA, TZ, SA, IR, ZM.
#' @export
countryOrderForGraphsU <- c(3, 2, 1) # Sets the order as US, UK, JP when we're looking only at energyType=U.
#' @export
timeTics <- c(1980, 1990, 2000, 2010)
#' @export
yTicsForIndexedGraphs <- c(1,2,3,4,5,6,7,8,9,10) # y tic mark locations.


# Full page lattice plot sizes
#' @export
ninePanelLayoutSpec <- c(3,3) # indicates a 3x3 arrangement of panels.
#' @export
threePanelLayoutSpec <- c(3,1) # indicates a 1 row x 3 column arangement of panels
#' @export
onePanelLayoutSpec <- c(1,1) # indicates a 1x1 arrangement of panels.

# Graph Heights
#' @export
maxHeight <- 8.5 # Inches
#' @export
ninePanelScatterGraphHeight <- maxHeight # Inches
#' @export
ninePanelTernaryGraphHeight <- 7 # Inches
#' @export
ninePanelSpaghettiGraphHeight <- 7 # Inches

# Graph Widths
#' @export
maxWidth <- 6.5 # Inches
#' @export
fourPanelScatterGraphWidth <- maxWidth # Inches
#' @export
threePanelScatterGraphWidth <- maxWidth*0.75 # Inches
#' @export
onePanelScatterGraphWidth <- maxWidth*0.25
#' @export
fourPanelTernaryGraphWidth <- maxWidth
#' @export
twoPanelTernaryGraphWidth <- maxWidth*0.45
#' @export
fourPanelSpaghettiGraphWidth <- maxWidth
#' @export
twoPanelSpaghettiGraphWidth <- maxWidth*0.55
#' @export
onePanelSpaghettiGraphWidth <- maxWidth*0.4

#' @export
presentationGraph1RowHeight <- 2.8 # Inches
#' @export
presentationGraph2ColWidth <- 2 # Inches
#' @export
presentationGraph1ColWidth <- 2*presentationGraph2ColWidth
#' @export
keyTextSize <- 0.85 # 85% of normal size
#' @export
keyColumns <- 1 # Want only 1 column in the key for lattice graphs
#' @export
defaultKeyXLoc <- 0.01 # x position of the key. This default is good for 9-panel graphs.
#' @export
defaultKeyYLoc <- 0.95 # y position of the key. This default is good for 9-panel graphs.
# Parameter graphs are tricky to ensure same size. I've found that with height set to 5 in for the 
# cd graphs and 2.93 in for the sf graphs, I get a nearly-exact match with graph size. From these settings I 
# can set up 2 equations:
#    5 in = 2*hgraph + haxis
#    2.93 in = hgraph + haxis
# to estimate that h_axis = 0.86 in and hgraph = 2.05 in.
#' @export
h_param_graph <- 2.07 # Inches for a single panel
#' @export
h_param_axis <- 0.86 # Inches (also includes whatever margin is applied)
#' @export
sfParameterGraphHeight <- h_param_graph + h_param_axis
#' @export
cdParameterGraphHeight <- 2*h_param_graph + h_param_axis
#' @export
cesParameterGraphHeight <- 3*h_param_graph + h_param_axis
#' @export
linexParameterGraphHeight <- h_param_graph + h_param_axis
#' @export
sfParameterGraphWidth <- maxWidth
#' @export
cdParameterGraphWidth <- maxWidth
#' @export
cdTernaryGraphWidth <- maxWidth/2.2
#' @export
cdSpaghettiGraphWidth <- maxWidth/2.2
#' @export
cesParameterGraphWidth <- maxWidth
#' @export
linexParameterGraphWidth <- maxWidth
# Other graph parameters that apply to all graphs
#' @export
scaleTextSize <- 0.8  # Multiple of normal size
#' @export
scaleTickSize <- -0.5 # 50% of normal size and pointing INWARD!
# Controls nls curve fits. Important thing is warnOnly=TRUE allows resamples to continue, 
# even if there is an error.
#' @export
nlsControl <- nls.control(maxiter=200, 
                          tol=1e-05, 
                          minFactor=1/1024,
                          printEval=FALSE, # Tells whether to print details of curve fit process.
                          warnOnly=TRUE)

# Controls how much data spread will be shown by the gray band on spaghetti graphs.
#' @export
spaghettiGraphLevel <- 0.95 


