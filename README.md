
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MacroGrowth

The MacroGrowth package provides sophisticated tools for fitting
macroeconomic growth models to data. MacroGrowth has several desirable
features for economic modelers:

  - Functions that fit several different macroeconomic growth models.
  - Functions fit along boundaries of the economically-meaningful
    region, thereby ensuring that the best possible fit is found.
  - Options to perform residual resampling, thereby providing a way to
    assess the variability of estimated parameters.
  - A consistent formula-based interface to fitting functions.

The functions in this package were developed for and used first in the
paper Heun, M.K., J. Santos, P.E. Brockway, R. Pruim, T. Domingos, and
M. Sakai. “From Theory to Econometrics to Energy Policy: Cautionary
Tales for Policymaking Using Aggregate Production Functions”.
*Energies*, pp. 1-44, 10 (203), 2017.
<http://doi.org/10.3390/en10020203>.

## Installation

You can install MacroGrowth from github with:

``` r
# install devtools if not already installed
# install.packages("devtools")
devtools::install_github("EconModels/MacroGrowth")
# To get vignettes built locally, use
devtools::install_github("EconModels/MacroGrowth", build_vignettes = TRUE)
```

## More Information

Find more information, including vignettes and function documentation at
<https://econmodels.github.io/MacroGrowth/>
