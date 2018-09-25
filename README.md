
<!-- README.md is generated from README.Rmd. Please edit that file -->
MacroGrowth
===========

The `R` package `MacroGrowth` provides sophisticated tools for fitting macroeconomic growth models to data. `MacroGrowth` has several desirable features for economic modelers:

-   Functions that fit several different macroeconomic growth models.
-   Functions fit along boundaries of the economically-meaningful region, thereby ensuring that the best possible fit is found.
-   Options to perform residual resampling, thereby providing a way to assess the variability of estimated parameters.
-   A consistent formula-based interface to fitting functions.

The functions in this package were developed for and used first in [Heun et al. (2017)](https://doi.org/10.3390/en10020203).

Installation
------------

You can install `MacroGrowth` from github with:

``` r
# install devtools if not already installed
# install.packages("devtools")
devtools::install_github("EconModels/MacroGrowth")
# To build vignettes locally, use
devtools::install_github("EconModels/MacroGrowth", build_vignettes = TRUE)
```

More Information
----------------

Find more information, including vignettes and function documentation at <https://econmodels.github.io/MacroGrowth/>

References
----------

Heun, Matthew K., João Santos, Paul E. Brockway, Randall J. Pruim, Tiago Domingos, and Marco Sakai. 2017. “From Theory to Econometrics to Energy Policy: Cautionary Tales for Policymaking Using Aggregate Production Functions.” *Energies* 10 (203): 1–44. doi:[10.3390/en10020203](https://doi.org/10.3390/en10020203).
