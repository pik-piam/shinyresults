# A collection of shiny apps and modules to visualize/handle model results

R package **shinyresults**, version **0.37.2**

[![CRAN
status](https://www.r-pkg.org/badges/version/shinyresults)](https://cran.r-project.org/package=shinyresults)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1478922.svg)](https://doi.org/10.5281/zenodo.1478922)
[![R build
status](https://github.com/pik-piam/shinyresults/workflows/check/badge.svg)](https://github.com/pik-piam/shinyresults/actions)
[![codecov](https://codecov.io/gh/pik-piam/shinyresults/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/shinyresults)
[![r-universe](https://pik-piam.r-universe.dev/badges/shinyresults)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

A collection of shiny apps and modules to explore and visualize time
series of modelling results.

## Installation

For installation of the most recent package version an additional
repository has to be added in R:

``` r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```

The additional repository can be made available permanently by adding
the line above to a file called `.Rprofile` stored in the home folder of
your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using
`install.packages`:

``` r
install.packages("shinyresults")
```

Package updates can be installed using `update.packages` (make sure that
the additional repository has been added before running that command):

``` r
update.packages()
```

## Tutorial

The package comes with a vignette describing the basic functionality of
the package and how to use it. You can load it with the following
command (the package needs to be installed):

``` r
vignette("configuring-shiny") # Configuring shinyresults
```

## Questions / Problems

In case of questions / problems please contact Jan Philipp Dietrich
<dietrich@pik-potsdam.de>.

## Citation

To cite package **shinyresults** in publications use:

Dietrich J, Humpenoeder F, Sauer P, Rein P (2026). “shinyresults: A
collection of shiny apps and modules to visualize/handle model results.”
<doi:10.5281/zenodo.1478922> <https://doi.org/10.5281/zenodo.1478922>,
Version: 0.37.2, <https://github.com/pik-piam/shinyresults>.

A BibTeX entry for LaTeX users is

``` latex
@Misc{,
 title = {shinyresults: A collection of shiny apps and modules to visualize/handle model
   results},
 author = {Jan Philipp Dietrich and Florian Humpenoeder and Pascal Sauer and Patrick Rein},
 doi = {10.5281/zenodo.1478922},
 date = {2026-03-31},
 year = {2026},
 url = {https://github.com/pik-piam/shinyresults},
 note = {Version: 0.37.2},
}
```
