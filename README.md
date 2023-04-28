# A collection of shiny apps and modules to visualize/handle model results

R package **shinyresults**, version **0.29.4**

[![CRAN status](https://www.r-pkg.org/badges/version/shinyresults)](https://cran.r-project.org/package=shinyresults) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1478922.svg)](https://doi.org/10.5281/zenodo.1478922) [![R build status](https://github.com/pik-piam/shinyresults/workflows/check/badge.svg)](https://github.com/pik-piam/shinyresults/actions) [![codecov](https://codecov.io/gh/pik-piam/shinyresults/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/shinyresults) [![r-universe](https://pik-piam.r-universe.dev/badges/shinyresults)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

A collection of tools which allow to manipulate and analyze code.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("shinyresults")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Jan Philipp Dietrich <dietrich@pik-potsdam.de>.

## Citation

To cite package **shinyresults** in publications use:

Dietrich J, Humpenoeder F (2023). _shinyresults: A collection of shiny apps and modules to visualize/handle model results_. doi: 10.5281/zenodo.1478922 (URL: https://doi.org/10.5281/zenodo.1478922), R package version 0.29.4, <URL: https://github.com/pik-piam/shinyresults>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {shinyresults: A collection of shiny apps and modules to visualize/handle model results},
  author = {Jan Philipp Dietrich and Florian Humpenoeder},
  year = {2023},
  note = {R package version 0.29.4},
  doi = {10.5281/zenodo.1478922},
  url = {https://github.com/pik-piam/shinyresults},
}
```
