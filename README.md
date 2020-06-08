# A collection of shiny apps and modules to visualize/handle model results

R package **shinyresults**, version **0.28.1**

[![Travis build status](https://travis-ci.com/pik-piam/shinyresults.svg?branch=master)](https://travis-ci.com/pik-piam/shinyresults) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1478922.svg)](https://doi.org/10.5281/zenodo.1478922) 

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

Dietrich J, Humpenoeder F (2020). _shinyresults: A collection of shiny
apps and modules to visualize/handle model results_. doi:
10.5281/zenodo.1478922 (URL: https://doi.org/10.5281/zenodo.1478922), R
package version 0.28.1, <URL:
https://github.com/pik-piam/shinyresults>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {shinyresults: A collection of shiny apps and modules to visualize/handle model results},
  author = {Jan Philipp Dietrich and Florian Humpenoeder},
  year = {2020},
  note = {R package version 0.28.1},
  doi = {10.5281/zenodo.1478922},
  url = {https://github.com/pik-piam/shinyresults},
}
```

