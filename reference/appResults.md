# appResults

appResults allows to explore and visualize time series of modelling
results.

## Usage

``` r
appResults(
  cfg = getOption("appResults"),
  readFilePar = FALSE,
  variableConfig = NULL,
  port = 3838,
  ...
)
```

## Arguments

- cfg:

  config-file containing information about one or more models on the
  items listed below. Usually you provide these settings in your
  .Rprofile as a list constructed as follows (to explore REMIND results
  please replace all occurrences of magpie with remind): url \<-
  https://rse.pik-potsdam.de/data/magpie/results/rev1 options(appResults
  = list(MAgPIE = list(file = paste0(url, "/reduced_overview.rds"),
  resultsfolder=url, valfile=paste0(url, "/validation.rds"),
  username="xxx", password="yyy")))

  file - Overview file in rds format containing a list of all runs
  available. To get access to all available filters use overview.rds
  (takes 15s longer to load) instead of reduced_overview.rds.
  resultsfolder - folder in which model results are stored in rds
  format. valfile - validation data. Can be a CSV/MIF file or rds file
  with a quitte object (saved with saveRDS). NULL by default; in this
  case the user can upload files directly in the tool. Can also be a
  named character vector of multiple validation files. username -
  username to access "file" and "resultsfolder". password - password to
  access "file" and "resultsfolder". selectionSets - named list of
  selection sets per filter column. Each element is a named list mapping
  set labels to character vectors of values. Example:
  `list(region = list("All EUR regions" = c("EUC", "EUS", "EUW", "DEU")))`

- readFilePar:

  read report data files in parallel (faster) (TRUE) or in sequence
  (FALSE)

- variableConfig:

  Path to a YAML configuration file with variable presets, or NULL to
  use default config. The config defines presets for the Dashboard tab
  and quick-load options in plot tabs. See
  [`loadVariableConfig`](loadVariableConfig.md).

- port:

  Port number for the Shiny app (default: 3838). Using a fixed port
  allows consistent URLs for bookmarking.

- ...:

  additional information to overwrite one of the settings from the cfg
  directly: file, resultsfolder, valfile, username, password, or
  selectionSets.

## Author

Florian Humpenoeder, Jan Philipp Dietrich, Lavinia Baumstark, Pascal
Sauer
