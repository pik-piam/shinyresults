# Analyze Model Statistics

Shiny app to analyze statistics collected with
[`runstatistics`](https://rdrr.io/pkg/lucode2/man/runstatistics.html)
and merged with
[`mergestatistics`](https://rdrr.io/pkg/lucode2/man/mergestatistics.html)

## Usage

``` r
appModelstats(
  files = c("https://www.pik-potsdam.de/rd3mod/magpie.rds",
    "https://www.pik-potsdam.de/rd3mod/remind.rds"),
  resultsfolder = NULL
)
```

## Arguments

- files:

  path to rds-files from which statistics should be read

- resultsfolder:

  path to a folder containing model results of the corresponding runs

## Author

Jan Philipp Dietrich
