# modRunSelect Module

Corresponding server logic to [`modRunSelectUI`](modRunSelectUI.md) to
select modules runs for further analysis

## Usage

``` r
modRunSelect(
  id,
  file,
  resultsfolder,
  username = NULL,
  password = NULL,
  readFilePar = FALSE,
  restoreIds = NULL,
  lassoIds = NULL
)
```

## Arguments

- id:

  Module ID string, must match the id used in
  [`modRunSelectUI`](modRunSelectUI.md)

- file:

  report data. Can be a CSV/MIF file or rds file with a quitte object
  (saved with saveRDS). file can also be a vector of rds files. NULL by
  default; in this case the user can upload files directly in the tool

- resultsfolder:

  folder in which MAgPIE run results are stored. File must come with a
  overview list called "files"

- username:

  username to be used to access file and resultsfolder

- password:

  password to access file and resultsfolder

- readFilePar:

  read report data files in parallel (faster) (TRUE) or in sequence
  (FALSE)

- restoreIds:

  reactive containing run IDs to restore from a bookmarked URL
  (optional)

- lassoIds:

  reactive containing run IDs selected via lasso/box selection on the
  stats plot (optional)

## Value

a reactive containing a merged data.frame containing results of selected
runs

## See also

[`modFilterUI`](modFilterUI.md), [`appModelstats`](appModelstats.md)

## Author

Jan Philipp Dietrich
