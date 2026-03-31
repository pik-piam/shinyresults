# appResultsLocal

version of appResults which is optimized to run on a local model folder.
In contrast to appResults, appResultsLocal only requires the path to an
output folder (with subfolders for each run).

## Usage

``` r
appResultsLocal(folder = "output/", valfile = NULL)
```

## Arguments

- folder:

  output folder containing the runs to be analyzed as subfolders (e.g.
  folder "output" in a MAgPIE model folder)

- valfile:

  Path to a validation file, preferably in rds format, but can also be
  provided as mif (in the latter case it will be converted to rds
  first). If not path is given the function will look automatically for
  an validation file in the output folder

## See also

[`appResults`](appResults.md)

## Author

Jan Philipp Dietrich, Lavinia Baumstark
