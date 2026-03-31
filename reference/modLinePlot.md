# modLinePlot Module

Shiny module which works together with
[`modLinePlotUI`](modLinePlotUI.md) to produce a line plot tab

## Usage

``` r
modLinePlot(id, report, validation, selectionSets = NULL)
```

## Arguments

- id:

  Module ID string, must match the id used in
  [`modLinePlotUI`](modLinePlotUI.md)

- report:

  A reactive containing the report to be visualized

- validation:

  A reactive containing validation data to be shown

- selectionSets:

  named list of selection sets per filter column (passed to
  [`modFilter`](modFilter.md)). Defaults to the `selectionSets` entry of
  the active `appResults` option.

## See also

[`modLinePlotUI`](modLinePlotUI.md), [`appResults`](appResults.md)

## Author

Florian Humpenoeder, Jan Philipp Dietrich
