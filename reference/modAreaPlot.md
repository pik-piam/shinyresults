# modAreaPlot Module

Shiny module which works together with
[`modAreaPlotUI`](modAreaPlotUI.md) to produce an area plot tab

## Usage

``` r
modAreaPlot(id, report, selectionSets = NULL)
```

## Arguments

- id:

  Module ID string, must match the id used in
  [`modAreaPlotUI`](modAreaPlotUI.md)

- report:

  A reactive containing the report to be visualized

- selectionSets:

  named list of selection sets per filter column (passed to
  [`modFilter`](modFilter.md)). Defaults to the `selectionSets` entry of
  the active `appResults` option.

## See also

[`modAreaPlotUI`](modAreaPlotUI.md), [`appResults`](appResults.md)

## Author

Jan Philipp Dietrich, Florian Humpenoeder
