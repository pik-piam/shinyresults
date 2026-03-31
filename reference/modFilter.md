# modFilter Module

Shiny module which works together with [`modFilterUI`](modFilterUI.md)
to filter a data set based on user input

## Usage

``` r
modFilter(
  id,
  data,
  exclude = NULL,
  showAll = FALSE,
  multiple = NULL,
  xdata = NULL,
  xdataExclude = NULL,
  order = NULL,
  name = NULL,
  preselectYear = NULL,
  preselectMinDate = NULL,
  selectionSets = NULL
)
```

## Arguments

- id:

  Module ID string, must match the id used in
  [`modFilterUI`](modFilterUI.md)

- data:

  A reactive returning a data.table with observations in rows and filter
  options in columns

- exclude:

  names of columns that should be not used as filter

- showAll:

  FALSE \| If set to TRUE all available filter are shown and the filter
  selector is hidden

- multiple:

  vector with booleans for each filter defining whether multiple
  selections are allowed or not. If information is not provided it is
  assumed that multiple selection is allowed

- xdata:

  additional data.tables which should be filtered by the same rules as
  data. If provided the format of the return value changes

- xdataExclude:

  similar to exclude a vector of filters that should be ignored for
  xdata. Useful if xdata should only filtered for a subset of filters
  applied to data

- order:

  order the filter should be listed (provided as a vector of filter
  names). Filter not listed here will be shown after the ones mentioned.

- name:

  name used to identify the filter in the log

- preselectYear:

  if provided the year filter will be preselected with this value

- preselectMinDate:

  if provided the date filter will be preselected with this as lower
  value

- selectionSets:

  named list of selection sets per filter column. Each element is a
  named list mapping set labels to character vectors of values. When a
  set is selected, all its member values are added to the selection.
  Only sets where all members are present in the data are shown.
  Example:
  `list(region = list("All EUR regions" = c("EUC", "EUS", "EUW", "DEU")))`

## Value

a reactive list with x as the filtered data and xdata containing the
list of additional, filtered data element.

## See also

[`modFilterUI`](modFilterUI.md), [`appModelstats`](appModelstats.md)

## Author

Jan Philipp Dietrich
