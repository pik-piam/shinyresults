# modDashboard Server Module

Shiny module which works together with
[`modDashboardUI`](modDashboardUI.md) to produce a dashboard with
pre-configured plots based on variable presets.

## Usage

``` r
modDashboard(id, report, validation, config)
```

## Arguments

- id:

  Module ID string, must match the id used in
  [`modDashboardUI`](modDashboardUI.md)

- report:

  A reactive list containing the report data (from modRunSelect)

- validation:

  A reactive containing validation data to be shown

- config:

  Variable configuration list from loadVariableConfig()

## See also

[`modDashboardUI`](modDashboardUI.md), [`appResults`](appResults.md),
[`loadVariableConfig`](loadVariableConfig.md)

## Author

Florian Humpenoeder
