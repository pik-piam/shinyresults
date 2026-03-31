# Load Variable Configuration

Loads variable configuration from YAML file with fallback hierarchy. The
function searches for configuration in the following order: 1. Explicit
path provided via configFile parameter 2. User config at
~/.shinyresults/variables.yaml 3. Package default at
inst/extdata/variables_default.yaml

## Usage

``` r
loadVariableConfig(configFile = NULL)
```

## Arguments

- configFile:

  Optional path to a YAML configuration file

## Value

List containing variable configuration with presets, key_variables, and
defaults

## Author

Florian Humpenoeder

## Examples

``` r
if (FALSE) { # \dontrun{
# Load default configuration
config <- loadVariableConfig()

# Load custom configuration
config <- loadVariableConfig("path/to/my_variables.yaml")
} # }
```
