#' Load Variable Configuration
#'
#' Loads variable configuration from YAML file with fallback hierarchy.
#' The function searches for configuration in the following order:
#' 1. Explicit path provided via configFile parameter
#' 2. User config at ~/.shinyresults/variables.yaml
#' 3. Package default at inst/extdata/variables_default.yaml
#'
#' @param configFile Optional path to a YAML configuration file
#' @return List containing variable configuration with presets, key_variables, and defaults
#' @author Florian Humpenoeder
#' @importFrom yaml read_yaml
#' @keywords internal
#' @examples
#' \dontrun{
#' # Load default configuration
#' config <- loadVariableConfig()
#'
#' # Load custom configuration
#' config <- loadVariableConfig("path/to/my_variables.yaml")
#' }
loadVariableConfig <- function(configFile = NULL) {
  # Priority order for config locations
  candidates <- c(
    configFile,
    file.path(Sys.getenv("HOME"), ".shinyresults", "variables.yaml"),
    system.file("extdata", "variables_default.yaml", package = "shinyresults")
  )

  # Remove NULL and empty entries
  candidates <- candidates[!is.null(candidates) & nchar(candidates) > 0]

  for (f in candidates) {
    if (file.exists(f)) {
      config <- tryCatch(
        yaml::read_yaml(f),
        error = function(e) {
          warning(paste("Failed to read config file:", f, "-", e$message))
          NULL
        }
      )
      if (!is.null(config)) {
        return(validateVariableConfig(config))
      }
    }
  }

  # Return minimal default if no config found
  warning("No variable configuration found. Using empty defaults.")
  return(list(
    meta = list(version = "1.0"),
    presets = list(),
    key_variables = character(0),
    defaults = list(
      line_plot = list(show_hist = TRUE, show_proj = TRUE, free_y = FALSE, auto_y = FALSE),
      area_plot = list(transpose_grid = FALSE)
    )
  ))
}

#' Validate Variable Configuration
#'
#' Validates the structure of a variable configuration list.
#'
#' @param config List read from YAML file
#' @return Validated and normalized config list
#' @keywords internal
#' @author Florian Humpenoeder
validateVariableConfig <- function(config) {
  if (!is.list(config)) {
    stop("Configuration must be a list")
  }

  # Ensure required sections exist
  if (is.null(config$meta)) {
    config$meta <- list(version = "1.0")
  }

  if (is.null(config$presets)) {
    config$presets <- list()
  }

  if (is.null(config$key_variables)) {
    config$key_variables <- character(0)
  }

  if (is.null(config$defaults)) {
    config$defaults <- list()
  }

  # Validate each preset
  for (name in names(config$presets)) {
    preset <- config$presets[[name]]

    # Ensure preset has a name
    if (is.null(preset$name)) {
      config$presets[[name]]$name <- name
    }

    # Ensure variables list exists
    if (is.null(preset$variables)) {
      warning(paste("Preset", name, "has no variables defined"))
      config$presets[[name]]$variables <- list()
    }

    # Validate each variable entry in preset
    for (i in seq_along(preset$variables)) {
      var <- preset$variables[[i]]
      if (is.null(var$variable)) {
        warning(paste("Variable entry", i, "in preset", name, "missing 'variable' field"))
      }
      # Set default plot_type if not specified
      if (is.null(var$plot_type)) {
        config$presets[[name]]$variables[[i]]$plot_type <- "line"
      }
    }
  }

  return(config)
}

#' Get Preset Names
#'
#' Extract the display names of all presets from a configuration.
#'
#' @param config Variable configuration list from loadVariableConfig()
#' @return Named character vector where names are preset IDs and values are display names
#' @author Florian Humpenoeder
#' @keywords internal
getPresetNames <- function(config) {
  if (is.null(config$presets) || length(config$presets) == 0) {
    return(character(0))
  }

  names_vec <- sapply(config$presets, function(p) {
    if (!is.null(p$name)) p$name else ""
  })

  names(names_vec) <- names(config$presets)
  return(names_vec)
}

#' Get Variables for Preset
#'
#' Extract the list of variables from a specific preset.
#'
#' @param config Variable configuration list from loadVariableConfig()
#' @param presetId ID of the preset to extract variables from
#' @return List of variable configurations, each with 'variable' and 'plot_type' fields
#' @author Florian Humpenoeder
#' @keywords internal
getPresetVariables <- function(config, presetId) {
  if (is.null(config$presets[[presetId]])) {
    return(list())
  }
  return(config$presets[[presetId]]$variables)
}
