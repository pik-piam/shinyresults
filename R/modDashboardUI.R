#' modDashboard UI Module
#'
#' Shiny module which works together with \code{\link{modDashboard}} to produce a dashboard
#' with pre-configured plots based on variable presets.
#'
#' @param id id of the module
#' @param presets Named list of presets from variable configuration (not used, kept for compatibility)
#' @author Florian Humpenoeder
#' @seealso \code{\link{modDashboard}}, \code{\link{appResults}}, \code{\link{loadVariableConfig}}
#' @export
modDashboardUI <- function(id, presets = NULL) {
  ns <- NS(id)

  tags$div(
    id = ns("dashboard_container"),
    # Horizontal filter bar at top
    fluidRow(
      column(
        width = 6,
        selectInput(
          ns("scenario_filter"),
          "Scenarios",
          choices = c("Loading..." = ""),
          selected = "",
          multiple = TRUE,
          width = "100%"
        )
      ),
      column(
        width = 6,
        selectInput(
          ns("region_filter"),
          "Region",
          choices = c("Loading..." = ""),
          selected = "",
          width = "100%"
        )
      )
    ),
    tags$hr(),
    # Full width plot area
    uiOutput(ns("dashboard_plots"))
  )
}
