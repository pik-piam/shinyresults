#' modRunSelectUI Module
#'
#' Corresponding user interface to \code{\link{modRunSelect}} to select modules runs for further analysis
#'
#' @param id id of the filter
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{modFilter}}, \code{\link{appModelstats}}
#' @importFrom shiny NS uiOutput
#' @export
modRunSelectUI <- function(id) {
  ns <- NS(id)
  tags$div(id = ns("runselectbox"),
           tags$div(id = "title", titlePanel("Run Selection")),
           uiOutput(ns("loaded_runs_info")),
           modFilterUI(ns("runfilter")),
           tags$hr(),
           actionButton(ns("load"), "Load selection"))
}
