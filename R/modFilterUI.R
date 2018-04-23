#' modFilterUI Module
#'
#' Corresponding user interface to \code{\link{modFilter}} to filter a data set based on user input
#'
#' @param id id of the filter
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{modFilter}}, \code{\link{appModelstats}}
#' @importFrom shiny NS
#' @export

modFilterUI <- function(id) {
  ns <- NS(id)
  tags$div(id=ns("filterbox"),
           selectInput(inputId = ns("filter"),
                       label = "Choose a filter",
                       choices = ""),
           tags$div(id=ns("filterend")),
           tags$hr(),
           textOutput(ns("observations")))
}