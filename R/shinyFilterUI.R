#' shinyFilterUI Module
#'
#' Corresponding user interface to \code{\link{shinyFilter}} to filter a data set based on user input
#'
#' @param id id of the filter
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{shinyFilter}}, \code{\link{modelstats}}
#' @importFrom shiny NS
#' @export

shinyFilterUI <- function(id) {
  ns <- NS(id)
  tags$div(id=ns("filterbox"),
           selectInput(inputId = ns("filter"),
                       label = "Choose a filter",
                       choices = "user"),
           tags$div(id=ns("filterend")),
           tags$hr(),
           textOutput(ns("observations")))
}