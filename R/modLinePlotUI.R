#' modLinePlot Module
#'
#' Shiny module which works together with \code{\link{modLinePlot}} to produce a line plot tab
#'
#' @param id id of the filter
#' @author Florian Humpenoeder, Jan Philipp Dietrich
#' @seealso \code{\link{modLinePlot}}, \code{\link{appMAgPIE}}
#' @export

modLinePlotUI <- function(id) {
  ns <- NS(id)
  tags$div(id=ns("filterbox"),
    modFilterUI(ns("runfilter")),
    tags$hr(),
    fluidRow(
      column(6,checkboxInput(ns('show_val'), 'Validation Data', value = TRUE)),
      column(6,checkboxInput(ns('free_y'), 'Free Y', value = FALSE))
    ),
    downloadButton(ns('downloadLinePlot'), 'Download Plot'),
    downloadButton(ns('downloadPlotObject'), 'Download Plot Object')
  )
}