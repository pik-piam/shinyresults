#' modAreaPlot Module
#'
#' Shiny module which works together with \code{\link{modAreaPlot}} to produce an area plot tab
#'
#' @param id id of the filter
#' @author Florian Humpenoeder, Jan Philipp Dietrich
#' @seealso \code{\link{modAreaPlot}}, \code{\link{appResults}}
#' @export

modAreaPlotUI <- function(id) {
  ns <- NS(id)
  tags$div(id=ns("filterbox"),
    modFilterUI(ns("runfilter")),
    tags$hr(),
    downloadButton(ns('downloadPlotPNG'), 'PNG'),
    downloadButton(ns('downloadPlotPDF'), 'PDF'),
    downloadButton(ns('downloadPlotEPS'), 'EPS'),
    downloadButton(ns('downloadPlotRDS'), 'RDS')
  )
}