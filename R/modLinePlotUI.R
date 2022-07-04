#' modLinePlot Module
#'
#' Shiny module which works together with \code{\link{modLinePlot}} to produce a line plot tab
#'
#' @param id id of the filter
#' @author Florian Humpenoeder, Jan Philipp Dietrich
#' @seealso \code{\link{modLinePlot}}, \code{\link{appResults}}
#' @export

modLinePlotUI <- function(id) {
  ns <- NS(id)
  tags$div(id=ns("filterbox"),
    modFilterUI(ns("runfilter")),
    tags$hr(),
    fluidRow(
      column(6,checkboxInput(ns('show_hist'), 'History',     value = TRUE)),
      column(6,checkboxInput(ns('show_proj'), 'Projections', value = TRUE)),
      column(6,checkboxInput(ns('free_y'),     'Free Y (Multi-Panel)',     value = FALSE)),
      column(6,checkboxInput(ns('auto_y'),     'Automatic Y scaling',     value = FALSE))
    ),
    downloadButton(ns('downloadPlotPNG'), 'PNG'),
    downloadButton(ns('downloadPlotPDF'), 'PDF'),
    downloadButton(ns('downloadPlotEPS'), 'EPS'),
    downloadButton(ns('downloadPlotRDS'), 'RDS')
  )
}
