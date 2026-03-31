#' modLinePlot Module
#'
#' Shiny module which works together with \code{\link{modLinePlot}} to produce a line plot tab
#'
#' @param id id of the filter
#' @param presets Optional list of presets for quick variable selection
#' @author Florian Humpenoeder, Jan Philipp Dietrich
#' @seealso \code{\link{modLinePlot}}, \code{\link{appResults}}
#' @keywords internal

modLinePlotUI <- function(id, presets = NULL) {
  ns <- NS(id)

  # Build quick select dropdown from presets
  quickSelectUI <- NULL
  if (!is.null(presets) && length(presets) > 0) {
    allVars <- unlist(lapply(presets, function(p) {
      sapply(p$variables, function(v) v$variable)
    }))
    if (length(allVars) > 0) {
      quickSelectUI <- tagList(
        selectInput(ns("quick_select"), NULL,
                    choices = c("Quick Select Variable..." = "", stats::setNames(allVars, allVars)),
                    selected = ""),
        tags$hr()
      )
    }
  }

  tags$div(id=ns("filterbox"),
    quickSelectUI,
    modFilterUI(ns("runfilter")),
    tags$hr(),
    fluidRow(
      column(6,checkboxInput(ns('show_hist'), 'History',     value = TRUE)),
      column(6,checkboxInput(ns('show_proj'), 'Projections', value = TRUE)),
      column(6,checkboxInput(ns('free_y'),     'Free Y (Multi-Panel)',     value = FALSE)),
      column(6,checkboxInput(ns('auto_y'),     'Automatic Y scaling',     value = FALSE)),
      column(6,checkboxInput(ns('legend_right'),'Legend on right side',     value = FALSE))#,
      #column(6,checkboxInput(ns('plus_size'), 'Increase vertical size', value = FALSE))
    ),
    downloadButton(ns('downloadPlotPNG'), 'PNG'),
    downloadButton(ns('downloadPlotPDF'), 'PDF'),
    downloadButton(ns('downloadPlotEPS'), 'EPS'),
    downloadButton(ns('downloadPlotRDS'), 'RDS')
  )
}