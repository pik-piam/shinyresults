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
    selectInput(ns('scenario'), 'Scenario', "Pending upload",multiple = TRUE),
    selectInput(ns('region'), 'Region', "Pending upload",multiple = TRUE),
    sliderInput(ns('year'), 'Year',min=2000,max=2100,value=c(2000,2100),step=10),
    selectInput(ns('variable'), 'Variable', "Pending upload",multiple = FALSE),
    tags$hr(),
    fluidRow(
      column(6,checkboxInput(ns('normalize'), 'Normalize', value = FALSE, width = NULL)),
      column(6,conditionalPanel(condition = "input.valfile != NULL", checkboxInput(ns('show_val'), 'Show Validation', value = TRUE, width = NULL)))
    ),
    selectInput(ns('scales'), 'Scales',c("fixed","free_y","free_x","free"),selected="fixed"),
    tags$hr(),
    fluidRow(
      column(6,checkboxInput(ns('update_plot'), 'Update Plot', value = TRUE, width = NULL)),
      column(6,downloadButton(ns('downloadLinePlot'), 'Download Plot'))
    )
  )
}