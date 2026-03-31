#' Set up plot download handlers
#'
#' Internal utility to create download handlers for PDF, PNG, EPS, and RDS
#' formats on a Shiny module output. Used by \code{\link{modLinePlot}} and
#' \code{\link{modAreaPlot}} to avoid code duplication.
#'
#' @param output Shiny output object from the module server
#' @param plotReactive A reactive expression returning the ggplot object
#' @param baseFilenameReactive A reactive expression returning the base filename (without extension)
#' @importFrom ggplot2 ggsave
#' @noRd
plotDownloadHandlers <- function(output, plotReactive, baseFilenameReactive) {

  makeGgsaveHandler <- function(format) {
    downloadHandler(
      filename = reactive(paste0(baseFilenameReactive(), ".", format)),
      content = function(file) {
        ggsave(file, plot = plotReactive(), device = format,
               scale = 1, width = 20, height = 18, units = "cm", dpi = 150)
      }
    )
  }

  output$downloadPlotPDF <- makeGgsaveHandler("pdf")
  output$downloadPlotPNG <- makeGgsaveHandler("png")
  output$downloadPlotEPS <- makeGgsaveHandler("eps")

  output$downloadPlotRDS <- downloadHandler(
    filename = reactive(paste0(baseFilenameReactive(), ".rds")),
    content = function(file) {
      saveRDS(plotReactive(), file = file)
    }
  )
}
