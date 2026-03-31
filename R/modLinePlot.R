#' modLinePlot Module
#'
#' Shiny module which works together with \code{\link{modLinePlotUI}} to produce a line plot tab
#'
#' @param id Module ID string, must match the id used in \code{\link{modLinePlotUI}}
#' @param report A reactive containing the report to be visualized
#' @param validation A reactive containing validation data to be shown
#' @param selectionSets named list of selection sets per filter column (passed to \code{\link{modFilter}}).
#' Defaults to the \code{selectionSets} entry of the active \code{appResults} option.
#' @author Florian Humpenoeder, Jan Philipp Dietrich
#' @seealso \code{\link{modLinePlotUI}}, \code{\link{appResults}}
#' @importFrom ggplot2 ggplot theme_void annotate
#' @importFrom shiny moduleServer renderCachedPlot observeEvent updateSelectInput
#' @export

modLinePlot <- function(id, report, validation,
                       selectionSets = getOption("appResults")[[1]]$selectionSets) {
  moduleServer(id, function(input, output, session) {

    # Quick variable selection handler
    observeEvent(input$quick_select, {
      if (!is.null(input$quick_select) && input$quick_select != "") {
        updateSelectInput(session, "runfilter-selectvariable", selected = input$quick_select)
        updateSelectInput(session, "quick_select", selected = "")
      }
    }, ignoreInit = TRUE)

    reduceVariables <- function(x) {
      if(!is.null(levels(x$variable))) levels(x$variable) <- gsub("\\|\\++\\|","|",levels(x$variable))
      return(x)
    }

    selection <- modFilter("runfilter",
                            data         = report$report,
                            exclude      = c("value","unit"), 
                            showAll      = TRUE, 
                            multiple     = c(variable=FALSE),
                            xdata        = list(validation=validation()),
                            xdataExclude = c("scenario","period"),
                            order        = c("variable"),
                            name         = sub("-$","",session$ns('')),
                            selectionSets = selectionSets)

    lineplot <- reactive({
      start <- Sys.time()
      message(".:|",sub("-$","",session$ns('')),"|:.  Create line plot..", appendLF = FALSE)
      if(report$ready()) {
        plotData <- tryCatch(selection()$x, error = function(e) NULL)
        # Check for NULL or empty data
        if (is.null(plotData) || !is.data.frame(plotData) || nrow(plotData) == 0) {
          p <- ggplot() +
            annotate("text", x=1, y=1, label= "No data available. Please adjust filters.") +
            theme_void()
        } else if(nrow(plotData)>20000) {
          p <- ggplot() +
            annotate("text", x=1, y=1, label= "Too many data points (>20000)! Please filter data!") +
            theme_void()
        } else {
          # Validate required columns exist
          requiredCols <- c("period", "value", "scenario", "region")
          missingCols <- setdiff(requiredCols, names(plotData))
          if (length(missingCols) > 0) {
            p <- ggplot() +
              annotate("text", x=1, y=1, label= paste("Missing columns:", paste(missingCols, collapse=", "))) +
              theme_void()
          } else {
            # Remove rows with NA in critical columns
            plotData <- plotData[!is.na(plotData$value) & !is.na(plotData$period), ]
            if (nrow(plotData) == 0) {
              p <- ggplot() +
                annotate("text", x=1, y=1, label= "No valid data after removing NA values.") +
                theme_void()
            } else {
              history     <- NULL
              projections <- NULL

              if(input$show_hist) {
                bla <- tryCatch(selection()$xdata$validation, error = function(e) NULL)
                if (!is.null(bla) && is.data.frame(bla) && nrow(bla) > 0) {
                  history <- bla[bla$scenario == "historical",]
                  if (nrow(history) == 0) history <- NULL
                }
              }
              if (input$show_proj) {
                blub <- tryCatch(selection()$xdata$validation, error = function(e) NULL)
                if (!is.null(blub) && is.data.frame(blub) && nrow(blub) > 0) {
                  projections <- blub[blub$scenario != "historical",]
                  if (nrow(projections) == 0) projections <- NULL
                }
              }

              validation <- rbind(history, projections)
              if (!is.null(validation) && nrow(validation) == 0) validation <- NULL

              p <- tryCatch({
                # Suppress warnings from mip package (aes_string deprecation)
                suppressWarnings(suppressMessages(mipLineHistorical(
                  x = plotData,
                  x_hist = validation,
                  size   = 10,
                  ylab   = as.character(plotData$unit[1]),
                  title  = as.character(plotData$variable[1]),
                  legend.pos = ifelse(input$legend_right, "right", "bottom"),
                  scales = ifelse(input$free_y, "free_y","fixed"),
                  ylim = switch(input$auto_y + 1, 0, NULL)
                )))
              }, error = function(e) {
                message("Error in mipLineHistorical: ", e$message)
                ggplot() +
                  annotate("text", x=1, y=1, label= paste("Plot error:", e$message)) +
                  theme_void()
              })
            }
          }
        }
      } else p <- NULL
      message("done! (",round(as.numeric(Sys.time()-start,units="secs"),2),"s)")
      return(p)
    })

    baseFilename <- reactive({
      sub("_+$", "", gsub("\\.+", "_", make.names(selection()$x$variable[1])))
    })
    plotDownloadHandlers(output, lineplot, baseFilename)

    return(renderCachedPlot(lineplot(), res = 120,
                            cacheKeyExpr = { list(selection(), input$show_hist, input$show_proj, input$free_y, input$auto_y, input$legend_right) }))
  })
}
