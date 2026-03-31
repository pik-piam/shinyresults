#' modAreaPlot Module
#'
#' Shiny module which works together with \code{\link{modAreaPlotUI}} to produce an area plot tab
#'
#' @param input,output,session Default input, output and session objects coming from shiny
#' @param report A reactive containing the report to be visualized
#' @param selectionSets named list of selection sets per filter column (passed to \code{\link{modFilter}}).
#' Defaults to the \code{selectionSets} entry of the active \code{appResults} option.
#' @author Jan Philipp Dietrich, Florian Humpenoeder
#' @seealso \code{\link{modAreaPlotUI}}, \code{\link{appResults}}
#' @importFrom data.table as.data.table merge.data.table
#' @export

modAreaPlot <- function(input, output, session, report,
                        selectionSets = getOption("appResults")[[1]]$selectionSets) {

  addGroup <- function(report) {
    start <- Sys.time()
    if(is.null(report)) return(NULL)
    message(".:|",sub("-$","",session$ns('')),"|:.  add groups..", appendLF = FALSE)
    tmp <- extractVariableGroups(levels(report$variable))
    out <- merge(as.data.table(report),as.data.table(tmp))
    message("done! (",round(as.numeric(Sys.time()-start,units="secs"),2),"s)")
    return(out)
  }

  sanitizeAreaPlot <- function(plot) {
    # Remove duplicate legend entries in areaplot from multiple facets and the implicit conversion from ggplot to plotly object
    # Solution from https://stackoverflow.com/questions/69034684/how-to-eliminate-ggplotly-duplicate-legend-entries-when-using-more-than-one-geom
    gp <- ggplotly(p = plot)

    for (i in seq_along(gp$x$data)) {
      # Is the layer the first entry of the group?
      is_first <- grepl("^\\(.*?,1\\)", gp$x$data[[i]]$name)
      # Extract the group identifier and assign it to the name and legendgroup arguments
      gp$x$data[[i]]$name <- gsub("^\\((.*?),\\d+\\)", "\\1", gp$x$data[[i]]$name)
      gp$x$data[[i]]$legendgroup <- gp$x$data[[i]]$name
      # Show the legend only for the first layer of the group
      if (!is_first) gp$x$data[[i]]$showlegend <- FALSE
    }
    return(gp)
  }

  selection <- callModule(modFilter, "runfilter",
                          data         = reactive(addGroup(report$report())), 
                          exclude      = c("variable","value","unit"), 
                          showAll      = TRUE, 
                          multiple     = c(group=FALSE),
                          order        = c("group"),
                          name         = sub("-$","",session$ns('')),
                          selectionSets = selectionSets)

  areaplot <- reactive({
    start <- Sys.time()
    message(".:|",sub("-$","",session$ns('')),"|:.  Create area plot..", appendLF = FALSE)
    if(report$ready()) {
      if(nrow(selection()$x)>5000) {
        p <- ggplot() +
          annotate("text", x=1, y=1, label= "Too many data points (>5000)! Please filter data!") +
          theme_void()
      } else {
        p <- suppressMessages(mipArea(x = selection()$x,
                                      transpose = input$transpose_grid)
                              + mip::theme_mip(size=10))
      }
    } else p <- NULL
    message("done! (",round(as.numeric(Sys.time()-start,units="secs"),2),"s)")
    return(p)
  })

  baseFilename <- reactive({
    tmp <- shorten_legend(selection()$x$variable, identical_only = TRUE)
    sub("_+$", "", gsub("\\.+", "_", make.names(attr(tmp, "front"))))
  })
  plotDownloadHandlers(output, areaplot, baseFilename)

  return(renderPlotly({
    sanitizeAreaPlot(areaplot())}))

  # return(renderCachedPlot(areaplot(), res = 120,
  #                         cacheKeyExpr = { list(selection(), input$transpose_grid, input$plus_size) }))
}