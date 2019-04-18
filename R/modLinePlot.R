#' modLinePlot Module
#'
#' Shiny module which works together with \code{\link{modLinePlotUI}} to produce a line plot tab
#'
#' @param input,output,session Default input, output and session objects coming from shiny
#' @param report A reactive containing the report to be visualized
#' @param validation A reactive containing validation data to be shown
#' @author Florian Humpenoeder, Jan Philipp Dietrich
#' @seealso \code{\link{modLinePlotUI}}, \code{\link{appResults}}
#' @export

modLinePlot <- function(input, output, session, report, validation) {
  
  reduceVariables <- function(x) {
    if(!is.null(levels(x$variable))) levels(x$variable) <- gsub("\\|\\++\\|","|",levels(x$variable))
    return(x)
  }
  
  selection <- callModule(modFilter, "runfilter",
                          #data         = reactive(reduceVariables(report$report())), 
                          data         = report$report,
                          exclude      = c("value","unit"), 
                          showAll      = TRUE, 
                          multiple     = c(variable=FALSE),
                          xdata        = list(validation=reduceVariables(validation())),
                          xdataExclude = c("scenario","period"),
                          order        = c("variable"),
                          name         = "LinePlot")
                        

  lineplot <- reactive({
    start <- Sys.time()
    message("Create lineplot in modLinePlot..")
    if(report$ready()) {
      if(input$show_val) {
        validation <- selection()$xdata$validation
      } else {
        validation <- NULL
      }
      p <- suppressMessages(mipLineHistorical(x    = selection()$x,
                           x_hist = validation,
                           size   = 10,
                           ylab   = selection()$x$unit,
                           title  = selection()$x$variable,
                           scales = ifelse(input$free_y,"free_y","fixed")))
    } else p <- NULL
    message("  ..finished lineplot in modLinePlot (",round(as.numeric(Sys.time()-start,units="secs"),4),"s)")
    return(p)
  })
  
  output$downloadLinePlot <- downloadHandler(
    filename = "export.pdf",
    content = function(file) {
      ggsave(file, plot = lineplot(), device = "pdf",scale=1,width=20,height=18,units="cm",dpi=150)
    }
  )
  
  output$downloadPlotObject <- downloadHandler(
    filename = "ggplot.rds",
    content = function(file) {
      saveRDS(lineplot(),file=file)
    }
  )
  
  return(renderPlot({
    lineplot()},res = 120))
  
}