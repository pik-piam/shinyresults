#' modAreaPlot Module
#'
#' Shiny module which works together with \code{\link{modAreaPlotUI}} to produce an area plot tab
#'
#' @param input,output,session Default input, output and session objects coming from shiny
#' @param report A reactive containing the report to be visualized
#' @author Jan Philipp Dietrich, Florian Humpenoeder
#' @seealso \code{\link{modAreaPlotUI}}, \code{\link{appMAgPIE}}
#' @export

modAreaPlot <- function(input, output, session, report) {
  
  addGroup <- function(report) {
    start <- Sys.time()
    #message("AddGroup in modAreaPlot..")
    #if(is.null(report)) return(NULL)
    #tmp <- extractVariableGroups(report$variable)
    #message("  ..finished AddGroup in modAreaPlot (",round(as.numeric(Sys.time()-start,units="secs"),4),"s)")
    return(report)
    return(merge(report,tmp))
  }
  
  selection <- callModule(modFilter, "runfilter",
                          data         = reactive(addGroup(report()$report)), 
                          exclude      = c("value","unit"), 
                          showAll      = TRUE, 
                          multiple     = c(variable=FALSE),
                          order        = c("variable"))

  areaplot <- reactive({
    start <- Sys.time()
    message("Create areaplot in modAreaPlot..")
    if(report()$ready) {
      p <- mipArea(x = selection()$x)
    } else p <- NULL
    message("  ..finished areaplot in modAreaPlot (",round(as.numeric(Sys.time()-start,units="secs"),4),"s)")
    return(p)
  })
  
  output$downloadLinePlot <- downloadHandler(
    filename = "export.pdf",
    content = function(file) {
      ggsave(file, plot = areaplot(), device = "pdf",scale=1,width=20,height=18,units="cm",dpi=150)
    }
  )
  
  return(renderPlot({
    areaplot()},res = 120))
  
}