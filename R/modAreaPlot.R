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
    if(is.null(report)) return(NULL)
    message("AddGroup in modAreaPlot..")
    tmp <- extractVariableGroups(levels(report$variable))
    message("  ..finished AddGroup in modAreaPlot (",round(as.numeric(Sys.time()-start,units="secs"),4),"s)")
    return(merge(report,tmp))
  }
  
  selection <- callModule(modFilter, "runfilter",
                          data         = reactive(addGroup(report()$report)), 
                          exclude      = c("variable","value","unit"), 
                          showAll      = TRUE, 
                          multiple     = c(group=FALSE),
                          order        = c("group"))

  areaplot <- reactive({
    start <- Sys.time()
    message("Create areaplot in modAreaPlot..")
    if(report()$ready) {
      p <- mipArea(x = selection()$x) + mip::theme_mip(size=10)
    } else p <- NULL
    message("  ..finished areaplot in modAreaPlot (",round(as.numeric(Sys.time()-start,units="secs"),4),"s)")
    return(ggplotly(p))
  })
  
  output$downloadLinePlot <- downloadHandler(
    filename = "export.pdf",
    content = function(file) {
      ggsave(file, plot = areaplot(), device = "pdf",scale=1,width=20,height=18,units="cm",dpi=150)
    }
  )
  
  return(renderPlotly({
    areaplot()}))
  
}