#' modLinePlot Module
#'
#' Shiny module which works together with \code{\link{modLinePlotUI}} to produce a line plot tab
#'
#' @param input,output,session Default input, output and session objects coming from shiny
#' @param report A reactive containing the report to be visualized
#' @param validation A reactive containing validation data to be shown
#' @author Florian Humpenoeder, Jan Philipp Dietrich
#' @seealso \code{\link{modLinePlotUI}}, \code{\link{appMAgPIE}}
#' @export

modLinePlot <- function(input, output, session, report, validation) {
  
  selection <- callModule(modFilter, 
                          "runfilter",
                          data=reactive(report()$report), 
                          exclude=c("value","unit"), 
                          showAll=TRUE, 
                          multiple=c(variable=FALSE),
                          xdata=list(validation=validation()),
                          xdataExclude=c("scenario","period"))
  
    # if(!is.null(validation()) & input$show_val) {
    #   print("subset validation data")
    #   val$val_sel <- subset(validation(),region %in% input$region)
    #   val$val_sel <- subset(val$val_sel,variable %in% input$variable)
    #   val$val_sel <- droplevels(val$val_sel)
    #   if(nrow(val$val_sel) == 0) val$val_sel <- NULL
    # } else val$val_sel <- NULL

  lineplot <- reactive({
    if(report()$ready) {
      p <- mipLineHistorical(x=selection()$x,
                             x_hist=selection()$xdata$validation,
                             size = 10,
                             ylab = selection()$x$unit,
                             title = selection()$x$variable,
                             scales = ifelse(input$free_y,"free_y","fixed"))
    } else p <- NULL
    return(p)
  })
  
  output$downloadLinePlot <- downloadHandler(
    filename = function() { paste("export", '.pdf', sep='') },
    content = function(file) {
      ggsave(file, plot = lineplot(), device = "pdf",scale=1,width=20,height=18,units="cm",dpi=150)
    }
  )
  
  return(renderPlot({
    lineplot()},res = 120))
  
}