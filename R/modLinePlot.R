#' modLinePlot Module
#'
#' Shiny module which works together with \code{\link{modLinePlotUI}} to produce a line plot tab
#'
#' @param input,output,session Default input, output and session objects coming from shiny
#' @param report A reactive containing the report to be visualized
#' @param validation A reactive containing validation data to be shown
#' @author Florian Humpenoeder, Jan Philipp Dietrich
#' @seealso \code{\link{modLinePlotUI}}, \code{\link{appResults}}
#' @importFrom ggplot2 ggplot theme_void annotate
#' @importFrom shiny renderCachedPlot
#' @export

modLinePlot <- function(input, output, session, report, validation) {
  
  reduceVariables <- function(x) {
    if(!is.null(levels(x$variable))) levels(x$variable) <- gsub("\\|\\++\\|","|",levels(x$variable))
    return(x)
  }
  
  selection <- callModule(modFilter, "runfilter",
                          data         = report$report,
                          exclude      = c("value","unit"), 
                          showAll      = TRUE, 
                          multiple     = c(variable=FALSE),
                          xdata        = list(validation=validation()),
                          xdataExclude = c("scenario","period"),
                          order        = c("variable"),
                          name         = sub("-$","",session$ns('')))
                        

  lineplot <- reactive({
    start <- Sys.time()
    message(".:|",sub("-$","",session$ns('')),"|:.  Create line plot..", appendLF = FALSE)
    if(report$ready()) {
      if(nrow(selection()$x)>20000) {
        p <- ggplot() +  
          annotate("text", x=1, y=1, label= "Too many data points (>20000)! Please filter data!") + 
          theme_void()  
      } else {
        history     <- NULL
        projections <- NULL
        
        if(input$show_hist) {
          bla <- selection()$xdata$validation
          history <- bla[bla$scenario == "historical",]
          
        } 
        if (input$show_proj) {
          blub <- selection()$xdata$validation
          projections <- blub[blub$scenario != "historical",]
        }
        
        validation <- rbind(history,projections)
        
        p <- suppressMessages(mipLineHistorical(x    = selection()$x,
                             x_hist = validation,
                             size   = 10,
                             ylab   = as.character(selection()$x$unit[1]),
                             title  = as.character(selection()$x$variable[1]),
                             scales = ifelse(input$free_y,"free_y","fixed"),
                             ylim = switch(input$auto_y + 1, 0, NULL)
                             ))
      }
    } else p <- NULL
    message("done! (",round(as.numeric(Sys.time()-start,units="secs"),2),"s)")
    return(p)
  })
  
  createFilename <- function(variable,ending) {
    out <- sub("_+$","",gsub("\\.+","_",make.names(variable[1])))
    out <- paste0(out,".",ending)
    return(out)
  }
  
  output$downloadPlotPDF <- downloadHandler(
    filename = reactive(createFilename(selection()$x$variable,"pdf")),
    content = function(file) {
      ggsave(file, plot = lineplot(), device = "pdf",scale=1,width=20,height=18,units="cm",dpi=150)
    }
  )
  output$downloadPlotPNG <- downloadHandler(
    filename = reactive(createFilename(selection()$x$variable,"png")),
    content = function(file) {
      ggsave(file, plot = lineplot(), device = "png",scale=1,width=20,height=18,units="cm",dpi=150)
    }
  )
  
  output$downloadPlotEPS <- downloadHandler(
    filename = reactive(createFilename(selection()$x$variable,"eps")),
    content = function(file) {
      ggsave(file, plot = lineplot(), device = "eps",scale=1,width=20,height=18,units="cm",dpi=150)
    }
  )
  
  output$downloadPlotRDS <- downloadHandler(
    filename = reactive(createFilename(selection()$x$variable,"rds")),
    content = function(file) {
      saveRDS(lineplot(),file=file)
    }
  )
  
  return(renderCachedPlot(lineplot(), res = 120,
                          cacheKeyExpr = { list(selection(), input$show_hist, input$show_proj, input$free_y, input$auto_y) }))
  
}