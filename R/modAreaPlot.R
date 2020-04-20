#' modAreaPlot Module
#'
#' Shiny module which works together with \code{\link{modAreaPlotUI}} to produce an area plot tab
#'
#' @param input,output,session Default input, output and session objects coming from shiny
#' @param report A reactive containing the report to be visualized
#' @author Jan Philipp Dietrich, Florian Humpenoeder
#' @seealso \code{\link{modAreaPlotUI}}, \code{\link{appResults}}
#' @importFrom data.table as.data.table merge.data.table
#' @export

modAreaPlot <- function(input, output, session, report) {
  
  addGroup <- function(report) {
    start <- Sys.time()
    if(is.null(report)) return(NULL)
    message(".:|",sub("-$","",session$ns('')),"|:.  add groups..", appendLF = FALSE)
    tmp <- extractVariableGroups(levels(report$variable))
    out <- merge(as.data.table(report),as.data.table(tmp))
    message("done! (",round(as.numeric(Sys.time()-start,units="secs"),2),"s)")
    return(out)
  }
  
  selection <- callModule(modFilter, "runfilter",
                          data         = reactive(addGroup(report$report())), 
                          exclude      = c("variable","value","unit"), 
                          showAll      = TRUE, 
                          multiple     = c(group=FALSE),
                          order        = c("group"),
                          name         = sub("-$","",session$ns('')))

  areaplot <- reactive({
    start <- Sys.time()
    message(".:|",sub("-$","",session$ns('')),"|:.  Create area plot..", appendLF = FALSE)
    if(report$ready()) {
      if(nrow(selection()$x)>5000) {
        p <- ggplot() +  
          annotate("text", x=1, y=1, label= "Too many data points (>5000)! Please filter data!") + 
          theme_void()  
      } else {
        p <- mipArea(x = selection()$x) + mip::theme_mip(size=10)
      }
    } else p <- NULL
    message("done! (",round(as.numeric(Sys.time()-start,units="secs"),2),"s)")
    return(p)
  })
  
  createFilename <- function(variable,ending) {
    tmp <- shorten_legend(variable, identical_only = TRUE)
    out <- sub("_+$","",gsub("\\.+","_",make.names(attr(tmp, "front"))))
    out <- paste0(out,".",ending)
    return(out)
  }
  
  output$downloadPlotPDF <- downloadHandler(
    filename = reactive(createFilename(selection()$x$variable,"pdf")),
    content = function(file) {
      ggsave(file, plot = areaplot(), device = "pdf",scale=1,width=20,height=18,units="cm",dpi=150)
    }
  )
  
  output$downloadPlotPNG <- downloadHandler(
    filename = reactive(createFilename(selection()$x$variable,"png")),
    content = function(file) {
      ggsave(file, plot = areaplot(), device = "png",scale=1,width=20,height=18,units="cm",dpi=150)
    }
  )
  
  output$downloadPlotEPS <- downloadHandler(
    filename = reactive(createFilename(selection()$x$variable,"eps")),
    content = function(file) {
      ggsave(file, plot = areaplot(), device = "eps",scale=1,width=20,height=18,units="cm",dpi=150)
    }
  )
  
  output$downloadPlotRDS <- downloadHandler(
    filename = reactive(createFilename(selection()$x$variable,"rds")),
    content = function(file) {
      saveRDS(areaplot(),file=file)
    }
  )
  
  return(renderPlotly({
    ggplotly(areaplot())}))
  
}