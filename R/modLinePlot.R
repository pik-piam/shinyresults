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
  
  val <- reactiveValues()
  
  #subsetting the data stepwise is faster than all at once
  observeEvent(c(input$scenario,input$region,input$year,input$variable,input$show_val),{
    print("full subset model data")
    scenario <- region <- period <- variable <- NULL
    val$rep_full <- report()$report
    val$rep_sel <- subset(val$rep_full,scenario %in% input$scenario)
    val$rep_sel <- subset(val$rep_sel,region %in% input$region)
    val$rep_sel <- subset(val$rep_sel,(period >= input$year[1]) & (period <= input$year[2])) 
    val$rep_sel <- subset(val$rep_sel,variable %in% input$variable)
    val$rep_sel <- droplevels(val$rep_sel)
    
    if(!is.null(validation()) & input$show_val) {
      print("subset validation data")
      val$val_sel <- subset(validation(),region %in% input$region)
      val$val_sel <- subset(val$val_sel,variable %in% input$variable)
      val$val_sel <- droplevels(val$val_sel)
      if(nrow(val$val_sel) == 0) val$val_sel <- NULL
    } else val$val_sel <- NULL
  })
  
  #normalize
  observeEvent(input$normalize,{
    if(input$normalize) {
      val$rep_sel_tmp <- val$rep_sel
      print("normalize data")
      years <- unique(val$rep_sel$period)
      base_year <- val$rep_sel$value[val$rep_sel$period==years[1]]
      val$rep_sel$value <- val$rep_sel$value/rep(base_year,length(years))
      if(!is.null(val$val_sel)) {
        val$val_sel_tmp <- val$val_sel
        val$val_sel$value <- val$val_sel$value/rep(base_year,length(unique(val$val_sel$period)))
      }
    } else {
      print("restore data")
      if(!is.null(val$rep_sel_tmp)) val$rep_sel <- val$rep_sel_tmp
      if(!is.null(val$val_sel_tmp)) val$val_sel <- val$val_sel_tmp
    }
  })
  
  observe({
    
    if(report()$ready)   {
      print("update choices data (LinePlot)")
      val$rep_full <- report()$report
      updateSelectInput(session, "model", choices = levels(val$rep_full$model),selected = levels(val$rep_full$model)[1])
      updateSelectInput(session, "scenario", choices = levels(val$rep_full$scenario),selected = levels(val$rep_full$scenario))
      updateSelectInput(session, "region", choices = levels(val$rep_full$region),selected = levels(val$rep_full$region))
      updateSliderInput(session, "year", min = min(val$rep_full$period), max = max(val$rep_full$period),value = c(min(val$rep_full$period),max(val$rep_full$period)))
      updateSelectInput(session, "variable", choices = levels(val$rep_full$variable),selected = levels(val$rep_full$variable)[1])
      
    }
  })
  
  lineplot <- reactive({
    if(input$update_plot & report()$ready & !is.null(val$rep_sel)) {
      p <- mipLineHistorical(x=val$rep_sel,x_hist=val$val_sel,size = 10,ylab = val$rep_sel$unit,title = val$rep_sel$variable,scales = input$scales)
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