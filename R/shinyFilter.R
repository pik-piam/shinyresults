#' shinyFilter Module
#'
#' Shiny module which works together with \code{\link{shinyFilterUI}} to filter a data set based on user input
#'
#' @param input,output,session Default input, output and session objects coming from shiny
#' @param data A data.table with observations in rows and filter options in columns
#' @param exclude names of columns that should be not used as filter
#' @return a reactive containing the filtered data.table
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{shinyFilterUI}}, \code{\link{modelstats}}
#' @importFrom data.table uniqueN
#' @export

shinyFilter <- function(input, output, session, data, exclude=NULL) {
  selectdata <- function(data,input,filter){
    if(is.null(data)) return(data.frame())
    choices <- list()
    fvec <- rep(TRUE,dim(data)[1])
    for(f in filter) {
      slf <- paste0("slider",f)
      if(!is.null(input[[slf]])) {
        #updateSliderInput(session,slf,   min = min(data[[f]][fvec], na.rm=TRUE),
        #                max = max(data[[f]][fvec], na.rm=TRUE),
        #                value = c(min(data[[f]][fvec], na.rm=TRUE),max(data[[f]][fvec], na.rm=TRUE)))
        
        fvec <- (fvec & (data[[f]] >= input[[slf]][1]) & (data[[f]] <= input[[slf]][2]))
      } else {
        sf <- paste0("select",f)
        if(!is.null(input[[sf]])) {
          #updateSelectInput(session, sf,  choices=data[[f]][fvec], selected=input[[sf]])
          fvec <- (fvec & (data[[f]] %in% input[[sf]]))
        }
      }
    }
    fvec[is.na(fvec)] <- FALSE
    return(data[fvec,])
  }
  
  selectUI <- function(session,filter, data, class) {
    if(is.na(class)) class <- "NA"
    if(class=="POSIXct") {
      return(tags$div(id=paste0("div",filter),
                      sliderInput(inputId = session$ns(paste0("slider", filter)),
                                  label = filter,
                                  min = min(data, na.rm=TRUE),
                                  max = max(data, na.rm=TRUE),
                                  value = c(min(data, na.rm=TRUE),max(data, na.rm=TRUE)),
                                  ticks = FALSE,
                                  step=60,
                                  timezone="CET",
                                  timeFormat = "%F %H:%M")))
    } else if(class=="numeric") {
      return(tags$div(id=paste0("div",filter),
                      sliderInput(inputId = session$ns(paste0("slider", filter)),
                                  label = filter,
                                  min = floor(min(data, na.rm=TRUE)),
                                  max = ceiling(max(data, na.rm=TRUE)),
                                  value = c(min(data, na.rm=TRUE),max(data, na.rm=TRUE)),
                                  ticks = FALSE)))        
    } else {
      return(tags$div(id=paste0("div",filter),
                      selectInput(inputId = session$ns(paste0("select", filter)),
                                  label = filter,
                                  choices = unique(data),
                                  multiple = TRUE, "display:inline")))
    }
  }
  
  
  x <- reactiveValues()
  
  observeEvent(data,{
    nelem <- apply(data,2,uniqueN)
    x$filter <- names(data)[!(names(data)%in%exclude) & nelem>1]
    x$filterclass <- sapply(data,function(x)return(class(x)[1]))
    for(f in x$activefilter) {
      removeUI(
        selector = escapeRegex(paste0("#div",f))
      )
    }
    x$activefilter <- NULL
    updateSelectInput(session, "filter", choices=x$filter, selected = "user")
  })
  
  observeEvent(input$filter, {
    if(!(input$filter %in% x$activefilter)){
      insertUI(
        selector = paste0("#",session$ns("filterend")),
        where = "beforeBegin",
        ui = selectUI(session,input$filter, data[[input$filter]], x$filterclass[input$filter])
      )
      x$activefilter <- c(x$activefilter,input$filter)
    }
    for(f in setdiff(x$activefilter, input$filter)) {
      if(!is.null(input[[paste0("slider",f)]])) {
        removeUI <- ((input[[paste0("slider",f)]][1] <= min(x$data[[f]], na.rm=TRUE)) &
                       (input[[paste0("slider",f)]][2] >= max(x$data[[f]], na.rm=TRUE)))
      } else {
        removeUI <- ifelse(is.null(input[[paste0("select",f)]]), TRUE, FALSE)
      }
      if(removeUI) {
        removeUI(
          selector = escapeRegex(paste0("#div",f))
        )
        x$activefilter <- setdiff(x$activefilter,f)
      }
    }
  })
  
  out <- reactive(selectdata(data,input,x$activefilter))
  output$observations <- renderText(paste0(dim(out())[1]," observations"))
  
  return(out)
}