#' modFilter Module
#'
#' Shiny module which works together with \code{\link{modFilterUI}} to filter a data set based on user input
#'
#' @param input,output,session Default input, output and session objects coming from shiny
#' @param data A reactive returning a data.table with observations in rows and filter options in columns
#' @param exclude names of columns that should be not used as filter
#' @return a reactive containing the filtered data.table
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{modFilterUI}}, \code{\link{appModelstats}}
#' @importFrom shiny updateSliderInput
#' @importFrom data.table uniqueN
#' @export

modFilter <- function(input, output, session, data, exclude=NULL, showAll=FALSE, multiple=NULL) {
  
  x <- reactiveValues()
  
  selectdata <- function(data,input,filter){
    if(is.null(data)) return(data.frame())
    choices <- list()
    fvec <- rep(TRUE,dim(data)[1])
    for(f in filter) {
      slf <- paste0("slider",f)
      if(!is.null(input[[slf]])) {
        slmax <- max(data[[f]][fvec], na.rm=TRUE)
        slmin <- min(data[[f]][fvec], na.rm=TRUE)
        if(x[[slf]]["max"]!=slmax | x[[slf]]["min"]!=slmin) {
          updateSliderInput(session, slf,   min = slmin, max = slmax,
                          value = input[[slf]])
          x[[slf]]["max"] <- slmax
          x[[slf]]["min"] <- slmin
        }
        
        fvec <- (fvec & (data[[f]] >= input[[slf]][1]) & (data[[f]] <= input[[slf]][2]))
      } else {
        sf <- paste0("select",f)
        if(!is.null(input[[sf]])) {
          slchoices <- sort(data[[f]][fvec])
          if(!setequal(slchoices,x[[sf]])) {
            updateSelectInput(session, sf,  choices=slchoices, selected=input[[sf]])
            x[[sf]] <- slchoices
          }
          fvec <- (fvec & (data[[f]] %in% input[[sf]]))
        }
      }
    }
    fvec[is.na(fvec)] <- FALSE
    return(data[fvec,])
  }
  
  selectUI <- function(session,filter, data, class, multiple) {
    if(filter=="") return(NULL)
    if(is.na(class)) class <- "NA"
    if(class=="POSIXct") {
      min <- min(data, na.rm=TRUE)
      max <- max(data, na.rm=TRUE)
      id <- paste0("slider", filter)
      x[[id]] <- c(min=min,max=max)
      return(tags$div(id=session$ns(paste0("div",filter)),
                      sliderInput(inputId = session$ns(id),
                                  label = filter,
                                  min = min,
                                  max = max,
                                  value = c(min(data, na.rm=TRUE),max(data, na.rm=TRUE)),
                                  ticks = FALSE,
                                  step=60,
                                  timezone="CET",
                                  timeFormat = "%F %H:%M")))
    } else if(class %in% c("integer","numeric")) {
      min <- floor(min(data, na.rm=TRUE))
      max <- ceiling(max(data, na.rm=TRUE))
      id <- paste0("slider", filter)
      x[[id]] <- c(min=min,max=max)
      return(tags$div(id=session$ns(paste0("div",filter)),
                      sliderInput(inputId = session$ns(id),
                                  label = filter,
                                  min = min,
                                  max = max,
                                  value = c(min(data, na.rm=TRUE),max(data, na.rm=TRUE)),
                                  ticks = FALSE)))        
    } else {
      choices <- sort(unique(data))
      id <- paste0("select", filter)
      x[[id]] <- choices
      return(tags$div(id=session$ns(paste0("div",filter)),
                      selectInput(inputId = session$ns(id),
                                  label = filter,
                                  choices = choices,
                                  multiple = multiple, "display:inline")))
    }
  }
  
  observeEvent(data(),{
    print(str(data()))
    x$data <- data()
    nelem <- apply(x$data,2,uniqueN)
    x$filter <- names(x$data)[!(names(x$data)%in%exclude) & nelem>1]
    x$filterclass <- sapply(x$data,function(x)return(class(x)[1]))
    x$filtermultiple <- multiple
    x$filtermultiple[x$filter[!(x$filter %in% names(multiple))]] <- TRUE
    for(f in x$activefilter) {
      removeUI(
        selector = paste0("#",session$ns(paste0("div",escapeRegex(f))))
      )
    }
    x$activefilter <- NULL
    if(showAll) {
      removeUI(selector = paste0("#",session$ns("filterselector")))
      for(xf in x$filter){
        insertUI(
          selector = paste0("#",session$ns("filterend")),
          where = "beforeBegin",
          ui = selectUI(session,xf, x$data[[xf]], x$filterclass[xf], x$filtermultiple[xf])
        )
      }
      x$activefilter <- x$filter
    } else {
      updateSelectInput(session, "filter", choices=x$filter)
      if(input$filter==x$filter[1]) {
        insertUI(
        selector = paste0("#",session$ns("filterend")),
        where = "beforeBegin",
        ui = selectUI(session,input$filter, x$data[[input$filter]], x$filterclass[input$filter], x$filtermultiple[input$filter])
      )
      x$activefilter <- c(x$activefilter,input$filter)
      }
    }
  })
  
  observeEvent(input$filter, {
    if(!(input$filter %in% x$activefilter)){
      insertUI(
        selector = paste0("#",session$ns("filterend")),
        where = "beforeBegin",
        ui = selectUI(session,input$filter, x$data[[input$filter]], x$filterclass[input$filter], x$filtermultiple[input$filter])
      )
      x$activefilter <- c(x$activefilter,input$filter)
    }
    for(f in setdiff(x$activefilter, input$filter)) {
      if(!is.null(input[[paste0("slider",f)]])) {
        id <- paste0("slider",f)
        removeUI <- ((input[[id]][1] <= x[[id]]["min"]) &
                       (input[[id]][2] >= x[[id]]["max"]))
      } else {
        removeUI <- ifelse(is.null(input[[paste0("select",f)]]), TRUE, FALSE)
      }
      if(removeUI) {
        removeUI(
          selector = paste0("#",session$ns(paste0("div",escapeRegex(f))))
        )
        x$activefilter <- setdiff(x$activefilter,f)
      }
    }
  })
  
  out <- reactive(selectdata(data(),input,x$activefilter))
  output$observations <- renderText(paste0(dim(out())[1]," observations"))
  
  return(out)
}