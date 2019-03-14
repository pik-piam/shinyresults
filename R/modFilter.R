#' modFilter Module
#'
#' Shiny module which works together with \code{\link{modFilterUI}} to filter a data set based on user input
#'
#' @param input,output,session Default input, output and session objects coming from shiny
#' @param data A reactive returning a data.table with observations in rows and filter options in columns
#' @param exclude names of columns that should be not used as filter
#' @param showAll FALSE | If set to TRUE all available filter are shown and the filter selector is hidden
#' @param multiple vector with booleans for each filter defining whether multiple selections are allowed
#' or not. If information is not provided it is assumed that multiple selection is allowed
#' @param xdata additional data.tables which should be filtered by the same rules as data. If provided
#' the format of the return value changes
#' @param xdataExclude similar to exclude a vector of filters that should be ignored for xdata. Useful if xdata should
#' only filtered for a subset of filters applied to data
#' @param order order the filter should be listed (provided as a vector of filter names). Filter not listed here will be shown after the ones mentioned.
#' @param name name used to identify the filter in the log 
#' @return  a reactive list with x as the filtered data and xdata containing the list of additional, filtered data element.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{modFilterUI}}, \code{\link{appModelstats}}
#' @importFrom shiny updateSliderInput debounce
#' @importFrom data.table uniqueN
#' @export

modFilter <- function(input, output, session, data, exclude=NULL, showAll=FALSE, multiple=NULL, xdata=NULL, xdataExclude=NULL, order=NULL, name=NULL) {
  
  if(!is.null(name)) name <- paste0(".:|",name,"|:. ")
  
  x <- reactiveValues()
  x$initialized <- FALSE
  
  selectdata <- function(data,input,filter,xdata,xdataExclude){
    start <- Sys.time()
    if(is.null(data)) return(data.frame())
    message("Run selectdata in modFilter ",name,"..")
    data <- as.data.table(data)
    for(i in names(xdata)) xdata[[i]] <- as.data.table(xdata[[i]])
    for(f in filter) {
      slf <- paste0("slider",f)
      if(!is.null(input[[slf]])) {
        slmax <- max(data[[f]], na.rm=TRUE)
        slmin <- min(data[[f]], na.rm=TRUE)
        if(x[[slf]]["max"]!=slmax | x[[slf]]["min"]!=slmin) {
          updateSliderInput(session, slf,   min = slmin, max = slmax,
                            value = input[[slf]])
          x[[slf]]["max"] <- slmax
          x[[slf]]["min"] <- slmin
        }
        
        tmp <- function(data,f,min,max) {
          if(is.factor(data[[f]])) {
            tmp <- as.numeric(levels(data[[f]]))[data[[f]]]
          } else {
            tmp <- data[[f]]
          }
          fvec <- ((tmp >= min) & (tmp <= max))
          fvec[is.na(fvec)] <- FALSE
          return(data[fvec,])
        }
        data <- tmp(data,f,input[[slf]][1],input[[slf]][2])
        if(!(f %in% xdataExclude)) {
          for(n in names(xdata)) {
            xdata[[n]] <- tmp(xdata[[n]],f,input[[slf]][1],input[[slf]][2])
          }
        }
      } else {
        sf <- paste0("select",f)
        if(!is.null(input[[sf]])) {
          slchoices <- sort(data[[f]])
          if(!setequal(slchoices,x[[sf]])) {
            updateSelectInput(session, sf,  choices=slchoices, selected=input[[sf]])
            x[[sf]] <- slchoices
          }
          tmp2 <- function(data,f,selection) {
            fvec <- (data[[f]] %in% selection)
            fvec[is.na(fvec)] <- FALSE
            return(data[fvec,])
          }
          data <- tmp2(data,f,input[[sf]])
          if(!(f %in% xdataExclude)) {
            for(n in names(xdata)) {
              xdata[[n]] <- tmp2(xdata[[n]],f,input[[sf]])
            }
          }
        }
      }
    }
    out <- list(x=data)
    if(!is.null(xdata)) out$xdata <- xdata
  
    message("  ..finished selectdata in modFilter ",name," (",round(as.numeric(Sys.time()-start,units="secs"),4),"s)")
    return(out)
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
                                  ticks = FALSE,
                                  sep="")))        
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


  initialize <- function(input,session,data,x,exclude,order,multiple) {
    if(!is.null(data())) {
      start <- Sys.time()
      message("Initialize modFilter ",name,"..")
      x$data <- data()
      nelem <- apply(x$data,2,uniqueN)
      x$filter <- names(x$data)[!(names(x$data)%in%exclude) & nelem>=1]
      if(!is.null(order)) x$filter <- c(intersect(order,x$filter),setdiff(x$filter,order))
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
      x$initialized <- TRUE
      message("  ..finished modFilter initialization ",name,"(",round(as.numeric(Sys.time()-start,units="secs"),4),"s)")
    }  
  }
  
  
  observeEvent(data(),{
    initialize(input,session,data,x,exclude,order,multiple)
  })
  
  # observeEvent(input$fixme,{
  #   print("HUHU")
  #   for(xf in x$filter){
  #     print(xf)
  #     removeUI(
  #       selector = paste0("#",session$ns(paste0("div",escapeRegex(xf))))
  #     )
  #     insertUI(
  #       selector = paste0("#",session$ns("filterend")),
  #       where = "beforeBegin",
  #       ui = selectUI(session,xf, x$data[[xf]], x$filterclass[xf], x$filtermultiple[xf])
  #     )
  #   }
  # })
  
  observe({
    x$out <- selectdata(data(),input,x$activefilter,xdata,xdataExclude)
  })
  
  observeEvent(input$filter, {
    if(!(input$filter %in% x$activefilter)){
      insertUI(
        selector = paste0("#",session$ns("filterend")),
        where = "beforeBegin",
        ui = selectUI(session,input$filter, x$out$x[[input$filter]], x$filterclass[input$filter], x$filtermultiple[input$filter])
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
  
  output$observations <- renderText(paste0(dim(x$out$x)[1]," observations"))
  
  return(debounce(reactive(x$out),500))
}