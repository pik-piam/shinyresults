#' modRunSelect Module
#'
#' Corresponding server logic to \code{\link{modRunSelectUI}} to select modules runs for further analysis
#'
#' @param input,output,session Default input, output and session objects coming from shiny
#' @param file report data. Can be a CSV/MIF file or rds file with a quitte object (saved with saveRDS). file can also be a vector of rds files. NULL by default; in this case the user can upload files directly in the tool
#' @param resultsfolder folder in which MAgPIE run results are stored. File must come with a overview list called "files" 
#' @param username username to be used to access file and resultsfolder
#' @param password password to access file and resultsfolder
#' @return a reactive containing a merged data.frame containing results of selected runs
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{modFilterUI}}, \code{\link{appModelstats}}
#' @importFrom shiny updateSliderInput withProgress incProgress
#' @importFrom tools file_path_sans_ext
#' @importFrom data.table uniqueN
#' @importFrom curl curl new_handle
#' @export

modRunSelect <- function(input, output, session, file, resultsfolder, username=NULL, password=NULL) {
  
  readdata <- function(file,username=NULL, password=NULL) {
    if(grepl("https://",file)) {
      out <- readRDS(gzcon(curl(file, handle=new_handle(username=username, password=password))))
    } else {
      out <- readRDS(file)
    }
    if("date" %in% names(out)) out$date <- as.POSIXct(out$date, origin="1970-01-01")
    if("revision_date" %in% names(out)) out$revision_date <- as.POSIXct(out$revision_date, origin="1970-01-01")
    return(out)
  }
  
  readreports <- function(ids, resultsfolder, username=NULL, password=NULL) {
    files <- paste0(resultsfolder,ids,".rds")
    fout <- NULL
    withProgress(message = 'Read selected data', value = 0, {
    for(file in files) {
      fout <- rbind(fout,readdata(file, username=username, password=password))
      incProgress(1/length(files), detail = basename(file))
    }
    })
    if (length(levels(fout$scenario)) != length(unique(levels(fout$scenario)))) {
      suffix <- format(as.POSIXct(as.numeric(file_path_sans_ext(basename(files)))/100000, origin="1970-01-01"))
      levels(fout$scenario) <- paste(levels(fout$scenario),suffix)
    }
    return(fout)
  }
  
  readtextfile <- function(file, username=NULL, password=NULL) {
    if(grepl("https://",file)) {
      out <- readLines(gzcon(curl(file, handle=new_handle(username=username, password=password))))
    } else {
      out <- readLines(file)
    }
  }
  
  data <- readdata(file, username=username, password=password)
  ids <- as.numeric(sub("\\.rds$","",readtextfile(paste0(resultsfolder,"/files"), username=username, password=password)))
  data <- data[(data$.id %in% ids),]
  
  selection <- callModule(modFilter,"runfilter",data=reactive(data),exclude=".id")
  
  x <- reactiveValues(out=NULL, ready=FALSE)
  
  fullReport <- reactive(readreports(selection()$x[[".id"]], resultsfolder, username=username, password=password))
  
  observeEvent(input$load, {
    print("read selected data")
    x$out <- fullReport()
    x$ready <- TRUE
  })
  
  return(reactive(list(report=x$out,ready=x$ready,variables=names(selection()$x)[-1],selection=selection)))
}