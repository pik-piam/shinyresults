#' modRunSelect Module
#'
#' Corresponding server logic to \code{\link{modRunSelectUI}} to select modules runs for further analysis
#'
#' @param input,output,session Default input, output and session objects coming from shiny
#' @param file report data. Can be a CSV/MIF file or rds file with a quitte object (saved with saveRDS). file can also be a vector of rds files. NULL by default; in this case the user can upload files directly in the tool
#' @param resultsfolder folder in which MAgPIE run results are stored. File must come with a overview list called "files" 
#' @param username username to be used to access file and resultsfolder
#' @param password password to access file and resultsfolder
#' @param readFilePar read report data files in parallel (faster) (TRUE) or in sequence (FALSE). Current default is FALSE.
#' @return a reactive containing a merged data.frame containing results of selected runs
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{modFilterUI}}, \code{\link{appModelstats}}
#' @importFrom shiny updateSliderInput withProgress incProgress
#' @importFrom tools file_path_sans_ext
#' @importFrom data.table uniqueN
#' @importFrom curl curl new_handle
#' @importFrom parallel detectCores
#' @importFrom snow makeCluster stopCluster
#' @importFrom doSNOW registerDoSNOW
#' @importFrom foreach foreach %dopar%
#' @export

modRunSelect <- function(input, output, session, file, resultsfolder, username=NULL, password=NULL,readFilePar=FALSE) {
  
  readdata <- function(file,username=NULL, password=NULL,addfilename=FALSE) {
    if(grepl("https://",file)) {
      out <- readRDS(gzcon(curl(file, handle=new_handle(username=username, password=password))))
    } else {
      out <- readRDS(file)
    }
    if(addfilename) out$filename <- as.factor(file)
    if("date" %in% names(out)) out$date <- as.POSIXct(out$date, origin="1970-01-01")
    if("revision_date" %in% names(out)) out$revision_date <- as.POSIXct(out$revision_date, origin="1970-01-01")
    return(out)
  }
  
  readreports <- function(ids, resultsfolder, username=NULL, password=NULL) {
    files <- paste0(resultsfolder,"/",ids,".rds")
    withProgress(message = 'Read selected data', value = 0, {
      if(readFilePar) {
        no_cores <- detectCores() - 1
        cl <- makeCluster(no_cores)
        registerDoSNOW(cl)
        fout <- foreach (file=files,.combine = rbind,.export = c("readdata"),.options.snow = list(progress = incProgress(1/length(files), detail = basename(file)))) %dopar% {
          readdata(file, username=username, password=password, addfilename = TRUE)
        }
      } else {
        fout <- NULL  
        for(file in files) {
          fout <- rbind(fout,readdata(file, username=username, password=password, addfilename = TRUE))
          incProgress(1/length(files), detail = basename(file))
        }
      }
    })
    if (nlevels(fout$scenario) != nlevels(fout$filename)) {
      levels(fout$filename) <- format(as.POSIXct(as.numeric(file_path_sans_ext(basename(levels(fout$filename))))/100000, origin="1970-01-01"))
      fout$scenario <- as.factor(paste(fout$scenario,fout$filename))
      short <- sub(" .*$","",levels(fout$scenario))
      unique <- (!duplicated(short) & !duplicated(short, fromLast = TRUE))
      levels(fout$scenario)[unique] <- short[unique]
    }
    fout$filename <- NULL
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
  if(grepl("https://",file)) {
    ids <- as.numeric(sub("\\.rds$","",readtextfile(paste0(resultsfolder,"/files"), username=username, password=password)))
    data <- data[(data$.id %in% ids),]
  } else {
   data <- data[file.exists(paste0(resultsfolder,"/",data$.id,".rds")),]
  }
  
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