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
#' @importFrom shiny updateSliderInput withProgress incProgress Progress
#' @importFrom tools file_path_sans_ext
#' @importFrom data.table is.data.table rbindlist
#' @importFrom curl curl new_handle
#' @importFrom parallel detectCores
#' @importFrom snow makeCluster stopCluster
#' @importFrom doSNOW registerDoSNOW
#' @importFrom foreach foreach %dopar%
#' @importFrom mip shorten_legend
#' @export

modRunSelect <- function(input, output, session, file, resultsfolder, username=NULL, password=NULL,readFilePar=FALSE) {
  
  readdata <- function(file,username=NULL, password=NULL,addfilename=FALSE) {
    if(grepl("https://",file)) {
      con <- gzcon(curl(file, handle=new_handle(username=username, password=password)))
      out <- readRDS(con)
      close(con)
    } else {
      out <- readRDS(file)
    }
    if(addfilename) out$filename <- as.factor(file)
    
    conv_date <- function(x) {
      tmp <- try(as.POSIXct(x, origin="1970-01-01"), silent=TRUE)
      if("try-error" %in% class(tmp)) return(x) 
      else return(tmp)
    }
    
    if("date" %in% names(out)) out$date <- conv_date(out$date)
    if("revision_date" %in% names(out)) out$revision_date <- conv_date(out$revision_date)
    if(!is.data.table(out)) out <- as.data.table(out)
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
        fout <- list() 
        for(file in files) {
          incProgress(1/(length(files)+1), detail = basename(file))
          fout[[file]] <- readdata(file, username=username, password=password, addfilename = TRUE)
        }
        incProgress(1, detail = "merge data")
        fout <- rbindlist(fout)
        
      }
    })
    if (nlevels(fout$scenario) != nlevels(fout$filename)) {
      # test two ways to distinguish identical runs. Prefer option2 (read date from file name) but use option1 (reduce file name to distinct components) as fallback
      option1 <- mip::shorten_legend(gsub("/"," ",levels(fout$filename),fixed=TRUE),identical_only = TRUE)
      option2 <- suppressWarnings(format(as.POSIXct(as.numeric(file_path_sans_ext(basename(levels(fout$filename))))/100000, origin="1970-01-01")))
      option2[is.na(option2)] <- option1[is.na(option2)]
      levels(fout$filename) <- option2
      tmpsep <- " #TMPSEPARATOR# "
      fout$scenario <- as.factor(paste0(fout$scenario,tmpsep,fout$filename))
      short <- sub(paste0(tmpsep,".*$"),"",levels(fout$scenario))
      levels(fout$scenario) <- sub(tmpsep," ",levels(fout$scenario))
      unique <- (!duplicated(short) & !duplicated(short, fromLast = TRUE))
      levels(fout$scenario)[unique] <- short[unique]
    }
    fout$filename <- NULL
    return(fout)
  }
  
  readtextfile <- function(file, username=NULL, password=NULL) {
    if(grepl("https://",file)) {
      out <- readLines(curl(file, handle=new_handle(username=username, password=password)))
    } else {
      out <- readLines(file)
    }
  }
  progress <- Progress$new(session, min=1, max=10)
  on.exit(progress$close())
  progress$set(message = 'Read in run overview',
               detail = 'This may take a while...',
               value = 2)
  data <- readdata(file, username=username, password=password)
  progress$set(message = 'Check for corresponding model run outputs',
               detail = 'That should be quick...',
               value = 7)
  if(grepl("https://",file)) {
    ids <- as.numeric(sub("\\.rds$","",readtextfile(paste0(resultsfolder,"/files"), username=username, password=password)))
    data <- data[(data$.id %in% ids),]
  } else {
   data <- data[file.exists(paste0(resultsfolder,"/",data$.id,".rds")),]
  }
  if(!is.data.table(data)) data <- as.data.table(data)
  
  progress$set(message = 'Load filter module',
               detail = 'That should be quick...',
               value = 9)
  
  selection <- callModule(modFilter,"runfilter",data=reactive(data),exclude=".id",name="RunSelect")
  
  x <- reactiveValues(out=NULL, ready=FALSE)
  
  fullReport <- reactive(readreports(selection()$x[[".id"]], resultsfolder, username=username, password=password))
  
  observeEvent(input$load, {
    start <- Sys.time()
    message(".:|RunSelect|:.  Read data..", appendLF = FALSE)
    x$out <- fullReport()
    x$ready <- TRUE
    message("done! (",round(as.numeric(Sys.time()-start,units="secs"),2),"s)")
  })
  out <- reactiveValues()
  out$report <- reactive(x$out)
  out$ready <- reactive(x$ready)
  out$variables <- reactive(names(selection()$x)[-1])
  out$selection <- selection
  progress$set(message = 'Run selection ready',
               detail = 'Move on to the next step...',
               value = 10)
  return(out)
}