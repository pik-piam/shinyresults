#' modRunSelect Module
#'
#' Corresponding server logic to \code{\link{modRunSelectUI}} to select modules runs for further analysis
#'
#' @param input,output,session Default input, output and session objects coming from shiny
#' @param file report data. Can be a CSV/MIF file or rds file with a quitte object (saved with saveRDS). file can also be a vector of rds files. NULL by default; in this case the user can upload files directly in the tool
#' @param resultsfolder folder in which MAgPIE run results are stored. File must come with a overview list called "files" 
#' @return a reactive containing a merged data.frame containing results of selected runs
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{modFilterUI}}, \code{\link{appModelstats}}
#' @importFrom shiny updateSliderInput withProgress incProgress
#' @importFrom tools file_path_sans_ext
#' @importFrom data.table uniqueN
#' @export

modRunSelect <- function(input, output, session, file, resultsfolder) {
  
  readdata <- function(file) {
    if(grepl("https://",file)) {
      out <- readRDS(gzcon(url(file)))
    } else {
      out <- readRDS(file)
    }
    if("date" %in% names(out)) out$date <- as.POSIXct(out$date, origin="1970-01-01")
    if("revision_date" %in% names(out)) out$revision_date <- as.POSIXct(out$revision_date, origin="1970-01-01")
    return(out)
  }
  
  readreports <- function(ids, resultsfolder) {
    files <- paste0(resultsfolder,ids,".rds")
    fout <- NULL
    withProgress(message = 'Read selected data', value = 0, {
    for(file in files) {
      fout <- rbind(fout,readdata(file))
      incProgress(1/length(files), detail = basename(file))
    }
    })
    if (length(levels(fout$scenario)) != length(unique(levels(fout$scenario)))) {
      suffix <- format(as.POSIXct(as.numeric(file_path_sans_ext(basename(files)))/100000, origin="1970-01-01"))
      levels(fout$scenario) <- paste(levels(fout$scenario),suffix)
    }
    return(fout)
  }
  
  dummy <- function() {
    model <- factor(c("Model1", "Model2", "Model3"))
    scenario <- factor(c("Scen1", "Scen2", "Scen3"))
    region <- factor(c("Region1", "Region2", "Region3"))
    period <- c(2005, 2050, 2100)
    variable <- factor(c("Variable1", "Variable2", "Variable3"))
    unit <- factor(c("Unit"))
    long <- expand.grid(model, scenario, region, variable, 
                        unit, period, 1, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = TRUE)
    names(long) <- c("model", "scenario", "region", 
                     "variable", "unit", "period", "value")
    long$value <- 1:length(long$value)
    return(long)
  }
  
  data <- readdata(file)
  ids <- as.numeric(sub("\\.rds$","",readLines(url(paste0(resultsfolder,"/files")))))
  data <- data[(data$.id %in% ids),]
  
  selection <- callModule(modFilter,"runfilter",data=reactive(data),exclude=".id")
  
  x <- reactiveValues(out=dummy(), ready=FALSE)
  
  fullReport <- reactive(readreports(selection()[[".id"]], resultsfolder))
  
  observeEvent(input$load, {
    print("read selected data")
    x$out <- fullReport()
    x$ready <- TRUE
  })
  
  return(reactive(list(report=x$out,ready=x$ready,variables=names(selection())[-1],selection=selection)))
}