#' appResultsLocal
#'
#' version of appResults which is optimized to run on a local model folder. In contrast to appResults, 
#' appResultsLocal only requires the path to an output folder (with subfolders for each run).
#'
#' @param folder output folder containing the runs to be analyzed as subfolders (e.g. folder "output" in a
#' MAgPIE model folder)
#' @param valfile Path to a validation file, preferably in rds format, but can also be provided as mif (in the 
#' latter case it will be converted to rds first). If not path is given the function will look automatically for 
#' an validation file in the output folder
#' @author Jan Philipp Dietrich, Lavinia Baumstark
#' @seealso \code{\link{appResults}}
#' @export

appResultsLocal <- function(folder="output/", valfile=NULL) {
  
  if (!requireNamespace("lucode2", quietly = TRUE)) {
    stop("Package \"lucode2\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  d <- lucode2::mergestatistics(dir=folder,pattern="runstatistics.rda")
  d$.id <- sub("runstatistics$","report",d$.id)
  file <- paste0(folder,"/summary.rds")
  saveRDS(d,file)
  
  # choose validation data automatically if no information is provided
  if(is.null(valfile)) {
    valfile <- paste0(folder,"/",sub("report","validation.rds",d$.id))
    if(any(file.exists(valfile))) {
      valfile <- valfile[file.exists(valfile)][1]
    } else {
      valfile <- sub(".rds$",".mif",valfile)
      if(any(file.exists(valfile))) {
        valfile <- valfile[file.exists(valfile)][1]
      } else {
        valfile <- sub("validation.mif","historical.mif",valfile)  # maybe the model is using historical.mif instead of validation.mif
      }
    }
  }
  
  # convert mif to rds
  if(grepl("\\.mif$",valfile)) {
    if (!requireNamespace("quitte", quietly = TRUE)) {
      warning("Package \"quitte\" needed to handle mif files. Validation file will be ignored!",
           call. = FALSE)
      valfile <- NULL
    } else {
      a <- quitte::read.quitte(valfile, check.duplicates=FALSE)
      a <- a[!is.na(a$value),]
      valfile <- sub(".mif$",".rds",valfile)
      saveRDS(a,valfile)
    }
  }
  
  # collect information on local settings
  cfg_local <- list("localModel"= list(file=file, 
                                       resultsfolder=folder, 
                                       valfile=valfile, 
                                       username="mine", 
                                       password="localModel"))
  
  appResults(cfg=cfg_local)
}