#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite
#' @import tibble

merge2database <- function(dbfile,files) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbfile)
  for(f in files) {
    runs <- readRDS(f)
    runs$id <- as.numeric(sub(".rds$","",basename(f)))
    class(runs) <-setdiff(class(runs),"quitte")
    DBI::dbWriteTable(con,name="runs",value=runs,append=TRUE)
  }
  return(con)
}