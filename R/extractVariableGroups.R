#' extractVariableGroups
#' 
#' Groups variable names by groups based on the |+| separators given in the variable names
#' 
#' @param x a vector of variable names
#' @return a data frame with variables and corresponding groups as columns. 
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{plotstyle.add}}
#' @importFrom data.table data.table
#' @examples
#' x <- c("a|+|1|+|aa","a|+|2|abc","a|+|1|+|bb","a|+|1|+|cc","a|+|3|+|aa","a|+|3|+|bb","b|2")
#' shinyresults:::extractVariableGroups(x)

extractVariableGroups <- function(x) {
  x <- grep("+",x,value=TRUE)
  tmp <- function(x,sep="|+|",ext="") {
    y <- strsplit(x,sep, fixed=TRUE)
    out <- NULL
    for(j in 1:length(y)) {
      for(i in 1:(length(y[[j]])-1)) {
        name <- paste0(paste(y[[j]][1:i],collapse=sep),ext)
        tmp <- x[j]
        names(tmp) <- name
        out <- c(out,tmp)
      }
    }
    return(out)
  }
  out <- NULL
  for(i in 1:10) {
    sep <- paste0("|",paste(rep("+",i),collapse=""),"|")
    matches <- grep(sep,x,fixed=TRUE, value = TRUE)
    if(length(matches)==0) break()
    ext <- ifelse(i>1,paste0(" ",i),"")
    out <- c(out,tmp(matches,sep=sep,ext=ext))
  }
  return(data.table(variable=out,group=names(out)))
}
