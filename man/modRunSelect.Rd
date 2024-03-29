% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modRunSelect.R
\name{modRunSelect}
\alias{modRunSelect}
\title{modRunSelect Module}
\usage{
modRunSelect(
  input,
  output,
  session,
  file,
  resultsfolder,
  username = NULL,
  password = NULL,
  readFilePar = FALSE
)
}
\arguments{
\item{input, output, session}{Default input, output and session objects coming from shiny}

\item{file}{report data. Can be a CSV/MIF file or rds file with a quitte object (saved with saveRDS).
file can also be a vector of rds files. NULL by default; in this case the user can upload files directly in the tool}

\item{resultsfolder}{folder in which MAgPIE run results are stored.
File must come with a overview list called "files"}

\item{username}{username to be used to access file and resultsfolder}

\item{password}{password to access file and resultsfolder}

\item{readFilePar}{read report data files in parallel (faster) (TRUE) or in sequence (FALSE)}
}
\value{
a reactive containing a merged data.frame containing results of selected runs
}
\description{
Corresponding server logic to \code{\link{modRunSelectUI}} to select modules runs for further analysis
}
\seealso{
\code{\link{modFilterUI}}, \code{\link{appModelstats}}
}
\author{
Jan Philipp Dietrich
}
