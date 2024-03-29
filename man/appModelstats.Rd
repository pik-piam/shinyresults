% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/appModelstats.R
\name{appModelstats}
\alias{appModelstats}
\title{Analyze Model Statistics}
\usage{
appModelstats(
  files = c("https://www.pik-potsdam.de/rd3mod/magpie.rds",
    "https://www.pik-potsdam.de/rd3mod/remind.rds"),
  resultsfolder = NULL
)
}
\arguments{
\item{files}{path to rds-files from which statistics should be read}

\item{resultsfolder}{path to a folder containing model results of the corresponding runs}
}
\description{
Shiny app to analyze statistics collected with \code{\link[lucode2]{runstatistics}}
and merged with \code{\link[lucode2]{mergestatistics}}
}
\author{
Jan Philipp Dietrich
}
