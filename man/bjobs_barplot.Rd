\name{bjobs_barplot}
\alias{bjobs_barplot}
\title{
Barplot of number of jobs
}
\description{
Barplot of number of jobs
}
\usage{
bjobs_barplot(status = c("RUN", "EXIT", "PEND", "DONE"), filter = NULL, df = NULL)
}
\arguments{

  \item{status}{Status of the jobs. Use "all" for all jobs.}
  \item{filter}{Regular expression to filter on job names.}
  \item{df}{Internally used.}

}
\details{
It draws barplots of number of jobs per day.
}
\examples{
# There is no example
NULL
}
