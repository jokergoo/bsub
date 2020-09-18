\name{bjobs_timeline}
\alias{bjobs_timeline}
\title{
Timeline of jobs
}
\description{
Timeline of jobs
}
\usage{
bjobs_timeline(status = c("RUN", "EXIT", "PEND", "DONE"), filter = NULL, df = NULL)
}
\arguments{

  \item{status}{Status of the jobs. Use "all" for all jobs.}
  \item{filter}{Regular expression to filter on job names.}
  \item{df}{Internally used.}

}
\details{
It draws segments of duration of jobs. In the plot, each segment represents
a job and the width of the segment correspond to its duration.
}
\examples{
# There is no example
NULL
}
