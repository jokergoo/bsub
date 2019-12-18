\name{bjobs}
\alias{bjobs}
\title{
Summary of jobs
}
\description{
Summary of jobs
}
\usage{
bjobs(status = c("RUN", "PEND"), max = Inf, filter = NULL, print = TRUE)
}
\arguments{

  \item{status}{Status of the jobs. Use "all" for all jobs.}
  \item{max}{Maximal number of recent jobs}
  \item{filter}{Regular expression to filter on job names}
  \item{print}{Wether print the table}

}
\details{
There is an additional column "RECENT" which is the order
for the job with the same name. 1 means the most recent job.

You can directly type \code{bjobs} without parentheses which runs \code{\link{bjobs}} with defaults.
}
\value{
A data frame with selected job summaries.
}
\examples{
# There is no example
NULL

}
