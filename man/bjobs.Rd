\name{bjobs}
\alias{bjobs}
\title{
Summary of jobs
}
\description{
Summary of jobs
}
\usage{
bjobs(status = c("RUN", "PEND"), max = Inf, filter = NULL)
}
\arguments{

  \item{status}{Status of the jobs}
  \item{max}{Maximal number of recent jobs}
  \item{filter}{Regular expression to filter on job names}

}
\details{
There is an additional column "RECENT" which is the order
for the job with the same name. 1 means the most recent job.
}
\value{
A data frame with job summaries.
}
\examples{
# There is no example
NULL
}
