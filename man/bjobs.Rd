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
  \item{max}{Maximal number of recent jobs.}
  \item{filter}{Regular expression to filter on job names.}
  \item{print}{Wether to print the table.}

}
\details{
There is an additional column "RECENT" which is the order
for the job with the same name. 1 means the most recent job.

You can directly type \code{bjobs} without parentheses which runs \code{\link{bjobs}} with defaults.
}
\value{
A data frame with selected job summaries.
}
\seealso{
\itemize{
  \item \code{\link{brecent}} shows the most recent.
  \item \code{\link{bjobs_done}} shows the "DONE" jobs.
  \item \code{\link{bjobs_exit}} shows the "EXIT" jobs.
  \item \code{\link{bjobs_pending}} shows the "PEND" jobs.
  \item \code{\link{bjobs_running}} shows the "RUN" jobs.
}
}
\examples{
# There is no example
NULL

}
