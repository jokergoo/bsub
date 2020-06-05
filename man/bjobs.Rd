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
  \item{print}{Whether to print the table.}

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
\dontrun{
bjobs # this is the same as bjobs()
bjobs() # all running and pending jobs
bjobs(status = "all") # all jobs
bjobs(status = "RUN") # all running jobs, you can also use `bjobs_running`
bjobs(status = "PEND") # all pending jobs, you can also use `bjobs_pending`
bjobs(status = "DONE") # all done jobs, you can also use `bjobs_done`
bjobs(status = "EXIT") # all exit jobs, you can also use `bjobs_exit`
bjobs(status = "all", max = 20) # last 20 jobs
bjobs(status = "DONE", filter = "example") # done jobs with name '.*example.*'
}
}
