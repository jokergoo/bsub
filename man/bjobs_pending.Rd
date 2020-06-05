\name{bjobs_pending}
\alias{bjobs_pending}
\title{
Pending jobs
}
\description{
Pending jobs
}
\usage{
bjobs_pending(max = Inf, filter = NULL)
}
\arguments{

  \item{max}{Maximal number of jobs.}
  \item{filter}{Regular expression to filter on job names.}

}
\details{
You can directly type \code{bjobs_pending} without parentheses which runs \code{\link{bjobs_pending}} with defaults.
}
\value{
The same output format as \code{\link{bjobs}}.
}
\examples{
\dontrun{
bjobs_pending  # this is the same as `bjobs_pending()`
bjobs_pending() # all pending jobs
bjobs_pending(max = 50) # last 50 pending jobs
bjobs_pending(filter = "example") # pending jobs with name ".*example.*"
}
}
