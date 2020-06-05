\name{bjobs_done}
\alias{bjobs_done}
\title{
Finished jobs
}
\description{
Finished jobs
}
\usage{
bjobs_done(max = Inf, filter = NULL)
}
\arguments{

  \item{max}{Maximal number of jobs.}
  \item{filter}{Regular expression to filter on job names.}

}
\details{
You can directly type \code{bjobs_done} without parentheses which runs \code{\link{bjobs_done}} with defaults.
}
\value{
The same output format as \code{\link{bjobs}}.
}
\examples{
\dontrun{
bjobs_done  # this is the same as `bjobs_done()`
bjobs_done() # all done jobs
bjobs_done(max = 50) # last 50 done jobs
bjobs_done(filter = "example") # done jobs with name ".*example.*"
}
}
