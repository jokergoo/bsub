\name{bjobs_running}
\alias{bjobs_running}
\title{
Running jobs
}
\description{
Running jobs
}
\usage{
bjobs_running(max = Inf, filter = NULL)
}
\arguments{

  \item{max}{Maximal number of jobs.}
  \item{filter}{Regular expression to filter on job names.}

}
\details{
You can directly type \code{bjobs_running} without parentheses which runs \code{\link{bjobs_running}} with defaults.
}
\value{
The same output format as \code{\link{bjobs}}.
}
\examples{
\dontrun{
bjobs_running  # this is the same as `bjobs_running()`
bjobs_running() # all running jobs
bjobs_running(max = 50) # last 50 running jobs
bjobs_running(filter = "example") # running jobs with name ".*example.*"
}
}
