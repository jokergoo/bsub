\name{bjobs_exit}
\alias{bjobs_exit}
\title{
Failed jobs
}
\description{
Failed jobs
}
\usage{
bjobs_exit(max = Inf, filter = NULL)
}
\arguments{

  \item{max}{Maximal number of jobs.}
  \item{filter}{Regular expression to filter on job names.}

}
\details{
You can directly type \code{bjobs_exit} without parentheses which runs \code{\link{bjobs_exit}} with defaults.
}
\value{
The same output format as \code{\link{bjobs}}.
}
\examples{
\dontrun{
bjobs_exit  # this is the same as `bjobs_exit()`
bjobs_exit() # all exit jobs
bjobs_exit(max = 50) # last 50 exit jobs
bjobs_exit(filter = "example") # exit jobs with name ".*example.*"
}
}
