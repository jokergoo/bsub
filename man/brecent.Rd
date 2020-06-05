\name{brecent}
\alias{brecent}
\title{
Recent jobs from all status
}
\description{
Recent jobs from all status
}
\usage{
brecent(max = 20, filter = NULL)
}
\arguments{

  \item{max}{Maximal number of recent jobs.}
  \item{filter}{Regular expression to filter on job names.}

}
\details{
You can directly type \code{brecent} without parentheses which runs \code{\link{brecent}} with defaults.
}
\value{
The same output format as \code{\link{bjobs}}.
}
\examples{
\dontrun{
brecent  # this is the same as `brecent()`
brecent() # last 20 jobs (from all status)
brecent(max = 50) # last 50 jobs
brecent(filter = "example") # last 20 jobs with name ".*example.*"
}
}
