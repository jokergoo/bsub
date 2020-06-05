\name{bkill}
\alias{bkill}
\title{
Kill jobs
}
\description{
Kill jobs
}
\usage{
bkill(job_id, filter = NULL)
}
\arguments{

  \item{job_id}{A vector of job ids.}
  \item{filter}{Regular expression to filter on job names (only the running and pending jobs).}

}
\value{
No value is returned.
}
\examples{
\dontrun{
job_id = c(10000000, 10000001, 10000002)  # job ids can be get from `bjobs`
bkill(job_id)
# kill all jobs (running and pending) of which the names contain "example"
bkill(filter = "example") 
}
}
