\name{job_log}
\alias{job_log}
\title{
Log for the running/finished/failed job
}
\description{
Log for the running/finished/failed job
}
\usage{
job_log(job_id, print = TRUE, n_line = 10)
}
\arguments{

  \item{job_id}{The job id. It can be a single job or a vector of job ids.}
  \item{print}{Whether print the log message.}
  \item{n_line}{Number of last lines for each job to show when multiple jobs are queried.}

}
\value{
The log message as a vector.
}
\examples{
# There is no example
NULL

}
