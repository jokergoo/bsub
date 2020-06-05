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
\dontrun{
# a single job
job_id = 1234567  # job ids can be get from `bjobs`
job_log(job_id)
# multiple jobs
job_id = c(10000000, 10000001, 10000002) 
job_log(job_id)  # by  default last 10 lines for each job are printed
job_log(job_id, n_line = 20) # print last 20 lines for each job
# logs for all running jobs
job_log()
}
}
