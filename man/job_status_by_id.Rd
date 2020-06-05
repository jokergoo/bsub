\name{job_status_by_id}
\alias{job_status_by_id}
\title{
Job status by id
}
\description{
Job status by id
}
\usage{
job_status_by_id(job_id)
}
\arguments{

  \item{job_id}{The job id.}

}
\value{
If the job has been deleted from the database, it returns MISSING.
}
\examples{
\dontrun{
job_id = 1234567  # job ids can be get from `bjobs`
job_status_by_id(job_id)
}
}
