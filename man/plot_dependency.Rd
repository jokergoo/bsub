\name{plot_dependency}
\alias{plot_dependency}
\title{
Plot the job dependency tree
}
\description{
Plot the job dependency tree
}
\usage{
plot_dependency(job_id, job_tb = NULL)
}
\arguments{

  \item{job_id}{A job ID.}
  \item{job_tb}{A table from \code{\link{bjobs}}. Optional.}

}
\value{
No value is returned.
}
\examples{
\dontrun{
job1 = random_job()
job2 = random_job()
job3 = random_job(dependency = c(job1, job2))
plot_dependency(job3)
}
}
