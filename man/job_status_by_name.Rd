\name{job_status_by_name}
\alias{job_status_by_name}
\title{
Job status by name
}
\description{
Job status by name
}
\usage{
job_status_by_name(job_name, output_dir = bsub_opt$output_dir)
}
\arguments{

  \item{job_name}{Job name.}
  \item{output_dir}{The output dir.}

}
\value{
If the job is finished, it returns DONE/EXIT/MISSING. If the job is running or pending, it returns the corresponding
status. If there are multiple jobs with the same name running or pending, it returns a vector.
}
\examples{
\dontrun{
job_status_by_name("example")
}
}
