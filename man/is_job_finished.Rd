\name{is_job_finished}
\alias{is_job_finished}
\title{
Test whether the jobs are finished
}
\description{
Test whether the jobs are finished
}
\usage{
is_job_finished(job_name, output_dir = bsub_opt$output_dir)
}
\arguments{

  \item{job_name}{A vector of job names.}
  \item{output_dir}{Output dir.}

}
\details{
It tests whether the ".done" flag files exist
}
\value{
A logical scalar.
}
\examples{
# There is no example
NULL

}
