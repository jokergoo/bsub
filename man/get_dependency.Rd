\name{get_dependency}
\alias{get_dependency}
\title{
Get the dependency of current jobs
}
\description{
Get the dependency of current jobs
}
\usage{
get_dependency(job_tb = NULL)
}
\arguments{

  \item{job_tb}{A table from \code{\link{bjobs}}. Optional.}

}
\value{
If there is no dependency of all jobs, it returns \code{NULL}. If there are dependencies,
it returns a list of three elements:

\describe{
  \item{\code{dep_mat}:}{a two column matrix containing dependencies from parents to children.}
  \item{\code{id2name}:}{a named vector containing mapping from job IDs to job names.}
  \item{\code{id2stat}:}{a named vector containing mapping from job IDs to job status.}
}
}
\examples{
\dontrun{
get_dependency()
}
}
