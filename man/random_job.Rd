\name{random_job}
\alias{random_job}
\title{
Submit a random job
}
\description{
Submit a random job
}
\usage{
random_job(name = paste0("R_random_job_", digest::digest(runif(1), "crc32")), ...)
}
\arguments{

  \item{name}{Job name.}
  \item{...}{Pass to \code{\link{bsub_chunk}}.}

}
\details{
It only submits \code{Sys.sleep(30)}.
}
\value{
The job id.
}
\examples{
\dontrun{
random_job()
random_job(name = "test")
}
}
