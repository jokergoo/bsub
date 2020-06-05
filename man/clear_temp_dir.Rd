\name{clear_temp_dir}
\alias{clear_temp_dir}
\title{
Clear temporary dir
}
\description{
Clear temporary dir
}
\usage{
clear_temp_dir(ask = TRUE)
}
\arguments{

  \item{ask}{Whether promote.}

}
\details{
The temporary files might be used by the running/pending jobs. Deleting them might affect some of the jobs.
You better delete them after all jobs are done.
}
\value{
No value is returned.
}
\examples{
\dontrun{
clear_temp_dir()
}
}
