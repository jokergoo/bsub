% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean.R
\name{list_temp_files}
\alias{list_temp_files}
\alias{remove_temp_files}
\title{Clear temporary dir}
\usage{
list_temp_files()

remove_temp_files(ask = TRUE)
}
\arguments{
\item{ask}{Whether to promote.}
}
\description{
Clear temporary dir
}
\details{
The temporary files might be used by the running/pending jobs. Deleting them would affect some jobs.
You should better delete them after all jobs are done.
}
\examples{
\dontrun{
list_temp_files()
}

}
