\name{check_dump_files}
\alias{check_dump_files}
\title{
Check whether there are dump files
}
\description{
Check whether there are dump files
}
\usage{
check_dump_files(print = TRUE)
}
\arguments{

  \item{print}{Whether print messages}

}
\details{
For the failed jobs, LSF cluster generates a core dump file and R generates a .RDataTmp file.

Note if you manually set working directory in your R code/script, the R dump file can be not caught.
}
\examples{
# There is no example
NULL
}
