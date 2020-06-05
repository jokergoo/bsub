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

  \item{print}{Whether to print messages.}

}
\details{
For the failed jobs, LSF cluster might generate a core dump file and R might generate a .RDataTmp file.

Note if you manually set working directory in your R code/script, the R dump file can be not caught.
}
\value{
A vector of file names.
}
\examples{
\dontrun{
check_dump_files()
}
}
