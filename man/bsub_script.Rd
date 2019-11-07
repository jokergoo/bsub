\name{bsub_script}
\alias{bsub_script}
\title{
Send R script
}
\description{
Send R script
}
\usage{
bsub_script(script,
    argv = "",
    name = NULL,
    hour = 1,
    memory = 1,
    core = 1,
    R_version = bsub_opt$R_version,
    temp_dir = bsub_opt$temp_dir,
    output_dir = bsub_opt$output_dir,
    dependency = NULL,
    enforce = bsub_opt$enforce,
    local = bsub_opt$local,
    ...)
}
\arguments{

  \item{script}{The R script.}
  \item{argv}{A string of command-line arguments.}
  \item{name}{If name is not specified, an internal name calculated by \code{\link[digest]{digest}} is automatically assigned. }
  \item{hour}{Running time of the job.}
  \item{memory}{Memory usage of the job. It is measured in GB.}
  \item{core}{Number of cores.}
  \item{R_version}{R version.}
  \item{temp_dir}{Path of temporary folder where the temporary R/bash scripts will be put.}
  \item{output_dir}{Path of output folder where the output/flag files will be put.}
  \item{dependency}{A vector of job IDs that current job depends on.}
  \item{enforce}{If a flag file for the job is found, whether to enforce to rerun the job.}
  \item{local}{Run job locally (not submitting to the LSF cluster)?}
  \item{...}{Command-line arguments can also be specified as name-value pairs.}

}
\value{
Job ID.
}
\seealso{
\itemize{
  \item \code{\link{bsub_chunk}} submits R code.
  \item \code{\link{bsub_cmd}}submits shell commands.
}
}
\examples{
# exaples are in the vignette.
}
