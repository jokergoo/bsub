\name{bsub_cmd}
\alias{bsub_cmd}
\title{
Submit shell commands
}
\description{
Submit shell commands
}
\usage{
bsub_cmd(cmd,
    name = NULL,
    hour = 1,
    memory = 1,
    core = 1,
    temp_dir = bsub_opt$temp_dir,
    output_dir = bsub_opt$output_dir,
    dependency = NULL,
    enforce = bsub_opt$enforce,
    local = bsub_opt$local,
    sh_head = bsub_opt$sh_head,
    ...)
}
\arguments{

  \item{cmd}{A list of commands.}
  \item{name}{If name is not specified, an internal name calculated by \code{\link[digest]{digest}} is automatically assigned. }
  \item{hour}{Running time of the job.}
  \item{memory}{Memory usage of the job. It is measured in GB.}
  \item{core}{Number of cores.}
  \item{temp_dir}{Path of temporary folder where the temporary R/bash scripts will be put.}
  \item{output_dir}{Path of output folder where the output/flag files will be put.}
  \item{dependency}{A vector of job IDs that current job depends on.}
  \item{enforce}{If a flag file for the job is found, whether to enforce to rerun the job.}
  \item{local}{Run job locally (not submitting to the LSF cluster)?}
  \item{sh_head}{Commands that are written as head of the sh script.}
  \item{...}{Command-line arguments can also be specified as name-value pairs.}

}
\value{
Job ID.
}
\seealso{
\itemize{
  \item \code{\link{bsub_chunk}}submits R code.
  \item \code{\link{bsub_script}} submits R scripts.
}
}
\examples{
\dontrun{
bsub_cmd("samtools sort ...", name = ..., memory = ..., core = ..., ...)
}
}
