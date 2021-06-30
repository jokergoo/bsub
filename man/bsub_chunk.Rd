\name{bsub_chunk}
\alias{bsub_chunk}
\title{
Submit R code
}
\description{
Submit R code
}
\usage{
bsub_chunk(code,
    name = NULL,
    packages = bsub_opt$packages,
    image = bsub_opt$image,
    variables = character(),
    share = character(),
    working_dir = bsub_opt$working_dir,
    hours = 1,
    memory = 1,
    cores = 1,
    R_version = bsub_opt$R_version,
    temp_dir = bsub_opt$temp_dir,
    output_dir = bsub_opt$output_dir,
    dependency = NULL,
    enforce = bsub_opt$enforce,
    local = bsub_opt$local,
    script = NULL,
    start = NULL,
    end = NULL,
    save_var = FALSE,
    sh_head = bsub_opt$sh_head)
}
\arguments{

  \item{code}{The code chunk, it should be embraced by \code{\{} \code{\}}.}
  \item{name}{If name is not specified, an internal name calculated by \code{\link[digest]{digest}} on the chunk is automatically assigned. }
  \item{packages}{A character vector with package names that will be loaded before running the script. There is a special name \code{_in_session_} that loads all the packages loaded in current R session.}
  \item{image}{A character vector of RData/rda files that will be loaded before running the script. When \code{image} is set to \code{TRUE}, all variables in \code{.GlobalEnv} will be saved into a temporary file and all attached packages will be recorded. The temporary files will be removed after the job is finished.}
  \item{variables}{A character vector of variable names that will be loaded before running the script. There is a special name \code{_all_functions_} that saves all functions defined in the global environment.}
  \item{share}{A character vector of variables names for which the variables are shared between jobs. Note the temporary .RData files are not deleted automatically.}
  \item{working_dir}{The working directory.}
  \item{hours}{Running time of the job.}
  \item{memory}{Memory usage of the job. It is measured in GB.}
  \item{cores}{Number of cores.}
  \item{R_version}{R version.}
  \item{temp_dir}{Path of temporary folder where the temporary R/bash scripts will be put.}
  \item{output_dir}{Path of output folder where the output/flag files will be put.}
  \item{dependency}{A vector of job IDs that current job depends on.}
  \item{enforce}{If a flag file for the job is found, whether to enforce to rerun the job.}
  \item{local}{Run job locally (not submitting to the LSF cluster)?}
  \item{script}{Path of a script where code chunks will be extracted and sent to the cluster.It is always used with \code{start} and \code{end} arguments.}
  \item{start}{A numeric vector that contains line indices of the starting code chunk or a character vector that contain regular expression to match the start of code chunks.}
  \item{end}{Same setting as \code{start}.}
  \item{save_var}{Whether save the last variable in the code chunk? Later the variable can be retrieved by \code{\link{retrieve_var}}.}
  \item{sh_head}{Commands that are written as head of the sh script.}

}
\value{
Job ID.
}
\seealso{
\itemize{
  \item \code{\link{bsub_script}} submits R scripts.
  \item \code{\link{bsub_cmd}}submits shell commands.
}
}
\examples{
\dontrun{
bsub_chunk(name = "example", memory = 10, hours = 10, cores = 4,
{
    Sys.sleep(5)
})
}
}
