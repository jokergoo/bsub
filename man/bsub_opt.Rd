\name{bsub_opt}
\alias{bsub_opt}
\title{
Parameters for bsub
}
\description{
Parameters for bsub
}
\usage{
bsub_opt(..., RESET = FALSE, READ.ONLY = NULL, LOCAL = FALSE, ADD = FALSE)
}
\arguments{

  \item{...}{Arguments for the parameters, see "details" section}
  \item{RESET}{reset to default values}
  \item{READ.ONLY}{please ignore}
  \item{LOCAL}{please ignore}
  \item{ADD}{please ignore}

}
\details{
There are following parameters:

\describe{
  \item{\code{packages}}{A character vector with package names that will be loaded before running the script.}
  \item{\code{image}}{A character vector of RData/rda files that will be loaded before running the script.}
  \item{\code{temp_dir}}{Path of temporary folder where the temporary R/bash scripts will be put.}
  \item{\code{output_dir}}{Path of output folder where the output/flag files will be put.}
  \item{\code{enforce}}{If a flag file for the job is found, whether to enforce to rerun the job.}
  \item{\code{R_version}}{The version of R.}
  \item{\code{wd}}{The working directory.}
  \item{\code{ignore}}{Whether ignore \code{\link{bsub_chunk}}, \code{\link{bsub_script}} and \code{\link{bsub_cmd}}.}
  \item{\code{local}}{Run job locally (not submitting to the LSF cluster)?}
  \item{\code{call_Rscript}}{How to call \code{Rscript} by specifying an R version number.}
  \item{\code{submission_node}}{A list of node names for submitting jobs.}
  \item{\code{sh_head}}{Commands that are written as head of the sh script.}
  \item{\code{user}}{Username on the submission node.}
  \item{\code{group}}{The user group}
  \item{\code{ssh_envir}}{The commands for setting bash environment for successfully running bjobs, bsub, ...}
  \item{\code{bsub_template}}{Template for constructing \code{bsub} command.}
}

For \code{ssh_envir} option, an example is as follows. The \code{LSF_ENVDIR} and \code{LSF_SERVERDIR} should be defined and exported.

  \preformatted{
    c("source /etc/profile",
      "export LSF_ENVDIR=/opt/lsf/conf",
      "export LSF_SERVERDIR=/opt/lsf/10.1/linux3.10-glibc2.17-x86_64/etc")  }
}
\examples{
# examples are in the vignette.
}
