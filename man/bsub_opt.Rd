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
  \item{\code{working_dir}}{The working directory.}
  \item{\code{ignore}}{Whether ignore \code{\link{bsub_chunk}}, \code{\link{bsub_script}} and \code{\link{bsub_cmd}}.}
  \item{\code{local}}{Run job locally (not submitting to the LSF cluster)?}
  \item{\code{call_Rscript}}{How to call \code{Rscript} by specifying an R version number.}
  \item{\code{submission_node}}{A list of node names for submitting jobs.}
  \item{\code{login_node}}{This value basically is the same as \code{submission_node} unless the login nodes are different from submission nodes.}
  \item{\code{sh_head}}{Commands that are written as head of the sh script.}
  \item{\code{user}}{Username on the submission node.}
  \item{\code{group}}{The user group}
  \item{\code{ssh_envir}}{The commands for setting bash environment for successfully running bjobs, bsub, ...}
  \item{\code{bsub_template}}{Template for constructing \code{bsub} command.}
  \item{\code{parse_time}}{A function that parses time string from the LSF \code{bjobs} command to a \code{\link{POSIXct}} object.}
  \item{\code{verbose}}{Whether to print more messages.}
}

\code{ssh_envir} should be properly set so that LSF binaries such as \code{bsub} or \code{bjobs} can be properly found.
There are some environment variables initialized when logging in the bash terminal while they are not initialized with the
ssh connection. Thus, some environment variables should be manually set.

An example for \code{ssh_envir} is as follows. The \code{LSF_ENVDIR} and \code{LSF_SERVERDIR} should be defined and exported.

  \preformatted{
    c("source /etc/profile",
      "export LSF_ENVDIR=/opt/lsf/conf",
      "export LSF_SERVERDIR=/opt/lsf/10.1/linux3.10-glibc2.17-x86_64/etc")  }

The values of these two variables can be obtained by entering following commands in your bash terminal (on the submission node):

  \preformatted{
    echo $LSF_ENVDIR
    echo $LSF_SERVERDIR  }

The time strings by LSF \code{bjobs} command might be different for different configurations. The **bsub**
package needs to convert the time strings to \code{\link{POSIXlt}} objects for calculating the time difference. Thus, if
the default time string parsing fails, users need to provide a user-defined function and set with \code{parse_time}
option in \code{\link{bsub_opt}}. The function accepts a vector of time strings and returns a \code{\link{POSIXlt}} object. For example,
if the time string returned from \code{bjobs} command is in a form of \code{Dec 1 18:00:00 2019}, the parsing function
can be defined as:

  \preformatted{
    bsub_opt$parse_time = function(x) \{
        as.POSIXlt(x, format = "\\%b \\%d \\%H:\\%M:\\%S \\%Y")
    \}  }
}
\value{
The corresponding option values.
}
\examples{
# The default bsub_opt
bsub_opt
}
