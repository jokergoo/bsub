\name{run_cmd}
\alias{run_cmd}
\title{
Run command on submission node
}
\description{
Run command on submission node
}
\usage{
run_cmd(cmd, print = FALSE)
}
\arguments{

  \item{cmd}{A single-line command.}
  \item{print}{Whether to print output from the command.}

}
\details{
If current node is not the submission node, the command is executed via ssh.
}
\value{
The output of the command
}
\examples{
\dontrun{
# run pwd on remote node
run_cmd("pwd")
}
}
