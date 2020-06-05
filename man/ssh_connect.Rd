\name{ssh_connect}
\alias{ssh_connect}
\title{
Connect to submisstion via ssh
}
\description{
Connect to submisstion via ssh
}
\usage{
ssh_connect()
}
\details{
If ssh connection is lost, run this function to reconnect.
}
\value{
No value is returned.
}
\examples{
# ssh is automatically connected. To manually connect ssh, run:
\dontrun{
ssh_connect()
}
# where the user name is the one you set in `bsub_opt$user` and
# the node is the one you set in `bsub_opt$login_node`.
}
