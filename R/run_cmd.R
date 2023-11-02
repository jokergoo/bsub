
#' Kill jobs
#'
#' @param job_id A vector of job IDs or a data frame returned by [`bjobs()`].
#' @param filter Regular expression on job names (only the running and pending jobs). It is
#'      only used when `job_id` is not set.
#'
#' @export
#' @examples
#' \dontrun{
#' job_id = c(10000000, 10000001, 10000002)  # job ids can be get from `bjobs()`
#' bkill(job_id)
#' # kill all jobs (running and pending) of which the names contain "example"
#' bkill(filter = "example") 
#' }
bkill = function(job_id, filter = NULL) {

    if(missing(job_id)) {
        job_df = bjobs(status = c("RUN", "PEND"), max = Inf, filter = filter, print = FALSE)
        job_id = job_df$JOBID
    }

    if(is.data.frame(job_id)) {
        df = job_id
        df = df[df$STAT %in% c("RUN", "PEND"), , drop = FALSE]
        if(nrow(df)) {
            job_id = df$JOBID
        } else {
            return(invisible(NULL))
        }
    }

    cmd = qq("bkill @{paste(job_id, collapse = ' ')} 2>&1")

    run_cmd(cmd, print = TRUE)
}

#' Run command on submission node
#'
#' @param cmd A single-line command.
#' @param print Whether to print output from the command.
#'
#' @details
#' If current node is not the submission node, the command is executed via ssh.
#'
#' @returns The output of the command.
#' @export
#' @examples
#' \dontrun{
#' # run pwd on remote node
#' run_cmd("pwd")
#' }
run_cmd = function(cmd, print = FALSE) {
    if(on_submission_node()) {
       con = pipe(cmd)
       ln = readLines(con)
       close(con)
    } else {
        ln = ssh_exec(cmd)
    }

    if(print) cat(ln, sep = "\n")
    return(invisible(ln))
}
