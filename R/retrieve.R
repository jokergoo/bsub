
#' Retrieve saved variable
#'
#' @param job_id A single job ID.
#' @param job_name A single job name. Since jobs may have the same names, the most recent job is selected.
#' @param wait Seconds to wait until the job is finished.
#'
#' @details
#' It retrieves the saved variable in [`bsub_chunk()`] when `save_rds = TRUE` is set.
#'
#' @returns The retrieved object.
#'
#' @examples
#' \dontrun{
#' job_id = bsub_chunk(name = "example", save_var = TRUE,
#' {
#'     Sys.sleep(10)
#'     1+1
#' })
#' retrieve_var(job_id)
#' }
retrieve_var = function(job_id, job_name = NULL, wait = 30) {

    if(!under_same_file_system()) {
        stop("Variable can only be retrieved on the same file system as submission node.")
    }

    if(!missing(job_id)) {
        output_dir = dirname(job_output_file_by_id(job_id))
        attached_vars = job_attached_vars_by_id(job_id)
        name_uid = attached_vars$name_uid
        temp_dir = attached_vars$temp_dir
    } else {
        output_file = job_output_file_by_name(job_name)
        if(length(output_file) > 1) {
            i = which.max(as.numeric(names(output_file)))
            message_wrap(qq("Found @{length(output_file)} jobs with name '@{job_name}'. Use the variable from the most recent job <@{names(output_file)[i]}>. If this is not what you want, please specify argument `job_id = ...`."))
        } else {
            i = 1
        }
        attached_vars = job_attached_vars_by_name(job_name)
        name_uid = attached_vars$name_uid
        temp_dir = attached_vars$temp_dir

        job_id = names(temp_dir)[i]
        output_dir = dirname(output_file[i])
        name_uid = name_uid[i]
        temp_dir = temp_dir[i]
    }

    job_tb = bjobs(status = "all", print = FALSE, job_id = job_id)
    name = job_tb$JOB_NAME

    rds_file = qq("@{output_dir}/@{name_uid}_returned_var.rds")
    out_file = qq("@{output_dir}/@{name_uid}.out")
    flag_file = qq("@{output_dir}/@{name}.done")

    if(file.exists(flag_file)) {
        if(file.exists(out_file)) {
            ln = readLines(out_file)
            if(any(grepl("^Successfully completed", ln))) {
                if(file.exists(rds_file)) {
                    return(readRDS(rds_file))
                } else {
                    stop("Maybe you forget to set `save_var = TRUE` in `bsub_chunk()`?")
                }
            } else {
                stop("job failed.")
            }
        } else {
            stop("job failed.")
        }
    }

    status = job_status_by_id(job_id)
    if(status %in% c("RUN", "PEND")) {
        message(qq("job is running or pending, retry in @{wait} seconds."))
        Sys.sleep(wait)
        retrieve_var(job_id, wait = wait)
    } else if(status == "DONE") {
        retrieve_var(job_id)
    } else if(status == "MISSING") {
        stop(qq("cannot find the job '@{job_name}'."))
    } else {
        stop("job failed.")
    }
}
