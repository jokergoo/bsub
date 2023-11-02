
#' Job status by ID or name
#'
#' @param job_name A single job name.
#' @param job_id A single job ID.
#'
#' @export
#' @rdname job_status
#' @examples
#' \dontrun{
#' job_status_by_name("example")
#' job_status_by_id(123456)
#' }
job_status_by_name = function(job_name) {

    ln = run_cmd(qq("bjobs -a -o 'jobid stat delimiter=\",\"' -J @{job_name} 2>&1"), print = FALSE)

    if(length(ln) == 1) {
        return("MISSING")
    }

    ln = ln[-1]
    lt = strsplit(ln, ",")

    structure(sapply(lt, function(x) x[2]), names = sapply(lt, function(x) x[1]))
}

#' @export
#' @rdname job_status
job_status_by_id = function(job_id) {

    if(missing(job_id)) {
        job_id = ""
    }

    ln = run_cmd(qq("bjobs -a -o 'jobid stat delimiter=\",\"' @{job_id} 2>&1"), print = FALSE)

    if(length(ln) == 1) {
        return("MISSING")
    }

    ln = ln[-1]
    lt = strsplit(ln, ",")

    structure(sapply(lt, function(x) x[2]), names = sapply(lt, function(x) x[1]))
}

job_output_file_by_id = function(job_id) {
    
    if(missing(job_id)) {
        job_id = ""
    }

    ln = run_cmd(qq("bjobs -a -o \"jobid output_file\" @{job_id} 2>&1"), print = FALSE)

    if(length(ln) == 1) {
        return(NA)
    }

    ln = ln[-1]
    lt = strsplit(ln, ",")

    structure(sapply(lt, function(x) x[2]), names = sapply(lt, function(x) x[1]))
}

job_output_file_by_name = function(job_name) {
    ln = run_cmd(qq("bjobs -a -o 'jobid output_file delimiter=\",\"' -J \"@{job_name}\" 2>&1"), print = FALSE)

    if(length(ln) == 1) {
        return(NA)
    }

    ln = ln[-1]
    lt = strsplit(ln, ",")

    structure(sapply(lt, function(x) x[2]), names = sapply(lt, function(x) x[1]))
}

job_attached_vars_by_id = function(job_id) {

    if(missing(job_id)) {
        job_id = ""
    }

    ln = run_cmd(qq("bjobs -a -o 'jobid job_description' @{job_id} 2>&1"), print = FALSE)

    if(length(ln) == 1) {
        return(NA)
    }

    ln = ln[-1]
    lt = strsplit(ln, ",")

    jid = sapply(lt, function(x) x[1])
    jd = sapply(lt, function(x) x[2])

    name_uid = gsub("^.*R_BSUB_NAME_UID=(.*)?;.*$", "\\1", jd)
    temp_dir = gsub("^.*R_BSUB_TEMP_DIR=(.*)$", "\\1", jd)

    names(name_uid) = jid
    names(temp_dir) = jid

    list(name_uid = name_uid, temp_dir = temp_dir)
}

job_attached_vars_by_name = function(job_name) {

    ln = run_cmd(qq("bjobs -a -o 'jobid job_description' -J @{job_name} 2>&1"), print = FALSE)

    if(length(ln) == 1) {
        return(NA)
    }

    ln = ln[-1]
    lt = strsplit(ln, ",")

    jid = sapply(lt, function(x) x[1])
    jd = sapply(lt, function(x) x[2])

    name_uid = gsub("^.*R_BSUB_NAME_UID=(.*)?;.*$", "\\1", jd)
    temp_dir = gsub("^.*R_BSUB_TEMP_DIR=(.*)$", "\\1", jd)

    names(name_uid) = jid
    names(temp_dir) = jid

    list(name_uid = name_uid, temp_dir = temp_dir)
}

#' Retrieve saved variable
#'
#' @param job_id A single job ID.
#' @param job_name A single Job name. Since jobs may be the same names, it is more suggested to set `job_id`.
#' @param wait Seconds to wait.
#'
#' @details
#' It retrieve the saved variable in `bsub_chunk()` when ``save_rds = TRUE`` is set.
#'
#' @returns The retrieved object.
#'
#' @examples
#' \dontrun{
#' bsub_chunk(name = "example", save_var = TRUE,
#' {
#'     Sys.sleep(10)
#'     1+1
#' })
#' retrieve_var("example")
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
        output_dir = dirname(job_output_file_by_name(job_name))
        if(length(output_dir) > 1) {
            message_wrap(qq("Found more than one jobs with name '@{job_name}', use the variable in most recent job. If this is not what you want, please specify `job_id`."))
            i = which.max(names(output_dir))
        }
        attached_vars = job_attached_vars_by_id(job_name)
        name_uid = attached_vars$name_uid
        temp_dir = attached_vars$temp_dir

        job_id = names(output_dir)[i]
        output_dir = output_dir[i]
        name_uid = name_uid[i]
        temp_dir = temp_dir[i]
    }

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


