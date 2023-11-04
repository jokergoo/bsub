
#' Job status by job ID or name
#'
#' @param job_name A single job name.
#' @param job_id A single job ID.
#'
#' @returns A vector of job status, with job IDs are names.
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

    ln = run_cmd(qq("bjobs -a -o 'jobid output_file delimiter=\",\"' @{job_id} 2>&1"), print = FALSE)

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

    ln = run_cmd(qq("bjobs -a -o 'job_description' @{job_id} 2>&1"), print = FALSE)

    if(length(ln) == 1) {
        return(NA)
    }

    ln = ln[-1]
    lt = strsplit(ln, ",")

    jid = sapply(lt, function(x) x[1])
    jd = sapply(lt, function(x) x[2])
    jd[jd == "-"] = NA

    name_uid = gsub("^.*R_BSUB_NAME_UID=(.*),?.*$", "\\1", jid)
    temp_dir = gsub("^.*R_BSUB_TEMP_DIR=(.*)$", "\\1", jd)

    names(name_uid) = job_id
    names(temp_dir) = job_id

    list(name_uid = name_uid, temp_dir = temp_dir)
}

job_attached_vars_by_name = function(job_name) {

    ln = run_cmd(qq("bjobs -a -o 'job_description jobid delimiter=\",\"' -J @{job_name} 2>&1"), print = FALSE)

    if(length(ln) == 1) {
        return(NA)
    }

    ln = ln[-1]
    lt = strsplit(ln, ",")

    job_id = sapply(lt, function(x) x[3])
    jid = sapply(lt, function(x) x[1])
    jd = sapply(lt, function(x) x[2])
    jd[jd == "-"] = NA

    name_uid = gsub("^.*R_BSUB_NAME_UID=(.*),?.*$", "\\1", jid)
    temp_dir = gsub("^.*R_BSUB_TEMP_DIR=(.*)$", "\\1", jd)

    names(name_uid) = job_id
    names(temp_dir) = job_id

    list(name_uid = name_uid, temp_dir = temp_dir)
}


