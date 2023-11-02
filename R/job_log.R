
#' Obtain Job log
#'
#' @param job_id The job id. It can be a single job or a vector of job ids.
#' @param print Whether to print the log message to the terminal.
#' @param n_line Number of last lines for each job to show when multiple jobs are queried.
#' 
#' @returns The log messages as a vector.
#' @import clisymbols
#' @export
#' @examples
#' \dontrun{
#' # a single job
#' job_id = 1234567  # job ids can be get from `bjobs`
#' job_log(job_id)
#' # multiple jobs
#' job_id = c(10000000, 10000001, 10000002) 
#' job_log(job_id)  # by  default last 10 lines for each job are printed
#' job_log(job_id, n_line = 20) # print last 20 lines for each job
#' # logs for all running jobs
#' job_log()
#' }
job_log = function(job_id, print = TRUE, n_line = 10) {
    
    tb = bjobs(print = FALSE, status = "all")
    
    if(missing(job_id)) {
        tb = bjobs(print = FALSE)
    
        if(is.null(tb)) {
            txt = ("No running job")
            if(print) cat(txt, sep = "\n")
            return(invisible(txt))
        }
        return(job_log(tb[, 1], print = print, n_line = n_line))
    }

    job_id = as.character(job_id)

    if(!all(grepl("^\\d+$", job_id))) {
        txt = qq("Job ID should be all digits.")
        if(print) cat(txt, sep = "\n")
        return(invisible(txt))
    }

    if(length(job_id) > 1) {
        txt2 = NULL
        for(id in job_id) {
            if(print) qqcat("retrieve log for job @{id}\n")
            txt = job_log(id, print = FALSE)
            if(length(txt) > n_line) {
                txt2 = c(txt2, "\n", paste0(strrep(symbol$double_line, 10), qq(" log for job @{id}, last @{n_line} lines "), strrep(symbol$double_line, 10)))
                txt2 = c(txt2, txt[seq(length(txt) - n_line + 1, length(txt))])
            } else {
                txt2 = c(txt2, "\n", paste0(strrep(symbol$double_line, 10), qq(" log for job @{id} "), strrep(symbol$double_line, 10)))
                txt2 = c(txt2, txt)
            }
        }
        if(print) {
            cat(txt2, sep = "\n")
        }
        return(invisible(txt2))
    }

    # if the job with job_id is not the newest one with the same job name
    job_name = tb$JOB_NAME[tb$JOBID == job_id]
    job_wd = tb$EXEC_CWD[tb$JOBID == job_id]  
    job_queue = tb$QUEUE[tb$JOBID == job_id]  

    if(length(job_name) == 0) {
        txt = qq("Job @{job_id} does not exist.")
        if(print) cat(txt, sep = "\n")
        return(invisible(txt))
    }

    if(job_queue == "interactive") {
        txt = qq("Job @{job_id} is an interactive job and has no log.")
        if(print) cat(txt, sep = "\n")
        return(invisible(txt))
    }

    # tb_subset = tb[tb$JOB_NAME == job_name & tb$EXEC_CWD == job_wd, , drop = FALSE]
    # nrr = nrow(tb_subset)
    # if(nrr > 1) {
    #     tb_subset = tb_subset[order(tb_subset$JOBID), , drop = FALSE]
    #     if(tb_subset$JOBID[nrr] != job_id) {
    #         txt = qq("Log file for job @{job_id} has been overwriten by job @{tb_subset$JOBID[nrr]} which is the newest job with the same job name '@{tb_subset$JOB_NAME[nrr]}'.")
    #         txt = strwrap(txt)
    #         if(print) cat(txt, sep = "\n")
    #         return(invisible(txt))
    #     }
    # }

    ln = run_cmd(qq("bjobs -o \"stat output_file\" @{job_id} 2>&1"), print = FALSE)

    if(length(ln) == 1) {
        status = "MISSING"
    }

    ln = ln[-1]  # remove header
    status = gsub("\\s.*$", "", ln)
    output_file = gsub("^\\S+\\s+", "", ln)

    if(status == "MISSING") {
        txt = qq("Cannot find output file for job @{job_id}.")
        if(print) cat(txt, sep = "\n")
        return(invisible(txt))
    } else if(status == "PEND") {
        txt = qq("Job @{job_id} is still pending.")
        if(print) cat(txt, sep = "\n")
        return(invisible(txt))
    } else if(status == "RUN") {
        # bpeek is slow, we first directly check the temporary output file
        user = bsub_opt$user

        # the temporary job dir
        ln = run_cmd("bparams -a", print = FALSE)
        ind = grep("JOB_SPOOL_DIR", ln)
        if(length(ind)) {
            job_temp_dir = gsub("^\\s*JOB_SPOOL_DIR = ", "", ln[ind])
            job_temp_dir = gsub("/%U$", "", job_temp_dir)
            job_temp_dir = qq("@{job_temp_dir}/@{user}")
        } else {
            remote_home = run_cmd("echo $HOME", print = FALSE)
            job_temp_dir = qq("@{remote_home}/.lsbatch")
        }

        no_file_flag = FALSE
        # check running jobs
        if(on_submission_node()) {
            file = list.files(path = job_temp_dir, pattern = paste0("\\.", job_id,, "\\.out"), full.names = TRUE)

            if(length(file) == 0) {
                no_file_flag = TRUE
            } else {
                txt = readLines(file, warn = FALSE)
                txt = c(txt, paste0("\n", symbol$warning, qq(" job @{job_id} is still running.")))
                if(print) cat(txt, sep = "\n")
                return(invisible(txt))
            }
        } else {
            # if no such file, ssh_exec gives an error
            oe = try(ln <- ssh_exec(qq("ls @{job_temp_dir}/*.@{job_id}.out")), silent = TRUE)

            if(inherits(oe, "try-error")) {
                no_file_flag = TRUE
            } else if(length(ln) == 0) {
                no_file_flag = TRUE
            } else {
                txt = ssh_exec(qq("cat @{ln}"))
                txt = c(txt, paste0(symbol$warning, qq(" job @{job_id} is still running.")))
                if(print) cat(txt, sep = "\n")
                return(invisible(txt))
            }
        }

        if(no_file_flag) {
            ln = run_cmd(qq("bpeek @{job_id}"), print = FALSE)[-1]
            if(length(ln) == 0) {
                txt = qq("Cannot find output file for job @{job_id}.")
                if(print) cat(txt, sep = "\n")
                return(invisible(txt))
            } else {
                txt = ln
                txt = c(txt, paste0(symbol$warning, qq("\n job @{job_id} is still running. "), symbol$warning))
                if(print) cat(txt, sep = "\n")
                return(invisible(txt))
            }
        }
    } else {
        no_file_flag = FALSE
        if(on_submission_node()) {
            
            if(!file.exists(output_file)) {
                no_file_flag = TRUE
            } else {
                txt = readLines(output_file, warn = FALSE)
                txt = extract_job_log(txt, job_id)
                if(print) cat(txt, sep = "\n")
                return(invisible(txt))
            }
        } else {
            # if no such file, ssh_exec gives an error
            oe = try(ln <- ssh_exec(qq("ls @{output_file}")), silent = TRUE)

            if(inherits(oe, "try-error")) {
                no_file_flag = TRUE
            } else if(length(ln) == 0) {
                no_file_flag = TRUE
            } else {
                txt = ssh_exec(qq("cat @{ln}"))
                txt = extract_job_log(txt, job_id)
                if(print) cat(txt, sep = "\n")
                return(invisible(txt))
            }
        }

        if(no_file_flag) {
            txt = qq("Cannot find output file for job @{job_id}.")
            if(print) cat(txt, sep = "\n")
            return(invisible(txt))
        }
    }
}


# it is possible multiple job logs are in one out file
extract_job_log = function(ln, job_id) {
    n = length(ln)
    ind = grep("^Sender: LSF System", ln)
    all_ids = gsub("^Subject: Job (\\d+):.*$", "\\1", ln[ind+1])
    k = which(all_ids == job_id)
    nk = length(k)

    if(k == nk) {
        ln[seq(ind[k], n)]
    } else {
        ln[seq(ind[k], ind[k+1]-1)]
    }
}
