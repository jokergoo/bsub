
#' Summary of jobs
#'
#' @param status Status of the jobs. Use "all" for all jobs.
#' @param max Maximal number of recent jobs.
#' @param filter Regular expression on job names.
#' @param print Whether to print the table.
#' @param job_id A single job ID, internally used.
#'
#' @details
#' There is an additional column "RECENT" which is the order
#' for the job with the same name. 1 means the most recent job.
#'
#' You can directly type ``bjobs`` without parentheses which runs `bjobs` with defaults.
#'
#' - `brecent` shows the most recent.
#' - `bjobs_done` shows the "DONE" jobs.
#' - `bjobs_exit` shows the "EXIT" jobs.
#' - `bjobs_pending` shows the "PEND" jobs.
#' - `bjobs_running` shows the "RUN" jobs.
#'
#' @returns A data frame with selected job summaries.
#' @rdname bjobs
#' @importFrom utils read.csv
#' @examples
#' \dontrun{
#' bjobs # this is the same as bjobs()
#' bjobs() # all running and pending jobs
#' bjobs(status = "all") # all jobs
#' bjobs(status = "RUN") # all running jobs, you can also use `bjobs_running`
#' bjobs(status = "PEND") # all pending jobs, you can also use `bjobs_pending`
#' bjobs(status = "DONE") # all done jobs, you can also use `bjobs_done`
#' bjobs(status = "EXIT") # all exit jobs, you can also use `bjobs_exit`
#' bjobs(status = "all", max = 20) # last 20 jobs
#' bjobs(status = "DONE", filter = "example") # done jobs with name '.*example.*'
#' }
bjobs = function(status = c("RUN", "PEND"), max = Inf, filter = NULL, print = TRUE, job_id = NULL) {

    if(is.null(job_id)) {
        cmd = "bjobs -a -o 'jobid stat job_name queue submit_time start_time finish_time runtimelimit slots mem max_mem dependency exec_cwd combined_resreq delimiter=\",\"' 2>&1"
    } else {
        cmd = qq("bjobs -a -o 'jobid stat job_name queue submit_time start_time finish_time runtimelimit slots mem max_mem dependency exec_cwd combined_resreq delimiter=\",\"' @{job_id} 2>&1")
    }
    ln = run_cmd(cmd, print = FALSE)
    
    if(length(ln) == 1) {
        cat(ln, "\n")
        return(invisible(NULL))
    }

    df = read.csv(textConnection(paste(ln, collapse = "\n")), stringsAsFactors = FALSE)
    df$STAT = factor(df$STAT)

    df$REQ_MEM = as.numeric(gsub("^.*mem=(.*?)\\].*$", "\\1", df$COMBINED_RESREQ))
    df$REQ_MEM = round(df$REQ_MEM/1024, 1)

    df$SUBMIT_TIME = convert_to_POSIXlt(df$SUBMIT_TIME)
    df$START_TIME = convert_to_POSIXlt(df$START_TIME)
    df$FINISH_TIME = convert_to_POSIXlt(df$FINISH_TIME)

    if(!is.null(bsub_opt$history_timestamp)) {
        l = df$FINISH_TIME > bsub_opt$history_timestamp
        df = df[l, , drop = FALSE]
    }

    # running/pending jobs
    df$TIME_PASSED = difftime(Sys.time(), df$START_TIME, units = "hours")
    l = !(df$STAT %in% c("RUN", "PEND"))
    # l[is.na(l)] = FALSE  # finish time is unavailable
    df$TIME_PASSED[l] = difftime(df$FINISH_TIME[l], df$START_TIME[l], units = "hours")
    df$TIME_LEFT = difftime(df$FINISH_TIME, Sys.time(), units = "hours")
    l = df$FINISH_TIME < Sys.time()
    l[is.na(l)] = TRUE
    df$TIME_LEFT[l] = NA

    df$JOBID = as.numeric(df$JOBID)
    df = df[order(df$JOBID), , drop = FALSE]
    tb = table(df$STAT)
    tb_today = table(df$STAT[as.numeric(difftime(Sys.time(), df$SUBMIT_TIME, units = "secs")) < 3600*24])

    recent = unlist(unname(tapply(df$JOBID, df$JOB_NAME, function(x) {
        structure(order(-x), names = x)
    }, simplify = FALSE)))
    df$RECENT = recent[as.character(df$JOBID)]

    if(! "all" %in% status) {
        df = df[df$STAT %in% toupper(status), , drop = FALSE]
    }
    if(!is.null(filter)) {
        df = df[grepl(filter, df$JOB_NAME), , drop = FALSE]
    }
    if(nrow(df)) {
        ind = sort((nrow(df):1)[1:min(c(nrow(df), max))])

        if(!print) {
            return(df[ind, , drop = FALSE])
        }

        df2 = format_summary_table(df[ind, , drop = FALSE])

        max_width = pmax(apply(df2, 2, function(x) max(nchar(x)+1)),
                         nchar(colnames(df2)) + 1)
        ow = getOption("width")
        options(width = sum(max_width) + 10)
        cat(strrep("=", sum(max_width)), "\n")
        print(df2, row.names = FALSE, right = FALSE, max = 99999)
        if(nrow(df2) > 20) {
            for(i in seq_len(ncol(df2))) {
                nm = colnames(df2)[i]
                cat(" ", nm, sep = "")
                cat(strrep(" ", max_width[i] - nchar(nm) - 1), sep = "")
            }
            cat("\n")
        }
        cat(strrep("=", sum(max_width)), "\n")
        cat(" ", paste(qq("@{tb} @{names(tb)} job@{ifelse(tb == 1, '', 's')}", collapse = FALSE), collapse = ", "), " within one week.\n", sep = "")
        cat(" ", paste(qq("@{tb_today} @{names(tb_today)} job@{ifelse(tb_today == 1, '', 's')}", collapse = FALSE), collapse = ", "), " in the last 24 hours.\n", sep = "")
        cat(" You can have more controls by `bjobs(status = ..., max = ..., filter = ...)`.\n")
        cat(" Use `brecent` to retrieve recent jobs from all status.\n")
        options(width = ow)

        return(invisible(df[ind, , drop = FALSE]))
    } else {
        if(!print) {
            return(NULL)
        }

        msg = paste0("status = '", paste(status, collapse = ", "), "'")
        if(!is.null(filter)) {
            msg = qq("@{msg}, filter = '@{filter}'")
        }
        qqcat("No job found (@{msg}).\n")
        cat("\n")
        cat(strrep("=", 78), "\n")
        cat(" ", paste(qq("@{tb} @{names(tb)} job@{ifelse(tb == 1, '', 's')}", collapse = FALSE), collapse = ", "), " within one week.\n", sep = "")
        cat(" ", paste(qq("@{tb_today} @{names(tb_today)} job@{ifelse(tb_today == 1, '', 's')}", collapse = FALSE), collapse = ", "), " in the last 24 hours.\n", sep = "")
        cat(" You can have more controls by `bjobs(status = ..., max = ..., filter = ...)`.\n")
        cat(" Use `brecent` to retrieve recent jobs from all status.\n")
        return(invisible(NULL))
    }
}
class(bjobs) = "bjobs"

#' @param fields Supported output fields, check \url{https://www.ibm.com/docs/en/spectrum-lsf/10.1.0?topic=information-customize-job-output}.
#' @details
#' `bjobs_raw()` returns the table from the original `bsubs -a -o '...'` call.
#' @rdname bjobs
bjobs_raw = function(fields = "jobid stat job_name queue") {

    if(length(fields) == 1) {
        fields = paste(fields, collapse = " ")
    }

    cmd = qq("bjobs -a -o '@{fields} delimiter=\",\"' 2>&1")
    ln = run_cmd(cmd, print = FALSE)
    
    # job done or exit
    if(length(ln) == 1) {
        cat(ln, "\n")
        return(invisible(NULL))
    }

    read.csv(textConnection(paste(ln, collapse = "\n")), stringsAsFactors = FALSE)
}

#' Clear job history
#'
#' @details
#' It sets a timestamp to only show jobs after it.
#' @export
bjobs_reset_timestamp = function() {
    bsub_opt$history_timestamp = Sys.time()
}


format_summary_table = function(df) {
    df2 = df[, c("JOBID", "STAT", "JOB_NAME", "RECENT","SUBMIT_TIME", "TIME_PASSED", "TIME_LEFT", "SLOTS", "MEM", "MAX_MEM")]

    df2$TIME_PASSED = format_difftime(df2$TIME_PASSED)
    df2$TIME_LEFT = format_difftime(df2$TIME_LEFT)
    df2$MAX_MEM = format_mem(df2$MAX_MEM)
    df2$MEM = format_mem(df2$MEM)

    if(all(df2$RECENT == 1)) {
        df2$RECENT = NULL
    }

    l = nchar(df2$JOB_NAME) > 50
    if(any(l)) {
        foo = substr(df2$JOB_NAME[l], 1, 48)
        foo = paste(foo, "..", sep = "")
        df2$JOB_NAME[l] = foo
    }

    return(df2)
}


#' Summary of jobs
#'
#' @param x A `bjobs` object.
#' @param ... other arguments.
#' 
#' @export
print.bjobs = function(x, ...) {
    x()
}


#' @rdname bjobs
#' @export
#' @examples
#' \dontrun{
#' brecent  # this is the same as `brecent()`
#' brecent() # last 20 jobs (from all status)
#' brecent(max = 50) # last 50 jobs
#' brecent(filter = "example") # last 20 jobs with name ".*example.*"
#' }
brecent = function(max = 20, filter = NULL) {
    bjobs(status = "all", max = max, filter = filter)
}
class(brecent) = "bjobs"

#' @rdname bjobs
#' @export
#' @examples
#' \dontrun{
#' bjobs_running  # this is the same as `bjobs_running()`
#' bjobs_running() # all running jobs
#' bjobs_running(max = 50) # last 50 running jobs
#' bjobs_running(filter = "example") # running jobs with name ".*example.*"
#' }
bjobs_running = function(max = Inf, filter = NULL) {
    bjobs(status = "RUN", max = max, filter = filter)
}
class(bjobs_running) = "bjobs"

#' @rdname bjobs
#' @export
#' @examples
#' \dontrun{
#' bjobs_pending  # this is the same as `bjobs_pending()`
#' bjobs_pending() # all pending jobs
#' bjobs_pending(max = 50) # last 50 pending jobs
#' bjobs_pending(filter = "example") # pending jobs with name ".*example.*"
#' }
bjobs_pending = function(max = Inf, filter = NULL) {
    bjobs(status = "PEND", max = max, filter = filter)
}
class(bjobs_pending) = "bjobs"

#' @rdname bjobs
#' @export
#' @examples
#' \dontrun{
#' bjobs_done  # this is the same as `bjobs_done()`
#' bjobs_done() # all done jobs
#' bjobs_done(max = 50) # last 50 done jobs
#' bjobs_done(filter = "example") # done jobs with name ".*example.*"
#' }
bjobs_done = function(max = Inf, filter = NULL) {
    bjobs(status = "DONE", max = max, filter = filter)
}
class(bjobs_done) = "bjobs"

#' @rdname bjobs
#' @export
#' @examples
#' \dontrun{
#' bjobs_exit  # this is the same as `bjobs_exit()`
#' bjobs_exit() # all exit jobs
#' bjobs_exit(max = 50) # last 50 exit jobs
#' bjobs_exit(filter = "example") # exit jobs with name ".*example.*"
#' }
bjobs_exit = function(max = Inf, filter = NULL) {
    bjobs(status = "EXIT", max = max, filter = filter)
}
class(bjobs_exit) = "bjobs"



