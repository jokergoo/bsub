
# == title
# Log for the running/finished/failed job
#
# == param
# -job_id The job id. It can be a single job or a vector of job ids.
# -print Whether print the log message.
# -n_line Number of last lines for each job to show when multiple jobs are queried.
# 
# == value
# The log message as a vector.
#
# == example
# \dontrun{
# # a single job
# job_id = 1234567  # job ids can be get from `bjobs`
# job_log(job_id)
# # multiple jobs
# job_id = c(10000000, 10000001, 10000002) 
# job_log(job_id)  # by  default last 10 lines for each job are printed
# job_log(job_id, n_line = 20) # print last 20 lines for each job
# # logs for all running jobs
# job_log()
# }
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

    job_id = as.numeric(job_id)

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

    if(job_queue == "interactive") {
        txt = qq("Job @{job_id} is an interactive job and has no log.")
        if(print) cat(txt, sep = "\n")
        return(invisible(txt))
    }

    tb_subset = tb[tb$JOB_NAME == job_name & tb$EXEC_CWD == job_wd, , drop = FALSE]
    nrr = nrow(tb_subset)
    if(nrr > 1) {
        tb_subset = tb_subset[order(tb_subset$JOBID), , drop = FALSE]
        if(tb_subset$JOBID[nrr] != job_id) {
            txt = qq("Log file for job @{job_id} has been overwriten by job @{tb_subset$JOBID[nrr]} which is the newest job with the same job name '@{tb_subset$JOB_NAME[nrr]}'.")
            txt = strwrap(txt)
            if(print) cat(txt, sep = "\n")
            return(invisible(txt))
        }
    }

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
        txt = qq("Job (@{job_id} is still pending.")
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
            file = list.files(path = job_temp_dir, pattern = paste0("\\.", job_id, "\\.out"), full.names = TRUE)

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

on_submission_node = function() {
    Sys.info()["nodename"] %in% bsub_opt$submission_node
}

convert_to_POSIXlt = function(x) {

    if(is.null(bsub_opt$parse_time)) {

        if(any(grepl("^\\w+\\s+\\d+\\s+\\d+:\\d+$", x))) { # Dec 1 18:00
            t = as.POSIXlt(x, format = "%b %d %H:%M")
        } else if(any(grepl("^\\w+\\s+\\d+\\s+\\d+:\\d+:\\d+$", x))) { # Dec 1 18:00:00
            t = as.POSIXlt(x, format = "%b %d %H:%M:%S")
        } else {                                        # Dec 1 18:00:00 2019
            t = as.POSIXlt(x, format = "%b %d %H:%M:%S %Y")
        }
    } else {
        t = bsub_opt$parse_time(x)
    }

    if(any(is.na(t) & x != "-")) {
        stop_wrap(qq("Cannot convert time string (e.g. '@{x[which(is.na(t))[1]]}'') to a `POSIXlt` object. Please set a proper parsing function for `bsub_opt$parse_time`. See ?bsub_opt for more details."))
    }

    if(inherits(t, "POSIXct")) t = as.POSIXlt(t)

    current_t = as.POSIXlt(Sys.time())
    l = t$year > current_t$year
    l[is.na(l)] = FALSE
    if(any(l)) {
        t[l]$year = t[l]$year - 1
    }

    return(t)
}

# == title
# Summary of jobs
# 
# == param
# -status Status of the jobs. Use "all" for all jobs.
# -max Maximal number of recent jobs.
# -filter Regular expression to filter on job names.
# -print Whether to print the table.
#
# == details
# There is an additional column "RECENT" which is the order
# for the job with the same name. 1 means the most recent job.
#
# You can directly type ``bjobs`` without parentheses which runs `bjobs` with defaults.
#
# == value
# A data frame with selected job summaries.
#
# == seealso
# - `brecent` shows the most recent.
# - `bjobs_done` shows the "DONE" jobs.
# - `bjobs_exit` shows the "EXIT" jobs.
# - `bjobs_pending` shows the "PEND" jobs.
# - `bjobs_running` shows the "RUN" jobs.
#
# == example
# \dontrun{
# bjobs # this is the same as bjobs()
# bjobs() # all running and pending jobs
# bjobs(status = "all") # all jobs
# bjobs(status = "RUN") # all running jobs, you can also use `bjobs_running`
# bjobs(status = "PEND") # all pending jobs, you can also use `bjobs_pending`
# bjobs(status = "DONE") # all done jobs, you can also use `bjobs_done`
# bjobs(status = "EXIT") # all exit jobs, you can also use `bjobs_exit`
# bjobs(status = "all", max = 20) # last 20 jobs
# bjobs(status = "DONE", filter = "example") # done jobs with name '.*example.*'
# }
bjobs = function(status = c("RUN", "PEND"), max = Inf, filter = NULL, print = TRUE) {


    cmd = "bjobs -a -o 'jobid stat job_name queue submit_time start_time finish_time slots mem max_mem dependency exec_cwd delimiter=\",\"' 2>&1"
    ln = run_cmd(cmd, print = FALSE)
    
    # job done or exit
    if(length(ln) == 1) {
        cat(ln, "\n")
        return(invisible(NULL))
    }

    df = read.csv(textConnection(paste(ln, collapse = "\n")), stringsAsFactors = FALSE)
    df$STAT = factor(df$STAT)
    df$SUBMIT_TIME = convert_to_POSIXlt(df$SUBMIT_TIME)
    df$START_TIME = convert_to_POSIXlt(df$START_TIME)
    df$FINISH_TIME = convert_to_POSIXlt(df$FINISH_TIME)
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

convert_to_byte = function(x) {
    num = as.numeric(gsub("\\D", "", x))
    v = ifelse(grepl("K", x), num*1024, ifelse(grepl("M", x), num*1024^2, ifelse(grepl("G", x), num*1024^3, x)))
    suppressWarnings(as.numeric(v))
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

format_mem = function(x) {
    gsub(" (.)bytes", "\\1b", x)
}

format_difftime = function(x, add_unit = FALSE) {
    units(x) = "hours"
    t = as.numeric(x)

    hour = floor(t)
    min = floor((t - hour)*60)
    
    l = is.na(x)
    if(add_unit) {
        txt = paste0(hour, "h", ifelse(min < 10, paste0("0", min), min), "m")
    } else {
        txt = paste0(hour, ":", ifelse(min < 10, paste0("0", min), min))
    }
    txt[l] = "-"
    txt[t == 0] = "-"
    txt
}


# == title
# Kill jobs
#
# == param
# -job_id A vector of job ids.
# -filter Regular expression to filter on job names (only the running and pending jobs).
#
# == value
# No value is returned.
#
# == example
# \dontrun{
# job_id = c(10000000, 10000001, 10000002)  # job ids can be get from `bjobs`
# bkill(job_id)
# # kill all jobs (running and pending) of which the names contain "example"
# bkill(filter = "example") 
# }
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

# == title
# Run command on submission node
#
# == param
# -cmd A single-line command.
# -print Whether to print output from the command.
#
# == details
# If current node is not the submission node, the command is executed via ssh.
#
# == value
# The output of the command.
#
# == example
# \dontrun{
# # run pwd on remote node
# run_cmd("pwd")
# }
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

# == title
# Summary of jobs
#
# == param
# -x a ``bjobs`` class object.
# -... other arguments.
#
# == value
# No value is returned.
print.bjobs = function(x, ...) {
    x()
}

# == title
# Test whether the jobs are finished
#
# == param
# -job_name A vector of job names.
# -output_dir Output dir.
#
# == details
# It tests whether the ".done" flag files exist
#
# == value
# A logical scalar.
is_job_finished = function(job_name, output_dir = bsub_opt$output_dir) {
    sapply(job_name, function(x) {
        flag_file = qq("@{output_dir}/@{x}.done")
        file.exists(flag_file)
    })
}


# == title
# Wait until all jobs are finished
#
# == param
# -job_name A vector of job names.
# -output_dir Output dir.
# -wait Seconds to wait.
#
# == value
# No value is returned.
wait_jobs = function(job_name, output_dir = bsub_opt$output_dir, wait = 30) {
    while(1) {
        finished = is_job_finished(job_name, output_dir)
        if(!all(finished)) {
            job_name = job_name[!finished]
            nj = length(job_name)
            message(qq("still @{nj} job@{ifelse(nj == 1, ' is', 's are')} not finished, wait for @{wait}s."))
            Sys.sleep(wait)
        } else {
            break
        }
    }
    return(invisible(NULL))
}


# == title
# Recent jobs from all status
#
# == param
# -max Maximal number of recent jobs.
# -filter Regular expression to filter on job names.
#
# == details
# You can directly type ``brecent`` without parentheses which runs `brecent` with defaults.
# 
# == value
# The same output format as `bjobs`.
#
# == example
# \dontrun{
# brecent  # this is the same as `brecent()`
# brecent() # last 20 jobs (from all status)
# brecent(max = 50) # last 50 jobs
# brecent(filter = "example") # last 20 jobs with name ".*example.*"
# }
brecent = function(max = 20, filter = NULL) {
    bjobs(status = "all", max = max, filter = filter)
}
class(brecent) = "bjobs"

# == title
# Running jobs
#
# == param
# -max Maximal number of jobs.
# -filter Regular expression to filter on job names.
#
# == details
# You can directly type ``bjobs_running`` without parentheses which runs `bjobs_running` with defaults.
#
# == value
# The same output format as `bjobs`.
#
# == example
# \dontrun{
# bjobs_running  # this is the same as `bjobs_running()`
# bjobs_running() # all running jobs
# bjobs_running(max = 50) # last 50 running jobs
# bjobs_running(filter = "example") # running jobs with name ".*example.*"
# }
bjobs_running = function(max = Inf, filter = NULL) {
    bjobs(status = "RUN", max = max, filter = filter)
}
class(bjobs_running) = "bjobs"

# == title
# Pending jobs
#
# == param
# -max Maximal number of jobs.
# -filter Regular expression to filter on job names.
#
# == details
# You can directly type ``bjobs_pending`` without parentheses which runs `bjobs_pending` with defaults.
#
# == value
# The same output format as `bjobs`.
#
# == example
# \dontrun{
# bjobs_pending  # this is the same as `bjobs_pending()`
# bjobs_pending() # all pending jobs
# bjobs_pending(max = 50) # last 50 pending jobs
# bjobs_pending(filter = "example") # pending jobs with name ".*example.*"
# }
bjobs_pending = function(max = Inf, filter = NULL) {
    bjobs(status = "PEND", max = max, filter = filter)
}
class(bjobs_pending) = "bjobs"

# == title
# Finished jobs
#
# == param
# -max Maximal number of jobs.
# -filter Regular expression to filter on job names.
#
# == details
# You can directly type ``bjobs_done`` without parentheses which runs `bjobs_done` with defaults.
#
# == value
# The same output format as `bjobs`.
#
# == example
# \dontrun{
# bjobs_done  # this is the same as `bjobs_done()`
# bjobs_done() # all done jobs
# bjobs_done(max = 50) # last 50 done jobs
# bjobs_done(filter = "example") # done jobs with name ".*example.*"
# }
bjobs_done = function(max = Inf, filter = NULL) {
    bjobs(status = "DONE", max = max, filter = filter)
}
class(bjobs_done) = "bjobs"

# == title
# Failed jobs
#
# == param
# -max Maximal number of jobs.
# -filter Regular expression to filter on job names.
#
# == details
# You can directly type ``bjobs_exit`` without parentheses which runs `bjobs_exit` with defaults.
#
# == value
# The same output format as `bjobs`.
#
# == example
# \dontrun{
# bjobs_exit  # this is the same as `bjobs_exit()`
# bjobs_exit() # all exit jobs
# bjobs_exit(max = 50) # last 50 exit jobs
# bjobs_exit(filter = "example") # exit jobs with name ".*example.*"
# }
bjobs_exit = function(max = Inf, filter = NULL) {
    bjobs(status = "EXIT", max = max, filter = filter)
}
class(bjobs_exit) = "bjobs"


# == title
# Job status by name
#
# == param
# -job_name Job name.
# -output_dir The output dir.
#
# == value
# If the job is finished, it returns DONE/EXIT/MISSING. If the job is running or pending, it returns the corresponding
# status. If there are multiple jobs with the same name running or pending, it returns a vector.
# 
# == example
# \dontrun{
# job_status_by_name("example")
# }
job_status_by_name = function(job_name, output_dir = bsub_opt$output_dir) {

    ln = run_cmd(qq("bjobs -J @{job_name} 2>&1"), print = FALSE)

    # job done or exit
    if(length(ln) == 1) {
        flag_file = qq("@{output_dir}/@{job_name}.done")
        out_file = qq("@{output_dir}/@{job_name}.out")
        if(file.exists(flag_file)) {
            return("DONE")
        } else if(file.exists(out_file)) {
            return("EXIT")
        } else {
            return("MISSING")
        }
    }

    lt = strsplit(ln, "\\s+")

    lt = lt[-1]
    sapply(lt, "[", 3)
}

# == title
# Job status by id
#
# == param
# -job_id The job id.
#
# == value
# If the job has been deleted from the database, it returns MISSING.
# 
# == example
# \dontrun{
# job_id = 1234567  # job ids can be get from `bjobs`
# job_status_by_id(job_id)
# }
job_status_by_id = function(job_id) {

    ln = run_cmd(qq("bjobs -o \"jobid user stat\" @{job_id} 2>&1"), print = FALSE)

    if(length(ln) == 1) {
        return("MISSING")
    }

    lt = strsplit(ln, "\\s+")

    lt = lt[-1]
    sapply(lt, "[", 3)
}

# == title
# A browser-based interactive job monitor 
#
# == details
# The monitor is implemented as a shiny app.
#
# == value
# No value is returned.
#
# == example
# \dontrun{
# # simply run:
# monitor
# # or
# monitor()
# }
monitor = function() {

    if(!requireNamespace("shiny", quietly = TRUE)) {
        stop_wrap("You need to install 'shiny' package to use the monitor.")
    }
    if(!requireNamespace("DT", quietly = TRUE)) {
        stop_wrap("You need to install 'DT' package to use the monitor.")
    }
    if(!requireNamespace("shinyjqui", quietly = TRUE)) {
        stop_wrap("You need to install 'shinyjqui' package to use the monitor.")
    }
    if(!requireNamespace("ggplot2", quietly = TRUE)) {
        stop_wrap("You need to install 'ggplot2' package to use the monitor.")
    }
    
    if(!on_submission_node()) {
        ssh_validate()
    }

    if(identical(topenv(), asNamespace("bsub"))) {
        if(bsub_opt$verbose) cat("run job monitor from the package.\n")
        suppressPackageStartupMessages(shiny::runApp(system.file("app", package = "bsub")))
    } else if(grepl("odcf", Sys.info()["nodename"])) {
        if(bsub_opt$verbose) cat("run job monitor from odcf node.\n")
        suppressPackageStartupMessages(shiny::runApp("/desktop-home/guz/project/development/bsub/inst/app"))
    } else if(grepl("w610", Sys.info()["nodename"])) {
        if(bsub_opt$verbose) cat("run job monitor from w610 node.\n")
        suppressPackageStartupMessages(shiny::runApp("~/project/development/bsub/inst/app"))
    } else {
        if(bsub_opt$verbose) cat("run job monitor from local laptop.\n")
        suppressPackageStartupMessages(shiny::runApp("~/project/development/bsub/inst/app"))
    }
}

class(monitor) = "bjobs"


STAT_COL = structure(names = c("RUN", "PEND", "DONE", "EXIT", "Others"), c("blue", "purple", "black", "red", "#EEEEEE"))

# == title
# Barplot of number of jobs
# 
# == param
# -status Status of the jobs. Use "all" for all jobs.
# -filter Regular expression to filter on job names.
# -df Internally used.
#
# == details
# It draws barplots of number of jobs per day.
#
# == value
# A ``ggplot2`` object.
bjobs_barplot = function(status = c("RUN", "EXIT", "PEND", "DONE"), filter = NULL, df = NULL) {
    if(is.null(df)) {
        df = bjobs(status = status, filter = filter, print = FALSE)
    } else {
        df = df[df$STAT %in% status , , drop = FALSE]
    }
    df$STAT = as.character(df$STAT)
    df$STAT[!df$STAT %in% status] = "Others"
    df$STAT = factor(df$STAT, levels = intersect(c(status, "Others"), as.character(df$STAT)))

    if(nrow(df) == 0) {
        plot(NULL, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, ann = FALSE)
        text(0.5, 0.5, "No jobs found.")
        return(invisible(NULL))
    }

    suppressWarnings(p <- ggplot2::ggplot(df, ggplot2::aes(x = as.Date(df$SUBMIT_TIME), fill = df$STAT)) + ggplot2::geom_bar(position=ggplot2::position_dodge()) +
        ggplot2::xlab("Submitted time") + ggplot2::ylab("Number of jobs") + ggplot2::labs(fill = "Status")
    )
    p = p + ggplot2::scale_fill_manual(breaks = names(STAT_COL), values = STAT_COL)
    tb = table(df$STAT)
    tb =  tb[tb > 0]
    p = p + ggplot2::ggtitle(paste0(paste(qq("@{tb} @{names(tb)} job@{ifelse(tb == 1, '', 's')}", collapse = FALSE), collapse = ", "), " within one week"))
    print(p)
}

class(bjobs_barplot) = "bjobs"

# == title
# Timeline of jobs
# 
# == param
# -status Status of the jobs. Use "all" for all jobs.
# -filter Regular expression to filter on job names.
# -df Internally used.
#
# == details
# It draws segments of duration of jobs. In the plot, each segment represents
# a job and the width of the segment correspond to its duration.
#
# == value
# No value is returned.
#
bjobs_timeline = function(status = c("RUN", "EXIT", "PEND", "DONE"), filter = NULL, df = NULL) {
    if(is.null(df)) {
        df = bjobs(status = status, filter = filter, print = FALSE)
    } else {
        df = df[df$STAT %in% status, , drop = FALSE]
    }
    df$STAT = as.character(df$STAT)
    df$STAT[!df$STAT %in% status] = "Others"
    df$STAT = factor(df$STAT, levels = intersect(c(status, "Others"), as.character(df$STAT)))
    
    if(nrow(df) == 0) {
        plot(NULL, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, ann = FALSE)
        text(0.5, 0.5, "No jobs found.")
        return(invisible(NULL))
    }

    x1 = as.numeric(df$START_TIME)
    x2 = x1 + as.numeric(df$TIME_PASSED)
    now = as.numeric(Sys.time())
    x2[is.na(x2)] = now
    y = runif(nrow(df))

    xlim = c(min(x1, na.rm = TRUE), max(x2, na.rm = TRUE))
    plot(NULL, xlim = xlim, ylim = c(0, 1), axes = FALSE, ann = FALSE)
    segments(x1, y, x2, y, col = STAT_COL[as.character(df$STAT)])
    at = unique(as.Date(c(df$START_TIME, df$FINISH_TIME)))
    labels = as.character(at)
    at = as.numeric(as.POSIXlt(at))
    l = at >= xlim[1] & at <= xlim[2]
    at = at[l]
    labels = labels[l]
    at = c(at, now)
    labels = c(labels, "now")
    box()
    axis(side = 1, at = at, labels = labels)
    op = par("xpd")
    par(xpd = NA)
    col = STAT_COL[intersect(names(STAT_COL), as.character(df$STAT))]
    legend(x = mean(par("usr")[1:2]), y = mean(par("usr")[4]), legend = names(col), 
        col = col, lty = 1, ncol = length(col), xjust = 0.5, yjust = 0, cex = 0.6)

    tb = table(df$STAT)
    tb =  tb[tb > 0]
    title(paste0(paste(qq("@{tb} @{names(tb)} job@{ifelse(tb == 1, '', 's')}", collapse = FALSE), collapse = ", "), " within one week"))

    par(xpd = op)
}

class(bjobs_timeline) = "bjobs"

