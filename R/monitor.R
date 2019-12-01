
# == title
# Log for the running/finished/failed job
#
# == param
# -job_id the job id
# 
job_log = function(job_id) {

    job_id = as.character(job_id)

    if(job_status_by_id(job_id) == "PEND") {
        qqcat("Not yet started.\n")
        return(invisible(NULL))
    }

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

    msg = ""
    # check running jobs
    if(on_submission_node()) {
    	file = list.files(path = job_temp_dir,
    		pattern = paste0("\\.", job_id, "\\.out"),
    		full.names = TRUE)

    	if(length(file) == 0) {
    		msg = qq("No log file for job: @{job_id}.")
    	} else if(length(file) > 1) {
            msg = qq("More than one of jobs: @{job_id} found.")
        } else {
            cat(readLines(file, warn = FALSE), sep = "\n")
            qqcat("**** job (@{job_id}) is still running. ****\n")
            return(invisible(NULL))
        }
    } else {
        ln = ssh_exec(qq("ls @{job_temp_dir}/*.@{job_id}.out"))

        if(length(ln) == 0) {
            msg = qq("No log file for job: @{job_id}.")
        } else if(length(ln) > 1) {
            msg = qq("More than one of jobs: @{job_id} found.")
        } else {
            ln2 = ssh_exec(qq("cat @{ln}"))
            cat(ln2, sep = "\n")
            qqcat("**** job (@{job_id}) is still running. ****\n")
            return(invisible(NULL))
        }
    }

    if(msg == "") {
        cat("No message.\n")
        return(invisible(NULL))
    }

    # check finished jobs
    if(file.exists("~/.bsub_history.rds")) {
        job_history = readRDS("~/.bsub_history.rds")
        if(job_id %in% job_history$job_id) {
            ind = which(job_history$job_id == job_id)
            file = qq("@{job_history$output_dir[ind]}/@{job_history$job_name[ind]}.out")
            cat(readLines(file, warn = FALSE), sep = "\n")
            return(invisible(NULL))
        }
    } else {
        stop(msg)
    }
}

on_submission_node = function() {
    Sys.info()["nodename"] %in% bsub_opt$submission_node
}

# == title
# Summary of jobs
#
# == param
# -stat status of the jobs
#
# == details
# Please use `bjobs` instead.
#
bu = function(stat = c("RUN", "PEND")) {

    ln = run_cmd("bu 2>&1", print = FALSE)
    
    # job done or exit
    if(length(ln) == 1) {
        cat(ln, "\n")
        return(invisible(NULL))
    }

    header = strsplit(ln[1], "\\s+")[[1]]
    pos = lapply(header, function(x) gregexpr(x, ln[1])[[1]])
    offset = unlist(pos)
    nf = length(offset)
    width = numeric(nf)
    width[-nf] = offset[-1] - offset[-nf]
    width[nf] = nchar(ln[1]) - offset[nf] + 1

    lt = lapply(1:nf, function(i) {
    	x = substr(ln[-1], offset[i], offset[i] + width[i] - 1)
    	gsub("\\s+$", "", x)
    })
    names(lt) = header
    df = do.call(cbind, lt)
    df = as.data.frame(df)

    df$SUBMIT_TIME = as.POSIXct(df$SUBMIT_TIME, format = "%b %d %H:%M:%S %Y")
    df$START_TIME = as.POSIXct(df$START_TIME, format = "%b %d %H:%M:%S %Y")
    df$FINISH_TIME = as.POSIXct(df$FINISH_TIME, format = "%b %d %H:%M:%S %Y")
    df$TIME_PASSED = Sys.time() - df$START_TIME
    # df$TIME_LEFT = df$FINISH_TIME - Sys.time()

    df$JOBID = as.numeric(df$JOBID)
    df = df[order(df$JOBID), , drop = FALSE]
    tb = table(df$STAT)
    df = df[df$STAT %in% stat, , drop = FALSE]
    if(nrow(df)) {
        df2 = df[, c("JOBID", "STAT", "JOB_NAME", "TIME_LEFT", "SLOTS", "MAX_MEM")]

        max_width = pmax(apply(df2, 2, function(x) max(nchar(x)+1)),
        	             nchar(colnames(df2)) + 1)
        ow = getOption("width")
        options(width = sum(max_width) + 10)
        print(df2, row.names = FALSE, right = FALSE)
        cat(strrep("=", sum(max_width)), "\n")
        cat(" ", paste(qq("@{tb} @{names(tb)} jobs", collapse = FALSE), collapse = ", "), ".\n", sep = "")
        options(width = ow)
    } else {
        cat("No job.\n")
    }

    cat("!!! Please use `bjobs()` instead. !!!\n")
    return(invisible(df))
}

class(bu) = "bjobs"

# == title
# Summary of jobs
# 
# == param
# -status Status of the jobs
# -max Maximal number of recent jobs
# -filter Regular expression to filter on job names
#
# == details
# There is an additional column "RECENT" which is the order
# for the job with the same name. 1 means the most recent job.
#
# == value
# A data frame with job summaries.
#
bjobs = function(status = c("RUN", "PEND"), max = Inf, filter = NULL) {

    cmd = "bjobs -a -o 'jobid stat job_name submit_time start_time finish_time slots mem max_mem delimiter=\",\"' 2>&1"
    ln = run_cmd(cmd, print = FALSE)
    
    # job done or exit
    if(length(ln) == 1) {
        cat(ln, "\n")
        return(invisible(NULL))
    }

    df = read.csv(textConnection(paste(ln, collapse = "\n")))
    df$SUBMIT_TIME = as.POSIXct(df$SUBMIT_TIME, format = "%b %d %H:%M:%S %Y")
    df$START_TIME = as.POSIXct(df$START_TIME, format = "%b %d %H:%M:%S %Y")
    df$FINISH_TIME = as.POSIXct(df$FINISH_TIME, format = "%b %d %H:%M:%S %Y")
    df$TIME_PASSED = Sys.time() - df$START_TIME
    l = df$FINISH_TIME < Sys.time()
    l[is.na(l)] = FALSE  # finish time is unavailable
    df$TIME_PASSED[l] = df$FINISH_TIME[l] - df$START_TIME[l]

    df$TIME_LEFT = df$FINISH_TIME - Sys.time()
    l = df$FINISH_TIME < Sys.time()
    l[is.na(l)] = TRUE
    df$TIME_LEFT[l] = NA

    df$JOBID = as.numeric(df$JOBID)
    df = df[order(df$JOBID), , drop = FALSE]
    tb = table(df$STAT)
    df_return = df

    recent = unlist(unname(tapply(df$JOBID, df$JOB_NAME, function(x) {
        structure(order(-x), names = x)
    }, simplify = FALSE)))
    df$RECENT = recent[as.character(df$JOBID)]

    if(! "all" %in% status) {
        df = df[df$STAT %in% status, , drop = FALSE]
    }
    if(!is.null(filter)) {
        df = df[grepl(filter, df$JOB_NAME), , drop = FALSE]
    }
    if(nrow(df)) {
        ind = (nrow(df):1)[1:min(c(nrow(df), max))]
        df2 = df[sort(ind), c("JOBID", "STAT", "JOB_NAME", "RECENT", "TIME_PASSED", "TIME_LEFT", "SLOTS", "MEM", "MAX_MEM")]

        df2$TIME_PASSED = format_difftime(df2$TIME_PASSED)
        df2$TIME_LEFT = format_difftime(df2$TIME_LEFT)
        df2$MAX_MEM = format_mem(df2$MAX_MEM)
        df2$MEM = format_mem(df2$MEM)

        if(all(df2$RECENT == 1)) {
            df2$RECENT = NULL
        }

        max_width = pmax(apply(df2, 2, function(x) max(nchar(x)+1)),
                         nchar(colnames(df2)) + 1)
        ow = getOption("width")
        options(width = sum(max_width) + 10)
        print(df2, row.names = FALSE, right = FALSE)
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
        cat(" You can have more controls by `bjobs(status = ..., max = ..., filter = ...)`.\n")
        options(width = ow)
    } else {
        cat("No job found.\n")
    }
    return(invisible(df_return))
}

class(bjobs) = "bjobs"

format_mem = function(x) {
    gsub(" (.)bytes", "\\1b", x)
}

format_difftime = function(x) {
    units(x) = "hours"
    t = as.numeric(x)

    hour = floor(t)
    min = floor((t - hour)*60)
    
    l = is.na(x)
    txt = paste0(hour, ":", ifelse(min < 10, paste0("0", min), min))
    txt[l] = "-"
    txt[t == 0] = "-"
    txt
}

# == title
# Kill jobs
#
# == param
# -job_id A vector of job ids.
#
bkill = function(job_id) {

    cmd = qq("bkill @{paste(job_id, collapse = ' ')} 2>&1")

    run_cmd(cmd, print = TRUE)
}

# == title
# Run command on submission node
#
# == param
# -cmd A single-line command
# -print Whether print output from the command
#
# == value
# The output of the command
#
run_cmd = function(cmd, print = FALSE) {
    if(on_submission_node()) {
       con = pipe(cmd)
       ln = readLines(con)
       close(con)
    } else {
        ln = ssh_exec(cmd)
    }

    if(print) cat(c(ln, "\n"), sep = "\n")
    return(invisible(ln))
}

# == title
# Summary of jobs
#
# == param
# -x a ``bjobs`` class object
# -... other arguments
#
print.bjobs = function(x, ...) {
    x()
}

# == title
# Test whether the jobs are finished
#
# == param
# -job_name A vector of job names
# -output_dir Output dir
#
is_job_finished = function(job_name, output_dir = bsub_opt$output_dir) {
    sapply(job_name, function(x) {
        flag_file = qq("@{output_dir}/@{x}.flag")
        file.exists(flag_file)
    })
}


# == title
# Wait until all jobs are finished
#
# == param
# -job_name A vector of job names
# -output_dir Output dir
# -wait seconds to wait
#
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
# Clear temporary dir
#
# == param
# -ask whether promote
#
clear_temp_dir = function(ask = TRUE) {
    files = list.files(bsub_opt$temp_dir, full.names = TRUE)
    if(length(files) == 0) {
        if(ask) qqcat("'@{bsub_opt$temp_dir}' is clear.")
        return(invisible(NULL))
    }
    if(!ask) {
        file.remove(files)
        return(invisible(NULL))
    }

    file_types = gsub("^.*\\.([^.]+)$", "\\1", files)
    tb = table(file_types)
    cat(qq("There @{ifelse(length(files) > 1, 'are', 'is')} "), qq("@{tb} .@{names(tb)} file@{ifelse(tb > 1, 's', '')}, "), "delete all? [y|n|s] ", sep = "")
    while(1) {
        answer = readline()
        if(answer == "y" || answer == "Y") {
            file.remove(files)
            break
        } else if(answer == "n" || answer == "N") {
            break
        } else if(answer == "s" || answer == "S") {
            for(nm in names(tb)) {
                qqcat("Remove all @{tb[nm]} .@{nm} file@{ifelse(tb[nm] > 1, 's', '')}? [y|n] ")
                while(1) {
                    answer2 = readline()
                    if(answer2 == "y" || answer2 == "Y") {
                        file.remove(files[file_types == nm])
                        break
                    } else if(answer2 == "n" || answer2 == "N") {
                        break
                    } else {
                        qqcat("Remove all @{tb[nm]} .@{nm} file@{ifelse(tb[nm] > 1, 's', '')}? [y|n] ")
                    }
                }
                
            }
            break
        } else {
            cat(qq("There @{ifelse(length(files) > 1, 'are', 'is')} "), qq("@{tb} .@{names(tb)} file@{ifelse(tb > 1, 's', '')}, "), "delete all? [y|n|s] ", sep = "")
        }
    }
    return(invisible(NULL))
}


# == title
# Recent jobs
#
# == param
# -max Maximal number of recent jobs
# -filter Regular expression to filter on job names
#
# == details
# It is ``bjobs(status = "all", max = 20, filter = ...)``.
#
brecent = function(max = 20, filter = NULL) {
    bjobs(status = "all", max = max, filter = filter)
}

class(brecent) = "bjobs"


job_status_by_name = function(job_name, output_dir = bsub_opt$output_dir) {

    ln = run_cmd(qq("bjobs -J @{job_name} 2>&1"), print = FALSE)

    # job done or exit
    if(length(ln) == 1) {
        flag_file = qq("@{output_dir}/@{job_name}.flag")
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

job_status_by_id = function(job_id) {

    ln = run_cmd(qq("bjobs -o \"jobid user stat\" @{job_id} 2>&1"), print = FALSE)

    if(length(ln) == 1) {
        return("MISSING")
    }

    lt = strsplit(ln, "\\s+")

    lt = lt[-1]
    sapply(lt, "[", 3)
}
