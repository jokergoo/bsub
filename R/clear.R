# == title
# Clear temporary dir
#
# == param
# -ask Whether promote.
#
# == details
# The temporary files might be used by the running/pending jobs. Deleting them might affect some of the jobs.
# You better delete them after all jobs are done.
#
# == value
# No value is returned.
#
# == example
# \dontrun{
# clear_temp_dir()
# }
clear_temp_dir = function(ask = TRUE) {
    files = list.files(bsub_opt$temp_dir, full.names = TRUE)
    if(length(files) == 0) {
        if(ask) qqcat("'@{bsub_opt$temp_dir}' is clear.\n")
        return(invisible(NULL))
    }
    if(!ask) {
        if(length(files)) {
            file.remove(files)
            return(invisible(NULL))
        }
    }

    file_types = gsub("^.*\\.([^.]+)$", "\\1", files)
    tb = table(file_types)
    job_tb = bjobs(status = "all", print = FALSE)
    if(any(job_tb$JOB_STAT %in% c("RUN", "PEND"))) {
        cat("There are still running/pending jobs. Deleting the temporary files might affect some of the jobs.\n")
    }
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
# Check whether there are dump files
#
# == param
# -print Whether to print messages.
#
# == details
# For the failed jobs, LSF cluster might generate a core dump file and R might generate a .RDataTmp file.
#
# Note if you manually set working directory in your R code/script, the R dump file can be not caught.
#
# == value
# A vector of file names.
#
# == example
# \dontrun{
# check_dump_files()
# }
check_dump_files = function(print = TRUE) {
    job_tb = bjobs(status = "all", print = FALSE)
    wd = job_tb$EXEC_CWD
    wd = wd[wd != "-"]
    wd = unique(wd)

    dump_files = NULL
    for(w in wd) {
        if(print) {
            qqcat("checking @{w}\n")
        }
        dump_files = c(dump_files, list.files(path = w, pattern = "^core.\\d$", all.files = TRUE, full.names = TRUE))
        dump_files = c(dump_files, list.files(path = w, pattern = "^\\.RDataTmp", all.files = TRUE, full.names = TRUE))
    }

    if(print) {
        if(length(dump_files)) {
            qqcat("found @{length(dump_files)} dump files:\n")
            qqcat("  @{dump_files}\n")
        } else {
            cat("no dump file found\n")
        }
    }

    return(invisible(dump_files))
}

