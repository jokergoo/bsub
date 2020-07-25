
# == title
# Submit R code
#
# == param
# -code The code chunk, it should be embraced by ``\{`` ``\}``.
# -name If name is not specified, an internal name calculated by `digest::digest` on the chunk is automatically assigned. 
# -packages A character vector with package names that will be loaded before running the script. There is a special name ``_in_session_``
#     that loads all the packages loaded in current R session.
# -image A character vector of RData/rda files that will be loaded before running the script.
#       When ``image`` is set to ``TRUE``, all variables in ``.GlobalEnv`` will be saved
#       into a temporary file and all attached packages will be recorded. The temporary files
#       will be removed after the job is finished.
# -variables A character vector of variable names that will be loaded before running the script. There is a special name ``_all_functions_``
#       that saves all functions defined in the global environment.
# -working_dir The working directory.
# -hour Running time of the job.
# -memory Memory usage of the job. It is measured in GB.
# -core Number of cores.
# -R_version R version.
# -temp_dir Path of temporary folder where the temporary R/bash scripts will be put.
# -output_dir Path of output folder where the output/flag files will be put.
# -dependency A vector of job IDs that current job depends on.
# -enforce If a flag file for the job is found, whether to enforce to rerun the job.
# -local Run job locally (not submitting to the LSF cluster)?
# -script Path of a script where code chunks will be extracted and sent to the cluster.It is always used with ``start`` and ``end`` arguments.
# -start A numeric vector that contains line indices of the starting code chunk or a character vector
#        that contain regular expression to match the start of code chunks.
# -end Same setting as ``start``.
# -save_var Whether save the last variable in the code chunk? Later the variable
#    can be retrieved by `retrieve_var`.
# -sh_head Commands that are written as head of the sh script.
#
# == value
# Job ID.
#
# == seealso
# - `bsub_script` submits R scripts.
# - `bsub_cmd`submits shell commands.
#
# == example
# \dontrun{
# bsub_chunk(name = "example", memory = 10, hour = 10, core = 4,
# {
#     Sys.sleep(5)
# })
# }
bsub_chunk = function(code, 
    name = NULL,
    packages = bsub_opt$packages, 
    image = bsub_opt$image,
    variables = character(),
    working_dir = bsub_opt$working_dir,
    hour = 1, 
    memory = 1, 
    core = 1,
    R_version = bsub_opt$R_version,
    temp_dir = bsub_opt$temp_dir,
    output_dir = bsub_opt$output_dir,
    dependency = NULL,
    enforce = bsub_opt$enforce,
    local = bsub_opt$local,
    script = NULL,
    start = NULL,
    end = NULL,
    save_var = FALSE,
    sh_head = bsub_opt$sh_head) {
    
    if(bsub_opt$ignore) return(invisible(NULL))

    if(!is.null(script)) {
        script = readLines(script)
        if(is.null(start)) start = 1
        if(is.null(end)) end = length(script)
        if(is.character(start)) {
            start = grep(start, script)
            if(length(start) == 0) {
                stop(qq("cannot find start mark '@{start}'"))
            }
            if(length(start) > 1) {
                stop(qq("find more than one start marks '@{start}'"))
            }
        }
        if(is.character(end)) {
            end = grep(end, script)
            if(length(end) == 0) {
                stop(qq("cannot find end mark '@{end}'"))
            }
            if(length(end) > 1) {
                stop(qq("find more than one end marks '@{end}'"))
            }
        }
        if(end < start) {
            stop(qq("start line (#@{start}) should be no larger than the end line (#@{end})."))
        }
        code = script[start:end]
    } else {
        code = deparse(substitute(code))
    }
    code = paste(code, collapse = "\n")
    code = paste0(code, "\n")

    if(save_var) {
        code[1] = paste0("foo = ", code[1])
    }

    if(is.null(name)) {
        name = paste0("R_code_", digest::digest(code, "crc32"))
    } else {
        name = qq(name)
    }


    if(!file.exists(temp_dir)) {
        answer = readline(qq("create temp_dir: @{temp_dir}? [y|n] "))
        if(answer %in% c("y", "Y", "yes", "Yes", "YES")) {
            dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
        } else {
            stop_wrap(qq("not allowed to create @{temp_dir}."))
        }
    }
    if(!file.exists(output_dir)) {
        answer = readline(qq("create output_dir: @{output_dir}? [y|n] "))
        if(answer %in% c("y", "Y", "yes", "Yes", "YES")) {
            dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
        } else {
            stop_wrap(qq("not allowed to create @{output_dir}."))
        }
    }

    temp_dir = normalizePath(temp_dir)
    output_dir = normalizePath(output_dir)

    cat(cyan(qq("- job: '@{magenta(name)}' from a code chunk\n")))

    if(length(variables)) {
        if(!inherits(variables, "character")) {
            stop_wrap("`variables` must be a character vector (the names of the variables you want to import).")
        }
    }

    # if `image` is true, save all variables and all packages that are loaded
    if(identical(image, TRUE)) {
        variables = c(variables, ls(envir = .GlobalEnv, all.names = TRUE))
        packages = c(rev(names(sessionInfo()$otherPkgs)), packages)
        image = NULL
    }

    ## a special tag for variables
    if(length(variables)) {
        if(any(grepl("^_all_functions", variables))) {
            var_name = ls(envir = .GlobalEnv, all.names = TRUE)
            l = sapply(var_name, function(x) inherits(get(x, envir = .GlobalEnv), "function"))
            var_name = var_name[l]
            variables = c(variables, var_name)
        }
    }

    ## a special tag for package: _in_session_
    if(length(packages)) {
        if(any(grepl("^_in_session", packages))) {
            packages = c(packages, rev(names(sessionInfo()$otherPkgs)))
        }
    }

    head = "############## temporary R script ##############"
    tail = ""
    for(p in unique(packages)) {
        head = qq("@{head}\nlibrary(@{p})\n")
    }

    # if all image files do not exist, write current session to image[1]
    if(!is.null(image)) {
        l = file.exists(image)
        if(all(!l)) {
            message_wrap(qq("All the image files specified do not exist, save current workspace to @{image[1]}? [y/n]"), appendLF = FALSE)
            answer = readline()
            if(answer %in% c("y", "Y", "yes", "Yes")) {
                save.image(file = image[1])
                image = image[1]
            } else {
                stop("All image files do not exist.")
            }
        }

        for(f in image) {
            f = normalizePath(f)
            head = qq("@{head}\nload(\"@{f}\")\n")
        }
    }

    if(length(variables)) {
        save(list = unique(variables), envir = parent.frame(), file = qq("@{temp_dir}/@{name}_var.RData"))
        head = qq("@{head}\nload(\"@{temp_dir}/@{name}_var.RData\")\ninvisible(file.remove(\"@{temp_dir}/@{name}_var.RData\"))\n")
    }

    tmp = tempfile(paste0(name, "_"), fileext = ".R", tmpdir = temp_dir)
    if(bsub_opt$debug) {
        head = qq("@{head}\ncat(readLines(\"@{tmp}\"), sep = \"\\n\")\n\n")
    }
    head = qq("@{head}\ninvisible(file.remove(\"@{tmp}\"))\n\n")

    tail = c(tail, "invisible(NULL)\n")

    if(save_var) {
        tail = c(tail, qq("saveRDS(foo, file = '@{output_dir}/@{name}_returned_var.rds')"))
    }

    if(working_dir != "") {
        if(!file.exists(working_dir)) {
            stop(qq("'@{working_dir}'' does not exist."))
        }
        head = c(head, qq("setwd('@{working_dir}')\n\n"))
    }
    writeLines(c(head, code, tail), con = tmp)

    command = qq("@{bsub_opt$call_Rscript(R_version)} '@{tmp}';")
    
    bsub_submit(
        command = command, 
        hour = hour,
        memory = memory,
        core = core,
        name = name,
        output_dir = output_dir,
        temp_dir = temp_dir,
        dependency = dependency,
        enforce = enforce,
        local = local,
        sh_head = sh_head)
}

# == title
# Retrieve saved variable
#
# == param
# -name Job name.
# -output_dir The output dir set in `bsub_chunk`.
# -wait Seconds to wait.
#
# == details
# It retrieve the saved variable in `bsub_chunk` when ``save_rds = TRUE`` is set.
#
# == value
# The retrieved object.
#
# == example
# \dontrun{
# bsub_chunk(name = "example", save_var = TRUE,
# {
#     Sys.sleep(10)
#     1+1
# })
# retrieve_var("example")
# }
retrieve_var = function(name, output_dir = bsub_opt$output_dir, wait = 30) {
    rds_file = qq("@{output_dir}/@{name}_returned_var.rds")
    out_file = qq("@{output_dir}/@{name}.out")
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

    status = job_status_by_name(name, output_dir)
    if(status %in% c("RUN", "PEND")) {
        message(qq("job is running or pending, retry in @{wait} seconds."))
        Sys.sleep(wait)
        retrieve_var(name, output_dir = output_dir, wait = wait)
    } else if(status == "DONE") {
        retrieve_var(name, output_dir = output_dir)
    } else if(status == "MISSING") {
        stop(qq("cannot find the job '@{name}'."))
    } else {
        stop("job failed.")
    }
}

# == title
# Submit R script
#
# == param
# -script The R script.
# -argv A string of command-line arguments.
# -name If name is not specified, an internal name calculated by `digest::digest` is automatically assigned. 
# -hour Running time of the job.
# -memory Memory usage of the job. It is measured in GB.
# -core Number of cores.
# -R_version R version.
# -temp_dir Path of temporary folder where the temporary R/bash scripts will be put.
# -output_dir Path of output folder where the output/flag files will be put.
# -dependency A vector of job IDs that current job depends on.
# -enforce If a flag file for the job is found, whether to enforce to rerun the job.
# -local Run job locally (not submitting to the LSF cluster)?
# -sh_head Commands that are written as head of the sh script.
# -... Command-line arguments can also be specified as name-value pairs.
#
# == value
# Job ID.
#
# == seealso
# - `bsub_chunk` submits R code.
# - `bsub_cmd`submits shell commands.
#
# == example
# \dontrun{
# bsub_script("/path/of/foo.R", name = ..., memory = ..., core = ..., ...)
# # with command-line arguments
# bsub_script("/path/of/foo.R", argv = "--a 1 --b 3", ...)
# }
bsub_script = function(script, 
    argv = "", 
    name = NULL, 
    hour = 1, 
    memory = 1, 
    core = 1,
    R_version = bsub_opt$R_version,
    temp_dir = bsub_opt$temp_dir,
    output_dir = bsub_opt$output_dir,
    dependency = NULL,
    enforce = bsub_opt$enforce, 
    local = bsub_opt$local,
    sh_head = bsub_opt$sh_head,
    ...) {
    
    if(bsub_opt$ignore) return(invisible(NULL))

    script = normalizePath(script)
    
    argv = qq(argv)

    argv_list = list(...)
    if(length(argv_list)) {
        argv_nm = names(argv_list)
        if(!all(grepl("^\\.", argv_nm))) {
            stop("Other command-line parameters specified in ... should have prefix '.'.")
        }

        argv_list = sapply(argv_list, function(x) {
                x = paste(x, collapse = " ")
        })

        argv_nm = gsub("^\\.", "", argv_nm)
        argv2 = qq("--@{argv_nm} @{argv_list} ")
        if(argv == "") {
            argv = argv2
        } else {
            argv = paste0(argv, " ", argv2)
        }
    }

    if(is.null(name)) {
        name = paste0("R_script_", digest::digest(list(script, argv), "crc32"))
    } else {
        name = qq(name)
    }

    if(!file.exists(temp_dir)) {
        answer = readline(qq("create temp_dir: @{temp_dir}? [y|n] "))
        if(answer %in% c("y", "Y", "yes", "Yes", "YES")) {
            dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
        } else {
            stop_wrap(qq("not allowed to create @{temp_dir}."))
        }
    }
    if(!file.exists(output_dir)) {
        answer = readline(qq("create output_dir: @{output_dir}? [y|n] "))
        if(answer %in% c("y", "Y", "yes", "Yes", "YES")) {
            dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
        } else {
            stop_wrap(qq("not allowed to create @{output_dir}."))
        }
    }
    temp_dir = normalizePath(temp_dir)
    output_dir = normalizePath(output_dir)

    cat(cyan(qq("- job: '@{magenta(name)}' from script @{basename(script)}\n")))
    
    command = qq("@{bsub_opt$call_Rscript(R_version)} '@{script}' @{argv};")

    bsub_submit(
        command = command, 
        hour = hour,
        memory = memory,
        core = core,
        name = name,
        output_dir = output_dir,
        temp_dir = temp_dir,
        dependency = dependency,
        enforce = enforce,
        local = local,
        sh_head = sh_head)
}


# == title
# Submit shell commands
#
# == param
# -cmd A list of commands.
# -name If name is not specified, an internal name calculated by `digest::digest` is automatically assigned. 
# -hour Running time of the job.
# -memory Memory usage of the job. It is measured in GB.
# -core Number of cores.
# -temp_dir Path of temporary folder where the temporary R/bash scripts will be put.
# -output_dir Path of output folder where the output/flag files will be put.
# -dependency A vector of job IDs that current job depends on.
# -enforce If a flag file for the job is found, whether to enforce to rerun the job.
# -local Run job locally (not submitting to the LSF cluster)?
# -sh_head Commands that are written as head of the sh script.
# -... Command-line arguments can also be specified as name-value pairs.
#
# == value
# Job ID.
#
# == seealso
# - `bsub_chunk`submits R code.
# - `bsub_script` submits R scripts.
#
# == example
# \dontrun{
# bsub_cmd("samtools sort ...", name = ..., memory = ..., core = ..., ...)
# }
bsub_cmd = function(cmd, 
    name = NULL, 
    hour = 1, 
    memory = 1, 
    core = 1,
    temp_dir = bsub_opt$temp_dir,
    output_dir = bsub_opt$output_dir,
    dependency = NULL,
    enforce = bsub_opt$enforce, 
    local = bsub_opt$local,
    sh_head = bsub_opt$sh_head,
    ...) {
    
    if(bsub_opt$ignore) return(invisible(NULL))

    if(is.null(name)) {
        name = paste0("cmd_", digest::digest(cmd, "crc32"))
    } else {
        name = qq(name)
    }

    if(!file.exists(temp_dir)) {
        answer = readline(qq("create temp_dir: @{temp_dir}? [y|n] "))
        if(answer %in% c("y", "Y", "yes", "Yes", "YES")) {
            dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
        } else {
            stop_wrap(qq("not allowed to create @{temp_dir}."))
        }
    }
    if(!file.exists(output_dir)) {
        answer = readline(qq("create output_dir: @{output_dir}? [y|n] "))
        if(answer %in% c("y", "Y", "yes", "Yes", "YES")) {
            dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
        } else {
            stop_wrap(qq("not allowed to create @{output_dir}."))
        }
    }
    temp_dir = normalizePath(temp_dir)
    output_dir = normalizePath(output_dir)

    cat(cyan(qq("- job: '@{magenta(name)}' from a list of commands\n")))
    
    command = paste(cmd, collapse = "\n")

    bsub_submit(
        command = command, 
        hour = hour,
        memory = memory,
        core = core,
        name = name,
        output_dir = output_dir,
        temp_dir = temp_dir,
        dependency = dependency,
        enforce = enforce,
        local = local,
        sh_head = sh_head)
}

bsub_submit = function(command,
    hour,
    memory,
    core,
    name,
    output_dir,
    temp_dir,
    dependency = NULL,
    enforce = TRUE,
    local = FALSE,
    sh_head = "") {

    output_dir = normalizePath(output_dir)
    temp_dir = normalizePath(temp_dir)

    output = qq("@{output_dir}/@{name}.out")
    done = qq("@{output_dir}/@{name}.done")
    old_flag = qq("@{output_dir}/@{name}.flag")
    pend = qq("@{output_dir}/@{name}.pend")
    run = qq("@{output_dir}/@{name}.run")

    if(!enforce && (file.exists(done) || file.exists(old_flag))) {
        cat(cyan(qq("Job '@{name}' is already done, skip.\n")))
        return(invisible(NULL))
    }
    if(!enforce && file.exists(pend)) {
        # if the pending job is killed, the pend flag is still there
        if("PEND" %in% job_status_by_name(name, output_dir)) {
            cat(cyan(qq("Job '@{name}' is pending, skip.\n")))
            return(invisible(NULL))
        }
    }
    if(!enforce && file.exists(run)) {
        # if the running job is killed, the run flag is still there
        if("RUN" %in% job_status_by_name(name, output_dir)) {
            cat(cyan(qq("Job '@{name}' is running, skip.\n")))
            return(invisible(NULL))
        }
    }

    if(file.exists(done)) {
        file.remove(done);
    }
    if(file.exists(output)) {
        file.remove(output);
    }
    
    sh_file = tempfile(paste0(name, "_"), tmpdir = temp_dir, fileext = ".sh")
    con = file(sh_file, "w")

    writeLines("########## temporary bash script ###############", con)
    
    writeLines(qq("[ -f '@{pend}' ] && rm '@{pend}'\n"), con)  # remove the pend flag file
    writeLines(qq("touch '@{run}'\n"), con)  # add the running flag
    
    if(!identical(sh_head, "")) {
        writeLines(sh_head, con)
        writeLines("\n", con)
    }
    if(bsub_opt$debug) {
        writeLines(qq("cat '@{sh_file}'\n"), con)
    }
    writeLines(qq("rm '@{sh_file}'\n"), con)

    # wrap command with eval
    writeLines(command, con)

    # test the command, 
    writeLines("
if [ $? -ne 0 ]
then
    rm $run
    echo Exit code is not equal to zero. There is an error.
    exit 666
fi", con)

    writeLines(qq("rm '@{run}'\n"), con)
    writeLines(qq("touch '@{done}'\n"), con)
    close(con)

    if(local) {
        cmd = qq("bash @{sh_file}")
        cat(cmd, "\n")
        system(cmd)
        return(invisible(NULL))
    }

    system(qq("chmod 755 @{sh_file}"))

    cmd = bsub_opt$bsub_template(name, hour, memory, core, output, bsub_opt$group)
    if(length(dependency)) {
        dependency_str = paste( paste("done(", dependency, ")"), collapse = " && " )
        cmd = qq("@{cmd} -w '@{dependency_str}'")
    }
    cmd = qq("@{cmd} '@{sh_file}'")
    cat(silver(cmd), "\n")

    file.create(pend)
    txt = run_cmd(cmd, print = FALSE)

    job_id = gsub("^.*<(\\d+)>.*$", "\\1", txt)

    job_tb = data.frame(
            job_id = job_id,
            job_name = name,
            hour = hour,
            memory = memory,
            core = core,
            submit_time = Sys.time(),
            output_dir = output_dir,
            temp_dir = temp_dir,
            stringsAsFactors = FALSE
        )

    return(job_id)
}


message_wrap = function (..., appendLF = TRUE) {
    x = paste0(...)
    x = paste(strwrap(x), collapse = "\n")
    message(x, appendLF = appendLF)
}

stop_wrap = function (...) {
    x = paste0(...)
    x = paste(strwrap(x), collapse = "\n")
    stop(x, call. = FALSE)
}

# == title
# Submit a random job
#
# == param
# -name Job name.
# -... Pass to `bsub_chunk`.
#
# == details
# It only submits ``Sys.sleep(30)``.
#
# == value
# The job id.
#
# == example
# \dontrun{
# random_job()
# random_job(name = "test")
# }
random_job = function(name = paste0("R_random_job_", digest::digest(runif(1), "crc32")), ...) {
    bsub_chunk({
        Sys.sleep(30)
    }, name = name, ...)
}


