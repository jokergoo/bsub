


# == title
# Parameters for bsub
#
# == param
# -... Arguments for the parameters, see "details" section
# -RESET reset to default values
# -READ.ONLY please ignore
# -LOCAL please ignore
# -ADD please ignore
# 
# == details
# There are following parameters:
# 
# -``packages`` A character vector with package names that will be loaded before running the script.
# -``image`` A character vector of RData/rda files that will be loaded before running the script.
# -``temp_dir`` Path of temporary folder where the temporary R/bash scripts will be put.
# -``output_dir`` Path of output folder where the output/flag files will be put.
# -``enforce`` If a flag file for the job is found, whether to enforce to rerun the job.
# -``R_version`` The version of R.
# -``wd`` The working directory.
# -``ignore`` Whether ignore `bsub_chunk`, `bsub_script` and `bsub_cmd`.
# -``local`` Run job locally (not submitting to the LSF cluster)?
# -``call_Rscript`` How to call ``Rscript`` by specifying an R version number.
# -``submission_node`` A list of node names for submitting jobs.
# -``sh_head`` Commands that are written as head of the sh script.
#
# == example
# # examples are in the vignette.
#
bsub_opt = function(..., RESET = FALSE, READ.ONLY = NULL, LOCAL = FALSE, ADD = FALSE) {}
bsub_opt = set_opt(
    packages = list(
        .class = "character"
    ),
    image = list(
        .class = "character",
        .filter = function(x) {
            l = file.exists(x)
            if(all(!l)) {
                message_wrap(qq("All the image files specified do not exist. Save current workspace to @{x[1]}? [y/n]"), appendLF = FALSE)
                answer = readline()
                if(answer %in% c("y", "Y", "yes", "Yes")) {
                    save.image(file = x[1])
                    x = x[1]
                    l = TRUE
                } else {
                    stop("All image files do not exist.")
                }

            }
            if(any(!l)) {
                stop(qq("Some of the image files do not exist.\n"))
            }
            normalizePath(x)
        }
    ),
    temp_dir = list(
        .value = "~/.bsub_temp",
        .validate = function(x) {
            if(!file.exists(x)) {
                qqcat("create temp_dir: @{x}\n")
                dir.create(x, recursive = TRUE, showWarnings = FALSE)
            }
            all_f = list.files(x)
            if(length(all_f)) {
                message(qq("There are @{length(all_f)} temporary files in @{x}."))
            }
            TRUE
        },
        .filter = function(x) {
            normalizePath(x)
        }
    ),
    output_dir = list(
        .value = function() .v$temp_dir,
        .validate = function(x) {
            if(!file.exists(x)) {
                qqcat("create output_dir: @{x}\n")
                dir.create(x, recursive = TRUE, showWarnings = FALSE)
            }
            TRUE
        },
        .filter = function(x) {
            normalizePath(x)
        }
    ),
    enforce = list(
        .value = TRUE,
        .class = "logical"
    ),
    R_version = list(
        .value = qq("@{R.version$major}.@{R.version$minor}")
    ),
    wd = list(
        .value = "",
        .validate = file.exists,
        .length = 1
    ),
    ignore = list(
        .value = FALSE,
        .class = "logical"
    ),
    local = list(
        .value = FALSE,
        .class = "logical"
    ),
    call_Rscript = list(
        .value = function(version) qq("module load gcc/7.2.0; module load java/1.8.0_131; module load R/@{version}; Rscript"),
        .class = "function"),
    submission_node = list(
        .value = c("odcf-worker01", "odcf-cn34u03s10", "odcf-cn34u03s12"),
        .class = "character"
    ),
    ssh_session = list(
        .value = NULL,
        .private = TRUE,
        .visible = FALSE
    ),
    debug = list(
        .value = TRUE,
        .visible = FALSE
    ),
    sh_head = list(
        .value = "",
        .class = "character"
    ),
    user = list(
        .value = Sys.info()['user']
    )
)

bsub_opt$temp_dir = "~/.bsub_temp"

# == title
# Send R code
#
# == param
# -code The code chunk, should be embrached by ``\{`` ``\}``.
# -name If name is not specified, an internal name calculated by `digest::digest` on the chunk is automatically assigned. 
# -packages A character vector with package names that will be loaded before running the script.
# -image A character vector of RData/rda files that will be loaded before running the script.
#       When ``image`` is set to ``TRUE``, all variables in ``.GlobalEnv`` will be saved
#       into a temporary file and all attached packages will be recorded. The temporary files
#       will be removed after the job is finished.
# -variables A character vector of variable names that will be loaded before running the script.
# -wd The working directory.
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
#
# == value
# Job ID.
#
# == seealso
# - `bsub_script` submits R scripts.
# - `bsub_cmd`submits shell commands.
#
# == example
# # exaples are in the vignette.
#
bsub_chunk = function(code, 
    name = NULL,
    packages = bsub_opt$packages, 
    image = bsub_opt$image,
    variables = character(),
    wd = bsub_opt$wd,
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

    output_dir = normalizePath(output_dir)
    flag = qq("@{output_dir}/@{name}.flag")

    if(!enforce && file.exists(flag)) {
        qqcat("Job '@{name}' is already done, skip.\n")
        return(invisible(NULL))
    }

    qqcat("- job: '@{name}' from a code chunk\n")

    # if `image` is true, save all variables and all packages that are loaded
    if(identical(image, TRUE)) {
        variables = c(variables, ls(envir = .GlobalEnv, all.names = TRUE))
        packages = c(packages, names(sessionInfo()$otherPkgs))
        image = NULL
    }

    head = ""
    tail = ""
    for(p in packages) {
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
        save(list = variables, envir = parent.frame(), file = qq("@{temp_dir}/@{name}_var.RData"))
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

    if(wd != "") {
        if(!file.exists(wd)) {
            stop(qq("'@{wd}'' does not exist."))
        }
        head = c(head, qq("setwd('@{wd}')\n\n"))
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
# -name Job name
# -output_dir The output dir set in `bsub_chunk`.
# -wait Seconds to wait.
#
# == details
# It retrieve the saved variable in `bsub_chunk` when ``save_rds = TRUE`` is set.
#
retrieve_var = function(name, output_dir = bsub_opt$output_dir, wait = 30) {
    rds_file = qq("@{output_dir}/@{name}_returned_var.rds")
    out_file = qq("@{output_dir}/@{name}.out")
    flag_file = qq("@{output_dir}/@{name}.flag")

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

job_status_by_name = function(job_name, output_dir = bsub_opt$output_dir) {
    if(on_submission_node()) {
        con = pipe(qq("bjobs -J @{job_name} 2>&1"))
        ln = readLines(con)
        close(con)
    } else {
        ln = ssh_exec(qq("bjobs -J @{job_name} 2>&1"))
    }

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
    if(on_submission_node()) {
        con = pipe(qq("bjobs -o \"jobid user stat\" @{job_id} 2>&1"))
        ln = readLines(con)
        close(con)
    } else {
        ln = ssh_exec(qq("bjobs -o \"jobid user stat\" @{job_id} 2>&1"))
    }

     if(length(ln) == 1) {
        return("MISSING")
    }

    lt = strsplit(ln, "\\s+")

    lt = lt[-1]
    sapply(lt, "[", 3)
}

# == title
# Send R script
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
# # exaples are in the vignette.
#
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

    output_dir = normalizePath(output_dir)
    flag = qq("@{output_dir}/@{name}.flag")

    if(!enforce && file.exists(flag)) {
        qqcat("Job '@{name}' is already done, skip.\n")
        return(invisible(NULL))
    }

    qqcat("- job: '@{name}' from script @{basename(script)}\n")
    
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
# Send shell commands
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
# # exaples are in the vignette.
#
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

    output_dir = normalizePath(output_dir)
    flag = qq("@{output_dir}/@{name}.flag")

    if(!enforce && file.exists(flag)) {
        qqcat("Job '@{name}' is already done, skip.\n")
        return(invisible(NULL))
    }

    qqcat("- job: '@{name}' from a list of commands\n")
    
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
    flag = qq("@{output_dir}/@{name}.flag")

    if(!enforce && file.exists(flag)) {
        qqcat("Job '@{name}' is already done, skip.\n")
        return(invisible(NULL))
    }

    if(file.exists(flag)) {
        file.remove(flag);
    }
    if(file.exists(output)) {
        file.remove(output);
    }
    
    sh_file = tempfile(paste0(name, "_"), tmpdir = temp_dir, fileext = ".sh")
    con = file(sh_file, "w")
    if(!identical(sh_head, "")) {
        writeLines(sh_head, con)
        writeLines("\n", con)
    }
    if(bsub_opt$debug) {
        writeLines(qq("cat '@{sh_file}'\n"), con)
    }
    writeLines(qq("rm '@{sh_file}'\n"), con)
    writeLines(command, con)

    # test the command, 
    writeLines("
if [ $? -ne 0 ]
then
    echo Exit code is not equal to zero. There is an error.
    exit 666
fi", con)

    writeLines(qq("touch '@{flag}'\n"), con)
    close(con)

    if(local) {
        cmd = qq("bash @{sh_file}")
        cat(cmd, "\n")
        system(cmd)
        return(invisible(NULL))
    }

    system(qq("chmod 755 @{sh_file}"))
    cmd = qq("bsub -J '@{name}' -W '@{hour}:00' -n @{core} -R 'rusage[mem=@{round(memory*1024)}]' -o '@{output}'")
    if(length(dependency)) {
        dependency_str = paste( paste("done(", dependency, ")"), collapse = " && " )
        cmd = qq("@{cmd} -w '@{dependency_str}'")
    }
    cmd = qq("@{cmd} '@{sh_file}'")
    cat(cmd, "\n")

    nodename = Sys.info()["nodename"]
    if(on_submission_node()) {
        con = pipe(cmd)
        txt = readLines(con)
        close(con)
        
    } else {

        txt = ssh_exec(cmd)
    }
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

    if(file.exists("~/.bsub_history.rds")) {
        job_history = readRDS("~/.bsub_history.rds")
        job_history = rbind(job_history, job_tb)
    } else {
        job_history = job_tb
    }
    saveRDS(job_history, file = "~/.bsub_history.rds")

    return(job_id)
}


message_wrap = function (..., appendLF = TRUE) {
    x = paste0(...)
    x = paste(strwrap(x), collapse = "\n")
    message(x, appendLF = appendLF)
}
