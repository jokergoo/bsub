
#' Submit jobs
#'
#' @param code The code chunk, it should be embraced by ``\{`` ``\}``.
#' @param name If name is not specified, an internal name calculated by [`digest::digest()`] on the chunk is automatically assigned. 
#' @param packages A character vector with package names that will be loaded before running the script. There is a special name `_in_session_`
#'     that loads all the packages loaded in current R session.
#' @param image A character vector of RData/rda files that will be loaded before running the script.
#'       When `image` is set to `TRUE`, all variables in [`.GlobalEnv`] will be saved
#'       into a temporary file and all attached packages will be recorded. The temporary files
#'       will be removed after the job is finished.
#' @param variables A character vector of variable names that will be loaded before running the script. There is a special name `_all_functions_`
#'       that saves all functions defined in the global environment.
#' @param share A character vector of variables names for which the variables are shared between jobs. Note the temporary .RData files are not deleted
#'        automatically.
#' @param working_dir The working directory.
#' @param hours Running time of the job.
#' @param memory Memory usage of the job. It is measured in GB.
#' @param cores Number of cores.
#' @param R_version R version.
#' @param temp_dir Path of temporary folder where the temporary R/bash scripts will be put.
#' @param output_dir Path of output folder where the output/flag files will be put.
#' @param dependency A vector of job IDs that current job depends on.
#' @param enforce If a flag file for the job is found, whether to enforce to rerun the job.
#' @param local Run job locally (not submitting to the LSF cluster)?
#' @param script In `bsub_chunk()`, it is the path of a script where code chunks will be extracted and sent to the cluster. 
#'       It is always used with `start` and `end` arguments. In `bsub_script()`, it is the path of the R script to submit.
#' @param start A numeric vector that contains line indices of the starting code chunk or a character vector
#'        that contain regular expression to match the start of code chunks.
#' @param end Same setting as `start`.
#' @param save_var Whether save the last variable in the code chunk? Later the variable
#'    can be retrieved by [`retrieve_var()`].
#' @param sh_head Commands that are written as head of the sh script.
#' @param ask Whether to promote.
#' 
#' @details
#' `job_chunk()` submits R chunk.
#' `job_script()` submits R script.
#' `job_cmd()` submits general commands.
#'
#' @returns A job ID.
#' @export
#' @importFrom codetools findGlobals
#' @importFrom utils sessionInfo
#' @import crayon
#' @rdname bsub
#' @examples
#' \dontrun{
#' bsub_chunk(name = "example", memory = 10, hours = 10, cores = 4,
#' {
#'     Sys.sleep(5)
#' })
#' 
#' # the R version is defined in bsub_opt$R_version
#' bsub_script("/path/of/foo.R", name = ..., memory = ..., cores = ..., ...)
#' # with command-line arguments
#' bsub_script("/path/of/foo.R", argv = "--a 1 --b 3", ...)
#' 
#' # put all arguments also in the command
#' bsub_cmd("sometool -arg1 1 -arg2 2", name = ..., memory = ..., cores = ..., ...)
#' }
bsub_chunk = function(code, 
    name = NULL,
    packages = bsub_opt$packages, 
    image = bsub_opt$image,
    variables = character(),
    share = character(),
    working_dir = bsub_opt$working_dir,
    hours = 1, 
    memory = 1, 
    cores = 1,
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
    sh_head = bsub_opt$sh_head,
    ask = TRUE) {

    if(!under_same_file_system()) {
        stop("Job can only be sumitted on the same file system as submission nodes.")
    }
    
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

        if(length(variables) == 0) {
            expr = substitute(code)
            code_fun = function() {}
            body(code_fun) = expr
            gl = findGlobals(code_fun, merge = FALSE)
            variables = gl$variables

            if(length(variables)) {
                message_wrap(qq("There are variables (@{paste(variables, collapse = ', ')}) not defined in the code chunk. Automatically use the ones from global environment. Note this is approximate."))
            }

            if(length(packages) == 0 && !identical(image, TRUE)) {
                functions = gl$functions
                for(f in functions) {
                    p = find(f)
                    if(length(p)) {
                        if(grep("package:", p)) {
                            packages = c(packages, p)
                        } else if(p == ".GlobalEnv") {
                            variables = c(variables, f)
                        }
                    }
                }
                packages = gsub("package:", "", packages)
                packages = unique(package)
                packages = setdiff(packages, c("base", "stats", "graphics", "grDevices", "utils", "datasets", "methods"))
                if(length(packages)) {
                    message_wrap(qq("There are packages (@{paste(packages, collapse = ', ')}) not specified in the code chunk. Automatically detech them. Note this is approximate."))
                }
            }

            code = deparse(expr)
        } else {
            code = deparse(substitute(code))
        }
    }
    code = paste(code, collapse = "\n")
    code = paste0(code, "\n")

    if(is.null(name)) {
        name = paste0("R_code_", digest::digest(code, "crc32"))
    } else {
        name = qq(name)
    }

    t = as.POSIXlt(Sys.time())
    t = as.numeric(t) + t$sec - floor(t$sec)
    t = gsub("\\.", "_", t)
    name_uid = paste0(name, "_", t)

    if(!file.exists(temp_dir)) {
        if(ask) {
            answer = readline(qq("create temp_dir: @{temp_dir}? [y|n] "))
            if(answer %in% c("y", "Y", "yes", "Yes", "YES")) {
                dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
            } else {
                stop_wrap(qq("not allowed to create @{temp_dir}."))
            }
        } else {
            message("create temp_dir: ", temp_dir)
            dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
        }
    }
    if(!file.exists(output_dir)) {
        if(ask) {
            answer = readline(qq("create output_dir: @{output_dir}? [y|n] "))
            if(answer %in% c("y", "Y", "yes", "Yes", "YES")) {
                dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
            } else {
                stop_wrap(qq("not allowed to create @{output_dir}."))
            }
        } else {
            message("create output_dir: ", output_dir)
            dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
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

    if(length(share)) {
        if(!inherits(share, "character")) {
            stop_wrap("`share` must be a character vector (the names of the variables you want to import).")
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
        head = c(head, qq("library(@{p})"))
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
            head = c(head, qq("load(\"@{f}\")"))
        }
    }

    if(length(variables)) {
        save(list = unique(variables), envir = parent.frame(), file = qq("@{temp_dir}/@{name_uid}_var.RData"))
        head = c(head, qq("load(\"@{temp_dir}/@{name_uid}_var.RData\")"))
        tail = c(tail, qq("invisible(file.remove(\"@{temp_dir}/@{name_uid}_var.RData\"))"))
    }

    if(length(share)) {
        for(i in seq_along(share)) {
            share_hash = digest::digest(list(name = share[i], value = get(share[i], envir = parent.frame())))
            share_file = qq("@{temp_dir}/@{name_uid}_var_shared_@{share_hash}.RData")
            if(!file.exists(share_file)) {
                save(list = share[i], envir = parent.frame(), file = share_file)
            }
            head = c(head, qq("load(\"@{share_file}\")"))
        }
    }

    tmp = tempfile(paste0(name_uid, "_"), fileext = ".R", tmpdir = temp_dir)
    if(bsub_opt$debug) {  # print R code to out file
        head = c(head, qq("cat(readLines(\"@{tmp}\"), sep = \"\\n\")"))
    }
    tail = c(tail, "invisible(file.remove(\"@{tmp}\"))\n\n")  # the R script is already loaded in memory, the temp R script can be deleted

    if(save_var) {
        tail = c(tail, qq("saveRDS(.Last.value, file = '@{output_dir}/@{name_uid}_returned_var.rds')"))
    }

    tail = c(tail, "invisible(NULL)\n")

    if(working_dir != "") {
        if(!file.exists(working_dir)) {
            stop(qq("'@{working_dir}'' does not exist."))
        }
        head = c(head, qq("setwd('@{working_dir}')\n\n"))
    }
    writeLines(c(head, 
                 "#### R chunk start ####",
                 code, 
                 "#### R chunk end ####",
                 tail), con = tmp)

    command = qq("@{bsub_opt$call_Rscript(R_version)} '@{tmp}';")
    
    bsub_submit(
        command = command, 
        hours = hours,
        memory = memory,
        cores = cores,
        name = name,
        name_uid = name_uid,
        output_dir = output_dir,
        temp_dir = temp_dir,
        dependency = dependency,
        enforce = enforce,
        local = local,
        sh_head = sh_head)
}

#' @param argv A string of command-line arguments.
#' @param ... Command-line arguments can also be specified as name-value pairs.
#' 
#' @rdname bsub
#' @export
bsub_script = function(script, 
    argv = "", 
    name = NULL, 
    hours = 1, 
    memory = 1, 
    cores = 1,
    R_version = bsub_opt$R_version,
    temp_dir = bsub_opt$temp_dir,
    output_dir = bsub_opt$output_dir,
    dependency = NULL,
    enforce = bsub_opt$enforce, 
    local = bsub_opt$local,
    sh_head = bsub_opt$sh_head,
    ask = TRUE,
    ...) {

    if(!under_same_file_system()) {
        stop("Job can only be sumitted on the same file system as submission nodes.")
    }
    
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

    t = as.POSIXlt(Sys.time())
    t = as.numeric(t) + t$sec - floor(t$sec)
    t = gsub("\\.", "_", t)
    name_uid = paste0(name, "_", t)

    if(!file.exists(temp_dir)) {
        if(ask) {
            answer = readline(qq("create temp_dir: @{temp_dir}? [y|n] "))
            if(answer %in% c("y", "Y", "yes", "Yes", "YES")) {
                dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
            } else {
                stop_wrap(qq("not allowed to create @{temp_dir}."))
            }
        } else {
            message("create temp_dir: ", temp_dir)
            dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
        }
    }
    if(!file.exists(output_dir)) {
        if(ask) {
            answer = readline(qq("create output_dir: @{output_dir}? [y|n] "))
            if(answer %in% c("y", "Y", "yes", "Yes", "YES")) {
                dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
            } else {
                stop_wrap(qq("not allowed to create @{output_dir}."))
            }
        } else {
            message("create output_dir: ", temp_dir)
            dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
        }
    }
    temp_dir = normalizePath(temp_dir)
    output_dir = normalizePath(output_dir)

    cat(cyan(qq("- job: '@{magenta(name)}' from script @{basename(script)}\n")))
    
    command = qq("@{bsub_opt$call_Rscript(R_version)} '@{script}' @{argv};")

    bsub_submit(
        command = command, 
        hours = hours,
        memory = memory,
        cores = cores,
        name = name,
        name_uid = name_uid,
        output_dir = output_dir,
        temp_dir = temp_dir,
        dependency = dependency,
        enforce = enforce,
        local = local,
        sh_head = sh_head)
}


#' @param cmd A single-line command.
#' @param sh Path of the bash script.
#' @param env_var Environment variables. It should be a named vector. Note environment variables can also be directly set in `sh_head`.
#' @rdname bsub
#' @export
bsub_cmd = function(cmd, 
    sh = NULL,
    name = NULL, 
    hours = 1, 
    memory = 1, 
    cores = 1,
    temp_dir = bsub_opt$temp_dir,
    output_dir = bsub_opt$output_dir,
    dependency = NULL,
    enforce = bsub_opt$enforce, 
    local = bsub_opt$local,
    env_var = NULL,
    sh_head = bsub_opt$sh_head,
    ask = TRUE,
    ...) {

    if(!under_same_file_system()) {
        stop("Job can only be sumitted on the same file system as submission nodes.")
    }
    
    if(bsub_opt$ignore) return(invisible(NULL))

    if(!is.null(sh)) {
        if(!file.exists(sh)) {
            stop_wrap(qq("Cannot find the bash script: @{sh}"))
        }

        cmd = readLines(sh)
    }

    if(is.null(name)) {
        name = paste0("cmd_", digest::digest(cmd, "crc32"))
    } else {
        name = qq(name)
    }

    t = as.POSIXlt(Sys.time())
    t = as.numeric(t) + t$sec - floor(t$sec)
    t = gsub("\\.", "_", t)
    name_uid = paste0(name, "_", t)

    if(!file.exists(temp_dir)) {
        if(ask) {
            answer = readline(qq("create temp_dir: @{temp_dir}? [y|n] "))
            if(answer %in% c("y", "Y", "yes", "Yes", "YES")) {
                dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
            } else {
                stop_wrap(qq("not allowed to create @{temp_dir}."))
            }
        } else {
            message("create temp_dir: ", temp_dir)
            dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
        }
    }
    if(!file.exists(output_dir)) {
        if(ask) {
            answer = readline(qq("create output_dir: @{output_dir}? [y|n] "))
            if(answer %in% c("y", "Y", "yes", "Yes", "YES")) {
                dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
            } else {
                stop_wrap(qq("not allowed to create @{output_dir}."))
            }
        } else {
            message("create output_dir: ", temp_dir)
            dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
        }
    }
    temp_dir = normalizePath(temp_dir)
    output_dir = normalizePath(output_dir)

    cat(cyan(qq("- job: '@{magenta(name)}' from a list of commands\n")))
    
    command = paste(cmd, collapse = "\n")

    if(!is.null(env_var)) {
        for(env_nm in names(env_var)) {
            sh_head = paste0(sh_head, "\n", env_nm, "=", env_var[env_nm])
        }
    }

    bsub_submit(
        command = command, 
        hours = hours,
        memory = memory,
        cores = cores,
        name = name,
        name_uid = name_uid,
        output_dir = output_dir,
        temp_dir = temp_dir,
        dependency = dependency,
        enforce = enforce,
        local = local,
        sh_head = sh_head)
}

#' @importFrom crayon silver
bsub_submit = function(command,
    hours,
    memory,
    cores,
    name,
    name_uid,
    output_dir,
    temp_dir,
    dependency = NULL,
    enforce = TRUE,
    local = FALSE,
    sh_head = "") {

    if(!under_same_file_system()) {
        stop("Job can only be sumitted on submission nodes.")
    }

    output_dir = normalizePath(output_dir)
    temp_dir = normalizePath(temp_dir)

    done = qq("@{output_dir}/@{name}.done")
    old_flag = qq("@{output_dir}/@{name}.flag")
    pend = qq("@{output_dir}/@{name}.pend")
    run = qq("@{output_dir}/@{name}.run")
    output = qq("@{output_dir}/@{name_uid}.out")

    if(!enforce && (file.exists(done) || file.exists(old_flag))) {
        cat(cyan(qq("Job '@{name}' is already done, skip.\n")))
        return(invisible(NULL))
    }
    if(!enforce && file.exists(pend)) {
        # if the pending job is killed, the pend flag is still there
        if("PEND" %in% job_status_by_name(name)) {
            cat(cyan(qq("Job '@{name}' is pending, skip.\n")))
            return(invisible(NULL))
        }
    }
    if(!enforce && file.exists(run)) {
        # if the running job is killed, the run flag is still there
        if("RUN" %in% job_status_by_name(name)) {
            cat(cyan(qq("Job '@{name}' is running, skip.\n")))
            return(invisible(NULL))
        }
    }

    if(file.exists(done)) {
        file.remove(done);
    }

    
    sh_file = tempfile(paste0(name_uid, "_"), tmpdir = temp_dir, fileext = ".sh")
    con = file(sh_file, "w")

    writeLines("########## temporary bash script ###############", con)
    
    writeLines(qq("[ -f '@{pend}' ] && rm '@{pend}'\n"), con)  # remove the pend flag file
    writeLines(qq("touch '@{run}'\n"), con)  # add the running flag
    
    if(!identical(sh_head, "")) {
        writeLines("##### sh_head start #####", con)
        writeLines(sh_head, con)
        writeLines("##### sh_head end #####\n\n", con)
    }

    # wrap command with eval
    writeLines("##### commands start #####", con)
    writeLines(command, con)
    writeLines("##### commands end #####\n\n", con)

    # test the command, 
    writeLines(qq("
if [ $? -ne 0 ]
then
    rm '@{run}'
    echo Exit code is not equal to zero. There is an error.
    exit 666
fi"), con)

    writeLines(qq("rm '@{run}'\n"), con)
    writeLines(qq("touch '@{done}'\n"), con)
    close(con)

    if(local) {
        cmd = qq("bash @{sh_file}")
        cat(cmd, "\n")
        system(cmd)
        file.remove(sh_file)
        return(invisible(NULL))
    }

    system(qq("chmod 755 @{sh_file}"))

    cmd = bsub_opt$bsub_template(name, hours, memory, cores, output, bsub_opt$group)
    if(length(dependency)) {
        dependency_str = paste( paste("done(", dependency, ")"), collapse = " && " )
        cmd = qq("@{cmd} \\\n -w '@{dependency_str}' \\\n")
    }
    cmd = qq("@{cmd} < '@{sh_file}'")
    cat(silver(cmd), "\n")

    cmd = bsub_opt$bsub_template(name, hours, memory, cores, output, bsub_opt$group)
    # add values of name_uid and temp_dir to `bjobs` in job description
    cmd = qq("@{cmd} -env 'all,R_BSUB_NAME_UID=@{name_uid},R_BSUB_TEMP_DIR=@{temp_dir}' -Jd 'R_BSUB_NAME_UID=@{name_uid},R_BSUB_TEMP_DIR=@{temp_dir}'")
    if(length(dependency)) {
        dependency_str = paste( paste("done(", dependency, ")"), collapse = " && " )
        cmd = qq("@{cmd} \\\n -w '@{dependency_str}' \\\n")
    }
    cmd = qq("@{cmd} < '@{sh_file}'")

    file.create(pend)
    txt = run_cmd(cmd, print = FALSE)
    file.remove(sh_file)

    job_id = gsub("^.*<(\\d+)>.*$", "\\1", txt)

    return(job_id)
}


#' Submit a random job
#' 
#' @param name Job name.
#' @param ... Pass to [`bsub_chunk()`].
#' 
#' @details
#' It simply runs `Sys.sleep(30)` in the job.
#' 
#' @export
#' @importFrom stats runif
#' @returns A job ID.
#' @examples
#' \dontrun{
#' random_job()
#' }
random_job = function(name, ...) {
    if(missing(name)) {
        name = paste0("R_random_job_", digest::digest(runif(1), "crc32"))
    }
    bsub_chunk({
        Sys.sleep(30)
    }, name = name, ...)
}


