 



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
# -``working_dir`` The working directory.
# -``ignore`` Whether ignore `bsub_chunk`, `bsub_script` and `bsub_cmd`.
# -``local`` Run job locally (not submitting to the LSF cluster)?
# -``call_Rscript`` How to call ``Rscript`` by specifying an R version number.
# -``submission_node`` A list of node names for submitting jobs.
# -``login_node`` This value basically is the same as ``submission_node`` unless the login nodes are different from submission nodes.
# -``sh_head`` Commands that are written as head of the sh script.
# -``user`` Username on the submission node.
# -``group`` The user group
# -``ssh_envir`` The commands for setting bash environment for successfully running bjobs, bsub, ...
# -``bsub_template`` Template for constructing ``bsub`` command.
# -``parse_time`` A function that parses time string from the LSF ``bjobs`` command to a `POSIXct` object.
# -``verbose`` Whether to print more messages.
#
# ``ssh_envir`` should be properly set so that LSF binaries such as ``bsub`` or ``bjobs`` can be properly found.
# There are some environment variables initialized when logging in the bash terminal while they are not initialized with the
# ssh connection. Thus, some environment variables should be manually set.
#
# An example for ``ssh_envir`` is as follows. The ``LSF_ENVDIR`` and ``LSF_SERVERDIR`` should be defined and exported.
# 
#     c("source /etc/profile",
#       "export LSF_ENVDIR=/opt/lsf/conf",
#       "export LSF_SERVERDIR=/opt/lsf/10.1/linux3.10-glibc2.17-x86_64/etc")
#
# The values of these two variables can be obtained by entering following commands in your bash terminal (on the submission node):
#
#     echo $LSF_ENVDIR
#     echo $LSF_SERVERDIR
#
# The time strings by LSF ``bjobs`` command might be different for different configurations. The **bsub**
# package needs to convert the time strings to `POSIXlt` objects for calculating the time difference. Thus, if
# the default time string parsing fails, users need to provide a user-defined function and set with ``parse_time``
# option in `bsub_opt`. The function accepts a vector of time strings and returns a `POSIXlt` object. For example,
# if the time string returned from ``bjobs`` command is in a form of ``Dec 1 18:00:00 2019``, the parsing function
# can be defined as:
#
#     bsub_opt$parse_time = function(x) {
#         as.POSIXlt(x, format = "\\%b \\%d \\%H:\\%M:\\%S \\%Y")
#     }
#
# == value
# The corresponding option values.
#
# == example
# # The default bsub_opt
# bsub_opt
#
bsub_opt = function(..., RESET = FALSE, READ.ONLY = NULL, LOCAL = FALSE, ADD = FALSE) {}
bsub_opt = set_opt(
    ask = list(
        .value = TRUE,
        .visible = FALSE
    ),
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
                stop(qq("Some of the image files do not exist."))
            }
            normalizePath(x)
        }
    ),
    temp_dir = list(
        .value = "~/.bsub_temp",
        .validate = function(x) {
            check_temp_dir(x, ask = .v$ask)
            TRUE
        },
        .filter = function(x) {
            normalizePath(x)
        }
    ),
    output_dir = list(
        .value = function() .v$temp_dir,
        .validate = function(x) {
            if(!file.exists(x) && .v$ask) {
                answer = readline(qq("create output_dir: @{x}? [y|n] "))
                if(answer %in% c("y", "Y", "yes", "Yes", "YES")) {
                    dir.create(x, recursive = TRUE, showWarnings = FALSE)
                } else {
                    stop_wrap(qq("not allowed to create @{x}."))
                }
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
    working_dir = list(
        .value = "",
        .validate = file.exists,
        .length = 1
    ),
    wd = list(.synonymous = "working_dir"),
    ignore = list(
        .value = FALSE,
        .class = "logical"
    ),
    local = list(
        .value = FALSE,
        .class = "logical"
    ),
    call_Rscript = list(
        .value = function(version) "Rscript",
        .class = "function"
    ),
    submission_node = list(
        .value = NULL
    ),
    login_node = list(
        .value = function() .v$submission_node
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
    ),
    group = list(
        .value = NULL
    ),
    ssh_envir = list(
    	.value = c("source /etc/profile")
    ),
    bsub_template = list(
        .value = function(name, hour, memory, core, output, group = NULL, ...) {
            if(identical(group, "") || identical(group, NULL)) {
                cmd = qq("bsub -J '@{name}' -W '@{floor(hour)}:@{floor((hour-floor(hour))*60)}' -n @{core} -R 'rusage[mem=@{round(memory*1024)}]'", 
                         "     -o '@{output}'", 
                         "   ", sep = " \\\n")
            } else {
                cmd = qq("bsub -J '@{name}' -W '@{floor(hour)}:@{floor((hour-floor(hour))*60)}' -n @{core} -R 'rusage[mem=@{round(memory*1024)}]'",
                         "     -G @{bsub_opt$group} -o '@{output}'",
                         "   ", sep = " \\\n")
            }
            return(cmd)
        },
        .class = "function"
    ),
    parse_time = list(
        .value = NULL,
        .class = "function"
    ),
    verbose = list(
        .value = FALSE,
        .class = "logical"
    )
)

check_temp_dir = function(x, ask = TRUE) {
    if(!file.exists(x) && ask) {
        answer = readline(qq("create temp_dir: @{x}? [y|n] "))
        if(answer %in% c("y", "Y", "yes", "Yes", "YES")) {
            dir.create(x, recursive = TRUE, showWarnings = FALSE)
        } else {
            stop_wrap(qq("not allowed to create @{x}."))
        }
    } else {
        all_f = list.files(x, full.names = TRUE)

        if(length(all_f)) {
            file_size = sum(file.info(all_f)[, "size"], na.rm = TRUE)
            if(file_size < 1024) {
                fs = paste0(file_size, "Byte")
            } else if(file_size < 1024^2) {
                fs = paste0(round(file_size/1024, 1), "KB")
            } else if(file_size < 1024^3) {
                fs = paste0(round(file_size/1024^2, 1), "MB")
            } else {
                fs = paste0(round(file_size/1024^3, 1), "GB")
            }
            message(qq("There are @{length(all_f)} temporary files (@{fs}) in @{x}"))
        }
    }
}

# == title (variable:bconf)
# Print current configuation
#
# == details
# This function is only for printing. Use `bsub_opt` to change configurations.
#
# You simply type ``bconf`` (without the brackets) in the interactive R console.
#
# == example
# bconf
bconf = function() {
    structure("foo", class = "bconf")
}
class(bconf) = "bconf"

# == title
# Print the configurations
#
# == param
# -x A bconf object
# -... Other parameters
#
# == value
# No value is returned.
print.bconf = function(x, ...) {
    cat(get_bconf_message(x), "\n")

    flag = 0
    msg = NULL
    if(!file.exists(bsub_opt$temp_dir)) {
        msg = c(msg, qq("!! The temp_dir (@{bsub_opt$temp_dir}) does not exist."))
        flag = 1
    }
    if(bsub_opt$temp_dir != bsub_opt$output_dir) {
        if(!file.exists(bsub_opt$output_dir)) {
            msg = c(msg, qq("!! The output_dir (@{bsub_opt$output_dir}) does not exist."))
            flag = 2
        }
    }
    if(flag) {
        if(flag == 1) {
            msg = c(msg, "!! The directory can be created manually, or by setting a value to",
                         "!! `bsub_opt$temp_dir`, or by the first call of `bsub_*()`.")
        } else {
            msg = c(msg, "!! The directory can be created manually, or by setting a value to",
                         "!! `bsub_opt$output_dir`, or by the first call of `bsub_*()`.")
        }

        cat("\n")
        cat(msg, sep = "\n")
        cat("\n")
    }
}

get_bconf_message = function(x, ...) {
    x = bsub_opt()
    msg = NULL
    msg = c(msg, "Configurations for bsub:")
    if(is.null(x$user)) {
        msg = c(msg, "  * user is not defined")
    } else {
        msg = c(msg, qq("  * user for connecting submission node: @{x$user}"))
    }

    if(!is.null(x$group)) {
        msg = c(msg, qq("  * user group : @{x$group}"))
    }

    if(is.null(x$submission_node)) {
        msg = c(msg, qq("  ! submission node is not defined"))
    } else {
        msg = c(msg, qq("  * submission node: @{paste(x$submission_node, collapse = ', ')}"))

        if(length(setdiff(x$login_node, x$submission_node))) {
            msg = c(msg, qq("  * login node: @{paste(x$login_node, collapse = ', ')}"))
        }
    }
    
    if(!is.null(x$R_version)) {
        msg = c(msg, qq("  * global R version: @{x$R_version}"))
    }

    msg = c(msg, "  * command to call `Rscript`:")
    msg = c(msg, paste0("     ", deparse(body(x$call_Rscript)), " foo.R"))

    if(!is.null(x$package)) {
        msg = c(msg, qq("  * Global R packages: @{paste(x$packages, collapse=',')}"))
    }

    if(!is.null(x$image)) {
        msg = c(msg, "  * Global image files that will be loaded to every job:")
        for(f in x$image) {
            msg = c(msg, qq("    - @{f}"))
        }
    }

    msg = c(msg, qq("  * temporary directory: @{x$temp_dir}"))

    if(!x$enforce) {
        msg = c(msg, "  - successful jobs are skipped.")
    }

    msg = c(msg, "")
    msg = c(msg, "Configurations can be modified by `bsub_opt()` function")
    return(paste(msg, collapse = "\n"))
}

