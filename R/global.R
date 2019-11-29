 



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
# -``user`` Username on the submission node.
# -``group`` The user group
# -``ssh_envir`` The commands for setting bash environment for successfully running bjobs, bsub, ...
# -``bsub_template`` Template for constructing ``bsub`` command.
#
# For ``ssh_envir`` option, an example is as follows. The ``LSF_ENVDIR`` and ``LSF_SERVERDIR`` should be defined and exported.
# 
#     c("source /etc/profile",
#       "export LSF_ENVDIR=/opt/lsf/conf",
#       "export LSF_SERVERDIR=/opt/lsf/10.1/linux3.10-glibc2.17-x86_64/etc")
#
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
        .value = NULL,
        .class = "function"
    ),
    submission_node = list(
        .value = NULL,
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
    ),
    group = list(
        .value = ""
    ),
    ssh_envir = list(
    	.value = c("source /etc/profile")
    ),
    bsub_template = list(
        .value = function(name, hour, memory, core, output, group = NULL, ...) {
            if(identical(group, "") || identical(group, NULL)) {
                cmd = qq("bsub -J '@{name}' -W '@{hour}:00' -n @{core} -R 'select[mem>@{round(memory*1024)}] rusage[mem=@{round(memory*1024)}]' -M@{round(memory*1024)} -o '@{output}'")
            } else {
                cmd = qq("bsub -J '@{name}' -W '@{hour}:00' -n @{core} -R 'select[mem>@{round(memory*1024)}] rusage[mem=@{round(memory*1024)}]' -M@{round(memory*1024)} -G @{bsub_opt$group} -o '@{output}'")
            }
            return(cmd)
        },
        .class = "function"
    )
)

bsub_opt$temp_dir = "~/.bsub_temp"
