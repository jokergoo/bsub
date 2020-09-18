 



.onAttach = function(libname, pkgname) {

	version = utils::packageDescription(pkgname, fields = "Version")

	msg = NULL
	msg = c(msg, "==================================================================")
	msg = c(msg, paste0("bsub version ", version))
	msg = c(msg, "Github page: https://github.com/jokergoo/bsub")
	msg = c(msg, "")

	msg = c(msg, "- submit R code: `bsub_chunk()`")
	msg = c(msg, "- submit R script: `bsub_script()`")
	msg = c(msg, "- submit shell commands: `bsub_cmd()`")
	msg = c(msg, "- kill jobs: `bkill()`")
	msg = c(msg, "- view job summary: `bjobs`/`brecent`/`bjobs_running`/")
	msg = c(msg, "                    `bjobs_pending`/`bjobs_done`/`bjobs_exit`")
	msg = c(msg, "- view job log: `job_log()`")
	msg = c(msg, "- remove temporary files : `clear_temp_dir()`/`check_dump_files()`")
	msg = c(msg, "- interactive job monitor: `monitor()`")

	msg = c(msg, "")
	msg = c(msg, "`bsub_chunk()`/`bsub_script()`/`bsub_cmd()` should only be")
	msg = c(msg, "applied on the node that has the same file system as the computing")
	msg = c(msg, "nodes. Other functions for monitoring and cleaning jobs can be")
	msg = c(msg, "applied on any computer.")

	msg = c(msg, "==================================================================")

	if(grepl("odcf|w610", Sys.info()["nodename"])) {
		config_odcf(verbose = FALSE)
		msg = c(msg, "")
		msg = c(msg, qq("Configure for user '@{bsub_opt$user}' on node @{paste(bsub_opt$submission_node, collapse = ', ')}"))
		msg = c(msg, "")

		msg = c(msg, get_bconf_message(bconf))

	} else {
		flag = 0
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
		}
	}
	

	packageStartupMessage(paste(msg, collapse = "\n"))
}


.onDetach = function(libpath) {
	if(!is.null(bsub_opt$ssh_session)) ssh_disconnect()
}

.onUnload = function(libpath) {
	if(!is.null(bsub_opt$ssh_session)) ssh_disconnect()
}

if(identical(topenv(), .GlobalEnv)) {
	if(grepl("odcf|w610", Sys.info()["nodename"])) {
		config_odcf()
	}
}


