 



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
	msg = c(msg, "- interactive job monitor: `monitor()`")

	msg = c(msg, "")
	msg = c(msg, "`bsub_chunk()`/`bsub_script()`/`bsub_cmd()` should only be")
	msg = c(msg, "applied on the node that has the same file system as the computing")
	msg = c(msg, "nodes. Other functions for monitoring and cleaning jobs can be")
	msg = c(msg, "applied on any computer.")

	msg = c(msg, "==================================================================")

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


