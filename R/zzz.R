 



.onAttach = function(libname, pkgname) {

	version = utils::packageDescription(pkgname, fields = "Version")

	packageStartupMessage("==================================================================")
	packageStartupMessage(paste0("bsub version ", version))
	packageStartupMessage("Github page: https://github.com/jokergoo/bsub")
	packageStartupMessage("")


	msg = FALSE
	if(!file.exists("~/.bsub_temp")) {
	    packageStartupMessage("create temp_dir: ~/.bsub_temp")
	    dir.create("~/.bsub_temp", recursive = TRUE, showWarnings = FALSE)
	    msg = TRUE
	}

	all_f = list.files("~/.bsub_temp/", full.names = TRUE)
	if(length(all_f)) {
		file_size = sum(file.info(all_f)[, "size"])
		if(file_size < 1024) {
			fs = paste0(file_size, "Byte")
		} else if(file_size < 1024^2) {
			fs = paste0(round(file_size/1024, 1), "KB")
		} else if(file_size < 1024^3) {
			fs = paste0(round(file_size/1024^2, 1), "MB")
		} else {
			fs = paste0(round(file_size/1024^3, 1), "GB")
		}
		packageStartupMessage(qq("There are @{length(all_f)} temporary files (@{fs}) in ~/.bsub_temp"))
		msg = TRUE
	}

	if(grepl("odcf|w610", Sys.info()["nodename"])) {
		msg = TRUE
		config_odcf(verbose = FALSE)
	}
	
	if(msg) packageStartupMessage("")
	packageStartupMessage("- submit R code: `bsub_chunk()`")
	packageStartupMessage("- submit R script: `bsub_script()`")
	packageStartupMessage("- submit shell commands: ``bsub_cmd()`")
	packageStartupMessage("- kill jobs: `bkill()`")
	packageStartupMessage("- view job summary: `bjobs`/`brecent`/`bjobs_running`/")
	packageStartupMessage("                    `bjobs_pending`/`bjobs_done`/`bjobs_exit`")
	packageStartupMessage("- view job log: `job_log()`")
	packageStartupMessage("- remove temporary files : `clear_temp_dir()`/`check_dump_files()`")
	packageStartupMessage("- interactive job monitor: `monitor()`")

	packageStartupMessage("")
	packageStartupMessage("`bsub_chunk()`/`bsub_script()`/`bsub_cmd()` should only be")
	packageStartupMessage("applied on the node that has the same file system as the computing")
	packageStartupMessage("nodes. Other functions for monitoring and cleaning jobs can be")
	packageStartupMessage("applied on any computer.")

	packageStartupMessage("==================================================================")

	packageStartupMessage(get_bconf_message(bconf))
	packageStartupMessage("==================================================================")

}


.onDetach = function(libpath) {
	if(!is.null(bsub_opt$ssh_session)) ssh_disconnect()
}

if(identical(environment(), .GlobalEnv)) {
	if(grepl("odcf|w610", Sys.info()["nodename"])) {
		config_odcf(verbose = FALSE)
	}
}


