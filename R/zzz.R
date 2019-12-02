 



.onAttach = function(libname, pkgname) {

	packageStartupMessage("-----------------------------------")

	if(!file.exists("~/.bsub_temp")) {
	    packageStartupMessage("create temp_dir: ~/.bsub_temp")
	    dir.create("~/.bsub_temp", recursive = TRUE, showWarnings = FALSE)
	}

	all_f = list.files("~/.bsub_temp/")
	if(length(all_f)) {
		packageStartupMessage(paste0("There are ", length(all_f), " temporary files in ~/.bsub_temp"))
	}

	if(grepl("odcf", Sys.info()["nodename"])) {
		packageStartupMessage("On ODCF cluster, set general environment.")
		config_odcf()
	}
	
	packageStartupMessage("")
	packageStartupMessage("- submit R code: `bsub_chunk()`")
	packageStartupMessage("- submit R script: `bsub_script()`")
	packageStartupMessage("- submit shell commands: `bsub_cmd()`")
	packageStartupMessage("- kill jobs: `bkill()`")
	packageStartupMessage("- view job summary: `bjobs`/`brecent`")
	packageStartupMessage("- view job log: `job_log()`")
	packageStartupMessage("-----------------------------------")

}


.onDetach = function(libpath) {
	if(!is.null(bsub_opt$ssh_session)) ssh_disconnect()
}


config_odcf = function() {
	bsub_opt$call_Rscript = function(version) qq("module load gcc/7.2.0; module load java/1.8.0_131; module load R/@{version}; Rscript")
	bsub_opt$submission_node = c("odcf-worker01", "odcf-cn34u03s10", "odcf-cn34u03s12")
	bsub_opt$ssh_envir = c("source /etc/profile",
                           "export LSF_ENVDIR=/opt/lsf/conf",
                           "export LSF_SERVERDIR=/opt/lsf/10.1/linux3.10-glibc2.17-x86_64/etc")
}

if(identical(environment(), .GlobalEnv)) {
	if(grepl("odcf", Sys.info()["nodename"])) {
		message("On ODCF cluster, set general environment.")
		config_odcf()
	}
}
