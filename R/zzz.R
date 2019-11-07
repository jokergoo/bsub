 
.onAttach = function(libname, pkgname) {
	if(!file.exists("~/.bsub_temp")) {
	    packageStartupMessage("create temp_dir: ~/.bsub_temp\n")
	    dir.create("~/.bsub_temp", recursive = TRUE, showWarnings = FALSE)
	}

	all_f = list.files("~/.bsub_temp/")
	if(length(all_f)) {
		packageStartupMessage(paste0("There are ", length(all_f), " temporary files in ~/.bsub_temp"))
	}
}


.onDetach = function(libpath) {
	if(!is.null(bsub_opt$ssh_session)) ssh_disconnect()
}
