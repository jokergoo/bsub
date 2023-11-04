
#' Configure bsub global options
#' 
#' @param verbose Whether to print messages.
#' 
#' @details
#' It sets the submission nodes, user name and how to call `Rscript`.
config_bsub = function(verbose = TRUE) {
	submission_node = readline(prompt = "What is the name of your submission node? ")
	user = readline(prompt = qq("What is your user name on @{submission_node}? "))

	bsub_opt$submission_node = submission_node
	bsub_opt$user = user

	# try to connect 
	ssh_disconnect()
	ssh_connect()

	LSF_ENVDIR = ssh_exec("echo $LSF_ENVDIR")
	LSF_SERVERDIR = ssh_exec("echo $LSF_SERVERDIR")

	bsub_opt$ssh_envir = c("source /etc/profile",
                           qq("export LSF_ENVDIR=@{LSF_ENVDIR}"),
                           qq("export LSF_SERVERDIR=@{LSF_SERVERDIR}"))

	R_version = readline(prompt = "What is the R version? ")
	bsub_opt$R_version = R_version

	call_Rscript = readline(prompt = "How is `Rscript` called, taking {version} as the variable for version? ")
	call_Rscript = gsub("\\{", "@{", call_Rscript)
	call_Rscript_fun = eval(parse(text = qq('function(version) qq("@{call_Rscript}")')))

	Rscript_call = call_Rscript_fun(paste0(R.version$major, ".", R.version$minor))
	oe = try(run_cmd(qq("@{Rscript_call} --version")), silent = TRUE)
	if(inherits(oe, "try-error")) {
		warning_wrap(qq("Cannot run `@{Rscript_call} --version` on the submission node, check your `bsub_opt$call_Rscript` global option."))
	}
	bsub_opt$call_Rscript = call_Rscript_fun

	if(verbose) {
		print(bconf)
	}
	
	invisible(NULL)
}


config_odcf = function(user = NULL, verbose = TRUE) {
	bsub_opt$call_Rscript = function(version) qq("module load gcc/7.2.0; module load java/1.8.0_131; module load R/@{version}; Rscript")
	bsub_opt$submission_node = c("bsub01.lsf.dkfz.de", "bsub02.lsf.dkfz.de")
	bsub_opt$ssh_envir = c("source /etc/profile",
                           "export LSF_ENVDIR=/opt/lsf/conf",
                           "export LSF_SERVERDIR=/opt/lsf/10.1/linux3.10-glibc2.17-x86_64/etc")
	if(!is.null(user)) bsub_opt$user = user
	if(!file.exists("~/.bsub_temp")) {
		dir.create("~/.bsub_temp", recursive = TRUE, showWarnings = FALSE)
	}
	bsub_opt$temp_dir = "~/.bsub_temp"

	ssh_disconnect()

	if(verbose) qqcat("configure for user '@{bsub_opt$user}' on node @{paste(bsub_opt$submission_node, collapse = ', ')}.\n")
	invisible(NULL)
}


config_sanger_farm3_head3 = function(user = NULL, group = NULL, verbose = TRUE) {
	bsub_opt$call_Rscript = function(version) qq("Rscript")
	bsub_opt$submission_node = c("farm3-head3")
	bsub_opt$temp_dir = "~/.bsub_temp"

	# values for LSF_SERVERDIR and LSF_ENVDIR can be get by:
	# echo $LSF_SERVERDIR
	# echo $LSF_ENVDIR
	bsub_opt$ssh_envir = c("source /etc/profile",
                           "export LSF_ENVDIR=/usr/local/lsf/conf",
                           "export LSF_SERVERDIR=/usr/local/lsf/9.1/linux2.6-glibc2.3-x86_64/etc")
	if(!is.null(user)) bsub_opt$user = user
	if(is.null(group) && is.null(bsub_opt$group)) {
		bsub_opt$bsub_template = function(name, hours, memory, cores, output, ...) {
			qq("bsub -J '@{name}' -W '@{hours}:00' -n @{cores} -R 'select[mem>@{round(memory*1024)}] rusage[mem=@{round(memory*1024)}]' -M@{round(memory*1024)} -o '@{output}'")
		}
	} else {
		if(is.null(group) && !is.null(bsub_opt$group)) group = bsub_opt$group
		bsub_opt$bsub_template = function(name, hours, memory, cores, output, ...) {
			qq("bsub -J '@{name}' -W '@{hours}:00' -n @{cores} -R 'select[mem>@{round(memory*1024)}] rusage[mem=@{round(memory*1024)}]' -M@{round(memory*1024)} -G @{group} -o '@{output}'")
		}
	}

	ssh_disconnect()

	if(verbose) qqcat("configure for user '@{bsub_opt$user}' on node @{paste(bsub_opt$submission_node, collapse = ', ')}.\n")
	invisible(NULL)
}


config_sanger = function(user = NULL, ssh_key = "~/.ssh/id_rsa", verbose = TRUE) {
	bsub_opt$call_Rscript = function(version) qq("Rscript")
	bsub_opt$login_node = "ssh.sanger.ac.uk"
	bsub_opt$submission_node = NULL
	bsub_opt$temp_dir = "~/.bsub_temp"

	# values for LSF_SERVERDIR and LSF_ENVDIR can be get by:
	# echo $LSF_SERVERDIR
	# echo $LSF_ENVDIR
	if(!is.null(user)) bsub_opt$user = user

	ssh_envir = "source /etc/profile; export LSF_ENVDIR=/usr/local/lsf/conf;export LSF_SERVERDIR=/usr/local/lsf/9.1/linux2.6-glibc2.3-x86_64/etc"

	bsub_opt$ssh_envir = c(
		"source /etc/profile",
		qq("alias bjobs=\"ssh -i @{ssh_key} @{bsub_opt$user}@farm3-head3 '@{ssh_envir};bjobs'\""),
		qq("alias bparam=\"ssh -i @{ssh_key} @{bsub_opt$user}@farm3-head3 '@{ssh_envir};bparam'\""),
		qq("alias bkill=\"ssh -i @{ssh_key} @{bsub_opt$user}@farm3-head3 '@{ssh_envir};bkill'\"")
	)

	ssh_disconnect()

	if(verbose) qqcat("configure for user '@{bsub_opt$user}' on node @{paste(bsub_opt$login_node, collapse = ', ')}.\n")
	invisible(NULL)
}


config_two_ssh = function(user = NULL) {
	bsub_opt$call_Rscript = function(version) qq("module load gcc/7.2.0; module load java/1.8.0_131; module load R/@{version}; Rscript")
	bsub_opt$login_node = "odcf-worker02"
	bsub_opt$submission_node = NULL
	
	# values for LSF_SERVERDIR and LSF_ENVDIR can be get by:
	# echo $LSF_SERVERDIR
	# echo $LSF_ENVDIR
	if(!is.null(user)) bsub_opt$user = user

	ssh_envir = "source /etc/profile; export LSF_ENVDIR=/opt/lsf/conf;export LSF_SERVERDIR=/opt/lsf/10.1/linux3.10-glibc2.17-x86_64/etc"

	bsub_opt$ssh_envir = c(
		"source /etc/profile",
		qq("alias bjobs=\"ssh -i ~/.ssh/id_rsa_dkfz_heidelberg @{bsub_opt$user}@odcf-cn34u03s12 '@{ssh_envir};bjobs'\""),
		qq("alias bparam=\"ssh -i ~/.ssh/id_rsa_dkfz_heidelberg @{bsub_opt$user}@odcf-cn34u03s12 '@{ssh_envir};bparam'\""),
		qq("alias bkill=\"ssh -i ~/.ssh/id_rsa_dkfz_heidelberg @{bsub_opt$user}@odcf-cn34u03s12 '@{ssh_envir};bkill'\"")
	)

	ssh_disconnect()

	qqcat("configure for user '@{bsub_opt$user}' on node @{paste(bsub_opt$login_node, collapse = ', ')}.\n")
	invisible(NULL)
}

