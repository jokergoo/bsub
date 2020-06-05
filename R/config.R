
config_odcf = function(user = NULL, verbose = TRUE) {
	bsub_opt$call_Rscript = function(version) qq("module load gcc/7.2.0; module load java/1.8.0_131; module load R/@{version}; Rscript")
	bsub_opt$submission_node = c("odcf-worker01", "odcf-cn34u03s10", "odcf-cn34u03s12")
	bsub_opt$ssh_envir = c("source /etc/profile",
                           "export LSF_ENVDIR=/opt/lsf/conf",
                           "export LSF_SERVERDIR=/opt/lsf/10.1/linux3.10-glibc2.17-x86_64/etc")
	if(!is.null(user)) bsub_opt$user = user
	if(!file.exists("~/.bsub_temp")) {
		dir.create("~/.bsub_temp", recursive = TRUE, showWarnings = FALSE)
	}
	bsub_opt$temp_dir = "~/.bsub_temp"

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
		bsub_opt$bsub_template = function(name, hour, memory, core, output, ...) {
			qq("bsub -J '@{name}' -W '@{hour}:00' -n @{core} -R 'select[mem>@{round(memory*1024)}] rusage[mem=@{round(memory*1024)}]' -M@{round(memory*1024)} -o '@{output}'")
		}
	} else {
		if(is.null(group) && !is.null(bsub_opt$group)) group = bsub_opt$group
		bsub_opt$bsub_template = function(name, hour, memory, core, output, ...) {
			qq("bsub -J '@{name}' -W '@{hour}:00' -n @{core} -R 'select[mem>@{round(memory*1024)}] rusage[mem=@{round(memory*1024)}]' -M@{round(memory*1024)} -G @{group} -o '@{output}'")
		}
	}
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

	if(verbose) qqcat("configure for user '@{bsub_opt$user}' on node @{paste(bsub_opt$login_node, collapse = ', ')}.\n")
	invisible(NULL)
}


config_two_ssh = function(user = NULL) {
	bsub_opt$call_Rscript = function(version) qq("module load gcc/7.2.0; module load java/1.8.0_131; module load R/@{version}; Rscript")
	bsub_opt$login_node = "odcf-worker01"
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

	qqcat("configure for user '@{bsub_opt$user}' on node @{paste(bsub_opt$login_node, collapse = ', ')}.\n")
	invisible(NULL)
}

