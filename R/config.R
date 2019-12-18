
config_odcf = function(user = NULL) {
	bsub_opt$call_Rscript = function(version) qq("module load gcc/7.2.0; module load java/1.8.0_131; module load R/@{version}; Rscript")
	bsub_opt$submission_node = c("odcf-worker01", "odcf-cn34u03s10", "odcf-cn34u03s12")
	bsub_opt$ssh_envir = c("source /etc/profile",
                           "export LSF_ENVDIR=/opt/lsf/conf",
                           "export LSF_SERVERDIR=/opt/lsf/10.1/linux3.10-glibc2.17-x86_64/etc")
	if(!is.null(user)) bsub_opt$user = user
	invisible(NULL)
}


config_sanger = function(user = NULL, group = NULL) {
	bsub_opt$call_Rscript = function(version) qq("Rscript")
	bsub_opt$submission_node = c("farm3-head3")
	
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
	invisible(NULL)
}

