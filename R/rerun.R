
#' Rerun jobs
#' 
#' @param job_id A single job ID. In `pipeline_rerun()`, it is the job ID of any one job in the pipeline.
#' @param dependency A vector of job IDs that current job depends on.
#' @param verbose Whether to print messages.
#' @param job_tb The data frame returned from [`bjobs()`], internally used.
#' 
#' @returns
#' `job_rerun()` returns the job IDs. `pipeline_rerun()` returns `NULL`.
#' @rdname rerun
#' @export
job_rerun = function(job_id, dependency = character(0), verbose = TRUE, job_tb = NULL) {

	if(is.null(job_tb)) {
		job_tb = bjobs(status = "all", print = FALSE, job_id = job_id)
	} else {
		job_tb = job_tb[job_tb$JOBID == job_id, , drop = FALSE]
	}

	if(verbose) message(qq("rerun job @{job_id} <@{job_tb$JOB_NAME}>"))
	if(verbose) message("  - obtain submission script")

	log = run_cmd(qq("bjobs -script @{job_id}"))
    i1 = grep("^# LSBATCH: User input", log)
    source = FALSE
    if(log[i1+1] == "( cat <<_USER_\\SCRIPT_") {
        i1 = i1 + 1
        source = TRUE
    }
    if(source) {
        i2 = which(log == "_USER_SCRIPT_")
        script = log[seq(i1+1, i2-1)]
    } else {
        i2 = grep("^# LSBATCH: End user input", log)
        script = log[seq(i1+1, i2-1-2)]
    }

    if(source) {
    	i1 = which(log == "##### commands start #####")
    	if(length(i1)) {
    		i2 = which(log == "##### commands end #####")
    		script = log[seq(i1+1, i2-1)]
    	}
    }

    if(verbose) message("  - obtain environment variables")
    ev = job_env(job_id)
    # ev = ev[names(ev) %in% env_var]
	
	if(verbose) message("  - obtain resource requirement")

    name = job_tb$JOB_NAME
    mem = as.numeric(gsub("^.*mem=(.*?)\\].*$", "\\1", job_tb$COMBINED_RESREQ))
    cores = job_tb$NREQ_SLOT
    walltime = job_tb$RUNTIMELIMIT
    walltime = ceiling(walltime)

    mem = ceiling(mem/1024)
    hour = ceiling(walltime/60)
     
    output_dir = dirname(job_output_file_by_id(job_id))
    gl = job_attached_vars_by_id(job_id)
    if(is.na(gl$temp_dir)) {
    	temp_dir = output_dir
    } else {
    	temp_dir = gl$temp_dir
    }

    if(under_same_file_system()) {

    	if(verbose) message("  - save into a temporary bash file")
    	sh_file = tempfile(fileext = ".sh")
    
	    con = file(sh_file, "w")
	    writeLines(ev, con)
	    writeLines("\n", con)
	    writeLines(script, con)
	    close(con)

	    if(verbose) message("  - directly submit the new job")
		job_id = bsub_cmd(sh = sh_file, name = name, memory = mem, hours = hour, cores = cores, dependency = dependency, ask = FALSE)
		file.remove(sh_file)

    } else {
    	if(verbose) message("  - save into a temporary bash file")
    	t = as.POSIXlt(Sys.time())
	    t = as.numeric(t) + t$sec - floor(t$sec)
	    t = gsub("\\.", "_", t)
	    name_uid = paste0(name, "_", t)
	    output = qq("@{output_dir}/@{name_uid}.out")

    	# upload to submission node
    	local_sh_file = tempfile(paste0(name_uid, "_"), tmpdir = tempdir(), fileext = ".sh")
    	con = file(local_sh_file, "w")
	    writeLines(ev, con)
	    writeLines("\n", con)
	    writeLines(script, con)
	    close(con)

	    if(verbose) message("  - upload to the submission node")
	    sh_file = paste0(temp_dir, "/", basename(local_sh_file))
    	ssh::scp_upload(bsub_opt$ssh_session, local_sh_file, to = sh_file, verbose = FALSE)
    	file.remove(local_sh_file)

    	if(verbose) message("  - submit the new job")
    	cmd = bsub_opt$bsub_template(name, hour, mem, cores, output, bsub_opt$group)
    	if(length(dependency)) {
	        dependency_str = paste( paste("done(", dependency, ")"), collapse = " && " )
	        cmd = qq("@{cmd} -w '@{dependency_str}' \\\n")
	    }
    	cat(silver(qq("@{cmd} < '@{sh_file}'")), "\n")

	    # add values of name_uid and temp_dir to `bjobs` in job description
	    cmd = qq("@{cmd} -env 'all,R_BSUB_NAME_UID=@{name_uid},R_BSUB_TEMP_DIR=@{temp_dir}' -Jd 'R_BSUB_NAME_UID=@{name_uid},R_BSUB_TEMP_DIR=@{temp_dir}'")
	    cmd = qq("@{cmd} < '@{sh_file}'; rm @{sh_file}")
	    txt = run_cmd(cmd, print = FALSE)

	    job_id = gsub("^.*<(\\d+)>.*$", "\\1", txt)

    }
    
    job_id
}

job_env = function(job_id) {
	ev = run_cmd(qq("bjobs -env @{job_id}"))
	ev = ev[!grepl("\\(\\)=", ev)]
	ev = ev[!grepl("^}", ev)]
	ev = ev[!grepl("=$", ev)]
	ev = ev[!grepl(" ", ev)]
	ev = ev[!grepl("R_BSUB_NAME_UID", ev)]
	ev = ev[!grepl("R_BSUB_TEMP_DIR", ev)]

	env_var_name = gsub("^(\\S+)=.*$", "\\1", ev)
	names(ev) = env_var_name
	ev
}

#' @param skip_done Whether to skip done jobs.
#' 
#' @details
#' In `pipeline_rerun()`, the full set of jobs can be captured by one job in the pipeline.
#' 
#' @rdname rerun
#' @export
pipeline_rerun = function(job_id, skip_done = TRUE, verbose = TRUE) {

	if(verbose) message("rerun pipeline")
	job_tb = bjobs(status = "all", print = FALSE)
	g = job_dependency_igraph(job_id, job_tb)
	all_job_ids = V(g)$name

	job_tb2 = job_tb[job_tb$JOBID %in% all_job_ids, , drop = FALSE]
	rownames(job_tb2) = job_tb2$JOBID
	job_tb2 = job_tb2[order(as.numeric(job_tb2$JOBID)), , drop = FALSE]

	l_run_pend = job_tb2$STAT %in% c("RUN", "PEND")
	if(any(l_run_pend)) {
		message("Found following RUN/PEND jobs:")
		for(i in which(l_run_pend)) {
			message(qq("  job @{job_tb2$JOBID[i]} <@{job_tb2$JOB_NAME[i]}>, @{job_tb2$STAT[i]}"))
		}
		stop_wrap("There should be no RUN/PEND job if rerun the pipeine.")
	}

	if(length(all_job_ids) == 1) {
		if(job_tb2$STAT[1] == "DONE") {
			if(skip_done) {
				message(qq("skip DONE job @{job_tb2$JOBID[1]} <@{job_tb2$JOB_NAME[1]}>"))
			} else {
				job_rerun(job_id, verbose = verbose, job_tb = job_tb)
			}
		} else {
			job_rerun(job_id, verbose = verbose, job_tb = job_tb)
		}
		return(invisible(NULL))
	}

	nr = nrow(job_tb2)
	done_jobs = structure(rep(FALSE, nr), names = job_tb2$JOBID)
	done_jobs[job_tb2$STAT == "DONE"] = TRUE

	new_job_id = character(0)
	for(i in seq_len(nr)) {
		if(job_tb2$STAT[i] == "DONE" && skip_done) {
			# skip
			if(verbose) {
				message(qq("skip DONE job @{job_tb2$JOBID[i]} <@{job_tb2$JOB_NAME[i]}>"))
			}
		} else {
			d = distances(g, which(V(g)$name == job_tb2$JOBID[i]), to = V(g), mode = "in")
			p = colnames(d)[which(d == 1)]
			if(skip_done) {
				dj = done_jobs[p]
				dj = dj[dj]
				p = setdiff(p, names(dj))
			}
			dep = character(0)
			if(length(p)) {
				dep = new_job_id[p]
			}
			jid = job_rerun(job_tb2$JOBID[i], dependency = dep, verbose = verbose, job_tb = job_tb)
			new_job_id[as.character(job_tb2$JOBID[i])] = jid
		}
	}

	return(invisible(NULL))
}
