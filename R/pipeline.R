

# cmd1 = "echo 1 > @{test_file}"
# cmd2 = "echo 2 >> @{test_file}"
# cmd3 = "echo 3 >> @{test_file}"

# job1 = job_cmd(cmd1, name = "job1")
# job2 = job_cmd(cmd2, name = "job2")
# job3 = job_cmd(cmd3, name = "job3")

# jobs = list(job1 = job1, job2 = job2, job3 = job3)


# dep = pipeline_dep(
# 	"job1 -> job2",
#     "job2 -> job3"
# )

# test_file = "~/test.txt"
# pipeline(dep = dep, jobs = jobs)

job_cmd = function(cmd, name, sh_head = "", ...) {
	args = list(...)
	args$cmd = cmd
	args$name = name
	args$sh_head = ""
	args$ask = FALSE
	args
}

pipeline_dep = function(...) {
	dep = unlist(list(...))
	lt = strsplit(dep, "\\s*->?\\s*")

	dep = cbind(from = sapply(lt, function(x) x[1]),
		        to = sapply(lt, function(x) x[2]))

	all_jobs = unique(as.vector(dep))
	n_jobs = length(all_jobs)
	job2ind = structure(seq_along(all_jobs), names = all_jobs)

	lt_parents2 = split(unname(job2ind[dep[, 1]]), dep[, 2])
	lt_children2 = split(unname(job2ind[dep[, 2]]), dep[, 1])
	
	lt_parents = rep(list(integer(0)), n_jobs)
	lt_children = lt_parents
	lt_parents[ job2ind[names(lt_parents2)] ] = lt_parents2
	lt_children[ job2ind[names(lt_children2)] ] = lt_children2

	roots = which(vapply(lt_parents, length, FUN.VALUE = integer(1)) == 0)

	depth = dag_depth_bfs(lt_children, roots)

	list(all_jobs = all_jobs,
		 n_jobs = n_jobs,
		 lt_children = lt_children, 
		 lt_parents = lt_parents,
		 roots = roots,
		 depth = depth)

}

pipeline = function(dep, jobs, prefix = "", suffix = random_str(), 
	envir = parent.frame(), vars = NULL) {

	jobs_not_defined = setdiff(dep$all_jobs, names(jobs))
	if(length(jobs_not_defined)) {
		stop_wrap(qq("Job: @{paste(jobs_not_defined, collapse = ', ')} not defined."))
	}

	depth = dep$depth

	if(!is.null(vars)) {
		if(is.list(vars) || is.environment(vars)) {
			stop_wrap("If `vars` is set, it should be a list or an environment object.")
		}
	}

	job_submitted = rep(NA, dep$n_jobs)

	for(i in order(depth)) {  # tpl ordered
		if(is.na(job_submitted[i])) { 
			from = dep$lt_parents[[i]] 
			if(length(from)) {
				jobs[[i]]$dependency = job_submitted[from]
			}

			id = .run_job(jobs[[i]], prefix = prefix, suffix = suffix, envir = envir, vars = vars)
			job_submitted[[i]] = id
		}
	}
}

random_str = function() {
	paste0(sample(c(letters, LETTERS), 6), collapse = "")
}


dag_depth_bfs = function(lt_children, roots) {

	n_terms = length(lt_children)

	d = rep(0L, n_terms)
	current_nodes = roots
	while(length(current_nodes)) {
		for(cr in current_nodes) {
			children = lt_children[[cr]]
			if(length(children)) {
				d[children] = base::pmax(d[children], d[cr] + 1L)
			}
		}
		current_nodes = unique(unlist(lt_children[current_nodes]))
	}
	d
}

.run_job = function(args, prefix, suffix, envir, vars = NULL) {
	args$name = paste0(prefix, args$name, suffix)
	if(is.null(vars)) {
		args$cmd = GetoptLong::qq(args$cmd, envir = envir)
		args$sh_head = GetoptLong::qq(args$sh_head, envir = envir)
	} else {
		args$cmd = GetoptLong::qq(args$cmd, envir = as.environment(vars))
		args$sh_head = GetoptLong::qq(args$sh_head, envir = as.environment(vars))
	}

	do.call(bsub_cmd, args)
}
