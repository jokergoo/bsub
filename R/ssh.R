
# == title
# Connect to submisstion via ssh
#
# == details
# If ssh connection is lost, run this function to reconnect.
ssh_connect = function() {

	if(!is.null(bsub_opt$ssh_session)) {
		message("ssh is already connected.")
	}

	if(Sys.info()["nodename"] %in% bsub_opt$submission_node) {
		message("already on submission node. No need to reconnect.")
	}

	user = bsub_opt$user

	if(!requireNamespace("ssh")) {
		message("You need to install ssh package.")
	}

	if(length(bsub_opt$submission_node) == 0) {
		stop("bsub_opt$submission_node is not defined.")
	}

	for(i in seq_along(bsub_opt$submission_node)) {
		message(qq("establish ssh connection to @{user}@@{bsub_opt$submission_node[i]}"))
		oe = try(session <- ssh::ssh_connect(paste0(user, "@", bsub_opt$submission_node[i])))

		if(!inherits(oe, "try-error")) {
			bsub_opt$ssh_session = session
			break
		}

		message(qq("establish ssh connection to @{user}@@{bsub_opt$submission_node[i]}, 2nd try"))
		oe = try(session <- ssh::ssh_connect(paste0(user, "@", bsub_opt$submission_node[i])))

		if(!inherits(oe, "try-error")) {
			bsub_opt$ssh_session = session
			break
		}

		message(qq("establish ssh connection to @{user}@@{bsub_opt$submission_node[i]}, 3rd try"))
		oe = try(session <- ssh::ssh_connect(paste0(user, "@", bsub_opt$submission_node[i])))

		if(!inherits(oe, "try-error")) {
			bsub_opt$ssh_session = session
			break
		}
	}

	if(is.null(bsub_opt$ssh_session)) {
		stop("cannot connect to submission nodes.")
	}
}


# == title
# Disconnect ssh connection
#
ssh_disconnect = function() {
	if(!is.null(bsub_opt$ssh_session)) {
		ssh::ssh_disconnect(bsub_opt$ssh_session)
	}
	bsub_opt$ssh_session = NULL
}

ssh_exec = function(cmd) {

	ssh_validate()

	ln = ""
	con = textConnection("ln", "w", local = TRUE)
    status = ssh::ssh_exec_wait(bsub_opt$ssh_session, command = c(
        bsub_opt$ssh_envir,
        cmd
    ), std_out = con, std_err = con)
    close(con)
    if(status) {
    	stop(paste(c("You have an error when executing remote commands", ln), collapse = "\n"))
    }
    return(ln)
}

ssh_validate = function() {
	if(is.null(bsub_opt$ssh_session)) {
		ssh_connect()
	} else {
		session = bsub_opt$ssh_session
		if(ssh::ssh_session_info(session)$connected) {
			return(NULL)
		} else {
			bsub_opt$ssh_session = NULL
			ssh_connect()
		}
	}
}
