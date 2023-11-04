
#' Connect to submisstion via ssh
#'
#' @param ... Pass to [`ssh::ssh_connect()`].
#' 
#' @importFrom ssh ssh_session_info
#' @rdname ssh
#' @export
#' @examples
#' # ssh is automatically connected. To manually connect ssh, run:
#' \dontrun{
#' ssh_connect()
#' ssh_disconnect()
#' }
#' # where the user name is the one you set in `bsub_opt$user` and
#' # the node is the one you set in `bsub_opt$login_node`.
ssh_connect = function(...) {

	if(!is.null(bsub_opt$ssh_session)) {
		if(ssh::ssh_session_info(bsub_opt$ssh_session)$connected) {
			message("ssh is already connected.")
			return(invisible(NULL))
		}
	}

	submission_node = bsub_opt$submission_node
	login_node = bsub_opt$login_node
	if(length(login_node) == 0) login_node = submission_node

	if(length(login_node) == 0) {
		stop("bsub_opt$submission_node is not defined. No node to connect.")
	}

	if(Sys.info()["nodename"] %in% login_node) {
		message("already on submission/login node. No need to reconnect.")
	}

	user = bsub_opt$user

	for(i in seq_along(login_node)) {
		message(qq("establish ssh connection to @{user}@@{login_node[i]}"))
		oe = try(session <- ssh::ssh_connect(paste0(user, "@", login_node[i]), ...))

		if(!inherits(oe, "try-error")) {
			bsub_opt$ssh_session = session
			break
		}
	}

	if(is.null(bsub_opt$ssh_session)) {
		stop("cannot connect to submission nodes.")
	}
}



#' @rdname ssh
#' @export
ssh_disconnect = function() {
	if(!is.null(bsub_opt$ssh_session)) {
		ssh::ssh_disconnect(bsub_opt$ssh_session)
	}
	bsub_opt$ssh_session = NULL
}

ssh_exec = function(cmd) {

	ssh_validate()

	if(bsub_opt$verbose) cat("========== ssh commands =============", bsub_opt$ssh_envir, cmd, sep = "\n")

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
