message_wrap = function (..., appendLF = TRUE) {
    x = paste0(...)
    x = paste(strwrap(x), collapse = "\n")
    message(x, appendLF = appendLF)
}

stop_wrap = function (...) {
    x = paste0(...)
    x = paste(strwrap(x), collapse = "\n")
    stop(x, call. = FALSE)
}


on_submission_node = function() {
    Sys.info()["nodename"] %in% bsub_opt$submission_node
}


under_same_file_system = function() {

    if(!is.null(ENV$on_same_file_system)) {
        return(ENV$on_same_file_system)
    }

    if(on_submission_node()) {
        ENV$on_same_file_system = TRUE
        return(TRUE)
    }

    f = tempfile()
    f = paste0(f, sample(10000000, 1))
    file.create(f)

    # now check the submission node
    oe = try(ln <- run_cmd(qq("ls @{f}")), silent = TRUE)

    if(inherits(oe, "try-error")) {
        ENV$on_same_file_system = FALSE
        return(FALSE)
    } else {
        ENV$on_same_file_system = TRUE
        return(TRUE)
    }
}
