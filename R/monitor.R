

#' Browser-based interactive job monitor 
#'
#' @details
#' The monitor is implemented as a shiny app.
#'
#' @examples
#' \dontrun{
#' monitor()
#' }
monitor = function() {

    pkgs = c("shiny", "DT", "shinyjqui", "ggplot2", "igraph", "Rgraphviz")
    l = sapply(pkgs, requireNamespace, quietly = TRUE)

    if(any(!l)) {
        stop_wrap("You need to install '@{paste(pkgs[!l], collapse = ',')}' to use the monitor.")
    }
    
    if(!on_submission_node()) {
        ssh_validate()
    }

    suppressPackageStartupMessages(shiny::runApp(system.file("app", package = "bsub")))

    # if(identical(topenv(), asNamespace("bsub"))) {
    #     if(bsub_opt$verbose) cat("run job monitor from the package.\n")
    #     suppressPackageStartupMessages(shiny::runApp(system.file("app", package = "bsub")))
    # } else if(grepl("odcf", Sys.info()["nodename"])) {
    #     if(bsub_opt$verbose) cat("run job monitor from odcf node.\n")
    #     suppressPackageStartupMessages(shiny::runApp("/desktop-home/guz/project/development/bsub/inst/app"))
    # } else if(grepl("w610", Sys.info()["nodename"])) {
    #     if(bsub_opt$verbose) cat("run job monitor from w610 node.\n")
    #     suppressPackageStartupMessages(shiny::runApp("~/project/development/bsub/inst/app"))
    # } else {
    #     if(bsub_opt$verbose) cat("run job monitor from local laptop.\n")
    #     suppressPackageStartupMessages(shiny::runApp("~/project/development/bsub/inst/app"))
    # }
}

#' Visualize statistics of jobs
#' 
#' @param status Status of the jobs. Use "all" for all jobs.
#' @param filter Regular expression to filter on job names.
#' @param job_tb A data frame returned by [`bjobs()`]. Only internally used.
#'
#' @details
#' `bjobs_barplot()` draws barplots of number of jobs per day.
#' `bjobs_timeline()` draws segments of duration of jobs. In the plot, each segment represents
#' a job and the width of the segment correspond to its duration.
#'
#' @rdname visualize
#' @importFrom graphics text
#' @export
bjobs_barplot = function(status = c("RUN", "EXIT", "PEND", "DONE"), filter = NULL, job_tb = NULL) {
    if(is.null(job_tb)) {
        job_tb = bjobs(status = status, filter = filter, print = FALSE)
    } else {
        job_tb = job_tb[job_tb$STAT %in% status , , drop = FALSE]
    }
    job_tb$STAT = as.character(job_tb$STAT)
    job_tb$STAT[!job_tb$STAT %in% status] = "Others"
    job_tb$STAT = factor(job_tb$STAT, levels = intersect(c(status, "Others"), as.character(job_tb$STAT)))

    if(nrow(job_tb) == 0) {
        plot(NULL, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, ann = FALSE)
        text(0.5, 0.5, "No job was found.")
        return(invisible(NULL))
    }

    suppressWarnings(p <- ggplot2::ggplot(job_tb, ggplot2::aes(x = as.Date(job_tb$SUBMIT_TIME), fill = job_tb$STAT)) + ggplot2::geom_bar(position=ggplot2::position_dodge()) +
        ggplot2::xlab("Submitted time") + ggplot2::ylab("Number of jobs") + ggplot2::labs(fill = "Status")
    )
    p = p + ggplot2::scale_fill_manual(breaks = names(STATUS_COL), values = STATUS_COL)
    tb = table(job_tb$STAT)
    tb =  tb[tb > 0]
    p = p + ggplot2::ggtitle(paste0(paste(qq("@{tb} @{names(tb)} job@{ifelse(tb == 1, '', 's')}", collapse = FALSE), collapse = ", "), " within one week"))
    print(p)
}

#' @rdname visualize
#' @importFrom graphics axis box legend par segments text title
#' @export
bjobs_timeline = function(status = c("RUN", "EXIT", "PEND", "DONE"), filter = NULL, job_tb = NULL) {
    if(is.null(job_tb)) {
        job_tb = bjobs(status = status, filter = filter, print = FALSE)
    } else {
        job_tb = job_tb[job_tb$STAT %in% status, , drop = FALSE]
    }
    job_tb$STAT = as.character(job_tb$STAT)
    job_tb$STAT[!job_tb$STAT %in% status] = "Others"
    job_tb$STAT = factor(job_tb$STAT, levels = intersect(c(status, "Others"), as.character(job_tb$STAT)))
    
    if(nrow(job_tb) == 0) {
        plot(NULL, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, ann = FALSE)
        text(0.5, 0.5, "No jobs found.")
        return(invisible(NULL))
    }

    units(job_tb$TIME_PASSED) = "secs"

    x1 = as.numeric(job_tb$START_TIME)
    x2 = x1 + as.numeric(job_tb$TIME_PASSED)
    now = as.numeric(Sys.time())
    x2[is.na(x2)] = now
    y = runif(nrow(job_tb))

    xlim = c(min(x1, na.rm = TRUE), max(x2, na.rm = TRUE))
    plot(NULL, xlim = xlim, ylim = c(0, 1), axes = FALSE, ann = FALSE)
    segments(x1, y, x2, y, col = STATUS_COL[as.character(job_tb$STAT)], lwd = 2)
    at = unique(as.Date(c(job_tb$START_TIME, job_tb$FINISH_TIME)))
    labels = as.character(at)
    at = as.numeric(as.POSIXlt(at))
    l = at >= xlim[1] & at <= xlim[2]
    at = at[l]
    labels = labels[l]
    at = c(at, now)
    labels = c(labels, "now")
    box()
    axis(side = 1, at = at, labels = labels)
    op = par("xpd")
    par(xpd = NA)
    col = STATUS_COL[intersect(names(STATUS_COL), as.character(job_tb$STAT))]
    legend(x = mean(par("usr")[1:2]), y = mean(par("usr")[4]), legend = names(col), 
        col = col, lty = 1, lwd = 2, ncol = length(col), xjust = 0.5, yjust = 0, cex = 0.6)

    tb = table(job_tb$STAT)
    tb =  tb[tb > 0]
    title(paste0(paste(qq("@{tb} @{names(tb)} job@{ifelse(tb == 1, '', 's')}", collapse = FALSE), collapse = ", "), " within one week"))

    par(xpd = op)
}

