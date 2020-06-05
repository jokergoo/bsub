
# == title
# Get the dependency of current jobs
#
# == param
# -job_tb A table from `bjobs`. Optional.
#
# == value
# If there is no dependency of all jobs, it returns ``NULL``. If there are dependencies,
# it returns a list of three elements:
#
# -``dep_mat``: a two column matrix containing dependencies from parents to children.
# -``id2name``: a named vector containing mapping from job IDs to job names.
# -``id2stat``: a named vector containing mapping from job IDs to job status.
#
# == example
# \dontrun{
# get_dependency()
# }
get_dependency = function(job_tb = NULL) {

    if(is.null(job_tb)) job_tb = bjobs(status = "all", print = FALSE)
    
    job_tb = job_tb[, c("JOBID", "STAT", "JOB_NAME", "DEPENDENCY"), drop = FALSE]

    id2name = structure(job_tb$JOB_NAME, names = job_tb$JOBID)
    id2stat = structure(as.character(job_tb$STAT), names = job_tb$JOBID)

    job_tb2 = job_tb[job_tb$DEPENDENCY != "-", , drop = FALSE]
    
    if(nrow(job_tb2) == 0) {
        return(NULL)
    }
    
    dep = lapply(strsplit(job_tb2$DEPENDENCY, " && "), function(x) {
        gsub("^done\\( (\\d+) \\)$", "\\1", x)
    })

    n = sapply(dep, length)
    dep_mat = cbind(parent = as.character(unlist(dep)),
                    child = as.character(rep(job_tb2$JOBID, times = n)))

    all_nodes = unique(dep_mat)
    id2name = id2name[all_nodes]; names(id2name) = all_nodes
    id2stat = id2stat[all_nodes]; names(id2stat) = all_nodes

    return(list(dep_mat = dep_mat, id2name = id2name, id2stat = id2stat))
}

# == title
# Plot the job dependency tree
#
# == param
# -job_id A job ID.
# -job_tb A table from `bjobs`. Optional.
#
# == value
# No value is returned.
#
# == example
# \dontrun{
# job1 = random_job()
# job2 = random_job()
# job3 = random_job(dependency = c(job1, job2))
# plot_dependency(job3)
# }
plot_dependency = function(job_id, job_tb = NULL) {

    job_id = as.character(job_id)

    job_dep = get_dependency(job_tb = NULL)

    if(is.null(job_dep)) {
        # no dependency
        plot(NULL, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, ann = FALSE)
        text(0.5, 0.5, qq("no dependency for job @{job_id}"))
        return(invisible(NULL))
    }

    if(!job_id %in% names(job_dep$id2name)) {
        # no dependency
        plot(NULL, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, ann = FALSE)
        text(0.5, 0.5, qq("no dependency for job @{job_id}"))
        return(invisible(NULL))
    }

    g = igraph::graph.edgelist(job_dep$dep_mat)
    g2 = igraph::graph.edgelist(job_dep$dep_mat, directed = FALSE)

    dist = igraph::distances(g2, v = job_id)

    # node in the connected sub-graph
    nodes = names(which(apply(dist, 2, function(x) any(is.finite(x)))))

    g = igraph::induced_subgraph(g, nodes)

    node_id = igraph::V(g)$name
    n_node = length(node_id)
    node_label = job_dep$id2name[node_id]
    node_label[is.na(node_label)] = "unknown"

    l = nchar(node_label) > 50
    if(any(l)) {
        foo = substr(node_label[l], 1, 48)
        foo = paste(foo, "..", sep = "")
        node_label[l] = foo
    }

    label_width = pmax(nchar(igraph::V(g)$name), nchar(node_label)+2)
    node_label = paste0(igraph::V(g)$name, "\n", "<", node_label, ">")
    node_stat = job_dep$id2stat[igraph::V(g)$name]
    node_stat[is.na(node_stat)] = "unknown"

    stat_col = c("blue", "purple", "black", "red", "grey")
    names(stat_col) = c("RUN", "PEND", "DONE", "EXIT", "unknown") 
    node_color = stat_col[node_stat]
    names(node_color) = node_id
    node_fill = rgb(t(col2rgb(node_color)/255), alpha = 0.2)
    names(node_fill) = node_id

    node_width = (5*label_width - 5)/5/5 * 1.2
    node_height = rep(1, n_node)

    node_shape = rep("rectangle", n_node)

    node_lwd = ifelse(node_id == job_id, 4, 1)

    requireNamespace("graph")
    g2 = new("graphAM", as.matrix(igraph::get.adjacency(g)), edgemode = "directed")

    names(node_width) = node_id
    names(node_height) = node_id
    names(node_label) = node_id
    names(node_shape) = node_id
    names(node_lwd) = node_id
    nAttr = list(width = node_width, height = node_height, label = node_label, shape = node_shape)
    x = Rgraphviz::layoutGraph(g2, nodeAttrs = nAttr)
    graph::nodeRenderInfo(x) = list(color = node_color, fill = node_fill, cex = 1, lwd = node_lwd)
    
    op = par(no.readonly = TRUE)
    on.exit(par(op))

    par(xpd = NA)
    layout(matrix(1:2, ncol = 1), heights = c(1, "2 cm"))
    Rgraphviz::renderGraph(x)

    usr = par("usr")
    par(mar = c(0, 0, 0, 0))
    plot(NULL, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, ann = FALSE)
    legend(x = 0.5, y = 0.5, xjust = 0.5, yjust = 0.5,
        pch = 0, col = stat_col, legend = names(stat_col), 
        pt.cex = 2, bty = "n", ncol = length(stat_col))
    layout(1)
}
