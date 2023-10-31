
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

    job_tb$DEPENDENCY = gsub("\\w+\\((.*?)\\)", "\\1", job_tb$DEPENDENCY)
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
# The DOT code of a job dependency graph
#
# == param 
# -job_id A job id.
# -job_tb A table from `bjobs`. Optional.
#
dep_dot = function(job_id, job_tb = NULL) {
    job_id = as.character(job_id)

    job_dep = get_dependency(job_tb = NULL)

    g = igraph::graph.edgelist(job_dep$dep_mat)
    g2 = igraph::graph.edgelist(job_dep$dep_mat, directed = FALSE)

    dist = igraph::distances(g2, v = job_id)

    # node in the connected sub-graph
    nodes = names(which(apply(dist, 2, function(x) any(is.finite(x)))))

    g = igraph::induced_subgraph(g, nodes)
    V(g)$label = job_dep$id2name[V(g)$name]
    V(g)$status = job_dep$id2stat[V(g)$name]

    stat_col = STATUS_COL

    ## the dot code
    color = stat_col[V(g)$status]
    fontsize = 10
    fontcolor = "black"
    penwidth = rep(1, length(color))
    penwidth[V(g)$name == job_id] = 3
    nodes = paste0(
        qq("  node [fontname=Helvetical]\n"),
        qq("  \"@{V(g)$name}\" [color=\"@{color}\", penwidth=@{penwidth} fontsize=@{fontsize}, fontcolor=\"@{fontcolor}\", tooltip=\"@{V(g)$label}\"];\n", collapse = TRUE)
    )

    edgelist = get.edgelist(g)
    edges = paste0(
        "  edge []\n",
        qq("  \"@{edgelist[, 1]}\" -> \"@{edgelist[, 2]}\";\n", collapse = TRUE)
    )

    DOT = paste0(
        "digraph {\n",
        "  graph [overlap = true]\n",
        "\n",
        nodes,
        "\n",
        edges,
        "}\n"
    )

    DOT
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
    dot = dep_dot(job_id, job_tb)
    DiagrammeR::grViz(dot)
}
