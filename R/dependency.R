
#' Job dependencies
#'
#' @param job_id A single job ID.
#' @param job_tb A data frame from [`bjobs()`]. Internally used.
#' @param ... Pass to [`DiagrammeR::grViz()`], such as the size of the html widget.
#'
#' @returns
#' `job_dependency_all()` returns a list that contains three elements:
#'
#' - `dep_mat`: a two column matrix containing dependencies from parents to children.
#' - `id2name`: a named vector containing mapping from job IDs to job names.
#' - `id2stat`: a named vector containing mapping from job IDs to job status.
#'
#' `job_dependency_igraph()` returns a [`igraph::igraph`] object which contains a dependency
#' graph induced by the input job ID.
#' 
#' `job_dependency_dot()` returns a DOT code for GraphViz visualization.
#' 
#' `job_dependency_diagram()` makes a HTML-based dependency diagram.
#' 
#' @rdname dependency
#' @export
#' @examples
#' \dontrun{
#' job1 = random_job()
#' job2 = random_job()
#' job3 = random_job(dependency = c(job1, job2))
#' 
#' job_dependency_all()
#' job_dependency_igraph(job3)
#' cat(job_dependency_dot(job3))
#' job_dependency_diagram(job3)
#' }
job_dependency_all = function(job_tb = NULL) {

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
        gsub(" ", "", gsub("^done\\( (\\d+) \\)$", "\\1", x))
    })

    n = sapply(dep, length)
    dep_mat = cbind(parent = as.character(unlist(dep)),
                    child = as.character(rep(job_tb2$JOBID, times = n)))

    return(list(dep_mat = dep_mat, id2name = id2name, id2stat = id2stat))
}

#' @rdname dependency
#' @export
#' @import igraph
job_dependency_igraph = function(job_id, job_tb = NULL) {
   
    job_id = as.character(job_id)
    job_dep = job_dependency_all(job_tb = job_tb)

    if(!job_id %in% job_dep$dep_mat) {
        g = induced_subgraph( make_graph(c(job_id, "foo")), vids = 1)
    } else {

        g = graph.edgelist(job_dep$dep_mat)
        g2 = graph.edgelist(job_dep$dep_mat, directed = FALSE)

        dist = distances(g2, v = job_id)

        # node in the connected sub-graph
        nodes = names(which(apply(dist, 2, function(x) any(is.finite(x)))))

        g = induced_subgraph(g, nodes)

    }
    V(g)$label = job_dep$id2name[V(g)$name]
    V(g)$status = job_dep$id2stat[V(g)$name]
    g
}

#' @rdname dependency
#' @importFrom grDevices col2rgb rgb
#' @export
job_dependency_dot = function(job_id, job_tb = NULL) {
        
    job_id = as.character(job_id)
    g = job_dependency_igraph(job_id, job_tb)

    ## the dot code
    color = STATUS_COL[V(g)$status]
    color = rgb(t(col2rgb(color)/255))
    fontsize = 10
    fontcolor = "black"
    penwidth = rep(1, length(color))
    penwidth[V(g)$name == job_id] = 3
    nodes = paste0(
        qq("  node [fontname=Helvetical]\n"),
        qq("  \"@{V(g)$name}\" [color=\"@{color}\", fillcolor=\"@{color}40\", style=filled, penwidth=@{penwidth}, fontsize=@{fontsize}, fontcolor=\"@{fontcolor}\", tooltip=\"@{V(g)$label}\"];\n", collapse = TRUE)
    )

    edgelist = get.edgelist(g)
    if(nrow(edgelist)) {
        edges = paste0(
            "  edge []\n",
            qq("  \"@{edgelist[, 1]}\" -> \"@{edgelist[, 2]}\";\n", collapse = TRUE)
        )
    } else {
        edges = ""
    }

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

#' @rdname dependency
#' @export
job_dependency_diagram = function(job_id, job_tb = NULL, ...) {

    job_id = as.character(job_id)
    dot = job_dependency_dot(job_id, job_tb)
    DiagrammeR::grViz(dot, ...)
}
