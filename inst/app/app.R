
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(GetoptLong))
suppressPackageStartupMessages(library(DiagrammeR))
suppressPackageStartupMessages(library(igraph))
suppressPackageStartupMessages(library(shinyjs))

STATUS_COL = bsub:::STATUS_COL

ui = fluidPage(
    shinyjs::useShinyjs(),

    tags$style(HTML(
        paste(readLines("style.css"), collapse = "\n")
    )),
    tags$script(HTML(
        paste(readLines("app.js"), collapse = "\n")
    )),

    titlePanel("LSF Job Monitor"),

    htmlOutput("info"),
    actionButton("reload", "Manually reload"),

    # tags$script(HTML("count_down('countdown');")),
    
    hr(style = "border-top: 1px solid black;"),
    checkboxGroupInput("status_select", label = "Select job status: ", choices = structure(names(STATUS_COL), names = names(STATUS_COL)), selected = names(STATUS_COL), inline = TRUE),
    div(id = "table_loading", p("Summary table is loading...", style = "font-size:20px")),
    DT::dataTableOutput("mytable"),
    HTML('<p><button id="kill" type="button" class="btn btn-default">Kill selected jobs</button></p>'),

    hr(style = "border-top: 1px solid black;"),
    p("built with ", a(qq("the bsub package v@{installed.packages()['bsub', 'Version']}"), href='https://github.com/jokergoo/bsub', target='_blank'))
)

ENV = new.env()
ENV$job_log = list()
ENV$timestamp = Sys.time()

server <- function(input, output, session) {
    
    # autoInvalidate <- reactiveTimer(1000 * 60*5)  # every 5 min
    
    # observe({
    #     t = autoInvalidate()

    #     if(as.double(t - ENV$timestamp, units = "secs") > 10) {
    #         message(qq("[@{format(Sys.time())}] Reloading the app."))
    #         ENV$timestamp = t
    #         session$reload() 
    #     }
    # })
        
    job_summary_table = reactive({
        
        # autoInvalidate()
        
        showNotification("Fetching job summary table...", duration = 1, type = "message")
        message(qq("[@{format(Sys.time())}] Fetching job summary table"))
        job_tb = bjobs(print = FALSE, status = "all")
        if(!is.null(job_tb)) {
            job_tb[order(job_tb$JOBID, decreasing = TRUE), , drop = FALSE]
        } else {
            job_tb
        }
    })

    observeEvent(input$reload, {
        ENV$timestamp = Sys.time()
        message(qq("[@{format(Sys.time())}] Reloading the app."))
        session$reload()  
    })

	output$mytable = DT::renderDataTable({
	    
	    # autoInvalidate()
	    
		job_tb = job_summary_table()
        if(is.null(job_tb)) {
            return(datatable(data.frame(numeric(0))))
        }
		nr = nrow(job_tb)

        job_dep = job_dependency_all(job_tb)
        
		showNotification("Formatting job summary table...", duration = 5, type = "message")
		job_tb2 = job_tb[, c("JOBID", "STAT", "JOB_NAME", "QUEUE", "SUBMIT_TIME", "TIME_PASSED", "RUNTIMELIMIT", "NREQ_SLOT", "MEM", "MAX_MEM", "REQ_MEM")]

		job_tb2$STAT = factor(job_tb2$STAT)
        units(job_tb2$TIME_PASSED) = "secs"; job_tb2$TIME_PASSED = as.integer(as.numeric(job_tb2$TIME_PASSED))
        job_tb2$RUNTIMELIMIT = job_tb2$RUNTIMELIMIT*60
        job_tb2$MAX_MEM = bsub:::convert_to_byte(job_tb2$MAX_MEM)
        job_tb2$MEM = bsub:::convert_to_byte(job_tb2$MEM)
        job_tb2$REQ_MEM = job_tb2$REQ_MEM*1024*1024

        l = nchar(job_tb2$JOB_NAME) > 50
        if(any(l)) {
            foo = substr(job_tb2$JOB_NAME[l], 1, 48)
            foo = paste(foo, "..", sep = "")
            job_tb2$JOB_NAME[l] = foo
        }
        
        job_tb2$dep = ""

        ## add the dependency table
        if(any(job_tb2$JOBID %in% names(job_dep$id2name))) {
            for(i in which(job_tb2$JOBID %in% job_dep$dep_mat)) {
                job_tb2$dep[i] = "Dep"
            }
        }

        col_index_add = 0
        if(all(job_tb2$dep == "")) {
            job_tb2 = cbind(job_tb2[, 1, drop = FALSE], job_tb2[, -c(1, length(job_tb2)), drop = FALSE])
            colnames(job_tb2) = c("Job ID", "Status", "Job name", "Queue", "Submit time", "Passed time", "Req time", "Slots", "Mem", "Max mem", "Req mem")
        } else {
            col_index_add = 1
            job_tb2 = cbind(job_tb2[, c(1, length(job_tb2)), drop = FALSE], job_tb2[, -c(1, length(job_tb2)), drop = FALSE])
            colnames(job_tb2) = c("Job ID", "", "Status", "Job name", "Queue", "Submit time", "Passed time", "Req time", "Slots", "Mem", "Max mem", "Req mem")
		}
        job_tb2[, "Submit time"] = as.character(job_tb2[, "Submit time"])

        job_tb2 = job_tb2[job_tb2$Status %in% input$status_select, , drop = FALSE]

        if(nrow(job_tb2) == 0) {
            return(datatable(job_tb2, escape = FALSE, rownames = FALSE))
        }
        dt = datatable(job_tb2, escape = FALSE, rownames = FALSE, selection = 'none',
            options = list(
                pageLength = 25
            ), callback = JS("
table.on( 'draw', function () {
    $('#table_loading').hide();
    dt_bind_events();
});")
        )

        dt %>% 
            formatDate(columns = "Submit time", method = "toLocaleString") %>%
            bsub:::formatTimeDiff(columns = c("Passed time", "Req time")) %>%
            bsub:::formatNumber(columns = c("Slots")) %>%
            bsub:::formatFileSize(columns = c("Mem", "Max mem", "Req mem"))

	})
	
	output$info = renderText({
	    
	    # autoInvalidate()
	    
	    job_tb = job_summary_table()
	    tb = table(job_tb$STAT)
	    txt = paste0(qq("User <b>@{bsub_opt$user}</b> has "), paste(qq("<span class='@{names(tb)}'>@{tb} @{names(tb)} job@{ifelse(tb == 1, '', 's')}</span>", collapse = FALSE), collapse = ", "), qq(" within one week. Summary table was generated at @{format(Sys.time())}."))
	    txt = paste0(txt, " ", as.character(actionLink("job_stats", "See job stats")))
        HTML(txt)
    })
    
	observeEvent(input$select_job, {

        job_tb = job_summary_table()

	    job_id = gsub(" ", "", input$select_job)
        job_name = job_tb$JOB_NAME[job_tb$JOBID == job_id]
        job_status = job_tb$STAT[job_tb$JOBID == job_id]
	    message(qq("[@{format(Sys.time())}] Click 'job @{job_id}'"))

	    output$job_log = renderUI({
            if(job_status %in% c("RUN", "PEND") || is.null(ENV$job_log[[job_id]])) {
    	        showNotification("Fetching job log...", duration = 5)
    	        message(qq("[@{format(Sys.time())}] Fetching job log for '@{job_id} <@{job_name}> @{job_status}'"))
    	        log = job_log(job_id, print = FALSE)
                log = gsub("<", "&lt;", log)
                log = gsub(">", "&gt;", log)

                log = reformat_log(log, job_status)
                if(!job_status %in% c("RUN", "PEND")) {
                    ENV$job_log[[job_id]] = log
                }
            } else {
                log = ENV$job_log[[job_id]]
                message(qq("[@{format(Sys.time())}] Fetching job log for '@{job_id} <@{job_name}> @{job_status}' from cache"))
                
            }

	        html = paste(log, collapse = "\n")
            html = paste0("<pre>", html, "</pre>")
            HTML(html)
	    })

        output$job_env = renderUI({
            log = run_cmd(qq("bjobs -env @{job_id}"))
            names(log) = gsub("^(\\S+)=.*$", "\\1", log)

            log = gsub("^(\\w+)", "<b>\\1</b>", log)
            log = paste("<code>", log, "</code>")
            
            html = paste(log, collapse = "\n")
            html = paste0("<pre>", html, "</pre>")
             HTML(html)
            
        })

        output$job_script = renderUI({
            log = run_cmd(qq("bjobs -script @{job_id}"))
            i1 = grep("^# LSBATCH: User input", log)
            source = FALSE
            if(log[i1+1] == "( cat <<_USER_\\SCRIPT_") {
                i1 = i1 + 1
                source = TRUE
            }
            if(source) {
                i2 = which(log == "_USER_SCRIPT_")
                log = log[seq(i1+1, i2-1)]
            } else {
                i2 = grep("^# LSBATCH: End user input", log)
                log = log[seq(i1+1, i2-1-2)]
            }
            
            log = paste("<code>", log, "</code>")
            html = paste(log, collapse = "\n")
            html = paste0("<pre>", html, "</pre>")
            HTML(html)
        })

	    showModal(modalDialog(
	        title = qq("Job log (@{job_id} <@{job_name}>, @{job_status})"),
            if(job_status %in% c("EXIT", "unknown")) {
                tags$p(HTML(qq("<a style='color:#337ab7;text-decoration:underline;cursor:pointer;' onclick='Shiny.onInputChange(\"select_dep\", 0);Shiny.onInputChange(\"select_dep\", @{job_id})'>Show job dependencies</a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a style='color:#337ab7;text-decoration:underline;cursor:pointer;' onclick='$(\"#rerun_pipeline_output\").show();rerun_pipeline(\"@{job_id}\")'>Click here to rerun the pipeline (DONE jobs will be skipped).</a>")))
            } else {
                tags$p(HTML(qq("<a style='color:#337ab7;text-decoration:underline;cursor:pointer;' onclick='Shiny.onInputChange(\"select_dep\", 0);Shiny.onInputChange(\"select_dep\", @{job_id})'>Show job dependencies</a>")))
            },
            if(job_status %in% c("EXIT", "unknown")) {
                HTML("<pre class='shiny-text-output noplaceholder' id='rerun_pipeline_output' style='display:hidden'></pre>")
            } else {
                NULL
            },
	        tabsetPanel(type= "tabs",
                tabPanel("Job log", uiOutput("job_log")),
                tabPanel("Script", uiOutput("job_script")),
                tabPanel("Env variables", uiOutput("job_env"))
            ),
	        footer = actionButton("close_job_log", "Close"),
	        easyClose = FALSE,
	        size = "l"
	    ))
	})

    observeEvent(input$select_dep, {

        job_tb = job_summary_table()

        job_id = gsub(" ", "", input$select_dep)
        job_name = job_tb$JOB_NAME[job_tb$JOBID == job_id]
        job_status = job_tb$STAT[job_tb$JOBID == job_id]
        message(qq("[@{format(Sys.time())}] Click dependency for 'job @{job_id}'"))

        # test whether the pipeline can be rerun
        g = job_dependency_igraph(job_id, job_tb)
        all_job_ids = V(g)$name

        job_tb2 = job_tb[job_tb$JOBID %in% all_job_ids, , drop = FALSE]
        rerunable = FALSE
        if(length(all_job_ids) == 1) {
            if(job_tb2$STAT[1] %in% c("DONE", "EXIT", "unknown")) {
                rerunable = TRUE
            }
        }
        if(all(job_tb2$STAT %in% c("DONE", "EXIT", "unknown"))) {
            rerunable = TRUE
        }
        if(all(job_tb2$STAT == "DONE")) {
            rerunable = FALSE
        }

        output$dependency_plot = renderGrViz({
            showNotification("Generating job dependency tree...", duration = 5)
            message(qq("[@{format(Sys.time())}] Generating job dependency tree '@{job_id} <@{job_name}> @{job_status}'"))
            dot = job_dependency_dot(job_id, job_tb)
            grViz(dot, width = 868, height = 600)
        })
        
        showModal(modalDialog(
            title = qq("Job dependency (@{job_id} <@{job_name}>)"),
            if(rerunable) tags$p(HTML(qq("<a style='color:#337ab7;text-decoration:underline;cursor:pointer;' onclick='$(\"#rerun_pipeline_output\").show();rerun_pipeline(\"@{job_id}\")'>Some jobs are failed, click here to rerun the pipeline (DONE jobs will be skipped).</a>"))) else NULL,
            if(rerunable) HTML("<pre class='shiny-text-output noplaceholder' id='rerun_pipeline_output' style='display:hidden'></pre>") else NULL,
            HTML("<p id='dep_info'>If you cannot click on the nodes, move your mouse outside of the diagram and then move in.</p>"),
            grVizOutput("dependency_plot", width = "868px", height = "600px"),
            tags$script("
                $('#dependency_plot').on('mouseenter', function(e) {
                    gviz_add_events()
                })"
            ),
            HTML("<p><center><b>Legend:</b> <span class='RUN'>RUN</span> <span class='PEND'>PEND</span> <span class='DONE'>DONE</span> <span class='EXIT'>EXIT</span> <span class='unknown'>unknown</span></center></p>"),
            footer = actionButton("close_job_dep", "Close"),
            easyClose = FALSE,
            size = "l"
        ))
    })

    observeEvent(input$close_job_log, {
        output$job_log = renderUI({pre("loading...")})
        output$job_env = renderUI({pre("loading...")})
        output$job_script = renderUI({pre("loading...")})
        removeModal()
    })

    observeEvent(input$close_job_dep, {
        removeModal()
    })

    observeEvent(input$kill_job_id, {
        
        job_id = input$kill_job_id
        job_id = gsub("@.*$", "", job_id)
        job_id = strsplit(job_id, ";")[[1]]
        tb = job_summary_table()

        if(length(job_id) == 0) {
            output$kill_job_info = renderText({
                "No job was selected."
            })
            showModal(modal_kill_nothing)
        } else {
            tb = tb[tb$JOBID %in% job_id, , drop = FALSE]
            killing_job_id = tb$JOBID[tb$STAT %in% c("RUN", "PEND")]
            if(length(killing_job_id) == 0) {
                output$kill_job_info = renderText({
                    "No RUN/PEND job was selected."
                })
                showModal(modal_kill_nothing)
            } else {
                output$kill_job_info = renderText({
                    paste0("Kill following jobs? ", paste(killing_job_id, collapse = ","))
                })
                
                m = structure(names = paste0(tb$JOBID, " <", tb$JOB_NAME, ">"), tb$JOBID)
                
                showModal(modalDialog(
                    title = "Kill jobs",
                    checkboxGroupInput("killing_job_id", "Kill following jobs?", m[m %in% killing_job_id], selected = killing_job_id),
                    HTML("<pre class='shiny-text-output noplaceholder' id='kill_job_output' style='display:hidden'></pre>"),
                    footer = list(
                        actionButton("kill_job_cancel", "Cancel"),
                        HTML('<button id="kill_job_confirm" type="button" class="btn btn-default action-button">Kill</button>')
                    )
                ))
            }
        }
    })
    
    observeEvent(input$kill_no_job, {
        removeModal()
    })
    
    observeEvent(input$kill_job_confirm, {
        killing_job_id = input$killing_job_id
        showNotification(qq("Kill jobs"), duration = 5)
        withCallingHandlers({
            shinyjs::html("kill_job_output", "")
            for(id in killing_job_id) {
                message(qq("Kill job @{id}"))
                try(bkill(id), silent = TRUE)
            }
            for(sec in 10:1) {
                message(qq("Reload the app (sleep @{sec} seconds)"))
                Sys.sleep(1)
            }
        }, message = function(m) {
            shinyjs::html(id = "kill_job_output", html = m$message, add = TRUE)
        })
        session$reload()
    })
    
    observeEvent(input$kill_job_cancel, {
        removeModal()
    })

    observeEvent(input$job_stats, {

        job_tb = job_summary_table()

        message(qq("[@{format(Sys.time())}] query global job statistics"))

        output$job_barplot = renderPlot({
            showNotification("Generating job barplot...", duration = 2)
            message(qq("[@{format(Sys.time())}] generating job barplot"))
            bjobs_barplot(job_tb = job_tb)
        })

        output$job_timeline = renderPlot({
            showNotification("Generating job timeline...", duration = 2)
            message(qq("[@{format(Sys.time())}] generating job timeline"))
            bjobs_timeline(job_tb = job_tb)
        })
        
        showModal(modalDialog(
            title = qq("Job statistics"),
            p(h4("Number of jobs per day")),
            plotOutput("job_barplot", width = "760px", height = "600px"),
            p(h4("Duration of jobs")),
            plotOutput("job_timeline", width = "760px", height = "600px"),
            footer = actionButton("close_job_stats", "Close"),
            easyClose = FALSE,
            size = "l"
        ))
    })

    observeEvent(input$close_job_stats, {
        output$job_barplot = NULL
        output$job_timeline = NULL
        removeModal()
    })

    observeEvent(input$rerun_pipeline, {
        job_id = input$rerun_pipeline_job_id
        showNotification(qq("rerun pipeline by @{job_id}..."), duration = 5)

        withCallingHandlers({
            shinyjs::html("rerun_pipeline_output", "")
            pipeline_rerun(job_id)
            for(sec in 10:1) {
                message(qq("Reload the app (sleep @{sec} seconds)"))
                Sys.sleep(1)
            }
        },
        message = function(m) {
            shinyjs::html(id = "rerun_pipeline_output", html = m$message, add = TRUE)
        })

        session$reload()
    })
}

modal_kill_nothing = modalDialog(
    title = "Kill jobs",
    textOutput("kill_job_info"),
    footer =  actionButton("kill_no_job", "OK"),
    easyClose = TRUE
)

reformat_log = function(log, status) {
    if(status == "EXIT") {
        ind = which(grepl("\\berror\\b|\\bfail|Exited with exit code", log, ignore.case = TRUE) & !grepl("^#", log))
        if(length(ind)) {
            log[ind] = paste0("<span style='color:#e41a1c;font-weight:bold;text-decoration:underline;'>", log[ind], "</span>")
        }
    }
    if(status == "DONE") {
        ind = grep("Successfully completed", log, ignore.case = TRUE)
        if(length(ind)) {
            log[ind] = paste0("<span style='color:#377eb8;font-weight:bold;text-decoration:underline;'>", log[ind], "</span>")
        }
    }
    if(status == "RUN") {
        ind = grep("is still running", log)
        if(length(ind)) {
            log[ind] = paste0("<span style='color:darkgreen;font-weight:bold;text-decoration:underline;'>", log[ind], "</span>")
        }
    }
    if(status == "PEND") {
        ind = grep("pending", log)
        if(length(ind)) {
            log[ind] = paste0("<span style='color:#ff7f00;font-weight:bold;text-decoration:underline;'>", log[ind], "</span>")
        }
    }
    paste("<code>", log, "</code>")
}

shinyApp(ui, server)
