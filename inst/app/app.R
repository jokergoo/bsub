
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(GetoptLong))
suppressPackageStartupMessages(library(shinyjqui))
suppressPackageStartupMessages(library(DiagrammeR))
suppressPackageStartupMessages(library(igraph))

STATUS_COL = bsub:::STATUS_COL

ui = fluidPage(
    tags$style(HTML(
        paste(readLines("style.css"), collapse = "\n")
    )),
    tags$script(HTML(
        paste(readLines("app.js"), collapse = "\n")
    )),

    titlePanel("LSF Job Monitor"),

    htmlOutput("info"),
    p(actionButton("reload", "Manually reload"), HTML("The monitor is automatically reloaded in <span id='countdown'>5m 0s</span>.")),
    tags$script(HTML("count_down('countdown');")),
    
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
    
    autoInvalidate <- reactiveTimer(1000 * 60*5)  # every 5 min
    
    observe({
        t = autoInvalidate()

        if(as.double(t - ENV$timestamp, units = "secs") > 10) {
            message(qq("[@{format(Sys.time())}] Reloading the app."))
            ENV$timestamp = t
            session$reload() 
        }
    })
        
    job_summary_df = reactive({
        
        autoInvalidate()
        
        showNotification("Fetching job summary table...", duration = 1, type = "message")
        message(qq("[@{format(Sys.time())}] Fetching job summary table"))
        df = bjobs(print = FALSE, status = "all")
        if(!is.null(df)) {
            df[order(df$JOBID, decreasing = TRUE), , drop = FALSE]
        } else {
            df
        }
    })

    observeEvent(input$reload, {
        ENV$timestamp = Sys.time()
        message(qq("[@{format(Sys.time())}] Reloading the app."))
        session$reload()  
    })

	output$mytable = DT::renderDataTable({
	    
	    autoInvalidate()
	    
		df = job_summary_df()
        if(is.null(df)) {
            return(datatable(data.frame(numeric(0))))
        }
		nr = nrow(df)

        job_dep = get_dependency(df)
        
		showNotification("Formatting job summary table...", duration = 5, type = "message")
		df2 = df[, c("JOBID", "STAT", "JOB_NAME", "QUEUE", "SUBMIT_TIME", "TIME_PASSED", "TIME_LEFT", "SLOTS", "MEM", "MAX_MEM", "REQ_MEM")]

		df2$STAT = factor(df2$STAT)
        units(df2$TIME_PASSED) = "secs"; df2$TIME_PASSED = as.integer(as.numeric(df2$TIME_PASSED))
        units(df2$TIME_LEFT) = "secs"; df2$TIME_LEFT = as.integer(as.numeric(df2$TIME_LEFT))
        df2$MAX_MEM = bsub:::convert_to_byte(df2$MAX_MEM)
        df2$MEM = bsub:::convert_to_byte(df2$MEM)
        df2$REQ_MEM = df2$REQ_MEM*1024*1024

        l = nchar(df2$JOB_NAME) > 50
        if(any(l)) {
            foo = substr(df2$JOB_NAME[l], 1, 48)
            foo = paste(foo, "..", sep = "")
            df2$JOB_NAME[l] = foo
        }
        
        df2$dep = ""

        ## add the dependency table
        if(any(df2$JOBID %in% names(job_dep$id2name))) {
            for(i in which(df2$JOBID %in% names(job_dep$id2name))) {
                df2$dep[i] = "Dep"
            }
        }

        col_index_add = 0
        if(all(df2$dep == "")) {
            df2 = cbind(df2[, 1, drop = FALSE], df2[, -c(1, length(df2)), drop = FALSE])
            colnames(df2) = c("Job ID", "Status", "Job name", "Queue", "Submit time", "T_passed", "T_left", "Slots", "Mem", "Max mem", "Req mem")
        } else {
            col_index_add = 1
            df2 = cbind(df2[, c(1, length(df2)), drop = FALSE], df2[, -c(1, length(df2)), drop = FALSE])
            colnames(df2) = c("Job ID", "", "Status", "Job name", "Queue", "Submit time", "T_passed", "T_left", "Slots", "Mem", "Max mem", "Req mem")
		}
        df2[, "Submit time"] = as.character(df2[, "Submit time"])

        df2 = df2[df2$Status %in% input$status_select, , drop = FALSE]

        if(nrow(df2) == 0) {
            return(datatable(df2, escape = FALSE, rownames = FALSE))
        }
        dt = datatable(df2, escape = FALSE, rownames = FALSE, selection = 'none',
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
            bsub:::formatTimeDiff(columns = c("T_passed", "T_left")) %>%
            bsub:::formatNumber(columns = c("Slots")) %>%
            bsub:::formatFileSize(columns = c("Mem", "Max mem", "Req mem"))

	})
	
	output$info = renderText({
	    
	    autoInvalidate()
	    
	    df = job_summary_df()
	    tb = table(df$STAT)
	    txt = paste0(qq("User <b>@{bsub_opt$user}</b> has "), paste(qq("<span class='@{names(tb)}'>@{tb} @{names(tb)} job@{ifelse(tb == 1, '', 's')}</span>", collapse = FALSE), collapse = ", "), qq(" within one week. Summary table was generated at @{format(Sys.time())}."))
	    txt = paste0(txt, " ", as.character(actionLink("job_stats", "See job stats")))
        HTML(txt)
    })
    
	observeEvent(input$select_job, {

        df = job_summary_df()

	    job_id = gsub(" ", "", input$select_job)
        job_name = df$JOB_NAME[df$JOBID == job_id]
        job_status = df$STAT[df$JOBID == job_id]
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
	        title = qq("Job log (@{job_id} <@{job_name}>)"),
            tags$p(HTML(qq("<a style='color:#337ab7;text-decoration:underline;cursor:pointer;' onclick='Shiny.onInputChange(\"select_dep\", 0);Shiny.onInputChange(\"select_dep\", @{job_id})'>Show job dependencies</a>"))),
	        tabsetPanel(type= "tabs",
                tabPanel("Job log", uiOutput("job_log")),
                tabPanel("Script", uiOutput("job_script")),
                tabPanel("Env variables", uiOutput("job_env"))
            ),
	        footer = actionButton("close_job_log", "Close"),
	        easyClose = FALSE,
	        size = "l"
	    ))

        outputOptions(output, "job_log", suspendWhenHidden = FALSE)
	})

    observeEvent(input$select_dep, {

        df = job_summary_df()

        job_id = gsub(" ", "", input$select_dep)
        job_name = df$JOB_NAME[df$JOBID == job_id]
        job_status = df$STAT[df$JOBID == job_id]
        message(qq("[@{format(Sys.time())}] Click dependency for 'job @{job_id}'"))

        output$dependency_plot = renderGrViz({
            showNotification("Generating job dependency tree...", duration = 5)
            message(qq("[@{format(Sys.time())}] Generating job dependency tree '@{job_id} <@{job_name}> @{job_status}'"))
            dot = job_dependency_dot(job_id, df)
            grViz(dot)
        })
        
        showModal(modalDialog(
            title = qq("Job dependency (@{job_id} <@{job_name}>)"),
            HTML("<p id='dep_info'>If you cannot click on the nodes, move your mouse outside of the diagram and then move in.</p>"),
            grVizOutput("dependency_plot", width = "auto", height = "600px"),
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

        outputOptions(output, "dependency_plot", suspendWhenHidden = FALSE)

    })

    observeEvent(input$close_job_log, {
        removeModal()
    })

    observeEvent(input$close_job_dep, {
        removeModal()
    })

    observeEvent(input$kill_job_id, {
        
        job_id = input$kill_job_id
        job_id = gsub("@.*$", "", job_id)
        job_id = strsplit(job_id, ";")[[1]]
        tb = job_summary_df()

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
                    footer = list(
                        actionButton("kill_job_cancel", "Cancel"),
                        actionButton("kill_job_confirm", "Kill them!")
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
        showNotification(qq("Killing @{length(killing_job_id)} jobs..."), duration = 5)
        message(qq("[@{format(Sys.time())}] Killing jobs: @{paste(killing_job_id, collapse=', ')}"))
        try(bkill(killing_job_id), silent = TRUE)
        removeModal()
        showNotification(qq("Sleep 5s before reloading the app"), duration = 5)
        message(qq("[@{format(Sys.time())}] Sleep 5s before reloading the app"))
        Sys.sleep(5)
        session$reload()
    })
    
    observeEvent(input$kill_job_cancel, {
        removeModal()
    })

    observeEvent(input$job_stats, {

        df = job_summary_df()

        message(qq("[@{format(Sys.time())}] query global job statistics"))

        output$job_barplot = renderPlot({
            showNotification("Generating job barplot...", duration = 2)
            message(qq("[@{format(Sys.time())}] generating job barplot"))
            bjobs_barplot(df = df)
        })

        output$job_timeline = renderPlot({
            showNotification("Generating job timeline...", duration = 2)
            message(qq("[@{format(Sys.time())}] generating job timeline"))
            bjobs_timeline(df = df)
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
