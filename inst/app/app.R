
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(GetoptLong))
suppressPackageStartupMessages(library(shinyjqui))

ui = fluidPage(
    tags$style("body {
        font-size: 1.4em;
        width:1200px;
        margin: auto;
    }
    .ui-resizable {
        border: 1px grey solid;
    }"),
    tags$script(HTML(
        paste(readLines("format.js"), collapse = "\n")
    )),
    titlePanel("LSF Job Monitor"),
    div(htmlOutput("info"), style = "background-color:#EEFFEE; padding:5px 5px; margin: 15px 0px 15px 0px; border: 1px solid green;"),
    p(actionButton("reload", "Manually reload"), "The monitor automatically reloads every 5 minutes."),
    hr(style = "border-top: 1px solid black;"),
    div(id = "table_loading", p("Summary table is loading...", style = "font-size:20px")),
    DT::dataTableOutput("mytable"),
    actionButton("kill", "Kill selected jobs"),
    # actionButton("unselect", "Unselect all jobs", onclick="$('tr.selected').removeClass('selected')"),
    hr(style = "border-top: 1px solid black;"),
    p("built with ", a("bsub package", href='https://github.com/jokergoo/bsub', target='_blank'))
)

server <- function(input, output, session) {
    
    autoInvalidate <- reactiveTimer(1000 * 60*5)  # every 5 min
    
    observe({
        autoInvalidate()
    })
        
    job_summary_df = reactive({
        
        autoInvalidate()
        
        showNotification("Fetching job summary table...", duration = 1, type = "message")
        message(qq("[@{Sys.time()}] Fetching job summary table"))
        df = bjobs(print = FALSE, status = "all")
        df[order(df$JOBID, decreasing = TRUE), , drop = FALSE]
    })
	
    observeEvent(input$reload, {
        message(qq("[@{Sys.time()}] Manually reloading the app."))
        session$reload()  
    })

	output$mytable = DT::renderDataTable({
	    
	    autoInvalidate()
	    
		df = job_summary_df()
		nr = nrow(df)

        job_dep = get_dependency(df)
        
		showNotification("Formatting job summary table...", duration = 5, type = "message")
		df2 = df[, c("JOBID", "STAT", "JOB_NAME", "SUBMIT_TIME", "TIME_PASSED", "TIME_LEFT", "SLOTS", "MEM", "MAX_MEM")]

		df2$STAT = factor(df2$STAT)
        units(df2$TIME_PASSED) = "secs"; df2$TIME_PASSED = as.integer(as.numeric(df2$TIME_PASSED))
        units(df2$TIME_LEFT) = "secs"; df2$TIME_LEFT = as.integer(as.numeric(df2$TIME_LEFT))
        df2$MAX_MEM = bsub:::convert_to_byte(df2$MAX_MEM)
        df2$MEM = bsub:::convert_to_byte(df2$MEM)

        l = nchar(df2$JOB_NAME) > 50
        if(any(l)) {
            foo = substr(df2$JOB_NAME[l], 1, 48)
            foo = paste(foo, "..", sep = "")
            df2$JOB_NAME[l] = foo
        }
        
        for(i in 1:nr) {
            df2$JOB_NAME[i] = as.character(actionLink(paste0("job_name_id_", df2$JOBID[i]), df2$JOB_NAME[i], 
                title = "Click to see the job log", "data-toggle" = "tooltip",
                onclick = "Shiny.onInputChange('select_link', 0);Shiny.onInputChange('select_link', this.id); var class_attr=this.parentElement.parentElement.getAttribute('class'); class_attr = /selected/.test(class_attr) ? class_attr.replace(/ selected/, '') : class_attr + ' selected'; this.parentElement.parentElement.setAttribute('class', class_attr)"))
        }

        df2$dep = ""

        ## add the dependency table
        if(any(df2$JOBID %in% names(job_dep$id2name))) {
            for(i in which(df2$JOBID %in% names(job_dep$id2name))) {
                df2$dep[i] = (as.character(actionLink(paste0("job_dep_id_", df2$JOBID[i]), "Tree", 
                        style = "border: 1px solid #009900; background-color:#009900; color: white;",
                        title = "Click to see the dependency tree", "data-toggle" = "tooltip",
                        onclick = "Shiny.onInputChange('select_dep', 0);Shiny.onInputChange('select_dep', this.id); var class_attr=this.parentElement.parentElement.getAttribute('class'); class_attr = /selected/.test(class_attr) ? class_attr.replace(/ selected/, '') : class_attr + ' selected'; this.parentElement.parentElement.setAttribute('class', class_attr)"))
                )
            }
        }

        col_index_add = 0
        if(all(df2$dep == "")) {
            df2 = cbind(df2[, 1, drop = FALSE], df2[, -c(1, length(df2)), drop = FALSE])
            colnames(df2) = c("Job ID", "Status", "Job name", "Submit time", "Time passed", "Time left", "Cores", "Memory", "Max memory")
        } else {
            col_index_add = 1
            df2 = cbind(df2[, c(1, length(df2)), drop = FALSE], df2[, -c(1, length(df2)), drop = FALSE])
            colnames(df2) = c("Job ID", "", "Status", "Job name", "Submit time", "Time passed", "Time left", "Cores", "Memory", "Max memory")
		}
        dt = datatable(df2, escape = FALSE, rownames = FALSE, filter = "top",
            options = list(
                pageLength = 10
            ), callback = JS(qq("table.on( 'draw', function () {
                                $('#table_loading').hide();
                                $('[data-toggle=\"tooltip\"]').tooltip(); 
                                $('#mytable th').eq(@{2+col_index_add}).css('width', '300px');
                            } );"))
        )

        dt %>% 
            formatStyle(columns = "Status", color = styleEqual(c("RUN", "PEND", "DONE", "EXIT"), c("blue", "purple", "black", "red"))) %>%
            formatDate(columns = "Submit time", method = "toLocaleString") %>%
            bsub:::formatTimeDiff(columns = c("Time passed", "Time left")) %>%
            bsub:::formatFileSize(columns = c("Memory", "Max memory"))

	})
	
	output$info = renderText({
	    
	    autoInvalidate()
	    
	    df = job_summary_df()
	    tb = table(df$STAT)
	    txt = paste0(qq("User <b>@{bsub_opt$user}</b> has "), paste(qq("@{tb} @{names(tb)} job@{ifelse(tb == 1, '', 's')}", collapse = FALSE), collapse = ", "), qq(" within one week. Summary table was generated at @{Sys.time()}."))
	    txt = paste0(txt, " ", as.character(actionLink("job_stats", "See job stats", 
                title = "Click to see the job statistics", "data-toggle" = "tooltip")))
        HTML(txt)
    })
    
	observeEvent(input$select_link, {

        df = job_summary_df()

	    job_name_selected = gsub("job_name_id_", "", input$select_link)
        job_name = df$JOB_NAME[df$JOBID == job_name_selected]
        job_status = df$STAT[df$JOBID == job_name_selected]
	    message(qq("[@{Sys.time()}] clicked job name @{input$select_link}"))

	    output$job_log = renderText({
	        showNotification("Fetching job log...", duration = 5)
	        message(qq("[@{Sys.time()}] Fetching job log for @{job_name_selected} <@{job_name}> @{job_status}"))
	        log = job_log(job_name_selected, print = FALSE)
	        paste(log, collapse = "\n")
	    })

	    showModal(modalDialog(
	        title = qq("Job log (@{job_name_selected} <@{job_name}>)"),
	        verbatimTextOutput("job_log"),
	        footer = actionButton("close_job_log", "Close"),
	        easyClose = TRUE,
	        size = "l"
	    ))
	})

    observeEvent(input$select_dep, {

        df = job_summary_df()

        job_name_selected = gsub("job_dep_id_", "", input$select_dep)
        job_name = df$JOB_NAME[df$JOBID == job_name_selected]
        job_status = df$STAT[df$JOBID == job_name_selected]
        message(qq("[@{Sys.time()}] clicked dependency for @{input$select_dep}"))

        output$dependency_plot = renderPlot({
            showNotification("Generating job dependency tree...", duration = 5)
            message(qq("[@{Sys.time()}] Generating job dependency tree @{job_name_selected} <@{job_name}> @{job_status}"))
            plot_dependency(job_name_selected)
        })
        
        showModal(modalDialog(
            title = qq("Job dependency (@{job_name_selected} <@{job_name}>)"),
            jqui_resizable(plotOutput("dependency_plot", width = "auto", height = "600px")),
            footer = actionButton("close_job_dep", "Close"),
            easyClose = TRUE,
            size = "l"
        ))
    })

    observeEvent(input$close_job_log, {
        output$job_log = NULL
        removeModal()
    })

    observeEvent(input$close_job_dep, {
        output$dependency_plot = NULL
        removeModal()
    })

    observeEvent(input$mytable_rows_selected, {
        selected_row = input$mytable_rows_selected
        if(length(selected_row) == 0) {
            message(qq("[@{Sys.time()}] No row is selected"))
        } else {
            message(qq("[@{Sys.time()}] @{length(selected_row)} rows are selected"))
        }
    })
    
    observeEvent(input$kill, {
        selected_row = input$mytable_rows_selected
        tb = job_summary_df()
        if(length(selected_row) == 0) {
            output$kill_job_info = renderText({
                "No job was selected."
            })
            showModal(modal_kill_nothing)
        } else {
            tb = tb[input$mytable_rows_selected, , drop = FALSE]
            killing_job_id = tb$JOBID[tb$STAT %in% c("RUN", "PEND")]
            if(length(killing_job_id) == 0) {
                output$kill_job_info = renderText({
                    "No RUN/PEND job is selected."
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
        message(qq("[@{Sys.time()}] Killing jobs: @{paste(killing_job_id, collapse=', ')}"))
        bkill(killing_job_id)
        removeModal()
        message(qq("[@{Sys.time()}] sleep 5s before reloading the app"))
        Sys.sleep(5)
        session$reload()
    })
    
    observeEvent(input$kill_job_cancel, {
        removeModal()
    })

    observeEvent(input$job_stats, {

        df = job_summary_df()

        message(qq("[@{Sys.time()}] query global job statistics"))

        output$job_barplot = renderPlot({
            showNotification("Generating job barplot...", duration = 5)
            message(qq("[@{Sys.time()}] Generating job barplot"))
            bjobs_barplot(df = df)
        })

        output$job_timeline = renderPlot({
            showNotification("Generating job timeline...", duration = 5)
            message(qq("[@{Sys.time()}] Generating job timeline"))
            bjobs_timeline(df = df)
        })
        
        showModal(modalDialog(
            title = qq("Job statistics"),
            p(h4("Number of jobs per day")),
            jqui_resizable(plotOutput("job_barplot", width = "600px", height = "400px")),
            p(h4("Duration of jobs")),
            jqui_resizable(plotOutput("job_timeline", width = "600px", height = "400px")),
            footer = actionButton("close_job_stats", "Close"),
            easyClose = TRUE,
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

shinyApp(ui, server)
