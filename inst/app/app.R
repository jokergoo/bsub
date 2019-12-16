
library(DT)
library(shiny)
library(GetoptLong)

ui = fluidPage(
    tags$style("body {
        font-size:1.2em;
    }"),
    titlePanel("LSF Job Monitor"),
    div(textOutput("info"), style = "background-color:#FFE0E0;padding:5px 5px; border: 1px solid red;"),
    actionButton("reload", "Reload"),
    DT::dataTableOutput("mytable"),
    actionButton("kill", "Kill selected jobs"),
    hr(),
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
        
		showNotification("Formatting job summary table...", duration = 1, type = "message")
		df2 = df[, c("JOBID", "STAT", "JOB_NAME", "TIME_PASSED", "TIME_LEFT", "SLOTS", "MEM", "MAX_MEM")]
        
		df2$STAT = factor(df2$STAT)
        df2$TIME_PASSED = bsub:::format_difftime(df2$TIME_PASSED)
        df2$TIME_LEFT = bsub:::format_difftime(df2$TIME_LEFT)
        df2$MAX_MEM = bsub:::format_mem(df2$MAX_MEM)
        df2$MEM = bsub:::format_mem(df2$MEM)
        
        for(i in 1:nr) {
            df2$JOB_NAME[i] = as.character(actionLink(paste0("job_name_id_", df2$JOBID[i]), df2$JOB_NAME[i], 
                onclick = "Shiny.onInputChange('select_link', 0);Shiny.onInputChange('select_link', this.id); var class_attr=this.parentElement.parentElement.getAttribute('class'); class_attr = /selected/.test(class_attr) ? class_attr.replace(/ selected/, '') : class_attr + ' selected'; this.parentElement.parentElement.setAttribute('class', class_attr)"))
        }
        
		colnames(df2) = c("Job ID", "Status", "Job name", "Time passed", "Time left", "Cores", "Memory", "Max memory")
		df2

	}, escape = FALSE, rownames = FALSE, filter = 'top',
	  options = list(
	    pageLength = 25, autoWidth = TRUE
	))
	
	output$info = renderText({
	    
	    autoInvalidate()
	    
	    df = job_summary_df()
	    tb = table(df$STAT)
	    paste0(qq("User @{bsub_opt$user} has "), qq("@{tb} @{names(tb)} jobs, "), qq(" summary table was generated at @{Sys.time()}."))
	})
    
	observeEvent(input$select_link, {

	    job_name_selected = gsub("job_name_id_", "", input$select_link)
	    message(qq("[@{Sys.time()}] clicked job name @{input$select_link}"))
	    
	    output$job_log = renderText({
	        showNotification("Fetching job log...", duration = 1)
	        message(qq("[@{Sys.time()}] Fetching job log for @{job_name_selected}"))
	        log = job_log(job_name_selected, print = FALSE)
	        paste(log, collapse = "\n")
	    })
	    
	    showModal(modalDialog(
	        title = qq("Job log (id: @{job_name_selected})"),
	        verbatimTextOutput("job_log"),
	        footer = actionButton("close_job_log", "Close"),
	        easyClose = TRUE,
	        size = "l"
	    ))
	})

    observeEvent(input$close_job_log, {
        removeModal()
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
                
                m = structure(names = paste0(tb$JOBID, " (", tb$JOB_NAME, ")"), tb$JOBID)
                
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
        showNotification(qq("Killing @{length(killing_job_id)} jobs..."), duration = 1)
        message(qq("[@{Sys.time()}] Killing jobs: @{paste(killing_job_id, collapse=', ')}"))
        bkill(killing_job_id)
        removeModal()
        message(qq("[@{Sys.time()}] sleep 5s before reloading the app."))
        Sys.sleep(5)
        session$reload()
    })
    
    observeEvent(input$kill_job_cancel, {
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
