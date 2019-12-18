
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(GetoptLong))

ui = fluidPage(
    tags$style("body {
        font-size:1.2em;
        width:1000px;
        margin: auto;
    }"),
    titlePanel("LSF Job Monitor"),
    hr(style = "border-top: 1px solid black;"),
    DT::dataTableOutput("mytable"),
    actionButton("kill", "Kill selected jobs"),
    hr(style = "border-top: 1px solid black;"),
    p("built with ", a("bsub package", href='https://github.com/jokergoo/bsub', target='_blank'))
)

server <- function(input, output, session) {
        
    job_summary_df = reactive({
                
        showNotification("Fetching job summary table...", duration = 1, type = "message")
        message(qq("[@{Sys.time()}] Fetching job summary table"))
        df = bjobs(print = FALSE, status = "all")
        df[order(df$JOBID, decreasing = TRUE), , drop = FALSE]
    })

	output$mytable = DT::renderDataTable({
	    	    
		df = job_summary_df()
		nr = nrow(df)
        
		showNotification("Formatting job summary table...", duration = 5, type = "message")
		df2 = df[, c("JOBID", "STAT", "JOB_NAME", "TIME_PASSED", "TIME_LEFT", "SLOTS", "MEM", "MAX_MEM")]
        
		df2$STAT = factor(df2$STAT)
        df2$TIME_PASSED = bsub:::format_difftime(df2$TIME_PASSED)
        df2$TIME_LEFT = bsub:::format_difftime(df2$TIME_LEFT)
        df2$MAX_MEM = bsub:::format_mem(df2$MAX_MEM)
        df2$MEM = bsub:::format_mem(df2$MEM)

        l = nchar(df2$JOB_NAME) > 50
        if(any(l)) {
            foo = substr(df2$JOB_NAME[l], 1, 48)
            foo = paste(foo, "..", sep = "")
            df2$JOB_NAME[l] = foo
        }
        
		colnames(df2) = c("Job ID", "Status", "Job name", "Time passed", "Time left", "Cores", "Memory", "Max memory")
		
        dt = datatable(df2, escape = FALSE, rownames = FALSE, filter = 'top', 
            options = list(
                pageLength = 10, autoWidth = TRUE
            )
        )

        formatStyle(dt, "Status", color = styleEqual(c("RUN", "PEND", "DONE", "EXIT"), c("blue", "purple", "black", "red")))

	})
	
	output$info = renderText({
	    
	    autoInvalidate()
	    
	    df = job_summary_df()
	    tb = table(df$STAT)
	    paste0(qq("User @{bsub_opt$user} has "), paste(qq("@{tb} @{names(tb)} job@{ifelse(tb == 1, '', 's')}", collapse = FALSE), collapse = ", "), qq(" within one week. Summary table was generated at @{Sys.time()}."))
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

}

shinyApp(ui, server)
