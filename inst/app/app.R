
library(DT)
library(shiny)

ui = basicPage(
	tags$style(
"#generated_time {
	text-align:right;
	font-size:12px;
	color: grey;
}"),

	DT::dataTableOutput("mytable"),
	actionButton("kill", "Kill jobs"),
	textOutput("job_selected"),
	hr(),
	textOutput("generated_time")
)

server <- function(input, output) {
	
	# auto refresh 5min
	autoInvalidate <- reactiveTimer(1000* 5*60)

	observe({
		autoInvalidate()
	})

	output$mytable = DT::renderDataTable({
		autoInvalidate()
		df = bjobs(print = FALSE)

		df2 = df[order(df$JOBID), c("JOBID", "STAT", "JOB_NAME", "TIME_PASSED", "TIME_LEFT", "SLOTS", "MEM", "MAX_MEM")]
        
		df2$STAT = factor(df2$STAT)
        df2$TIME_PASSED = bsub:::format_difftime(df2$TIME_PASSED)
        df2$TIME_LEFT = bsub:::format_difftime(df2$TIME_LEFT)
        df2$MAX_MEM = bsub:::format_mem(df2$MAX_MEM)
        df2$MEM = bsub:::format_mem(df2$MEM)

		colnames(df2) = c("Job ID", "Status", "Job name", "Time passed", "Time left", "Cores", "Memory", "Max memory")
		df2

	}, escape = FALSE, rownames = FALSE, filter = 'top',
	  options = list(
	    pageLength = 25, autoWidth = TRUE
	))

	output$generated_time = renderText({
	    autoInvalidate()
		paste0("generated at ", Sys.time())
	})

	output$job_selected = renderText({
		""
	})

	job_selected = eventReactive(input$kill, {
		input$tableId_rows_selected
	})

	output$job_selected = renderText({
		paste(job_selected(), collapse = ", ")
	})
}

shinyApp(ui, server)
