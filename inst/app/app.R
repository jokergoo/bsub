
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
		df2

	}, escape = FALSE, rownames = FALSE, filter = 'top', 
	  options = list(
	    pageLength = 10, autoWidth = TRUE
	))

}

shinyApp(ui, server)
