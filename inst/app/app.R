
library(DT)
library(shiny)

ui = basicPage(
	DT::dataTableOutput("mytable"),
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
		tb = bjobs(print = FALSE)
		rownames(tb) = NULL
		# checkbox = qq("<input type='checkbox' name='job-@{tb$JOB_ID}' value='0' />")
		# tb = cbind(checkbox, tb)
		# colnames(tb)[1] = ""
		tb
	}, escape = FALSE, rownames = FALSE)
	output$generated_time = renderText({
	    autoInvalidate()
		paste0("generated at ", Sys.time())
	})
}

shinyApp(ui, server)
