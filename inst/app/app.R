library(DT)
library(shiny)

ui = basicPage(
  tags$style(
    "#generated_time {
	text-align:right;
	font-size:12px;
	color: grey;
}"
  ),
  h5("Messages from server:"),
  verbatimTextOutput("dialog"),
  hr(),
  actionButton("kill", "Kill jobs"),
  hr(),
  DT::dataTableOutput("mytable")
)

server <- function(input, output) {
  # auto refresh 5min
  autoInvalidate <- reactiveTimer(1000 * 5 * 60)
  
  observe({
    autoInvalidate()
  })
  
  bjobsTable <- reactiveValues(dt = NULL)
  
  bjobsReactive <- reactive({
    autoInvalidate()
    df = bjobs(print = FALSE)
    df2 = df[order(df$JOBID), c(
      "JOBID",
      "STAT",
      "JOB_NAME",
      "TIME_PASSED",
      "TIME_LEFT",
      "SLOTS",
      "MEM",
      "MAX_MEM"
    )]
    df2$STAT = factor(df2$STAT)
    df2$TIME_PASSED = bsub:::format_difftime(df2$TIME_PASSED)
    df2$TIME_LEFT = bsub:::format_difftime(df2$TIME_LEFT)
    df2$MAX_MEM = bsub:::format_mem(df2$MAX_MEM)
    df2$MEM = bsub:::format_mem(df2$MEM)
    colnames(df2) = c(
      "Job ID",
      "Status",
      "Job name",
      "Time passed",
      "Time left",
      "Cores",
      "Memory",
      "Max memory"
    )
    bjobsTable$dt <- df2
  })
  
  output$mytable = DT::renderDataTable({
    bjobsReactive()
    bjobsTable$dt
  }, escape = FALSE, rownames = FALSE, filter = 'top',
  options = list(pageLength = 25, autoWidth = TRUE))
  
  output$generated_time = renderText({
    autoInvalidate()
    paste0("generated at ", Sys.time())
  })
  
  output$job_selected = renderText({
    ""
  })
  
  # Kill jobs
  observeEvent(input$kill, {
    selected_row <- input$mytable_rows_selected
    if (length(selected_row)) {
      selectedTable <- bjobsTable$dt[selected_row, ]
      output$dialog <- renderPrint({
        print(paste0(
          "Selected Job ID: ",
          paste(selectedTable[, 'Job ID'], collapse = ", ")
        ))
        for (i in 1:nrow(selectedTable)) {
          if (selectedTable[i, 'Status'] == 'RUN' |
              selectedTable[i, 'Status'] == 'PEND') {
            bkill(selectedTable[i, 'Job ID'])
          }
          else{
            print(
              paste0(
                "Selected Job ID ",
                selectedTable[i, 'Job ID'],
                ": Cannot Kill ",
                selectedTable[i, 'Status'],
                " Job"
              )
            )
          }
        }
      })
      output$mytable = DT::renderDataTable({
        bjobsReactive()
        bjobsTable$dt
      }, escape = FALSE, rownames = FALSE, filter = 'top',
      options = list(pageLength = 25, autoWidth = TRUE))
    }
  })
}

shinyApp(ui, server)
