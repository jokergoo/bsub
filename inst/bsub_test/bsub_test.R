
bsub_opt$output_dir = "/desktop-home/guz/project/development/bsub/inst/bsub_test"
bsub_opt$debug = TRUE

# Send R code
bsub_chunk(name = "example_01", 
{
	Sys.sleep(10)
})

bsub_chunk(name = "example_02", R_version = "3.5.1",
{
	Sys.sleep(5)
})

bsub_chunk(name = "example_03", packages = c("circlize", "ComplexHeatmap"),
{
	Sys.sleep(5)
})


foo = 1
bsub_chunk(name = "example_04", variables = "foo",
{ 
	bar = foo
	Sys.sleep(5)
})

bsub_chunk(name = "example_05", save_var = TRUE,
{
	Sys.sleep(10)
	1+1
})
retrieve_var("example_05")

bsub_chunk(name = "load_cola",
{ 
	library(cola)
})


