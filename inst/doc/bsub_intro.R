## ---- message = FALSE----------------------------------------------------
library(bsub)

## ---- eval = FALSE-------------------------------------------------------
#  bsub_chunk(
#  {
#  	NMF::nmf(...)
#  })

## ------------------------------------------------------------------------
bsub_chunk(
{
	Sys.sleep(5)
})

## ------------------------------------------------------------------------
bsub_chunk(name = "example", memory = 10, hour = 10, core = 4, 
{
	Sys.sleep(5)
})

## ---- eval = FALSE-------------------------------------------------------
#  bsub_chunk(name = "example", R_version = "3.6.0",
#  {
#  	Sys.sleep(5)
#  })

## ---- eval = FALSE-------------------------------------------------------
#  bsub_opt$R_version = "3.6.0"
#  bsub_chunk(name = "example",
#  {
#  	Sys.sleep(5)
#  })

## ---- eval = FALSE-------------------------------------------------------
#  function(version) {
#  	qq("module load gcc/7.2.0; module load java/1.8.0_131; module load R/@{version}; Rscript")
#  }

## ---- eval = FALSE-------------------------------------------------------
#  bsub_chunk(name = "example",
#  {
#  	library(package1)
#  	library(package2)
#  	Sys.sleep(5)
#  })

## ---- eval = FALSE-------------------------------------------------------
#  bsub_chunk(name = "example", packages = c("package1", "package2"),
#  {
#  	Sys.sleep(5)
#  })

## ---- eval = FALSE-------------------------------------------------------
#  bsub_opt$packages = c("package1", "package2")
#  bsub_chunk(name = "example",
#  {
#  	Sys.sleep(5)
#  })

## ---- eval = FALSE-------------------------------------------------------
#  foo = 1
#  bsub_chunk(name = "example", variables = "foo",
#  {
#  	bar = foo
#  	Sys.sleep(5)
#  })

## ---- eval = FALSE-------------------------------------------------------
#  save.image(file = "/path/foo.RData")
#  # or
#  # save(var1, var2, ..., file = "...")
#  bsub_chunk(name = "example", image = "/path/foo.RData",
#  {
#  	...
#  	Sys.sleep(5)
#  })

## ---- eval = FALSE-------------------------------------------------------
#  save.image(file = "/path/foo.RData")
#  bsub_opt$image = "/path/foo.RData"
#  bsub_chunk(name = "example",
#  {
#  	...
#  	Sys.sleep(5)
#  })

## ---- eval = FALSE-------------------------------------------------------
#  bsub_chunk(name = "example", wd = "/path"
#  {
#  	Sys.sleep(5)
#  })

## ---- eval = FALSE-------------------------------------------------------
#  bsub_opt$wd = "/path"
#  bsub_chunk(name = "example",
#  {
#  	Sys.sleep(5)
#  })

## ------------------------------------------------------------------------
bsub_chunk(name = "example2", save_var = TRUE,
{
	Sys.sleep(10)
	1+1
})
retrieve_var("example2")

## ---- eval = FALSE-------------------------------------------------------
#  bsub_chunk(name = "example",
#  {
#  	...
#  	save(...)
#  	# or
#  	saveRDS(...)
#  })

## ---- echo = FALSE-------------------------------------------------------
Sys.sleep(10)

## ------------------------------------------------------------------------
bsub_chunk(name = "example", enforce = FALSE,
{ 
	Sys.sleep(5)
})

## ---- eval = FALSE-------------------------------------------------------
#  bsub_opt$enforce = FALSE
#  bsub_chunk(name = "example",
#  {
#  	Sys.sleep(5)
#  })

## ---- eval = FALSE-------------------------------------------------------
#  job1 = bsub_chunk(name = "example1",
#  {
#  	Sys.sleep(5)
#  })
#  bsub_chunk(name = "example2", dependency = job1,
#  {
#  	Sys.sleep(5)
#  })

## ---- eval = FALSE-------------------------------------------------------
#  bsub_chunk(name = "example", temp_dir = ..., output_dir = ...,
#  {
#  	Sys.sleep(5)
#  })

## ---- eval = FALSE-------------------------------------------------------
#  bsub_opt$temp_dir = ...
#  bsub_opt$output_dir = ...
#  bsub_chunk(name = "example",
#  {
#  	Sys.sleep(5)
#  })

## ---- eval = FALSE-------------------------------------------------------
#  bsub_chunk(name = "example",
#  	script = "/path/foo.R",
#  	start = 10, end = 20, ...)

## ---- eval = FALSE-------------------------------------------------------
#  ...
#  # BSUB_START
#  you code chunk here
#  # BSUB_END
#  ...

## ---- eval = FALSE-------------------------------------------------------
#  bsub_chunk(name = "example",
#  	script = "/path/foo.R",
#  	start = "^# BSUB_START",
#  	end = "^# BSUB_END", ...)

## ------------------------------------------------------------------------
bsub_chunk(name = "example", local = TRUE,
{ 
	cat("blablabla...\n")
})

## ---- eval = FALSE-------------------------------------------------------
#  bsub_script("/path/foo.R", name = ..., memory = ..., core = ..., ...)

## ---- eval = FALSE-------------------------------------------------------
#  bsub_script("/path/foo.R", argv = "--a 1 --b 3", ...)

## ---- eval = FALSE-------------------------------------------------------
#  bsub_script("/path/foo.R", .a = 1, .b = 3, ...)

## ---- eval = FALSE-------------------------------------------------------
#  bsub_cmd("samtools sort ...", name = ..., memory = ..., core = ..., ...)
#  bsub_cmd(c("cmd1", "cmd2", ...), name = ..., memory = ..., core = ..., ...)

## ------------------------------------------------------------------------
bsub_opt

## ------------------------------------------------------------------------
library(GetoptLong)
for(i in 1:4) {
	bsub_chunk(name = qq("example_@{i}"),
	{ 
		Sys.sleep(5)
	})
}
bjobs

## ------------------------------------------------------------------------
bjobs(status = "all", filter = "example")

## ------------------------------------------------------------------------
brecent

## ------------------------------------------------------------------------
sessionInfo()

