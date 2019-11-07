This package sends R code/scripts or normal shell commands to LSF clusters.

```r
library(bsub)

# R code
bsub_chunk(name = "example", memory = 10, hour = 10, core = 4, 
{
	NMF::nmf(...)
})

# R script
bsub_script(name = "example",
	script = "/path/foo.R", ...)

# shell commands
bsub_cmd(name = "example",
	cmd = "samtools view ...", ...)

# show job summaries
bjobs(...)

# show job log
job_log(job_id)

# kill jobs
bkill(job_id)
```

More information can be found in the package vignette.

Note it only works on DKFZ internal HPC.
