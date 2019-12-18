# Submitter and monitor of LSF cluster

It Sends R code/R scripts/shell commands to LSF cluster without leaving R.

### Examples

```r
library(bsub)

# R code
bsub_chunk(name = "example", memory = 10, hour = 10, core = 4, 
{
    fit = NMF::nmf(...)
    # you better save `fit` into a permanent file
    saveRDS(fit, file = "fit.rds")
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

# interactive job monitor
monitor()
```

### Documentation

The online documentation is available at https://jokergoo.github.io/bsub/.

There are three vignettes:

- [Send R code/R scripts/shell commands to LSF cluster](https://jokergoo.github.io/bsub/docs/articles/bsub_intro.html) 
- [Use bsub package on the DKFZ ODCF cluster](https://jokergoo.github.io/bsub/docs/articles/dkfz_odcf.html)
- [Configure bsub package for other LSF institutes](https://jokergoo.github.io/bsub/docs/articles/other_institute.html)


### License

MIT @ Zuguang Gu
