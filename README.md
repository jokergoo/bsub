# Submitter and Monitor of the LSF Cluster

[![R-CMD-check](https://github.com/jokergoo/bsub/workflows/R-CMD-check/badge.svg)](https://github.com/jokergoo/bsub/actions)
[![CRAN](https://www.r-pkg.org/badges/version/bsub)](https://cran.r-project.org/web/packages/bsub/index.html)


It sends R code/R scripts/shell commands to LSF cluster without leaving R.

### Install

```r
if (!requireNamespace("devtools", quietly = TRUE))
    install.packages("devtools")
devtools::install_github("jokergoo/bsub")
```

If you want the full functionality of the package, you need to install a few more packages:

```r
devtools::install_github('jokergoo/bsub', dependencies = 'Suggests')
```

### Documentation

The online documentation is available at https://jokergoo.github.io/bsub/.


There are the following vignettes:

- [Send R code/R scripts/shell commands to LSF cluster](https://jokergoo.github.io/bsub_vignettes/bsub_intro.html)
- [Configure bsub package](https://jokergoo.github.io/bsub_vignettes/configure_bsub_package.html)
- [What if you need to establish two ssh connections to reach the submission node](https://jokergoo.github.io/bsub_vignettes/two_ssh.html)

### Submit jobs

Directly submit R chunk:

```r
library(bsub)

# R code
bsub_chunk(name = "example", memory = 10, hours = 10, cores = 4, 
{
    fit = NMF::nmf(...)
    # you better save `fit` into a permanent file
    saveRDS(fit, file = "/path/of/fit.rds")
})
```

Submit an R script:

```r
# R script
bsub_script(name = "example",
    script = "/path/of/foo.R", ...)
```

Submit shell commands:

```r
# shell commands
bsub_cmd(name = "example",
    cmd = "samtools view ...", ...)
```

Kill jobs:

```r
bkill(job_id)
```

### View job info

View job summaries:

```r
bjobs
brecent
bjobs_running
bjobs_pending
bjobs_done
bjobs_exit
```

An example of the job queries is as follows:

![image](https://user-images.githubusercontent.com/449218/71292253-2577ad00-2374-11ea-9402-855e7f01652c.png)

View job log: 

```r
job_log(job_id)
```


### Interactive job monitor

```r
monitor()
```

The job summary table:

<p><img width="907" alt="monitor" src="https://github.com/jokergoo/bsub/assets/449218/9970a50d-dbf6-4477-9a8b-5344f641b8ba"></p>

Job log:

<p><img width="905" alt="job_log" src="https://github.com/jokergoo/bsub/assets/449218/b2faa77c-6dc1-4578-b52c-73bc7a739ba9"></p>

Job dependency diagram:

<p><img width="895" alt="dependency_graph" src="https://github.com/jokergoo/bsub/assets/449218/eaf85ef6-32bb-4c09-a3d4-1069cf11a404"></p>



### License

MIT @ Zuguang Gu


