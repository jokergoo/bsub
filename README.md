# Submitter and Monitor of the LSF Cluster

[![Build Status](https://travis-ci.org/jokergoo/bsub.svg)](https://travis-ci.org/jokergoo/bsub)
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

There are four vignettes:

- [Send R code/R scripts/shell commands to LSF cluster](https://jokergoo.github.io/bsub/articles/bsub_intro.html) 
- [Configure bsub package for other LSF institutes](https://jokergoo.github.io/bsub/articles/configure_bsub_package.html)
- [What if you need to establish two ssh connections to reach the submission node](https://jokergoo.github.io/bsub/articles/two_ssh.html)

### Submit jobs

Directly submit R chunk:

```r
library(bsub)

# R code
bsub_chunk(name = "example", memory = 10, hour = 10, core = 4, 
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

<p><img width="907" alt="monitor" src="https://user-images.githubusercontent.com/449218/71278574-40d4bf00-2358-11ea-9849-339aeb324601.png"></p>

Job log:

<p><img width="905" alt="job_log" src="https://user-images.githubusercontent.com/449218/71278573-403c2880-2358-11ea-8c82-c99677c0ebb0.png"></p>

Job dependency tree:

<p><img width="895" alt="dependency_tree" src="https://user-images.githubusercontent.com/449218/71278571-403c2880-2358-11ea-8e05-69c064148b2d.png"></p>

Kill jobs:

<p><img width="895" alt="Screenshot 2019-12-22 at 16 03 49" src="https://user-images.githubusercontent.com/449218/71323523-8ddda000-24d4-11ea-8fd3-350cbe93763c.png"></p>



### License

MIT @ Zuguang Gu


