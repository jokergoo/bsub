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

# interactive job monitor
monitor()
```

The **bsub** package is only tested on DKFZ ODCF cluster. It is also possible for other 
clusters that use LSF system to use **bsub** package. Following global options should be
properly configured:

- How to call the `Rscript` binary, especially when you have different versions of R installed.
Following is how we call `Rscript` on our cluster (we use `module` to manage the bash environment).

```r
bsub_opt$call_Rscript = function(version) {
    qq("module load gcc/7.2.0; module load java/1.8.0_131; module load R/@{version}; Rscript")
}
```

- Your username on the submission node.

```r
bsub_opt$user = ...
```

- The names of the nodes where the jobs are submitted.

```r
bsub_opt$submission_node = ...
```

- Bash environment. It is basically for properly finding `bsub`/`bjobs`/... Following is how we set up
on our cluster.

```r
bsub_opt$ssh_envir = c("source /etc/profile",
                       "export LSF_ENVDIR=/opt/lsf/conf",
                       "export LSF_SERVERDIR=/opt/lsf/10.1/linux3.10-glibc2.17-x86_64/etc")
```

- The template of `bsub` command. The self-defined function should accepts following arguments:

    * `name` job name.
    * `hour` running time.
    * `memory` memory, in GB.
    * `core` number of cores to use.
    * `output` path of output file
    * `...` should be added as the last argument of the function. Following is a simple example
for calling `bsub`.

```r
bsub_opt$bsub_template = function(name, hour, memory, core, output, ...) {
    glue::glue("bsub -J '{name}' -W '{hour}:00' -n {core} -R 'rusage[mem={memory}GB]' -o '{output}'")
}
```

More information can be found in [the package vignette](https://jokergoo.github.io/bsub/articles/bsub_intro.html).
