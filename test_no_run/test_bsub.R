
job1 = bsub_chunk({
	Sys.sleep(30)
}, name = "job1")

job2 = bsub_chunk({
	Sys.sleep(30)
}, name = "job2")

job3 = bsub_chunk({
	Sys.sleep(30)
}, name = "job3", dependency = c(job1, job2))

job4 = bsub_chunk({
	Sys.sleep(30)
}, name = "job4", dependency = c(job3))



a = 1
bsub_chunk({
	print(a)
}, name = "job1")


### test bkill ####


bsub_chunk({
	Sys.sleep(600)
}, name = "long_job1")

bsub_chunk({
	Sys.sleep(600)
}, name = "long_job2")

bsub_chunk({
	Sys.sleep(600)
}, name = "long_job3")

bsub_chunk({
	Sys.sleep(600)
}, name = "long_job4")


## test rerun pipeline
job1 = bsub_chunk({
	Sys.sleep(300)
}, name = "job1")

job2 = bsub_chunk({
	Sys.sleep(300)
}, name = "job2")

job3 = bsub_chunk({
	Sys.sleep(300)
}, name = "job3", dependency = c(job1, job2))

job4 = bsub_chunk({
	Sys.sleep(300)
}, name = "job4", dependency = c(job3))


job1 = bsub_chunk({
	Sys.sleep(10)
}, name = "job1")

job2 = bsub_chunk({
	Sys.sleep(10)
}, name = "job2")

job3 = bsub_chunk({
	Sys.sleep(300)
}, name = "job3", dependency = c(job1, job2))

job4 = bsub_chunk({
	Sys.sleep(300)
}, name = "job4", dependency = c(job3))

