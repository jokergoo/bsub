
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
