
job1 = bsub_chunk({
	Sys.sleep(30)
})

job2 = bsub_chunk({
	Sys.sleep(30)
})

job3 = bsub_chunk({
	Sys.sleep(30)
}, dependency = c(job1, job2))

job4 = bsub_chunk({
	Sys.sleep(30)
}, dependency = c(job3))

