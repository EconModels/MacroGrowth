# Note: this won't do anything in parallel unless the 
# redis server is running.
require(doRedis)
require(foreach)
require(mosaic)
library('doRedis')
registerDoRedis(queue='jobs')
startLocalWorkers(n=50, queue='jobs')
setChunkSize(1)
print(getDoParWorkers())

sleepy <- 1 + runif(400,-.5,.5) 

etime <- system.time(
x <- foreach( i = 1:200, .combine=c, .inorder=FALSE ) %dopar% {
  Sys.sleep(sleepy[i])
  i
}
)[3]
removeQueue('jobs')
print(xyplot( x ~ 1:length(x), 
              main=paste("total:", sum(sleepy[1:length(x)])),
              sub=paste("time:", etime)
              ))
print(etime)
