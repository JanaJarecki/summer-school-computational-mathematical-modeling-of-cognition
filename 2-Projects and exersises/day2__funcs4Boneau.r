#functions for Boneau project in separate source file

#run simulation for a cell
runReps <- function(nrep, dist, n, sd) {
  tee <- pee <- rep(0,nrep)

  for (i in c(1:nrep)) {
    x <- genDat(dist, n[1], sd[1])
    y <- genDat(dist, n[2], sd[2])
    t <- t.test(x,y,var.equal=TRUE)
    tee[i] <- t$statistic
    pee[i] <- t$p.value
  }
  return(list(tee=tee,pee=pee,df=as.numeric(t$parameter)))
}

#generate data for a cell
genDat <- function(distribution,n,sd) {
switch(distribution,
  normal = rnorm(n,0,sd),
  #http://math.stackexchange.com/questions/140071/generating-uniformly-distributed-random-variables-with-given-mean-and-deviation
  uniform = (runif(n)-.5)*(sd*2*sqrt(3)),
  #http://en.wikipedia.org/wiki/Exponential_distribution#Generating_exponential_variates
  exp = (-sd*log(runif(n)))-sd )
}

#plot results for a cell
plotReps <- function(dist,teepee,nrep,ens,sds) {
  hist(teepee$tee, xlab="t statistic",las=1,
              main=paste("Distribution = ",dist,"  Ns = ",ens[1],ens[2],"  s= ",sds[1],sds[2]))

  abline(v=qt(.025,teepee$df),col="red")
  abline(v=qt(.975,teepee$df),col="red")
  return(list(ens=ens,sds=sds,type1=sum(teepee$p<.05)/nrep))
}

