# clears workspace:  
rm(list=ls()) 

library(R2jags)

k <- 3 #number of cases
ns <- 5
means = c(148, 32, 29, 151, 150, 30, 40, 140, 150, 40, 51, 139); halfRange = 3
data = array(dim = c(k, 4, ns)); vec = array(NA, dim = 12)
for (n in 1:ns){
  for (i in 1:12){
    vec[i] = round(runif(1,means[i]-halfRange,means[i]+halfRange))
  }
  data[,,n] <- matrix(vec, nrow=k, ncol=4, byrow=T)
}
#let's make an outlier participant
data[,c(1,4),ns] = round(data[,c(1,4),ns]*0.5)

h <- t(data[,1,])
f <- t(data[,2,])
MI <- t(data[,3,])
CR <- t(data[,4,])
s <- h + MI
n <- f + CR

data <- list("h", "f", "k", "s", "n", "ns") # to be passed on to JAGS
myinits <- list(
  list(d = array(rep(0,k*ns), dim = c(ns, k)), 
       c = array(rep(0,k*ns), dim = c(ns, k))))

# parameters to be monitored:	
parameters <- c("d", "c", "thetah", "thetaf")

# The following command calls JAGS with specific options.
# For a detailed description see the R2jags documentation.
samples <- jags(data, inits=myinits, parameters,
	 			model.file ="day7__individual-sdt.txt",
	 			n.chains=1, n.iter=10000, n.burnin=1, n.thin=1)
# Now the values for the monitored parameters are in the "samples" object, 
# ready for inspection.

d1 <- samples$BUGSoutput$sims.list$d[,,1]
hist(d1[,1], breaks = seq(0, 3, by = 0.2), col = 1)
hist(d1[,2], breaks = seq(0, 3, by = 0.2), add = TRUE, col = 2)
hist(d1[,3], breaks = seq(0, 3, by = 0.2), add = TRUE, col = 3)
hist(d1[,4], breaks = seq(0, 3, by = 0.2), add = TRUE, col = 4)
hist(d1[,5], breaks = seq(0, 3, by = 0.2), add = TRUE, col = 5)

d2 <- samples$BUGSoutput$sims.list$d[,,2]
hist(d2[,1], breaks = seq(0, 3, by = 0.2), col = 1)
hist(d2[,2], breaks = seq(0, 3, by = 0.2), add = TRUE, col = 2)
hist(d2[,3], breaks = seq(0, 3, by = 0.2), add = TRUE, col = 3)
hist(d2[,4], breaks = seq(0, 3, by = 0.2), add = TRUE, col = 4)
hist(d2[,5], breaks = seq(0, 3, by = 0.2), add = TRUE, col = 5)

d3 <- samples$BUGSoutput$sims.list$d[,,3]
hist(d3[,1], breaks = seq(-2, 3, by = 0.2), col = 1)
hist(d3[,2], breaks = seq(-2, 3, by = 0.2), add = TRUE, col = 2)
hist(d3[,3], breaks = seq(-2, 3, by = 0.2), add = TRUE, col = 3)
hist(d3[,4], breaks = seq(-2, 3, by = 0.2), add = TRUE, col = 4)
hist(d3[,5], breaks = seq(-2, 3, by = 0.2), add = TRUE, col = 5)

c1 <- samples$BUGSoutput$sims.list$c[,1,]
c2 <- samples$BUGSoutput$sims.list$c[,2,]
c3 <- samples$BUGSoutput$sims.list$c[,3,]

h1 <- samples$BUGSoutput$sims.list$thetah[,1,]
h2 <- samples$BUGSoutput$sims.list$thetah[,2,]
h3 <- samples$BUGSoutput$sims.list$thetah[,3,]

f1 <- samples$BUGSoutput$sims.list$thetaf[,1,]
f2 <- samples$BUGSoutput$sims.list$thetaf[,2,]
f3 <- samples$BUGSoutput$sims.list$thetaf[,3,]

