# clears workspace:  
rm(list=ls()) 

# sets working directories:
#setwd("/Users/jennifer/Dropbox/Documents/Teaching/CognitiveModelingSummerSchool2018/BayesianModeling/Codes")

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
                model.file ="day7__hierarchical-sdt.txt",
                n.chains=1, n.iter=10000, n.burnin=1, n.thin=1)
# Now the values for the monitored parameters are in the "samples" object, 
# ready for inspection.

d1 <- samples$BUGSoutput$sims.list$d[,,1]
d2 <- samples$BUGSoutput$sims.list$d[,,2]
d3 <- samples$BUGSoutput$sims.list$d[,,3]

c1 <- samples$BUGSoutput$sims.list$c[,,1]
c2 <- samples$BUGSoutput$sims.list$c[,,2]
c3 <- samples$BUGSoutput$sims.list$c[,,3]

D1 <- samples$BUGSoutput$sims.list$D[,1]
D2 <- samples$BUGSoutput$sims.list$D[,2]
D3 <- samples$BUGSoutput$sims.list$D[,3]

C1 <- samples$BUGSoutput$sims.list$C[,1]
C2 <- samples$BUGSoutput$sims.list$C[,2]
C3 <- samples$BUGSoutput$sims.list$C[,3]

#make the four panel plot:
layout(matrix(c(1,2), 1, 2, byrow = TRUE))
#layout.show(4)
#some plotting options to make things look better:
par(cex.main = 1.5, mar = c(5, 6, 1, 1) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
    font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
# Discriminability panel:    
plot(density(D1), lwd=2, col="red", main="", ylab="", xlab="", 
     xlim=c(0,3), axes=F)
axis(1)
axis(2, labels=F, at=c(0,24))

lines(density(D2), lwd=2, col="green", lty=2)
lines(density(D3), lwd=2, col="blue", lty=2)

mtext("Probability Density", side=2, line = 2, cex=1.5, las=0)
mtext("Discriminability", side=1, line = 2.5, cex=1.5)

# Bias panel:    
plot(density(C1), lwd=2, col="red", main="", ylab="", xlab="", 
     xlim=c(-1,1), axes=F)
axis(1)
axis(2, labels=F, at=c(0,24))

lines(density(C2), lwd=2, col="green", lty=2)
lines(density(C3), lwd=2, col="blue", lty=2)

mtext("Probability Density", side=2, line = 2, cex=1.5, las=0)
mtext("Bias", side=1, line = 2.5, cex=1.5)

layout(1)
hist(d1[,1], breaks = seq(0, 3, by = 0.2), col = 1)
hist(d1[,2], breaks = seq(0, 3, by = 0.2), add = TRUE, col = 2)
hist(d1[,3], breaks = seq(0, 3, by = 0.2), add = TRUE, col = 3)
hist(d1[,4], breaks = seq(0, 3, by = 0.2), add = TRUE, col = 4)
hist(d1[,5], breaks = seq(0, 3, by = 0.2), add = TRUE, col = 5)

