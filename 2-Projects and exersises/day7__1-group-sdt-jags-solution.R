# clears workspace:  
rm(list=ls()) 

# sets working directories:
#setwd("/Users/jennifer/Dropbox/Documents/Teaching/CognitiveModelingSummerSchool2018/BayesianModeling/Codes")

library(R2jags)

k <- 3 #number of cases
data <- matrix(c(148, 32, 29, 151, 150, 30, 40, 140, 150, 40, 51, 139), nrow=k, ncol=4, byrow=T)

h <- data[,1]
f <- data[,2]
MI <- data[,3]
CR <- data[,4]
s <- h + MI
n <- f + CR

data <- list("h", "f", "k", "s", "n") # to be passed on to JAGS
myinits <- list(
  list(d = rep(0,k), c = rep(0,k)))  

# parameters to be monitored: 
parameters <- c("d", "c", "thetah", "thetaf")

# The following command calls JAGS with specific options.
# For a detailed description see the R2jags documentation.
samples <- jags(data, inits=myinits, parameters,
                    model.file ="day7__group-sdt.txt",
                    n.chains=1, n.iter=10000, n.burnin=1000, n.thin=1)
# Now the values for the monitored parameters are in the "samples" object, 
# ready for inspection.

d1 <- samples$BUGSoutput$sims.list$d[,1]
d2 <- samples$BUGSoutput$sims.list$d[,2]
d3 <- samples$BUGSoutput$sims.list$d[,3]

c1 <- samples$BUGSoutput$sims.list$c[,1]
c2 <- samples$BUGSoutput$sims.list$c[,2]
c3 <- samples$BUGSoutput$sims.list$c[,3]

h1 <- samples$BUGSoutput$sims.list$thetah[,1]
h2 <- samples$BUGSoutput$sims.list$thetah[,2]
h3 <- samples$BUGSoutput$sims.list$thetah[,3]

f1 <- samples$BUGSoutput$sims.list$thetaf[,1]
f2 <- samples$BUGSoutput$sims.list$thetaf[,2]
f3 <- samples$BUGSoutput$sims.list$thetaf[,3]

#make the four panel plot:
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
#layout.show(4)
#some plotting options to make things look better:
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
    font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
# Discriminability panel:    
plot(density(d1), lwd=2, col="red", main="", ylab="", xlab="", 
     xlim=c(-2,6), axes=F)
axis(1)
axis(2, labels=F, at=c(0,24))

lines(density(d2), lwd=2, col="green", lty=2)
lines(density(d3), lwd=2, col="blue", lty=2)

mtext("Probability Density", side=2, line = 2, cex=1.5, las=0)
mtext("Discriminability", side=1, line = 2.5, cex=1.5)

# Bias panel:    
plot(density(c1), lwd=2, col="red", main="", ylab="", xlab="", 
     xlim=c(-2,2), axes=F)
axis(1)
axis(2, labels=F, at=c(0,24))

lines(density(c2), lwd=2, col="green", lty=2)
lines(density(c3), lwd=2, col="blue", lty=2)

mtext("Probability Density", side=2, line = 2, cex=1.5, las=0)
mtext("Bias", side=1, line = 2.5, cex=1.5)

# Hit Rate panel:    
plot(density(h1), lwd=2, col="red", main="", ylab="", xlab="", 
     xlim=c(0,1), axes=F)
axis(1)
axis(2, labels=F, at=c(0,24))

lines(density(h2), lwd=2, col="green", lty=2)
lines(density(h3), lwd=2, col="blue", lty=2)

lines(c(0, 0.1),c(7,7), lwd=2, lty=1, col="red")
lines(c(0, 0.1),c(6,6), lwd=2, lty=2, col="green")
lines(c(0, 0.1),c(5,5), lwd=2, lty=3, col="blue")
  
text(0.15, 7, labels="first", offset=0, cex = 1.3, pos=4)
text(0.15, 6, labels="second", offset=0, cex = 1.3, pos=4)
text(0.15, 5, labels="third", offset=0, cex = 1.3,pos=4)

mtext("Probability Density", side=2, line = 2, cex=1.5, las=0)
mtext("Hit Rate", side=1, line = 2.5, cex=1.5)

# False-Alarm Rate panel:    
plot(density(f1), lwd=2, col="red", main="", ylab="", xlab="", 
     xlim=c(0,1), axes=F)
axis(1)
axis(2, labels=F, at=c(0,24))

lines(density(f2), lwd=2, col="green", lty=2)
lines(density(f3), lwd=2, col="blue", lty=2)

mtext("Probability Density", side=2, line = 2, cex=1.5, las=0)
mtext("False-Alarm Rate", side=1, line = 2.5, cex=1.5)

predHs = cbind(h1,h2,h3); predFs = cbind(f1,f2,f3)
layout(matrix(c(1,2,3), 1, 3, byrow = TRUE)); par(mar=c(5,5,1,1))
for (i in 1:3){
  plot(NULL, ylim = c(0,1), xlim = c(0,1), pch = 15, xlab = 'False Alarms', ylab = 'Hits')
  points(x = predFs[1:2000,i], y = predHs[1:2000,i],pch = 16, col = rgb(0,0.5,1,alpha=.02))
  points(x = f[i]/n[i], y = h[i]/s[i], pch=15)
}
