rm(list=ls())
graphics.off()

#James Stein estimator
# if c==1 no shrinkage
# if c==0 full shrinkage (grand mean)
jamesstein <- function(y1,c,ygm) {
  return(ygm + c*(y1-ygm))
}

#plot the second-half scores together with estimator
plotEstimator <- function(secondhalf, ll, ul, secondMe, shrinkParm, tofest) {
  predError <- sqrt(sum((secondhalf-secondMe)^2)/length(secondhalf))
  error2true <- sqrt(sum((secondMe-(5+(1:12)/10))^2)/length(secondhalf))
  print(c(shrinkParm,predError,error2true))
  
  plot(secondhalf,type="n",
       xaxt="n",xlab="", ylim=c(ll,ul), ylab="Second half observed & predicted")
  
  apply(cbind(1:12,1:12,secondMe,secondhalf),1,FUN=function(x) lines(x[1:2],x[3:4]))
  title(main=paste(tofest,"sp: ",as.character(round(shrinkParm,2)),
                          "  er: ",as.character(round(predError,2))))
  
  points(secondhalf,pch=21,cex=3,bg="white")
  points(secondhalf, pch=c(as.character(1:9),"a","b","c"))
  points(secondMe,pch=21,bg="red")
} 


############### mainline begins here 
players <- t(sapply(1:12,FUN=function(x) rnorm(20,5+x/10,2))) #generate 12 players' scores for 20-game season

firsthalf <-  rowMeans(players[,1:10])
secondhalf <- rowMeans(players[,11:20])

#some graphical work ....
chklims <- c(firsthalf,secondhalf)
ll <- floor(min(chklims))
ul <- ceiling(max(chklims))

#look at individual differences between players
plot(rowMeans(players),type="p",pch=c(as.character(1:9),"a","b","c"),
     xaxt="n",xlab="", ylim=c(ll,ul), ylab="Overall score")

#plot first-half scores and (grand) mean across players
ygm <- mean(firsthalf)
plot(firsthalf,type="p",pch=c(as.character(1:9),"a","b","c"),
     xaxt="n",xlab="", ylim=c(ll,ul), ylab="First half")
abline(h=ygm,col="red",lty="dashed")

#compute and plot various predicted second-half scores
secondMe <- firsthalf

plotEstimator(secondhalf, ll, ul, secondMe, 1, "me")
for (shrinkParm in c(.8, .6, .4, .2, .0)) {
    secondYouMe <- jamesstein(firsthalf,shrinkParm,ygm)
    plotEstimator(secondhalf, ll, ul, secondYouMe, shrinkParm, "you and me")
}
