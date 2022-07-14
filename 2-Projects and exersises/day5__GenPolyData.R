#more pseudopsychophysics
library(stats)

N  <- 20
o  <- 4
sd <- 20 #standard deviation for noise in data

pcoefs <- c(333,7,5,-1,-.1) #polynomial coefficients
x=seq(-4,4,length.out=N)    #arbitrary stimulus magnitudes

# Generate training data from polynomial defined by pcoefs
y=(pcoefs[1]+pcoefs[2]*x+pcoefs[3]*x^2+pcoefs[4]*x^3+pcoefs[5]*x^4)+rnorm(N,0,sd)
TrainData=data.frame(x=x,y=y)
plot(x,y)

# Fit the training data with a 2nd, 4-th and 6-th ... etc. order polynomial
lm_2 <- lm(y ~ poly(x,2))
yy=predict(lm_2)
lines(x,yy,col="grey",lty=1,lwd=2)

lm_4 <- lm(y ~ poly(x,4))
yy=predict(lm_4)
lines(x,yy,col="blue",lty=1,lwd=2)

lm_6 <- lm(y ~ poly(x,6))
yy=predict(lm_6)
lines(x,yy,col="yellow",lty=1,lwd=2)

lm_10 <- lm(y ~ poly(x,10))
yy=predict(lm_10)
lines(x,yy,col="green",lty=1,lwd=2)

lm_19 <- lm(y ~ poly(x,19))
yy=predict(lm_19)
lines(x,yy,col="black",lty=1,lwd=2)


# Generate test data
y=(pcoefs[1]+pcoefs[2]*x+pcoefs[3]*x^2+pcoefs[4]*x^3+pcoefs[5]*x^4)+rnorm(N,0,sd)
TestData=data.frame(x=x,y=y)
plot(x,y)

# Fit the transfer data with polynomials estimated from training data
lines(x,predict(lm_2),col="grey",lty=1,lwd=2)
lines(x,predict(lm_4),col="blue",lty=1,lwd=2)
lines(x,predict(lm_10),col="green",lty=1,lwd=2)
lines(x,predict(lm_19),col="black",lty=1,lwd=2)


write.csv(TrainData,"PolyTrainData.csv")
write.csv(TestData,"PolyTestData.csv")
