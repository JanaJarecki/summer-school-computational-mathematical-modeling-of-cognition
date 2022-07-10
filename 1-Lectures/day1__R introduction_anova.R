rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory

setwd("C:/Users/Lewan/Documents/Talks & Conferences organized/22/07 09-23 Summer School/_Teaching for SS2022/_R Teaching")

#generate data for an ANOVA design
oneway <- rep(c(1:8),each=10)
facA   <- rep(c("a1","a2"),each=40)
facB   <- rep(c("b1","b2","b1","b2"),each=20)
facC   <- rep(rep(c("c1","c2"),each=10),4)
dv     <- rnorm(80)

#create data frame
anovadata <- data.frame(dv=dv,oneway=as.factor(oneway),
                        facA=facA,facB=facB,facC=facC)
str(anovadata)

#perform analysis two different ways
aov1way <- aov(dv~oneway,data=anovadata)
(aov1sum <- summary(aov1way))
min(as.numeric(unlist(aov1sum[[1]][5])),na.rm=TRUE)

aov3way <- aov(dv~facA*facB*facC,data=anovadata)
(aov3sum<-summary(aov3way))
min(as.numeric(unlist(aov3sum[[1]][5])),na.rm=TRUE)


#now do simulation in function and replicate
onerep <- function(anovadata,pval) {
  anovadata$dv <- rnorm(80)
  a1s <- summary(aov(dv~oneway,data=anovadata))
  a1sigs <- sum(as.numeric(unlist(a1s[[1]][5])) < pval,na.rm=TRUE)
  a3s <- summary(aov(dv~facA*facB*facC,data=anovadata))
  a3sigs <- sum(as.numeric(unlist(a3s[[1]][5])) < pval,na.rm=TRUE)
  return(list(a1sigs=a1sigs,a3sigs=a3sigs))
}

n1 <- n3 <- 0
for (i in c(1:1000)) {
      onerepresult <- onerep(anovadata,.05)
      n1 <- n1 + onerepresult$a1sigs
      n3 <- n3 + onerepresult$a3sigs
}
print(c(n1,n3))


#do some writing and reading of data
write.csv(anovadata,"anovadata.csv",row.names = FALSE)
adread <- read.csv("anovadata.csv",
                   colClasses = c("numeric",
                                  rep("factor",4)))
