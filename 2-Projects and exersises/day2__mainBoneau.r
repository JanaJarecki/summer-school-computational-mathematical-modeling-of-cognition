#Boneau 1960: Monte Carlo examination of the t-test
# not needed with .RProj: setwd("C:/Users/Lewan/Documents/Talks & Conferences organized/22/07 09-23 Summer School/_Teaching for SS2022/_R Teaching/Boneau project")

rm(list=ls())
graphics.off()
source("day2__funcs4Boneau.r")

# specify design cells
ens <- matrix(c(5, 5, 5, 15, 15, 15),3,2,byrow=TRUE)
sds <- matrix(sqrt(c(1, 1, 1, 4, 4, 4)),3,2,byrow=TRUE)
distnames <- c('normal','uniform','exp')
nrep <- 1000

#conduct experiment using overall design parameters
for (distribution in distnames) {
    for (nptr in c(1:dim(ens)[1])) {
        for (sdptr in c(1:dim(sds)[1])) {
            #run the replications in function for this cell
            teepee <- runReps(nrep,distribution,ens[nptr,],sds[sdptr,])
            
            #plot results for this cell
            pr<-plotReps(distribution,teepee,nrep,ens[nptr,],sds[sdptr,])
            print(c(distribution,unlist(pr)))
            
            #if it's the "odd" cell, repeat it but reverse assignment
            if ((nptr==2) && (sdptr==2)) {
                teepee <- runReps(nrep,distribution,rev(ens[nptr,]),sds[sdptr,])
                pr<-plotReps(distribution,teepee,nrep,rev(ens[nptr,]),sds[sdptr,])
                print(c(distribution,unlist(pr)))
            }
        }
    }
}
