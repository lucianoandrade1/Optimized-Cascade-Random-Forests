#MOPSO optmization for Cascade Random Forest parameters
#Author: Luciano Andrade

library(mopsocd)

varcount <- 4
fncount <- 2
lbound <- c(85,5,10,3)
ubound <- c(95,15,20,7)
optmax <- 1
optmin <- 0

mopsoCredit<-mopsocd(cost,varcnt=varcount,fncnt=fncount,  
               lowerbound=lbound,upperbound=ubound,opt=optmax,popsize = 40,maxgen = 10)

print(mopsoCredit$numsols)
print(mopsoCredit$objfnvalues)
print(mopsoCredit$paramvalues)

# Plot 
plot(mopsoCredit$objfnvalues[,1],mopsoCredit$objfnvalues[,2])

