#NSGA-II optmization for Cascade Random Forest parameters
#Author: Luciano Andrade


library(nsga2R)

varcount <- 4
fncount <- 2
lbound <- c(85,5,10,3)
ubound <- c(95,15,20,7)

nsgar2Credit<-nsga2(cost, varcount, fncount, generations=10, popsize=40, 
                    lower.bounds=lbound, upper.bounds=ubound)

nsgar2Credit$value
nsgar2Credit$par
nsgar2Credit$pareto.optimal


plot(nsgar2Credit$value[,1],nsgar2Credit$value[,2], col = "red", pch=19)
par(new=TRUE)
plot(nsgar2Credit$value[,1],nsgar2Credit$value[,2])
