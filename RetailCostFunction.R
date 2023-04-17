#Cascade Random Forests cost function using a retail dataset
#Author: Luciano Andrade

require(data.table)
library(randomForest)
library(caret)


#https://github.com/ltorgo/DMwR2/raw/master/data/sales.RData

retail<-fread("Datasets\\RetailFraudDataset.csv", sep = ';', header = TRUE)   


cost <- function(x){

  retailComplete<-retail[complete.cases(retail)]
  
  retailComplete$ID<-as.integer(gsub("v","",retailComplete$ID))
  
  retailComplete$Prod<-as.integer(gsub("p","",retailComplete$Prod))
  
  retailComplete$resp<-as.factor(ifelse(retailComplete$Insp == "ok", 0, 1))
  
  retailCompleteN<-retailComplete[,c(-5)]
  
  retailCompleteNGenuine<-retailCompleteN[retailCompleteN$resp==0,]
  
  retailCompleteNFraud<-retailCompleteN[retailCompleteN$resp==1,]
  
  mylist<-list()

  for(ind in 1:round(x[2])){

    subgenuine.idx<-sample(1:nrow(retailCompleteNGenuine), nrow(retailCompleteNFraud))
    
    subgenuine<-retailCompleteNGenuine[subgenuine.idx,]
    
    subretailComplete<-rbind(subgenuine,retailCompleteNFraud)
    
    subretailComplete.idx<-sample(1:nrow(subretailComplete), nrow(subretailComplete))
    
    subretailComplete<-subretailComplete[subretailComplete.idx,]
    
    train.idx<-sample(1:nrow(subretailComplete), as.integer(0.7*nrow(subretailComplete)))
    
    train<-subretailComplete[train.idx,]
    
    test<-subretailComplete[-train.idx,]
    
    print("*")
    print(x[1])
    print("**")
    print(round(x[2]))
    print("***")
    print(round(x[3]))

    
    TPR<-x[1]/100; #0.97 #(0.85,0.95)   #0.82
    
    nTree<-round(x[3])
    
    minLeaf<-round(x[4])#5
    
    pDmin<--1
    
    for (pCut in c(0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.11, 0.12, 0.13, 0.14, 0.15, 0.16, 0.17, 0.18, 0.19, 0.2, 0.21, 0.22, 0.23, 0.24,
                   0.25, 0.26, 0.27, 0.28, 0.29, 0.3, 0.31, 0.32, 0.33, 0.34, 0.35, 0.36, 0.37, 0.38, 0.39, 0.4, 0.41, 0.42, 0.43, 0.44, 0.45, 0.46, 0.47, 0.48, 0.49,
                   0.40, 0.41, 0.42, 0.43, 0.44, 0.45, 0.46, 0.47, 0.48, 0.49, 0.5, 0.51, 0.55, 0.56, 0.57, 0.58, 0.59, 0.6, 0.61, 0.62, 0.63, 0.64, 0.65, 0.66, 0.67, 
                   0.68, 0.69, 0.7, 0.71, 0.72, 0.73, 0.74, 0.75, 0.76, 0.77, 0.78, 0.79, 0.8, 0.81, 0.82, 0.83, 0.84, 0.85, 0.86, 0.87, 0.88, 0.89, 0.9, 0.91, 0.92, 
                   0.93, 0.94, 0.95)){
      
      rf <- randomForest(resp~. , data=train, ntree=nTree, minLeaf=minLeaf, cutoff=c(pCut,1-pCut))
      
      prevs<- predict(rf,test, type='response', cutoff=c(pCut,1-pCut))
      
      test$respRF<-prevs
      
      respDmaj<-test[test$resp==0,]
      
      respDmin<-test[test$resp==1,]
      
      pDmaj<-sum(respDmaj$resp==respDmaj$respRF)/nrow(respDmaj)
      
      pDmin<-sum(respDmin$resp==respDmin$respRF)/nrow(respDmin)
      
      print(pDmin)
      
      if (pDmin>=TPR) {print("break")
        break}
      
    }
    
    mylist[[ind]] = rf
    
  }
  
  
  #-----------------------------------------------------------------------------------------------------

  retailComplete<-retail[complete.cases(retail)]
  
  retailComplete$ID<-as.integer(gsub("v","",retailComplete$ID))
  
  retailComplete$Prod<-as.integer(gsub("p","",retailComplete$Prod))
  
  retailComplete$resp<-as.factor(ifelse(retailComplete$Insp == "ok", 0, 1))
  
  retailCompleteN<-retailComplete[,c(-5)]
  
  retailCompleteNGenuine<-retailCompleteN[retailCompleteN$resp==0,]
  
  retailCompleteNFraud<-retailCompleteN[retailCompleteN$resp==1,]

  subgenuine.idx<-sample(1:nrow(retailCompleteNGenuine), nrow(retailCompleteNFraud))
  
  subgenuine<-retailCompleteNGenuine[subgenuine.idx,]
  
  subretailComplete<-rbind(subgenuine,retailCompleteNFraud)
  
  subretailComplete.idx<-sample(1:nrow(subretailComplete), nrow(subretailComplete))
  
  subretailComplete<-subretailComplete[subretailComplete.idx,]

  s<-subretailComplete
  
  pCut<-c(0,0)
  
  respMaj<-NULL
  
  for (ind in 1:round(x[2])) {
    
    print(ind)
    
    rf<-mylist[[ind]]
    
    pCutAvg<-rf$forest$cutoff
    
    prevs<- predict(rf,s, type='response', cutoff=pCutAvg)
    
    s$respRF<-prevs
    
    respDmaj<-s[s$respRF==0,]
    
    s<-s[s$respRF==1,]
    
    respMaj<-rbind(respMaj,respDmaj)
    
    respMin<-s
    
  }
  
  
  allResp<-rbind(respMin,respMaj)
  
  cm<-confusionMatrix(data=allResp$resp, allResp$respRF, mode="everything")
  
  f1<-cm$byClass['Pos Pred Value']
  
  f2<-cm$byClass['Neg Pred Value']
  
  return(c(f1,f2))
}


