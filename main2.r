library(readr)
library(csv)
library(tidyr)  #spread
library(dplyr)  #%>%
library(tibble)
library(plyr)
library(Metrics) # mae
library(textshape)

## Read Data 1 - Movie
ratings1 <- read.csv("C:/Users/haji/Desktop/ratingDB/the-movies-dataset/ratings_small.csv", header = T)

trueRatingMat <- select(ratings1, userId, movieId, rating) %>%
  spread(movieId, rating) %>%
  group_by(userId) %>%
  column_to_rownames(1)

######################################################
######################################################

# trueRatingMat <- trueRatingMat[1:80, 1:80]

# Origin : 671 x 9066

# Netflix test 440 * 11543
userLen <- nrow(trueRatingMat)
itemLen <- ncol(trueRatingMat)

siMat <- list()

siMat1 <- c() 
siMat2 <- c() 
siMat3 <- c()
siMat4 <- c()
siMat5 <- c()
siMat6 <- c()


Cal.all.similarity <- function(){
  apply(trueRatingMat, 1, function(y) {
    siMat1 <<- cbind( siMat1, apply(trueRatingMat, 1, function(x) {Cos.similarity(y, x)}) )
  } )
  apply(trueRatingMat, 1, function(y) {
    siMat2 <<- cbind( siMat2, apply(trueRatingMat, 1, function(x) {Cor.similarity(y, x)}) )
  } )
  apply(trueRatingMat, 1, function(y) {
    siMat3 <<- cbind( siMat3, apply(trueRatingMat, 1, function(x) {Cpc.similarity(y, x)}) )
  } )
  apply(trueRatingMat, 1, function(y) {
    siMat4 <<- cbind( siMat4, apply(trueRatingMat, 1, function(x) {Msd.similarity(y, x)}) )
  } )
  apply(trueRatingMat, 1, function(y) {
    siMat5 <<- cbind( siMat5, apply(trueRatingMat, 1, function(x) {Jaccard.similarity(y, x)}) )
  } )
  apply(trueRatingMat, 1, function(y) {
    siMat6 <<- cbind( siMat6, apply(trueRatingMat, 1, function(x) {AJac.similarity(y, x)}) )
  } )
  
  ##TODO : remove code - temporally saved file 
  write.csv( siMat1, file="C:/Users/haji/Desktop/5-Jaccrd_Implement/siMat1.csv" )
  write.csv( siMat2, file="C:/Users/haji/Desktop/5-Jaccrd_Implement/siMat2.csv" )
  write.csv( siMat3, file="C:/Users/haji/Desktop/5-Jaccrd_Implement/siMat3.csv" )
  write.csv( siMat4, file="C:/Users/haji/Desktop/5-Jaccrd_Implement/siMat4.csv" )
  write.csv( siMat5, file="C:/Users/haji/Desktop/5-Jaccrd_Implement/siMat5.csv" )
  write.csv( siMat6, file="C:/Users/haji/Desktop/5-Jaccrd_Implement/siMat6.csv" )
}

##TODO : remove code - temporally saved file 
tmpReadSim() <- function(){
  siMat1 <<- read.csv("C:/Users/haji/Desktop/5-Jaccrd_Implement/siMat1.csv", header = T)[,-1]
  siMat2 <<- read.csv("C:/Users/haji/Desktop/5-Jaccrd_Implement/siMat2.csv", header = T)[,-1]
  siMat3 <<- read.csv("C:/Users/haji/Desktop/5-Jaccrd_Implement/siMat3.csv", header = T)[,-1]
  siMat4 <<- read.csv("C:/Users/haji/Desktop/5-Jaccrd_Implement/siMat4.csv", header = T)[,-1]
  siMat5 <<- read.csv("C:/Users/haji/Desktop/5-Jaccrd_Implement/siMat5.csv", header = T)[,-1]
  siMat6 <<- read.csv("C:/Users/haji/Desktop/5-Jaccrd_Implement/siMat6.csv", header = T)[,-1]
  
  siMat <<- list( siMat1, siMat2, siMat3, siMat4, siMat5, siMat6)
}

siMat7 <<- (siMat1+siMat2+siMat3+siMat4+siMat5+siMat6)/6
siMat8 <<- (siMat1*siMat2*siMat3*siMat4*siMat5*siMat6)
siMat9 <<- (siMat2*siMat3)
siMat10 <<- (siMat4*siMat5)
siMat11 <<- (siMat2*siMat5)
siMat12 <<- (siMat4*siMat6)
siMat13 <<- (siMat2*siMat7)
siMat14 <<- (siMat3*siMat7)
siMat15 <<- Weighted.sim()


siMat16 <<- (siMat1 * siMat2 * siMat3 )
siMat17 <<- (siMat1 * siMat2 * siMat4 )
siMat18 <<- (siMat1 * siMat3 * siMat4 )

siMat19 <<- (siMat2 * siMat4 * siMat5 )
siMat20 <<- (siMat3 * siMat4 * siMat5 )
siMat21 <<- (siMat2 * siMat4 * siMat6 )
siMat22 <<- (siMat3 * siMat4 * siMat6 )

siMat23 <<- (siMat1 * siMat2 * siMat5 )
siMat24 <<- (siMat1 * siMat3 * siMat5 )
siMat25 <<- (siMat1 * siMat2 * siMat6 )
siMat26 <<- (siMat1 * siMat3 * siMat6 )

siMat27 <<- (siMat5 * siMat6 )
siMat28 <<- (siMat5 * siMat5 )
siMat29 <<- (siMat6 * siMat6 )

siMat <<- list( siMat1, siMat2, siMat3, siMat4, siMat5, siMat6,
                siMat7, siMat8, siMat9, siMat10, siMat11, siMat12, siMat13, siMat14, siMat15,
                siMat16, siMat17, siMat18, siMat19, siMat20, siMat21, siMat22, siMat23, 
                siMat24, siMat25, siMat26, siMat27 , siMat28, siMat29)
nameList <- c("Cos", "Pearson", "CPC", "MSD", "Jaccard","aJaccard",
              "Mean", "Product",  
              "Pearson*CPC", "MSD*Jac", "Pearson*Jac", 
              "MSD*aJac", "Pearson*aJac", "CPC*aJac" , "weighted",
              
              "Cos*PEA*CPC", "Cos*PEA*MSD", "Cos*CPC*MSD",
              "Pea*MSD*Jac", "CPC*MSD*Jac", "Pea*MSD*aJac", "CPC*MSD*aJac",
              "Cos*Pea*Jac", "Cos*CPC*Jac", "Cos*Pea*aJac", "Cos*CPC*aJac",
              "Jac*aJac", "Jac*Jac", "aJac*aJac"
              )
simCnt <<- length(siMat) 


### ### ### Weighted ### ### ### 
Weighted.sim <- function(){
  # cost <- function( w ){
  #   tmpsiMat <- matrix(0, nrow=userLen, ncol=userLen)
  #   for( i in 1:6){
  #     tmpsiMat <- tmpsiMat + w[[i]] * siMat[[i]]
  #   }
  #   return( Get.mae(tmpsiMat) )
  # }
  # 
  # initialW <- rep(1/6, 6)
  # wOptim <- optim(par=initialW, fn=cost)
  ## wOptim$par : [1]  0.16375877  0.08508979 -0.08205659 -0.17434590 -0.38501187  1.88218490
  
  # optimW <- wOptim$par/sum(wOptim$par)
  optimW <- c(0.10993332,  0.05712184, -0.05508562, -0.11704059, -0.25846330, 1.26353435)
  
  sim <- matrix(0, nrow=userLen, ncol=userLen)
  for( i in 1:6){
    sim <- sim + optimW[i] * siMat[[i]]
  }
  
  return(sim)  
}


################################################################# 
### Predict Rating : Predict.all.ratings(sim)[userId, MoveId] ### 
Predict.all.ratings <- function( sim ){
  ### Left factor
  userAvg <- rowMeans(trueRatingMat, na.rm = TRUE, dims = 1)
  
  ### Right Factor
  # ( rating of V - avg of V )
  userDeviation <- trueRatingMat - userAvg
  userDeviation[is.na(userDeviation)] <- 0
  
  # siR =  sim(U,V) * ( rating of V - avg of V )
  sumSimRating <- as.matrix(sim) %*% as.matrix(userDeviation)
  
  # for counting used value
  notUsedValue <- as.matrix( ifelse(is.na(trueRatingMat), 0, 1) )
  
  # abs sumSim for Avg
  sumSim <- as.matrix (abs(sim))  %*% notUsedValue
  
  # Right Factor
  totalSim <-  as.matrix(sumSimRating) / sumSim
  
  ### Result
  predictValue <- userAvg + totalSim
  
  predictValue[predictValue>5] <- 5
  predictValue[predictValue<0] <- 0
  return( predictValue )
}


### ### ### ### ### ### ### ###
### ### 평가1 : MAE ### ### ###

Get.mae <- function( sim ){
  predictValue <- Predict.all.ratings(sim)
  
  ### MAE (평가값이 있는 항목에 대해서만)
  maeMatrix <- abs(trueRatingMat-predictValue)
  mae <- sum(maeMatrix, na.rm=T)/sum(!is.na(maeMatrix))
  
  return( mae )
}


Get.all.mae <- function(){
  for( i in 1:simCnt){
    cat(nameList[i],"MAE : ", Get.mae(siMat[[i]]),"\n")
  }
}



### ### ### ### ### ### ### ###
### ### 평가2: TopN ### ### ###

### True와 Predict의 TonN 일치 개수 
Get.topN <- function( sim , n=10 ){
  predictValue <- Predict.all.ratings( sim )
  
  predictTopN <-t( apply(predictValue, 1, function(x) {order(x, na.last=TRUE, decreasing=T)[1:n]}) )
  trueTopN <- t( apply(trueRatingMat, 1, function(x) {order(x, na.last=TRUE, decreasing=T)[1:n]}) )
  
  diffMat <- matrix(ifelse( (predictTopN %in% trueTopN) == TRUE , 1 , 0 ), nrow=userLen, ncol=n)
  return( sum(diffMat)/userLen )
}


Get.all.topN <- function(n=10){
  for( i in 1:simCnt){
    cat(nameList[i],"top", n, " : ", Get.topN(siMat[[i]], n), "\n")
  }  
}

