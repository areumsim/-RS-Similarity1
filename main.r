library(readr)
library(csv)
library(tidyr)  #spread
library(dplyr)  #%>%
library(tibble)
library(plyr)
library(Metrics) # mae

library(textshape)


## Read Data 
ratings <-read.csv("C:/Users/haji/Desktop/ratingDB/the-movies-dataset/ratings_small.csv", header = T)

trueRatingMat <- select(ratings, userId, movieId, rating) %>%
  spread(movieId, rating) %>%
  group_by(userId) %>%
  column_to_rownames(1)

# trueRatingMat <- trueRatingMat[1:100, 1:100]

# Origin : 671 x 9066
userLen <- nrow(trueRatingMat)
movieLen <- ncol(trueRatingMat)

simCnt <- 6
siMat <- list()


Cal.all.similarity <- function(){
  siMat1 <- c() ; siMat2 <- c() ; siMat3 <- c()
  siMat4 <- c() ; siMat5 <- c() ; siMat6 <- c()
  
  for( i in 1:userLen){
    tmp <<- apply(trueRatingMat, 1,
                  function(x) {
                    Cal.similarity(trueRatingMat[i,], x)
                  }) 
    tmp <- round(tmp, digits = 4)
    
    siMat1 <- cbind(siMat1, tmp[1,])
    siMat2 <- cbind(siMat2, tmp[2,])
    siMat3 <- cbind(siMat3, tmp[3,])
    siMat4 <- cbind(siMat4, tmp[4,])
    siMat5 <- cbind(siMat5, tmp[5,])
    siMat6 <- cbind(siMat6, tmp[6,]) 
  }
  
  siMat <<- list( siMat1, siMat2, siMat3, siMat4 , siMat5, siMat6 )
  
  # # TODO : remove code - temporally saved file 
  write.csv( siMat1, file="C:/Users/haji/Desktop/5-Jaccrd_Implement/siMat1.csv" )
  write.csv( siMat2, file="C:/Users/haji/Desktop/5-Jaccrd_Implement/siMat2.csv" )
  write.csv( siMat3, file="C:/Users/haji/Desktop/5-Jaccrd_Implement/siMat3.csv" )
  write.csv( siMat4, file="C:/Users/haji/Desktop/5-Jaccrd_Implement/siMat4.csv" )
  write.csv( siMat5, file="C:/Users/haji/Desktop/5-Jaccrd_Implement/siMat5.csv" )
  write.csv( siMat6, file="C:/Users/haji/Desktop/5-Jaccrd_Implement/siMat6.csv" )
}

# # TODO : remove code - temporally saved file 
tmpReadSim() <- function(){
  siMat1 <- read.csv("C:/Users/haji/Desktop/5-Jaccrd_Implement/siMat1.csv", header = T)[,-1]
  siMat2 <- read.csv("C:/Users/haji/Desktop/5-Jaccrd_Implement/siMat2.csv", header = T)[,-1]
  siMat3 <- read.csv("C:/Users/haji/Desktop/5-Jaccrd_Implement/siMat3.csv", header = T)[,-1]
  siMat4 <- read.csv("C:/Users/haji/Desktop/5-Jaccrd_Implement/siMat4.csv", header = T)[,-1]
  siMat5 <- read.csv("C:/Users/haji/Desktop/5-Jaccrd_Implement/siMat5.csv", header = T)[,-1]
  siMat6 <- read.csv("C:/Users/haji/Desktop/5-Jaccrd_Implement/siMat6.csv", header = T)[,-1]
  siMat <<- list( siMat1, siMat2, siMat3, siMat4, siMat5, siMat6 )
}


Get.similarity <- function( userId ){
  UserSiMat <- matrix(0, nrow=userLen, ncol=simCnt)
  colnames(UserSiMat) <- Get.similarity.names()
  
  UserSiMat[,1] <- t(siMat[[1]][userId, ])
  UserSiMat[,2] <- t(siMat[[2]][userId, ])
  UserSiMat[,3] <- t(siMat[[3]][userId, ])
  UserSiMat[,4] <- t(siMat[[4]][userId, ])
  UserSiMat[,5] <- t(siMat[[5]][userId, ])
  UserSiMat[,6] <- t(siMat[[6]][userId, ])
  UserSiMat[userId,] <- 0
  
  siList <- rowMeans(UserSiMat, na.rm=T)
  
  return ( siList )
}


#########################################################
Predicted.rating <- function(userId, movieId){
  siList <-  Get.similarity(userId)
  
  if( sum(trueRatingMat[,movieId], na.rm=T)==0 ||
      sum(trueRatingMat[userId,], na.rm=T)==0 || 
      sum(abs(siList), na.rm=T)==0 ){
    return ( 0 )
  }
  
  # siR =  sim(U,V) * ( rating of V - avg of V ) 
  siR <- siList * (trueRatingMat[, movieId] - rowMeans(trueRatingMat, na.rm=T))
  
  result <- ( sum(trueRatingMat[userId,], na.rm=T) / sum(!is.na(trueRatingMat[userId,])) ) +
    ( sum(siR, na.rm=T) / sum(abs(siList[!is.na(siR)]), na.rm=T) )
  return( result )
}


Predict.all.ratings <- function(){
  cnt <- 0
  trueValue <- c()
  predictValue <- c()
  
  for (i in 1:userLen) {
    for (j in 1:movieLen) {
      if( !is.na(trueRatingMat[i,j]) ){
        cnt <- cnt + 1
        
        # get real rating
        trueValue[cnt] <<- trueRatingMat[i,j]
        
        # get predict rating
        predictValue[cnt] <<- Predicted.rating(i, j)
      }
    }
  }
  cat("MAE : ", mae(trueValue, predictValue),"\n")
}

