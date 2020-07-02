library(readr)
library(csv)
library(tidyr)  #spread
library(dplyr)  #%>%
library(tibble)
library(plyr)
library(Metrics) # mae
library(textshape)
library(prodlim)
library(robustbase) # rowMedian
library(stringr)

############ ############ ############ ############ 
###         Load Data - dataset_small           ###
############ ############ ############ ############ 

load(  file="C:/Users/haji/Desktop/200518/DB/ml_small_ratings.RData"  )  # ratings
# ml-latest-small :  610 * 9724

######################################################
###                 setTrainingSet                 ### 
######################################################

existRatingInx <- which( !is.na(ratings) , arr.ind=T )

maxRating <- 5  # max(as.matrix(trainingRatingMat), na.rm = TRUE)
minRating <- 0  # min(as.matrix(trainingRatingMat), na.rm = TRUE)

siCnt <- 7 #("Cos", "ACos","Pea", "CPC", "MSD", "Jac", "aJac")

minCnt <- 10

# >>>>> Reset.trainingset(1) <<<<< #
set.seed(1)
testIdx <<- sample_frac(as.data.frame(existRatingInx), 0.3 )

trainingRatingMat <- ratings
apply(testIdx, 1, function(x){ trainingRatingMat[x[1], x[2]] <<- NA })

userAvg <- rowMeans(trainingRatingMat, na.rm = TRUE, dims = 1)
rMed <- median(as.matrix(trainingRatingMat), na.rm = TRUE)

siMatList <- list()
# >>>>> >>>>> >>>>> <<<<< <<<<< <<<<< #

######################################################

Reset.trainingset <- function( seedNo ){
  set.seed(seedNo)
  
  testIdx <<- sample_frac(as.data.frame(existRatingInx), 0.3 )
  
  trainingRatingMat <<- ratings
  apply(testIdx, 1, function(x){ trainingRatingMat[x[1], x[2]] <<- NA })
  
  userAvg <<- rowMeans(trainingRatingMat, na.rm = TRUE, dims = 1)
  
  # siMatList <<- Cal.all.default.similarity(trainingRatingMat)    ## list()
  mat <<- as.matrix(trainingRatingMat)
  userLen <<- nrow(mat)
  rMed <<- median(mat, na.rm = TRUE)
  
  siMat1 <- matrix(data = NA, ncol = userLen, nrow=userLen)  # Cos
  siMat2 <- matrix(data = NA, ncol = userLen, nrow=userLen)  # Adjusted Cos
  siMat3 <- matrix(data = NA, ncol = userLen, nrow=userLen)  # Pearson
  siMat4 <- matrix(data = NA, ncol = userLen, nrow=userLen)  # Constrined Pearson C
  siMat5 <- matrix(data = NA, ncol = userLen, nrow=userLen)  # MSD  
  siMat6 <- matrix(data = NA, ncol = userLen, nrow=userLen)  # Jaccard
  siMat7 <- matrix(data = NA, ncol = userLen, nrow=userLen)  # aJaccard
  
  tmp <- list()
  cat("getSim\t", format(Sys.time(), usetz = TRUE), "\n")
  for( i in 1:(userLen-1) ) {  # 610
    # cat(seedNo, "-", i, "(getSim)\t", format(Sys.time(), usetz = TRUE), "\n")
    
    tmp <- apply(mat[i:userLen,], 1, function(x) { Cal.similarity(mat[i,], x) } )
    tmp <- matrix(unlist(tmp), ncol = siCnt, byrow = TRUE)
    
    siMat1[, i] <- c(rep(NA,i-1), unlist(tmp[,1]))
    siMat2[, i] <- c(rep(NA,i-1), unlist(tmp[,2]))
    siMat3[, i] <- c(rep(NA,i-1), unlist(tmp[,3]))
    siMat4[, i] <- c(rep(NA,i-1), unlist(tmp[,4]))
    siMat5[, i] <- c(rep(NA,i-1), unlist(tmp[,5]))
    siMat6[, i] <- c(rep(NA,i-1), unlist(tmp[,6]))
    siMat7[, i] <- c(rep(NA,i-1), unlist(tmp[,7]))
  }
  
  siMat1 <- round(siMat1, 5)
  siMat2 <- round(siMat2, 5)
  siMat3 <- round(siMat3, 5)
  siMat4 <- round(siMat4, 5)
  siMat5 <- round(siMat5, 5)
  siMat6 <- round(siMat6, 5)
  siMat7 <- round(siMat7, 5)
  
  siMat1[upper.tri(siMat1)] <- t(siMat1)[upper.tri(t(siMat1), diag=FALSE)]
  siMat2[upper.tri(siMat2)] <- t(siMat2)[upper.tri(t(siMat2), diag=FALSE)]
  siMat3[upper.tri(siMat3)] <- t(siMat3)[upper.tri(t(siMat3), diag=FALSE)]
  siMat4[upper.tri(siMat4)] <- t(siMat4)[upper.tri(t(siMat4), diag=FALSE)]
  siMat5[upper.tri(siMat5)] <- t(siMat5)[upper.tri(t(siMat5), diag=FALSE)]
  siMat6[upper.tri(siMat6)] <- t(siMat6)[upper.tri(t(siMat6), diag=FALSE)]
  siMat7[upper.tri(siMat7)] <- t(siMat7)[upper.tri(t(siMat7), diag=FALSE)]
  
  siMatList <<- list( siMat1, siMat2, siMat3, siMat4,  siMat5 , siMat6 , siMat7 )
}

######################################################

Get.sim.byWeighted <- function( w=rep(1/siCnt, siCnt) ){
  sim <- w[[1]]*siMatList[[1]] + w[[2]]*siMatList[[2]] + w[[3]]*siMatList[[3]] +
    w[[4]]*siMatList[[4]] + w[[5]]*siMatList[[5]] + w[[6]]*siMatList[[6]] + + w[[7]]*siMatList[[7]]  
  return( sim )
}

######################################################

Get.weighted <- function( mat , initW=rep(1/siCnt, siCnt) ){
  idx <- which( !is.na(mat) , arr.ind=T )

  # Use all target col item
  if( nrow(idx) < minCnt ){
    return ( rep(0, siCnt) )
  }
  
  trgMat <- mat
  apply(idx, 1, function(x){ trgMat[x[[1]], x[[2]]] <- NA })
  
  cost <- function( w ){
    return( Get.mae(idx, mat, trgMat, Get.sim.byWeighted(w) ) )
  }
  
  # #TODO  : < 10  or < 0
  # if ( all(initW == rep(0, siCnt)) ){
  #   return( initW )   
  # }
  
  #TODO : method = c("BFGS") or method 지정 없이
  wOptim <- optim(par=initW, fn=cost, method = c("BFGS")) 
  optimW <- wOptim$par/sum(wOptim$par)
  return( optimW )   
}


Get.weighted.byItem <- function( mat ){
  itemWList <- list()
  
  cat("byItem",format(Sys.time(),usetz = TRUE),"\n")
  for( k in 1:ncol(mat) ){
    # cat(seedNo,"- [", k,"] byItem",format(Sys.time(),usetz = TRUE),"\n")
    
    trgCol <- as.matrix(mat[,k])
    
    initW <- rep(1/siCnt, siCnt)
    
    #TODO  : < 10  or < 0
    if ( all(initW == rep(0, siCnt)) || sum(!is.na(trgCol)) < minCnt  ){
      itemWList[[k]] <- rep(0, siCnt)
    }else{
      itemWList[[k]] <- Get.weighted( trgCol , initW )
    }
  }
  
  return ( itemWList )
}

######################################################

Get.mae <- function(idx, trueMat, trgMat, sim ){
  tmp <- apply(idx, 1, function(x){ trueMat[ x[[1]], x[[2]] ] - Predict.rating.bySim( x[[1]], x[[2]], trgMat, sim) })
  return( mean(abs(tmp), na.rm=T) )
}


Get.mae.byItem <- function( idx, trueMat, trgMat, itemWList  ){
  tmp <- apply(idx, 1, function(x){ trueMat[x[1], x[2]] -
      Predict.rating.bySim( x[1], x[2],  trgMat,  Get.sim.byWeighted( itemWList[[ x[[2]] ]] ) ) })
  return( mean(abs(tmp), na.rm=T) )
}

######################################################

Predict.rating.bySim <- function(userId,  itemId, mat, sim ){
  userDeviation <- mat[, itemId] - userAvg
  userDeviation[is.na(userDeviation)] <- 0
  
  #  sim(U,V) * ( rating of V - avg of V ) / abs sumSim only used ratings
  totalSim <- ( sim[1,] %*% userDeviation ) / (abs(sim[1,]) %*% ifelse(is.na(mat[, itemId]), 0, 1))
  
  predictRating <- userAvg[userId] + totalSim
  
  predictRating[predictRating>maxRating] <- maxRating
  predictRating[predictRating<minRating] <- minRating
  predictRating[is.nan(predictRating)] <- NA
  
  return( predictRating )
}

######################################################
###                   get mae                      ### 
######################################################

totalWList <- list()
itemWList <- list()
userWList <- list()

fold <<- list()
tmpSimList <<-list()

for( f in 1:3 ){
  f<-1
  Reset.trainingset(f)  ## f로 set.seed() 재설정
  t <- c()
  
  t[1] <- Get.mae(testIdx, ratings, trainingRatingMat , siMatList[[1]])
  t[2] <- Get.mae(testIdx, ratings, trainingRatingMat , siMatList[[2]])
  t[3] <- Get.mae(testIdx, ratings, trainingRatingMat , siMatList[[3]])
  t[4] <- Get.mae(testIdx, ratings, trainingRatingMat , siMatList[[4]])
  t[5] <- Get.mae(testIdx, ratings, trainingRatingMat , siMatList[[5]])
  t[6] <- Get.mae(testIdx, ratings, trainingRatingMat , siMatList[[6]])
  t[7] <- Get.mae(testIdx, ratings, trainingRatingMat , siMatList[[7]])
  
  totalWList <- Get.weighted( trainingRatingMat )
  t[8] <- Get.mae( testIdx, ratings, trainingRatingMat, Get.sim.byWeighted(totalWList) )
  
  itemWList <- Get.weighted.byItem( trainingRatingMat ) # 9724 > 
  t[9] <- Get.mae.byItem( testIdx , ratings, trainingRatingMat , itemWList )  
  
  # 
  # itemWList <- Get.weighted.byItem( trainingRatingMat ) # 9724 > 
  # t[9] <- Get.mae.byItem( testIdx , ratings, trainingRatingMat , itemWList )  
  # 
  
  tmpSimList[[f]]  <- list(siMatList, totalWList ,itemWList)
  fold[[f]] <- t
  # 
  # save( tmpSimList, file="C:/Users/haji/Desktop/200518/DB/ml_small_N100_23_2_SimList.RData"  )
  # save( fold, file="C:/Users/haji/Desktop/200518/DB/ml_small_N100_23_2_fold.RData"  )
}

