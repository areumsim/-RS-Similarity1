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
load(  file="C:/Users/haji/Desktop/200518/DB/ml_small_movie_genre.RData"  )  # movie_genre
load(  file="C:/Users/haji/Desktop/200518/DB/ml_small_genre_cnt.RData"  )  #genre_cnt

### ml-latest-small :  610 * 9724 


######################################################
###                 setTrainingSet                 ### 
######################################################

existRatingInx <- which( !is.na(ratings) , arr.ind=T )

maxRating <- 5  # max(as.matrix(trainingRatingMat), na.rm = TRUE)
minRating <- 0  # min(as.matrix(trainingRatingMat), na.rm = TRUE)

siCnt <- 5 #("Cos", "Pearson", "CPC", "MSD", "Jaccard", "aJaccard")


# >>>>> Reset.trainingset(1) <<<<< #
seedNo <- 1

set.seed(1)
testIdx <- sample_frac(as.data.frame(existRatingInx), 0.2 )

trainingRatingMat <- ratings
apply(testIdx, 1, function(x){ trainingRatingMat[x[1], x[2]] <<- NA })

userAvg <- rowMeans(trainingRatingMat, na.rm = TRUE, dims = 1)

load(  file="C:/Users/haji/Desktop/200518/DB/ml_small_siMatList.RData"  ) # siMatList
# >>>>> >>>>> >>>>> <<<<< <<<<< <<<<< #

######################################################

Reset.trainingset <- function( seedNo ){
  seedNo <<- seedNo
  
  set.seed(seedNo)
  testIdx <<- sample_frac(as.data.frame(existRatingInx), 0.2 )
  
  trainingRatingMat <<- ratings
  apply(testIdx, 1, function(x){ trainingRatingMat[x[1], x[2]] <<- NA })
  
  userAvg <<- rowMeans(trainingRatingMat, na.rm = TRUE, dims = 1)
  
  # siMatList <<- Cal.all.default.similarity(trainingRatingMat)    ## list()
  load(  file="C:/Users/haji/Desktop/200518/DB/ml_small_siMatList.RData"  ) # siMatList
}

######################################################

Get.sim.byWeighted <- function( w=rep(1/siCnt, siCnt) ){
  sim <- w[[1]]*siMatList[[1]] + w[[2]]*siMatList[[2]] + w[[3]]*siMatList[[3]] +
    w[[4]]*siMatList[[4]] + w[[5]]*siMatList[[5]] #+ w[[6]]*siMatList[[6]] 
  
  return( sim )
}

######################################################

Get.weighted <- function( mat , initW=rep(1/siCnt, siCnt) ){
  set.seed(seedNo)
  idx <- which( !is.na(mat) , arr.ind=T )
  # if(  nrow(idx) >= N  ){
  #   trgIdx <- sample_n(as.data.frame(idx), N, replace = FALSE)
  # }else{
  #   trgIdx <- sample_n(as.data.frame(idx), N, replace = TRUE)
  # }
  
  #TODO 
  if(  nrow(idx) < N  ){
    return ( initW )
  }else{
    trgIdx <- sample_n(as.data.frame(idx), N, replace = FALSE)
  }
  
  trgMat <- mat
  apply(trgIdx, 1, function(x){ trgMat[x[[1]], x[[2]]] <- NA })
  
  cost <- function( w ){
    return( Get.mae(trgIdx, mat, trgMat, Get.sim.byWeighted(w) ) )
  }
  
  wOptim <- optim(par=initW, fn=cost, method = c("BFGS")) 
  optimW <- wOptim$par/sum(wOptim$par)
  return( optimW )   
}

Get.weighted.byGenre <- function( mat ){
  genreWList <- list()
  genreCnt <- length(genre_cnt$genres) #19
  
  for( k in 1:genreCnt ){
    cat("[", k,"/",genreCnt,"] -",format(Sys.time(),usetz = TRUE),"  ")
    
    trgIdx <- movie_genre[movie_genre$genreNo==k, 1]
    
    if( length(trgIdx)>0 ){
      trgRatingMat <- mat[, colnames(mat) %in% as.character(trgIdx)]
      genreWList[[k]] <- Get.weighted( trgRatingMat ) 
    }else{
      # TODO 
      genreWList[[k]] <- totalWList
      # genreWList[[k]] <- rep(1/siCnt, siCnt) 
    }
  }
  return ( genreWList )
}

Get.weighted.byItem <- function( mat ){
  itemWList <- list()
  
  #  initW : 1/siCnt ?  GenreW ? TotalW ?
  # initW <- totalWList #genreWList[[ movie_genre[itemId,3] ]]
  # if( sum(!is.na(mat))==0 ){
  #   return ( initW )
  # }
  
  
  # [ 865 / 9724 ] - 2020-06-08 23:49:07 KST 
  # Error in optim(par = initW, fn = cost, method = c("BFGS")) : 
    # initial value in 'vmmin' is not finite
  
  for( k in  1:ncol(mat) ){
    cat("[", k,"/",ncol(mat),"] -",format(Sys.time(),usetz = TRUE),"\n")
    
    trgCol <- as.matrix(mat[,k])
    
    ### TODO 
    #  initW <- rep(1/siCnt, siCnt)  
    initW <- genreWList[[ movie_genre[k,3] ]] # genreWList[[ movie_genre[movie_genre$movieId==k, 3] ]]
    # initW <- totalWList
    
    if( sum(!is.na(trgCol))>0 ){
      itemWList[[k]] <<- Get.weighted( trgCol , initW )
    }else{
      itemWList[[k]] <<- initW
    }
  }
  
  return ( itemWList )
}

######################################################

Get.mae <- function(idx, trueMat, trgMat, sim ){
  tmp <- apply(idx, 1, function(x){ trueMat[ x[[1]], x[[2]] ] - Predict.rating.bySim( x[[1]], x[[2]], trgMat, sim) })
  return( mean(abs(tmp), na.rm=T) )
}

Get.mae.byGenre <- function(idx, trueMat, trgMat, genreWList ){
  genreNobyItemId <- movie_genre[idx[,2],3] #MovieLens Specification
  
  tmp <- apply(idx, 1, function(x){ trueMat[x[1], x[2]] -
      Predict.rating.bySim( x[1], x[2],  trgMat,  Get.sim.byWeighted( genreWList[[ genreNobyItemId[x[2]] ]] ) ) })
  return( mean(abs(tmp), na.rm=T) )
}

# ( testIdx , ratings, trainingRatingMat , itemWList )


# tmp <- apply(testIdx, 1, function(x){ cat(x[2],"\t") })

Get.mae.byItem <- function( idx, trueMat, trgMat, itemWList ){
  tmp <- apply(idx, 1, function(x){ trueMat[x[1], x[2]] -
      Predict.rating.bySim( x[1], x[2],  trgMat,  Get.sim.byWeighted( itemWList[x[2]] ) ) })
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

N <- 10
# for( n in c(10, 100, 1000, 5000, 10000, 15000) ){
#   N <<- n

fold <<- list()
# for( f in 1:10 ){
f <- 1

Reset.trainingset(f)  ## f로 set.seed() 재설정

t <- c()

t[1] <- Get.mae(testIdx, ratings, trainingRatingMat , siMatList[[1]])
t[2] <- Get.mae(testIdx, ratings, trainingRatingMat , siMatList[[2]])
t[3] <- Get.mae(testIdx, ratings, trainingRatingMat , siMatList[[3]])
t[4] <- Get.mae(testIdx, ratings, trainingRatingMat , siMatList[[4]])
t[5] <- Get.mae(testIdx, ratings, trainingRatingMat , siMatList[[5]])
#t[6] <- Get.mae(testIdx, ratings, trainingRatingMat , siMatList[[6]])

totalWList <- Get.weighted( trainingRatingMat )
t[7] <- Get.mae( testIdx, ratings, trainingRatingMat, Get.sim.byWeighted(totalWList) )

genreWList <- Get.weighted.byGenre( trainingRatingMat ) # 20
t[8] <- Get.mae.byGenre( testIdx , ratings, trainingRatingMat , genreWList )

itemWList <- Get.weighted.byItem( trainingRatingMat ) # 9724
t[9] <- Get.mae.byItem ( testIdx , ratings, trainingRatingMat , itemWList )
# [ 1 / 9724 ] - 2020-06-09 19:10:22 KST 
# [ 9724 / 9724 ] - 2020-06-09 19:54:35 KST 

fold[[f]] <- t

cat("[",f,"] ",t,"\n")

