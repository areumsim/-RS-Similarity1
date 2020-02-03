library(readr)
library(tidyr)
library(dplyr)
library(tibble)
library(plyr)

library(textshape)

N <- 5
lamda <- 0.7
delta <- 0.7

## Read Data 
ratings <- read_csv("C:/Users/haji/Desktop/ratingDB/SUIR/ratings_small.csv")

realRatingMat <- select(ratings, userId, movieId, rating) %>% 
  spread(movieId, rating) %>% 
  group_by(userId) %>% 
  column_to_rownames(1)

tmpMat <- data.matrix(realRatingMat)

userLen <- nrow(tmpMat)
movieLen <- ncol(tmpMat)

su_list <- c()   #사용자 유사도 list
si_list <- c()   #아이템 유사도 list
sui_mat <- matrix(nrow=userLen,ncol=movieLen)   #사용자 아이템 유사도 matrix

### ###
cos_similarity <- function(a, b) {
  cnt1 <- 0 #동일 개채를 평가한 것의 갯수
  tmp1 <-0
  tmp2 <-0
  tmp3 <-0
  for(i in 1:length(a)){
    if( !is.na(a[i]) && !is.na(b[i]) ){
      tmp1 <- tmp1 + a[i]*b[i]
      tmp2 <- tmp2 + a[i]*a[i]
      tmp3 <- tmp3 + b[i]*b[i]
      cnt1 <- cnt1+1
    }
  }
  
  if( cnt1 > 0 ){
    return (  list("similarity" = tmp1/(sqrt(tmp2*tmp3)), "cnt" = cnt1 )   )
  }else{
    return ( list("similarity" = 0 , "cnt" = 0 ) )
  }
}

#   Error in su_list[[i]] <- cos_similarity(tmpMat[userID, ], tmpMat[i, ])$similarity : 
#   replacement has length zero
cal_similarity_user <- function(userId){
  # cnt <- c()
  for( i in 1:userLen){
    if( i != userId ){
      su_list[[i]] = unname(cos_similarity(tmpMat[userId,], tmpMat[i,])$similarity)
      # cnt <- c(cnt, cos_similarity(tmpMat[userId,], tmpMat[i,])$cnt)
    }else{
      su_list[[i]] = 99
    }
  }
  # View(su_list)
  tmpMat <- cbind(as.numeric(unlist(su_list)) , tmpMat)
  tmpMat <- tmpMat[order(tmpMat[,1], decreasing = TRUE),]
  
  sui_mat[,1] <- tmpMat[,1]
  
  tmpMat <- tmpMat[,-1]
  # View(tmpMat)
  # View(sui_mat)
}


cal_similarity_item <- function(movieId){
  # cnt <- c()
  for( i in 1:movieLen){
    if( i != movieId ){
      si_list[[i]] = cos_similarity(tmpMat[,movieId], tmpMat[,i])$similarity
      #cnt <- c(cnt, cos_similarity(tmpMat[userId,], tmpMat[i,])$cnt)
    }else{
      si_list[[i]] <- 99
    }
  }
  tmpMat <- rbind(as.numeric(unlist(si_list)) , tmpMat)
  tmpMat <- tmpMat[,order(tmpMat[1,], decreasing = TRUE)]
  sui_mat[1,] <- tmpMat[1,]
  
  tmpMat <- tmpMat[-1,]
  # View(tmpMat)
  # View(sui_mat)
}


cal_similarity_ui <- function(){
  # rank(Su) 와 rank(Si) 를 알아내야함 ...
  rankNSu <- 50
  rankNSi <- 50
  
  # Su 와 Si 가 아니고 , SuN*SiN 에 해당하는 부분만 계산
  for( i in 2:rankNSu ){
    for( j in 2:rankNSi ){
      sui_mat[i,j] <- ( 1 / sqrt((1/sui_mat[i,1])^2 + (1/sui_mat[1,j])^2 ) )
    }
  }
  # View(sui_mat)
}


# userId의 movieId에 대한 rating 예측
predicted_rating <- function(){
  N <-50
  result <- 0
  
  sumSUR <- sum(sui_mat[2:N,1], na.rm=TRUE)
  sumSIR <- sum(sui_mat[1,2:N], na.rm=TRUE) 
  sumSUIR <- 0
  for( i in 2:N ){
    for( j in 2:N ){
      if(!is.na(tmpMat[i,j])){
        sumSUIR <- sumSUIR + sui_mat[i,j]
      }
    }
  }
  cat(sumSUR , sumSIR, sumSUIR, "\n")
  
  for( i in 1:N ){
    for( j in 1:N ){
      pkm = tmpMat[i,j] -
        (mean(tmpMat[i,], na.rm=TRUE) - mean(tmpMat[1,], na.rm=TRUE)) -
        (mean(tmpMat[,j], na.rm=TRUE) - mean(tmpMat[,1], na.rm=TRUE))
      
      if( !is.na(pkm) ){
        if( i!=1 && j!=1 ){  #SUIR
          result <- result + pkm * ( sui_mat[i,j] / sumSUIR ) * delta 
        }else if( i==1 && j!=1 ){  #SUR
          result <- result + pkm * ( sui_mat[i,j] / sumSUIR ) * (lamda) * (1-delta) 
        }else if( i!=1 && j==1){ #SIR
          result <- result + pkm * ( sui_mat[i,j] / sumSUIR ) * (1-lamda) * (1-delta) 
        }
      }
    }
  }
  cat("result : ", result,"\n")
  return ( result )
}


#########################################################
#########################################################

main <- function(userId, movieId){
  valusIsNA <- function(userId, movieId){
    if( !is.na(tmpMat[userId,movieId]) ){
      return (tmpMat[userId,movieId])
    }else{
      return ( -1 )
    }
  }
  
  # 값이 존재하면 출력하고, 아니면 예측
  if( valusIsNA(userId, movieId) != -1 ){
    cat("r(", userId , ", " , movieId, ") already gave grades. ")
    return (valusIsNA(userId, movieId))
  }
  cal_similarity_user(userId)
  cal_similarity_item(movieId)
  cal_similarity_ui()
  expectedRating <- predicted_rating()
}



predictMat <- matrix(nrow=userLen, ncol=movieLen)
predictedAll <- function(){
  for( i in 1:userLen ){
    for( j in 1:movieLen ){
      predictMat[i, j] <<- main(i, j)
    }
  }
}

